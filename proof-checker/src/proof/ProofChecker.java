package proof;

import expression.*;
import parser.ExpressionParser;

import java.io.*;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class ProofChecker {
    private final List<Expression> LOGIC_AXIOM_SCHEMES;
    private final List<Expression> ARITHMETIC_AXIOMS;

    private final ExpressionParser parser;
    private final Map<Expression, Set<Expression>> impliesFrom;
    private final Map<Expression, Integer> firstOccurrenceIndex;

    public ProofChecker() {
        firstOccurrenceIndex = new HashMap<>();
        impliesFrom = new HashMap<>();
        parser = new ExpressionParser();

        LOGIC_AXIOM_SCHEMES = List.of(
                "A->B->A",
                "(A->B)->(A->B->C)->(A->C)",
                "A->B->A&B",
                "A&B->A",
                "A&B->B",
                "A->A|B",
                "B->A|B",
                "(A->C)->(B->C)->(A|B->C)",
                "(A->B)->(A->!B)->!A",
                "!!A->A"
        ).stream().map(parser::parseExpression).collect(Collectors.toList());

        ARITHMETIC_AXIOMS = List.of(
                "a=b->a'=b'",
                "a=b->a=c->b=c",
                "a'=b'->a=b",
                "!a'=0",
                "a+b'=(a+b)'",
                "a+0=a",
                "a*0=0",
                "a*b'=a*b+a"
        ).stream().map(parser::parseExpression).collect(Collectors.toList());
    }

    private boolean checkStructureMatch(Expression source, Expression target, Map<Variable, Expression> varToExprMatching) {
        if (target instanceof Variable) {
            Variable varAxiom = (Variable) target;
            varToExprMatching.putIfAbsent(varAxiom, source);
            return varToExprMatching.get(varAxiom).equals(source);
        }
        if (source.getClass() != target.getClass()) {
            return false;
        }
        if (target instanceof BinaryOperation) {
            BinaryOperation bopExpr = (BinaryOperation) source;
            BinaryOperation bopAxiom = (BinaryOperation) target;
            return checkStructureMatch(bopExpr.getLeft(), bopAxiom.getLeft(), varToExprMatching)
                    && checkStructureMatch(bopExpr.getRight(), bopAxiom.getRight(), varToExprMatching);
        }
        if (target instanceof UnaryOperation) {
            UnaryOperation uopExpr = (UnaryOperation) source;
            UnaryOperation uopAxiom = (UnaryOperation) target;
            return checkStructureMatch(uopExpr.getExpression(), uopAxiom.getExpression(), varToExprMatching);
        }
        return false;
    }

    private String tryLogicAxiomSchemes(Expression expression) {
        for (int i = 0; i < LOGIC_AXIOM_SCHEMES.size(); i++) {
            if (checkStructureMatch(expression, LOGIC_AXIOM_SCHEMES.get(i), new HashMap<>())) {
                return String.format("Ax. sch. %d", i + 1);
            }
        }
        return null;
    }

    private boolean checkVariableReplaceMatch(Expression before, Expression after, Variable replaceVariable, Map<Variable, Expression> varToExprMatching) {
        if (before instanceof Variable) {
            if (before.equals(replaceVariable)) {
                varToExprMatching.putIfAbsent(replaceVariable, after);
                return varToExprMatching.get(replaceVariable).equals(after);
            } else {
                return before.equals(after);
            }
        }
        if (before.getClass() != after.getClass()) {
            return false;
        }
        if (before instanceof BinaryOperation) {
            BinaryOperation bopSource = (BinaryOperation) before;
            BinaryOperation bopTarget = (BinaryOperation) after;
            return checkVariableReplaceMatch(bopSource.getLeft(), bopTarget.getLeft(), replaceVariable, varToExprMatching)
                    && checkVariableReplaceMatch(bopSource.getRight(), bopTarget.getRight(), replaceVariable, varToExprMatching);
        }
        if (before instanceof UnaryOperation) {
            return checkVariableReplaceMatch(((UnaryOperation) before).getExpression(),
                    ((UnaryOperation) after).getExpression(), replaceVariable, varToExprMatching);
        }
        if (before instanceof Quantifier) {
            Quantifier qSource = (Quantifier) before;
            Quantifier qTarget = (Quantifier) after;
            return qSource.equals(qTarget) || (!replaceVariable.equals(qSource.getVariable())
                    && qSource.getVariable().equals(qTarget.getVariable())
                    && checkVariableReplaceMatch(qSource.getExpression(), qTarget.getExpression(), replaceVariable, varToExprMatching));
        }
        throw new ProofCheckerException("Unsupported operation");
    }

    private void getFreeVariables(Expression expression, Set<Variable> freeVariables) {
        if (expression instanceof Variable) {
            Variable exprVariable = (Variable) expression;
            if (!exprVariable.getName().equals("0")) {
                freeVariables.add(exprVariable);
            }
        } else if (expression instanceof BinaryOperation) {
            BinaryOperation bopExpr = (BinaryOperation) expression;
            getFreeVariables(bopExpr.getLeft(), freeVariables);
            getFreeVariables(bopExpr.getRight(), freeVariables);
        } else if (expression instanceof UnaryOperation) {
            getFreeVariables(((UnaryOperation) expression).getExpression(), freeVariables);
        } else if (expression instanceof Quantifier) {
            Quantifier qExpression = (Quantifier) expression;
            boolean remove = !freeVariables.contains(qExpression.getVariable());
            getFreeVariables(qExpression.getExpression(), freeVariables);
            if (remove) {
                freeVariables.remove(qExpression.getVariable());
            }
        }
    }

    private boolean isFreeForSubstitution(Expression expression, Variable x, Set<Variable> replacingFreeVariables, List<Variable> tiedVariables) {
        if (expression instanceof Variable) {
            if (!expression.equals(x) || tiedVariables.contains(x)) {
                return true;
            }
            return Collections.disjoint(replacingFreeVariables, tiedVariables);
        }
        if (expression instanceof UnaryOperation) {
            return isFreeForSubstitution(((UnaryOperation) expression).getExpression(), x, replacingFreeVariables, tiedVariables);
        }
        if (expression instanceof BinaryOperation) {
            return isFreeForSubstitution(((BinaryOperation) expression).getLeft(), x, replacingFreeVariables, tiedVariables)
                    && isFreeForSubstitution(((BinaryOperation) expression).getRight(), x, replacingFreeVariables, tiedVariables);
        }
        if (expression instanceof Quantifier) {
            Variable quantifierVar = ((Quantifier) expression).getVariable();
            tiedVariables.add(quantifierVar);
            boolean result = isFreeForSubstitution(((Quantifier) expression).getExpression(), x, replacingFreeVariables, tiedVariables);
            tiedVariables.remove(quantifierVar);
            return result;
        }
        throw new ProofCheckerException("Unsupported operation");
    }

    // (@x.phi) -> phi[x := t]
    private String tryAxiomScheme11(Expression expression) {
        if (!(expression instanceof Implication)) {
            return null;
        }

        Expression forallPhi = ((Implication) expression).getLeft();
        Expression phiSubstituted = ((Implication) expression).getRight();

        if (!(forallPhi instanceof ForallQuantifier)) {
            return null;
        }

        Variable x = ((ForallQuantifier) forallPhi).getVariable();
        Expression phi = ((ForallQuantifier) forallPhi).getExpression();

        Map<Variable, Expression> replacing = new HashMap<>();
        if (!checkVariableReplaceMatch(phi, phiSubstituted, x, replacing)) {
            return null;
        }

        Set<Variable> freeVariablesInT = new HashSet<>();
        if (!replacing.isEmpty()) {
            Expression t = replacing.get(x);
            getFreeVariables(t, freeVariablesInT);
        }

        if (freeVariablesInT.isEmpty() || isFreeForSubstitution(phi, x, freeVariablesInT, new ArrayList<>())) {
            return "Ax. sch. 11";
        }

        throw new ProofCheckerException("variable " + x + " is not free for term "
                + replacing.get(x) + " in @-axiom.");
    }

    // phi[x := t] -> (?x.phi)
    private String tryAxiomScheme12(Expression expression) {
        if (!(expression instanceof Implication)) {
            return null;
        }
        Expression phiSubstituted = ((Implication) expression).getLeft();
        Expression existsPhi = ((Implication) expression).getRight();
        if (!(existsPhi instanceof ExistsQuantifier)) {
            return null;
        }
        Variable x = ((ExistsQuantifier) existsPhi).getVariable();
        Expression phi = ((ExistsQuantifier) existsPhi).getExpression();

        Map<Variable, Expression> replacing = new HashMap<>();
        if (!checkVariableReplaceMatch(phi, phiSubstituted, x, replacing)) {
            return null;
        }

        Set<Variable> freeVariablesInT = new HashSet<>();
        if (!replacing.isEmpty()) {
            getFreeVariables(replacing.get(x), freeVariablesInT);
        }

        if (freeVariablesInT.isEmpty() || isFreeForSubstitution(phi, x, freeVariablesInT, new ArrayList<>())) {
            return "Ax. sch. 12";
        }
        throw new ProofCheckerException("variable " + x + " is not free for term "
                + replacing.get(x) + " in ?-axiom.");
    }

    // (phi[x:=0]&(@x.phi->phi[x:=x']))->phi
    private String tryAxiomSchemeA9(Expression expression) {
        if (!(expression instanceof Implication)) {
            return null;
        }
        Expression conj = ((Implication) expression).getLeft(); // (phi[x:=0]&(@x.phi->phi[x:=x']))
        Expression phi = ((Implication) expression).getRight(); // phi
        if (!(conj instanceof Conjunction)) {
            return null;
        }
        Expression phiZeroSubstituted = ((Conjunction) conj).getLeft(); // phi[x:=0]
        Expression forallQuantifier = ((Conjunction) conj).getRight(); // @x.phi->phi[x:=x']
        if (!(forallQuantifier instanceof ForallQuantifier)) {
            return null;
        }
        Variable x = ((ForallQuantifier) forallQuantifier).getVariable(); // x
        Expression impl = ((ForallQuantifier) forallQuantifier).getExpression(); // phi->phi[x:=x']
        if (!(impl instanceof Implication)) {
            return null;
        }
        Expression underForallPhi = ((Implication) impl).getLeft(); // phi
        Expression underForallPhiNextSubst = ((Implication) impl).getRight(); // phi[x:=x']

        if (!phi.equals(underForallPhi)) {
            return null;
        }

        if (!checkVariableReplaceMatch(phi, phiZeroSubstituted, x, new HashMap<>(Map.of(x, new Variable("0"))))) {
            return null;
        }

        if (!checkVariableReplaceMatch(phi, underForallPhiNextSubst, x, new HashMap<>(Map.of(x, new Next(x))))) {
            return null;
        }

        Set<Variable> freeVariables = new HashSet<>();
        getFreeVariables(phi, freeVariables);
        if (freeVariables.contains(x)) {
            return "Ax. sch. A9";
        }
        throw new ProofCheckerException("variable " + x + " is not free for term "
                + phi + " in A9-axiom.");
    }

    private String tryArithmeticAxioms(Expression expression) {
        for (int i = 0; i < ARITHMETIC_AXIOMS.size(); i++) {
            if (expression.equals(ARITHMETIC_AXIOMS.get(i))) {
                return String.format("Ax. A%d", i + 1);
            }
        }
        return null;
    }

    private String tryModusPonens(Expression expression) {
        Set<Expression> leftParts = impliesFrom.getOrDefault(expression, Collections.emptySet());
        for (Expression leftPart : leftParts) {
            if (firstOccurrenceIndex.containsKey(leftPart)) {
                int k = firstOccurrenceIndex.get(leftPart);
                int l = firstOccurrenceIndex.get(new Implication(leftPart, expression));
                return String.format("M.P. %d, %d", k + 1, l + 1);
            }
        }
        return null;
    }

    // psi -> phi
    // ----------
    // (?x.psi) -> phi
    private String tryExistsIntro(Expression expression) {
        if (!(expression instanceof Implication)) {
            return null;
        }
        Expression existsQuantifier = ((Implication) expression).getLeft();
        Expression phi = ((Implication) expression).getRight();
        if (!(existsQuantifier instanceof ExistsQuantifier)) {
            return null;
        }
        Variable x = ((ExistsQuantifier) existsQuantifier).getVariable();
        Expression psi = ((ExistsQuantifier) existsQuantifier).getExpression();

        Set<Variable> phiFreeVariables = new HashSet<>();
        getFreeVariables(phi, phiFreeVariables);
        if (phiFreeVariables.contains(x)) {
            throw new ProofCheckerException("variable " + x + " occurs free in ?-rule.");
        }
        int implIndex = firstOccurrenceIndex.getOrDefault(new Implication(psi, phi), -1);
        if (implIndex != -1) {
            return String.format("?-intro %d", implIndex + 1);
        }
        return null;
    }

    // phi -> psi
    // ----------
    // phi -> @x.psi
    private String tryForallIntro(Expression expression) {
        if (!(expression instanceof Implication)) {
            return null;
        }
        Expression phi = ((Implication) expression).getLeft();
        Expression forallQuantifier = ((Implication) expression).getRight();
        if (!(forallQuantifier instanceof ForallQuantifier)) {
            return null;
        }
        Variable x = ((ForallQuantifier) forallQuantifier).getVariable();
        Expression psi = ((ForallQuantifier) forallQuantifier).getExpression();

        Set<Variable> phiFreeVariables = new HashSet<>();
        getFreeVariables(phi, phiFreeVariables);
        if (phiFreeVariables.contains(x)) {
            throw new ProofCheckerException("variable " + x + " occurs free in @-rule.");
        }
        int implIndex = firstOccurrenceIndex.getOrDefault(new Implication(phi, psi), -1);
        if (implIndex != -1) {
            return String.format("@-intro %d", implIndex + 1);
        }
        return null;
    }

    public void checkProof() {
        List<Function<Expression, String>> checkers = List.of(
                this::tryLogicAxiomSchemes,
                this::tryAxiomScheme11,
                this::tryAxiomScheme12,
                this::tryAxiomSchemeA9,
                this::tryArithmeticAxioms,
                this::tryModusPonens,
                this::tryExistsIntro,
                this::tryForallIntro
        );
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
             BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {

            Expression provingExpression = parser.parseExpression(reader.readLine().substring(2));
            writer.write("|-" + provingExpression);
            writer.newLine();

            String line;
            String prevLine = "#";
            boolean proved = false;
            int counter = 0;
            try {
                while ((line = reader.readLine()) != null && !line.isEmpty()) {
                    prevLine = line;
                    proved = false;

                    Expression expression = parser.parseExpression(line);

                    String result = String.format("Expression %d is not proved.%n", counter + 1);

                    for (Function<Expression, String> checker : checkers) {
                        String checkerResult = checker.apply(expression);
                        if (checkerResult != null) {
                            result = String.format("[%d. %s] %s%n", counter + 1, checkerResult, expression);
                            proved = true;
                            break;
                        }
                    }

                    writer.write(result);
                    if (!proved) {
                        break;
                    }

                    firstOccurrenceIndex.putIfAbsent(expression, counter);
                    if (expression instanceof Implication) {
                        Expression left = ((Implication) expression).getLeft();
                        Expression right = ((Implication) expression).getRight();
                        impliesFrom.computeIfAbsent(right, key -> new TreeSet<>(Comparator.comparingInt(value -> firstOccurrenceIndex.get(new Implication(value, right)))));
                        impliesFrom.get(right).add(left);
                    }
                    counter++;
                }
            } catch (ProofCheckerException e) {
                writer.write(String.format("Expression %d: %s%n", counter + 1, e.getMessage()));
            }

            if (proved && !parser.parseExpression(prevLine).equals(provingExpression)) {
                writer.write("The proof proves different expression.");
            }
        } catch (IOException e) {
            System.err.println("Reading/Writing error occurred.");
        }
    }
}
