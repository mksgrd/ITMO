package parser;

import expression.*;

public class ExpressionParser {
    private static final String IMPLIES = "->";
    private static final String OR = "|";
    private static final String AND = "&";
    private static final String EQUALS = "=";
    private static final String ADD = "+";
    private static final String MULTIPLY = "*";
    private static final String NEXT = "'";

    private String string;
    private int pos;

    private char peek() {
        return peek(1).charAt(0);
    }

    private String peek(int step) {
        return string.substring(pos, pos + step);
    }

    private boolean hasSymbols() {
        return hasSymbols(1);
    }

    private boolean hasSymbols(int cnt) {
        return pos + cnt - 1 < string.length();
    }

    private void next() {
        next(1);
    }

    private void next(int step) {
        pos += step;
    }

    private String peekBehind(int step) {
        return string.substring(pos - step, pos);
    }

    private boolean isVariable() {
        return Character.isAlphabetic(peek()) && Character.isLowerCase(peek());
    }

    private boolean isPredicate() {
        return Character.isAlphabetic(peek()) && Character.isUpperCase(peek());
    }

    private Expression parsePredicate() {
        int len = 0;
        while (hasSymbols() && isPredicate()) {
            len++;
            next();
        }
        return new Variable(peekBehind(len));
    }

    private Variable parseVariable() {
        int len = 0;
        while (hasSymbols() && isVariable()) {
            len++;
            next();
        }
        return new Variable(peekBehind(len));
    }

    private Expression parseUnary() {
        if (isPredicate()) {
            return parsePredicate();
        }
        if (isVariable()) {
            return parseVariable();
        }
        if (peek() == '0') {
            next();
            return new Variable("0");
        }
        if (peek() == '!') {
            next();
            return new Negation(parseEquals());
        }
        if (peek() == '@') {
            next();
            Variable var = parseVariable();
            next();
            return new ForallQuantifier(var, parseImplication());
        }
        if (peek() == '?') {
            next();
            Variable var = parseVariable();
            next();
            return new ExistsQuantifier(var, parseImplication());
        }
        if (peek() == '(') {
            next();
            Expression parsed = parseImplication();
            next();
            return parsed;
        }
        return null;
    }

    private Expression parseNext() {
        Expression parsed = parseUnary();
        while (hasSymbols(NEXT.length()) && peek(NEXT.length()).equals(NEXT)) {
            next(NEXT.length());
            parsed = new Next(parsed);
        }
        return parsed;
    }

    private Expression parseMultiply() {
        Expression parsed = parseNext();
        while (hasSymbols(MULTIPLY.length()) && peek(MULTIPLY.length()).equals(MULTIPLY)) {
            next(MULTIPLY.length());
            parsed = new Multiplication(parsed, parseNext());
        }
        return parsed;
    }

    private Expression parseAdd() {
        Expression parsed = parseMultiply();
        while (hasSymbols(ADD.length()) && peek(ADD.length()).equals(ADD)) {
            next(ADD.length());
            parsed = new Addition(parsed, parseMultiply());
        }
        return parsed;
    }

    private Expression parseEquals() {
        Expression parsed = parseAdd();
        while (hasSymbols(EQUALS.length()) && peek(EQUALS.length()).equals(EQUALS)) {
            next(EQUALS.length());
            parsed = new Equals(parsed, parseAdd());
        }
        return parsed;
    }

    private Expression parseAnd() {
        Expression parsed = parseEquals();
        while (hasSymbols(AND.length()) && peek(AND.length()).equals(AND)) {
            next(AND.length());
            parsed = new Conjunction(parsed, parseEquals());
        }
        return parsed;
    }

    private Expression parseOr() {
        Expression parsed = parseAnd();
        while (hasSymbols(OR.length()) && peek(OR.length()).equals(OR)) {
            next(OR.length());
            parsed = new Disjunction(parsed, parseAnd());
        }
        return parsed;
    }

    private Expression parseImplication() {
        Expression parsed = parseOr();
        while (hasSymbols(IMPLIES.length()) && peek(IMPLIES.length()).equals(IMPLIES)) {
            next(IMPLIES.length());
            parsed = new Implication(parsed, parseImplication());
        }
        return parsed;
    }

    public Expression parseExpression(String expression) {
        this.string = expression;
        this.pos = 0;
        return parseImplication();
    }
}