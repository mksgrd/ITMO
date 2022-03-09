package expression;

public class ForallQuantifier extends Quantifier {

    public ForallQuantifier(Variable variable, Expression expression) {
        super(variable, expression);
    }

    @Override
    public String getToken() {
        return "@";
    }
}
