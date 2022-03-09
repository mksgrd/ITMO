package expression;

public class ExistsQuantifier extends Quantifier{

    public ExistsQuantifier(Variable variable, Expression expression) {
        super(variable, expression);
    }

    @Override
    public String getToken() {
        return "?";
    }
}
