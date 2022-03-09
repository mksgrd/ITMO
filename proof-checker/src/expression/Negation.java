package expression;

public class Negation extends UnaryOperation {

    public Negation(Expression expression) {
        super(expression);
    }

    @Override
    public String getToken() {
        return "!";
    }
}
