package expression;

public class Implication extends BinaryOperation {

    public Implication(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public String getToken() {
        return "->";
    }
}
