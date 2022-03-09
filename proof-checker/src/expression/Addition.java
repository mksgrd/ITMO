package expression;

public class Addition extends BinaryOperation {

    public Addition(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public String getToken() {
        return "+";
    }
}
