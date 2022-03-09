package expression;

public class Equals extends BinaryOperation {

    public Equals(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public String getToken() {
        return "=";
    }
}
