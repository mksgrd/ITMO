package expression;

public class Multiplication extends BinaryOperation {

    public Multiplication(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public String getToken() {
        return "*";
    }
}
