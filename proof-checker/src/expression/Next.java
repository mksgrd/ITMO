package expression;

public class Next extends UnaryOperation {
    public Next(Expression expression) {
        super(expression);
    }

    @Override
    public String getToken() {
        return "'";
    }

    @Override
    public String toString() {
        return getExpression() + getToken();
    }
}
