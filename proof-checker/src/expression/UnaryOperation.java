package expression;

import java.util.Objects;

public abstract class UnaryOperation implements Expression {
    private final Expression expression;

    public UnaryOperation(Expression expression) {
        this.expression = expression;
    }

    public abstract String getToken();

    public Expression getExpression() {
        return expression;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UnaryOperation that = (UnaryOperation) o;
        return Objects.equals(expression, that.expression);
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression);
    }

    @Override
    public String toString() {
        return "(" + getToken() + expression + ")";
    }
}
