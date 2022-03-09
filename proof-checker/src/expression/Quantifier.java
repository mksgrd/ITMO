package expression;

import java.util.Objects;

public abstract class Quantifier implements Expression {
    private final Variable variable;
    private final Expression expression;

    public Quantifier(Variable variable, Expression expression) {
        this.variable = variable;
        this.expression = expression;
    }

    public abstract String getToken();

    public Variable getVariable() {
        return variable;
    }

    public Expression getExpression() {
        return expression;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Quantifier that = (Quantifier) o;
        return Objects.equals(variable, that.variable) && Objects.equals(expression, that.expression);
    }

    @Override
    public int hashCode() {
        return Objects.hash(variable, expression);
    }

    @Override
    public String toString() {
        return "(" + getToken() + variable + "." + expression + ")";
    }
}
