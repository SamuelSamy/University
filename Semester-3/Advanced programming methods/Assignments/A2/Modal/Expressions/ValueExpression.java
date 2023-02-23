package Modal.Expressions;

import Exceptions.EvaluateException;
import Exceptions.MyException;
import Modal.Collections.MyDictionaryInterface;
import Modal.Values.Value;

public class ValueExpression implements Expression {
    private final Value expression;

    public ValueExpression(Value expression) {
        this.expression = expression;
    }

    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable) throws MyException {
        return expression;
    }

    @Override
    public Expression deepCopy() {
        return new ValueExpression(expression.deepCopy());
    }

    @Override
    public String toString() {
        return this.expression.toString();
    }
}
