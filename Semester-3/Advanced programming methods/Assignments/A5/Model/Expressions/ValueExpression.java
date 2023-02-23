package Model.Expressions;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Values.Value;

public class ValueExpression implements Expression {
    private final Value expression;

    public ValueExpression(Value expression) {
        this.expression = expression;
    }

    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) throws MyException {
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
