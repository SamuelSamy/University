package Model.Expressions;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Types.Type;
import Model.Values.BoolValue;
import Model.Values.Value;

public class NegateExpression implements Expression {

    private final Expression expression;

    public NegateExpression(Expression expression) {
        this.expression = expression;
    }

    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) throws MyException {
        BoolValue value = (BoolValue) this.expression.evaluate(symbolTable, heap);
        return new BoolValue(!value.getValue());
    }

    @Override
    public Expression deepCopy() {
        return new NegateExpression(this.expression.deepCopy());
    }

    @Override
    public Type typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        return this.expression.typeCheck(table);
    }

    @Override
    public String toString() {
        return "!" + expression.toString();
    }
}
