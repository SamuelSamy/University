package Model.Expressions;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Values.ReferenceValue;
import Model.Values.Value;

public class ReadHeapExpression implements Expression {

    private final Expression expression;

    public ReadHeapExpression(Expression expression) {
        this.expression = expression;
    }

    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) throws MyException {
        Value value = this.expression.evaluate(symbolTable, heap);

        if (!(value instanceof ReferenceValue referenceValue)) {
            throw new MyException("Expected RefType, got " + value.getType().toString());
        }

        int address = referenceValue.getAddress();

        if (!heap.exists(address)) {
            throw new MyException("No value at address " + address);
        }

        return heap.get(address);
    }

    @Override
    public String toString() {
        return "readHeap(" + expression.toString() + ")";
    }

    @Override
    public Expression deepCopy() {
        return new ReadHeapExpression(this.expression.deepCopy());
    }
}
