package Model.Expressions;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Values.Value;

public class VariableExpression implements Expression {
    private final String name;

    public VariableExpression(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }

    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) throws MyException {
        return symbolTable.get(this.name);
    }

    @Override
    public Expression deepCopy() {
        return new VariableExpression(name);
    }
}
