package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.ReferenceType;
import Model.Types.Type;
import Model.Values.ReferenceValue;
import Model.Values.Value;

public class WriteHeapStatement implements StatementInterface{

    private final String variableName;
    private final Expression expression;

    public WriteHeapStatement(String variableName, Expression expression) {
        this.variableName = variableName;
        this.expression = expression;
    }



    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        MyHeapInterface<Value> heap = state.getHeap();

        if (!symbolTable.hasKey(this.variableName)) {
            throw new MyException(this.variableName + " was not declared");
        }

        Value value = symbolTable.get(this.variableName);

        if (!(value instanceof ReferenceValue referenceValue)) {
            throw new MyException("Expected RefType, got " + value.getType().toString());
        }

        int address = referenceValue.getAddress();
        if (!heap.exists(address)) {
            throw new MyException("No value at address " + address);
        }

        Value expressionValue = this.expression.evaluate(symbolTable, heap);
        if (!expressionValue.getType().equals(heap.get(address).getType())) {
            throw new MyException("Expected " + referenceValue.getType() + ", got " + expressionValue.getType());
        }

        heap.set(address, expressionValue);
        state.setHeap(heap);
        return state;
    }

    @Override
    public String toString() {
        return "writeHeap(" + variableName + ", " + expression.toString() + ")";
    }

    @Override
    public StatementInterface deepCopy() {
        return new WriteHeapStatement(variableName, expression.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeExp = this.expression.typeCheck(table);
        Type typeVar = table.get(this.variableName);

        if (!(typeVar instanceof ReferenceType referenceType)) {
            throw new MyException("Write heap: Variable not of ReferenceType");
        }

        if (!typeExp.equals(referenceType.getInner())) {
            throw new MyException("Error; can not modify heap to specified type");
        }

        return table;
    }
}
