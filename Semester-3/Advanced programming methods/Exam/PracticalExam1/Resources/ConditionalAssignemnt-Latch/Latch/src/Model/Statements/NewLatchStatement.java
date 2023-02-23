package Model.Statements;

import Exceptions.MyException;
import Model.Collections.LatchTableInterface;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;

public class NewLatchStatement implements StatementInterface {

    private final String variableName;
    private final Expression expression;

    public NewLatchStatement(String variableName, Expression expression) {
        this.variableName = variableName;
        this.expression = expression;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {

        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        MyHeapInterface<Value> heap = state.getHeap();
        LatchTableInterface latchTable = state.getLatchTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("Variable " + variableName + " is not defined!");
        }

        int value = ((IntValue) this.expression.evaluate(symbolTable, heap)).getValue();
        int freeAddress = latchTable.getFreeAddress();

        latchTable.put(freeAddress, value);
        symbolTable.set(variableName, new IntValue(freeAddress));

        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new NewLatchStatement(variableName, expression.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeVariable = table.get(variableName);
        Type typeExpression = expression.typeCheck(table);

        if (!typeVariable.equals(new IntValue(0).getType())) {
            throw new MyException("NewLatchStatement: variable " + variableName + " is not of type int!");
        }

        if (!typeExpression.equals(new IntValue(0).getType())) {
            throw new MyException("NewLatchStatement: expression " + expression + " is not of type int!");
        }

        return table;
    }

    @Override
    public String toString() {
        return "newLatch(" + variableName + ", " + expression.toString() + ")";
    }
}
