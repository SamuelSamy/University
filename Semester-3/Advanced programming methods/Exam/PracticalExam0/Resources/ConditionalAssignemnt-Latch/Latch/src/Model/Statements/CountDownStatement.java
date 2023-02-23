package Model.Statements;

import Exceptions.MyException;
import Model.Collections.LatchTableInterface;
import Model.Collections.MyDictionary;
import Model.Collections.MyDictionaryInterface;
import Model.Expressions.ValueExpression;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;

public class CountDownStatement implements StatementInterface {

    private final String variableName;

    public CountDownStatement(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {

        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        LatchTableInterface latchTable = state.getLatchTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("Variable " + variableName + " is not defined!");
        }

        int value = ((IntValue) symbolTable.get(variableName)).getValue();

        if (!latchTable.exists(value)) {
            throw new MyException("Latch " + value + " is not defined!");
        }

        if (latchTable.get(value) > 0) {
            latchTable.set(value, latchTable.get(value) - 1);
        }

        state.getExecutionStack().push(new PrintStatement(new ValueExpression(new IntValue(state.getCurrentId()))));
        return state;
    }


    @Override
    public StatementInterface deepCopy() {
        return new CountDownStatement(variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeVariable = table.get(variableName);

        if (!typeVariable.equals(new IntType())) {
            throw new MyException("CountDownStatement: variable " + variableName + " is not of type int!");
        }

        return table;
    }

    @Override
    public String toString() {
        return "countDown(" + variableName + ")";
    }
}
