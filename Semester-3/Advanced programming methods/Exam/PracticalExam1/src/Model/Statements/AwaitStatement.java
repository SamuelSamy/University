package Model.Statements;

import Exceptions.MyException;
import Model.Collections.LatchTableInterface;
import Model.Collections.MyDictionaryInterface;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;

public class AwaitStatement implements StatementInterface {

    private final String variableName;

    public AwaitStatement(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        LatchTableInterface latchTable = state.getLatchTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("Variable " + variableName + " is not defined!");
        }

        if (!symbolTable.get(variableName).getType().equals(new IntType())) {
            throw new MyException("Variable " + variableName + " is not of type int!");
        }


        int value = ((IntValue) symbolTable.get(variableName)).getValue();

        if (!latchTable.exists(value)) {
            throw new MyException("Latch " + value + " is not defined!");
        }

        if (latchTable.get(value) > 0) {
            state.getExecutionStack().push(this);
        }

        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new AwaitStatement(variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeVariable = table.get(variableName);

        if (!typeVariable.equals(new IntType())) {
            throw new MyException("AwaitStatement: variable " + variableName + " is not of type int!");
        }

        return table;
    }

    @Override
    public String toString() {
        return "await(" + variableName + ")";
    }
}
