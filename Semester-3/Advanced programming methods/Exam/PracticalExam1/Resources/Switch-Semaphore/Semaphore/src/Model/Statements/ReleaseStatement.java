package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.SemaphoreTableInterface;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;
import javafx.util.Pair;

import java.util.List;

public class ReleaseStatement implements StatementInterface{

    private final String variableName;

    public ReleaseStatement(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("Variable " + variableName + " is not defined");
        }

        if (!(symbolTable.get(variableName)).getType().equals(new IntType())) {
            throw new MyException("Variable " + variableName + " is not an integer");
        }

        SemaphoreTableInterface semaphoreTable = state.getSemaphoreTable();

        int index = ((IntValue) symbolTable.get(variableName)).getValue();
        if (!semaphoreTable.exists(index)) {
            throw new MyException("Acquire: Index " + index + " is not defined in the semaphore table");
        }

        Pair<Integer, List<Integer>> semaphore = semaphoreTable.get(index);

        if (semaphore.getValue().contains(state.getCurrentId())) {
            semaphore.getValue().remove((Integer) state.getCurrentId());
        }

        semaphoreTable.set(index, new Pair<>(semaphore.getKey(), semaphore.getValue()));

        return null;
    }

    @Override
    public StatementInterface deepCopy() {
        return new ReleaseStatement(variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeVariable = table.get(variableName);

        if (!typeVariable.equals(new IntType())) {
            throw new MyException("Release: Variable " + variableName + " is not an integer");
        }

        return table;
    }

    @Override
    public String toString() {
        return "release(" + variableName + ")";
    }
}
