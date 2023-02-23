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

public class AcquireStatement implements StatementInterface {

    private final String variableName;

    public AcquireStatement(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        SemaphoreTableInterface semaphoreTable = state.getSemaphoreTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("Acquire: Variable " + variableName + " is not defined");
        }

        if (!(symbolTable.get(variableName).getType().equals(new IntType()))) {
            throw new MyException("Acquire: Variable " + variableName + " is not an integer");
        }

        int index = ((IntValue) symbolTable.get(variableName)).getValue();

        if (!semaphoreTable.exists(index)) {
            throw new MyException("Acquire: Index " + index + " is not defined in the semaphore table");
        }

        Pair<Integer, List<Integer>> semaphore = semaphoreTable.get(index);
        int nl = semaphore.getValue().size();
        int n1 = semaphore.getKey();

        if (n1 <= nl) {
            state.getExecutionStack().push(this);
            return null;
        }

        if (!semaphore.getValue().contains(state.getCurrentId())) {
            semaphore.getValue().add(state.getCurrentId());
            semaphoreTable.set(index, new Pair<>(n1, semaphore.getValue()));
        }

        return null;
    }

    @Override
    public StatementInterface deepCopy() {
        return new AcquireStatement(variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type = table.get(variableName);
        if (!type.equals(new IntType())) {
            throw new MyException("Variable " + variableName + " is not an integer");
        }

        return table;
    }

    @Override
    public String toString() {
        return "acquire(" + variableName + ")";
    }
}
