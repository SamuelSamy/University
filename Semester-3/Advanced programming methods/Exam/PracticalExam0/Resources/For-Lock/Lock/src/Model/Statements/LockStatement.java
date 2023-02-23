package Model.Statements;

import Exceptions.MyException;
import Model.Collections.LockTableInterface;
import Model.Collections.MyDictionaryInterface;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;

public class LockStatement implements StatementInterface {

    private final String variableName;

    public LockStatement(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        LockTableInterface lockTable = state.getLockTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("LockStatement: Variable does not exist in symbol table");
        }

        if (!symbolTable.get(variableName).getType().equals(new IntType())) {
            throw new MyException("LockStatement: Variable is not an integer");
        }

        int address = ((IntValue)symbolTable.get(variableName)).getValue();

        if (!lockTable.contains(address)) {
            throw new MyException("LockStatement: Address does not exist in lock table");
        }

        if (lockTable.get(address) == -1) {
            lockTable.set(address, state.getCurrentId());
            state.setLockTable(lockTable);
            return state;
        }

        state.getExecutionStack().push(this);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new LockStatement(variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type = table.get(variableName);

        if (!type.equals(new IntType())) {
            throw new MyException("LockStatement: Variable is not an integer");
        }

        return table;
    }

    @Override
    public String toString() {
        return "lock(" + variableName + ")";
    }
}
