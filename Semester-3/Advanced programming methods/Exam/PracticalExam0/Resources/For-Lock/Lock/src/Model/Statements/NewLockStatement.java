package Model.Statements;

import Exceptions.MyException;
import Model.Collections.LockTable;
import Model.Collections.LockTableInterface;
import Model.Collections.MyDictionaryInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;

public class NewLockStatement implements StatementInterface{

    private final String variableName;

    public NewLockStatement(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        LockTableInterface lockTable = state.getLockTable();
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("NewLockStatement: Variable does not exist in symbol table");
        }

        if (!symbolTable.get(variableName).getType().equals(new IntType())) {
            throw new MyException("NewLockStatement: Variable is not an integer");
        }

        int freeAddress = lockTable.getFreeAddress();
        lockTable.put(freeAddress, -1);
        symbolTable.set(variableName, new IntValue(freeAddress));

        return state;

    }

    @Override
    public StatementInterface deepCopy() {
        return new NewLockStatement(variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type = table.get(variableName);

        if (!type.equals(new IntType())) {
            throw new MyException("NewLockStatement: Variable is not an integer");
        }
        return table;
    }

    @Override
    public String toString() {
        return "newLock(" + variableName + ")";
    }
}
