package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyStack;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Types.Type;
import Model.Values.Value;

public class ForkStatement implements StatementInterface {

    private final StatementInterface statement;

    public ForkStatement(StatementInterface statement) {
        this.statement = statement;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> newSymbolTable = state.getSymbolTable().deepCopy();
        ProgramState newProgramState = new ProgramState(new MyStack<>(), newSymbolTable, state.getOutput(), state.getFileTable(), state.getHeap(), this.statement.deepCopy());
        newProgramState.setId();
        return newProgramState;
    }

    @Override
    public StatementInterface deepCopy() {
        return new ForkStatement(this.statement.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        this.statement.typeCheck(table.deepCopy());
        return table;
    }

    @Override
    public String toString() {
        return "fork(" + this.statement.toString() + ")";
    }
}
