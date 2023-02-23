package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionary;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyStack;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Types.Type;
import Model.Values.Value;

import java.util.ArrayList;

public class ForkStatement implements StatementInterface {

    private final StatementInterface statement;

    public ForkStatement(StatementInterface statement) {
        this.statement = statement;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyStackInterface<MyDictionaryInterface<String, Value>> oldSymbolTable = state.getFullSymbolTable();

        // deep copy la buget; asta este, atat s-a putut
        ArrayList<MyDictionaryInterface<String, Value>> list = oldSymbolTable.toList();
        MyStackInterface<MyDictionaryInterface<String, Value>> newSymbolTable = new MyStack<>();
        for (MyDictionaryInterface<String, Value> table : list) {
            newSymbolTable.push(table.deepCopy());
        }

        ProgramState newProgramState = new ProgramState(new MyStack<>(), newSymbolTable, state.getOutput(), state.getFileTable(), state.getHeap(), state.getProceduresTable(),this.statement.deepCopy());
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
