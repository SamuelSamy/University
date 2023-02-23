package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.ProgramState;
import Model.Types.Type;

public class NoOperationStatement implements StatementInterface{

    public NoOperationStatement() {

    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new NoOperationStatement();
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        return table;
    }

    @Override
    public String toString() {
        return "\n";
    }
}
