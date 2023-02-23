package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.ProgramState;
import Model.Types.Type;

public class ReturnStatement implements StatementInterface {
    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        state.getFullSymbolTable().pop();
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new ReturnStatement();
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        return table;
    }

    @Override
    public String toString() {
        return "return";
    }
}
