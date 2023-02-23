package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.ProgramState;
import Model.Types.Type;

public interface StatementInterface {
    ProgramState execute(ProgramState state) throws MyException;
    StatementInterface deepCopy();
    MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException;
}
