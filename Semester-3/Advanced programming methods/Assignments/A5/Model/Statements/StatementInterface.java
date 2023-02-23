package Model.Statements;

import Exceptions.MyException;
import Model.ProgramState;

public interface StatementInterface {
    ProgramState execute(ProgramState state) throws MyException;

    StatementInterface deepCopy();
}
