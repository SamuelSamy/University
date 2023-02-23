package Modal.Statements;

import Exceptions.MyException;
import Modal.ProgramState;

public interface StatementInterface {
    ProgramState execute(ProgramState state) throws MyException;

    StatementInterface deepCopy();
}
