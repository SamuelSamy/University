package Modal.Statements;

import Exceptions.MyException;
import Modal.ProgramState;

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
    public String toString() {
        return "\n";
    }
}
