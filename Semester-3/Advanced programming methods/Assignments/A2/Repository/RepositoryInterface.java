package Repository;

import Exceptions.MyException;
import Modal.ProgramState;

public interface RepositoryInterface {
    ProgramState getCurrentProgram() throws MyException;
    void addProgram(ProgramState program);
}
