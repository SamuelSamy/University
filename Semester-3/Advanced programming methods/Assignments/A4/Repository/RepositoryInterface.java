package Repository;

import Exceptions.FileExceptions;
import Exceptions.MyException;
import Model.ProgramState;

public interface RepositoryInterface {
    ProgramState getCurrentProgram() throws MyException;
    void logProgramStateExecution(ProgramState programState) throws MyException;

    int getProgramsCount();

    void addProgram(ProgramState program);
    RepositoryInterface deepCopy() throws MyException;

    ProgramState getTopProgram() throws MyException;
}
