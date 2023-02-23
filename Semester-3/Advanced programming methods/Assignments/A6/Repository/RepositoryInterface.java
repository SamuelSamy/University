package Repository;

import Exceptions.FileExceptions;
import Exceptions.MyException;
import Model.Collections.MyList;
import Model.ProgramState;

import java.util.List;

public interface RepositoryInterface {
    void logProgramStateExecution(ProgramState programState) throws MyException;

    int getProgramsCount();

    MyList<ProgramState> getPrograms();

    void setPrograms(MyList<ProgramState> programs) throws MyException;

    void addProgram(ProgramState program);
    RepositoryInterface deepCopy() throws MyException;

    ProgramState getTopProgram() throws MyException;

    void setProgramsList(List<ProgramState> programs);

    List<ProgramState> getProgramsList();
}
