package Repository;

import Exceptions.ADTException;
import Exceptions.MyException;
import Modal.Collections.MyList;
import Modal.ProgramState;

public class Repository implements RepositoryInterface {

    private final MyList<ProgramState> programs;

    public Repository() {
        this.programs = new MyList<>();
    }

    @Override
    public ProgramState getCurrentProgram() throws MyException {
        ProgramState current;
        try {
            current = this.programs.getAt(0);
        } catch (ADTException error) {
            throw new MyException("There are no programs available");
        }
        this.programs.removeAt(0);
        return current;
    }

    @Override
    public void addProgram(ProgramState program) {
        this.programs.add(program);
    }
}
