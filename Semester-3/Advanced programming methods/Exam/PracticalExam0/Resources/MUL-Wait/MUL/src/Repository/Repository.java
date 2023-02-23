package Repository;

import Exceptions.ADTException;
import Exceptions.FileExceptions;
import Exceptions.MyException;
import Model.Collections.MyList;
import Model.ProgramState;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

public class Repository implements RepositoryInterface {

    private final MyList<ProgramState> programs;
    private final String filePath;

    public Repository(String filePath) throws FileExceptions {
        try {
            // test if the file exists and empty it
            PrintWriter testWriter = new PrintWriter(new BufferedWriter(new FileWriter(filePath, false)));
        } catch (IOException exception) {
            throw new FileExceptions(exception.getMessage());
        }

        this.programs = new MyList<>();
        this.filePath = filePath;
    }

    @Override
    public void logProgramStateExecution(ProgramState programState) throws MyException {
        PrintWriter fileWriter;

        try {
            fileWriter = new PrintWriter(new BufferedWriter(new FileWriter(this.filePath, true)));
        } catch (IOException exception) {
            throw new FileExceptions(exception.getMessage());
        }

        fileWriter.println(programState.toString());
        fileWriter.close();
    }


    @Override
    public int getProgramsCount() {
        return this.programs.size();
    }

    @Override
    public MyList<ProgramState> getPrograms() {
        return this.programs;
    }

    @Override
    public void setPrograms(MyList<ProgramState> programs) throws MyException {
        this.programs.clear();
        for (int i = 0; i < programs.size(); i++) {
            this.programs.add(programs.getAt(i));
        }
    }

    @Override
    public void addProgram(ProgramState program) {
        this.programs.add(program);
    }

    @Override
    public RepositoryInterface deepCopy() throws MyException {
        RepositoryInterface newRepo = new Repository(this.filePath);

        for (int i = 0; i < this.programs.size(); i++){
            newRepo.addProgram(this.programs.getAt(i).deepCopy());
        }


        return newRepo;
    }

    @Override
    public ProgramState getTopProgram() throws MyException {
        return this.programs.getAt(0);
    }

    @Override
    public void setProgramsList(List<ProgramState> programs) {
        this.programs.clear();
        for (ProgramState program : programs) {
            this.programs.add(program);
        }
    }

    @Override
    public List<ProgramState> getProgramsList() {
        return this.programs.getList();
    }
}
