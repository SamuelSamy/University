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
}
