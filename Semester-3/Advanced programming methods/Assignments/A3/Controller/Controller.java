package Controller;

import Exceptions.MyException;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Statements.StatementInterface;
import Repository.RepositoryInterface;

public class Controller {
    private final RepositoryInterface repo;
    boolean displayFlag;


    public Controller(RepositoryInterface repo) {
        this.repo = repo;
        this.displayFlag = true;
    }


    public void toggleDisplay() {
        this.displayFlag = !this.displayFlag;
    }


    public boolean getDisplayFlag() {
        return this.displayFlag;
    }


    public void addProgram(ProgramState program) {
        this.repo.addProgram(program);
    }


    public void executeOneStep(ProgramState program) throws MyException {
        MyStackInterface<StatementInterface> stack = program.getExecutionStack();

        if (stack.isEmpty()) {
            throw new MyException("Execution stack is empty");
        }

        StatementInterface currentStatement = stack.pop();
        currentStatement.execute(program);
    }


    public void executeAllSteps() throws MyException {
        ProgramState program = this.repo.getCurrentProgram();

        this.displayStep(program);
        while (!program.getExecutionStack().isEmpty()) {
            try {
                this.executeOneStep(program);
            } catch (Exception e) {
                throw new MyException(e.getMessage());
            }
            this.displayStep(program);
        }

        if (!displayFlag) {
            System.out.println(program);
        }
    }


    public void displayStep(ProgramState program) throws MyException {
        if (this.displayFlag) {
            System.out.println(program);
        }
        repo.logProgramStateExecution(program);
    }

    public Controller deepCopy() throws MyException{
        return new Controller(this.repo.deepCopy());
    }

}
