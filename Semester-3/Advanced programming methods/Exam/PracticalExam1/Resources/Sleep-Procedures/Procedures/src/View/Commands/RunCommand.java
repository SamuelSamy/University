package View.Commands;

import Controllers.Controller;
import Exceptions.MyException;

public class RunCommand extends Command {
    private final Controller controller;

    public RunCommand(String description, Controller controller) {
        super(description);
        this.controller = controller;
    }

    @Override
    public void execute() {
        try {
            Controller copyController = controller.deepCopy();
            copyController.typeCheck();
            copyController.executeAllSteps();
        } catch (MyException exception) {
            System.out.println(exception.getMessage());
        }
    }


    @Override
    public Controller getController() {
        return this.controller;
    }
}
