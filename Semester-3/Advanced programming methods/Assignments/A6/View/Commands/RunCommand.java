package View.Commands;

import Controller.Controller;
import Exceptions.MyException;

public class RunCommand extends Command {
    private final Controller controller;

    public RunCommand(String key, String description, Controller controller) {
        super(key, description);
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
}
