package View.Commands;

import Controllers.Controller;
import Model.ProgramState;

import java.util.List;

public abstract class Command {
    private final String description;

    public Command(String description) {
        this.description = description;
    }

    public abstract void execute();


    public String getDescription() {
        return this.description;
    }

    public abstract Controller getController();
}
