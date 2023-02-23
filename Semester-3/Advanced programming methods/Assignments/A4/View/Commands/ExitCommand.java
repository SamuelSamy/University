package View.Commands;

public class ExitCommand extends Command {

    public ExitCommand(String key, String description) {
        super(key, description);
    }

    @Override
    public void execute() {
        System.out.println("Exiting application...");
        System.exit(0);
    }
}
