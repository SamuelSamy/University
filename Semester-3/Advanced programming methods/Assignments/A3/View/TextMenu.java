package View;

import View.Commands.Command;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class TextMenu {
    private Map<String, Command> commands;

    public TextMenu() {
        this.commands = new HashMap<>();
    }

    public void addCommand(Command command) {
        this.commands.put(command.getKey(), command);
    }

    private void printMenu() {
        for (Command command : commands.values()) {
            String line = String.format("%4s: %s", command.getKey(), command.getDescription());
            System.out.println(line);
        }
    }

    public void run() {
        Scanner scanner = new Scanner(System.in);
        while (true) {
            this.printMenu();
            String key = scanner.nextLine();
            Command command = this.commands.get(key);

            if (command == null) {
                System.out.println("Invalid option!");
                continue;
            }

            command.execute();
        }
    }
}
