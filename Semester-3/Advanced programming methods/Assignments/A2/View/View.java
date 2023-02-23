package View;

import Controller.Controller;
import Exceptions.MyException;
import Modal.Collections.MyDictionary;
import Modal.Collections.MyList;
import Modal.Collections.MyListInterface;
import Modal.Collections.MyStack;
import Modal.Expressions.ArithmeticalExpression;
import Modal.Expressions.ValueExpression;
import Modal.Expressions.VariableExpression;
import Modal.ProgramState;
import Modal.Statements.*;
import Modal.Types.BoolType;
import Modal.Types.IntType;
import Modal.Values.BoolValue;
import Modal.Values.IntValue;

import java.util.Scanner;

public class View {
    private final Controller controller;
    private final MyListInterface<StatementInterface> programs;

    public View(Controller controller) {
        this.controller = controller;
        this.programs = new MyList<>();
        this.initializeData();
    }

    public void printMenu() {
        System.out.println("Display flag: " + this.controller.getDisplayFlag());
        System.out.println("1. Toggle display flag");
        System.out.println("2. See all programs");
        System.out.println("3. Execute a program");
        System.out.println("0. Exit");
    }

    private int readInteger(String message) {
        int number = 0;
        boolean valid = false;

        do {
            System.out.println(message);

            try {
                Scanner readOption = new Scanner(System.in);
                number = readOption.nextInt();
                valid = true;
            } catch (Exception e) {
                System.out.println("Invalid integer!");
            }

        } while (!valid);

        return number;
    }

    public void run() {

        int option;

        while (true) {
            this.printMenu();

            option = readInteger(">> Enter an option");

            if (option == 0) {
                break;
            }

            try {
                if (option == 1) {
                    this.controller.toggleDisplay();
                    System.out.println("The display flag has been set to " + this.controller.getDisplayFlag());
                    continue;
                }

                if (option == 2) {
                    this.showAllPrograms();
                    continue;
                }

                if (option == 3) {
                    this.executeProgram();
                    continue;
                }


                System.out.println("Invalid option!\n");

            } catch (MyException e) {
                System.out.println(e.getMessage());
            }
        }

        System.out.println("Exiting...");
    }

    private void showAllPrograms() {
        System.out.println("\n0 - int v; v = 2; print(v)");
        System.out.println("1 - int a; int b; a = 2 + 3 * 5; b = a + 1; print(b)");
        System.out.println("2 - bool a; int v; a = true; (if a then v = 2 else v = 3); print(v)\n");
    }

    private void executeProgram() throws MyException {
        int programIndex = readInteger(">> Enter program index");
        if (programIndex < 0 || programIndex >= this.programs.size()) {
            System.out.println("Invalid index\n");
            return;
        }

        StatementInterface program = this.programs.getAt(programIndex);
        try {
            this.createProgram(program);
            this.controller.executeAllSteps();
        } catch (MyException e) {
            System.out.println(e.getMessage());
        }
    }

    private void createProgram(StatementInterface statement) {
        ProgramState program = new ProgramState(
                new MyStack<>(),
                new MyDictionary<>(),
                new MyList<>(),
                statement
        );
        this.controller.addProgram(program);
    }


    private void initializeData() {
        StatementInterface program0 = new CompoundStatement(
                new VariableDeclarationStatement("v", new IntType()),
                new CompoundStatement(
                        new AssignmentStatement("v", new ValueExpression(new IntValue(2))),
                        new PrintStatement(new VariableExpression("v"))
                )
        );

        StatementInterface program1 = new CompoundStatement(
            new VariableDeclarationStatement("a", new IntType()),
            new CompoundStatement(
                    new VariableDeclarationStatement("b", new IntType()),
                    new CompoundStatement(
                            new AssignmentStatement(
                                    "a",
                                    new ArithmeticalExpression(
                                           '+',
                                            new ValueExpression(new IntValue(2)),
                                            new ArithmeticalExpression(
                                                    '*',
                                                    new ValueExpression(new IntValue(3)),
                                                    new ValueExpression(new IntValue(5))
                                            )
                                    )
                            ),
                            new CompoundStatement(
                                    new AssignmentStatement(
                                            "b",
                                            new ArithmeticalExpression(
                                                    '+',
                                                    new VariableExpression("a"),
                                                    new ValueExpression(new IntValue(1))
                                            )
                                    ),
                                    new PrintStatement(new VariableExpression("b"))
                            )
                    )
            )
        );

        StatementInterface program2 = new CompoundStatement(
                new VariableDeclarationStatement("a", new BoolType()),
                new CompoundStatement(
                        new VariableDeclarationStatement("v", new IntType()),
                        new CompoundStatement(
                                new AssignmentStatement(
                                        "a",
                                        new ValueExpression(new BoolValue(true))
                                ),
                                new CompoundStatement(
                                        new ConditionalStatement(
                                                new VariableExpression("a"),
                                                new AssignmentStatement(
                                                        "v",
                                                        new ValueExpression(new IntValue(2))
                                                ),
                                                new AssignmentStatement(
                                                        "v",
                                                        new ValueExpression((new IntValue(3)))
                                                )
                                        ),
                                        new PrintStatement(new VariableExpression("v"))
                                )
                        )
                )
        );

        this.programs.add(program0);
        this.programs.add(program1);
        this.programs.add(program2);
    }
}
