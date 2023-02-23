package Controllers;

import Exceptions.FileExceptions;
import Model.Collections.MyDictionary;
import Model.Collections.MyHeap;
import Model.Collections.MyList;
import Model.Collections.MyStack;
import Model.ProgramState;
import Repository.Repository;
import Repository.RepositoryInterface;
import View.Commands.Command;
import View.Commands.RunCommand;
import View.Programs;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldListCell;

public class LoadController {

    private RunController runnerController;

    public ListView<Command> programsListView;
    public Button loadButton;

    private void loadProgram(RunCommand command) {
        runnerController.showProgram(command);
    }

    @FXML
    public void initialize() {

        this.loadButton.setOnAction(actionEvent -> {
            RunCommand command = (RunCommand) this.programsListView.getSelectionModel().getSelectedItem();
            if (command != null) {
                loadProgram(command);
            }
        });

        Programs programs = new Programs();

        try {
            // 0
            ProgramState programState0 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(0)
            );
            RepositoryInterface repository0 = new Repository("log0.txt");
            repository0.addProgram(programState0);
            Controller controller0 = new Controller(repository0);

            // 1
            ProgramState programState1 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(1)
            );
            RepositoryInterface repository1 = new Repository("log1.txt");
            repository1.addProgram(programState1);
            Controller controller1 = new Controller(repository1);

            // 2
            ProgramState programState2 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(2)
            );

            RepositoryInterface repository2 = new Repository("log2.txt");
            repository2.addProgram(programState2);
            Controller controller2 = new Controller(repository2);

            // 3
            ProgramState programState3 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(3)
            );
            RepositoryInterface repository3 = new Repository("log3.txt");
            repository3.addProgram(programState3);
            Controller controller3 = new Controller(repository3);

            // 4
            ProgramState programState4 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(4)
            );
            RepositoryInterface repository4 = new Repository("log4.txt");
            repository4.addProgram(programState4);
            Controller controller4 = new Controller(repository4);

            // 5
            ProgramState programState5 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(5)
            );
            RepositoryInterface repository5 = new Repository("log5.txt");
            repository5.addProgram(programState5);
            Controller controller5 = new Controller(repository5);

            // 6
            ProgramState programState6 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(6)
            );
            RepositoryInterface repository6 = new Repository("log6.txt");
            repository6.addProgram(programState6);
            Controller controller6 = new Controller(repository6);

            // 7
            ProgramState programState7 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(7)
            );
            RepositoryInterface repository7 = new Repository("log7.txt");
            repository7.addProgram(programState7);
            Controller controller7 = new Controller(repository7);

            // 8
            ProgramState programState8 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(8)
            );
            RepositoryInterface repository8 = new Repository("log8.txt");
            repository8.addProgram(programState8);
            Controller controller8 = new Controller(repository8);


            // 9
            ProgramState programState9 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(9)
            );
            RepositoryInterface repository9 = new Repository("log9.txt");
            repository9.addProgram(programState9);
            Controller controller9 = new Controller(repository9);

            // 10
            ProgramState programState10 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(10)
            );
            RepositoryInterface repository10 = new Repository("log10.txt");
            repository10.addProgram(programState10);
            Controller controller10 = new Controller(repository10);

            this.programsListView.setCellFactory(param -> new ListCell<Command>() {
                @Override
                protected void updateItem(Command item, boolean empty) {
                    super.updateItem(item, empty);

                    if (empty || item == null || item.getDescription() == null) {
                        setText(null);
                    } else {
                        setText(item.getDescription());
                    }
                }
            });

            // 11
            ProgramState programState11 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
                    new MyHeap<>(),
                    programs.get(11)
            );

            RepositoryInterface repository11 = new Repository("log11.txt");
            repository11.addProgram(programState11);
            Controller controller11 = new Controller(repository11);



            this.programsListView.getItems().add(new RunCommand(programs.get(0).toString(), controller0));
            this.programsListView.getItems().add(new RunCommand(programs.get(1).toString(), controller1));
            this.programsListView.getItems().add(new RunCommand(programs.get(2).toString(), controller2));
            this.programsListView.getItems().add(new RunCommand(programs.get(3).toString(), controller3));
            this.programsListView.getItems().add(new RunCommand(programs.get(4).toString(), controller4));
            this.programsListView.getItems().add(new RunCommand(programs.get(5).toString(), controller5));
            this.programsListView.getItems().add(new RunCommand(programs.get(6).toString(), controller6));
            this.programsListView.getItems().add(new RunCommand(programs.get(7).toString(), controller7));
            this.programsListView.getItems().add(new RunCommand(programs.get(8).toString(), controller8));
            this.programsListView.getItems().add(new RunCommand(programs.get(9).toString(), controller9));
            this.programsListView.getItems().add(new RunCommand(programs.get(10).toString(), controller10));
            this.programsListView.getItems().add(new RunCommand(programs.get(11).toString(), controller11));


            this.programsListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        } catch (FileExceptions exceptions) {
            System.out.println(exceptions.getMessage());
        }


    }

    public void setRunnerController(RunController runnerController) {
        this.runnerController = runnerController;
    }


}
