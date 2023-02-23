import Controller.Controller;
import Exceptions.FileExceptions;
import Exceptions.MyException;
import Model.Collections.MyDictionary;
import Model.Collections.MyList;
import Model.Collections.MyStack;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Statements.StatementInterface;
import Repository.Repository;
import Repository.RepositoryInterface;
import View.Commands.ExitCommand;
import View.Commands.RunCommand;
import View.Programs;
import View.TextMenu;


public class Main {
    public static void main(String[] args) {
        Programs programs = new Programs();

        try {
            // 0
            ProgramState programState0 = new ProgramState(
                    new MyStack<>(),
                    new MyDictionary<>(),
                    new MyList<>(),
                    new MyDictionary<>(),
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
                    programs.get(3)
            );
            RepositoryInterface repository3 = new Repository("log3.txt");
            repository3.addProgram(programState3);
            Controller controller3 = new Controller(repository3);

            TextMenu menu = new TextMenu();
            menu.addCommand(new ExitCommand("0", "Exit"));
            menu.addCommand(new RunCommand("1", programs.get(0).toString(), controller0));
            menu.addCommand(new RunCommand("2", programs.get(1).toString(), controller1));
            menu.addCommand(new RunCommand("3", programs.get(2).toString(), controller2));
            menu.addCommand(new RunCommand("4", programs.get(3).toString(), controller3));

            menu.run();

        } catch (FileExceptions exceptions) {
            System.out.println(exceptions.getMessage());
        }
    }

}
