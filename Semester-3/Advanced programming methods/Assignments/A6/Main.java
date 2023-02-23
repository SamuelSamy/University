import Controller.Controller;
import Exceptions.FileExceptions;
import Exceptions.MyException;
import Model.Collections.*;
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

            TextMenu menu = new TextMenu();
            menu.addCommand(new ExitCommand("0", "Exit"));
            menu.addCommand(new RunCommand("1", programs.get(0).toString(), controller0));
            menu.addCommand(new RunCommand("2", programs.get(1).toString(), controller1));
            menu.addCommand(new RunCommand("3", programs.get(2).toString(), controller2));
            menu.addCommand(new RunCommand("4", programs.get(3).toString(), controller3));
            menu.addCommand(new RunCommand("5", programs.get(4).toString(), controller4));
            menu.addCommand(new RunCommand("6", programs.get(5).toString(), controller5));
            menu.addCommand(new RunCommand("7", programs.get(6).toString(), controller6));
            menu.addCommand(new RunCommand("8", programs.get(7).toString(), controller7));
            menu.addCommand(new RunCommand("9", programs.get(8).toString(), controller8));
            menu.addCommand(new RunCommand("10", programs.get(9).toString(), controller9));
            menu.run();

        } catch (FileExceptions exceptions) {
            System.out.println(exceptions.getMessage());
        }
    }

}
