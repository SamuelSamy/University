package Controller;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeap;
import Model.Collections.MyHeapInterface;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Statements.StatementInterface;
import Model.Values.ReferenceValue;
import Model.Values.Value;
import Repository.RepositoryInterface;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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

            // garbage collection
            program.getHeap().setHeap(
                safeGarbageCollector(
                    program.getSymbolTable(),
                    program.getHeap()
                )
            );

            this.displayStep(program);
        }

        if (!displayFlag) {
            System.out.println(program);
        }
    }

    private Map<Integer, Value> unsafeGarbageCollector(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> myHeap) {

        List<Integer> addresses = symbolTable.values().stream()
                .filter(v -> v instanceof ReferenceValue)
                .map(v -> ((ReferenceValue) v).getAddress())
                .toList();

        Map<Integer, Value> heap = myHeap.getHeap();
        return heap.entrySet().stream()
                .filter(e -> addresses.contains(e.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }


    private Map<Integer, Value> safeGarbageCollector(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> myHeap) {
        Map<Integer, Value> heap = myHeap.getHeap();

        List<Integer> addressesFromSymbolTable = symbolTable.values().stream()
                .filter(v -> v instanceof ReferenceValue)
                .map(v -> ((ReferenceValue) v).getAddress())
                .toList();

        List<Integer> addressesFromHeap = heap.values().stream()
                .filter(v -> v instanceof ReferenceValue)
                .map(v -> ((ReferenceValue) v).getAddress())
                .toList();

        return heap.entrySet().stream()
                .filter(e -> addressesFromSymbolTable.contains(e.getKey()) || addressesFromHeap.contains(e.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
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
