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

import java.io.ObjectStreamException;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Controller {
    private final RepositoryInterface repo;
    boolean displayFlag;

    public ExecutorService executor;

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


    public List<ProgramState> removeCompletedPrograms(List<ProgramState> inputProgramList) {
        return inputProgramList.stream().
                filter(ProgramState::isNotCompleted)
                .collect(Collectors.toList());
    }



    public void executeAllSteps() throws MyException {
        executor = Executors.newFixedThreadPool(2);
        List<ProgramState> programs = this.removeCompletedPrograms(repo.getProgramsList());

        programs.forEach(
                program -> {
                    try {
                        this.displayStep(program);
                    } catch (MyException e) {
                        e.printStackTrace();
                    }
                }
        );

        while (programs.size() > 0) {
            this.conservativeGarbageCollector(programs);
            this.executeOneStepForAllPrograms(programs);
            programs = this.removeCompletedPrograms(repo.getProgramsList());
        }

        executor.shutdownNow();

        this.repo.setProgramsList(programs);
    }

    public void executeOneStepForAllPrograms(List<ProgramState> programs) {

        List<Callable<ProgramState>> callList = programs.stream()
                .map((ProgramState program) -> (Callable<ProgramState>) (program::executeOneStep))
                .collect(Collectors.toList());

        try {
            List<ProgramState> newPrograms = executor.invokeAll(callList).stream()
                    .map(future -> {
                        try {
                            return future.get();
                        } catch (InterruptedException | ExecutionException e) {
                            throw new RuntimeException(e);
                        }
                    })
                    .filter(Objects::nonNull).toList();

            programs.addAll(newPrograms);
            programs = programs.stream()
                    .distinct()
                    .collect(Collectors.toList());
        }
        catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        programs.forEach(program -> {
            try {
                this.displayStep(program);
            } catch (MyException e) {
                throw new RuntimeException(e);
            }
        });

        this.repo.setProgramsList(programs);
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


    private List<Integer> getAddressesFromSymbolTableAndHeap(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) {
        return Stream.concat(
            symbolTable.values().stream()
                    .filter(v -> v instanceof ReferenceValue)
                    .map(v -> ((ReferenceValue) v).getAddress()),
            heap.getHeap().values().stream()
                    .filter(v -> v instanceof ReferenceValue)
                    .map(v -> ((ReferenceValue) v).getAddress())
        ).collect(Collectors.toList());
    }


    private Map<Integer, Value> safeGarbageCollector(List<Integer> addresses, MyHeapInterface<Value> myHeap) {
        return myHeap.getHeap().entrySet().stream()
                .filter(e -> addresses.contains(e.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    public void conservativeGarbageCollector(List<ProgramState> programs) {
        // one HEAP shared by multiple PrgStates and multiple SymbolTables

        List<Integer> addresses = Objects.requireNonNull(
                programs.stream()
                        .map(program -> getAddressesFromSymbolTableAndHeap(program.getSymbolTable(), program.getHeap()))
                        .map(Collection::stream)
                        .reduce(Stream::concat).orElse(null)
        ).toList();

        programs.forEach(
                program -> program.getHeap().setHeap(
                    safeGarbageCollector(addresses, programs.get(0).getHeap())
                )
        );
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
