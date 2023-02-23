package Model;

import Exceptions.MyException;
import Model.Collections.*;
import Model.Statements.StatementInterface;
import Model.Values.Value;

import java.io.BufferedReader;

public class ProgramState {
    private MyStackInterface<StatementInterface> executionStack;
    private MyDictionaryInterface<String, Value> symbolTable;
    private MyListInterface<Value> output;
    private MyDictionaryInterface<String, BufferedReader> fileTable;

    private MyHeapInterface<Value> heap;

    StatementInterface originalProgram;

    public ProgramState(
            MyStackInterface<StatementInterface> stack,
            MyDictionaryInterface<String, Value> table,
            MyListInterface<Value> list,
            MyDictionaryInterface<String, BufferedReader> fileTable,
            MyHeapInterface<Value> heap,
            StatementInterface program
    ) {
        this.executionStack = stack;
        this.symbolTable = table;
        this.output = list;
        this.fileTable = fileTable;
        this.heap = heap;
        this.originalProgram = program.deepCopy();
        this.executionStack.push(program);
    }

    public MyStackInterface<StatementInterface> getExecutionStack() {
        return executionStack;
    }

    public MyDictionaryInterface<String, Value> getSymbolTable() {
        return symbolTable;
    }

    public MyListInterface<Value> getOutput() {
        return output;
    }

    public MyHeapInterface<Value> getHeap() {
        return heap;
    }

    public MyDictionaryInterface<String, BufferedReader> getFileTable() {
        return fileTable;
    }

    public void setExecutionStack(MyStackInterface<StatementInterface> executionStack) {
        this.executionStack = executionStack;
    }

    public void setSymbolTable(MyDictionaryInterface<String, Value> symbolTable) {
        this.symbolTable = symbolTable;
    }

    public void setOutput(MyListInterface<Value> output) {
        this.output = output;
    }

    public void setOriginalProgram(StatementInterface originalProgram) {
        this.originalProgram = originalProgram;
    }

    public void setHeap(MyHeapInterface<Value> heap) {
        this.heap = heap;
    }

    public void setFileTable(MyDictionaryInterface<String, BufferedReader> fileTable) {
        this.fileTable = fileTable;
    }

    public StatementInterface getOriginalProgram() {
        return this.originalProgram;
    }




    public ProgramState deepCopy() throws MyException {
        return new ProgramState(
                new MyStack<>(),
                new MyDictionary<>(),
                new MyList<>(),
                new MyDictionary<>(),
                new MyHeap<>(),
                this.originalProgram.deepCopy()
        );
    }

    @Override
    public String toString() {
        return ">>Program State:\n" +
                "executionStack = " + executionStack +
                "\nsymbolTable = " + symbolTable +
                "\noutput = " + output +
                "\nfiles = " + fileTable +
                "\nheap = " + heap + "\n\n";
    }
}
