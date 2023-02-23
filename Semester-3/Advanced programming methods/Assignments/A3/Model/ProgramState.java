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

    StatementInterface originalProgram;

    public ProgramState(
            MyStackInterface<StatementInterface> stack,
            MyDictionaryInterface<String, Value> table,
            MyListInterface<Value> list,
            MyDictionaryInterface<String, BufferedReader> fileTable,
            StatementInterface program
    ) {
        this.executionStack = stack;
        this.symbolTable = table;
        this.output = list;
        this.fileTable = fileTable;
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

    public StatementInterface getOriginalProgram() {
        return this.originalProgram;
    }

    public MyDictionaryInterface<String, BufferedReader> getFileTable() {
        return fileTable;
    }

    public void setFileTable(MyDictionaryInterface<String, BufferedReader> fileTable) {
        this.fileTable = fileTable;
    }

    public ProgramState deepCopy() throws MyException {
        return new ProgramState(
                new MyStack<>(),
                new MyDictionary<>(),
                new MyList<>(),
                new MyDictionary<>(),
                this.originalProgram.deepCopy()
        );
    }

    @Override
    public String toString() {
        return ">>Program State:\n" +
                "executionStack = " + executionStack +
                "\nsymbolTable = " + symbolTable +
                "\noutput = " + output +
                "\nfiles = " + fileTable + "\n\n";
    }
}
