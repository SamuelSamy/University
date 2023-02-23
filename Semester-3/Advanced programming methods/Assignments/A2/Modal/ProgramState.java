package Modal;

import Modal.Collections.MyDictionaryInterface;
import Modal.Collections.MyListInterface;
import Modal.Collections.MyStackInterface;
import Modal.Statements.StatementInterface;
import Modal.Values.Value;

public class ProgramState {
    MyStackInterface<StatementInterface> executionStack;
    MyDictionaryInterface<String, Value> symbolTable;
    MyListInterface<Value> output;

    StatementInterface originalProgram;

    public ProgramState(MyStackInterface<StatementInterface> stack, MyDictionaryInterface<String, Value> table, MyListInterface<Value> list, StatementInterface program){
        this.executionStack = stack;
        this.symbolTable = table;
        this.output = list;
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

    @Override
    public String toString() {
        return ">>Program State:\n" +
                "executionStack = " + executionStack +
                "\nsymbolTable = " + symbolTable +
                "\noutput = " + output + "\n\n";
    }
}
