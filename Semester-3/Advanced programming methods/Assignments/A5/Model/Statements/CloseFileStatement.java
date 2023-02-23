package Model.Statements;

import Exceptions.FileExceptions;
import Exceptions.MyException;
import Exceptions.StatementException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.StringType;
import Model.Values.StringValue;
import Model.Values.Value;

import java.io.BufferedReader;
import java.io.IOException;

public class CloseFileStatement implements StatementInterface {
    private final Expression expression;

    public CloseFileStatement(Expression expression) {
        this.expression = expression;
    }

    @Override
    public String toString() {
        return "closeReadFile(" + this.expression + ")";
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        MyDictionaryInterface<String, BufferedReader> fileTable = state.getFileTable();
        MyHeapInterface<Value> heap = state.getHeap();

        Value value = this.expression.evaluate(symbolTable, heap);

        if (!value.getType().equals(new StringType())) {
            throw new StatementException("closeReadFile: expected StringType; got " + value.getType());
        }

        StringValue stringValue = (StringValue) value;

        if (!fileTable.hasKey(stringValue.getValue())) {
            throw new FileExceptions("File does not exist - " + stringValue.getValue());
        }

        try {
            BufferedReader reader = fileTable.get(stringValue.getValue());
            reader.close();
            fileTable.remove(stringValue.getValue());
            state.setFileTable(fileTable);
        } catch (IOException e) {
            throw new FileExceptions("File can not be close - " + stringValue.getValue() + "\n" + e.getMessage());
        }

        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new CloseFileStatement(expression.deepCopy());
    }
}
