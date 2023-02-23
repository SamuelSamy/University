package Model.Statements;

import Exceptions.FileExceptions;
import Exceptions.MyException;
import Exceptions.StatementException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Collections.MyStackInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.StringType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.StringValue;
import Model.Values.Value;

import java.io.BufferedReader;
import java.io.IOException;

public class ReadFileStatement implements StatementInterface{

    private final Expression expression;
    private final String variableName;

    public ReadFileStatement(Expression expression, String variableName) {
        this.expression = expression;
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getCurrentSymbolTable();
        MyStackInterface<MyDictionaryInterface<String, Value>> fullTable = state.getFullSymbolTable();
        MyDictionaryInterface<String, BufferedReader> fileTable = state.getFileTable();
        MyHeapInterface<Value> heap = state.getHeap();

        if (!symbolTable.hasKey(this.variableName)) {
            throw new StatementException(this.variableName + " is not defined");
        }

        Value value = symbolTable.get(this.variableName);

        if (!value.getType().equals(new IntType())) {
            throw new StatementException("Expected IntType; got " + value.getType());
        }

        value = this.expression.evaluate(symbolTable, heap);

        if (!value.getType().equals(new StringType())) {
            throw new StatementException("Expected StringType; got " + value.getType());
        }

        StringValue stringValue = (StringValue) value;

        if (!fileTable.hasKey(stringValue.getValue())) {
            throw new StatementException("File " + stringValue.getValue() + " is not opened");
        }

        try {
            IntValue number = new IntValue(0);

            BufferedReader reader = fileTable.get(stringValue.getValue());
            String line = reader.readLine();

            if (line != null) {
                number = new IntValue(Integer.parseInt(line));
            }

            symbolTable.set(this.variableName, number);
            state.setSymbolTable(fullTable);
        } catch (IOException exception) {
            throw new FileExceptions("Can not read from " + stringValue.getValue() + "\n" + exception.getMessage());
        }

        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new ReadFileStatement(this.expression.deepCopy(), this.variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type = this.expression.typeCheck(table);

        if (type.equals(new StringType())) {
            return table;
        }

        throw new MyException("FileName not of type String");
    }

    @Override
    public String toString() {
        return "readFile(" + this.expression + ")";
    }
}
