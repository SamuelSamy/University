package Model.Statements;

import Exceptions.FileExceptions;
import Exceptions.MyException;
import Exceptions.StatementException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.StringType;
import Model.Types.Type;
import Model.Values.StringValue;
import Model.Values.Value;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class OpenReadFileStatement implements StatementInterface {
    private final Expression expression;

    public OpenReadFileStatement(Expression expression) {
        this.expression = expression;
    }

    @Override
    public String toString() {
        return "openReadFile(" + this.expression.toString() +")";
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getCurrentSymbolTable();
        MyDictionaryInterface<String, BufferedReader> fileTable = state.getFileTable();
        MyHeapInterface<Value> heap = state.getHeap();

        Value value = this.expression.evaluate(symbolTable, heap);
        if (!value.getType().equals(new StringType())) {
            throw new StatementException("openReadFile - Expected StringType; got " + value.getType() + "\n" + this.expression);
        }

        StringValue stringValue = (StringValue) value;

        if (fileTable.hasKey(stringValue.getValue())) {
            throw new StatementException("File " + stringValue.getValue() + " is already opened");
        }

        try {
            BufferedReader reader = new BufferedReader(new FileReader(stringValue.getValue()));
            fileTable.put(stringValue.getValue(), reader);
        } catch (IOException e) {
            throw new FileExceptions("File can not be opened\n" + e.getMessage());
        }

        state.setFileTable(fileTable);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new OpenReadFileStatement(this.expression.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type = this.expression.typeCheck(table);

        if (type.equals(new StringType())) {
            return table;
        }

        throw new MyException("FileName not of type String");
    }
}
