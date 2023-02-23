package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Collections.SemaphoreTableInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.List;

public class CreateSemaphore implements StatementInterface {

    private final String variableName;
    private final Expression expression;


    public CreateSemaphore(String variableName, Expression expression) {
        this.variableName = variableName;
        this.expression = expression;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {

        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();

        if (!symbolTable.hasKey(variableName)) {
            throw new MyException("Variable " + variableName + " is not defined");
        }

        if (!(symbolTable.get(variableName) instanceof IntValue)) {
            throw new MyException("Variable " + variableName + " is not an integer");
        }

        MyHeapInterface<Value> heap = state.getHeap();
        SemaphoreTableInterface semaphoreTable = state.getSemaphoreTable();

        int value = ((IntValue) expression.evaluate(symbolTable, heap)).getValue();
        int address = semaphoreTable.getFreeAddress();

        symbolTable.set(variableName, new IntValue(address));
        semaphoreTable.put(address, new Pair<>(value, new ArrayList<>()));
        return null;
    }

    @Override
    public StatementInterface deepCopy() {
        return new CreateSemaphore(variableName, expression.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeVariable = table.get(variableName);

        if (!typeVariable.equals(new IntType())) {
            throw new MyException("CreateSemaphore: variable is not an integer");
        }

        Type typeExpression = expression.typeCheck(table);
        if (!typeExpression.equals(new IntType())) {
            throw new MyException("CreateSemaphore: expression is not an integer");
        }

        return table;
    }

    @Override
    public String toString() {
        return "CreateSemaphore(" + variableName + ", " + expression + ")";
    }
}
