package Model.Statements;

import Exceptions.MyException;
import Exceptions.StatementException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Collections.MyStackInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.BoolType;
import Model.Values.BoolValue;
import Model.Values.Value;

public class ConditionalStatement implements StatementInterface {
    private final Expression conditionExpression;
    private final StatementInterface thenStatement;
    private final StatementInterface elseStatement;

    public ConditionalStatement(Expression conditionExpression, StatementInterface thenStatement, StatementInterface elseStatement) {
        this.conditionExpression = conditionExpression;
        this.thenStatement = thenStatement;
        this.elseStatement = elseStatement;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        MyStackInterface<StatementInterface> stack = state.getExecutionStack();
        MyHeapInterface<Value> heap = state.getHeap();

        Value condition = conditionExpression.evaluate(symbolTable, heap);

        if (!condition.getType().equals(new BoolType())) {
            throw new StatementException("Expression: expected BoolType; got " + condition.getType());
        }

        if (condition.equals(new BoolValue(true))) {
            stack.push(this.thenStatement);
        } else {
            stack.push(this.elseStatement);
        }

        state.setExecutionStack(stack);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new ConditionalStatement(this.conditionExpression, this.thenStatement, this.elseStatement);
    }

    @Override
    public String toString() {
        return "( if (" + conditionExpression.toString() + ") then (" + thenStatement.toString() + ") else (" + elseStatement.toString() + ") )";
    }
}
