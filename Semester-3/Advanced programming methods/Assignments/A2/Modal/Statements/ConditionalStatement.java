package Modal.Statements;

import Exceptions.MyException;
import Exceptions.StatementException;
import Modal.Collections.MyDictionaryInterface;
import Modal.Collections.MyStackInterface;
import Modal.Expressions.Expression;
import Modal.ProgramState;
import Modal.Types.BoolType;
import Modal.Values.BoolValue;
import Modal.Values.Value;

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

        Value condition = conditionExpression.evaluate(symbolTable);

        if (!condition.getType().equals(new BoolType())) {
            throw new StatementException("Expression is not of type boolean");
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
