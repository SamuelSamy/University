package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyListInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Values.Value;

public class PrintStatement implements StatementInterface {

    Expression expression;

    public PrintStatement(Expression expression) {
        this.expression = expression;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyListInterface<Value> list = state.getOutput();
        list.add(expression.evaluate(state.getSymbolTable(), state.getHeap()));
        state.setOutput(list);
        return state;
    }

    @Override
    public String toString() {
        return "print(" + expression.toString() + ")";
    }

    @Override
    public StatementInterface deepCopy() {
        return new PrintStatement(this.expression);
    }
}
