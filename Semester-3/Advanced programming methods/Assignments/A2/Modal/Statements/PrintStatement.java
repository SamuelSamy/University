package Modal.Statements;

import Exceptions.MyException;
import Modal.Collections.MyListInterface;
import Modal.Expressions.Expression;
import Modal.ProgramState;
import Modal.Values.Value;

public class PrintStatement implements StatementInterface {

    Expression expression;

    public PrintStatement(Expression expression) {
        this.expression = expression;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyListInterface<Value> list = state.getOutput();
        list.add(expression.evaluate(state.getSymbolTable()));
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
