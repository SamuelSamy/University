package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Expressions.ValueExpression;
import Model.ProgramState;
import Model.Types.Type;
import Model.Values.IntValue;

public class WaitStatement implements StatementInterface {

    private final int number;

    public WaitStatement(int number) {
        this.number = number;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        if (this.number <= 0) {
            return state;
        }

        state.getExecutionStack().push(
                new CompoundStatement(
                        new PrintStatement(new ValueExpression(new IntValue(this.number))),
                        new WaitStatement(this.number - 1)
                )
        );

        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new WaitStatement(this.number);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        return table;
    }

    @Override
    public String toString() {
        return "wait(" + this.number + ")";
    }
}
