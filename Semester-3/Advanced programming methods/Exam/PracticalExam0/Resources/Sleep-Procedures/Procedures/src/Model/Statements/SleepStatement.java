package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.ProgramState;
import Model.Types.Type;

public class SleepStatement implements StatementInterface {

    private final int number;

    public SleepStatement(int number) {
        this.number = number;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        if (number == 0) {
            return state;
        }

        state.getExecutionStack().push(new SleepStatement(number - 1));
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new SleepStatement(number);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        return table;
    }

    @Override
    public String toString() {
        return "sleep(" + number + ")";
    }
}
