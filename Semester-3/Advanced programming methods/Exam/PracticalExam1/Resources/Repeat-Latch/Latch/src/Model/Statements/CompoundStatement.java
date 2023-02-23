package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Types.Type;

public class CompoundStatement implements StatementInterface {
    StatementInterface first;
    StatementInterface second;

    public CompoundStatement(StatementInterface first, StatementInterface second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyStackInterface<StatementInterface> stack = state.getExecutionStack();
        stack.push(second);
        stack.push(first);
        state.setExecutionStack(stack);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new CompoundStatement(first.deepCopy(), second.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        return this.second.typeCheck(first.typeCheck(table));
    }


    @Override
    public String toString() {
        return "(" + first.toString() + "; " + second.toString() + ")";
    }
}
