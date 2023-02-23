package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyStackInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.BoolType;
import Model.Types.Type;
import Model.Values.BoolValue;
import Model.Values.Value;

public class WhileStatement implements StatementInterface {
    public final StatementInterface statement;
    public final Expression expression;

    public WhileStatement(Expression expression, StatementInterface statement) {
        this.expression = expression;
        this.statement = statement;
    }


    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyStackInterface<StatementInterface> stack = state.getExecutionStack();

        Value value = this.expression.evaluate(state.getSymbolTable(), state.getHeap());

        if (!(value.getType().equals(new BoolType()))) {
            throw new MyException("Expected BoolType, got " + value.getType().toString());
        }

        if (value.equals(new BoolValue(true))) {
            stack.push(new WhileStatement(this.expression, this.statement));
            stack.push(this.statement);
        }

        state.setExecutionStack(stack);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new WhileStatement(this.expression.deepCopy(), this.statement.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type = this.expression.typeCheck(table);

        if (type.equals(new BoolType())) {
            this.statement.typeCheck(table.deepCopy());
            return table;
        }

        throw new MyException("While condition not of type Bool");
    }

    @Override
    public String toString() {
        return "while(" + this.expression.toString() + ") { " + this.statement.toString() + " }";
    }
}
