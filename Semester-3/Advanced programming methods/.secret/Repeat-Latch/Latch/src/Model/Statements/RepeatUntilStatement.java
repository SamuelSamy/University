package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Expressions.Expression;
import Model.Expressions.NegateExpression;
import Model.ProgramState;
import Model.Types.BoolType;
import Model.Types.Type;

public class RepeatUntilStatement implements StatementInterface {

    private final StatementInterface statement;
    private final Expression expression;

    public RepeatUntilStatement(StatementInterface statement, Expression expression) {
        this.statement = statement;
        this.expression = expression;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        state.getExecutionStack().push(
                new CompoundStatement(
                        this.statement,
                        new WhileStatement(
                                new NegateExpression(this.expression),
                                this.statement
                        )
                )
        );
        return state;
     }

    @Override
    public StatementInterface deepCopy() {
        return new RepeatUntilStatement(statement.deepCopy(), expression.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeExpression = expression.typeCheck(table);

        if (!typeExpression.equals(new BoolType())) {
            throw new MyException("RepeatUntilStatement: The condition of REPEAT is not of type BoolValue!");
        }

        statement.typeCheck(table.deepCopy());
        return table;
    }

    @Override
    public String toString() {
        return  "repeat " + statement.toString() + " until " + expression.toString();
    }
}
