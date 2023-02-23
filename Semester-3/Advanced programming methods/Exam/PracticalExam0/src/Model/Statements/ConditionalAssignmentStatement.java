package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.BoolType;
import Model.Types.Type;

public class ConditionalAssignmentStatement implements StatementInterface {

    private final String variableName;
    private final Expression condition;
    private final Expression thenValue;
    private final Expression elseValue;

    public ConditionalAssignmentStatement(String variableName, Expression condition, Expression thenValue, Expression elseValue) {
        this.variableName = variableName;
        this.condition = condition;
        this.thenValue = thenValue;
        this.elseValue = elseValue;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        StatementInterface newStatement = new ConditionalStatement(
                this.condition,
                new AssignmentStatement(this.variableName, this.thenValue),
                new AssignmentStatement(this.variableName, this.elseValue)
        );

        state.getExecutionStack().push(newStatement);
        state.setExecutionStack(state.getExecutionStack());
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new ConditionalAssignmentStatement(this.variableName, this.condition.deepCopy(), this.thenValue.deepCopy(), this.elseValue.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type conditionType = this.condition.typeCheck(table);

        if (!conditionType.equals(new BoolType())) {
            throw new MyException("ConditionalAssignmentStatement: condition is not boolean");
        }

        Type variableType = table.get(this.variableName);
        Type thenType = this.thenValue.typeCheck(table);
        Type elseType = this.elseValue.typeCheck(table);

        if (!variableType.equals(thenType)) {
            throw new MyException("ConditionalAssignmentStatement: thenValue is not of the same type as the variable");
        }

        if (!variableType.equals(elseType)) {
            throw new MyException("ConditionalAssignmentStatement: elseValue is not of the same type as the variable");
        }

        return table;
    }

    @Override
    public String toString() {
        return this.variableName + " = " + this.condition.toString() + " ? " + this.thenValue.toString() + " : " + this.elseValue.toString();
    }
}
