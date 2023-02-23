package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionary;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyStackInterface;
import Model.Expressions.Expression;
import Model.Expressions.RelationalExpression;
import Model.Expressions.VariableExpression;
import Model.ProgramState;
import Model.Types.BoolType;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;

public class ForStatement implements StatementInterface {

    private final String variableName;
    private final Expression initialExpression, conditionExpression, stepExpression;
    private final StatementInterface statement;

    public ForStatement(String variableName, Expression initialExpression, Expression conditionExpression, Expression stepExpression, StatementInterface statement) {
        this.variableName = variableName;
        this.initialExpression = initialExpression;
        this.conditionExpression = conditionExpression;
        this.stepExpression = stepExpression;
        this.statement = statement;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        MyStackInterface<StatementInterface> stack = state.getExecutionStack();

        StatementInterface forStatement = new CompoundStatement(
                new CompoundStatement(
                        new VariableDeclarationStatement(this.variableName, new IntType()),
                        new AssignmentStatement(
                                this.variableName,
                                this.initialExpression
                        )
                ),
                new WhileStatement(
                        new RelationalExpression(
                                "<",
                                new VariableExpression(this.variableName),
                                this.conditionExpression
                        ),
                        new CompoundStatement(
                                this.statement,
                                new AssignmentStatement(
                                        this.variableName,
                                        this.stepExpression
                                )
                        )
                )
        );

        stack.push(forStatement);
        state.setExecutionStack(stack);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new ForStatement(this.variableName, this.initialExpression.deepCopy(), this.conditionExpression.deepCopy(), this.stepExpression.deepCopy(), this.statement.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        MyDictionaryInterface<String, Type> newTable = table.deepCopy();

        if (table.hasKey(this.variableName)) {
            throw new MyException("Variable " + this.variableName + " already defined");
        }

        newTable.put(this.variableName, new IntType());

        Type initialType = this.initialExpression.typeCheck(newTable);
        Type conditionType = this.conditionExpression.typeCheck(newTable);
        Type stepType = this.stepExpression.typeCheck(newTable);

        if (!initialType.equals(new IntType())) {
            throw new MyException("Initial expression not of type Int");
        }

        if (!conditionType.equals(new IntType())) {
            throw new MyException("Condition expression not of type Int");
        }

        if (!stepType.equals(new IntType())) {
            throw new MyException("Step expression not of type Int");
        }

        return table;
    }

    @Override
    public String toString() {
        return "for (" + this.variableName + " = " + this.initialExpression.toString() + "; " + this.variableName + " < " + this.conditionExpression.toString() + "; " + this.variableName + " = " + this.stepExpression.toString() + ") " + this.statement.toString();
    }
}
