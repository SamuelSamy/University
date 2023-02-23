package Model.Expressions;

import Exceptions.EvaluateException;
import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Types.BoolType;
import Model.Values.BoolValue;
import Model.Values.Value;

public class LogicExpression implements Expression {
    private final Expression expression1;
    private final Expression expression2;
    private final char operator;

    public LogicExpression(Expression expression1, Expression expression2, char operator) {
        this.expression1 = expression1;
        this.expression2 = expression2;
        this.operator = operator;
    }

    @Override
    public String toString() {
        return expression1.toString() + this.operator + this.operator + expression2.toString();
    }

    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable) throws MyException {
        Value v1, v2;

        v1 = expression1.evaluate(symbolTable);
        if (!(v1.getType().equals(new BoolType()))) {
            throw new EvaluateException("The first operand is not a boolean");
        }

        v2 = expression2.evaluate(symbolTable);
        if (!(v2.getType().equals(new BoolType()))) {
            throw new EvaluateException("The second operand is not a boolean");
        }

        boolean b1 = ((BoolValue)v1).getValue();
        boolean b2 = ((BoolValue)v2).getValue();

        switch (this.operator) {
            case '&' -> {
                return new BoolValue(b1 & b2);
            }

            case '|' -> {
                return new BoolValue(b1 | b2);
            }

            default -> throw new EvaluateException("Invalid operator");
        }
    }

    @Override
    public Expression deepCopy() {
        return new LogicExpression(expression1.deepCopy(), expression2.deepCopy(), operator);
    }


}
