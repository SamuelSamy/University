package Model.Expressions;

import Exceptions.EvaluateException;
import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Types.IntType;
import Model.Values.BoolValue;
import Model.Values.IntValue;
import Model.Values.Value;

public class RelationalExpression implements Expression {
    private final Expression expression1;
    private final Expression expression2;
    private final String operator;

    public RelationalExpression(String operator, Expression expression1, Expression expression2) {
        this.expression1 = expression1;
        this.expression2 = expression2;
        this.operator = operator;
    }
    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) throws MyException {
        Value v1, v2;

        v1 = expression1.evaluate(symbolTable, heap);
        if (!v1.getType().equals(new IntType())) {
            throw new EvaluateException("Expected IntType, got " + v1.getType() + "\nfirst: " + v1);
        }

        v2 = expression2.evaluate(symbolTable, heap);
        if (!v2.getType().equals(new IntType())) {
            throw new EvaluateException("Expected IntType, got " + v2.getType() + "\nsecond: " + v2);
        }

        int number1 = ((IntValue) v1).getValue();
        int number2 = ((IntValue) v2).getValue();

        switch (this.operator) {
            case "<" -> {
                return new BoolValue(number1 < number2);
            }

            case "<=" -> {
                return new BoolValue(number1 <= number2);
            }

            case ">" -> {
                return new BoolValue(number1 > number2);
            }

            case ">=" -> {
                return new BoolValue(number1 >= number2);
            }

            case "==" -> {
                return new BoolValue(number1 == number2);
            }

            case "!=" -> {
                return new BoolValue(number1 != number2);
            }

            default -> throw new EvaluateException("Invalid operator!");
        }
    }

    @Override
    public Expression deepCopy() {
        return new RelationalExpression(this.operator, this.expression1.deepCopy(), this.expression2.deepCopy());
    }

    @Override
    public String toString() {
        return this.expression1.toString() + " " + this.operator + " " + this.expression2.toString();
    }
}
