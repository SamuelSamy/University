package Model.Expressions;

import Exceptions.EvaluateException;
import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;

public class ArithmeticalExpression implements Expression{
    final Expression expression1;
    final Expression expression2;
    final char operator;

    public ArithmeticalExpression(char operator, Expression expression1, Expression expression2) {
        this.expression1 = expression1;
        this.expression2 = expression2;
        this.operator = operator;
    }

    @Override
    public String toString() {
        return "(" + expression1.toString() + " " + operator + " " + expression2.toString() + ")";
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
            case '+' -> {
                return new IntValue(number1 + number2);
            }

            case '-' -> {
                return new IntValue(number1 - number2);
            }

            case '*' -> {
                return new IntValue(number1 * number2);
            }

            case '/' -> {
                if (number2 == 0) {
                    throw new EvaluateException("Division by zero!");
                }
                return new IntValue(number1 / number2);
            }

            default -> throw new EvaluateException("Invalid operator!");
        }
    }

    @Override
    public Expression deepCopy() {
        return new ArithmeticalExpression(operator, expression1.deepCopy(), expression2.deepCopy());
    }

    @Override
    public Type typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type1 = this.expression1.typeCheck(table);
        Type type2 = this.expression2.typeCheck(table);

        if (!type1.equals(new IntType())) {
            throw new MyException("First operand is not an integer");
        }

        if (!type2.equals(new IntType())) {
            throw new MyException("Second operand is not an integer");
        }

        return new IntType();
    }
}
