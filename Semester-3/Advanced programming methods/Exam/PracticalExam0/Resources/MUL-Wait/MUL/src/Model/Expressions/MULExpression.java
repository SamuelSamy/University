package Model.Expressions;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.Value;

public class MULExpression implements Expression {

    private final Expression firstExpression;
    private final Expression secondExpression;

    public MULExpression(Expression firstExpression, Expression secondExpression) {
        this.firstExpression = firstExpression;
        this.secondExpression = secondExpression;
    }

    @Override
    public Value evaluate(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) throws MyException {
        Expression expression = new ArithmeticalExpression(
                '-',
                new ArithmeticalExpression(
                        '*',
                        firstExpression,
                        secondExpression
                ),
                new ArithmeticalExpression(
                        '+',
                        firstExpression,
                        secondExpression
                )
        );

        return expression.evaluate(symbolTable, heap);
    }

    @Override
    public Expression deepCopy() {
        return new MULExpression(firstExpression.deepCopy(), secondExpression.deepCopy());
    }

    @Override
    public Type typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type firstType = firstExpression.typeCheck(table);
        Type secondType = secondExpression.typeCheck(table);

        if (!firstType.equals(secondType)) {
            throw new MyException("The types of the operands are not the same!");
        }

        if (!firstType.equals(new IntType())) {
            throw new MyException("The operands are not integers!");
        }

        return new IntType();
    }

    @Override
    public String toString() {
        return "MUL(" + firstExpression.toString() + ", " + secondExpression.toString() + ")";
    }
}
