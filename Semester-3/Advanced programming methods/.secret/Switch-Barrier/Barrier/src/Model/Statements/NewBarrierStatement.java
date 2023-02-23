package Model.Statements;

import Exceptions.MyException;
import Model.Collections.BarrierTableInterface;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeap;
import Model.Collections.MyHeapInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class NewBarrierStatement implements StatementInterface {

    private final String variableName;
    private final Expression expression;

    public NewBarrierStatement(String variableName, Expression expression) {
        this.variableName = variableName;
        this.expression = expression;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        MyHeapInterface<Value> heap = state.getHeap();
        BarrierTableInterface barrierTable = state.getBarrierTable();

        IntValue value = (IntValue) this.expression.evaluate(symbolTable, heap);
        int address = barrierTable.getFreeAddress();
        barrierTable.put(address, new Pair<>(value.getValue(), new ArrayList<>()));

        if (!symbolTable.hasKey(this.variableName)) {
            throw new MyException("Variable not declared");
        }

        symbolTable.set(this.variableName, new IntValue(address));

        return null;
    }

    @Override
    public StatementInterface deepCopy() {
        return new NewBarrierStatement(this.variableName, this.expression.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {

        if (!table.get(this.variableName).equals(new IntType())) {
            throw new MyException("Variable is not of type IntValue");
        }

        if (!this.expression.typeCheck(table).equals(new IntType())) {
            throw new MyException("Expression is not of type IntValue");
        }

        return table;
    }

    @Override
    public String toString() {
        return "NewBarrier(" + this.variableName + ", " + this.expression.toString() + ")";
    }
}
