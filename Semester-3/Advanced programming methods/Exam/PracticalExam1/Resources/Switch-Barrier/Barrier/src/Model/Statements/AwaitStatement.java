package Model.Statements;

import Exceptions.MyException;
import Model.Collections.BarrierTable;
import Model.Collections.BarrierTableInterface;
import Model.Collections.MyDictionaryInterface;
import Model.ProgramState;
import Model.Types.IntType;
import Model.Types.Type;
import Model.Values.IntValue;
import Model.Values.Value;
import javafx.util.Pair;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class AwaitStatement implements StatementInterface {

    private final String variableName;

    public AwaitStatement(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        BarrierTableInterface barrierTable = state.getBarrierTable();

        if (!symbolTable.hasKey(this.variableName)) {
            throw new MyException("Variable not declared");
        }

        int index = ((IntValue)symbolTable.get(this.variableName)).getValue();

        if (!barrierTable.exists(index)) {
            throw new MyException("Index not found in barrier table");
        }

        Pair<Integer, List<Integer>> barrier = barrierTable.get(index);

        int n1 = barrier.getKey();
        int nl = barrier.getValue().size();

        if (n1 <= nl) {
            return null;
        }

        ArrayList<Integer> list = (ArrayList<Integer>) barrier.getValue();
        if (list.contains(state.getCurrentId())) {
            state.getExecutionStack().push(this);
        } else {
            list.add(state.getCurrentId());
            barrierTable.set(index, new Pair<>(n1, list));
            state.setBarrierTable(barrierTable);
        }

        return null;
    }

    @Override
    public StatementInterface deepCopy() {
        return new AwaitStatement(this.variableName);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type type = table.get(this.variableName);

        if (!type.equals(new IntType())) {
            throw new MyException("Variable " + this.variableName + " is not of type IntValue");
        }

        return table;
    }

    @Override
    public String toString() {
        return "await(" + this.variableName + ")";
    }
}
