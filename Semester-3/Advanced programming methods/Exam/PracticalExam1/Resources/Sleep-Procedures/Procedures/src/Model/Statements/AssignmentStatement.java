package Model.Statements;

import Exceptions.MyException;
import Exceptions.StatementException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Collections.MyStackInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.Type;
import Model.Values.Value;

public class AssignmentStatement implements StatementInterface {

    private final String name;
    private final Expression expression;

    public AssignmentStatement(String name, Expression expression) {
        this.name = name;
        this.expression = expression;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getCurrentSymbolTable();
        MyStackInterface<MyDictionaryInterface<String, Value>> fullTable = state.getFullSymbolTable();
        MyHeapInterface<Value> heap = state.getHeap();
        if (!symbolTable.hasKey(this.name)) {
            throw new StatementException(this.name + " was not declared");
        }

        Value value = expression.evaluate(symbolTable, heap);
        Type type = symbolTable.get(this.name).getType();
        if (!(value.getType().equals(type))) {
            throw new StatementException(this.name + " - invalid conversion from " + type + " to " + value.getType());
        }

        symbolTable.set(this.name, value);
        state.setSymbolTable(fullTable);
        return state;
    }

    @Override
    public String toString() {
        return this.name + " = " + expression.toString();
    }

    @Override
    public StatementInterface deepCopy() {
        return new AssignmentStatement(this.name, this.expression);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeVar = table.get(this.name);
        Type typeExp = this.expression.typeCheck(table);

        if (typeVar.equals(typeExp)) {
            return table;
        }

        throw new MyException("Assignment: right hand side and left hand side have different types");
    }
}
