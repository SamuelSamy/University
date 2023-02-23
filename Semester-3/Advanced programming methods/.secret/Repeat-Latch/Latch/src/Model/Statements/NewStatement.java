package Model.Statements;

import Exceptions.MyException;
import Exceptions.StatementException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.ReferenceType;
import Model.Types.Type;
import Model.Values.ReferenceValue;
import Model.Values.Value;

public class NewStatement implements StatementInterface {

    private final String variableName;
    private final Expression expression;


    public NewStatement(String variableName, Expression expression) {
        this.variableName = variableName;
        this.expression = expression;
    }

    @Override
    public String toString() {
        return "new(" + variableName + ", " + expression.toString() + ")";
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        MyHeapInterface<Value> heap = state.getHeap();

        if (!symbolTable.hasKey(this.variableName)) {
            throw new StatementException(this.variableName + " was not declared");
        }

        Value value = symbolTable.get(this.variableName);

        if (!(value.getType() instanceof ReferenceType)) {
            throw new StatementException("Expected RefType, got " + value.getType().toString());
        }

        Value expressionValue = this.expression.evaluate(symbolTable, heap);

        if (!expressionValue.getType().equals(((ReferenceType) value.getType()).getInner())) {
            throw new StatementException("Expected " + ((ReferenceType) value.getType()).getInner() + ", got " + expressionValue.getType());
        }

        int address = heap.allocate(expressionValue);
        symbolTable.set(this.variableName, new ReferenceValue(address, expressionValue.getType()));

        state.setSymbolTable(symbolTable);
        state.setHeap(heap);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new NewStatement(variableName, expression.deepCopy());
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type typeVar = table.get(this.variableName);
        Type typeExp = this.expression.typeCheck(table);

        if (typeVar.equals(new ReferenceType(typeExp))) {
            return table;
        }

        throw new MyException("New statement: right hand side and left hand side have different types");
    }
}
