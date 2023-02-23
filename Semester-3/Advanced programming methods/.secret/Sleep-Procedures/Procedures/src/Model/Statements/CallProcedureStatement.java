package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionary;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeapInterface;
import Model.Collections.ProcedureTableInterface;
import Model.Expressions.Expression;
import Model.ProgramState;
import Model.Types.Type;
import Model.Values.Value;

import java.util.List;

public class CallProcedureStatement implements StatementInterface {

    private final String procedureName;
    private final List<Expression> expressions;

    public CallProcedureStatement(String procedureName, List<Expression> expressions) {
        this.procedureName = procedureName;
        this.expressions = expressions;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getCurrentSymbolTable();
        ProcedureTableInterface procedures = state.getProceduresTable();
        MyHeapInterface<Value> heap = state.getHeap();

        if (!procedures.exists(this.procedureName)) {
            throw new MyException("Procedure " + this.procedureName + " not defined!");
        }

        List<String> parameters = procedures.get(this.procedureName).getKey();
        StatementInterface statement = procedures.get(this.procedureName).getValue();

        MyDictionaryInterface<String, Value> newSymbolTable = new MyDictionary<>();

        for (int i = 0; i < parameters.size(); i++) {
            newSymbolTable.put(parameters.get(i), this.expressions.get(i).evaluate(symbolTable, heap));
        }

        state.getFullSymbolTable().push(newSymbolTable);
        state.getExecutionStack().push(new ReturnStatement());
        state.getExecutionStack().push(statement);

        return state;
    }


    @Override
    public StatementInterface deepCopy() {
        return new CallProcedureStatement(this.procedureName, this.expressions);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        return table;
    }

    @Override
    public String toString() {
        return "call " + this.procedureName + this.expressions.toString();
    }
}
