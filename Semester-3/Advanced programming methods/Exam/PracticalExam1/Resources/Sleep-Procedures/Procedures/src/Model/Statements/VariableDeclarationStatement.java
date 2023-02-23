package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Types.Type;
import Model.Values.Value;

public class VariableDeclarationStatement implements StatementInterface{

    private final String name;
    private final Type type;

    public VariableDeclarationStatement(String name, Type type) {
        this.name = name;
        this.type = type;
    }



    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getCurrentSymbolTable();
        MyStackInterface<MyDictionaryInterface<String, Value>> fullTable = state.getFullSymbolTable();
        symbolTable.put(this.name, this.type.getDefault());
        state.setSymbolTable(fullTable);
        return state;
    }

    @Override
    public String toString() {
        return this.type.toString() + " " + this.name;
    }

    @Override
    public StatementInterface deepCopy() {
        return new VariableDeclarationStatement(this.name, this.type);
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        table.put(this.name, this.type);
        return table;
    }
}
