package Modal.Statements;

import Exceptions.MyException;
import Modal.Collections.MyDictionaryInterface;
import Modal.ProgramState;
import Modal.Types.Type;
import Modal.Values.Value;

public class VariableDeclarationStatement implements StatementInterface{

    private final String name;
    private final Type type;

    public VariableDeclarationStatement(String name, Type type) {
        this.name = name;
        this.type = type;
    }



    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyDictionaryInterface<String, Value> symbolTable = state.getSymbolTable();
        symbolTable.put(this.name, this.type.getDefault());
        state.setSymbolTable(symbolTable);
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
}
