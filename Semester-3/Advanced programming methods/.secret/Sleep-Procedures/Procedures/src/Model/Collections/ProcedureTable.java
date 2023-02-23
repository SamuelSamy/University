package Model.Collections;

import Exceptions.MyException;
import Model.Statements.StatementInterface;
import javafx.util.Pair;

import java.util.HashMap;
import java.util.List;
import java.util.Set;

public class ProcedureTable implements ProcedureTableInterface {

    HashMap<String, Pair<List<String>, StatementInterface>> procedures;

    public ProcedureTable() {
        this.procedures = new HashMap<>();
    }

    @Override
    public void put(String key, Pair<List<String>, StatementInterface> value) throws MyException {
        synchronized (this) {
            if (this.exists(key)) {
                throw new MyException("Key already in procedure table");
            }

            this.procedures.put(key, value);
        }
    }

    @Override
    public void set(String key, Pair<List<String>, StatementInterface> value) throws MyException {
        synchronized (this) {
            if (!this.exists(key)) {
                throw new MyException("Key not defined in procedure table");
            }

            this.procedures.put(key, value);
        }
    }

    @Override
    public Pair<List<String>, StatementInterface> get(String key) throws MyException {
        synchronized (this) {
            if (!this.exists(key)) {
                throw new MyException("No procedure named " + key + " found");
            }
            return this.procedures.get(key);
        }
    }

    @Override
    public boolean exists(String key) {
        synchronized (this) {
            return this.procedures.containsKey(key);
        }
    }

    @Override
    public HashMap<String, Pair<List<String>, StatementInterface>> getContent() {
        synchronized (this) {
            return this.procedures;
        }
    }

    @Override
    public void setContent(HashMap<String, Pair<List<String>, StatementInterface>> content) {
        synchronized (this) {
            this.procedures = content;
        }
    }

    @Override
    public Set<String> getKeySet() {
        synchronized (this) {
            return this.procedures.keySet();
        }
    }

    @Override
    public MyDictionaryInterface<String, Pair<List<String>, StatementInterface>> deepCopy() throws MyException {
        synchronized (this) {
            MyDictionaryInterface<String, Pair<List<String>, StatementInterface>> newTable = new MyDictionary<>();

            for (String key : this.procedures.keySet()) {
                newTable.put(key, this.procedures.get(key));
            }

            return newTable;
        }
    }

    @Override
    public String toString() {
        return this.procedures.toString();
    }
}
