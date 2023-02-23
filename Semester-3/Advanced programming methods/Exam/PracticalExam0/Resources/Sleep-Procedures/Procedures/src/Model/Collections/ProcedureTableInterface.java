package Model.Collections;

import Exceptions.MyException;
import Model.Statements.StatementInterface;
import javafx.util.Pair;

import java.util.HashMap;
import java.util.List;
import java.util.Set;

public interface ProcedureTableInterface {

    void put(String key, Pair<List<String>, StatementInterface> value) throws MyException;
    void set(String key, Pair<List<String>, StatementInterface> value) throws MyException;

    Pair<List<String>, StatementInterface> get(String key) throws MyException;

    boolean exists(String key);

    HashMap<String, Pair<List<String>, StatementInterface>> getContent();
    void setContent(HashMap<String, Pair<List<String>, StatementInterface>> content);

    Set<String> getKeySet();
    MyDictionaryInterface<String, Pair<List<String>, StatementInterface>> deepCopy() throws MyException;
}
