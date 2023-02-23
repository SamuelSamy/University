package Model.Collections;

import Exceptions.MyException;

import java.util.Collection;
import java.util.Map;

public interface MyDictionaryInterface<T1, T2> {
    boolean hasKey(T1 key);
    void put(T1 key, T2 value) throws MyException;
    T2 get(T1 key) throws MyException;
    void set(T1 key, T2 value) throws MyException;
    void remove(T1 key) throws MyException;

    MyDictionaryInterface<T1, T2> deepCopy() throws MyException;

    Collection<T2> values();

    Map<T1, T2> getContent();
}
