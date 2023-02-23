package Model.Collections;

import Exceptions.ADTException;
import Exceptions.MyException;

import java.util.Collection;
import java.util.HashMap;

public class MyDictionary<T1, T2> implements MyDictionaryInterface<T1, T2> {
    HashMap<T1, T2> dictionary;

    public MyDictionary() {
        this.dictionary = new HashMap<>();
    }

    @Override
    public boolean hasKey(T1 key) {
        return this.dictionary.containsKey(key);
    }

    @Override
    public void put(T1 key, T2 value) throws MyException {
        if (this.hasKey(key)) {
            throw new ADTException("Key already exists " + key);
        }
        this.dictionary.put(key, value);
    }

    @Override
    public T2 get(T1 key) throws MyException {
        if (!this.hasKey(key)) {
            throw new ADTException("Unknown key '" + key + "'");
        }

        return this.dictionary.get(key);
    }

    @Override
    public void set(T1 key, T2 value) throws MyException {
        if (!this.hasKey(key)) {
            throw new ADTException("Unknown key '" + key + "'");
        }

        this.dictionary.put(key, value);
    }

    @Override
    public void remove(T1 key) throws MyException {
        if (!this.hasKey(key)) {
            throw new ADTException("Unknown key '" + key + "'");
        }

        this.dictionary.remove(key);
    }

    @Override
    public String toString() {
        return this.dictionary.toString();
    }

    @Override
    public MyDictionaryInterface<T1, T2> deepCopy() throws MyException {
        MyDictionaryInterface<T1, T2> copy = new MyDictionary<>();
        for (T1 key : this.dictionary.keySet()) {
            copy.put(key, this.dictionary.get(key));
        }
        return copy;
    }

    @Override
    public Collection<T2> values() {
        return this.dictionary.values();
    }
}
