package Model.Collections;

import Exceptions.ADTException;
import Exceptions.MyException;

import java.util.HashMap;

public class MyDictionary<_Type1, _Type2> implements MyDictionaryInterface<_Type1, _Type2> {
    HashMap<_Type1, _Type2> dictionary;

    public MyDictionary() {
        this.dictionary = new HashMap<>();
    }

    @Override
    public boolean hasKey(_Type1 key) {
        return this.dictionary.containsKey(key);
    }

    @Override
    public void put(_Type1 key, _Type2 value) throws MyException {
        if (this.hasKey(key)) {
            throw new ADTException("Key already exists " + key);
        }
        this.dictionary.put(key, value);
    }

    @Override
    public _Type2 get(_Type1 key) throws MyException {
        if (!this.hasKey(key)) {
            throw new ADTException("Unknown key '" + key + "'");
        }

        return this.dictionary.get(key);
    }

    @Override
    public void set(_Type1 key, _Type2 value) throws MyException {
        if (!this.hasKey(key)) {
            throw new ADTException("Unknown key '" + key + "'");
        }

        this.dictionary.put(key, value);
    }

    @Override
    public void remove(_Type1 key) throws MyException {
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
    public MyDictionaryInterface<_Type1, _Type2> deepCopy() throws MyException {
        MyDictionaryInterface<_Type1, _Type2> copy = new MyDictionary<>();
        for (_Type1 key : this.dictionary.keySet()) {
            copy.put(key, this.dictionary.get(key));
        }
        return copy;
    }
}
