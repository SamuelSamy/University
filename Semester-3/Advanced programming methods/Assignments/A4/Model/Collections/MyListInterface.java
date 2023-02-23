package Model.Collections;

import Exceptions.MyException;

public interface MyListInterface<T> {
    void add(T value);
    T pop() throws MyException;
    boolean isEmpty();
    int size();
    T getAt(int index) throws MyException;
    void removeAt(int index) throws MyException;
    MyListInterface<T> deepCopy() throws MyException;
}
