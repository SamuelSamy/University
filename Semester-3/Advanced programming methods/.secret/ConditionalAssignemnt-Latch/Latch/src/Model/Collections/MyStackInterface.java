package Model.Collections;

import Exceptions.MyException;

public interface MyStackInterface<T> {
    T pop() throws MyException;
    void push(T element);
    T peek() throws MyException;
    boolean isEmpty();

    MyStackInterface<T> deepCopy() throws MyException;
}
