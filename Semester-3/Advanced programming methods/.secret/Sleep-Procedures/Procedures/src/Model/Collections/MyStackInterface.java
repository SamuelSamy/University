package Model.Collections;

import Exceptions.MyException;

import java.util.ArrayList;

public interface MyStackInterface<T> {
    T pop() throws MyException;
    void push(T element);
    T peek() throws MyException;
    boolean isEmpty();

    ArrayList<T> toList();
    MyStackInterface<T> deepCopy() throws MyException;
}
