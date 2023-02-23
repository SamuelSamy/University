package Model.Collections;

import Exceptions.ADTException;
import Exceptions.MyException;

import java.util.ArrayList;
import java.util.List;

public class MyList<T> implements MyListInterface<T> {

    List<T> list;

    public MyList() {
        this.list = new ArrayList<>();
    }

    @Override
    public void add(T value) {
        this.list.add(value);
    }

    @Override
    public T pop() throws MyException {
        if (this.list.isEmpty()) {
            throw new ADTException("Empty list");
        }

        return this.list.remove(0);
    }



    @Override
    public boolean isEmpty() {
        return this.list.isEmpty();
    }

    @Override
    public int size() {
        return this.list.size();
    }

    @Override
    public T getAt(int index) throws MyException {
        if (index < 0 || index >= this.size()) {
            throw new ADTException("Index out of range");
        }
        return list.get(index);
    }

    @Override
    public void removeAt(int index) throws MyException {
        if (index < 0 || index >= this.size()) {
            throw new ADTException("Index out of range");
        }
        if (this.size() == 0) {
            throw new MyException("No elements to delete");
        }

        this.list.remove(index);
    }

    @Override
    public MyListInterface<T> deepCopy() throws MyException {
        MyListInterface<T> copy = new MyList<>();
        for (T element : this.list) {
            copy.add(element);
        }
        return copy;
    }


    @Override
    public String toString() {
        return this.list.toString();
    }
}
