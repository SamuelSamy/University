package Model.Collections;

import Exceptions.ADTException;
import Exceptions.MyException;

import java.util.ArrayList;
import java.util.Stack;

public class MyStack<T> implements MyStackInterface<T> {

    Stack<T> stack;

    public MyStack() {
        this.stack = new Stack<>();
    }

    @Override
    public T pop() throws MyException {
        if (this.stack.isEmpty()) {
            throw new ADTException("Empty stack");
        }

        return this.stack.pop();
    }

    @Override
    public void push(T element) {
        this.stack.push(element);
    }

    @Override
    public T peek() throws MyException{
        if (this.stack.isEmpty()) {
            throw new ADTException("Empty stack");
        }
        return this.stack.peek();
    }

    @Override
    public boolean isEmpty() {
        return this.stack.isEmpty();
    }

    @Override
    public MyStackInterface<T> deepCopy() throws MyException {
        MyStackInterface<T> copy = new MyStack<>();
        for (T element : this.stack) {
            copy.push(element);
        }
        return copy;
    }

    @Override
    public ArrayList<T> toList() {
        return new ArrayList<>(this.stack);
    }

    @Override
    public String toString() {
        return this.stack.toString();
    }
}
