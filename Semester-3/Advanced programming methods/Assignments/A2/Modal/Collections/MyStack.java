package Modal.Collections;

import Exceptions.ADTException;
import Exceptions.MyException;

import java.util.Stack;

public class MyStack<_Type> implements MyStackInterface<_Type> {

    Stack<_Type> stack;

    public MyStack() {
        this.stack = new Stack<>();
    }

    @Override
    public _Type pop() throws MyException {
        if (this.stack.isEmpty()) {
            throw new ADTException("Empty stack");
        }

        return this.stack.pop();
    }

    @Override
    public void push(_Type element) {
        this.stack.push(element);
    }

    @Override
    public _Type peek() throws MyException{
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
    public String toString() {
        return this.stack.toString();
    }
}
