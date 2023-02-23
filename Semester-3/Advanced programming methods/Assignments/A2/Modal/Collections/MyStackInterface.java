package Modal.Collections;

import Exceptions.MyException;

public interface MyStackInterface<_Type> {
    _Type pop() throws MyException;
    void push(_Type element);
    _Type peek() throws MyException;
    boolean isEmpty();
}
