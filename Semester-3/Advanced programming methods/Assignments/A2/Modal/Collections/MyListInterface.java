package Modal.Collections;

import Exceptions.ADTException;
import Exceptions.MyException;

public interface MyListInterface<_Type> {
    void add(_Type value);
    _Type pop() throws MyException;
    boolean isEmpty();
    int size();
    _Type getAt(int index) throws MyException;
    void removeAt(int index) throws MyException;
}
