package Modal.Collections;

import Exceptions.ADTException;
import Exceptions.MyException;

public interface MyDictionaryInterface<_Type1, _Type2> {
    boolean hasKey(_Type1 key);
    void put(_Type1 key, _Type2 value) throws MyException;
    _Type2 get(_Type1 key) throws MyException;
    void set(_Type1 key, _Type2 value) throws MyException;
    void remove(_Type1 key) throws MyException;
}
