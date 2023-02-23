package Modal.Expressions;

import Exceptions.ADTException;
import Exceptions.EvaluateException;
import Exceptions.MyException;
import Modal.Collections.MyDictionaryInterface;
import Modal.Values.Value;

public interface Expression {
    Value evaluate(MyDictionaryInterface<String, Value> symbolTable) throws MyException;

    Expression deepCopy();
}
