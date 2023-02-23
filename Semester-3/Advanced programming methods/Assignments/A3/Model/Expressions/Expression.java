package Model.Expressions;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Values.Value;

public interface Expression {
    Value evaluate(MyDictionaryInterface<String, Value> symbolTable) throws MyException;

    Expression deepCopy();
}
