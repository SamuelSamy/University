package Model.Expressions;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyHeap;
import Model.Collections.MyHeapInterface;
import Model.Types.Type;
import Model.Values.Value;

public interface Expression {
    Value evaluate(MyDictionaryInterface<String, Value> symbolTable, MyHeapInterface<Value> heap) throws MyException;
    Expression deepCopy();
    Type typeCheck(MyDictionaryInterface<String, Type> table) throws MyException;
}
