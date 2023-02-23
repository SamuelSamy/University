package Model.Types;

import Model.Values.IntValue;
import Model.Values.Value;

public class IntType implements Type {

    @Override
    public boolean equals(Object obj) {
        return obj instanceof IntType;
    }

    @Override
    public String toString() {
        return "int";
    }

    @Override
    public Value getDefault() {
        return new IntValue(0);
    }

    @Override
    public Type deepCopy() {
        return new IntType();
    }
}
