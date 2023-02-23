package Model.Types;

import Model.Values.IntValue;
import Model.Values.StringValue;
import Model.Values.Value;

public class StringType implements Type {
    @Override
    public boolean equals(Object obj) {
        return obj instanceof StringType;
    }

    @Override
    public String toString() {
        return "string";
    }

    @Override
    public Value getDefault() {
        return new StringValue("");
    }

    @Override
    public Type deepCopy() {
        return new StringType();
    }
}
