package Model.Values;

import Model.Types.IntType;
import Model.Types.StringType;
import Model.Types.Type;

import java.util.Objects;

public class StringValue implements Value {
    private final String value;

    public StringValue(String value) {
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }

    @Override
    public String toString() {
        return "'" + this.value + "'";
    }

    @Override
    public Type getType() {
        return new StringType();
    }

    @Override
    public Value deepCopy() {
        return new StringValue(value);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof StringValue)) {
            return false;
        }

        return Objects.equals(this.value, ((StringValue) obj).value);

    }
}
