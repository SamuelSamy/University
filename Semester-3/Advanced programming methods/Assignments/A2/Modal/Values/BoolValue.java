package Modal.Values;

import Modal.Types.BoolType;
import Modal.Types.IntType;
import Modal.Types.Type;

public class BoolValue implements Value {
    private final boolean value;

    public BoolValue(Boolean value) {
        this.value = value;
    }

    public boolean getValue() {
        return this.value;
    }

    @Override
    public String toString() {
        return Boolean.toString(this.value);
    }

    @Override
    public Type getType() {
        return new BoolType();
    }

    @Override
    public Value deepCopy() {
        return new BoolValue(value);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof BoolValue)) {
            return false;
        }

        return this.value == ((BoolValue) obj).value;

    }
}
