package Modal.Values;

import Modal.Types.IntType;
import Modal.Types.Type;

public class IntValue implements Value {
    private final int value;

    public IntValue(int value) {
        this.value = value;
    }

    public int getValue() {
        return this.value;
    }

    @Override
    public String toString() {
        return Integer.toString(this.value);
    }

    @Override
    public Type getType() {
        return new IntType();
    }

    @Override
    public Value deepCopy() {
        return new IntValue(value);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof IntValue)) {
            return false;
        }

        return this.value == ((IntValue) obj).value;

    }
}
