package Model.Values;

import Model.Types.ReferenceType;
import Model.Types.Type;

public class ReferenceValue implements Value {
    private final int address;
    private final Type locationType;

    public ReferenceValue(int address, Type locationType) {
        this.address = address;
        this.locationType = locationType;
    }

    public int getAddress() {
        return this.address;
    }

    public Type getType() {
        return new ReferenceType(this.locationType);
    }

    @Override
    public Value deepCopy() {
        return new ReferenceValue(this.address, this.locationType.deepCopy());
    }

    public String toString() {
        return "(" + this.address + ", " + this.locationType.toString() + ")";
    }
}
