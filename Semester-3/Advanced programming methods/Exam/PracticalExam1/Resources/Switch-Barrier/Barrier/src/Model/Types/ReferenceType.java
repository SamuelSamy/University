package Model.Types;

import Model.Values.ReferenceValue;
import Model.Values.Value;

public class ReferenceType implements Type {
    private final Type inner;

    public ReferenceType(Type inner) {
        this.inner = inner;
    }

    public Type getInner() {
        return this.inner;
    }

    public boolean equals(Object another) {
        if (!(another instanceof ReferenceType))
            return false;

        return this.inner.equals(((ReferenceType) another).getInner());
    }

    public String toString() {
        return "Ref(" + this.inner.toString() + ")";
    }

    public Value getDefault() {
        return new ReferenceValue(0, this.inner);
    }

    @Override
    public Type deepCopy() {
        return new ReferenceType(this.inner.deepCopy());
    }
}
