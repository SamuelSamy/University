package Utilities;

public class Triple<X, Y, Z> extends Pair<X, Y> {
    private final Z third;

    public Triple(X first, Y second, Z third) {
        super(first, second);
        this.third = third;
    }

    public Z getThird() {
        return third;
    }
}
