package Utilities;

public class Pair<X, Y> implements IPair<X, Y> {
    private final X first;
    private final Y second;

    public Pair(X first, Y second) {
        this.first = first;
        this.second = second;
    }


    @Override
    public X getFirst() {
        return first;
    }

    @Override
    public Y getSecond() {
        return second;
    }
}
