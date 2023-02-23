package Model;

public class Cube implements Shape {
    private double length;

    public Cube() {
        this.length = 0;
    }

    public Cube(double length) {
        this.length = length;
    }

    public double getLength() {
        return this.length;
    }

    public void setLength(double newLength) {
        this.length = newLength;
    }

    @Override
    public double getVolume() {
        return this.length * this.length * this.length;
    }

    @Override
    public String toString() {
        return "Cube with length = " + this.length + " has volume = " + this.getVolume();
    }
}
