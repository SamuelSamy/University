package Model;

public class Sphere implements Shape {
    private double radius;

    public Sphere() {
        this.radius = 0;
    }

    public Sphere(double radius) {
        this.radius = radius;
    }

    public double getRadius() {
        return this.radius;
    }

    public void setRadius(double newRadius) {
        this.radius = newRadius;
    }
    @Override
    public double getVolume() {
        return (4 * Math.PI * this.radius * this.radius * this.radius) / 3;
    }

    @Override
    public String toString() {
        return "Sphere with radius = " + this.radius + " has volume = " + this.getVolume();
    }
}
