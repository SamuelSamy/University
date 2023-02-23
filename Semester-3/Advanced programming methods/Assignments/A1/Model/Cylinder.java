package Model;

public class Cylinder implements Shape {
    private double radius;
    private double height;

    public Cylinder() {
        this.radius = 0;
        this.height = 0;
    }

    public Cylinder(double radius, double height) {
        this.radius = radius;
        this.height = height;
    }


    public double getRadius() {
        return this.radius;
    }

    public void setRadius(double newRadius) {
        this.radius = newRadius;
    }

    public double getHeight() {

        return this.height;
    }

    public void setHeight(double newHeight) {

        this.height = newHeight;
    }

    @Override
    public double getVolume() {
        return Math.PI * this.radius * this.radius * this.height;
    }

    @Override
    public String toString() {
        return "Cylinder with radius = " + this.radius + ", height = " + this.height + " has volume = " + this.getVolume();
    }
}
