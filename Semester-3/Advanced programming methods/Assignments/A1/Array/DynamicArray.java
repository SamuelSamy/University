package Array;

import Exceptions.InvalidIndexException;
import Exceptions.SizeException;
import Model.Shape;

public class DynamicArray {
    private Shape[] Shapes;
    private int capacity;
    private int size;

    public DynamicArray() {
        this.Shapes = new Shape[1];
        this.size = 0;
        this.capacity = 1;
    }

    public DynamicArray(int capacity) throws SizeException {

        if (capacity < 1) {
            throw new SizeException("The capacity can not be less or equal to 0");
        }

        this.Shapes = new Shape[capacity];
        this.size = 0;
        this.capacity = capacity;
    }

    private void resize() {
        Shape[] temp = new Shape[this.capacity * 2];
        if (this.capacity >= 0) {
            System.arraycopy(this.Shapes, 0, temp, 0, this.capacity);
        }
        this.Shapes = temp;
        this.capacity *= 2;
    }

    public void append(Shape shape) {
        if (this.size >= this.capacity) {
            this.resize();
        }

        this.Shapes[this.size] = shape;
        this.size++;
    }

    public void eraseAt(int index) throws InvalidIndexException {
        if (index >= this.size || index < 0) {
            throw new InvalidIndexException("Invalid index specified");
        }

        for (int i = index; i < this.size - 1; i++) {
            this.Shapes[i] = this.Shapes[i + 1];
        }
        this.size -= 1;
    }

    public Shape getAt(int index) throws InvalidIndexException {
        if (index >= this.size || index < 0) {
            throw new InvalidIndexException("Invalid index specified");
        }

        return this.Shapes[index];
    }

    public void setAt(int index, Shape shape) throws InvalidIndexException {
        if (index >= this.size || index < 0) {
            throw new InvalidIndexException("Invalid index specified");
        }

        this.Shapes[index] = shape;
    }

    public int getSize() {
        return this.size;
    }
}
