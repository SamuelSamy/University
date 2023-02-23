package Array;

import Exceptions.CustomException;
import Exceptions.InvalidIndexException;
import Exceptions.SizeException;
import Model.Shape;

public class FixedArray {
    private final Shape[] Shapes;
    private final int capacity;
    private int size;


    public FixedArray() {
        this.Shapes = new Shape[64];
        this.capacity = 64;
        this.size = 0;
    }

    public FixedArray(int capacity) throws SizeException {

        if (capacity < 1) {
            throw new SizeException("The capacity can not be less or equal to 0");
        }

        this.Shapes = new Shape[capacity];
        this.size = 0;
        this.capacity = capacity;
    }



    public void append(Shape shape) throws CustomException {
        if (this.size >= this.capacity) {
            throw new CustomException("The array is full!");
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
