package Repository;

import Array.DynamicArray;
import Array.FixedArray;
import Exceptions.CustomException;
import Exceptions.InvalidIndexException;
import Model.Shape;

public class Repository implements IRepository {
    private final FixedArray shapes;

    public Repository() {
        this.shapes = new FixedArray();
    }
    public void addShape(Shape shape) throws CustomException {
        this.shapes.append(shape);
    }

    public void removeShape(int index) throws InvalidIndexException {
        this.shapes.eraseAt(index);

    }

    public FixedArray getShapes() {
        return this.shapes;
    }

    public int getSize() {
        return this.shapes.getSize();
    }
}
