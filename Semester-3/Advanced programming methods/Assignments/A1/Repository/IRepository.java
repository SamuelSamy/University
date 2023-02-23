package Repository;

import Array.FixedArray;
import Exceptions.CustomException;
import Exceptions.InvalidIndexException;
import Model.Shape;

public interface IRepository {
    void addShape(Shape shape) throws CustomException;
    void removeShape(int index) throws InvalidIndexException;

    FixedArray getShapes();
}
