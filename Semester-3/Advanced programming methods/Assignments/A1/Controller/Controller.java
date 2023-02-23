package Controller;

import Array.FixedArray;
import Array.FixedArray;
import Exceptions.CustomException;
import Exceptions.InvalidIndexException;
import Exceptions.SizeException;
import Model.Shape;
import Repository.IRepository;

public class Controller {
    private final IRepository repository;

    public Controller(IRepository repository) {
        this.repository = repository;
    }

    public void add(Shape shape) throws CustomException  {
        this.repository.addShape(shape);
    }

    public void remove(int index) throws InvalidIndexException {
        this.repository.removeShape(index);
    }

    public FixedArray filterShapes(double volume) throws InvalidIndexException, SizeException, CustomException {
        FixedArray filteredShapes = new FixedArray();
        FixedArray shapes = repository.getShapes();

        for (int i = 0; i < shapes.getSize(); i++) {
            if (shapes.getAt(i).getVolume() > volume) {
                filteredShapes.append(shapes.getAt(i));
            }
        }

        return filteredShapes;
    }

    public FixedArray getShapes() {
        return this.repository.getShapes();
    }
}
