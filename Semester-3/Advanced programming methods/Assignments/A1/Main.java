import Array.FixedArray;
import Controller.Controller;
import Model.Cube;
import Model.Cylinder;
import Model.Sphere;
import Repository.IRepository;
import Repository.Repository;
import View.View;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        IRepository repository = new Repository();
        Controller controller = new Controller(repository);

        try {
            controller.add(new Cube(3));
            controller.add(new Cylinder(3, 1));
            controller.add(new Cube(5));
            controller.add(new Sphere(3));
        } catch (Exception e){
            System.out.println("Error");
            return;
        }


        View view = new View(controller);
        view.run();
    }
}
