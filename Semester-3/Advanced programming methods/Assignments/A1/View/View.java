package View;

import Array.DynamicArray;
import Array.FixedArray;
import Controller.Controller;
import Exceptions.CustomException;
import Exceptions.InvalidIndexException;
import Exceptions.InputException;
import Exceptions.SizeException;
import Model.Cube;
import Model.Cylinder;
import Model.Sphere;

import java.util.Scanner;

public class View {
    private final Controller controller;

    public View(Controller controller) {
        this.controller = controller;
    }

    public void printMenu() {
        System.out.println("");
        System.out.println("1. Display shapes");
        System.out.println("2. Add shape");
        System.out.println("3. Remove shape");
        System.out.println("4. Filter shapes");
        System.out.println("0. Exit");
    }

    private int readInteger(String message) {
        int number = 0;
        boolean valid = false;

        do {
            System.out.println(message);

            try {
                Scanner readOption = new Scanner(System.in);
                number = readOption.nextInt();
                valid = true;
            } catch (Exception e) {
                System.out.println("Invalid integer!");
            }

        } while (!valid);

        return number;
    }

    private double readDouble(String message) {
        double number = 0;
        boolean valid = false;

        do {
            System.out.println(message);

            try {
                Scanner readOption = new Scanner(System.in);
                number = readOption.nextDouble();
                valid = true;
            } catch (Exception e) {
                System.out.println("Invalid double!");
            }
        } while (!valid);

        return number;
    }
    private void printShapes() throws InvalidIndexException {
        FixedArray shapes = this.controller.getShapes();

        if (shapes.getSize() == 0) {
            System.out.println("No shapes found");
            return;
        }

        for (int i = 0; i < shapes.getSize(); i++) {
            System.out.println(i + ". " + shapes.getAt(i));
        }
    }

    private void addShape() throws InputException, CustomException {
        int option;

        do {
            System.out.println("1. Cube");
            System.out.println("2. Sphere");
            System.out.println("3. Cylinder");
            System.out.println("0. Back");

            option = this.readInteger("> Please select the shape type: ");

        } while (option < 0 || option > 3);

        if (option == 0) {
            return;
        }

        if (option == 1) {
            double len = this.readDouble("> Please enter the length of the side: ");

            if (len <= 0) {
                throw new InputException("Invalid length!");
            }

            this.controller.add(new Cube(len));
        }

        if (option == 2) {
            double radius = this.readDouble("> Please enter the radius: ");

            if (radius <= 0) {
                throw new InputException("Invalid radius!");
            }

            this.controller.add(new Sphere(radius));
        }

        if (option == 3) {
            double radius = this.readDouble("> Please enter the radius: ");

            if (radius <= 0) {
                throw new InputException("Invalid radius!");
            }

            double height = this.readDouble("> Please enter the height: ");

            if (height <= 0) {
                throw new InputException("Invalid height!");
            }

            this.controller.add(new Cylinder(radius, height));
        }
    }

    private void removeShape() throws InputException, InvalidIndexException {
        if (this.controller.getShapes().getSize() == 0) {
            throw new InputException("No shapes found!");
        }

        int index = this.readInteger("Please enter the index of the shape you want to remove: ");

        this.controller.remove(index);
    }

    private void filterShapes() throws InputException, SizeException, InvalidIndexException, CustomException {
        double volume = this.readDouble("> Please enter the volume: ");

        FixedArray filteredShapes = this.controller.filterShapes(volume);

        if (filteredShapes.getSize() == 0) {
            throw new InputException("No shapes found!");
        }

        for (int i = 0; i < filteredShapes.getSize(); i++) {
            System.out.println(i + ". " + filteredShapes.getAt(i));
        }
    }

    public void run() {

        int option = 0;

        do {
            this.printMenu();

            try {
                option = this.readInteger("Please enter an option: ");

                if (option > 4) {
                    System.out.println("Invalid option!");
                    continue;
                }

                if (option == 1) {
                    this.printShapes();
                    continue;
                }

                if (option == 2) {
                    this.addShape();
                    System.out.println("Successfully added the shape!");
                    continue;
                }

                if (option == 3) {
                    this.removeShape();
                    System.out.println("Successfully removed the shape!");
                    continue;
                }

                if (option == 4) {
                    this.filterShapes();
                }

            } catch (Exception e) {
                System.out.println(e.getMessage());
            }

        } while (option != 0);

    }
}
