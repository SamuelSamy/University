import Controller.Controller;
import Repository.Repository;
import Repository.RepositoryInterface;
import View.View;

public class Main {
    public static void main(String[] args) {
        RepositoryInterface repo = new Repository();
        Controller controller = new Controller(repo);
        View view = new View(controller);
        view.run();
    }
}
