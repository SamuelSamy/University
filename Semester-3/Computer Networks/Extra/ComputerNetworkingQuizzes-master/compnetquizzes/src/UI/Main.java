package UI;

import Interfaces.IQuestionsManager;
import Interfaces.IRepository;
import Managers.QuestionsManager;
import UI.Controllers.StartAppController;
import Utilities.GlobalVars;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import Repository.Repository;

public class Main extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception{
        FXMLLoader loader = new FXMLLoader(getClass().getResource("Views/StartAppView.fxml"));
        Parent root = loader.load();
        StartAppController controller = loader.getController();

        IRepository repository = new Repository(GlobalVars.PATHTOPROJ + "/Data/questions.data");//TODO refactor
        IQuestionsManager questionsManager = new QuestionsManager(repository);

        controller.setQuestionsManager(questionsManager);

        primaryStage.setTitle("Computer Networks Quizzes");
        primaryStage.setScene(new Scene(root));
        primaryStage.show();
    }


    public static void main(String[] args) {
        launch(args);
    }
}
