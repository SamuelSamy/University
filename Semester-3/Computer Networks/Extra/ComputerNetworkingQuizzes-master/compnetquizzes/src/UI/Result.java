package UI;

import Interfaces.IQuestionsManager;
import Interfaces.IQuizManager;
import Managers.QuizManager;
import UI.Controllers.QuizModeController;
import UI.Controllers.ResultController;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class Result {

    public void show(IQuizManager quizManager) throws Exception {
        FXMLLoader loader = new FXMLLoader(getClass().getResource("Views/ResultView.fxml"));
        Parent resultView = loader.load();
        ResultController controller = loader.getController();

        controller.setQuizManager(quizManager);

        Stage stage = new Stage();
        stage.initModality(Modality.APPLICATION_MODAL);
        stage.setTitle("Computer Networks Quiz");
        stage.setScene(new Scene(resultView));
        stage.showAndWait();
    }
}
