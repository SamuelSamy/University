package UI.Controllers;

import Interfaces.IMessageBox;
import Interfaces.IQuestionsManager;
import UI.Messages.MessageBox;
import UI.Quiz;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class StartAppController implements Initializable {

    private IQuestionsManager questionsManager;

    public StartAppController() {}

    public void setQuestionsManager(IQuestionsManager questionsManager) {
        this.questionsManager = questionsManager;
    }

    @FXML public Button newQuizButton;

    @FXML public void newQuizButton_onAction() {
        try {
            int noOfQuestions = Integer.parseInt(noOfQuestionsTextArea.getText());

            new Quiz().show(questionsManager, noOfQuestions);
        } catch (Exception e) {
            IMessageBox message = new MessageBox("Computer Network Quizzes", "CompNet", "Invalid no. of questions.");
            message.show();
        }
    }

    @FXML public Button exitButton;

    @FXML public void exitButton_onAction() {
        Stage stage = (Stage) exitButton.getScene().getWindow();
        stage.close();
    }

    @FXML public TextArea noOfQuestionsTextArea;

    @Override
    public void initialize(URL location, ResourceBundle resources) {

    }
}
