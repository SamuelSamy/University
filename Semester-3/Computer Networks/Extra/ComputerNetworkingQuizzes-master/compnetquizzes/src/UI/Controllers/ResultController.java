package UI.Controllers;

import Interfaces.IQuestionModel;
import Interfaces.IQuizManager;
import Interfaces.IGradeModel;
import UI.Messages.MessageBox;
import Utilities.Triple;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.cell.PropertyValueFactory;

import java.net.URL;
import java.util.ResourceBundle;

public class ResultController implements Initializable {

    public ResultController() {}

    @FXML public TextArea gradeMessageTextArea;

    @FXML public TableView<Triple<String, String, String>> questionsTableView;

    @FXML public TableColumn<Triple<String, String, String>, String> questionNoTableColumn;
    @FXML public TableColumn<Triple<String, String, String>, String> questionStatementTableColumn;
    @FXML public TableColumn<Triple<String, String, String>, String> questionStatusTableColumn;

    private IQuizManager quizManager;

    public void setQuizManager(IQuizManager quizManager) {
        this.quizManager = quizManager;

        loadData();
    }

    private void loadData() {
        IGradeModel result = quizManager.evaluate();

        gradeMessageTextArea.setText(
                "Your grade is " + result.getGrade() + "(" + result.getUserPoints() + " out of " + result.getTotalPoints() +")\n" +
                "Click on the question to see more details."
        );

        questionsTableView.setItems(FXCollections.observableArrayList(quizManager.getQuestionsStats()));
    }

    @FXML public void questionsTableView_onMouseClicked() {
        int questionIndex = Integer.parseInt(questionsTableView.getSelectionModel().getSelectedItem().getFirst());
        IQuestionModel questionModel = quizManager.getQuestionByIndex(questionIndex);

        new MessageBox(
                "Computer Networks Quiz",
                "Question #" + questionIndex,
                questionModel.toString() +
                        "\nYour answer: " + quizManager.getUserAnswer(questionIndex) +
                        "\nCorrect answer: " + questionModel.getAnswer(),
                500,
                400
        ).show();
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        questionNoTableColumn.setCellValueFactory(new PropertyValueFactory<>("first"));
        questionStatementTableColumn.setCellValueFactory(new PropertyValueFactory<>("second"));
        questionStatusTableColumn.setCellValueFactory(new PropertyValueFactory<>("third"));
    }
}
