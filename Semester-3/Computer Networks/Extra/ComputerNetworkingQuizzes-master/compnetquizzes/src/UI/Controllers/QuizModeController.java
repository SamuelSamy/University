package UI.Controllers;

import Interfaces.IQuestionModel;
import Interfaces.IQuizManager;
import UI.Result;
import Utilities.GlobalVars;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextArea;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;
import java.util.stream.IntStream;

public class QuizModeController implements Initializable {

    public QuizModeController() {}

    @FXML public Button nextButton;

    @FXML public void nextButton_onAction() {
        saveUserAnswer();

        if (currentQuestionIndex + 1 < quizManager.getNoOfQuestions()) {
            currentQuestionIndex++;

            loadData();
        }
    }

    @FXML public Button prevButton;

    @FXML public void prevButton_onAction() {
        saveUserAnswer();

        if (currentQuestionIndex > 0) {
            currentQuestionIndex--;

            loadData();
        }
    }

    @FXML public void questionsListView_onMouseClicked() {
        saveUserAnswer();

        currentQuestionIndex = questionsListView.getSelectionModel().getSelectedItem();

        loadData();
    }

    private void setNextAndPrevButtonsState() {
        if (currentQuestionIndex == 0 && !prevButton.isDisabled()) {
            prevButton.setDisable(true);
        }

        if (currentQuestionIndex + 1 >= quizManager.getNoOfQuestions() && !nextButton.isDisabled()) {
            nextButton.setDisable(true);
        }

        if (currentQuestionIndex != 0 && prevButton.isDisabled()) {
            prevButton.setDisable(false);
        }

        if (currentQuestionIndex != quizManager.getNoOfQuestions() - 1 && nextButton.isDisabled()) {
            nextButton.setDisable(false);
        }
    }


    @FXML public Button finishButton;

    @FXML public void finishButton_onAction() throws Exception {
        saveUserAnswer();

        new Result().show(quizManager);

        closeWindow();
    }

    @FXML public ListView<Integer> questionsListView;
    @FXML public ListView<String> questionOptionsListView;
    @FXML public ListView<String> attachmentsListView;

    @FXML public void attachmentsListView_onMouseClicked() {
        attachmentImageView.setImage(new Image("file:" + GlobalVars.PATHTOPROJ + "/Data/Attachments/" + attachmentsListView.getSelectionModel().getSelectedItem()));
    }

    @FXML public TextArea questionStatementTextArea;
    @FXML public TextArea userAnswerTextArea;
    @FXML public ImageView attachmentImageView;

    private int currentQuestionIndex;

    private IQuizManager quizManager;

    public void setQuizManager(IQuizManager quizManager) {
        this.quizManager = quizManager;

        ArrayList<Integer> questionIndexes = new ArrayList<>();
        IntStream.range(0, this.quizManager.getNoOfQuestions()).forEach(q -> questionIndexes.add(q));
        questionsListView.setItems(FXCollections.observableArrayList(questionIndexes));
        loadData();
    }

    private void saveUserAnswer() {
        String userAnswer = userAnswerTextArea.getText();

        if (userAnswer != null && !userAnswer.isEmpty()) {
            quizManager.saveUserAnswer(currentQuestionIndex, userAnswer);
        }
    }

    public void closeWindow() {
        Stage stage = (Stage) nextButton.getScene().getWindow();
        stage.close();
    }

    private void loadData() {
        IQuestionModel questionModel = quizManager.getQuestionByIndex(currentQuestionIndex);
        questionStatementTextArea.setText(questionModel.getStatement());
        questionOptionsListView.setItems(FXCollections.observableArrayList(questionModel.getAnswerOptions()));

        attachmentImageView.setImage(null);
        ArrayList<String> attachmentPaths = questionModel.getAttachmentsPath();
        attachmentsListView.setItems(FXCollections.observableArrayList(attachmentPaths));
        if (attachmentPaths.size() > 0) {
            attachmentImageView.setImage(new Image("file:" + GlobalVars.PATHTOPROJ + "/Data/Attachments/" + attachmentPaths.get(0))); //TODO refactor
        }

        userAnswerTextArea.clear();
        String userAnswer = quizManager.getUserAnswer(currentQuestionIndex);
        if (userAnswer != null && !userAnswer.isEmpty()) {
            userAnswerTextArea.setText(userAnswer);
        }

        questionsListView.getSelectionModel().select(currentQuestionIndex);
        setNextAndPrevButtonsState();
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        currentQuestionIndex = 0;
    }
}
