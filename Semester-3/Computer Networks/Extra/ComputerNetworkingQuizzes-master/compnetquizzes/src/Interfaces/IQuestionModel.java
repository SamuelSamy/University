package Interfaces;

import java.util.ArrayList;

public interface IQuestionModel {

    String getStatement();

    ArrayList<String> getAnswerOptions();

    Integer getPoints();

    ArrayList<String> getAttachmentsPath();

    String getAnswer();

    boolean checkAnswer(String userAnswer);
}
