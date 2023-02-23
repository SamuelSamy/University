package Interfaces;

import Utilities.Triple;

import java.util.ArrayList;

public interface IQuizManager {
    void saveUserAnswer(int questionIndex, String answer);

    IGradeModel evaluate();

    IQuestionModel getQuestionByIndex(int index);

    Integer getNoOfQuestions();

    ArrayList<Triple<String, String, String>> getQuestionsStats();

    String getUserAnswer(int index);
}
