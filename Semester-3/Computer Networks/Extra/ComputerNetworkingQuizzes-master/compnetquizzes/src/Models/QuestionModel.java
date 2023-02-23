package Models;

import Interfaces.IQuestionModel;

import java.util.ArrayList;
import java.util.Arrays;

public class QuestionModel implements IQuestionModel {
    private final String statement;
    private final ArrayList<String> answerOptions;
    private final Integer points;
    private final ArrayList<String> attachmentsPath;
    private final String answer;

    public QuestionModel(String statement, ArrayList<String> answerOptions, Integer points, ArrayList<String> attachmentsPath, String answer) {
        this.statement = statement;
        this.answerOptions = answerOptions;
        this.points = points;
        this.attachmentsPath = attachmentsPath;
        this.answer = answer;
    }

    @Override
    public String getStatement() {
        return statement;
    }

    @Override
    public ArrayList<String> getAnswerOptions() {
        return answerOptions;
    }

    @Override
    public Integer getPoints() {
        return points;
    }

    @Override
    public ArrayList<String> getAttachmentsPath() {
        return attachmentsPath;
    }

    @Override
    public String getAnswer() {
        return answer;
    }

    @Override
    public boolean checkAnswer(String userAnswer) {
        userAnswer = userAnswer.replace(" ", "");
        userAnswer = userAnswer.replace("\n", "");

        String[] userChoices = userAnswer.split(",");
        String[] correctChoices = answer.split("\\s*,\\s*");

        Arrays.sort(userChoices);
        Arrays.sort(correctChoices);

        if (correctChoices.length != userChoices.length) {
            return false;
        }

        for(int i = 0; i < correctChoices.length; i++) {
            if (!correctChoices[i].equalsIgnoreCase(userChoices[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder(statement + "\n");

        for(String option: answerOptions) {
            str.append(option);
            str.append("\n");
        }

        return str.toString();
    }
}
