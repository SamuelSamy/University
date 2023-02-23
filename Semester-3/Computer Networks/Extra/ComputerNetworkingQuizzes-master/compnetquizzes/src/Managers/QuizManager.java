package Managers;

import Interfaces.IQuestionModel;
import Interfaces.IQuizManager;
import Interfaces.IRepository;
import Interfaces.IGradeModel;
import Models.GradeModel;
import Utilities.Triple;

import java.util.ArrayList;
import java.util.Collections;

public class QuizManager implements IQuizManager {
    private IRepository repository;
    private ArrayList<Integer> questionsIndexes;
    private ArrayList<String> userAnswers;

    public QuizManager(IRepository repository, ArrayList<Integer> questionsIndexes) {
        this.repository = repository;
        this.questionsIndexes = questionsIndexes;

        userAnswers = new ArrayList<>(Collections.nCopies(questionsIndexes.size(), ""));
    }

    @Override
    public void saveUserAnswer(int questionIndex, String answer) {
        userAnswers.set(questionIndex, answer);
    }

    @Override
    public IGradeModel evaluate() {
        ArrayList<IQuestionModel> questions = repository.getQuestions();
        Integer totalSum = 0;

        Integer userScore = 0;
        for (int i = 0; i < questionsIndexes.size(); i++) {
            IQuestionModel question = questions.get(questionsIndexes.get(i));
            totalSum += question.getPoints();

            if (userAnswers.get(i) == null) continue;

            userScore += (question.checkAnswer(userAnswers.get(i)) ? question.getPoints() : -question.getPoints());
        }

        Double grade = 10.0 * (double)userScore / totalSum;

        return new GradeModel(grade, userScore, totalSum);
    }

    @Override
    public IQuestionModel getQuestionByIndex(int index) {
        int questionIndex = questionsIndexes.get(index);

        return repository.getQuestions().get(questionIndex);
    }

    @Override
    public Integer getNoOfQuestions() {
        return questionsIndexes.size();
    }

    @Override
    public ArrayList<Triple<String, String, String>> getQuestionsStats() {
        ArrayList<IQuestionModel> questions = repository.getQuestions();

        ArrayList<Triple<String, String, String>> questionsStats = new ArrayList<>();

        for(int index = 0; index < questionsIndexes.size(); index++) {
            int questionIndex = questionsIndexes.get(index);
            IQuestionModel question = questions.get(questionIndex);

            String status = "Unanswered";

            if (userAnswers.get(index) != null && userAnswers.get(index) != "") {
                status = (question.checkAnswer(userAnswers.get(index)) ? "Correct" : "Incorrect");
            }

            questionsStats.add(
                    new Triple<> (
                            String.valueOf(index),
                            question.getStatement(),
                            status
                    )
            );
        }

        return questionsStats;
    }

    @Override
    public String getUserAnswer(int index) {
        return userAnswers.get(index);
    }
}
