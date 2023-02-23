package Managers;

import Interfaces.IQuestionModel;
import Interfaces.IQuestionsManager;
import Interfaces.IRepository;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;
import java.util.stream.IntStream;

public class QuestionsManager implements IQuestionsManager {
    private IRepository repository;
    private Random random = new Random();

    public QuestionsManager(IRepository repository) {
        this.repository = repository;
    }

    @Override
    public ArrayList<Integer> computeQuizQuestions(Integer nrOfQuestions) throws CloneNotSupportedException {
        ArrayList<IQuestionModel> questions = repository.getQuestions();

        ArrayList<Integer> questionsIndexes = new ArrayList<>();
        IntStream.range(0, questions.size()).forEach(questionsIndexes::add);

        Collections.shuffle(questionsIndexes);

        return new ArrayList<>(questionsIndexes.subList(0, nrOfQuestions));
    }

    @Override
    public IRepository getRepository() {
        return repository;
    }
}
