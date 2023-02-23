package Interfaces;

import java.util.ArrayList;

public interface IQuestionsManager {
    ArrayList<Integer> computeQuizQuestions(Integer nrOfQuestions) throws CloneNotSupportedException;
    IRepository getRepository();
}
