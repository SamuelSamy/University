package Repository;

import Interfaces.IQuestionModel;
import Interfaces.IRepository;
import Models.QuestionModel;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

public class Repository implements IRepository {
    private ArrayList<IQuestionModel> questions;
    private String filePath;

    public Repository(String filePath) throws IOException {
        this.filePath = filePath;

        loadData();
    }

    private void loadData() throws IOException { //TODO: create an exception for this!
        questions = new ArrayList<>();

        BufferedReader reader = new BufferedReader(new FileReader(filePath));

        while(true) {
            String statement = reader.readLine();
            if (statement == null || statement.length() == 0) {
                break;
            }

            ArrayList<String> options = new ArrayList<>();
            for(int optionsCnt = Integer.parseInt(reader.readLine()); optionsCnt > 0; optionsCnt--) {
                options.add(reader.readLine());
            }

            ArrayList<String> attachments = new ArrayList<>();
            for (int attachmentsCnt = Integer.parseInt(reader.readLine()); attachmentsCnt > 0; attachmentsCnt--) {
                attachments.add(reader.readLine());
            }

            String answer = reader.readLine();
            int points = Integer.parseInt(reader.readLine());

            questions.add(
                    new QuestionModel(
                            statement,
                            options,
                            points,
                            attachments,
                            answer
                    )
            );
        }
    }

    @Override
    public ArrayList<IQuestionModel> getQuestions() {
        return questions;
    }
}
