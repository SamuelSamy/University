package Main;

import Controllers.LoadController;
import Controllers.RunController;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

public class Main extends Application {
    @Override
    public void start(Stage stage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(Main.class.getResource("Loader.fxml"));
        Scene scene = new Scene(fxmlLoader.load(), 720, 570);
        LoadController loadController = fxmlLoader.getController();

        FXMLLoader fxmlLoader2 = new FXMLLoader(Main.class.getResource("Runner.fxml"));
        Scene scene2 = new Scene(fxmlLoader2.load(), 840, 600);
        RunController runnerController = fxmlLoader2.getController();

        loadController.setRunnerController(runnerController);

        stage.setTitle("Load Window");
        stage.setScene(scene);

        Stage stage2 = new Stage();
        stage2.setTitle("Runner Window");
        stage2.setScene(scene2);
        stage2.show();
        stage.show();

    }

    public static void main(String[] args) {
        launch();
    }
}
