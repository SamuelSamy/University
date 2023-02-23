package Controllers;

import Exceptions.InfoException;
import Exceptions.MyException;
import Model.Collections.MyStackInterface;
import Model.ProgramState;
import Model.Statements.StatementInterface;
import Model.Values.StringValue;
import Model.Values.Value;
import View.Commands.Command;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.util.Callback;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

public class RunController {

    public TableView<Map.Entry<Integer, Integer>> lockTable;
    public TableColumn<Map.Entry<Integer, Integer>, Integer> lockTableLocation;
    public TableColumn<Map.Entry<Integer, Integer>, Integer> lockTableValue;
    private Controller controller = null;

    public TextField noOfStatesField;
    public Button nextStepButton;
    public TableView<Map.Entry<Integer, Value>> heapTableView;
    public ListView<Value> outListView;
    public ListView<String> fileListView;
    public ListView<ProgramState> statesListView;
    public TableView<Map.Entry<String, Value>> symbolTableView;
    public ListView<StatementInterface> stackView;
    public TableColumn<Map.Entry<Integer, Value>, String> heapTableViewAddress;
    public TableColumn<Map.Entry<Integer, Value>, String> heapTableViewValue;
    public TableColumn<Map.Entry<String, Value>, String> symbolTableViewName;
    public TableColumn<Map.Entry<String, Value>, String> symbolTableViewValue;

    @FXML
    public void initialize() {
        this.nextStepButton.setOnAction(actionEvent -> this.nextStepButtonHandler());

        this.statesListView.setCellFactory(new Callback<>() {
            @Override
            public ListCell<ProgramState> call(ListView<ProgramState> programStateListView) {
                return new ListCell<>() {
                    @Override
                    protected void updateItem(ProgramState item, boolean empty) {
                        super.updateItem(item, empty);
                        if (item == null || empty) {
                            setText(null);
                        } else {
                            setText(Integer.toString(item.getCurrentId()));
                        }
                    }
                };
            }
        });

        this.symbolTableViewName.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getKey()));
        this.symbolTableViewValue.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getValue().toString()));

        this.heapTableViewAddress.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getKey().toString()));
        this.heapTableViewValue.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getValue().toString()));

        this.statesListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        this.statesListView.getSelectionModel().selectedItemProperty().addListener((observable, oldState, newState) -> updateThreadState(newState));

        this.lockTableLocation.setCellValueFactory(param -> new SimpleIntegerProperty(param.getValue().getKey()).asObject());
        this.lockTableValue.setCellValueFactory(param -> new SimpleIntegerProperty(param.getValue().getValue()).asObject());
    }

    private void updateThreadState(ProgramState newState) {
        this.setSymbolTable(newState);
        this.setExecutionStack(newState);
    }

    private void setSymbolTable(ProgramState newState) {
        if (newState == null) {
            this.symbolTableView.getItems().clear();
            return;
        }
        this.symbolTableView.getItems().setAll(FXCollections.observableArrayList(newState.getSymbolTable().getContent().entrySet()));
    }

    private void setExecutionStack(ProgramState newState) {
        if (newState == null) {
            this.stackView.getItems().clear();
            return;
        }

        ObservableList<StatementInterface> statements = FXCollections.observableArrayList();
        MyStackInterface<StatementInterface>  executionStack = newState.getExecutionStack();

        try {
            while (!executionStack.isEmpty()) {
                statements.add(executionStack.pop());
            }
        }
        catch (Exception exception) {
            System.out.println(exception.getMessage());
        }

        // add in reverse order
        IntStream.range(0, statements.size())
                .forEach(index -> executionStack.push(statements.get(statements.size() - index - 1)));

        this.stackView.setItems(statements);
    }

    public void clearData() {
        this.noOfStatesField.clear();
        this.heapTableView.getItems().clear();
        this.outListView.getItems().clear();
        this.fileListView.getItems().clear();
        this.statesListView.getItems().clear();
        this.symbolTableView.getItems().clear();
        this.lockTable.getItems().clear();
        this.stackView.getItems().clear();
    }

    @FXML
    public void nextStepButtonHandler() {
        try {
            this.controller.executeOneStepForAllProgramsGUI();
            this.updateFields();

            if (this.statesListView.getSelectionModel().getSelectedIndices().size() == 0) {
                this.statesListView.getSelectionModel().selectFirst();
            }

        } catch (InfoException exception) {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setContentText(exception.getMessage());
            alert.showAndWait();
            this.nextStepButton.setDisable(true);
        }
    }

    private void updateFields() {
        this.setNumberOfStates();
        this.setHeapTable();
        this.setOutList();
        this.setFileList();
        this.setProgramStatesList();
        this.setLockTable();
        this.updateThreadState(this.statesListView.getSelectionModel().getSelectedItem());
    }

    public void setLockTable() {
        if (this.controller.getRepo().getProgramsList().size() == 0) {
            this.lockTable.getItems().clear();
            return;
        }

        this.lockTable.getItems().setAll(FXCollections.observableArrayList(this.controller.getLockTable().getContent().entrySet()));
    }

    private void setNumberOfStates() {
        this.noOfStatesField.setText(Integer.toString(this.controller.getRepo().getProgramsList().size()));
    }

    private void setHeapTable() {
        if (this.controller.getRepo().getProgramsList().size() == 0) {
            this.heapTableView.getItems().clear();
            return;
        }

        this.heapTableView.getItems().setAll(FXCollections.observableArrayList(this.controller.getHeap().getContent().entrySet()));
    }

    private void setOutList() {
        if (this.controller.getRepo().getProgramsList().size() == 0) {
            this.outListView.getItems().clear();
            return;
        }

        this.outListView.getItems().setAll(FXCollections.observableArrayList(this.controller.getOutput().getList()));
    }

    private void setFileList() {
        this.fileListView.getItems().clear();
        if (this.controller.getRepo().getProgramsList().size() == 0) {
            return;
        }

        this.controller.getFileTable().getContent().forEach((name, path) -> this.fileListView.getItems().add(name));
    }

    private void setProgramStatesList() {
        int index = this.statesListView.getSelectionModel().getSelectedIndex();
        this.statesListView.getItems().setAll(FXCollections.observableArrayList(this.controller.getRepo().getProgramsList()));

        if (index == -1 || index >= this.statesListView.getItems().size()) {
            this.statesListView.getSelectionModel().selectFirst();
            return;
        }

        this.statesListView.getSelectionModel().select(index);
    }

    protected void showProgram(Command command) {
        try {
            this.clearData();
            this.controller = command.getController().deepCopy();
            this.controller.typeCheck();
            this.updateFields();
            this.statesListView.getSelectionModel().select(0);
        } catch (MyException exception) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setContentText(exception.getMessage());
            alert.showAndWait();
        }
        this.nextStepButton.setDisable(false);
    }
}
