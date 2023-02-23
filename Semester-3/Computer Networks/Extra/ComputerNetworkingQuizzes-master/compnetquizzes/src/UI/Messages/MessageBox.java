package UI.Messages;

import Interfaces.IMessageBox;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;

public class MessageBox implements IMessageBox{

    private String title;
    private String header;
    private String content;
    private Integer minHeight;
    private Integer minWidth;

    public MessageBox(String title, String header, String content) {
        this.title = title;
        this.header = header;
        this.content = content;
    }

    public MessageBox(String title, String header, String content, int minHeight, int minWidth) {
        this(title, header, content);
        this.minHeight = minHeight;
        this.minWidth = minWidth;
    }

    @Override
    public void show() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle(title);
        alert.setHeaderText(header);
        alert.setContentText(content);
        if (minHeight != null) {
            alert.getDialogPane().setMinHeight(minHeight);
        }
        if (minWidth != null) {
            alert.getDialogPane().setMinWidth(minWidth);
        }
        alert.showAndWait();
    }

}
