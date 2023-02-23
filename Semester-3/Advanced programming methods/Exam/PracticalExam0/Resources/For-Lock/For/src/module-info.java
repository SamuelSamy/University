module main {
    requires javafx.controls;
    requires javafx.fxml;

    opens main to javafx.fxml;
    exports main;

    opens Controllers to javafx.fxml;
    exports Controllers;

    opens View to javafx.fxml;
    exports View;

    opens Model to javafx.fxml;
    exports Model;

    opens Repository to javafx.fxml;
    exports Repository;

    opens Exceptions to javafx.fxml;
    exports Exceptions;

    opens View.Commands to javafx.fxml;
    exports View.Commands;

    opens Model.Statements to javafx.fxml;
    exports Model.Statements;

    opens Model.Expressions to javafx.fxml;
    exports Model.Expressions;

    opens Model.Collections to javafx.fxml;
    exports Model.Collections;
    
    opens Model.Types to javafx.fxml;
    exports Model.Types;

    opens Model.Values to javafx.fxml;
    exports Model.Values;


}
