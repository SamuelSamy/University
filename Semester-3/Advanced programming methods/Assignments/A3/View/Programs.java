package View;

import Model.ProgramState;
import Model.Statements.StatementInterface;

import java.util.ArrayList;
import java.util.List;

import Model.Collections.*;
import Model.Expressions.*;
import Model.Statements.*;
import Model.Types.*;
import Model.Values.*;

public class Programs {

    private final List<StatementInterface> programs;

    public Programs() {
        this.programs = new ArrayList<>();

        this.initializeData();
    }

    public StatementInterface get(int index) {
        return this.programs.get(index);
    }

    private void initializeData() {

        StatementInterface program0 = new CompoundStatement(
                new VariableDeclarationStatement("v", new IntType()),
                new CompoundStatement(
                        new AssignmentStatement("v", new ValueExpression(new IntValue(2))),
                        new PrintStatement(new VariableExpression("v"))
                )
        );

        StatementInterface program1 = new CompoundStatement(
                new VariableDeclarationStatement("a", new IntType()),
                new CompoundStatement(
                        new VariableDeclarationStatement("b", new IntType()),
                        new CompoundStatement(
                                new AssignmentStatement(
                                        "a",
                                        new ArithmeticalExpression(
                                                '+',
                                                new ValueExpression(new IntValue(2)),
                                                new ArithmeticalExpression(
                                                        '*',
                                                        new ValueExpression(new IntValue(3)),
                                                        new ValueExpression(new IntValue(5))
                                                )
                                        )
                                ),
                                new CompoundStatement(
                                        new AssignmentStatement(
                                                "b",
                                                new ArithmeticalExpression(
                                                        '+',
                                                        new VariableExpression("a"),
                                                        new ValueExpression(new IntValue(1))
                                                )
                                        ),
                                        new PrintStatement(new VariableExpression("b"))
                                )
                        )
                )
        );

        StatementInterface program2 = new CompoundStatement(
                new VariableDeclarationStatement("a", new BoolType()),
                new CompoundStatement(
                        new VariableDeclarationStatement("v", new IntType()),
                        new CompoundStatement(
                                new AssignmentStatement(
                                        "a",
                                        new ValueExpression(new BoolValue(true))
                                ),
                                new CompoundStatement(
                                        new ConditionalStatement(
                                                new VariableExpression("a"),
                                                new AssignmentStatement(
                                                        "v",
                                                        new ValueExpression(new IntValue(2))
                                                ),
                                                new AssignmentStatement(
                                                        "v",
                                                        new ValueExpression((new IntValue(3)))
                                                )
                                        ),
                                        new PrintStatement(new VariableExpression("v"))
                                )
                        )
                )
        );

        StatementInterface program3 = new CompoundStatement(
                new VariableDeclarationStatement("varf", new StringType()),
                new CompoundStatement(
                        new AssignmentStatement("varf", new ValueExpression(new StringValue("test.in"))),
                        new CompoundStatement(
                                new OpenReadFileStatement(new VariableExpression("varf")),
                                new CompoundStatement(
                                        new VariableDeclarationStatement("varc", new IntType()),
                                        new CompoundStatement(
                                                new ReadFileStatement(new VariableExpression("varf"),"varc"),
                                                new CompoundStatement(
                                                        new PrintStatement(new VariableExpression("varc")),
                                                        new CompoundStatement(
                                                                new ReadFileStatement(new VariableExpression("varf"), "varc"),
                                                                new CompoundStatement(
                                                                        new PrintStatement(new VariableExpression("varc")),
                                                                        new CloseFileStatement(new VariableExpression("varf"))
                                                                )
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );

        this.programs.add(program0);
        this.programs.add(program1);
        this.programs.add(program2);
        this.programs.add(program3);
    }
}
