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
                        new AssignmentStatement("v", new ValueExpression(new BoolValue(false))),
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

        // Ref int v; new(v,20); Ref Ref int a; new(a,v); print(v); print(a)
        StatementInterface program4 = new CompoundStatement(
                new VariableDeclarationStatement("v", new ReferenceType(new IntType())),
                new CompoundStatement(
                        new NewStatement("v", new ValueExpression(new IntValue(20))),
                        new CompoundStatement(
                                new VariableDeclarationStatement("a", new ReferenceType(new ReferenceType(new IntType()))),
                                new CompoundStatement(
                                        new NewStatement("a", new VariableExpression("v")),
                                        new CompoundStatement(
                                                new PrintStatement(new VariableExpression("v")),
                                                new PrintStatement(new VariableExpression("a"))
                                        )
                                )
                        )
                )
        );

        // Ref int v;new(v,20);Ref Ref int a; new(a,v);print(rH(v));print(rH(rH(a))+5)
        StatementInterface program5 = new CompoundStatement(
                new VariableDeclarationStatement("v", new ReferenceType(new IntType())),
                new CompoundStatement(
                        new NewStatement("v", new ValueExpression(new IntValue(20))),
                        new CompoundStatement(
                                new VariableDeclarationStatement("a", new ReferenceType(new ReferenceType(new IntType()))),
                                new CompoundStatement(
                                    new NewStatement("a", new VariableExpression("v")),
                                    new CompoundStatement(
                                            new PrintStatement(new ReadHeapExpression(new VariableExpression("v"))),
                                            new PrintStatement(
                                                    new ArithmeticalExpression(
                                                            '+',
                                                            new ReadHeapExpression(new ReadHeapExpression(new VariableExpression("a"))),
                                                            new ValueExpression(new IntValue(5))
                                                    )

                                            )

                                    )
                                )
                        )
                )
        );

        // Ref int v; new(v,20); print(rH(v)); wH(v,30); print(rH(v)+5)
        StatementInterface program6 = new CompoundStatement(
                new VariableDeclarationStatement("v", new ReferenceType(new IntType())),
                new CompoundStatement(
                        new NewStatement("v", new ValueExpression(new IntValue(20))),
                        new CompoundStatement(
                                new PrintStatement(new ReadHeapExpression(new VariableExpression("v"))),
                                new CompoundStatement(
                                        new WriteHeapStatement("v", new ValueExpression(new IntValue(30))),
                                        new PrintStatement(
                                                new ArithmeticalExpression(
                                                        '+',
                                                        new ReadHeapExpression(new VariableExpression("v")),
                                                        new ValueExpression(new IntValue(5))
                                                )
                                        )
                                )
                        )
                )
        );

        //  Ref int v;new(v,20);Ref Ref int a; new(a,v); new(v,30);print(rH(rH(a)))
        StatementInterface program7 = new CompoundStatement(
                new VariableDeclarationStatement("v", new ReferenceType(new IntType())),
                new CompoundStatement(
                        new NewStatement("v", new ValueExpression(new IntValue(20))),
                        new CompoundStatement(
                                new VariableDeclarationStatement("a", new ReferenceType(new ReferenceType(new IntType()))),
                                new CompoundStatement(
                                        new NewStatement("a", new VariableExpression("v")),
                                        new CompoundStatement(
                                                new NewStatement("v", new ValueExpression(new IntValue(30))),
                                                new PrintStatement(new ReadHeapExpression(new ReadHeapExpression(new VariableExpression("a"))))
                                        )
                                )
                        )
                )
        );

        // int v; v=4; (while (v>0) print(v);v=v-1);print(v)
        StatementInterface program8 = new CompoundStatement(
                new VariableDeclarationStatement("v", new IntType()),
                new CompoundStatement(
                        new AssignmentStatement("v", new ValueExpression(new IntValue(4))),
                        new CompoundStatement(
                                new WhileStatement(
                                        new RelationalExpression(
                                                ">",
                                                new VariableExpression("v"),
                                                new ValueExpression(new IntValue(0))
                                        ),
                                        new CompoundStatement(
                                                new PrintStatement(new VariableExpression("v")),
                                                new AssignmentStatement("v", new ArithmeticalExpression('-', new VariableExpression("v"), new ValueExpression(new IntValue(1))))
                                        )
                                ),
                                new PrintStatement(new VariableExpression("v"))
                        )
                )
        );

        //    int v; Ref int a; v=10;new(a,22);   fork(wH(a,30);v=32;print(v);print(rH(a)));   print(v);print(rH(a))
        StatementInterface program9 = new CompoundStatement(
            new VariableDeclarationStatement("v", new IntType()),
            new CompoundStatement(
                    new VariableDeclarationStatement("a", new ReferenceType(new IntType())),
                    new CompoundStatement(
                            new AssignmentStatement("v", new ValueExpression(new IntValue(10))),
                            new CompoundStatement(
                                    new NewStatement("a", new ValueExpression(new IntValue(22))),
                                    new CompoundStatement(
                                            new ForkStatement(
                                                    new CompoundStatement(
                                                            new WriteHeapStatement("a", new ValueExpression(new IntValue(30))),
                                                            new CompoundStatement(
                                                                    new AssignmentStatement("v", new ValueExpression(new IntValue(32))),
                                                                    new CompoundStatement(
                                                                            new PrintStatement(new VariableExpression("v")),
                                                                            new PrintStatement(new ReadHeapExpression(new VariableExpression("a")))
                                                                    )
                                                            )
                                                    )
                                            ),
                                            new CompoundStatement(
                                                    new PrintStatement(new VariableExpression("v")),
                                                    new PrintStatement(new ReadHeapExpression(new VariableExpression("a")))
                                            )
                                    )
                            )
                    )
            )
        );

        // 10
        StatementInterface program10 = new CompoundStatement(
                new VariableDeclarationStatement("v", new IntType()),
                new CompoundStatement(
                        new AssignmentStatement("v", new ValueExpression(new IntValue(1))),
                        new CompoundStatement(
                                new ForkStatement(
                                        new AssignmentStatement("v", new ValueExpression(new IntValue(2)))
                                ),
                                new ForkStatement(
                                        new AssignmentStatement("v", new ValueExpression(new IntValue(3)))
                                )
                        )
                )
        );

        // 11
        // v1=2;v2=3; (if (v1) then print(MUL(v1,v2)) else print (v1))

        StatementInterface program11 = new CompoundStatement(
                new VariableDeclarationStatement("v1", new IntType()),
                new CompoundStatement(
                        new AssignmentStatement("v1", new ValueExpression(new IntValue(2))),
                        new CompoundStatement(
                                new VariableDeclarationStatement("v2", new IntType()),
                                new CompoundStatement(
                                        new AssignmentStatement("v2", new ValueExpression(new IntValue(3))),
                                        new ConditionalStatement(
                                                new RelationalExpression(
                                                        "!=",
                                                        new VariableExpression("v1"),
                                                        new ValueExpression(new IntValue(0))
                                                ),
                                                new PrintStatement(new MULExpression(new VariableExpression("v1"), new VariableExpression("v2"))),
                                                new PrintStatement(new VariableExpression("v1"))
                                        )
                                )
                        )
                )
        );

        // 12
        //v=20; wait(10);print(v*10)

        StatementInterface program12 = new CompoundStatement(
                new VariableDeclarationStatement("v", new IntType()),
                new CompoundStatement(
                        new AssignmentStatement("v", new ValueExpression(new IntValue(20))),
                        new CompoundStatement(
                                new WaitStatement(10),
                                new PrintStatement(
                                        new ArithmeticalExpression(
                                                '*',
                                                new VariableExpression("v"),
                                                new ValueExpression(new IntValue(10))
                                        )
                                )
                        )

                )
        );


        this.programs.add(program0);
        this.programs.add(program1);
        this.programs.add(program2);
        this.programs.add(program3);
        this.programs.add(program4);
        this.programs.add(program5);
        this.programs.add(program6);
        this.programs.add(program7);
        this.programs.add(program8);
        this.programs.add(program9);
        this.programs.add(program10);
        this.programs.add(program11);
        this.programs.add(program12);

    }
}