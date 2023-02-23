package Model.Statements;

import Exceptions.MyException;
import Model.Collections.MyDictionaryInterface;
import Model.Collections.MyStackInterface;
import Model.Expressions.Expression;
import Model.Expressions.RelationalExpression;
import Model.ProgramState;
import Model.Types.Type;

public class SwitchStatement implements StatementInterface {

    private final Expression conditionMain;
    private final Expression conditionCase1;
    private final Expression conditionCase2;
    private final StatementInterface case1;
    private final StatementInterface case2;
    private final StatementInterface defaultCase;

    public SwitchStatement (Expression conditionMain, Expression conditionCase1, Expression conditionCase2, StatementInterface case1, StatementInterface case2, StatementInterface defaultCase) {
        this.conditionMain = conditionMain;
        this.conditionCase1 = conditionCase1;
        this.conditionCase2 = conditionCase2;
        this.case1 = case1;
        this.case2 = case2;
        this.defaultCase = defaultCase;
    }

    @Override
    public ProgramState execute(ProgramState state) throws MyException {
        MyStackInterface<StatementInterface> stack = state.getExecutionStack();

        StatementInterface newStatement = new ConditionalStatement(
                new RelationalExpression(
                    "==",
                    conditionMain,
                    conditionCase1
                ),
                case1,
                new ConditionalStatement(
                        new RelationalExpression(
                                "==",
                                conditionMain,
                                conditionCase2
                        ),
                        case2,
                        defaultCase
                )
        );

        stack.push(newStatement);
        state.setExecutionStack(stack);
        return state;
    }

    @Override
    public StatementInterface deepCopy() {
        return new SwitchStatement(
                conditionMain.deepCopy(),
                conditionCase1.deepCopy(),
                conditionCase2.deepCopy(),
                case1.deepCopy(),
                case2.deepCopy(),
                defaultCase.deepCopy()
        );
    }

    @Override
    public MyDictionaryInterface<String, Type> typeCheck(MyDictionaryInterface<String, Type> table) throws MyException {
        Type mainType = conditionMain.typeCheck(table);
        Type case1Type = conditionCase1.typeCheck(table);
        Type case2Type = conditionCase2.typeCheck(table);

        if (!mainType.equals(case1Type)) {
            throw new MyException("SwitchStatement: main condition and case 1 condition have different types");
        }

        if (!mainType.equals(case2Type)) {
            throw new MyException("SwitchStatement: main condition and case 2 condition have different types");
        }

        case1.typeCheck(table.deepCopy());
        case2.typeCheck(table.deepCopy());
        defaultCase.typeCheck(table.deepCopy());
        return table;
    }

    @Override
    public String toString() {
        return "switch (" + conditionMain.toString() + ") (case " + conditionCase1.toString() + ": " + case1.toString() + ") (case " + conditionCase2.toString() + ": " + case2.toString() + ") (default: " + defaultCase.toString() + ")";
    }
}
