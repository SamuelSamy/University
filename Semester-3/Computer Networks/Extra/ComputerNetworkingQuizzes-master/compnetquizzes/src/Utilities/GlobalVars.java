package Utilities;

public class GlobalVars {
    private static GlobalVars ourInstance = new GlobalVars();

    public static GlobalVars getInstance() {
        return ourInstance;
    }

    public static final String PATHTOPROJ = System.getProperty("user.dir") + "/src";

    private GlobalVars() {
    }
}
