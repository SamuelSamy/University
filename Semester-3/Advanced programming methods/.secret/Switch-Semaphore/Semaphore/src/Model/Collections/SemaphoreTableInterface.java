package Model.Collections;

import Exceptions.MyException;
import javafx.util.Pair;

import java.util.List;
import java.util.Map;

public interface SemaphoreTableInterface {
    void put(int key, Pair<Integer, List<Integer>> value) throws MyException;
    void set(int key, Pair<Integer, List<Integer>> value) throws MyException;
    Pair<Integer, List<Integer>> get(int key) throws MyException;

    boolean exists(int key);
    int getFreeAddress();
    void setFreeAddress(int freeAddress);

    Map<Integer, Pair<Integer, List<Integer>>> getContent();
    void setContent(Map<Integer, Pair<Integer, List<Integer>>> content);
}
