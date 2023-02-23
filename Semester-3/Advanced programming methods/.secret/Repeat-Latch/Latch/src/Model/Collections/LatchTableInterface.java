package Model.Collections;

import Exceptions.MyException;

import java.util.Map;

public interface LatchTableInterface {
    void put(int key, int value) throws MyException;
    void set(int key, int value) throws MyException;

    int get(int key) throws MyException;

    boolean exists(int key);

    int getFreeAddress();
    void setFreeAddress(int freeAddress);

    Map<Integer, Integer> getContent();
    void setContent(Map<Integer, Integer> content);

}
