package Model.Collections;

import Exceptions.MyException;

import java.util.HashMap;

public interface LockTableInterface {
    int getFreeAddress();

    void put(int address, int value) throws MyException;
    void set(int address, int value) throws MyException;

    int get(int address) throws MyException;

    boolean contains(int address);

    void setContent(HashMap<Integer, Integer>content);
    HashMap<Integer, Integer> getContent();

}
