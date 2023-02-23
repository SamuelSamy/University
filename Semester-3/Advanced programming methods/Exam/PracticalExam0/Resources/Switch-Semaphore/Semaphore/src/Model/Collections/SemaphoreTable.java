package Model.Collections;

import Exceptions.MyException;
import javafx.util.Pair;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SemaphoreTable implements SemaphoreTableInterface {

    Map<Integer, Pair<Integer, List<Integer>>> semaphoreTable;
    private  int freeAddress = 0;

    public SemaphoreTable() {
        this.semaphoreTable = new HashMap<>();
    }

    @Override
    public void put(int key, Pair<Integer, List<Integer>> value) throws MyException{
        synchronized (this) {
            if (semaphoreTable.containsKey(key)) {
                throw new MyException("SemaphoreTable: key already exists");
            }

            semaphoreTable.put(key, value);
        }
    }

    @Override
    public void set(int key, Pair<Integer, List<Integer>> value) throws MyException{
        synchronized (this) {
            if (!semaphoreTable.containsKey(key)) {
                throw new MyException("SemaphoreTable: key does not exist");
            }

            semaphoreTable.replace(key, value);
        }
    }

    @Override
    public Pair<Integer, List<Integer>> get(int key) throws MyException{
        synchronized (this) {
            if (!semaphoreTable.containsKey(key)) {
                throw new MyException("SemaphoreTable: key does not exist");
            }

            return semaphoreTable.get(key);
        }
    }

    @Override
    public boolean exists(int key) {
        synchronized (this) {
            return semaphoreTable.containsKey(key);
        }
    }

    @Override
    public int getFreeAddress() {
        synchronized (this) {
            return this.freeAddress;
        }
    }

    @Override
    public void setFreeAddress(int freeAddress) {
        synchronized (this) {
            this.freeAddress = freeAddress;
        }
    }

    @Override
    public Map<Integer, Pair<Integer, List<Integer>>> getContent() {
        synchronized (this) {
            return semaphoreTable;
        }
    }

    @Override
    public void setContent(Map<Integer, Pair<Integer, List<Integer>>> content) {
        synchronized (this) {
            this.semaphoreTable = content;
        }
    }

    @Override
    public String toString() {
        return semaphoreTable.toString();
    }
}
