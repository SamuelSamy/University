package Model.Collections;

import Exceptions.MyException;
import javafx.util.Pair;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BarrierTable implements BarrierTableInterface {

    private Map<Integer, Pair<Integer, List<Integer>>> barrierTable;
    private int freeAddress;

    public BarrierTable() {
        this.barrierTable = new HashMap<>();
        this.freeAddress = 0;
    }


    @Override
    public void put(int key, Pair<Integer, List<Integer>> value) throws MyException {
        synchronized (this) {
            if (this.barrierTable.containsKey(key)) {
                throw new MyException("Key already exists in the barrier table");
            }
            this.barrierTable.put(key, value);
        }
    }

    @Override
    public void set(int key, Pair<Integer, List<Integer>> value) throws MyException {
        synchronized (this) {
            if (!this.barrierTable.containsKey(key)) {
                throw new MyException("Key does not exist in the barrier table");
            }
            this.barrierTable.put(key, value);
        }
    }

    @Override
    public Pair<Integer, List<Integer>> get(int key) throws MyException {
        synchronized (this) {
            if (!this.barrierTable.containsKey(key)) {
                throw new MyException("Key does not exist in the barrier table");
            }
            return this.barrierTable.get(key);
        }
    }

    @Override
    public boolean exists(int key) {
        synchronized (this) {
            return this.barrierTable.containsKey(key);
        }
    }

    @Override
    public int getFreeAddress() {
        synchronized (this) {
            return this.freeAddress;
        }
    }

    @Override
    public void setFreeAddress(int address) {
        synchronized (this) {
            this.freeAddress = address;
        }
    }

    @Override
    public Map<Integer, Pair<Integer, List<Integer>>> getContent() {
        synchronized (this) {
            return this.barrierTable;
        }
    }

    @Override
    public void setContent(Map<Integer, Pair<Integer, List<Integer>>> map) {
        synchronized (this) {
            this.barrierTable = map;
        }
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (Map.Entry<Integer, Pair<Integer, List<Integer>>> entry : this.barrierTable.entrySet()) {
            result.append(entry.getKey()).append(" -> ").append(entry.getValue()).append("\n");
        }
        return result.toString();
    }
}
