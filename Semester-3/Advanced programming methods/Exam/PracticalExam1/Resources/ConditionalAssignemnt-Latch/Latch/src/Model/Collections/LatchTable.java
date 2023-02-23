package Model.Collections;

import Exceptions.MyException;

import java.util.HashMap;
import java.util.Map;

public class LatchTable implements LatchTableInterface {

    private HashMap<Integer, Integer> latchTable;
    private int freeAddress = 0;

    public LatchTable() {
        this.latchTable = new HashMap<>();
    }

    @Override
    public void put(int key, int value) throws MyException {
        synchronized (this) {
            if (this.latchTable.containsKey(key))
                throw new MyException("LatchTable already contains key " + key);

            this.latchTable.put(key, value);
        }
    }

    @Override
    public void set(int key, int value) throws MyException {
        synchronized (this) {
            if (!this.latchTable.containsKey(key))
                throw new MyException("LatchTable does not contain key " + key);

            this.latchTable.put(key, value);
        }
    }

    @Override
    public int get(int key) throws MyException {
        synchronized (this) {
            if (!this.latchTable.containsKey(key))
                throw new MyException("LatchTable does not contain key " + key);

            return this.latchTable.get(key);
        }
    }

    @Override
    public boolean exists(int key) {
        synchronized (this) {
            return this.latchTable.containsKey(key);
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
    public Map<Integer, Integer> getContent() {
        synchronized (this) {
            return this.latchTable;
        }
    }

    @Override
    public void setContent(Map<Integer, Integer> content) {
        synchronized (this) {
            this.latchTable = (HashMap<Integer, Integer>) content;
        }
    }

    @Override
    public String toString() {
        return this.latchTable.toString();
    }
}
