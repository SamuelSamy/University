package Model.Collections;

import Exceptions.MyException;

import java.util.HashMap;

public class LockTable implements LockTableInterface {

    private HashMap<Integer, Integer> lockTable;
    private int freeAddress = 0;


    public LockTable() {
        this.lockTable = new HashMap<>();
    }

    @Override
    public int getFreeAddress() {
        synchronized (this) {
            return ++this.freeAddress;
        }
    }

    @Override
    public void put(int address, int value) throws MyException {
        synchronized (this) {
            if (this.lockTable.containsKey(address)) {
                throw new MyException("LockTable: Address already exists");
            }
            this.lockTable.put(address, value);
        }
    }

    @Override
    public void set(int address, int value) throws MyException {
        synchronized (this) {
            if (!this.lockTable.containsKey(address)) {
                throw new MyException("LockTable: Address does not exist");
            }
            this.lockTable.put(address, value);
        }
    }

    @Override
    public int get(int address) throws MyException {
        synchronized (this) {
            if (!this.lockTable.containsKey(address)) {
                throw new MyException("LockTable: Address does not exist");
            }
            return this.lockTable.get(address);
        }
    }

    @Override
    public boolean contains(int address) {
        synchronized (this) {
            return this.lockTable.containsKey(address);
        }
    }

    @Override
    public void setContent(HashMap<Integer, Integer> content) {
        synchronized (this) {
            this.lockTable = content;
        }
    }

    @Override
    public HashMap<Integer, Integer> getContent() {
        synchronized (this) {
            return this.lockTable;
        }
    }

    @Override
    public String toString() {
        synchronized (this) {
            return this.lockTable.toString();
        }
    }
}
