package Model.Collections;

import java.util.HashMap;
import java.util.Map;

public class MyHeap<T> implements MyHeapInterface<T> {

    private int currentFreeAddress;
    Map<Integer, T> heap;


    public MyHeap() {
        this.heap = new HashMap<>();
        this.currentFreeAddress = 1;
    }

    @Override
    public int allocate(T value) {
        this.heap.put(this.currentFreeAddress, value);
        this.currentFreeAddress++;
        return this.currentFreeAddress - 1;
    }

    @Override
    public T deallocate(int address) {
        return this.heap.remove(address);
    }

    @Override
    public T get(int address) {
        return this.heap.get(address);
    }

    @Override
    public void set(int address, T value) {
        this.heap.put(address, value);
    }

    @Override
    public boolean exists(int address) {
        return this.heap.containsKey(address);
    }

    @Override
    public Map<Integer, T> getHeap() {
        return this.heap;
    }

    @Override
    public void setHeap(Map<Integer, T> map) {
        this.heap = map;
    }

    @Override
    public Map<Integer, T> getContent() {
        return this.heap;
    }

    @Override
    public String toString() {
        return this.heap.toString();
    }
}
