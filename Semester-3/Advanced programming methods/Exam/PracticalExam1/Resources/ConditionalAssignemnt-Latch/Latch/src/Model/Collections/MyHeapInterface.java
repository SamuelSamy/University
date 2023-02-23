package Model.Collections;

import java.util.Map;

public interface MyHeapInterface<T> {
    int allocate(T value);
    T deallocate(int address);

    T get(int address);
    void set(int address, T value);
    boolean exists(int address);

    Map<Integer, T> getHeap();
    void setHeap(Map<Integer, T> map);

    Map<Integer, T> getContent();
}
