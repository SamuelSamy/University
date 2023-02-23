#pragma once

#include <vector>

class Observer
{
public:
    virtual void update() = 0;
    virtual ~Observer() {}
};

class Subject
{
private:
    std::vector<Observer*> observers;

public:
    void registerObserver(Observer* observer);
    void unregisterObsedrver(Observer* observer);
    void notify();
};