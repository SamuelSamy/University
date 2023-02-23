#include "observer.h"

void Subject::registerObserver(Observer* observer)
{
    this->observers.push_back(observer);
}

void Subject::unregisterObsedrver(Observer* observer)
{
    auto it = std::find(this->observers.begin(), this->observers.end(), observer);
    if (it != this->observers.end())
    {
        this->observers.erase(it);
    }
}

void Subject::notify()
{
    for (Observer* observer : this->observers)
    {
        observer->update();
    }
}
