#pragma once

#include <iostream>
#include <vector>
#include <algorithm>

template <typename Type>
class Comparator
{
    public:
        virtual bool compare(Type e0, Type e1) = 0;
};


template <typename Type>
class ComparatorAscendingBySize : public Comparator<Type>
{
    public:
        bool compare(Type e0, Type e1) override;
};

template <typename Type>
class ComparatorDescendingByPrice : public Comparator<Type>
{
    public:
        bool compare(Type e0, Type e1) override;
};

template <typename Type>
inline bool ComparatorAscendingBySize<Type>::compare(Type e0, Type e1)
{
    return e0.get_size() <= e1.get_size();
}

template <typename Type>
inline bool ComparatorDescendingByPrice<Type>::compare(Type e0, Type e1)
{
    return e0.get_price() >= e1.get_price();
}



template <typename Type>
void genericSort(std::vector<Type>& v, Comparator<Type>* cmp)
{
    std::sort(v.begin(), v.end(), [cmp](Type e0, Type e1) { return cmp->compare(e0, e1); });
}

