#include <iostream>
#include <vector>

template <typename T>
class grades
{
private:
    std::vector<int> oop;
    std::vector<int> fp;
    bool isOopMark;

public:
    grades() 
    {
        isOopMark = false;
    };

    ~grades() {};

    grades& operator+(T mark)
    {
        if (isOopMark)
        {
            oop.push_back(mark);
            isOopMark = false;
            return this;
        }

        fp.push_back(mark);
        isOopMark = true;
        return this;
    }


    grades& operator=(grades _grades)
    {
        this->oop = _grades.oop;
        this->fp = _grades.fp;
        this->isOopMark = _grades.isOopMark;
        return this;
    }

    int getNRGrages()
    {
        return oop.size() + fp.size();
    }

    class iterator
    {
    private:
        T* ptr;

    public:
        iterator(T* ptrT)
        {
            return &ptrT;
        }
        
        iterator operator++()
        {
            this->ptr++;

            if (this->ptr == oop.end())
            {
                this->ptr = fp.begin();
            }

            return *this;
        }

        iterator& operator++(int)
        {
            iterator temp = *this;
            this->ptr++;

            if (this->ptr == oop.end())
            {
                this->ptr = fp.begin();
            }

            return temp;
        }

        T& operator*(const iterator& it)
        {
            return *this->ptr;
        }
        
        bool operator!=(const iterator& it)
        {
            return it.ptr != this->ptr;
        }
        
    };

    iterator begin()
    {
        return iterator{ this->oop };
    }

    iterator end()
    {
        return iterator{ this->fp + this->fp.size() };
    }

    
};

int main()
{
    grades<int> myg;

    myg = myg + 10; // adaugam nota 10 la OOP
    myg = myg + 9; //adaugam nota 9 la FP

    double avg = 0.0;

    for (auto g : myg) { //iteram toate notele
        avg += g;
    }

    std::cout << avg / myg.getNRGrages();//compute average

    return 0;
}