#include <iostream>
#include <vector>

class MainInstance
{
    public:
        int age;
        int height;

    public:

        MainInstance(int age = 0, int height = 0)
        {
            this->age = age;
            this->height = height;
        }
};


class Instance0 : public MainInstance
{
    public:

        Instance0(const MainInstance* inst)
        {
            this->age = inst->age;
            this->height = inst->height;
        }

    friend std::ostream& operator<<(std::ostream& os, const Instance0& inst);
};


class Instance1 : public MainInstance
{  
    public:
        Instance1(const MainInstance* inst)
        {
            this->age = inst->age;
            this->height = inst->height;
        }

    friend std::ostream& operator<<(std::ostream& os, const Instance1& inst);
};


inline std::ostream& operator<<(std::ostream& os, const Instance0& inst)
{
    os << inst.age << ", " << inst.height << "\n";
    return os;
}


inline std::ostream& operator<<(std::ostream& os, const Instance1& inst)
{
    os << inst.age << " - " << inst.height << "\n";
    return os;
}

class ConsoleStream : std::ostream
{

};

int main()
{
    MainInstance m0{ 10, 20 };
    MainInstance m1{ 5, 25 };
    MainInstance m2{ 7, 36 };

    std::vector<MainInstance*> vec;

    vec.push_back(&m0);
    vec.push_back(&m1);
    vec.push_back(&m2);

    for (const MainInstance* instance : vec)
    {
        Instance0 castedInst = (Instance0)instance;
        std::cout << castedInst;
    }

    std::cout << "\n\n";

    for (const MainInstance* instance : vec)
    {
        Instance1 castedInst = (Instance1)instance;
        std::cout << castedInst;
    }

    return 0;
}
