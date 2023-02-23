#include "user_repository.h"
#include "errors.h"

UserRepository::UserRepository(UserRepository* repo)
{
    this->basket = repo->basket;
}

std::pair<int, int> UserRepository::operator[](int index)
{
    return this->basket[index];
}

UserRepository& UserRepository::operator=(const UserRepository& v)
{
    this->basket = v.basket;
    return *this;
}

int UserRepository::length()
{
    return (int)this->basket.size();
}

void UserRepository::add(int ID)
{
    std::vector<std::pair<int, int>>::iterator it = std::find_if(this->basket.begin(), this->basket.end(), [ID](std::pair<int, int> entry) {return entry.first == ID; });
    if (it == this->basket.end())
    {
        this->basket.push_back(std::make_pair(ID, 1));
        return;
    }
    (*it).second++;
}



std::vector<std::pair<int, int>> UserRepository::get_vector()
{
    return this->basket;
}

UserRepository::~UserRepository()
{
}
