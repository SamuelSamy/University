#include "user_repository.h"
#include "errors.h"

UserRepository::UserRepository()
{

}

UserRepository::UserRepository(const UserRepository& repo)
{
    this->data = repo.data;
}

std::pair<int, int> UserRepository::operator[](int index)
{
    return this->data[index];
}

UserRepository& UserRepository::operator=(const UserRepository& v)
{
    this->data = v.data;
    return *this;
}

int UserRepository::length()
{
    return this->data.size();
}

void UserRepository::add(int ID)
{
    for (int i = 0; i < this->length(); i++)
    {
        if (this->data[i].first == ID)
        {
            this->data[i].second++;
            return;
        }
    }

    this->data.push_back(std::make_pair(ID, 1));
}

int UserRepository::get_total_price(AdminRepository adminRepo)
{
    int total = 0;

    for (int i = 0; i < this->length(); i++)
    {
        Coat coat = adminRepo.get_coat_by_ID(this->data[i].first);
        total += coat.get_price() * this->data[i].second;
    }

    return total;
}

UserRepository::~UserRepository()
{
}
