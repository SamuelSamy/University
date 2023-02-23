#include "admin_repository.h"
#include "errors.h"

AdminRepository::AdminRepository()
{
    
}

AdminRepository::AdminRepository(const AdminRepository& repo)
{
    this->data = repo.data;
}

Coat& AdminRepository::operator[](int index)
{
    return this->data[index];
}

AdminRepository& AdminRepository::operator=(const AdminRepository& v)
{
    this->data = v.data;
    return *this;
}

Vector<Coat> AdminRepository::get_coats_by_size(int size)
{
    if (size == 0)
    {
        return this->data;
    }

    Vector<Coat> coats;

    for (int i = 0; i < this->data.size(); i++)
    {
        if (this->data[i].get_size() == size)
        {
            coats.push_back(this->data[i]);
        }
    }

    return coats;
}

Coat AdminRepository::get_coat_by_ID(int ID)
{
    for (int i = 0; i < this->length(); i++)
    {
        if (this->data[i].get_ID() == ID)
        {
            return this->data[i];
        }
    }

    throw Errors().CoatNotFound;
}

int AdminRepository::length()
{
    return this->data.size();
}

void AdminRepository::add(Coat element)
{
    for (int i = 0; i < this->length(); i++)
    {
        if (this->data[i].get_ID() == element.get_ID())
        {
            throw Errors().DuplicateID;
        }
    }

    this->data.push_back(element);
}

void AdminRepository::remove(int ID)
{
    int index = -1;

    for (int i = 0; i < this->length(); i++)
    {
        if (this->data[i].get_ID() == ID)
        {
            index = i;
        }
    }

    if (index == -1)
    {
        throw Errors().CoatNotFound;
    }

    if (this->data[index].get_quantity() != 0)
    {
        throw Errors().CoatNotSoldOut;
    }

    this->data.remove(index);
}

void AdminRepository::update(int ID, Coat newData)
{
    int index = -1;

    for (int i = 0; i < this->length(); i++)
    {
        if (this->data[i].get_ID() == ID)
        {
            index = i;
        }
    }
    
    if (index == -1)
    {
        throw Errors().CoatNotFound;
    }

    this->data[index] = newData;
}

AdminRepository::~AdminRepository()
{
}
