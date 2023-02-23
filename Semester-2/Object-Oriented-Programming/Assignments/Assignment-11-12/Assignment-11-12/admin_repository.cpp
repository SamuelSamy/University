#include "admin_repository.h"
#include "errors.h"
#include <algorithm>

Repository::Repository()
{
    
}


Repository::Repository(const Repository& repo)
{
    this->data.clear();
    this->data.resize(0);

    for (Coat* c : repo.data)
    {
        Coat* new_coat = new Coat(*c);
        this->data.push_back(new_coat);
    }
}

Repository::Repository(Repository* repo)
{
    this->data.clear();
    this->data.resize(0);

    for (Coat* c : (*repo).data)
    {
        Coat* new_coat = new Coat(*c);
        this->data.push_back(new_coat);
    }
}

Coat* Repository::operator[](int index)
{
    return this->data[index];
}

Repository& Repository::operator=(const Repository& v)
{
    this->data.clear();
    this->data.resize(0);

    for (Coat* c : v.data)
    {
        Coat* new_coat = new Coat(*c);
        this->data.push_back(new_coat);
    }

    return *this;
}

std::vector<Coat> Repository::get_coats_by_size(int size)
{
    if (size == 0)
    {
        std::vector<Coat> coats;
        for (Coat* c : this->data)
        {
            coats.push_back(*c);
        }
        return coats;
    }

    std::vector<Coat*> ptr_coats{this->data.size()};
    std::vector<Coat*>::iterator it = std::copy_if(this->data.begin(), this->data.end(), ptr_coats.begin(), [size](Coat* coat) {return coat->get_size() == size; });
    ptr_coats.resize(std::distance(ptr_coats.begin(), it));

    std::vector<Coat> coats;

    for (Coat* c : ptr_coats)
    {
        coats.push_back(*c);
    }

    return coats;
}

Coat Repository::get_coat_by_ID(int ID)
{
    std::vector<Coat*>::iterator it = std::find_if(this->data.begin(), this->data.end(), [ID](Coat* coat) {return coat->get_ID() == ID; });
    if (it == this->data.end())
    {
        throw RepositoryException(CoatNotFound);
    }
    return Coat{ **it };
}

int Repository::length()
{
    return (int)this->data.size();
}

void Repository::add(Coat element)
{
    Coat* new_coat = new Coat{ element };

    std::vector<Coat*>::iterator it = std::find_if(this->data.begin(), this->data.end(), [new_coat](Coat* coat) {return new_coat->get_ID() == coat->get_ID(); });
    if (it != this->data.end())
    {
        delete new_coat;
        throw RepositoryException(DuplicateID);
    }

    this->data.push_back(new_coat);
}

void Repository::remove(int ID)
{
    std::vector<Coat*>::iterator it = std::find_if(this->data.begin(), this->data.end(), [ID](Coat* coat) {return coat->get_ID() == ID; });
    
    if (it == this->data.end())
    {
        throw RepositoryException(CoatNotFound);
    }
  
    if ((*it)->get_quantity() != 0)
    {
        throw RepositoryException(CoatNotSoldOut);
    }

    delete *it;
    this->data.erase(it);
}

void Repository::update(int ID, Coat newData)
{
    std::vector<Coat*>::iterator it = std::find_if(this->data.begin(), this->data.end(), [ID](Coat* coat) {return coat->get_ID() == ID; });
    if (it == this->data.end())
    {
        throw RepositoryException(CoatNotFound);
    }
    (**it) = newData;
}

void Repository::write() const
{
}

void Repository::read()
{
}

Repository::~Repository()
{
    for (Coat* c : this->data)
    {
        delete c;
    }
}
