#include "admin_service.h"
#include "admin_repository.h"
#include "validators.h"

AdminService::AdminService(Repository* repo)
{
    this->repository = repo;
    this->lastID = repo->length();
}

AdminService::AdminService(const AdminService& service)
{
    this->repository = service.repository;
    this->lastID = service.lastID;
}

void AdminService::add(int size, int price, int quantity, std::string color, std::string photo)
{
    Coat coat{ this->lastID, size, price, quantity, color, photo };

    CoatValidator::validate(&coat);

    this->repository->add(coat);
    this->lastID++;
}

void AdminService::remove(int ID)
{
    this->repository->remove(ID);
}

Coat AdminService::get_coat_by_ID(int ID)
{
    return this->repository->get_coat_by_ID(ID);
}

std::vector<Coat> AdminService::get_coats_by_size(int size)
{
    return this->repository->get_coats_by_size(size);
}

void AdminService::update(int ID, int size, int price, int quantity, std::string color, std::string photo, int changes)
{
    Coat oldCoat = this->get_coat_by_ID(ID);

    if ((changes & 1) == 0)
    {
        size = oldCoat.get_size();
    }

    if ((changes & 2) == 0)
    {
        price = oldCoat.get_price();
    }

    if ((changes & 4) == 0)
    {
        quantity = oldCoat.get_quantity();
    }

    if ((changes & 8) == 0)
    {
        color = oldCoat.get_color();
    }

    if ((changes & 16) == 0)
    {
        photo = oldCoat.get_photo();
    }

    Coat newCoat{ ID, size, price, quantity, color, photo };
    CoatValidator::validate(&newCoat);

    this->repository->update(ID, newCoat);
}

Repository* AdminService::get_repo()
{
    return this->repository;
}


AdminService::~AdminService()
{
}
