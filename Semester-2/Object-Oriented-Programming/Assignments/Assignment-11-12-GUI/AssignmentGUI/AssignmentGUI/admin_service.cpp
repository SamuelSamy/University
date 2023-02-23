#include "admin_service.h"
#include "admin_repository.h"
#include "validators.h"

AdminService::AdminService(Repository* repo)
{
    this->repository = repo;
    this->lastID = repo->get_last_ID() + 1;
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

void AdminService::update(int ID, int size, int price, int quantity, std::string color, std::string photo)
{
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
