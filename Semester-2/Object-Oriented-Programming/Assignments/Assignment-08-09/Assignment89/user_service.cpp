#include "user_service.h"
#include "user_repository.h"


UserService::UserService(const UserService& service)
{
    this->repository = service.repository;
    this->lastID = service.lastID;
}

void UserService::add(int ID)
{
    this->repository->add(ID);
    this->lastID++;
}

UserRepository* UserService::get_repo()
{
    return this->repository;
}

std::string UserService::get_file_name()
{
    return this->repository->get_file_name();
}

int UserService::get_total_price()
{
    return this->repository->get_total_price();
}


UserService::~UserService()
{

}
