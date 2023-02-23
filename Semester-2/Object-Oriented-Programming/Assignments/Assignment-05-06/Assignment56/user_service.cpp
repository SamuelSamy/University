#include "user_service.h"
#include "user_repository.h"


UserService::UserService(const UserService& service)
{
    this->repository = service.repository;
    this->lastID = service.lastID;
}

void UserService::add(int ID)
{
    this->repository.add(ID);
    this->lastID++;
}

UserRepository UserService::get_repo()
{
    return this->repository;
}

int UserService::get_total_price(AdminService adminService)
{
    return this->repository.get_total_price(adminService.get_repo());
}


UserService::~UserService()
{

}
