#include "admin_service.h"
#include "admin_repository.h"
#include "validators.h"

AdminService::AdminService(Repository* repo)
{
    this->repository = repo;
    this->lastID = repo->get_last_ID() + 1;
    this->actionIndex = -1;
}

AdminService::AdminService(const AdminService& service)
{
    this->repository = service.repository;
    this->lastID = service.lastID;
    this->actionIndex = service.actionIndex;
}

void AdminService::add(int size, int price, int quantity, std::string color, std::string photo)
{
    Coat coat{ this->lastID, size, price, quantity, color, photo };

    CoatValidator::validate(&coat);

    this->repository->add(coat);
    this->lastID++;

    this->actionIndex++;
    this->actions.erase(this->actions.begin() + this->actionIndex, this->actions.end());
    std::unique_ptr<Action> action_ptr = std::make_unique<ActionAdd>(ActionAdd{ coat, *this->repository});
    this->actions.push_back(std::move(action_ptr));
}

void AdminService::remove(int ID)
{
    Coat c = this->repository->remove(ID, false);

    this->actionIndex++;
    this->actions.erase(this->actions.begin() + this->actionIndex, this->actions.end());
    std::unique_ptr<Action> action_ptr = std::make_unique<ActionRemove>(ActionRemove{ c, *this->repository });
    this->actions.push_back(std::move(action_ptr));
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
    Coat oldCoat = this->repository->get_coat_by_ID(ID);
    Coat newCoat{ ID, size, price, quantity, color, photo };
    CoatValidator::validate(&newCoat);
    this->repository->update(ID, newCoat);

    this->actionIndex++;
    this->actions.erase(this->actions.begin() + this->actionIndex, this->actions.end());
    std::unique_ptr<Action> action_ptr = std::make_unique<ActionUpdate>(ActionUpdate{ oldCoat, newCoat, *this->repository });
    this->actions.push_back(std::move(action_ptr));
}

Repository* AdminService::get_repo()
{
    return this->repository;
}


bool AdminService::undoAction()
{
    if (this->actionIndex == -1)
    {
        return false;
    }

    this->actions[actionIndex]->executeUndo();
    this->actionIndex--;
    return true;
}

bool AdminService::redoAction()
{
    if (this->actionIndex == this->actions.size() - 1)
    {
        return false;
    }

    this->actionIndex++;
    this->actions[actionIndex]->executeRedo();
    return true;
}

AdminService::~AdminService()
{
}
