#pragma once

#include "coat.h"
#include "admin_repository.h"

class Action
{
public:
    virtual void executeUndo() = 0;
    virtual void executeRedo() = 0;

    virtual ~Action() {};
};

class ActionAdd : public Action
{

private:
    Coat addedCoat;
    Repository& repo;

public:
    ActionAdd(Coat coat, Repository& _repo);

    void executeUndo() override;
    void executeRedo() override;

    ~ActionAdd() {};
};

class ActionRemove : public Action
{
private:
    Coat removedCoat;
    Repository& repo;

public:

    ActionRemove(Coat coat, Repository& _repo);

    void executeUndo() override;
    void executeRedo() override;

    ~ActionRemove() {};
};

class ActionUpdate : public Action
{
private:
    Coat oldCoat;
    Coat newCoat;
    Repository& repo;

public:
    ActionUpdate(Coat oldCoat, Coat newCoat, Repository& _repo);

    void executeUndo() override;
    void executeRedo() override;

    ~ActionUpdate() {};
};