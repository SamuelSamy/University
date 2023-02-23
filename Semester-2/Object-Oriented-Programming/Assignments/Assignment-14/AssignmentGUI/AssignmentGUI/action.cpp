#include "action.h"


ActionAdd::ActionAdd(Coat coat, Repository& _repo) : repo(_repo)
{
    this->addedCoat = coat;
}

void ActionAdd::executeUndo()
{
    repo.remove(this->addedCoat.get_ID(), true);
}

void ActionAdd::executeRedo()
{
    repo.add(this->addedCoat);
}



ActionRemove::ActionRemove(Coat coat, Repository& _repo) : repo(_repo)
{
    this->removedCoat = coat;
}

void ActionRemove::executeUndo()
{
    repo.add(this->removedCoat);
}

void ActionRemove::executeRedo()
{
    repo.remove(this->removedCoat.get_ID(), true);
}



ActionUpdate::ActionUpdate(Coat oldCoat, Coat newCoat, Repository& _repo) : repo(_repo)
{
    this->oldCoat = oldCoat;
    this->newCoat = newCoat;
}

void ActionUpdate::executeUndo()
{
    repo.remove(newCoat.get_ID(), true);
    repo.add(oldCoat);
}

void ActionUpdate::executeRedo()
{
    repo.remove(oldCoat.get_ID(), true);
    repo.add(newCoat);
}