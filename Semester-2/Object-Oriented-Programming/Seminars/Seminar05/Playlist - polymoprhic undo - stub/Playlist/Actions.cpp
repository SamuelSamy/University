#include "Actions.h"


ActionAdd::ActionAdd(Song song, Repository& _repo) : repo(_repo)
{
    addedSong = song;
}

void ActionAdd::executeUndo()
{
    repo.removeSong(addedSong);
}

void ActionAdd::executeRedo()
{
    repo.addSong(addedSong);
}

ActionRemove::ActionRemove(Song song, Repository& _repo) : repo(_repo)
{
    removedSong = song;
}

void ActionRemove::executeUndo()
{
    repo.addSong(removedSong);
}

void ActionRemove::executeRedo()
{
    repo.removeSong(removedSong);
}

ActionUpdate::ActionUpdate(Song oldSong, Song newSong, Repository& _repo) : repo(_repo)
{
    this->oldSong = oldSong;
    this->newSong = newSong;
}

void ActionUpdate::executeUndo()
{
    this->repo.removeSong(newSong);
    this->repo.addSong(oldSong);
}

void ActionUpdate::executeRedo()
{
    this->repo.removeSong(oldSong);
    this->repo.addSong(newSong);
}
