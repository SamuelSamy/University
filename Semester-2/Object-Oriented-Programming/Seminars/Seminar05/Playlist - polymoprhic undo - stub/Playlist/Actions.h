#pragma once

#include "Song.h"
#include "Repository.h"

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
    Song addedSong;
    Repository& repo;

public:

    ActionAdd(Song song, Repository& _repo);

    void executeUndo() override;
    void executeRedo() override;

    ~ActionAdd() {};
};

class ActionRemove : public Action
{
private:
    Song removedSong;
    Repository& repo;

public:

    ActionRemove(Song song, Repository& _repo);

    void executeUndo() override;
    void executeRedo() override;

    ~ActionRemove() {};
};

class ActionUpdate : public Action
{
private:
    Song oldSong;
    Song newSong;
    Repository& repo;

public:

    ActionUpdate (Song oldSong, Song newSong, Repository& _repo);

    void executeUndo() override;
    void executeRedo() override;

    ~ActionUpdate() {};
};