#include "Service.h"
#include <algorithm>
#include "FilePlaylist.h"
#include "RepositoryExceptions.h"
#include "Actions.h"

using namespace std;

void Service::addSongToRepository(const std::string& artist, const std::string& title, double minutes, double seconds, const std::string& source)
{
	Song s{ artist, title, Duration{minutes, seconds}, source };
	this->validator.validate(s);
	this->repo.addSong(s);
	
	this->action_index++;
	this->actions.erase(this->actions.begin() + this->action_index, this->actions.end());
	std::unique_ptr<Action> action_ptr = std::make_unique<ActionAdd>(ActionAdd{s, this->repo});
	this->actions.push_back(std::move(action_ptr));
}

void Service::removeSongFromRepository(const std::string & artist, const std::string & title)
{
	Song s = this->repo.findByArtistAndTitle(artist, title);
	this->repo.removeSong(s);

	this->action_index++;
	this->actions.erase(this->actions.begin() + this->action_index + 1, this->actions.end());
	std::unique_ptr<Action> action_ptr = std::make_unique<ActionRemove>(ActionRemove{ s, this->repo });
	this->actions.push_back(std::move(action_ptr));
}

void Service::addSongToPlaylist(const Song& song)
{
	if (this->playList == nullptr)
		return;
	this->playList->add(song);
}

void Service::addAllSongsByArtistToPlaylist(const std::string& artist)
{
	vector<Song> songs = this->repo.getSongs();
	int nSongs = static_cast<int>(count_if(songs.begin(), songs.end(),
		[artist](const Song& s)
		{
			return s.getArtist() == artist;
		}));

	vector<Song> songsByArtist(nSongs);
	copy_if(songs.begin(), songs.end(), songsByArtist.begin(),
		[artist](const Song& s)
		{
			return s.getArtist() == artist;
		});

	for (auto s : songsByArtist)
		this->playList->add(s);
}

void Service::startPlaylist()
{
	if (this->playList == nullptr)
		return;
	this->playList->play();
}

void Service::updateSong(const std::string& artist, const std::string& title, double minutes, double seconds, const std::string& source)
{
	Song s = this->repo.findByArtistAndTitle(artist, title);
	Song newSong{ artist, title, Duration{minutes, seconds}, source };

	this->repo.removeSong(s);
	this->repo.addSong(newSong);

	this->action_index++;
	this->actions.erase(this->actions.begin() + this->action_index, this->actions.end());
	std::unique_ptr<Action> action_ptr = std::make_unique<ActionUpdate>(ActionUpdate{ s, newSong, this->repo });
	this->actions.push_back(std::move(action_ptr));

}

void Service::nextSongPlaylist()
{
	if (this->playList == nullptr)
		return;
	this->playList->next();
}

void Service::savePlaylist(const std::string& filename)
{
	if (this->playList == nullptr)
		return;

	this->playList->setFilename(filename);
	this->playList->writeToFile();
}

void Service::openPlaylist() const
{
	if (this->playList == nullptr)
		return;

	this->playList->displayPlaylist();
}

bool Service::undoAction()
{
	if (this->action_index == -1)
	{
		return false;
	}

	this->actions[action_index]->executeUndo();
	this->action_index--;
	return true;
}

bool Service::redoAction()
{
	if (this->action_index == this->actions.size() - 1)
	{
		return false;
	}

	this->action_index++;
	this->actions[action_index]->executeRedo();
	return true;
}
