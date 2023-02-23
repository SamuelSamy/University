#pragma once
#include "Planet.h"
#include "DynamicArray.h"

typedef struct
{
	DynamicArray* planets;
} PlanetRepo;

/// <summary>
/// Creates a PlanetRepo.
/// </summary>
PlanetRepo* createRepo();

/// <summary>
/// Destroys a given planet repository. The memory is freed.
/// </summary>
void destroyRepo(PlanetRepo* v);

// Returns 0 - if the planet was successfully added or -1 otherwise.
int addPlanet(PlanetRepo* repo, Planet* p);

int getRepoLength(PlanetRepo* repo);

int deletePlanet(PlanetRepo* repo, char* name);

Planet* findByName(PlanetRepo* repo, char* name);

Planet* getPlanetOnPos(PlanetRepo* v, int pos);
