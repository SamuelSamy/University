#pragma once
#include "PlanetRepository.h"

typedef struct
{
	PlanetRepo* repo;
	DynamicArray* undoStack;
} Service;

Service* createService(PlanetRepo* r);
void destroyService(Service* s);

/// <summary>
/// Adds a planet to the repository of planets.
/// </summary>
/// <param name="s">Pointer to theService.</param>
/// <param name = "name">A string, the name of the planet.</param>
/// <param name = "type">A string, the planet's type.</param>
/// <param name = "distanceFromEarth">Double, the distance from the planet to Earth, in light years.</param>
/// <returns>1 - if the planet was sucessfully added; 0 - if the planet could not be added, as another planet with the same symbol already exists.</returns>
int addPlanetServ(Service* s, char* name, char* type, double distanceFromEarth);

PlanetRepo* getRepo(Service* s);

int deletePlanetServ(Service* s, char* name);

/*
* Return 0 - succes; -1 - error; 1 - no more undos
*/
int undo(Service* s);