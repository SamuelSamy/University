#include "PlanetRepository.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

PlanetRepo* createRepo()
{
	PlanetRepo* v = (PlanetRepo*)malloc(sizeof(PlanetRepo));
	if (v == NULL)
		return NULL;
	v->planets = createDynamicArray(CAPACITY, &destroyPlanet);

	return v;
}

void destroyRepo(PlanetRepo* v)
{
	if (v == NULL)
		return;

	// !!! If the dynamic vector takes responsibility of these planets, then the memory can be deallocated in the function "destroy" in DynamicArray.c.
	// Otherwise, the repository should deallocate the memory for the planets.
	/*for (int i = 0; i < getLength(v->planets); i++)
	{
		Planet* p = get(v->planets, i);
		destroyPlanet(p);
	}*/

	// then destroy the dynamic array
	destroy(v->planets);
	free(v);
}

int findPosOfPlanet(PlanetRepo * v, char * name)
{
	if (v == NULL || name == NULL)
		return -1;

	// TODO
	return -1;
}

Planet* find(PlanetRepo* v, char* name)
{
	if (v == NULL || name == NULL)
		return NULL;
	// TODO
	return NULL;
}

int addPlanet(PlanetRepo* v, Planet* p)
{
	if (v == NULL || p == NULL)
		return -1;

	add(v->planets, p);
	
	return 0;
}


int deletePlanet(PlanetRepo* repo, char* name) {
	if (repo == NULL || name == NULL) {
		return -1;
	}
	Planet* p = findByName(repo, name);
	for (int i = 0; i < getLength(repo->planets); i++) {
		if (get(repo->planets, i) == p) {
			delete(repo->planets, i);
			return 0;
		}
	}

	return -1;
}


int getRepoLength(PlanetRepo* v)
{
	if (v == NULL)
		return -1;

	return getLength(v->planets);
}

Planet* getPlanetOnPos(PlanetRepo* v, int pos)
{
	if (v == NULL)
		return NULL;
	return get(v->planets, pos);
}


Planet* findByName(PlanetRepo* repo, char* name) {
	if (repo == NULL || name == NULL) {
		return NULL;
	}

	for (int i = 0; i < getLength(repo->planets); i++) {
		Planet* p = get(repo->planets, i);
		if (strcmp(getName(p), name) == 0) {
			return p;
		}
	}

	return NULL;
}


// Tests
void testAdd()
{
	PlanetRepo* v = createRepo();

	Planet* p1 = createPlanet("Wolf 1061 c", "terrestrial", 13.8);
	addPlanet(v, p1);
	assert(getRepoLength(v) == 1);
	assert(strcmp(getName(getPlanetOnPos(v, 0)), "Wolf 1061 c") == 0);

	Planet* p2 = createPlanet("HAT-P-26b", "Neptune-like", 450);
	assert(addPlanet(v, p2) == 1);
	assert(getRepoLength(v) == 2);

	// now try to add the same planet again -> add must return 0
	assert(addPlanet(v, p2) == 0);

	// destroy the test repository
	destroyRepo(v);

	// Option 1:
	// if the repository does not store copies, then the memory allocated for the planets will be deallocated
	// in the repository and the two planets should not be deallocated here.

	// Option 2:
	// otherwise, if the repository makes copied, then the memory allocated for the planets must be freed
	/*destroyPlanet(p1);
	destroyPlanet(p2);*/
}

void testsPlanetRepo()
{
	testAdd();
}