#include "Service.h"
#include <stdlib.h>
#include <string.h>
#include "Operation.h"

Service* createService(PlanetRepo* r)
{
	Service* s = (Service*)malloc(sizeof(Service));
	if (s == NULL)
		return NULL;
	s->repo = r;
	s->undoStack = createDynamicArray(10, &destroyOperation);
	
	return s;
}

void destroyService(Service* s)
{
	// first destroy the repository inside
	destroyRepo(s->repo);

	destroy(s->undoStack);

	// then free the memory
	free(s);
}

int addPlanetServ(Service* s, char* name, char* type, double distanceFromEarth)
{
	Planet* p = createPlanet(name, type, distanceFromEarth);
	
	int res = addPlanet(s->repo, p);
	if (res == 0) // the planet was added
	{
		Operation* op = createOperation(ADD, p);
		if (op == NULL)
			return -1;
		add(s->undoStack, op);
	}

	return res;
}


int deletePlanetServ(Service* s, char* name) {

	// create the operation before deleting the planet (memory of planet will be freed)
	// OR save a copy of the planet before deleting it

	Planet* p = findByName(s->repo, name);
	Operation* op = createOperation(DELETE, p);
	if (op == NULL)
		return -1;
	add(s->undoStack, op);

	return deletePlanet(s->repo, name);
}

int undo(Service* s)
{
	if (s == NULL)
		return -1; // error

	int stackSize = getLength(s->undoStack);
	if (stackSize == 0)
		return 1; // no more undos
	Operation* op = get(s->undoStack, stackSize - 1);
	if (op == NULL)
		return -1;
	Planet* p = getOpObject(op);
	if (p == NULL)
		return -1;

	if (getOpType(op) == ADD)
		deletePlanet(s->repo, getName(p));
	else if (getOpType(op) == DELETE)
		addPlanet(s->repo, copyPlanet(p)); // must have a copy, because p
										   // will be destroyed when deleting
										   // the operation from the stack

	// remove the operation from "the stack"
	// should be added to the redo stack
	delete(s->undoStack, stackSize - 1);
	return 0;
}


PlanetRepo* getRepo(Service* s)
{
	return s->repo;
}