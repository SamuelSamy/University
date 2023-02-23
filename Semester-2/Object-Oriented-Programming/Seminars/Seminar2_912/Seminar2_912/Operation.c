#include "Operation.h"
#include "Planet.h"
#include <stdlib.h>

Operation* createOperation(opType type, Planet* p) {
	Operation* op = malloc(sizeof(Operation));
	op->type = type;

	Planet* copyOfP = copyPlanet(p);
	op->p = copyOfP;

	return op;
}

void destroyOperation(Operation* o) {
	if (o == NULL)
		return;

	destroyPlanet(o->p);
	free(o);
}

opType getOpType(Operation* o) {
	if (o == NULL)
		return -1;
	return o->type;
}

Planet* getOpObject(Operation* o) {
	if (o == NULL)
		return NULL;
	return o->p; 
}
