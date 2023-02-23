#pragma once
#include "Planet.h"

// version 2 - operation contains functions (function pointers) for 
// the undo and redo functions, as well as the parameters for them
//typedef int (*undoPtrFct)(void*, void*);
//
//typedef struct
//{
//	void* obj;
//	undoPtrFct undoFct, redoFct;
//} Operation;


typedef enum {
	ADD, 
	DELETE,
	UPDATE
} opType;

typedef struct {	
	opType type;
	Planet* p;
} Operation;

Operation* createOperation(opType type, Planet* p);
void destroyOperation(Operation* o);

opType getOpType(Operation* o);
Planet* getOpObject(Operation* o);
