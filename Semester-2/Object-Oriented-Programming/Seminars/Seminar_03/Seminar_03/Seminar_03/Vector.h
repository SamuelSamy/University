#pragma once

typedef void* TElem;

typedef void (*destroyFunc)(void*);

typedef struct _Vector {
	TElem* elements;
	int size;
	int capacity;
	destroyFunc destroy;
} Vector;

Vector* createVector(int capacity, destroyFunc destroy);
void destroyVector(Vector* vector);

int vectorInsertTail(Vector* vector, TElem element);
int vectorInsertHead(Vector* vector, TElem element);
int vectorInsetAtIndex(Vector* vector, int index, TElem element);

int vectorGetValueByIndex(Vector* vector, int index, TElem* value);
int vectorRemoveByIndex(Vector* vector, int index);

int vectorGetSize(Vector* vector);

int vectorClear(Vector* vector);

void vectorSwap(Vector* vector, int index0, int index1);