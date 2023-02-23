#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#include "Vector.h"
#include "ErrorLevels.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int resize(Vector* vector)
{
    if (vector == NULL)
    {
        return MemoryIssue;
    }

    if (vector->elements == NULL)
    {
        return MemoryIssue;
    }

    TElem* temp = malloc(sizeof(TElem) * vector->capacity * 2);

    if (temp == NULL)
    {
        return MemoryIssue;
    }

    memcpy(temp, vector->elements, sizeof(TElem) * vector->capacity);
    free(vector->elements);
    vector->elements = temp;
    vector->capacity *= 2;
    return Success;
}

Vector* createVector(int capacity, destroyFunc destroy)
{
    Vector* vector = malloc(sizeof(Vector));

    if (vector == NULL)
        return NULL;

    vector->elements = malloc(sizeof(TElem) * capacity);
    if (vector->elements == NULL)
    {
        free(vector);
        return NULL;
    }

    vector->capacity = capacity;
    vector->size = 0;
    vector->destroy = destroy;
    
    return vector;
}

void destroyVector(Vector* vector)
{
    if (vector == NULL)
    {
        return;
    }

    if (vector->elements == NULL)
    {
        return;
    }

    vectorClear(vector);

    free(vector->elements);
    free(vector);
    vector = NULL;
}

int vectorInsertTail(Vector* vector, TElem element)
{
    if (vector == NULL)
    {
        return MemoryIssue;
    }

    if (vector->size >= vector->capacity)
    {
        int retVal = resize(vector);
        if (retVal != 0)
        {
            return retVal;
        }
    }

    vector->elements[vector->size++] = element;
    return Success;
}

//int vectorInsertHead(Vector* vector, TElem element)
//{
//    if (vector == NULL)
//    {
//        return MemoryIssue;
//    }
//
//    if (vector->size >= vector->capacity)
//    {
//        int retVal = resize(vector);
//        if (retVal != 0)
//        {
//            return retVal;
//        }
//    }
//
//    for (int i = vector->size; i > 0; i--)
//    {
//        vector->elements[i] = vector->elements[i - 1];
//    }
//
//    vector->elements[0] = element;
//    vector->size++;
//    return Success;
//}

int vectorInsetAtIndex(Vector* vector, int index, TElem element)
{
    if (vector == NULL)
    {
        return MemoryIssue;
    }

    if (index < 0 || index > vector->size)
    {
        return IndexNotFound;
    }

    if (vector->size >= vector->capacity)
    {
        int retVal = resize(vector);
        if (retVal != 0)
        {
            return retVal;
        }
    }

    for (int i = vector->size; i > index; i--)
    {
        vector->elements[i] = vector->elements[i - 1];
    }

    vector->elements[index] = element;
    vector->size++;
    return Success;
}

int vectorGetValueByIndex(Vector* vector, int index, TElem* value)
{
    if (vector == NULL)
    {
        return MemoryIssue;
    }

    if (index < 0 || index >= vector->size)
    {
        return IndexNotFound;
    }

    *value = vector->elements[index];
    return Success;
}

int vectorRemoveByIndex(Vector* vector, int index)
{
    if (vector == NULL)
    {
        return MemoryIssue;
    }

    if (index < 0 || index >= vector->size)
    {
        return IndexNotFound;
    }

    vector->destroy(vector->elements[index]);

    for (int i = index; i < vector->size - 1; i++)
    {
        vector->elements[i] = vector->elements[i + 1];
    }

    vector->size--;
    return Success;
}

int vectorGetSize(Vector* vector)
{
    if (vector == NULL)
    {
        return MemoryIssue;
    }

    return vector->size;
}

int vectorClear(Vector* vector)
{
    if (vector == NULL)
    {
        return MemoryIssue;
    }

    for (int i = 0; i < vector->size; i++)
    {
        vector->destroy(vector->elements[i]);
    }

    vector->size = 0;
    return Success;
}

void vectorSwap(Vector* vector, int index0, int index1)
{
    if (vector == NULL)
    {
        return;
    }

    int len = vector->size;

    if (index0 < 0 || index0 >= len || index1 < 0 || index1 >= len)
    {
        return;
    }

    TElem* temp = vector->elements[index0];
    vector->elements[index0] = vector->elements[index1];
    vector->elements[index1] = temp;
}