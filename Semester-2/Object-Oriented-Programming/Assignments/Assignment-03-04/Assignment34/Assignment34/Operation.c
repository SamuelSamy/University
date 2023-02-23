#define _CRTDBG_MAP_ALLOC

#include <stdlib.h>
#include <crtdbg.h>
#include "Operation.h"
#include "Vector.h"

Operation* createOperation(Function undoFunction, Function redoFunction, Vector* undoData, Vector* redoData)
{
    Operation* operation = malloc(sizeof(Operation));

    operation->undo = undoFunction;
    operation->redo = redoFunction;
    operation->undoData = undoData;
    operation->redoData = redoData;

    return operation;
}

void destroyOperation(Operation* operation)
{
    if (operation->undoData != operation->redoData)
    {
        destroyVector(operation->redoData);
    }

    destroyVector(operation->undoData);

    free(operation);
    operation = NULL;
}

Vector* getOperationUndoData(Operation* operation)
{
    return operation->undoData;
}

Vector* getOperationRedoData(Operation* operation)
{
    return operation->redoData;
}