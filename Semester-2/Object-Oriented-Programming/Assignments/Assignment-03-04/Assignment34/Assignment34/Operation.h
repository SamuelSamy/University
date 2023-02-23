#pragma once

#include "Vector.h"

typedef int (*Function)(void*, void*);

typedef struct _Operation {
    Vector* undoData;
    Function undo;

    Vector* redoData;
    Function redo;

} Operation;

/// <summary>
/// Creates and initializes an Operation object
/// </summary>
/// <param name="undoFunction"> A pointer to the undo function </param>
/// <param name="redoFunction"> A pointer to the redo function </param>
/// <param name="undoData"> A Vector* representing the undo data </param>
/// <param name="redoData"> A Vector* representing the undo data </param>
/// <returns> A Operation* object </returns>
Operation* createOperation(Function undoFunction, Function redoFunction, Vector* undoData, Vector* redoData);

/// <summary>
/// Destorys an operation
/// </summary>
/// <param name="operation"> An Operation* object </param>
void destroyOperation(Operation* operation);

/// <summary>
/// Gets the undo data of an operation
/// </summary>
/// <param name="operation"> An Operation* object </param>
/// <returns> A Vector* representing the undo data</returns>
Vector* getOperationUndoData(Operation* operation);

/// <summary>
/// Gets the redo data of an operation
/// </summary>
/// <param name="operation"> An Operation* object </param>
/// <returns> A Vector* representing the redo data</returns>
Vector* getOperationRedoData(Operation* operation);