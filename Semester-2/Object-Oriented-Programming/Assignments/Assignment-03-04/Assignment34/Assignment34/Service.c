#define _CRTDBG_MAP_ALLOC
#define _CRT_SECURE_NO_WARNINGS

#include "Service.h"
#include "Operation.h"
#include "ErrorLevels.h"
#include <stdlib.h>
#include <string.h>
#include <crtdbg.h>
#include <stdio.h>


void saveRepoForUndo(Service* service);

Service* createService(Repository* repository)
{
    if (repository == NULL)
    {
        return NULL;
    }

    Service* service = malloc(sizeof(Service));
    if (service == NULL)
    {
        return NULL;
    }

    service->repository = repository;
    service->undoStack = createVector(2, destroyOperation);
    service->undoIndex = 0;

    service->repoCopies = createVector(2, destroyRepositroy);
    service->repoCopiesIndex = 0;

    saveRepoForUndo(service);

    return service;
}

void destroyService(Service* service)
{
    if (service == NULL)
    {
        return;
    }

    destroyRepositroy(service->repository);
    destroyVector(service->undoStack);
    destroyVector(service->repoCopies);
    free(service);
}

void insertOperation(Service* service, Operation* operation)
{
    int len = vectorGetSize(service->undoStack);
    for (int i = service->undoIndex; i < len; i++)
    {
        vectorRemoveByIndex(service->undoStack, service->undoIndex);
    }
    vectorInsertTail(service->undoStack, operation);
    service->undoIndex = vectorGetSize(service->undoStack);
}

void saveRepoForUndo(Service* service)
{
    int len = vectorGetSize(service->repoCopies);
    for (int i = service->repoCopiesIndex; i < len; i++)
    {
        vectorRemoveByIndex(service->repoCopies, service->repoCopiesIndex);
    }

    Repository* newRepo = copyRepositoryOfEstates(service->repository);
    vectorInsertTail(service->repoCopies, newRepo);
    service->repoCopiesIndex = vectorGetSize(service->repoCopies);
}

int addEstateService(Service* service, char* type, char* address, double surface, double price)
{
    int errorLevel;

    Estate* testEstate = getEstateByAddress(service->repository, address, &errorLevel);
    if (testEstate != NULL)
    {
        return EstateAlreadyExists;
    }

    Estate* estate = createEstate(type, address, surface, price, &errorLevel);
    if (errorLevel != 0)
    {
        return errorLevel;
    }

    Vector* vector = createVector(2, destroyEstate);
    int retVal = vectorInsertTail(vector, estate);
    if (retVal != 0)
    {
        destroyVector(vector);
        return retVal;
    }

    Estate* estateCopy = copyEstate(estate);

    int finalValue = addEstate(service->repository, vector);
    destroyVector(vector);


    Vector* undoVector = createVector(2, destroyEstate);
    retVal = vectorInsertTail(undoVector, estateCopy);
    if (retVal != 0)
    {
        destroyVector(vector);
        return retVal;
    }

    Operation* operation = createOperation(removeEstate, addEstate, undoVector, undoVector);
    if (operation == NULL)
    {
        return ErrorUndoStack;
    }
    insertOperation(service, operation);
    saveRepoForUndo(service);
    return finalValue;
}

int removeEstateService(Service* service, char* address)
{
    if (service == NULL || address == NULL)
    {
        return MemoryIssue;
    }

    int errorLevel;
    Estate* estate = getEstateByAddress(service->repository, address, &errorLevel);
    if (errorLevel != 0)
    {
        return errorLevel;
    }

    Vector* vector = createVector(2, destroyEstate);
    int retVal = vectorInsertTail(vector, copyEstate(estate));
    if (retVal != 0)
    {
        destroyVector(vector);
        return retVal;
    }

    Estate* estateCopy = copyEstate(estate);

    int finalReturn = removeEstate(service->repository, vector);
    destroyVector(vector);
    Vector* undoVector = createVector(2, destroyEstate);
    retVal = vectorInsertTail(undoVector, estateCopy);
    if (retVal != 0)
    {
        destroyVector(vector);
        return retVal;
    }

    Operation* operation = createOperation(addEstate, removeEstate, undoVector, undoVector);
    if (operation == NULL)
    {
        return ErrorUndoStack;
    }
    insertOperation(service, operation);
    saveRepoForUndo(service);
   
    return finalReturn;
}

int updateEstateService(Service* service, char* oldAddress, char* newAddress, char* newType, double newSurface, double newPrice, int changes)
{
    if (service == NULL || oldAddress == NULL)
    {
        return MemoryIssue;
    }

    if (strcmp(newAddress, oldAddress))
    {
        int retValue = checkIfEstateExists(service, newAddress);
        if (retValue != EstateNotFound)
        {
            return EstateAlreadyExists;
        }
    }
   
    int errorLevel;
    Estate* estate = getEstateByAddress(service->repository, oldAddress, &errorLevel);
    if (errorLevel != 0)
    {
        return errorLevel;
    }

    if ((changes & 1) == 0)
    {
        strcpy(newAddress, oldAddress);
    }

    if ((changes & 2) == 0)
    {
        strcpy(newType, getType(estate));
    }

    if ((changes & 4) == 0)
    {
        newSurface = getSurface(estate);
    }

    if ((changes & 8) == 0)
    {
        newPrice = getPrice(estate);
    }

    Estate* newEstate = createEstate(newType, newAddress, newSurface, newPrice, &errorLevel);
    if (errorLevel != 0)
    {
        return errorLevel;
    }


    Vector* vector = createVector(2, destroyEstate);
    int retVal = vectorInsertTail(vector, copyEstate(estate));
    if (retVal != 0)
    {
        destroyVector(vector);
        return retVal;
    }

    retVal = vectorInsertTail(vector, copyEstate(newEstate));
    if (retVal != 0)
    {
        destroyVector(vector);
        return retVal;
    }

    Vector* undoVector = createVector(2, destroyEstate);
    retVal = vectorInsertTail(undoVector, copyEstate(newEstate));
    if (retVal != 0)
    {
        destroyVector(undoVector);
        return retVal;
    }

    retVal = vectorInsertTail(undoVector, copyEstate(estate));
    if (retVal != 0)
    {
        destroyVector(undoVector);
        return retVal;
    }

    Estate* estateCopy = copyEstate(estate);

    updateEstate(service->repository, vector);
    destroyVector(vector);

    Vector* redoVector = createVector(2, destroyEstate);
    retVal = vectorInsertTail(redoVector, estateCopy);
    if (retVal != 0)
    {
        destroyVector(redoVector);
        return retVal;
    }

    retVal = vectorInsertTail(redoVector, copyEstate(newEstate));
    if (retVal != 0)
    {
        destroyVector(redoVector);
        return retVal;
    }

    Operation* operation = createOperation(updateEstate, updateEstate, undoVector, redoVector);
    if (operation == NULL)
    {
        return ErrorUndoStack;
    }
    insertOperation(service, operation);
    saveRepoForUndo(service);

    destroyEstate(newEstate);

    return Success;
}


Repository* filterByAddress(Service* service, char* string)
{
    if (service == NULL)
    {
        return NULL;
    }

    return filterByAddressRepo(service->repository, string);
}


Repository* filterByTypeAndSurface(Service* service, char* type, double surface)
{
    if (service == NULL)
    {
        return NULL;
    }

    return filterByTypeAndSurfaceRepo(service->repository, type, surface);
}

Repository* filterByTypeAndPrice(Service* service, char* type, double price)
{
    if (service == NULL)
    {
        return NULL;
    }

    return filterByTypeAndPriceRepo(service->repository, type, price);
}

Repository* getRepo(Service* service)
{
    if (service == NULL)
    {
        return NULL;
    }
    return service->repository;
}


int checkIfEstateExists(Service* service, char* address)
{
    if (service == NULL || address == NULL)
    {
        return MemoryIssue;
    }
    return checkIfEstateExistsRepo(service->repository, address);
}


int undo(Service* service)
{
    if (service == NULL)
    {
        return MemoryIssue;
    }


    if (service->undoIndex < 1)
    {
        return NothingLeftToUndo;
    }

    Operation* operation;
    vectorGetValueByIndex(service->undoStack, --service->undoIndex, &operation);
    int retVal = operation->undo(service->repository, getOperationUndoData(operation));
    return retVal;
}

int redo(Service* service)
{
    if (service == NULL)
    {
        return MemoryIssue;
    }

    int len = vectorGetSize(service->undoStack);

    if (service->undoIndex >= len)
    {
        return NothingLeftToRedo;
    }

    Operation* operation;
    vectorGetValueByIndex(service->undoStack, service->undoIndex++, &operation);
    int retVal = operation->redo(service->repository, getOperationRedoData(operation));
    return retVal;
}


int undoUsingCopies(Service* service)
{
    if (service == NULL)
    {
        return MemoryIssue;
    }


    if (service->repoCopiesIndex < 2)
    {
        return NothingLeftToUndo;
    }

    Repository* repo;
    vectorGetValueByIndex(service->repoCopies, --service->repoCopiesIndex - 1, &repo);
    destroyRepositroy(service->repository);
    service->repository = copyRepositoryOfEstates(repo);
     return Success;
}

int redoUsingCopies(Service* service)
{
    if (service == NULL)
    {
        return MemoryIssue;
    }

    int len = vectorGetSize(service->repoCopies);

    if (service->repoCopiesIndex >= len)
    {
        return NothingLeftToRedo;
    }

    Repository* repo;
    vectorGetValueByIndex(service->repoCopies, service->repoCopiesIndex++, &repo);
    destroyRepositroy(service->repository);
    service->repository = copyRepositoryOfEstates(repo);
    return Success;
}


