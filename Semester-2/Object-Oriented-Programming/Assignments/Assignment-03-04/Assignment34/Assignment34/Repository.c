#define _CRTDBG_MAP_ALLOC

#include "Repository.h"
#include "Vector.h"
#include "ErrorLevels.h"
#include <stdlib.h>
#include <string.h>
#include <crtdbg.h>

Repository* createRepository()
{
    Repository* repository = malloc(sizeof(Repository));
    if (repository == NULL)
    {
        return NULL;
    }

    repository->data = createVector(2, destroyEstate);
    return repository;
}

void destroyRepositroy(Repository* repository)
{
    if (repository == NULL)
    {
        return;
    }

    destroyVector(repository->data);
    free(repository);
}

int addEstate(Repository* repository, Vector* parameters)
{
    if (repository == NULL || parameters == NULL)
    {
        return MemoryIssue;
    }

    Estate* estate;
    int retVal = vectorGetValueByIndex(parameters, 0, &estate);
    if (retVal != Success)
    {
        return retVal;
    }

    int errorLevel;
    Estate* testEstate = getEstateByAddress(repository, estate->address, &errorLevel);
    if (testEstate != NULL)
    {
        return EstateAlreadyExists;
    }

    retVal = vectorInsertTail(repository->data, copyEstate(estate));
    return retVal;
}

int removeEstate(Repository* repository, Vector* parameters)
{
    if (repository == NULL || parameters == NULL)
    {
        return MemoryIssue;
    }

    Estate* estate;
    int retVal = vectorGetValueByIndex(parameters, 0, &estate);
    if (retVal != Success)
    {
        return retVal;
    }

    int index = EstateNotFound;
    int len = getLength(repository);

    for (int i = 0; i < len; i++)
    {
        Estate* currentEstate;
        retVal = vectorGetValueByIndex(repository->data, i, &currentEstate);

        if (strcmp(getAddress(currentEstate), getAddress(estate)) == 0)
        {
            index = i;
            break;
        }
    }

    if (index == EstateNotFound)
    {
        return EstateNotFound;
    }

    retVal = vectorRemoveByIndex(repository->data, index);
    if (retVal != Success)
    {
        return retVal;
    }

    return Success;
}

int updateEstate(Repository* repository, Vector* parameters)
{
    if (repository == NULL || parameters == NULL)
    {
        return MemoryIssue;
    }

    Estate* estate;
    int retVal = vectorGetValueByIndex(parameters, 0, &estate);
    if (retVal != Success)
    {
        return retVal;
    }

    Estate* newEstate;
    retVal = vectorGetValueByIndex(parameters, 1, &newEstate);
    if (retVal != Success)
    {
        return retVal;
    }

    int index = EstateNotFound;
    int len = getLength(repository);

    for (int i = 0; i < len; i++)
    {
        Estate* currentEstate;
        retVal = vectorGetValueByIndex(repository->data, i, &currentEstate);

        if (strcmp(getAddress(currentEstate), getAddress(estate)) == 0)
        {
            index = i;
            break;
        }
    }

    if (index == EstateNotFound)
    {
        return EstateNotFound;
    }

    retVal = vectorRemoveByIndex(repository->data, index);
    if (retVal != Success)
    {
        return retVal;
    }

    retVal = vectorInsetAtIndex(repository->data, index, copyEstate(newEstate));
    if (retVal != Success)
    {
        return retVal;
    }

    return Success;
}

int getLength(Repository* repository)
{
    if (repository == NULL)
    {
        return MemoryIssue;
    }
    return vectorGetSize(repository->data);
}

int checkIfEstateExistsRepo(Repository* repository, char* address)
{
    if (repository == NULL || address == NULL)
    {
        return MemoryIssue;
    }

    int len = getLength(repository);
    for (int i = 0; i < len; i++)
    {
        Estate* estate;
        int retVal = vectorGetValueByIndex(repository->data, i, &estate);
        if (retVal != 0)
        {
            return EstateNotFound;
        }

        if (strcmp(getAddress(estate), address) == 0)
        {
            return EstateAlreadyExists;
        }
    }

    return EstateNotFound;
}

Estate* getEstateByAddress(Repository* repository, char* address, int* errorLevel)
{
    *errorLevel = Success;
    if (repository == NULL || address == NULL)
    {
        return NULL;
    }

    int len = getLength(repository);

    for (int i = 0; i < len; i++)
    {
        Estate* estate;
        int retVal = vectorGetValueByIndex(repository->data, i, &estate);
        if (retVal != 0)
        {
            *errorLevel = retVal;
            return NULL;
        }

        if (strcmp(getAddress(estate), address) == 0)
        {
            return estate;
        }
    }

    *errorLevel = EstateNotFound;
    return NULL;
}


Repository* filterByAddressRepo(Repository* repository, char* address)
{
    Repository* newRepository = createRepository();

    for (int i = 0; i < getLength(repository); i++)
    {
        Estate* estate;
        vectorGetValueByIndex(repository->data, i, &estate);

        if (address == NULL || strstr(getAddress(estate), address) != NULL)
        {
            int errorLevel;
            Estate* newEstate = createEstate(getType(estate), getAddress(estate), getSurface(estate), getPrice(estate), &errorLevel);
            Vector* vector = createVector(2, destroyEstate);

            int retVal = vectorInsertTail(vector, newEstate);
            if (retVal != Success)
            {
                return NULL;
            }

            addEstate(newRepository, vector);

            destroyVector(vector);
        }
    }

    return newRepository;
}

Repository* filterByTypeAndSurfaceRepo(Repository* repository, char* type, double surface)
{
    Repository* newRepository = createRepository();

    for (int i = 0; i < getLength(repository); i++)
    {
        Estate* estate;
        vectorGetValueByIndex(repository->data, i, &estate);

        if (strcmp(getType(estate), type) == 0 && getSurface(estate) > surface)
        {
            int errorLevel;
            Estate* newEstate = createEstate(getType(estate), getAddress(estate), getSurface(estate), getPrice(estate), &errorLevel);
            Vector* vector = createVector(2, destroyEstate);

            int retVal = vectorInsertTail(vector, newEstate);
            if (retVal != Success)
            {
                return NULL;
            }

            addEstate(newRepository, vector);

            destroyVector(vector);
        }
    }

    return newRepository;
}


Repository* filterByTypeAndPriceRepo(Repository* repository, char* type, double price)
{
    Repository* newRepository = createRepository();

    for (int i = 0; i < getLength(repository); i++)
    {
        Estate* estate;
        vectorGetValueByIndex(repository->data, i, &estate);

        if (strcmp(getType(estate), type) == 0 && getPrice(estate) < price)
        {
            int errorLevel;
            Estate* newEstate = createEstate(getType(estate), getAddress(estate), getSurface(estate), getPrice(estate), &errorLevel);
            Vector* vector = createVector(2, destroyEstate);

            int retVal = vectorInsertTail(vector, newEstate);
            if (retVal != Success)
            {
                return NULL;
            }

            addEstate(newRepository, vector);

            destroyVector(vector);
        }
    }

    return newRepository;
}

Repository* copyRepositoryOfEstates(Repository* repository)
{
    if (repository == NULL)
    {
        return NULL;
    }

    Repository* newRepository = createRepository();

    int len = getLength(repository);
    for (int i = 0; i < len; i++)
    {
        Estate* estate;
        vectorGetValueByIndex(repository->data, i, &estate);
        Estate* newEstate = copyEstate(estate);
        vectorInsertTail(newRepository->data, newEstate);
    }

    return newRepository;
}


void sortEstatesByPrice(Repository* repository, int ascending)
{
    if (repository == NULL)
    {
        return;
    }

    int len = getLength(repository);
    int sorted;

    do
    {
        sorted = 1;

        for (int i = 0; i < len - 1; i++)
        {
            Estate* estate0;
            vectorGetValueByIndex(repository->data, i, &estate0);

            Estate* estate1;
            vectorGetValueByIndex(repository->data, i + 1, &estate1);

            if (getPrice(estate0) > getPrice(estate1))
            {
                vectorSwap(repository->data, i, i + 1);
                sorted = 0;
            }
        }
    } while (sorted == 0);

    if (ascending == 0)
    {
        for (int i = 0; i < len / 2; i++)
        {
            vectorSwap(repository->data, i, len - i - 1);
        }
    }
}

