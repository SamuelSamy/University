#include <stdio.h>
#include "Repository.h"
#include "Service.h"
#include "ErrorLevels.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>


void runRepoTests();
void runServiceTests();
void runEstateTests();

void runAllTests()
{
    runRepoTests();
    runServiceTests();
    runEstateTests();

    printf("All tests passed\n");
}


void testAddRepo();
void testRemoveRepo();
void testUpdateRepo();
void testFilterRepo();
void testCopyRepo();
void testSort();

void runRepoTests()
{
    testAddRepo();
    testRemoveRepo();
    testUpdateRepo();
    testFilterRepo();
    testCopyRepo();
    testSort();
}


void testAddService();
void testRemoveService();
void testUpdateService();
void testFilterByAddress();
void testUndoRedo();
void testUndoRedoCopies();
void testFilterByTypeAndPrice();
void testFilterByTypeAndSurface();

void runServiceTests()
{
    testAddService();
    testRemoveService();
    testUpdateService();
    testFilterByAddress();
    testUndoRedo();
    testUndoRedoCopies();
    testFilterByTypeAndPrice();
    testFilterByTypeAndSurface();
}


void testGetters();
void testEstateCreate();
void testSetters();
void testToString();

void runEstateTests()
{
    testGetters();
    testEstateCreate();
    testSetters();
    testToString();
}

void initEstateRepoForTests(Repository* repository)
{
    int error;
    Estate* estate = createEstate("house", "1", 100, 200, &error);
    Vector* vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    addEstate(repository, vector);
    destroyVector(vector);
}


void testAddRepo()
{
    Repository* repository = createRepository();
    initEstateRepoForTests(repository);

    assert(getLength(repository) == 1);

    int error;
    Estate* estate = createEstate("penthouse", "2", 100, 200, &error);
    Vector* vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);

    assert(getLength(repository) == 2);

    assert(addEstate(repository, vector) == EstateAlreadyExists);

    destroyRepositroy(repository);
    destroyVector(vector);
}

void testRemoveRepo()
{
    Repository* repository = createRepository();
    initEstateRepoForTests(repository);

    assert(getLength(repository) == 1);

    int error;
    Estate* estate = createEstate("penthouse", "2", 100, 200, &error);
    Vector* vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    addEstate(repository, vector);
    destroyVector(vector);

    estate = createEstate("house", "3", 100, 200, &error);
    vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    addEstate(repository, vector);
    destroyVector(vector);

    estate = createEstate("house", "1", 100, 200, &error);
    vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(removeEstate(repository, vector) == Success);

    assert(getLength(repository) == 2);

    assert(removeEstate(repository, vector) == EstateNotFound);

    destroyRepositroy(repository);
    destroyVector(vector);
}


void testUpdateRepo()
{

    Repository* repository = createRepository();
    initEstateRepoForTests(repository);

    assert(getLength(repository) == 1);

    int error;
    Estate* estate = createEstate("house", "1", 100, 200, &error);
    Estate* newEstate = createEstate("penthouse", "2", 50, 40, &error);
    
    Vector* vector = createVector(2, destroyEstate);

    vectorInsertTail(vector, estate);
    vectorInsertTail(vector, newEstate);

    assert(updateEstate(repository, vector) == Success);

    Estate* result = getEstateByAddress(repository, "2", &error);

    assert(result != NULL);

    assert(strcmp(getType(result), "penthouse") == 0);
    assert(strcmp(getAddress(result), "2") == 0);
    assert(getSurface(result) == 50);
    assert(getPrice(result) == 40);


    destroyRepositroy(repository);
    destroyVector(vector);
}


void testFilterRepo()
{
    Repository* repository = createRepository();
    initEstateRepoForTests(repository);

    int error;
    Estate* estate = createEstate("penthouse", "2", 100, 200, &error);
    Vector* vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);
    destroyVector(vector);


    estate = createEstate("house", "3", 400, 200, &error);
    vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);
    destroyVector(vector);

    Repository* newRepo = filterByTypeAndSurfaceRepo(repository, "house", 150);

    assert(getLength(newRepo) == 1);

    destroyRepositroy(newRepo);
    destroyRepositroy(repository);
}


void testCopyRepo()
{
    Repository* repository = createRepository();
    initEstateRepoForTests(repository);

    Repository* newRepo = copyRepositoryOfEstates(repository);
    assert(getLength(newRepo) == 1);

    destroyRepositroy(repository);
    destroyRepositroy(newRepo);
}


void testSort()
{
    Repository* repository = createRepository();
    initEstateRepoForTests(repository);

    int error;
    Estate* estate = createEstate("penthouse", "2", 100, 100, &error);
    Vector* vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);
    destroyVector(vector);

    sortEstatesByPrice(repository, 0);

    Estate* es;
    vectorGetValueByIndex(repository->data, 0, &es);

    assert(getPrice(es) == 200);

    destroyRepositroy(repository);
}

void initServiceForTest(Service* service)
{
    addEstateService(service, "house", "1", 200, 300);
}

void testAddService()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    assert(getLength(getRepo(service)) == 1);

    addEstateService(service, "house", "2", 200, 300);

    assert(getLength(getRepo(service)) == 2);
    assert(addEstateService(service, "house", "2", 200, 300) == EstateAlreadyExists);

    destroyService(service);
}

void testRemoveService()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    assert(getLength(getRepo(service)) == 1);
    assert(removeEstateService(service, "1") == Success);
    assert(getLength(getRepo(service)) == 0);
    assert(removeEstateService(service, "1") == EstateNotFound);

    destroyService(service);
}

void testUpdateService()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    assert(getLength(getRepo(service)) == 1);
    assert(updateEstateService(service, "1", "200", "penthouse", 500, 600, 15) == Success);

    char address[100];
    char type[100];
    double surface = -1;
    double price = -1;

    assert(updateEstateService(service, "200", address, type, surface, price, 0) == Success);

    int error;
    Estate* estate = getEstateByAddress(repository, "200", &error);

    assert(strcmp(getType(estate), "penthouse") == 0);
    assert(strcmp(getAddress(estate), "200") == 0);
    assert(getSurface(estate) == 500);
    assert(getPrice(estate) == 600);

    destroyService(service);
}


void testFilterByTypeAndSurface()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    int error;
    Estate* estate = createEstate("house", "2", 300, 300, &error);
    Vector* vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);
    destroyVector(vector);


    estate = createEstate("house", "3", 400, 200, &error);
    vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);
    destroyVector(vector);

    Repository* newRepo = filterByTypeAndSurface(service, "house", 250);

    assert(getLength(newRepo) == 2);

    destroyRepositroy(newRepo);
    destroyService(service);
}

void testFilterByTypeAndPrice()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    int error;
    Estate* estate = createEstate("house", "2", 100, 300, &error);
    Vector* vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);
    destroyVector(vector);


    estate = createEstate("house", "3", 400, 200, &error);
    vector = createVector(2, destroyEstate);
    vectorInsertTail(vector, estate);
    assert(addEstate(repository, vector) == Success);
    destroyVector(vector);

    Repository* newRepo = filterByTypeAndPrice(service, "house", 250);

    assert(getLength(newRepo) == 1);

    destroyRepositroy(newRepo);
    destroyService(service);
}


void testFilterByAddress()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    assert(getLength(repository) == 1);

    addEstateService(service, "house", "20", 200, 300);
    addEstateService(service, "house", "200", 200, 300);

    assert(getLength(repository) == 3);


    Repository* newRepo = filterByAddress(service, "2");
    assert(getLength(newRepo) == 2);
    destroyRepositroy(newRepo);

    destroyService(service);
}

void testUndoRedo()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    assert(getLength(repository) == 1);

    addEstateService(service, "house", "20", 200, 300);
    addEstateService(service, "house", "200", 200, 300);

    assert(getLength(repository) == 3);

    undo(service);
    assert(getLength(repository) == 2);

    redo(service);
    assert(getLength(repository) == 3);
    
    removeEstateService(service, "200");
    assert(getLength(repository) == 2);

    undo(service);
    assert(getLength(repository) == 3);

    redo(service);
    assert(getLength(repository) == 2);

    undo(service);
    undo(service);
    undo(service);
    undo(service);
    assert(undo(service) == NothingLeftToUndo);

    addEstateService(service, "house", "2", 200, 300);
    assert(redo(service) == NothingLeftToRedo);

    assert(undo(NULL) == MemoryIssue);
    assert(redo(NULL) == MemoryIssue);

    destroyService(service);
}


void testUndoRedoCopies()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    assert(getLength(repository) == 1);

    addEstateService(service, "house", "20", 200, 300);
    addEstateService(service, "house", "200", 200, 300);

    assert(getLength(repository) == 3);

    undoUsingCopies(service);
    assert(getLength(service->repository) == 2);

    redoUsingCopies(service);
    assert(getLength(service->repository) == 3);

    removeEstateService(service, "200");
    assert(getLength(service->repository) == 2);

    undoUsingCopies(service);
    assert(getLength(service->repository) == 3);

    redoUsingCopies(service);
    assert(getLength(service->repository) == 2);

    undoUsingCopies(service);
    undoUsingCopies(service);
    undoUsingCopies(service);
    undoUsingCopies(service);
    assert(undoUsingCopies(service) == NothingLeftToUndo);

    addEstateService(service, "house", "2", 200, 300);
    assert(redoUsingCopies(service) == NothingLeftToRedo);

    assert(undoUsingCopies(NULL) == MemoryIssue);
    assert(redoUsingCopies(NULL) == MemoryIssue);

    destroyService(service);
}


void testGetters()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    int error;
    Estate* estate = getEstateByAddress(repository, "1", &error);

    assert(strcmp(getType(estate), "house") == 0);
    assert(strcmp(getAddress(estate), "1") == 0);
    assert(getSurface(estate) == 200);
    assert(getPrice(estate) == 300);

    destroyService(service);
}

void testEstateCreate()
{
    int errorLevel;
    Estate* estate = createEstate("nope", "1", 100, 200, &errorLevel);
    assert(errorLevel == TypeError);

    estate = createEstate("house", "1", -100, 200, &errorLevel);
    assert(errorLevel == SurfaceError);

    estate = createEstate("house", "1", 100, -200, &errorLevel);
    assert(errorLevel == PriceError);

    destroyEstate(estate);
}

void testSetters()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    int error;
    Estate* estate = getEstateByAddress(repository, "1", &error);

    setType(estate, "penthouse");
    setAddress(estate, "2");
    setSurface(estate, 300);
    setPrice(estate, 400);

    assert(strcmp(getType(estate), "penthouse") == 0);
    assert(strcmp(getAddress(estate), "2") == 0);
    assert(getSurface(estate) == 300);
    assert(getPrice(estate) == 400);

    destroyService(service);
}


void testToString()
{
    Repository* repository = createRepository();
    Service* service = createService(repository);

    initServiceForTest(service);

    int error;
    Estate* estate = getEstateByAddress(repository, "1", &error);

    char str[500];
    toString(estate, str);

    assert(strlen(str) == 91);

    destroyService(service);
}