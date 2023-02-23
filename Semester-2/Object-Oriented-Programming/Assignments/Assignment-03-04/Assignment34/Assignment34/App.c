#define _CRTDBG_MAP_ALLOC
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include "UI.h"
#include <stdio.h>
#include "Repository.h"
#include <crtdbg.h>
#include <string.h>
#include "Tests.h"

int main()
{
    int runTests = 1;

    if (runTests)
    {
        runAllTests();
    }
    //else
    {
        Repository* repository = createRepository();
        Service* service = createService(repository);

        addEstateService(service, "house", "1", 50, 100);
        addEstateService(service, "apartment", "2", 20, 40);
        addEstateService(service, "apartment", "3", 10, 20);
        addEstateService(service, "house", "4", 40, 80);
        addEstateService(service, "apartment", "5", 30, 120);
        addEstateService(service, "penthouse", "6", 50, 100);
        addEstateService(service, "house", "7", 20, 940);
        addEstateService(service, "house", "8", 10, 420);
        addEstateService(service, "penthouse", "9", 40, 800);
        addEstateService(service, "penthouse", "10", 30, 360);


        UI* ui = createUI(service);
        startUI(ui, 1);
        destroyUI(ui);
    }


    int retValue = _CrtDumpMemoryLeaks();
    printf("\n----------------------------------------------------------\n");
    printf("\nMemory leaks: %d\n", retValue);
    printf("\n----------------------------------------------------------\n");

    return 0;
}
