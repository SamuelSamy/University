//https://www.yesstyle.com/zh_CN/donnae-%E5%8F%8C%E6%8E%92%E6%89%A3%E5%A4%A7%E8%A1%A3/info.html/pid.1045377185
//https://www.yesstyle.com/zh_CN/kwuartz-%E5%8D%95%E6%8E%92%E6%89%A3%E9%95%BF%E9%A3%8E%E8%A1%A3/info.html/pid.1106875409

#define _CRT_SECURE_NO_WARNINGS

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif

#include <iostream>
#include <string>

#include "admin_repository.h"
#include "admin_repository_file.h"
#include "admin_repository_html.h"
#include "admin_repository_csv.h"
#include "admin_service.h"
#include "user_repository.h"
#include "user_service.h"
#include "ui.h"
#include "tests.h"
#include <fstream>
#include "comparator.h"

int pick_option()
{
    std::cout << "1. HTML\n2. CSV\n";
    int option = 0;
    
    do
    {
        option = readInteger("Enter an option: ");
    } while (option < 1 || option > 2);
    
    return option;
}

void startUI()
{
    FileRepository fileRepo{ "data.txt" };
    fileRepo.read();

    AdminService adminService{ &fileRepo };

    int option = pick_option();
    
    if (option == 1)
    {
        HTMLRepository userRepo{ "data.html", &fileRepo };
        UserService userService{ &userRepo };

        UI ui{ adminService, userService };
        ui.start_menu();
    }
    else
    {
        CSVRepository userRepo{ "data.csv", &fileRepo };
        UserService userService{ &userRepo };

        UI ui{ adminService, userService };
        ui.start_menu();
    }
}



int main()
{
    bool ignoreUI = false;
    
    runTests();

    if (!ignoreUI)
    {
        startUI();
    }

    std::cout << "\n\n--------------------------------------------------------\n\n";
    std::cout << "Memory leaks: " << _CrtDumpMemoryLeaks();
    return 0;
}

