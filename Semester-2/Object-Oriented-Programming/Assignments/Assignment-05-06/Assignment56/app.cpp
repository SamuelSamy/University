//https://www.yesstyle.com/zh_CN/donnae-%E5%8F%8C%E6%8E%92%E6%89%A3%E5%A4%A7%E8%A1%A3/info.html/pid.1045377185
//https://www.yesstyle.com/zh_CN/kwuartz-%E5%8D%95%E6%8E%92%E6%89%A3%E9%95%BF%E9%A3%8E%E8%A1%A3/info.html/pid.1106875409

#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <string>
#include "vector.h"
#include "admin_repository.h"
#include "admin_service.h"
#include "user_repository.h"
#include "user_service.h"
#include "ui.h"
#include "tests.h"

void startUI()
{
    AdminRepository adminRepo;
    AdminService adminService{ adminRepo };

    UserRepository userRepo;
    UserService userService{ userRepo };

    UI ui{ adminService, userService };
    ui.populate_repo();
    ui.start_menu();
}

int main()
{
    bool ignoreUI = false;
    
    runTests();

    if (!ignoreUI)
    {
        startUI();
    }

    int retValue = _CrtDumpMemoryLeaks();
    std::cout << "\n\n--------------------------------------------------------\n\n";
    std::cout << "Memory leaks: " << retValue;
    return 0;
}