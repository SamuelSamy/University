#pragma once

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif



#include "admin_service.h"
#include "user_service.h"

class UI
{
    private:
        AdminService adminService;
        UserService userService;

        void addCoatUI();

        void removeCoatUI();

        void updateCoatUI();

        void printAll();

        void addToBasket(int index, std::vector<Coat>& coats);

        int basketPriceUI();

        void showCoat(int &index, std::vector<Coat> coats);

        void handleBuying();

        void displayBasket();

        void openRepository();

        void runAdminMode();

        void runUserMode();
    

    public:

        UI(const AdminService& _adminService, const UserService& _userService) : adminService(_adminService), userService(_userService) {};

        void start_menu();

        void populate_repo();

        ~UI();
};


int readInteger(std::string message);