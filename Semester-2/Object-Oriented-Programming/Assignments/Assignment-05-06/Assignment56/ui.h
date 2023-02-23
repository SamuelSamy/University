#pragma once

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

        void addToBasket(int index, Vector<Coat>& coats);

        int basketPriceUI();

        void showCoat(int &index, Vector<Coat> coats);

        void handleBuying();

        void displayBasket();

        void runAdminMode();

        void runUserMode();
    

    public:

        UI(const AdminService& _adminService, const UserService& _userService) : adminService(_adminService), userService(_userService) {};

        void start_menu();

        void populate_repo();

        ~UI();
};