#define _CRT_SECURE_NO_WARNINGS

#include "ui.h"
#include <fstream>
#include "admin_repository_file.h"
#include "errors.h"
#include <stdio.h>
#include <stdlib.h>


int readInteger(std::string message)
{
    int retVal = 0;
    int number = -1;

    std::cout << message << '\n';
    std::cin >> number;

    while (std::cin.fail())
    {   
        std::cout << "Error reading number!\n";
        std::cout << message << '\n';
        std::cin.clear();
        std::cin.ignore(256, '\n');
        std::cin >> number;
    }

    return number;
}


void printMainMenu()
{
    std::cout << "\n--------------------------------------------\n";
    std::cout << "1. Admin\n";
    std::cout << "2. User\n";
    std::cout << "0. Exit\n";
    std::cout << "\n--------------------------------------------\n";

}

void printAdminMenu()
{
    std::cout << "\n--------------------------------------------\n";
    std::cout << "1. Add a coat\n";
    std::cout << "2. Remove a coat\n";
    std::cout << "3. Update a coat\n";
    std::cout << "4. List all coats\n";
    std::cout << "0. Logout\n";
    std::cout << "\n--------------------------------------------\n";
}

void printUserMenu()
{
    std::cout << "\n--------------------------------------------\n";
    std::cout << "1. List coats having a given size\n";
    std::cout << "2. See basket\n";
    std::cout << "3. Open repository\n";
    std::cout << "0. Logout\n";
    std::cout << "\n--------------------------------------------\n";
}

void printBuyingMenu()
{
    std::cout << "\n--------------------------------------------\n";
    std::cout << "1. Add to basket\n";
    std::cout << "2. Next\n";
    std::cout << "0. Exit view mode\n";
    std::cout << "\n--------------------------------------------\n";
}

bool isValidMainOption(int option)
{
    return option >= 0 && option <= 2;
}

bool isValidAdminOption(int option)
{
    return option >= 0 && option <= 4;
}

bool isValidUserOption(int option)
{
    return option >= 0 && option <= 3;
}

bool isValidBuyingOption(int option)
{
    return option >= 0 && option <= 2;
}

void UI::addCoatUI()
{
    int size = readInteger("Enter the size: ");
    int price = readInteger("Enter the price: ");
    int quantity = readInteger("Enter the quantity: ");
    
    std::string color;
    std::cout << "Enter a color:\n";
    std::cin >> color;
    
    std::string photo;
    std::cout << "Enter a photo:\n";
    std::cin >> photo;

    this->adminService.add(size, price, quantity, color, photo);
}


void UI::removeCoatUI()
{
    int ID = readInteger("Enter the ID of the coat: ");
    this->adminService.remove(ID);
}

void UI::updateCoatUI()
{
    int changes = 0;

    int size = 0;
    int price = 0; 
    int quantity = 0;
    std::string color = "";
    std::string photo = "";

    int ID = readInteger("Enter the ID of the coat you want to update: ");

    std::string answer;
    std::cout << "Do you want to change the size? (y/n): ";
    std::cin >> answer;
    if (answer == "y")
    {
        size = readInteger("Enter the new size: ");
        changes |= 1;
    }

    std::cout << "Do you want to change the price? (y/n): ";
    std::cin >> answer;
    if (answer == "y")
    {
        price = readInteger("Enter the new price: ");
        changes |= 2;
    }

    std::cout << "Do you want to change the quantity? (y/n): ";
    std::cin >> answer;
    if (answer == "y")
    {
        quantity = readInteger("Enter the new quantity: ");
        changes |= 4;
    }

    std::cout << "Do you want to change the color? (y/n): ";
    std::cin >> answer;
    if (answer == "y")
    {
        std::cout << "Enter the new color:\n";
        std::cin >> color;
        changes |= 8;
    }

    std::cout << "Do you want to change the photo? (y/n): ";
    std::cin >> answer;
    if (answer == "y")
    {
        std::cout << "Enter the new photo:\n";
        std::cin >> photo;
        changes |= 16;
    }

    this->adminService.update(ID, size, price, quantity, color, photo, changes);
}

void UI::printAll()
{
    if (this->adminService.get_repo()->length() == 0)
    {
        std::cout << "No elements in the database!\n";
        return;
    }

    std::vector<Coat> coats = this->adminService.get_repo()->get_coats_by_size(0);

    for (Coat c : coats)
    {
        Coat* cPtr = new Coat(c);
        ConsoleCoat* fileCoat = (ConsoleCoat*)cPtr;
        std::cout << *fileCoat << '\n';
        delete cPtr;
    }
}


void UI::addToBasket(int index, std::vector<Coat>& coats)
{
    Coat coat = coats[index];

    if (coat.get_quantity() < 1)
    {
        std::cout << "This coat is out of stock!\n";
        return;
    }

    coats[index].set_quantity(coat.get_quantity() - 1);
    adminService.update(coat.get_ID(), 0, 0, coats[index].get_quantity(), "", "", 4);
    userService.add(coat.get_ID());
}

int UI::basketPriceUI()
{
    return userService.get_total_price();
}


void UI::showCoat(int &index, std::vector<Coat> coats)
{
    index %= coats.size();
    Coat coat = coats[index];

    std::cout << coat << '\n';

    std::string link = "start " + coat.get_photo();
    system(link.c_str());
}


void UI::handleBuying()
{
    int size = readInteger("Enter the size of the coat (0 for all): ");

    std::vector<Coat> coats = adminService.get_coats_by_size(size);

    if (coats.size() == 0)
    {
        std::cout << "There are no coats with the specified size!\n";
        return;
    }

    int option = 0;

    int index = 0;
    showCoat(index, coats);

    do
    {
        printBuyingMenu();
        std::cout << "Total basket price: " << basketPriceUI() << "$\n\n";

        option = readInteger("Please enter an option: ");

        if (!isValidBuyingOption(option))
        {
            std::cout << "Invalid option!\n";
            continue;
        }

        if (option == 0)
        {
            continue;
        }

        try
        {
            if (option == 1)
            {
                addToBasket(index, coats);
                std::cout << "\nCoat added succssfully to the basket!\n";
            }

            showCoat(++index, coats);
        }
        catch (RepositoryException error)
        {
            std::cout << "\n->" << error.what() << "\n";
        }
        catch (ValidationException error)
        {
            std::cout << "\n->" << error.what() << "\n";
        }
        catch (...)
        {
            std::cout << "\n->Uh oh... Unexpected error\n";
        }

    } while (option != 0);
}

void UI::displayBasket()
{
    UserRepository* repo = this->userService.get_repo();
    std::vector<std::pair<int, int>> v = repo->get_vector();
    for (const auto& entry : v)
    {
        Coat coat = adminService.get_coat_by_ID(entry.first);
        coat.set_quantity(entry.second);
        std::cout << coat << '\n';
    }
    std::cout << "Total price: " << userService.get_total_price() << "$\n\n";
}

void UI::openRepository()
{
    system(this->userService.get_file_name().c_str());
}

void UI::runAdminMode()
{
    int option = 0;

    do
    {
        printAdminMenu();
        option = readInteger("Please enter an option: ");

        if (!isValidAdminOption(option))
        {
            std::cout << "Invalid option!\n";
            continue;
        }

        try
        {
            if (option == 1)
            {
                addCoatUI();
                std::cout << "\n->Coat added succssfully!\n";
                continue;
            }

            if (option == 2)
            {
                removeCoatUI();
                std::cout << "\n->Coat removed succssfully!\n";
                continue;
            }

            if (option == 3)
            {
                updateCoatUI();
                std::cout << "\n->Coat updated succssfully!\n";
                continue;
            }

            if (option == 4)
            {
                printAll();
            }
        }
        catch (RepositoryException error)
        {
            std::cout << "\n->" << error.what() << "\n";
        }
        catch (ValidationException error)
        {
            std::cout << "\n->" << error.what() << "\n";
        }
        catch (...)
        {
            std::cout << "\n->Uh oh... Unexpected error\n";
        }

    } while (option != 0);
}

void UI::runUserMode()
{
    int option = 0;

    do
    {
        printUserMenu();
        option = readInteger("Please enter an option: ");

        if (!isValidUserOption(option))
        {
            std::cout << "Invalid option!\n";
            continue;
        }

        try
        {
            if (option == 1)
            {
                handleBuying();
                continue;
            }

            if (option == 2)
            {
                displayBasket();
                continue;
            }

            if (option == 3)
            {
                openRepository();
            }


        }
        catch (RepositoryException error)
        {
            std::cout << "\n->" << error.what() << "\n";
        }
        catch (ValidationException error)
        {
            std::cout << "\n->" << error.what() << "\n";
        }
        catch (...)
        {
            std::cout << "\n->Uh oh... Unexpected error\n";
        }

    } while (option != 0);
}



void UI::start_menu()
{
    int mainOption = 0;

    do
    {
        printMainMenu();
        mainOption = readInteger("Please enter an option: ");

        if (!isValidMainOption(mainOption))
        {
            std::cout << "Invalid option!\n";
            continue;
        }

        if (mainOption == 1)
        {
            runAdminMode();
            continue;
        }
        
        if (mainOption == 2)
        {
            runUserMode();
        }

    } while (mainOption != 0);
}

void UI::populate_repo()
{
    this->adminService.add(30,  30,     10,     "Green",    "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/31/435/L_g0060143531.jpg");
    this->adminService.add(30,  40,     20,     "Red",      "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/05/436/L_g0060143605.jpg");
    this->adminService.add(30,  60,     2,      "Pink",     "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/77/463/L_g0060146377.jpg");
    this->adminService.add(32,  90,     2,      "Yellow",   "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/80/463/L_g0060146380.jpg");
    this->adminService.add(32,  10,     2,      "Green",    "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/82/463/L_g0060146382.jpg");
    this->adminService.add(34,  120,    16,     "Blue",     "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/84/463/L_g0060146384.jpg");
    this->adminService.add(36,  50,     32,     "Black",    "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/93/463/L_g0060146393.jpg");
    this->adminService.add(36,  80,     4,      "Brown",    "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/96/463/L_g0060146396.jpg");
    this->adminService.add(36,  30,     0,      "Red",      "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/99/463/L_g0060146399.jpg");
    this->adminService.add(36,  60,     3,      "Green",    "https://d1flfk77wl2xk4.cloudfront.net/Assets/GalleryImage/86/147/L_g0161214786.jpg");
}

UI::~UI()
{

}
