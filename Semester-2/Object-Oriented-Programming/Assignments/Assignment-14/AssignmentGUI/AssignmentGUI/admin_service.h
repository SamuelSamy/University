#pragma once

#include <stdlib.h>
#include <crtdbg.h>
#include "admin_repository.h"
#include <vector>
#include <memory>
#include "action.h"

class AdminService
{
    private:
        Repository* repository;
        int lastID;
        std::vector<std::unique_ptr<Action>> actions;
        int actionIndex;

    public:

        /// <summary>
        /// Constructor
        /// </summary>
        AdminService(Repository* repo);
        
        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="service"> The service that will be copied </param>
        AdminService(const AdminService& service);

        /// <summary>
        /// Adds a coat into the repository linked to the service
        /// </summary>
        /// <param name="size"> The size of the coat </param>
        /// <param name="price"> The price of the coat </param>
        /// <param name="quantity"> The quantity of the coat </param>
        /// <param name="color"> The color of the coat </param>
        /// <param name="photo"> The photo of the coat </param>
        void add(int size, int price, int quantity, std::string color, std::string photo);

        /// <summary>
        /// Removes a coat from the repository linked to the service
        /// </summary>
        /// <param name="ID"> The ID of the coat </param>
        void remove(int ID);

        /// <summary>
        /// Gets a coat by the given ID
        /// </summary>
        /// <param name="ID"> The ID of the coat </param>
        /// <returns> The coat with the given ID </returns>
        Coat get_coat_by_ID(int ID);
        
        std::vector<Coat> get_coats_by_size(int size);

        /// <summary>
        /// Updates a coat object from the repository linked to the service
        /// </summary>
        /// <param name="ID"> The ID of the coat </param>
        /// <param name="size"> New size of the coat </param>
        /// <param name="price"> New price of the coat </param>
        /// <param name="quantity"> New quantity of the coat </param>
        /// <param name="color"> New color of the coat </param>
        /// <param name="photo"> New photo of the coat </param>
        /// <param name="changes"> An integer representing the changes: (&1) - the size was changed; (&2) - the price was changed; (&4) - the quantity was changed; (&8) - the color was changed; (&16) - the photo was changed </param>
        void update(int ID, int size, int price, int quantity, std::string color, std::string photo);

        /// <summary>
        /// Gets the repo linked 
        /// </summary>
        /// <returns></returns>
        Repository* get_repo();

        bool redoAction();
        bool undoAction();

        /// <summary>
        /// Destructor
        /// </summary>
        ~AdminService();

        /// <summary>
        /// Creates an ostream for the Service class 
        /// </summary>
        /// <param name="os"> An ostream object </param>
        /// <param name="service"> A Service object </param>
        /// <returns> An ostream for the Service class </returns>
        friend std::ostream& operator<<(std::ostream& os, const AdminService& service);

};

inline std::ostream& operator<<(std::ostream& os, const AdminService& service)
{
    os << *service.repository;
    return os;
}