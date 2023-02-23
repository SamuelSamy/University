#pragma once

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif



#include "user_repository.h"
#include "admin_service.h"

class UserService
{
    private:
        UserRepository* repository;
        int lastID;

    public:

        /// <summary>
        /// Constructor
        /// </summary>
        UserService(UserRepository* repo) : repository(repo), lastID(repo->length()) {};

        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="service"> The service that will be copied </param>
        UserService(const UserService& service);

        /// <summary>
        /// Adds a coat's ID into the repository linked to the service
        /// </summary>
        /// <param name="ID"> The ID of the coat </param>
        void add(int ID);

        /// <summary>
        /// Gets the linked repo
        /// </summary>
        /// <returns></returns>
        UserRepository* get_repo();

        std::string get_file_name();


        /// <summary>
        /// Computes the total price of the basket
        /// </summary>
        /// <returns> The total price of the basket </returns>
        int get_total_price();

        /// <summary>
        /// Destructor
        /// </summary>
        ~UserService();
};