#pragma once

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif


#include "coat.h"
#include "admin_repository.h"
#include <vector>

class UserRepository
{
    protected:
        std::vector<std::pair<int, int>> basket;

    public:
        UserRepository() {};

        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="repo">  </param>
        UserRepository(UserRepository* repo);

        /// <summary>
        /// Get the ID and quantity in the basket from the specified index
        /// </summary>
        /// <param name="index"> An integer representing a position </param>
        /// <returns> The <ID, quantity> found at the specified index </returns>
        virtual std::pair<int, int> operator[](int index);

        /// <summary>
        /// Overrides the oprator =
        /// </summary>
        /// <param name="v"> A Repository object that will be assigned to this </param>
        /// <returns> A pointer to this </returns>
        UserRepository& operator=(const UserRepository& v);

        /// <summary>
        /// Get the size of the repository
        /// </summary>
        /// <returns> The size of the repository </returns>
        virtual int length();

        /// <summary>
        /// Adds the coat to the basket
        /// </summary>
        /// <param name="ID"> The ID of the coat </param>
        virtual void add(int ID);

        virtual int get_total_price() const = 0;
        virtual std::string get_file_name() const = 0;

        /// <summary>
        /// Returns the vector
        /// </summary>
        /// <returns> The vector </returns>
        std::vector<std::pair<int, int>> get_vector();


        virtual void write() const = 0;


        /// <summary>
        /// Destructor
        /// </summary>
        ~UserRepository();
};