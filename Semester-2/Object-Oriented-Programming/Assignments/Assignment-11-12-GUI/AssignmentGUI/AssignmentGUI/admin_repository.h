#pragma once

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif



#include <vector>
#include "coat.h"

class Repository
{
    protected:
        std::vector<Coat*> data;

    public:
        
        /// <summary>
        /// Constructor
        /// </summary
        Repository();
        
        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="repo">  </param>
        Repository(Repository* repo);

        Repository(const Repository& repo);

        /// <summary>
        /// Get the Coat from the specified index
        /// </summary>
        /// <param name="index"> An integer representing a position </param>
        /// <returns> The element found at the specified index </returns>
        Coat* operator[](int index);

        Coat* at(int index);


        /// <summary>
        /// Overrides the oprator =
        /// </summary>
        /// <param name="v"> A Repository object that will be assigned to this </param>
        /// <returns> A pointer to this </returns>
        Repository& operator=(const Repository& v);

        std::vector<Coat> get_coats_by_size(int size);

        /// <summary>
        /// Search for the coat object with the specified ID
        /// </summary>
        /// <param name="ID"> The ID of the coat </param>
        /// <returns> The coat object with the specified ID </returns>
        Coat get_coat_by_ID(int ID);

        /// <summary>
        /// Get the size of the repository
        /// </summary>
        /// <returns> The size of the repository </returns>
        int length();

        /// <summary>
        /// Adds the coat object in the repository
        /// </summary>
        /// <param name="element"> The Coat object that will be added </param>
        virtual void add(Coat element);

        /// <summary>
        /// Removes a coat from the repository
        /// </summary>
        /// <param name="ID"> The ID of the coat that will be removed </param>
        virtual void remove(int ID);

        /// <summary>
        /// Updates a coat object
        /// </summary>
        /// <param name="ID"> The ID of the coat that will be updated </param>
        /// <param name="newData"> The new data of the coat </param>
        virtual void update(int ID, Coat newData);

        int get_last_ID();

        virtual void write() const;
        virtual void read();

        /// <summary>
        /// Destructor
        /// </summary>
        ~Repository();

        /// <summary>
        /// Creates an ostream for the Repository class 
        /// </summary>
        /// <param name="os"> An ostream object </param>
        /// <param name="repository"> A repository object </param>
        /// <returns> An ostream for the Repository class </returns>
        friend std::ostream& operator<<(std::ostream& os, const Repository& repository);
        
};

template <typename Type>
std::ostream& operator<<(std::ostream& os, const std::vector<Type>& v)
{
    for (const Type& coat : v)
    {
        os << coat << '\n';
    }

    return os;
}


inline std::ostream& operator<<(std::ostream& os, const Repository& repository)
{
    os << repository.data;
    return os;
}