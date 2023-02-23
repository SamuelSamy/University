#pragma once

#include "vector.h"
#include "coat.h"

class AdminRepository
{
    private:
        Vector<Coat> data;

    public:
        
        /// <summary>
        /// Constructor
        /// </summary
        AdminRepository();
        
        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="repo">  </param>
        AdminRepository(const AdminRepository& repo);

        /// <summary>
        /// Get the Coat from the specified index
        /// </summary>
        /// <param name="index"> An integer representing a position </param>
        /// <returns> The element found at the specified index </returns>
        Coat& operator[](int index);

        /// <summary>
        /// Overrides the oprator =
        /// </summary>
        /// <param name="v"> A Repository object that will be assigned to this </param>
        /// <returns> A pointer to this </returns>
        AdminRepository& operator=(const AdminRepository& v);

        Vector<Coat> get_coats_by_size(int size);

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
        void add(Coat element);

        /// <summary>
        /// Removes a coat from the repository
        /// </summary>
        /// <param name="ID"> The ID of the coat that will be removed </param>
        void remove(int ID);

        /// <summary>
        /// Updates a coat object
        /// </summary>
        /// <param name="ID"> The ID of the coat that will be updated </param>
        /// <param name="newData"> The new data of the coat </param>
        void update(int ID, Coat newData);

        /// <summary>
        /// Destructor
        /// </summary>
        ~AdminRepository();

        /// <summary>
        /// Creates an ostream for the Repository class 
        /// </summary>
        /// <param name="os"> An ostream object </param>
        /// <param name="repository"> A repository object </param>
        /// <returns> An ostream for the Repository class </returns>
        friend std::ostream& operator<<(std::ostream& os, const AdminRepository& repository);
        
};

inline std::ostream& operator<<(std::ostream& os, const AdminRepository& repository)
{
    os << repository.data;
    return os;
}