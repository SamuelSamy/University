#pragma once

#include "vector.h"
#include "coat.h"
#include "admin_repository.h"

class UserRepository
{
private:
    Vector<std::pair<int, int>> data;

public:

    /// <summary>
    /// Constructor
    /// </summary
    UserRepository();

    /// <summary>
    /// Copy constructor
    /// </summary>
    /// <param name="repo">  </param>
    UserRepository(const UserRepository& repo);

    /// <summary>
    /// Get the ID and quantity in the basket from the specified index
    /// </summary>
    /// <param name="index"> An integer representing a position </param>
    /// <returns> The <ID, quantity> found at the specified index </returns>
    std::pair<int, int> operator[](int index);

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
    int length();

    /// <summary>
    /// Adds the coat to the basket
    /// </summary>
    /// <param name="ID"> The ID of the coat </param>
    void add(int ID);

    /// <summary>
    /// Get the total price of the basket
    /// </summary>
    /// <param name="adminRepo"> An AdminRepository object </param>
    /// <returns> The total price of the basket </returns>
    int get_total_price(AdminRepository adminRepo);

    /// <summary>
    /// Destructor
    /// </summary>
    ~UserRepository();
};