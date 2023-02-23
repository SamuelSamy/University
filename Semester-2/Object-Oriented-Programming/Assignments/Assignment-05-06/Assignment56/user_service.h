#pragma once

#include "user_repository.h"
#include "admin_service.h"

class UserService
{
private:
    UserRepository repository;
    int lastID;

public:

    /// <summary>
    /// Constructor
    /// </summary>
    UserService(const UserRepository& repo) : repository(repo), lastID(0) {};

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
    UserRepository get_repo();

    /// <summary>
    /// Computes the total price of the basket
    /// </summary>
    /// <param name="adminService"> An AdminService where the coats are stored </param>
    /// <returns> The total price of the basket </returns>
    int get_total_price(AdminService adminService);

    /// <summary>
    /// Destructor
    /// </summary>
    ~UserService();
};