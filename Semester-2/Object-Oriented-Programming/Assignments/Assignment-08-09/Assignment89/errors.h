#pragma once

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif

#include <string>

#define CoatNotFound "There's no coat with the specified ID"
#define DuplicateID "There's already a coat with the specified ID"
#define IndexOutOfBounds "The specified index is smaller than 0 or greater than the vector size"
#define CoatNotSoldOut "This coat can not be removed because it's not sold out"


class ValidationException : public std::exception
{
    private:
        std::string message;

    public:
        ValidationException(std::string _message);
        const char* what() const noexcept override;
};

class RepositoryException : public std::exception
{
    private:
        std::string message;

    public:
        RepositoryException(std::string _message);
        const char* what() const noexcept override;
};

