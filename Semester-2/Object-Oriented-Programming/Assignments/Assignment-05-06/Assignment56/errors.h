#pragma once

#include <iostream>

class Errors
{
    public:
        std::string CoatNotFound = "There's no coat with the specified ID";
        std::string DuplicateID = "There's already a coat with the specified ID";
        std::string IndexOutOfBounds = "The specified index is smaller than 0 or greater than the vector size";
        std::string CoatNotSoldOut = "This coat can not be removed because it's not sold out";
};

