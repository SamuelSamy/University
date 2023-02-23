#include "validators.h"

void CoatValidator::validate(Coat* coat)
{
    std::string errors = "";

    if (coat->get_ID() < 0)
    {
        errors += "The ID must be a positive number\n";
    }

    if (coat->get_price() <= 0)
    {
        errors += "The price must be a positive number\n";
    }

    if (coat->get_quantity() < 0)
    {
        errors += "The quantity must be a positive number\n";
    }

    if (coat->get_size() < 0)
    {
        errors += "The size must be a positive number\n";
    }

    if (coat->get_color().size() < 3)
    {
        errors += "The color must be at least 3 characters long\n";
    }

    if (coat->get_photo().size() < 5)
    {
        errors += "The photo link must be at least 5 characters long\n";
    }

    if (errors.size() > 0)
    {
        throw ValidationException(errors);
    }
}
