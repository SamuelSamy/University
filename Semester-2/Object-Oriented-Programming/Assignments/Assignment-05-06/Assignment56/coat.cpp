#include "coat.h"

Coat::Coat()
{
    this->ID = -1;
    this->size = -1;
    this->price = -1;
    this->quantity = -1;
    this->color = "";
    this->photo = "";
}

Coat::Coat(int ID, int size, int price, int quantity, std::string color, std::string photo)
{
    this->set_ID(ID);
    this->set_size(size);
    this->set_price(price);
    this->set_quantity(quantity);
    this->set_color(color);
    this->set_photo(photo);
}

bool Coat::operator==(const Coat& coat)
{
    return this->ID == coat.ID;
}

int Coat::get_ID() const
{
    return this->ID;
}

int Coat::get_size() const
{
    return this->size;
}

int Coat::get_price() const
{
    return this->price;
}

int Coat::get_quantity() const
{
    return this->quantity;
}

std::string Coat::get_color() const
{
    return this->color;
}

std::string Coat::get_photo() const
{
    return this->photo;
}

void Coat::set_ID(int newID)
{
    this->ID = newID;
}

void Coat::set_size(int newSize)
{
    this->size = newSize;
}

void Coat::set_price(int newPrice)
{
    this->price = newPrice;
}

void Coat::set_quantity(int newQuantity)
{
    this->quantity = newQuantity;
}

void Coat::set_color(std::string newColor)
{
    this->color = newColor;
}

void Coat::set_photo(std::string newPhoto)
{
    this->photo = newPhoto;
}

Coat::~Coat()
{
}

