#pragma once

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif

#include <iostream>
#include <iomanip>
#include <string>


class Coat
{
    protected:
        int ID;
        int size;
        int price;
        int quantity;
        std::string color;
        std::string photo;

    public:

        /// <summary>
        /// Creates a Coat object
        /// Default values:
        /// int ID = -1;
        /// int size = -1;
        /// int price = -1;
        /// int quantity = -1;
        /// std::string color = "";
        /// std::string photo = "";
        /// </summary>
        Coat();

        /// <summary>
        /// Creates a Coat object
        /// </summary>
        /// <param name="ID"> The ID of the Coat </param>
        /// <param name="size"> The size of the Coat </param>
        /// <param name="price"> The price of the Coat </param>
        /// <param name="quantity"> The quantity of the Coat </param>
        /// <param name="color"> The color of the Coat </param>
        /// <param name="photo"> The photo of the Coat </param>
        Coat(int ID, int size, int price, int quantity, std::string color, std::string photo);

        Coat(const Coat& c);

        /// <summary>
        /// Overrides the operator ==
        /// </summary>
        /// <param name="coat"> A Coat object that will be compared to this </param>
        /// <returns> True - if all fields of both coats are equal, False - otherwise </returns>
        bool operator==(const Coat& coat);

        /// <summary>
        /// Getter for the ID field
        /// </summary>
        /// <returns> The ID of this </returns>
        int get_ID() const;
        
        /// <summary>
        /// Getter for the size field
        /// </summary>
        /// <returns> The size of this </returns>
        int get_size() const;
        
        /// <summary>
        /// Getter for the price field
        /// </summary>
        /// <returns> The price of this </returns>
        int get_price() const;

        /// <summary>
        /// Getter for the quantity field
        /// </summary>
        /// <returns> The quantity of this </returns>
        int get_quantity() const;

        /// <summary>
        /// Getter for the color field
        /// </summary>
        /// <returns> The color of this </returns>
        std::string get_color() const;

        /// <summary>
        /// Getter for the photo field
        /// </summary>
        /// <returns> The photo of this </returns>
        std::string get_photo() const;

        /// <summary>
        /// Setter for the ID field
        /// </summary>
        /// <param name="newID"> The new ID </param>
        void set_ID(int newID);

        /// <summary>
        /// Setter for the size field
        /// </summary>
        /// <param name="newSize"> The new size </param>
        void set_size(int newSize);

        /// <summary>
        /// Setter for the price field
        /// </summary>
        /// <param name="newPrice"> The new price </param>
        void set_price(int newPrice);

        /// <summary>
        /// Setter for the quantity field
        /// </summary>
        /// <param name="newQuantity"> The new quantity </param>
        void set_quantity(int newQuantity);

        /// <summary>
        /// Setter for the color field
        /// </summary>
        /// <param name="newColor"> The new color </param>
        void set_color(std::string newColor);
        
        /// <summary>
        /// Setter for the photo field
        /// </summary>
        /// <param name="newPhoto"> The new photo </param>
        void set_photo(std::string newPhoto);

        /// <summary>
        /// 
        /// </summary>
        ~Coat();

        friend std::ostream& operator<<(std::ostream& os, const Coat& coat);
        friend std::ostream& operator<<(std::ostream& os, Coat* coat);
};


class FileCoat : Coat
{
    /// <summary>
    /// Creates an ostream for the FileCoat class
    /// </summary>
    /// <param name="os"> An ostream& type object </param>
    /// <param name="coat"> The coat object </param>
    /// <returns> An ostream for the FileCoat class </returns>
    friend std::ostream& operator<<(std::ostream& os, const FileCoat& coat);

    friend std::istream& operator>>(std::istream& is, FileCoat& coat);

};

class ConsoleCoat : Coat
{
    /// <summary>
    /// Creates an ostream for the ConsoleCoat class
    /// </summary>
    /// <param name="os"> An ostream& type object </param>
    /// <param name="coat"> The coat object </param>
    /// <returns> An ostream for the ConsoleCoat class </returns>
    friend std::ostream& operator<<(std::ostream& os, const ConsoleCoat& coat);

    friend std::istream& operator>>(std::istream& is, const ConsoleCoat& coat);
};

inline std::ostream& operator<<(std::ostream& os, const Coat& coat)
{
    os << "ID: " << std::setw(10) << std::left << coat.ID << " ";
    os << "Size: " << std::setw(10) << std::left << coat.size << " ";
    os << "Price: " << std::setw(10) << std::left << std::to_string(coat.price) + "$" << " ";
    os << "Quantity: " << std::setw(10) << std::left << coat.quantity << " ";
    os << "Color: " << std::setw(12) << std::left << coat.color << " ";
    os << "Photo: " << std::setw(20) << std::left << coat.photo;
    return os;
}

inline std::ostream& operator<<(std::ostream& os, Coat* coat)
{
    os << "ID: " << std::setw(10) << std::left << coat->ID << " ";
    os << "Size: " << std::setw(10) << std::left << coat->size << " ";
    os << "Price: " << std::setw(10) << std::left << std::to_string(coat->price) + "$" << " ";
    os << "Quantity: " << std::setw(10) << std::left << coat->quantity << " ";
    os << "Color: " << std::setw(12) << std::left << coat->color << " ";
    os << "Photo: " << std::setw(20) << std::left << coat->photo;
    return os;
}

inline std::ostream& operator<<(std::ostream& os, const ConsoleCoat& coat)
{
    os << "ID: "        << std::setw(10)    << std::left   << coat.ID                           << " ";
    os << "Size: "      << std::setw(10)    << std::left   << coat.size                         << " ";
    os << "Price: "     << std::setw(10)    << std::left   << std::to_string(coat.price) + "$"  << " ";
    os << "Quantity: "  << std::setw(10)    << std::left   << coat.quantity                     << " ";
    os << "Color: "     << std::setw(12)    << std::left   << coat.color                        << " ";
    os << "Photo: "     << std::setw(20)    << std::left   << coat.photo;
    return os;
}

inline std::ostream& operator<<(std::ostream& os, const FileCoat& coat)
{
    os << coat.ID << "," << coat.size << "," << coat.price << "," << coat.quantity << "," << coat.color << "," << coat.photo;
    return os;
}

inline std::istream& operator>>(std::istream& is, const ConsoleCoat& coat)
{
    return is;
}

inline std::istream& operator>>(std::istream& is, FileCoat& coat)
{
    if (is.eof())
    {
        is.setstate(std::ios_base::failbit);
        return is;
    }

        
    std::string id;
    std::getline(is, id, ',');
    coat.ID = atoi(id.c_str());

    std::string size;
    std::getline(is, size, ',');
    coat.size = atoi(size.c_str());

    std::string price;
    std::getline(is, price, ',');
    coat.price = atoi(price.c_str());

    std::string quantity;
    std::getline(is, quantity, ',');
    coat.quantity = atoi(quantity.c_str());

    std::getline(is, coat.color, ',');
    std::getline(is, coat.photo, '\n');
    
    return is;
}