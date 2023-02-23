#include "admin_repository_html.h"
#include "admin_repository_html.h"
#include <fstream>


void HTMLRepository::write() const
{
    std::ofstream fout(fileName);

    fout << "<!DOCTYPE html>\n";
    fout << "<html>\n";
    fout << "\t<head>\n";
    fout << "\t\t<title>Shopping Basket</title>\n";
    fout << "\t</head>\n";
    fout << "\t<body>\n";
    fout << "\t\t<table border=\"1\">\n";
    fout << "\t\t<tr>\n";
    fout << "\t\t\t<td>Size</td>\n";
    fout << "\t\t\t<td>Price</td>\n";
    fout << "\t\t\t<td>Quantity</td>\n";
    fout << "\t\t\t<td>Color</td>\n";
    fout << "\t\t\t<td>Photo</td>\n";
    fout << "\t\t\t</tr>\n";

    for (std::pair<int, int> p : this->basket)
    {
        Coat c = this->repo->get_coat_by_ID(p.first);

        fout << "\t\t<tr>\n";
        fout << "\t\t\t<td>" << c.get_size() << "</td>\n";
        fout << "\t\t\t<td>" << c.get_price() << "</td>\n";
        fout << "\t\t\t<td>" << p.second << "</td>\n";
        fout << "\t\t\t<td>" << c.get_color() << "</td>\n";
        fout << "\t\t\t<td><a href=\"" << c.get_photo() << "\">Link</a></td>\n";
        fout << "\t\t\t</tr>\n";
    }

    fout.close();
}

std::pair<int, int> HTMLRepository::operator[](int index)
{
    return this->UserRepository::operator[](index);
}

int HTMLRepository::length()
{
    return this->UserRepository::length();
}

void HTMLRepository::add(int ID)
{
    this->UserRepository::add(ID);

    this->write();
}

int HTMLRepository::get_total_price() const
{
    int total = 0;

    for (const auto& element : this->basket)
    {
        const Coat coat = this->repo->get_coat_by_ID(element.first);
        total += coat.get_price() * element.second;
    }

    return total;
    
}

std::string HTMLRepository::get_file_name() const
{
    return this->fileName;
}
