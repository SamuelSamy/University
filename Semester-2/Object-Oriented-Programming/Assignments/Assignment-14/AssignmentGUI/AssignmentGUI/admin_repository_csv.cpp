#include "admin_repository_csv.h"
#include <fstream>


void CSVRepository::write() const
{
    std::ofstream fout(fileName);

    for (std::pair<int, int> p : this->basket)
    {
        Coat c = this->repo->get_coat_by_ID(p.first);

        fout << c.get_size() << ",";
        fout << c.get_price() << ",";
        fout << p.second << ",";
        fout << c.get_color() << ",";
        fout << c.get_photo() << "\n";
    }

    fout.close();
}

std::pair<int, int> CSVRepository::operator[](int index)
{
    return this->UserRepository::operator[](index);
}

int CSVRepository::length()
{
    return this->UserRepository::length();
}

void CSVRepository::add(int ID)
{
    this->UserRepository::add(ID);

    this->write();
}

int CSVRepository::get_total_price() const
{
    int total = 0;

    for (const auto& element : this->basket)
    {
        const Coat coat = this->repo->get_coat_by_ID(element.first);
        total += coat.get_price() * element.second;
    }

    return total;

}

std::string CSVRepository::get_file_name() const
{
    return this->fileName;
}
