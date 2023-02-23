#include "admin_repository_file.h"

#include <iostream>
#include <fstream>



void FileRepository::write() const
{
    std::ofstream fout(fileName);
    fout << *this;
}


void FileRepository::read()
{
    std::ifstream fin(fileName);
    fin >> *this;
}

void FileRepository::add(Coat element)
{
    this->Repository::add(element);
    this->write();
}

Coat FileRepository::remove(int ID, bool forceRemove = false)
{
    Coat c = this->Repository::remove(ID, forceRemove);
    this->write();
    return c;
}

void FileRepository::update(int ID, Coat newData)
{
    this->Repository::update(ID, newData);
    this->write();
}
