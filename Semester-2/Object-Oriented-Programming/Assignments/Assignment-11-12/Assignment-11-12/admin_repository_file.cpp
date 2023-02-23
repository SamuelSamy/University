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

void FileRepository::remove(int ID)
{
    this->Repository::remove(ID);
    this->write();
}

void FileRepository::update(int ID, Coat newData)
{
    this->Repository::update(ID, newData);
    this->write();
}
