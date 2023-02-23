#pragma once

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#ifdef _DEBUG
#define DEBUG_CLIENTBLOCK   new( _CLIENT_BLOCK, __FILE__, __LINE__)
#define new DEBUG_CLIENTBLOCK
#endif



#include "admin_repository.h"



class FileRepository : public Repository
{  
    private:
        std::string fileName;

    public:
        FileRepository() = default;
        FileRepository(std::string _fileName) : fileName {_fileName} {};

        virtual void write() const override;
        void read() override;

        void add(Coat element) override;
        Coat remove(int ID, bool forceRemove) override;
        void update(int ID, Coat newData) override;

        friend std::istream& operator>>(std::istream& is, FileRepository& repo);
        friend std::ostream& operator<<(std::ostream& os, const FileRepository& repo);
};

inline std::istream& operator>>(std::istream& is, FileRepository& repo)
{
    FileCoat coat;

    while (is >> coat)
    {
        FileCoat* dynamicCoat = new FileCoat{ coat };
        Coat* newCoat = (Coat*)dynamicCoat;
        repo.Repository::add(*newCoat);
        delete dynamicCoat;
    }

    return is;
}   

inline std::ostream& operator<<(std::ostream& os, const FileRepository& repo)
{
    for (Coat* coat : repo.data)
    {
        FileCoat* fileCoat = (FileCoat*)coat;
        os << *fileCoat << '\n';
    }

    return os;
}