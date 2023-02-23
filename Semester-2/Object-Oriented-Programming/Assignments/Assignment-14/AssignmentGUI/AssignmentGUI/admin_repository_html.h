#pragma once

#include "admin_repository_file.h"
#include "user_repository.h"

class HTMLRepository : public UserRepository
{
    private:
        std::string fileName;
        FileRepository* repo;

    public:

        HTMLRepository() : repo{ nullptr } {};
        HTMLRepository(std::string _fileName, FileRepository* _repo) : fileName{ _fileName }, repo{ _repo } {};

        void write() const override;
        
        std::pair<int, int> operator[](int index) override;
        int length() override;
        void add(int ID) override;

        int get_total_price() const;
        
        std::string get_file_name() const;

};