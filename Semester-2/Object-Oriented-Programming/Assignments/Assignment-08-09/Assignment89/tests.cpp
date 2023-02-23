#include "coat.h"
#include "admin_repository.h"
#include "user_repository.h"
#include "admin_service.h"
#include "user_service.h"
#include <iostream>
#include "tests.h"
#include <assert.h>
#include <sstream>
#include <vector>
#include "errors.h"
#include "admin_repository_html.h"
#include "admin_repository.h"
#include "comparator.h"


void runCoatTests();
void runVectorTests();
void runAdminRepoTests();
void runAdminServiceTests();
void runUserRepoTests();
void runUserServiceTests();

void testSort()
{
    std::vector<Coat> general;
    general.push_back(Coat{ 0, 20, 30, 0, "Blue", "Link_" });
    general.push_back(Coat{ 1, 30, 20, 0, "Blue", "Link_" });
    general.push_back(Coat{ 2,  5, 25, 0, "Blue", "Link_" });

    std::vector<Coat> size{ general };
    std::vector<Coat> price{ general };

    Comparator<Coat>* cmpSize = new ComparatorAscendingBySize<Coat>{};
    Comparator<Coat>* cmpPrice = new ComparatorDescendingByPrice<Coat>{};

    genericSort<Coat>(size, cmpSize);
    genericSort<Coat>(price, cmpPrice);

    assert(size[0].get_ID() == 2);
    assert(size[1].get_ID() == 0);
    assert(size[2].get_ID() == 1);

    assert(price[0].get_ID() == 0);
    assert(price[1].get_ID() == 2);
    assert(price[2].get_ID() == 1);

    delete cmpSize;
    delete cmpPrice;
}

void runTests()
{
    runCoatTests();
    runVectorTests();
    runAdminRepoTests();
    runAdminServiceTests();
    runUserRepoTests();
    runUserServiceTests();
    testSort();
}



void runCoatTests()
{
    Coat coat0;
    Coat coat1(0, 20, 30, 40, "Red", "Link1");
    Coat coat2(coat1);

    assert(coat0.get_ID() == -1);
    assert(coat0.get_size() == -1);
    assert(coat0.get_price() == -1);
    assert(coat0.get_quantity() == -1);
    assert(coat0.get_color() == "");
    assert(coat0.get_photo() == "");

    assert(coat1.get_ID() == 0);
    assert(coat1.get_size() == 20);
    assert(coat1.get_price() == 30);
    assert(coat1.get_quantity() == 40);
    assert(coat1.get_color() == "Red");
    assert(coat1.get_photo() == "Link1");

    assert(coat2.get_ID() == 0);
    assert(coat2.get_size() == 20);
    assert(coat2.get_price() == 30);
    assert(coat2.get_quantity() == 40);
    assert(coat2.get_color() == "Red");
    assert(coat2.get_photo() == "Link1");

    coat0.set_ID(0);
    coat0.set_size(20);
    coat0.set_price(30);
    coat0.set_quantity(40);
    coat0.set_color("Red");
    coat0.set_photo("Link1");

    assert(coat0.get_ID() == 0);
    assert(coat0.get_size() == 20);
    assert(coat0.get_price() == 30);
    assert(coat0.get_quantity() == 40);
    assert(coat0.get_color() == "Red");
    assert(coat0.get_photo() == "Link1");

    std::stringstream out;
    out << coat0;
    assert(out.str() == "ID: 0          Size: 20         Price: 30$        Quantity: 40         Color: Red          Photo: Link1               ");
}

void testVectorStream();

void runVectorTests()
{
    testVectorStream();
}

void testAdminRepoCreate();
void testAdminRepoAdd();
void testAdminRepoOperators();
void testAdminRepoRemove();
void testAdminRepoUpdate();
void testAdminRepoGet();
void testAdminRepoStream();
void testAdminRepoGetBySize();

void runAdminRepoTests()
{
    testAdminRepoCreate();
    testAdminRepoAdd();
    testAdminRepoOperators();
    testAdminRepoRemove();
    testAdminRepoUpdate();
    testAdminRepoGet();
    testAdminRepoStream();
}

void testAdminServiceCreate();
void testAdminServiceAdd();
void testAdminServiceRemove();
void testAdminServiceUpdate();
void testAdminServiceGet();
void testAdminServiceStream();
void testAdminServiceGetBySize();

void runAdminServiceTests()
{
    testAdminServiceCreate();
    testAdminServiceAdd();
    testAdminServiceRemove();
    testAdminServiceUpdate();
    testAdminServiceGet();
    testAdminServiceStream();
    testAdminServiceGetBySize();
}

void testUserRepoCreate();
void testUserRepoAdd();
void testUserRepoOperators();
void testUserRepoPrice();
void testUserRepoGetVector();

void runUserRepoTests()
{
    testUserRepoCreate();
    testUserRepoAdd();
    testUserRepoOperators();
    testUserRepoPrice();
    testUserRepoGetVector();
}

void testUserServiceCreate();
void testUserServiceAdd();
void testUserServiceGetRepo();
void testUserServiceGetPrice();

void runUserServiceTests()
{
    testUserServiceCreate();
    testUserServiceAdd();
    testUserServiceGetRepo();
    testUserServiceGetPrice();
}

void testVectorStream()
{
    std::vector<int> v;
    v.push_back(0);
    v.push_back(1);

    std::stringstream out;
    out << v;
    assert(out.str() == "0\n1\n");

}

void testAdminRepoCreate()
{
    Repository repo0;
    Repository repo1(repo0);

    assert(repo0.length() == repo1.length());
    assert(repo0.length() == 0);
}

void testAdminRepoAdd()
{
    Repository repo0;
    Coat coat0(0, 100, 200, 300, "Blue", "Link1");
    Coat coat1(1, 100, 200, 300, "Blue", "Link1");
    
    repo0.add(coat0);
    repo0.add(coat1);

    assert(repo0.length() == 2);

    try
    {
        repo0.add(coat0);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == DuplicateID);
    }

}

void testAdminRepoOperators()
{
    Repository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link1");
    Coat coat1(1, 100, 200, 300, "Blue", "Link1");

    repo0.add(coat0);
    repo0.add(coat1);

    assert(*repo0[0] == coat0);

    Repository repo1, repo2;
    repo2 = repo1 = repo0;

    assert(repo2.length() == repo1.length());
    assert(repo1.length() == repo0.length());
    assert(repo0.length() == 2);
}

void testAdminRepoRemove()
{
    Repository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link1");
    Coat coat1(1, 100, 200, 300, "Blue", "Link1");

    repo0.add(coat0);
    repo0.add(coat1);

    repo0.remove(0);

    assert(repo0.length() == 1);

    try
    {
        repo0.remove(100);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == CoatNotFound);
    }

    try
    {
        repo0.remove(1);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == CoatNotSoldOut);
    }
}

void testAdminRepoUpdate()
{
    Repository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link1");
    Coat coat1(0, 200, 300, 400, "Red", "AnotherLink");

    repo0.add(coat0);

    repo0.update(0, coat1);
    Coat repoCoat = *repo0[0];

    assert(repoCoat == coat1);

    try
    {
        repo0.update(1, coat1);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == CoatNotFound);
    }
}

void testAdminRepoGet()
{
    Repository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link1");

    repo0.add(coat0);

    Coat repoCoat = repo0.get_coat_by_ID(0);

    assert(repoCoat == coat0);

    try
    {
        Coat repoCoat = repo0.get_coat_by_ID(100);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == CoatNotFound);
    }
}

void testAdminRepoStream()
{
    Repository repo0;
    Coat coat0(0, 20, 30, 40, "Red", "Link1");

    repo0.add(coat0);

    std::stringstream out;
    out << repo0;
    assert(out.str() == "ID: 0          Size: 20         Price: 30$        Quantity: 40         Color: Red          Photo: Link1               \n");

}


void testAdminServiceCreate()
{
    Repository repo;
    AdminService service0 {&repo};
    AdminService service1{ service0 };

    assert(service0.get_repo()->length() == 0);
}

void testAdminServiceAdd()
{
    Repository repo;
    AdminService service0{ &repo };
    service0.add(0, 100, 200, "Blue", "Nope1");
    assert(service0.get_repo()->length() == 1);
}

void testAdminServiceRemove()
{
    Repository repo;
    AdminService service0{ &repo };
    service0.add(0, 100, 0, "Blue", "Nope1");
    assert(service0.get_repo()->length() == 1);

    service0.remove(0);
    assert(service0.get_repo()->length() == 0);

    try
    {
        service0.remove(100);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == CoatNotFound);
    }
}

void testAdminServiceUpdate()
{
    Repository repo;
    AdminService service0{ &repo };
    service0.add(0, 100, 200, "Blue", "Nope1");
    assert(service0.get_repo()->length() == 1);

    service0.update(0, 300, 400, 500, "Red", "Link1", (1 | 2 | 4 | 8 | 16));

    Coat coat = service0.get_coat_by_ID(0);
    assert(coat.get_ID() == 0);
    assert(coat.get_size() == 300);
    assert(coat.get_price() == 400);
    assert(coat.get_quantity() == 500);
    assert(coat.get_color() == "Red");
    assert(coat.get_photo() == "Link1");

    try
    {
        service0.update(100, 0, 0, 0, "", "", 0);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == CoatNotFound);
    }

    service0.update(0, 100, 100, 100, "-", "-", 0);
    coat = service0.get_coat_by_ID(0);
    assert(coat.get_ID() == 0);
    assert(coat.get_size() == 300);
    assert(coat.get_price() == 400);
    assert(coat.get_quantity() == 500);
    assert(coat.get_color() == "Red");
    assert(coat.get_photo() == "Link1");
}

void testAdminServiceGet()
{
    Repository repo;
    AdminService service{ &repo };
    Coat coat0(0, 100, 200, 0, "Blue", "Link1");

    service.add(100, 200, 0, "Blue", "Link1");

    Coat repoCoat = service.get_coat_by_ID(0);

    assert(repoCoat == coat0);

    try
    {
        Coat repoCoat = service.get_coat_by_ID(100);
        assert(false);
    }
    catch (RepositoryException error)
    {
        assert(std::string(error.what()) == CoatNotFound);
    }
}

void testAdminServiceStream()
{
    Repository repo;
    AdminService service{ &repo };
    service.add(20, 30, 40, "Red", "Link1");

    std::stringstream out;
    out << service;
    assert(out.str() == "ID: 0          Size: 20         Price: 30$        Quantity: 40         Color: Red          Photo: Link1               \n");

}


void testUserRepoCreate()
{
    Coat c{ 0, 1, 2, 3, "Blue", "Photo" };
    FileRepository fileRepo;
    fileRepo.add(c);
   HTMLRepository repo{ "tests.html", &fileRepo };

    HTMLRepository repo1{ repo };

    assert(repo.length() == repo1.length());
    assert(repo.length() == 0);

    repo.add(0);

    HTMLRepository repo2, repo3;
    repo2 = repo3 = repo;
    assert(repo2.length() == repo.length());
    assert(repo3.length() == repo2.length());
    assert(repo2.length() == 1);
}

void testUserRepoAdd()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };

    fileRepo.add(Coat{ 0, 0, 0, 0, "", "" });
    fileRepo.add(Coat{ 1, 0, 0, 0, "", "" });

    repo.add(0);
    repo.add(1);

    assert(repo.length() == 2);
}

void testUserRepoOperators()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };

    fileRepo.add(Coat{ 1, 0, 0, 0, "", "" });
    fileRepo.add(Coat{ 2, 0, 0, 0, "", "" });

    repo.add(1);
    repo.add(2);
    repo.add(2);

    assert(repo[0].first == 1);
    assert(repo[0].second == 1);
    assert(repo[1].first == 2);
    assert(repo[1].second == 2);
}

void testUserRepoPrice()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };

    fileRepo.add(Coat{ 0, 30, 20, 1, "Red", "Nope1" });
    fileRepo.add(Coat{ 1, 30, 20, 2, "Red", "Nope1" });

    repo.add(0);
    repo.add(1);

    assert(repo.get_total_price() == 40);
}


void testAdminServiceGetBySize()
{
    
    Repository repo;
    AdminService service{ &repo };
    service.add(30, 20, 1, "Red", "Nope1");

    assert(service.get_coats_by_size(30).size() == 1);
    assert(service.get_coats_by_size(0).size() == 1);
}

void testAdminRepoGetBySize()
{
    Repository repo;
    Coat c0 = Coat(0, 30, 20, 1, "Red", "Nope1");
    repo.add(c0);

    assert(repo.get_coats_by_size(30).size() == 1);
    assert(repo.get_coats_by_size(0).size() == 1);
}

void testUserServiceCreate()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };
    UserService service{ &repo };
    UserService service1{ service };

    assert(service.get_repo()->length() == service1.get_repo()->length());
}

void testUserServiceAdd()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };
    fileRepo.add(Coat{ 0, 0, 0, 0, "Blue", "Photo" });
    fileRepo.add(Coat{ 1, 0, 0, 0, "Blue", "Photo" });
    UserService service{ &repo };

    service.add(0);
    service.add(1);

    assert(service.get_repo()->length() == 2);
}

void testUserServiceGetRepo()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };
    fileRepo.add(Coat{ 0, 0, 0, 0, "Blue", "Photo" });
    fileRepo.add(Coat{ 1, 0, 0, 0, "Blue", "Photo" });
    UserService service{ &repo };

    service.add(0);
    service.add(1);

    repo.add(0);
    repo.add(1);

    assert(service.get_repo()->length() == repo.length());
}

void testUserServiceGetPrice()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };
    fileRepo.add(Coat{ 0, 0, 40, 0, "Blue", "Photo" });
    fileRepo.add(Coat{ 1, 0, 40, 0, "Blue", "Photo" });
    UserService service{ &repo };


    service.add(0);
    service.add(1);

    assert(service.get_total_price() == 80);

}

void testUserRepoGetVector()
{
    FileRepository fileRepo;
    HTMLRepository repo{ "tests.html", &fileRepo };
    fileRepo.add(Coat{ 0, 0, 0, 0, "Blue", "Photo" });
    repo.add(0);
    std::vector<std::pair<int, int>> v = repo.get_vector();
    assert(v.size() == 1);
}