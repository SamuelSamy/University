#include "coat.h"
#include "admin_repository.h"
#include "user_repository.h"
#include "admin_service.h"
#include "user_service.h"
#include <iostream>
#include "tests.h"
#include <assert.h>
#include <sstream>

void runCoatTests();
void runVectorTests();
void runAdminRepoTests();
void runAdminServiceTests();
void runUserRepoTests();
void runUserServiceTests();

void runTests()
{
    runCoatTests();
    runVectorTests();
    runAdminRepoTests();
    runAdminServiceTests();
    runUserRepoTests();
    runUserServiceTests();
}


void runCoatTests()
{
    Coat coat0;
    Coat coat1(0, 20, 30, 40, "Red", "Link");
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
    assert(coat1.get_photo() == "Link");

    assert(coat2.get_ID() == 0);
    assert(coat2.get_size() == 20);
    assert(coat2.get_price() == 30);
    assert(coat2.get_quantity() == 40);
    assert(coat2.get_color() == "Red");
    assert(coat2.get_photo() == "Link");

    coat0.set_ID(0);
    coat0.set_size(20);
    coat0.set_price(30);
    coat0.set_quantity(40);
    coat0.set_color("Red");
    coat0.set_photo("Link");

    assert(coat0.get_ID() == 0);
    assert(coat0.get_size() == 20);
    assert(coat0.get_price() == 30);
    assert(coat0.get_quantity() == 40);
    assert(coat0.get_color() == "Red");
    assert(coat0.get_photo() == "Link");

    std::stringstream out;
    out << coat0;
    assert(out.str() == "ID: 0          Size: 20         Price: 30$        Quantity: 40         Color: Red          Photo: Link                ");
}


void testVectorCreate();
void testVectorAdd();
void testVectorOperators();
void testVectorRemove();
void testVectorStream();
void testVectorIterator();


void runVectorTests()
{
    testVectorCreate();
    testVectorAdd();
    testVectorOperators();
    testVectorRemove();
    testVectorStream();
    testVectorIterator();
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

void runUserRepoTests()
{
    testUserRepoCreate();
    testUserRepoAdd();
    testUserRepoOperators();
    testUserRepoPrice();
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

void testVectorCreate()
{
    Vector<int> v0;
    Vector<int> v1 = v0;

    assert(v0.size() == v1.size());
    assert(v0.size() == 0);
}

void testVectorAdd()
{
    Vector<int> v0;
    for (int i = 0; i < 10; i++)
    {
        v0.push_back(i);
    }

    Vector<int> v1 = v0;

    assert(v0.size() == v1.size());
    assert(v0.size() == 10);
}

void testVectorOperators()
{
    Vector<int> v0;
    for (int i = 0; i < 10; i++)
    {
        v0.push_back(i);
    }

    Vector<int> v1;
    Vector<int> v2;
    v2 = v1 = v0;

    assert(v0.size() == 10);
    assert(v1.size() == 10);
    assert(v2.size() == 10);

    for (int i = 0; i < 10; i++)
    {
        assert(v0[i] == i);
        assert(v1[i] == i);
        assert(v2[i] == i);
    }


    Vector<int> plusVector;

    for (int i = 0; i < 10; i++)
    {
        plusVector = plusVector + i;
    }

    for (int i = 0; i < 10; i++)
    {
        assert(plusVector[i] == i);
    }

    Vector<int> v5;
    v5 += 2;
    v5 += 5;
    assert(v5.size() == 2);

}

void testVectorRemove()
{
    Vector<int> v;
    for (int i = 0; i < 10; i++)
    {
        v.push_back(i);
    }

    assert(v.size() == 10);

    v.remove(0);
    assert(v[0] == 1);
    assert(v.size() == 9);

    try
    {
        v.remove(100);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().IndexOutOfBounds);
    }
}

void testVectorStream()
{
    Vector<int> v;
    v.push_back(0);
    v.push_back(1);

    std::stringstream out;
    out << v;
    assert(out.str() == "0\n1\n");

}

void testVectorIterator()
{
    Vector<int> v1;
    v1 += 10;
    v1 += 25;

    Vector<int>::Iterator it = v1.begin();
    assert(*it == 10);
    assert(it != v1.end());
    it++;
    assert(*it++ == 25);
    assert(it == v1.end());
}


void testAdminRepoCreate()
{
    AdminRepository repo0;
    AdminRepository repo1(repo0);

    assert(repo0.length() == repo1.length());
    assert(repo0.length() == 0);
}

void testAdminRepoAdd()
{
    AdminRepository repo0;
    Coat coat0(0, 100, 200, 300, "Blue", "Link");
    Coat coat1(1, 100, 200, 300, "Blue", "Link");
    
    repo0.add(coat0);
    repo0.add(coat1);

    assert(repo0.length() == 2);

    try
    {
        repo0.add(coat0);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().DuplicateID);
    }

}

void testAdminRepoOperators()
{
    AdminRepository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link");
    Coat coat1(1, 100, 200, 300, "Blue", "Link");

    repo0.add(coat0);
    repo0.add(coat1);

    assert(repo0[0] == coat0);

    AdminRepository repo1, repo2;
    repo2 = repo1 = repo0;

    assert(repo2.length() == repo1.length());
    assert(repo1.length() == repo0.length());
    assert(repo0.length() == 2);
}

void testAdminRepoRemove()
{
    AdminRepository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link");
    Coat coat1(1, 100, 200, 300, "Blue", "Link");

    repo0.add(coat0);
    repo0.add(coat1);

    repo0.remove(0);

    assert(repo0.length() == 1);

    try
    {
        repo0.remove(100);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().CoatNotFound);
    }

    try
    {
        repo0.remove(1);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().CoatNotSoldOut);
    }
}

void testAdminRepoUpdate()
{
    AdminRepository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link");
    Coat coat1(0, 200, 300, 400, "Red", "AnotherLink");

    repo0.add(coat0);

    repo0.update(0, coat1);
    Coat repoCoat = repo0[0];

    assert(repoCoat == coat1);

    try
    {
        repo0.update(1, coat1);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().CoatNotFound);
    }
}

void testAdminRepoGet()
{
    AdminRepository repo0;
    Coat coat0(0, 100, 200, 0, "Blue", "Link");

    repo0.add(coat0);

    Coat repoCoat = repo0.get_coat_by_ID(0);

    assert(repoCoat == coat0);

    try
    {
        Coat repoCoat = repo0.get_coat_by_ID(100);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().CoatNotFound);
    }
}

void testAdminRepoStream()
{
    AdminRepository repo0;
    Coat coat0(0, 20, 30, 40, "Red", "Link");

    repo0.add(coat0);

    std::stringstream out;
    out << repo0;
    assert(out.str() == "ID: 0          Size: 20         Price: 30$        Quantity: 40         Color: Red          Photo: Link                \n");

}


void testAdminServiceCreate()
{
    AdminRepository repo;
    AdminService service0 {repo};
    AdminService service1{ service0 };

    assert(service0.get_repo().length() == 0);
}

void testAdminServiceAdd()
{
    AdminRepository repo;
    AdminService service0{ repo };
    service0.add(0, 100, 200, "Blue", "Nope");
    assert(service0.get_repo().length() == 1);
}

void testAdminServiceRemove()
{
    AdminRepository repo;
    AdminService service0{ repo };
    service0.add(0, 100, 0, "Blue", "Nope");
    assert(service0.get_repo().length() == 1);

    service0.remove(0);
    assert(service0.get_repo().length() == 0);

    try
    {
        service0.remove(100);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().CoatNotFound);
    }
}

void testAdminServiceUpdate()
{
    AdminRepository repo;
    AdminService service0{ repo };
    service0.add(0, 100, 200, "Blue", "Nope");
    assert(service0.get_repo().length() == 1);

    service0.update(0, 300, 400, 500, "Red", "Link", (1 | 2 | 4 | 8 | 16));

    Coat coat = service0.get_coat_by_ID(0);
    assert(coat.get_ID() == 0);
    assert(coat.get_size() == 300);
    assert(coat.get_price() == 400);
    assert(coat.get_quantity() == 500);
    assert(coat.get_color() == "Red");
    assert(coat.get_photo() == "Link");

    try
    {
        service0.update(100, 0, 0, 0, "", "", 0);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().CoatNotFound);
    }

    service0.update(0, 100, 100, 100, "-", "-", 0);
    coat = service0.get_coat_by_ID(0);
    assert(coat.get_ID() == 0);
    assert(coat.get_size() == 300);
    assert(coat.get_price() == 400);
    assert(coat.get_quantity() == 500);
    assert(coat.get_color() == "Red");
    assert(coat.get_photo() == "Link");
}

void testAdminServiceGet()
{
    AdminRepository repo;
    AdminService service{ repo };
    Coat coat0(0, 100, 200, 0, "Blue", "Link");

    service.add(100, 200, 0, "Blue", "Link");

    Coat repoCoat = service.get_coat_by_ID(0);

    assert(repoCoat == coat0);

    try
    {
        Coat repoCoat = service.get_coat_by_ID(100);
        assert(false);
    }
    catch (std::string error)
    {
        assert(error == Errors().CoatNotFound);
    }
}

void testAdminServiceStream()
{
    AdminRepository repo;
    AdminService service{ repo };
    service.add(20, 30, 40, "Red", "Link");

    std::stringstream out;
    out << service;
    assert(out.str() == "ID: 0          Size: 20         Price: 30$        Quantity: 40         Color: Red          Photo: Link                \n");

}


void testUserRepoCreate()
{
    UserRepository repo;
    UserRepository repo1{ repo };

    assert(repo.length() == repo1.length());
    assert(repo.length() == 0);

    repo.add(0);
    UserRepository repo2, repo3;
    repo2 = repo3 = repo;
    assert(repo2.length() == repo.length());
    assert(repo3.length() == repo2.length());
    assert(repo2.length() == 1);
}

void testUserRepoAdd()
{
    UserRepository repo;
    repo.add(1);
    repo.add(2);

    assert(repo.length() == 2);
}

void testUserRepoOperators()
{
    UserRepository repo;
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
    UserRepository repo;
    AdminRepository adminRepo;
    Coat c0 = Coat(0, 30, 20, 1, "Red", "Nope");
    Coat c1 = Coat(1, 30, 20, 2, "Red", "Nope");
    adminRepo.add(c0);
    adminRepo.add(c1);

    repo.add(0);
    repo.add(1);

    assert(repo.get_total_price(adminRepo) == 40);
}

void testAdminServiceGetBySize()
{
    
    AdminRepository repo;
    AdminService service{ repo };
    service.add(30, 20, 1, "Red", "Nope");

    assert(service.get_coats_by_size(30).size() == 1);
    assert(service.get_coats_by_size(0).size() == 1);
}

void testAdminRepoGetBySize()
{
    AdminRepository repo;
    Coat c0 = Coat(0, 30, 20, 1, "Red", "Nope");
    repo.add(c0);

    assert(repo.get_coats_by_size(30).size() == 1);
    assert(repo.get_coats_by_size(0).size() == 1);
}

void testUserServiceCreate()
{
    UserRepository repo;
    UserService service{ repo };
    UserService service1{ service };

    assert(service.get_repo().length() == service1.get_repo().length());
}

void testUserServiceAdd()
{
    UserRepository repo;
    UserService service{ repo };

    service.add(0);
    service.add(1);

    assert(service.get_repo().length() == 2);
}

void testUserServiceGetRepo()
{
    UserRepository repo;
    UserService service{ repo };

    service.add(0);
    service.add(1);

    repo.add(0);
    repo.add(1);

    assert(service.get_repo().length() == repo.length());
}

void testUserServiceGetPrice()
{
    UserRepository repo;
    UserService service{ repo };

    AdminRepository adminRepo;
    Coat c0 = Coat(0, 30, 20, 1, "Red", "Nope");
    Coat c1 = Coat(1, 30, 20, 2, "Red", "Nope");
    adminRepo.add(c0);
    adminRepo.add(c1);

    service.add(0);
    service.add(1);

    assert(service.get_total_price(adminRepo) == 40);

}