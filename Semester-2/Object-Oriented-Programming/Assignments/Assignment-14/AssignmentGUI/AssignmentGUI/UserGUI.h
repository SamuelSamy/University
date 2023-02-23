#pragma once

#include <vector>
#include "everything.h"
#include "user_service.h"
#include "BasketModel.h"

class UserGUI : public QWidget
{
    Q_OBJECT

private:

    UserService service;
    AdminService adminService;

    QPushButton* basketButton;
    QPushButton* buyingButton;
    QPushButton* repoButton;
    BasketModel* model;

    void handleBasketGUI();
    void handleBuyingGUI();
    void handleRepo();

    void initGUI();
    void connectSignalsAndSlots();

public:
    UserGUI(const UserService& _service, const AdminService& _adminService);

signals: void basketSignal();
signals: void buyingSignal();
signals: void repoSignal();

public slots: void openBasketGUI();
public slots: void openBuyingGUI();
public slots: void openRepo();
};


class TableViewWidget : public QDialog
{
    Q_OBJECT

public:
    TableViewWidget(BasketModel* model, QWidget* parent = Q_NULLPTR);
    ~TableViewWidget();
};

class BasketGUI : public QDialog
{
    Q_OBJECT

private:
    UserService& service;
    AdminService& adminService;


public:
    BasketGUI(UserService& _service, AdminService& _adminService);

    QListWidget* coatsList;
    QLabel* priceLabel;

    void initGUI();
};


class SizeBuyingGUI : public QDialog
{
    Q_OBJECT

private:
    int size;

    QPushButton* enterButton;
    QLineEdit* sizeEdit;
    void initGUI();
    void connectSignalsAndSlots();


public:

    SizeBuyingGUI();

    int get_size();
    void setSizeHandler();
    
signals: void setSizeSignal(int size);

public slots: void setSize(int size);
};



class BuyingGUI : public QDialog
{
    Q_OBJECT

private:
    
    std::vector<Coat> coats;

    UserService& service;
    AdminService& adminService;
    BasketModel& model;


    QPushButton* nextButton;
    QPushButton* buyButton;

    QLabel* IDLabel;
    QLabel* sizeLabel;
    QLabel* priceLabel;
    QLabel* quantityLabel;
    QLabel* colorLabel;
    QLabel* totalPriceLabel;

    void initGUI();
    int size;
    int index;

    void connectSignalsAndSlots();

    void updateLabels();
    void nextButtonHandler();
    void buyButtonHandler();


signals: void nextSignal();
signals: void buySignal();

public slots: void nextCoat();
public slots: void buyCoat();

public:
    BuyingGUI(BasketModel& model, UserService& _service, AdminService& _adminService, int size);

};