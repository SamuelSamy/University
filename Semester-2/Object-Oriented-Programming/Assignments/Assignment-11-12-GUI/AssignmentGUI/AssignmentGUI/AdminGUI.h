#pragma once

#include "everything.h"
#include "admin_service.h"

class AdminGUI : public QWidget
{
    Q_OBJECT

public:
    AdminGUI(const AdminService& _service);

private:
    AdminService service;

    QListWidget* coatList;

    QLabel* IDLabelView;
    QLineEdit* sizeEdit;
    QLineEdit* priceEdit;
    QLineEdit* quantityEdit;
    QLineEdit* colorEdit;
    QLineEdit* photoEdit;

    QPushButton* addButton;
    QPushButton* removeButton;
    QPushButton* updateButton;
    QPushButton* filterButton;

    void initGUI();
    void connectSignalsAndSlots();


    void populateCoatList();

    int getSelectedIndex();
    void listItemChanged();

    void addCoatButtonHandler();
    void removeCoatButtonHandler();
    void updateCoatButtonHandler();
    void handleFilter();

signals: void coatsUpdatedSignal();
signals: void addCoatSignal(int size, int price, int quantity, std::string color, std::string photo);
signals: void removeCoatSignal(int ID);
signals: void updateCoatSignal(int ID, int size, int price, int quantity, std::string color, std::string photo);
signals: void filterSignal();

public slots: void addCoat(int size, int price, int quantity, std::string color, std::string photo);
public slots: void removeCoat(int ID);
public slots: void updateCoat(int ID, int size, int price, int quantity, std::string color, std::string photo);
public slots: void openFilter();
};


class FilterGUI : public QDialog
{
    Q_OBJECT

private:

    Repository& repo;

    QLineEdit* searchBar;
    QListWidget* list;

    void initGUI();
    void connectSignalsAndSlots();

    void handlePopulate();

public:
    FilterGUI(Repository& _repo);

signals: void populateFilterSignal(std::string text);

public slots: void populateFilter(std::string text);
};