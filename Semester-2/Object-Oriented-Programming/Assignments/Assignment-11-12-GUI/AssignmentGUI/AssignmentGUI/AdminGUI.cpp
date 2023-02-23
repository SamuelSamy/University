#include "AdminGUI.h"
#include <string>
#include "errors.h"

AdminGUI::AdminGUI(const AdminService& _service) : service(_service)
{
    this->initGUI();
    this->connectSignalsAndSlots();

    this->populateCoatList();
};

void AdminGUI::initGUI()
{
    QHBoxLayout* layout = new QHBoxLayout{ this };

    this->coatList = new QListWidget{};
    this->coatList->setSelectionMode(QAbstractItemView::SingleSelection);


    QWidget* rightSide = new QWidget{};
    QVBoxLayout* vLayout = new QVBoxLayout{ rightSide };

    QWidget* coatDataWidget = new QWidget{};
    QFormLayout* formLayout = new QFormLayout{ coatDataWidget };

    this->sizeEdit = new QLineEdit{};
    this->priceEdit = new QLineEdit{};
    this->quantityEdit = new QLineEdit{};
    this->colorEdit = new QLineEdit{};
    this->photoEdit = new QLineEdit{};

    QFont font{ "Times", 18 };

    this->IDLabelView = new QLabel{};
    QLabel* IDLabel = new QLabel{ "&ID: " };
    QLabel* sizeLabel = new QLabel{ "&Size: " };
    QLabel* priceLabel = new QLabel{ "&Price: " };
    QLabel* quantityLabel = new QLabel{ "&Quantitity: " };
    QLabel* colorLabel = new QLabel{ "&Color: " };
    QLabel* photoLabel = new QLabel{ "&Photo: " };

    IDLabel->setBuddy(this->IDLabelView);
    sizeLabel->setBuddy(this->sizeEdit);
    priceLabel->setBuddy(this->priceEdit);
    quantityLabel->setBuddy(this->quantityEdit);
    colorLabel->setBuddy(this->colorEdit);
    photoLabel->setBuddy(this->photoEdit);

    IDLabel->setFont(font);
    sizeLabel->setFont(font);
    priceLabel->setFont(font);
    quantityLabel->setFont(font);
    colorLabel->setFont(font);
    photoLabel->setFont(font);

    this->IDLabelView->setFont(font);
    this->sizeEdit->setFont(font);
    this->priceEdit->setFont(font);
    this->quantityEdit->setFont(font);
    this->colorEdit->setFont(font);
    this->photoEdit->setFont(font);

    formLayout->addRow(IDLabel, this->IDLabelView);
    formLayout->addRow(sizeLabel, this->sizeEdit);
    formLayout->addRow(priceLabel, this->priceEdit);
    formLayout->addRow(quantityLabel, this->quantityEdit);
    formLayout->addRow(colorLabel, this->colorEdit);
    formLayout->addRow(photoLabel, this->photoEdit);

    vLayout->addWidget(coatDataWidget);


    QWidget* buttonsWidget = new QWidget{};
    QHBoxLayout* hLayout = new QHBoxLayout{ buttonsWidget };

    this->addButton = new QPushButton{ "&Add" };
    this->removeButton = new QPushButton{ "&Remove" };
    this->updateButton = new QPushButton{ "&Update" };
    this->filterButton = new QPushButton{ "&Filter" };

    this->addButton->setFont(font);
    this->removeButton->setFont(font);
    this->updateButton->setFont(font);
    this->filterButton->setFont(font);

    hLayout->addWidget(this->addButton);
    hLayout->addWidget(this->removeButton);
    hLayout->addWidget(this->updateButton);
    hLayout->addWidget(this->filterButton);

    vLayout->addWidget(buttonsWidget);

    // {Gradient}
    QLinearGradient listGradient(0, 0, 0, this->coatList->height());
    QPalette palette = qApp->palette();
    palette.setBrush(QPalette::Base, listGradient);
    palette.setBrush(QPalette::Highlight, listGradient);
    palette.setBrush(QPalette::HighlightedText, Qt::white);
    palette.setBrush(QPalette::Text, Qt::white);
    this->coatList->setPalette(palette);


    layout->addWidget(this->coatList);
    layout->addWidget(rightSide);

}

void AdminGUI::connectSignalsAndSlots()
{
    QObject::connect(this, &AdminGUI::coatsUpdatedSignal, this, &AdminGUI::populateCoatList);

    QObject::connect(this->coatList, &QListWidget::itemSelectionChanged, this, [this]() {this->listItemChanged(); });

    QObject::connect(this->addButton, &QPushButton::clicked, this, &AdminGUI::addCoatButtonHandler);
    QObject::connect(this->removeButton, &QPushButton::clicked, this, &AdminGUI::removeCoatButtonHandler);
    QObject::connect(this->updateButton, &QPushButton::clicked, this, &AdminGUI::updateCoatButtonHandler);
    QObject::connect(this->filterButton, &QPushButton::clicked, this, &AdminGUI::handleFilter);


    QObject::connect(this, &AdminGUI::addCoatSignal, this, &AdminGUI::addCoat);
    QObject::connect(this, &AdminGUI::removeCoatSignal, this, &AdminGUI::removeCoat);
    QObject::connect(this, &AdminGUI::updateCoatSignal, this, &AdminGUI::updateCoat);
    QObject::connect(this, &AdminGUI::filterSignal, this, &AdminGUI::openFilter);

}

void AdminGUI::populateCoatList()
{
    QFont font{ "Times", 18 };

    if (this->coatList->count() > 0)
    {
        this->coatList->clear();
    }

    std::vector<Coat> coats = this->service.get_repo()->get_coats_by_size(0);
    for (Coat c : coats)
    {
        QString stringItem = QString::fromStdString(
            std::to_string(c.get_ID())          + " " + 
            std::to_string(c.get_size())        + " " + 
            std::to_string(c.get_price())       + " " + 
            std::to_string(c.get_quantity())    + " " + 
            c.get_color()
        );

        QListWidgetItem* item = new QListWidgetItem{ stringItem };
        item->setFont(font);
        this->coatList->addItem(item);
    }
}

int AdminGUI::getSelectedIndex()
{
    if (this->coatList->count() == 0)
    {
        return -1;
    }

    QModelIndexList index = this->coatList->selectionModel()->selectedRows();
    if (index.size() == 0)
    {
        this->IDLabelView->clear();
        this->sizeEdit->clear();
        this->priceEdit->clear();
        this->quantityEdit->clear();
        this->colorEdit->clear();
        this->photoEdit->clear();
        return -1;
    }

    return index.at(0).row();
}

void AdminGUI::listItemChanged()
{
    int index = this->getSelectedIndex();

    if (index == -1 || index >= this->coatList->count())
    {
        return;
    }

    Coat* c = this->service.get_repo()->at(index);

    this->IDLabelView->setText(QString::fromStdString(std::to_string(c->get_ID())));
    this->priceEdit->setText(QString::fromStdString(std::to_string(c->get_price())));
    this->sizeEdit->setText(QString::fromStdString(std::to_string(c->get_size())));
    this->quantityEdit->setText(QString::fromStdString(std::to_string(c->get_quantity())));
    this->colorEdit->setText(QString::fromStdString(c->get_color()));
    this->photoEdit->setText(QString::fromStdString(c->get_photo()));
}

void AdminGUI::addCoatButtonHandler()
{
    int size = atoi(this->sizeEdit->text().toStdString().c_str());
    int price = atoi(this->priceEdit->text().toStdString().c_str());
    int quantity = atoi(this->quantityEdit->text().toStdString().c_str());
    std::string color = this->colorEdit->text().toStdString();
    std::string photo = this->photoEdit->text().toStdString();

    emit addCoatSignal(size, price, quantity, color, photo);
}

void AdminGUI::removeCoatButtonHandler()
{
    int ID = atoi(this->IDLabelView->text().toStdString().c_str());
    emit removeCoatSignal(ID);
}

void AdminGUI::updateCoatButtonHandler()
{
    int ID = atoi(this->IDLabelView->text().toStdString().c_str());
    int size = atoi(this->sizeEdit->text().toStdString().c_str());
    int price = atoi(this->priceEdit->text().toStdString().c_str());
    int quantity = atoi(this->quantityEdit->text().toStdString().c_str());
    std::string color = this->colorEdit->text().toStdString();
    std::string photo = this->photoEdit->text().toStdString();

    emit updateCoatSignal(ID, size, price, quantity, color, photo);
}

void AdminGUI::handleFilter()
{
    emit filterSignal();
}

void AdminGUI::addCoat(int size, int price, int quantity, std::string color, std::string photo)
{
    std::string text = "Coat added successfully";

    try
    {
        this->service.add(size, price, quantity, color, photo);
        emit coatsUpdatedSignal();
    }
    catch (RepositoryException error)
    {
        text = error.what();
    }
    catch (ValidationException error)
    {
        text = error.what();
    }
    catch (...)
    {
        text = "Unexpected error - coat not added";
    }

    QMessageBox msgBox;
    msgBox.setText(text.c_str());
    msgBox.exec();
}

void AdminGUI::removeCoat(int ID)
{
    if (this->getSelectedIndex() == -1)
    {
        std::string text = "Please select a coat first";
        QMessageBox msgBox;
        msgBox.setText(text.c_str());
        msgBox.exec();
        return;
    }

    std::string text = "Coat removed successfully";

    try
    {
        this->service.remove(ID);
        emit coatsUpdatedSignal();
    }
    catch (RepositoryException error)
    {
        text = error.what();
    }
    catch (ValidationException error)
    {
        text = error.what();
    }
    catch (...)
    {
        text = "Unexpected error - coat not removed";
    }

    QMessageBox msgBox;
    msgBox.setText(text.c_str());
    msgBox.exec();
}

void AdminGUI::openFilter()
{
    FilterGUI filterGUI{ *this->service.get_repo() };
    filterGUI.exec();
}

void AdminGUI::updateCoat(int ID, int size, int price, int quantity, std::string color, std::string photo)
{
    if (this->getSelectedIndex() == -1)
    {
        std::string text = "Please select a coat first";
        QMessageBox msgBox;
        msgBox.setText(text.c_str());
        msgBox.exec();
        return;
    }

    std::string text = "Coat updated successfully";

    try
    {
        this->service.update(ID, size, price, quantity, color, photo);
        emit coatsUpdatedSignal();
    }
    catch (RepositoryException error)
    {
        text = error.what();
    }
    catch (ValidationException error)
    {
        text = error.what();
    }
    catch (...)
    {
        text = "Unexpected error - coat not updated";
    }

    QMessageBox msgBox;
    msgBox.setText(text.c_str());
    msgBox.exec();
}

void FilterGUI::initGUI()
{
    QVBoxLayout* layout = new QVBoxLayout{ this };

    QFont font{ "Times", 18 };

    this->list = new QListWidget{};
    this->list->setSelectionMode(QAbstractItemView::NoSelection);
    this->list->setFont(font);

    this->searchBar = new QLineEdit{};
    this->searchBar->setFont(font);

    layout->addWidget(this->searchBar);
    layout->addWidget(this->list);

    emit populateFilter("");
}

void FilterGUI::connectSignalsAndSlots()
{
    QObject::connect(this->searchBar, &QLineEdit::textChanged, this, &FilterGUI::handlePopulate);

    QObject::connect(this, &FilterGUI::populateFilterSignal, this, &FilterGUI::populateFilter);
}

void FilterGUI::handlePopulate()
{
    std::string text = this->searchBar->text().toStdString();
    emit populateFilterSignal(text);

}

FilterGUI::FilterGUI(Repository& _repo) : repo(_repo)
{
    this->initGUI();
    this->connectSignalsAndSlots();
}

void FilterGUI::populateFilter(std::string text)
{
    if (this->list->count() > 0)
    {
        this->list->clear();
    }

    QFont font{ "Times", 18 };

    std::vector<Coat> coats = this->repo.get_coats_by_size(0);
    for (Coat c : coats)
    {
        QString stringItem = QString::fromStdString(
            std::to_string(c.get_ID()) + " " +
            std::to_string(c.get_size()) + " " +
            std::to_string(c.get_price()) + " " +
            std::to_string(c.get_quantity()) + " " +
            c.get_color()
        );

        if (stringItem.toStdString().find(text) != std::string::npos || text == "")
        {
            QListWidgetItem* item = new QListWidgetItem{ stringItem };
            item->setFont(font);
            this->list->addItem(item);
        }
    }
}