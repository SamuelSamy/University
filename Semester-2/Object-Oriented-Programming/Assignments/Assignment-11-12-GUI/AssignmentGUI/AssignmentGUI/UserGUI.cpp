#include "UserGUI.h"
#include <string>

UserGUI::UserGUI(const UserService& _service, const AdminService& _adminService) : service(_service), adminService(_adminService)
{
    this->initGUI();
    this->connectSignalsAndSlots();
}

void UserGUI::initGUI()
{
    QHBoxLayout* layout = new QHBoxLayout{ this };


    QWidget* buttonsWidget = new QWidget{};
    QHBoxLayout* hLlayout = new QHBoxLayout{ buttonsWidget };

    this->basketButton = new QPushButton{ "&See basket" };
    this->buyingButton = new QPushButton{ "&Buy" };
    this->repoButton = new QPushButton{ "&Open repository" };

    QFont font{ "Times", 18 };

    this->basketButton->setFont(font);
    this->buyingButton->setFont(font);
    this->repoButton->setFont(font);

    hLlayout->addWidget(this->basketButton);
    hLlayout->addWidget(this->buyingButton);
    hLlayout->addWidget(this->repoButton);

    layout->addWidget(buttonsWidget);
}

void UserGUI::connectSignalsAndSlots()
{
    QObject::connect(this->basketButton, &QPushButton::clicked, this, &UserGUI::handleBasketGUI);
    QObject::connect(this->buyingButton, &QPushButton::clicked, this, &UserGUI::handleBuyingGUI);
    QObject::connect(this->repoButton, &QPushButton::clicked, this, &UserGUI::handleRepo);


    QObject::connect(this, &UserGUI::basketSignal, this, &UserGUI::openBasketGUI);
    QObject::connect(this, &UserGUI::buyingSignal, this, &UserGUI::openBuyingGUI);
    QObject::connect(this, &UserGUI::repoSignal, this, &UserGUI::openRepo);
}


void UserGUI::handleBasketGUI()
{
    emit basketSignal();
}

void UserGUI::handleBuyingGUI()
{
    emit buyingSignal();
}

void UserGUI::handleRepo()
{
    emit repoSignal();
}


void UserGUI::openBasketGUI()
{
    BasketGUI basket{this->service, this->adminService};
    basket.exec();
}


void UserGUI::openBuyingGUI()
{
    SizeBuyingGUI sizeBuying;
    sizeBuying.exec();
    int size = sizeBuying.get_size();
    BuyingGUI buying{ this->service, this->adminService, size };
    buying.exec();
}


void UserGUI::openRepo()
{
    system(this->service.get_file_name().c_str());
}

BasketGUI::BasketGUI(UserService& _service, AdminService& _adminService) : service(_service), adminService(_adminService)
{
    this->initGUI();
}

void BasketGUI::initGUI()
{
    QFont font{ "Times", 18 };

    QVBoxLayout* layout = new QVBoxLayout{ this };

    this->coatsList = new QListWidget{};
    this->coatsList->setSelectionMode(QAbstractItemView::NoSelection);
    this->coatsList->setFont(font);
   
    UserRepository* repo = this->service.get_repo();
    std::vector<std::pair<int, int>> v = repo->get_vector();
    for (const auto& entry : v)
    {
        Coat c = this->adminService.get_coat_by_ID(entry.first);
        c.set_quantity(entry.second);

        QString stringItem = QString::fromStdString(
            std::to_string(c.get_ID()) + " " +
            std::to_string(c.get_size()) + " " +
            std::to_string(c.get_price()) + " " +
            std::to_string(c.get_quantity()) + " " +
            c.get_color()
        );

        QListWidgetItem* item = new QListWidgetItem{ stringItem };
        item->setFont(font);
        this->coatsList->addItem(item);
    }

    std::string price = "Total price: $" + std::to_string(this->service.get_total_price());
    this->priceLabel = new QLabel{ price.c_str()};
    this->priceLabel->setFont(font);


    layout->addWidget(this->coatsList);
    layout->addWidget(this->priceLabel);
}

void BuyingGUI::updateLabels()
{
    Coat c = this->coats[index];

    this->IDLabel->setText(QString::fromStdString(std::string("ID: " + std::to_string(c.get_ID()))));
    this->sizeLabel->setText(QString::fromStdString(std::string("Size: " + std::to_string(c.get_size()))));
    this->priceLabel->setText(QString::fromStdString(std::string("Price: " + std::to_string(c.get_price()))));
    this->quantityLabel->setText(QString::fromStdString(std::string("Quantity: " + std::to_string(c.get_quantity()))));
    this->colorLabel->setText(QString::fromStdString(std::string("Color: " + c.get_color())));
    this->totalPriceLabel->setText(QString::fromStdString(std::string("Total price: $" + std::to_string(this->service.get_total_price()))));

    std::string link = "start " + c.get_photo();
    system(link.c_str());
}

void BuyingGUI::connectSignalsAndSlots()
{
    QObject::connect(this->buyButton, &QPushButton::clicked, this, &BuyingGUI::buyButtonHandler);
    QObject::connect(this->nextButton, &QPushButton::clicked, this, &BuyingGUI::nextButtonHandler);

    QObject::connect(this, &BuyingGUI::buySignal, this, &BuyingGUI::buyCoat);
    QObject::connect(this, &BuyingGUI::nextSignal, this, &BuyingGUI::nextCoat);
}


void BuyingGUI::nextButtonHandler()
{
    emit nextSignal();
}

void BuyingGUI::buyButtonHandler()
{
    emit buySignal();
}

void BuyingGUI::nextCoat()
{
    this->index++;
    this->index %= this->coats.size();
    this->updateLabels();
}

void BuyingGUI::buyCoat()
{
    Coat coat = this->coats[this->index];

    if (coat.get_quantity() < 1)
    {
        QMessageBox msgBox;
        msgBox.setText(std::string("This coat is out of stock.").c_str());
        msgBox.exec();
        return;
    }

    this->coats[this->index].set_quantity(coat.get_quantity() - 1);
    this->adminService.update(coat.get_ID(), coat.get_size(), coat.get_price(), coats[index].get_quantity(), coat.get_color(), coat.get_photo());
    this->service.add(coat.get_ID());

    QMessageBox msgBox;
    msgBox.setText(std::string("Coat successfully added to the basket").c_str());
    msgBox.exec();

    emit nextSignal();
}

BuyingGUI::BuyingGUI(UserService& _service, AdminService& _adminService, int size) : service(_service), adminService(_adminService)
{
    this->index = 0;
    this->size = size;
    this->coats = this->adminService.get_coats_by_size(this->size);
    this->initGUI();
    this->connectSignalsAndSlots();
}


void BuyingGUI::initGUI()
{
    QVBoxLayout* layout = new QVBoxLayout{ this };

    QFont font{ "Times", 18 };

    Coat c = this->coats[0];

    this->IDLabel = new QLabel{ std::string("ID: " + std::to_string(c.get_ID())).c_str() };
    this->sizeLabel = new QLabel{ std::string("Size: " + std::to_string(c.get_size())).c_str() };
    this->priceLabel = new QLabel{ std::string("Price: " + std::to_string(c.get_price())).c_str() };
    this->quantityLabel = new QLabel{ std::string("Quantity: " + std::to_string(c.get_quantity())).c_str() };
    this->colorLabel = new QLabel{ std::string("Color: " + c.get_color()).c_str() };
    this->totalPriceLabel = new QLabel{ std::string("Total price: $" + std::to_string(this->service.get_total_price())).c_str()};

    IDLabel->setFont(font);
    sizeLabel->setFont(font);
    priceLabel->setFont(font);
    quantityLabel->setFont(font);
    colorLabel->setFont(font);
    this->totalPriceLabel->setFont(font);

    layout->addWidget(this->IDLabel);
    layout->addWidget(this->sizeLabel);
    layout->addWidget(this->priceLabel);
    layout->addWidget(this->quantityLabel);
    layout->addWidget(this->colorLabel);
    layout->addWidget(this->totalPriceLabel);


    QWidget* buttonsWidget = new QWidget{};
    QHBoxLayout* hLayout = new QHBoxLayout{ buttonsWidget };

    this->buyButton = new QPushButton{ "&Buy" };
    this->nextButton = new QPushButton{ "&Next" };

    this->buyButton->setFont(font);
    this->nextButton->setFont(font);

    hLayout->addWidget(this->buyButton);
    hLayout->addWidget(this->nextButton);

    layout->addWidget(buttonsWidget);

    std::string link = "start " + c.get_photo();
    system(link.c_str());
}


SizeBuyingGUI::SizeBuyingGUI()
{
    this->size = 0;
    this->initGUI();
    this->connectSignalsAndSlots();
}

void SizeBuyingGUI::initGUI()
{
    QVBoxLayout* layout = new QVBoxLayout{ this };

    QFont font{ "Times", 18 };

    QWidget* sizeWidget = new QWidget{};
    QFormLayout* formLayout = new QFormLayout{ sizeWidget };

    QLabel* sizeLabel = new QLabel{ "&Enter the size (0 for all): " };
    this->sizeEdit = new QLineEdit{};

    this->sizeEdit->setFont(font);

    sizeLabel->setFont(font);

    sizeLabel->setBuddy(this->sizeEdit);

    formLayout->addRow(sizeLabel, sizeEdit);

    layout->addWidget(sizeWidget);

    QWidget* buttonsWidget = new QWidget{};
    QHBoxLayout* hLayout = new QHBoxLayout{ buttonsWidget };

    this->enterButton = new QPushButton{ "&Continue" };
    this->enterButton->setFont(font);

    hLayout->addWidget(this->enterButton);
    layout->addWidget(buttonsWidget);
}

void SizeBuyingGUI::connectSignalsAndSlots()
{
    QObject::connect(this->enterButton, &QPushButton::clicked, this, &SizeBuyingGUI::setSizeHandler);
    QObject::connect(this, &SizeBuyingGUI::setSizeSignal, this, &SizeBuyingGUI::setSize);
}



int SizeBuyingGUI::get_size()
{
    return this->size;
}

void SizeBuyingGUI::setSizeHandler()
{
    int size = atoi(this->sizeEdit->text().toStdString().c_str());
    emit setSizeSignal(size);
}

void SizeBuyingGUI::setSize(int size)
{
    this->size = size;
    this->close();
}