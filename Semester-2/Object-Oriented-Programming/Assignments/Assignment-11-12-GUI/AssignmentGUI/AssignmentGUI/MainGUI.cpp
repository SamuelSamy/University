#include "MainGUI.h"


MainGUI::MainGUI()
{
	this->mode = "";
	this->initGUI();
	this->connectSignalsAndSlots();
}

std::string MainGUI::getUserMode()
{
	return this->mode;
}

void MainGUI::initGUI()
{
	QVBoxLayout* layout = new QVBoxLayout{ this };

	QFormLayout* formLayout = new QFormLayout{};
	QLabel* labelInfo = new QLabel{ "Select application mode" };
	labelInfo->setFont(QFont("Times"));
	formLayout->addRow(labelInfo);

	QWidget* options = new QWidget{};
	QHBoxLayout* buttonLayout = new QHBoxLayout{};

	this->adminButton = new QPushButton{ "&Admin" };
	this->userButton = new QPushButton{ "&User" };

	this->adminButton->setFont(QFont("Times"));
	this->userButton->setFont(QFont("Times"));


	buttonLayout->addWidget(adminButton);
	buttonLayout->addWidget(userButton);
	options->setLayout(buttonLayout);

	layout->addLayout(formLayout);
	layout->addWidget(options);
}

void MainGUI::connectSignalsAndSlots()
{
	QObject::connect(this, &MainGUI::buttonPressedSignal, this, &MainGUI::handleInput);

	QObject::connect(this->adminButton, &QPushButton::clicked, this, &MainGUI::setAdminMode);
	QObject::connect(this->userButton, &QPushButton::clicked, this, &MainGUI::setUserMode);

}

void MainGUI::setAdminMode()
{
	std::string mode = "admin";
	emit buttonPressedSignal(mode);
}

void MainGUI::setUserMode()
{
	std::string mode = "user";
	emit buttonPressedSignal(mode);
}

void MainGUI::handleInput(std::string mode)
{
	this->mode = mode;
	this->close();
}
