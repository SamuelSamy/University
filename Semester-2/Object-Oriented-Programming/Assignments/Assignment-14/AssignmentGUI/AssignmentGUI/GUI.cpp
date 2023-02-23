#include "GUI.h"

RepositoryOptionGUI::RepositoryOptionGUI()
{
	this->option = "";
    this->initGUI();
	this->connectSignalsAndSlots();
}

std::string RepositoryOptionGUI::getOption()
{
	return this->option;
}

void RepositoryOptionGUI::initGUI()
{
	QVBoxLayout* layout = new QVBoxLayout{ this };

	QFormLayout* formLayout = new QFormLayout{};
	QLabel* labelInfo = new QLabel{ "Select the type of the repository" };
	labelInfo->setFont(QFont("Times"));
	formLayout->addRow(labelInfo);

	QWidget* options = new QWidget{};
	QHBoxLayout* buttonLayout = new QHBoxLayout{};

	this->htmlButton = new QPushButton{ "&HTML" };
	this->csvButton = new QPushButton{ "&CSV" };

	this->htmlButton->setFont(QFont("Times"));
	this->csvButton->setFont(QFont("Times"));

	buttonLayout->addWidget(htmlButton);
	buttonLayout->addWidget(csvButton);
	options->setLayout(buttonLayout);

	layout->addLayout(formLayout);
	layout->addWidget(options);
}

void RepositoryOptionGUI::connectSignalsAndSlots()
{
	QObject::connect(this, &RepositoryOptionGUI::buttonPressedSignal, this, &RepositoryOptionGUI::handleOption);

	QObject::connect(this->htmlButton, &QPushButton::clicked, this, &RepositoryOptionGUI::setOptionToHTML);
	QObject::connect(this->csvButton, &QPushButton::clicked, this, &RepositoryOptionGUI::setOptionToCSV);


}

void RepositoryOptionGUI::setOptionToHTML()
{
	std::string option = "html";
	emit buttonPressedSignal(option);
}

void RepositoryOptionGUI::setOptionToCSV()
{
	std::string option = "csv";
	emit buttonPressedSignal(option);
}

void RepositoryOptionGUI::handleOption(std::string option)
{
	this->option = option;
	this->close();
}

