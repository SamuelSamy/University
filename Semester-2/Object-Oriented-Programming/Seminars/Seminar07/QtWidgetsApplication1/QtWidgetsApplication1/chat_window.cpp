#include "chat_window.h"


ChatWindow::ChatWindow(std::string owner, ChatSession& _session) : session(_session)
{
    this->owner = owner;

    QFont font{ "Times", 16 };

    QVBoxLayout* layout = new QVBoxLayout{ this };

    QLabel* label = new QLabel{};
    label->setText(QString::fromStdString(owner));
    label->setFont(font);
    layout->addWidget(label);
    
    this->messagesList = new QListWidget{};
    this->messagesList->setFont(font);
    layout->addWidget(this->messagesList);

    QWidget* textWidget = new QWidget{};
    QHBoxLayout* textLayout = new QHBoxLayout{ textWidget };


    this->textBox = new QLineEdit{};
    this->textBox->setFont(font);
    textLayout->addWidget(textBox);

    this->sendButton = new QPushButton{};
    this->sendButton->setText(QString::fromStdString("Send"));
    this->sendButton->setFont(font);
    textLayout->addWidget(sendButton);
    
    layout->addWidget(textWidget);

    session.registerObserver(this);

    QObject::connect(this->sendButton, &QPushButton::clicked, this, &ChatWindow::sendMessage);
}

ChatWindow::~ChatWindow()
{
    this->session.unregisterObsedrver(this);
}


void ChatWindow::update()
{
    this->messagesList->clear();

    QListWidgetItem* item;

    for (const auto &message : this->session.getMessages())
    {
        if (message.first == owner)
        {
            item = new QListWidgetItem{ QString::fromStdString("[You]: " + message.second)};
            item->setTextAlignment(Qt::AlignRight); // <- from Rares Descas
        }
        else
        {
            item = new QListWidgetItem{ QString::fromStdString("[" + message.first + "]: " + message.second) };
        }
        this->messagesList->addItem(item);
    }
}

void ChatWindow::sendMessage()
{
    this->session.addMessage(owner, this->textBox->text().toStdString());
    this->textBox->setText(QString::fromStdString(""));
    this->session.notify();
}

