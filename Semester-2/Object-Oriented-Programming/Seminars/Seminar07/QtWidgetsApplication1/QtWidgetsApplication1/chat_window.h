#pragma once

#include "chat_session.h"
#include "observer.h"
#include <QWidget>
#include <QtWidgets/QLabel>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QWidget>
#include <QtWidgets/QDialog>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QMessageBox>

class ChatWindow : public QWidget, public Observer
{
    Q_OBJECT

private:
    std::string owner;
    ChatSession& session;

    QLineEdit* textBox;
    QListWidget* messagesList;
    QPushButton* sendButton;

public:

    ChatWindow(std::string owner, ChatSession& _session);
    ~ChatWindow();

    void update() override;
    void sendMessage();
};