#include "QtWidgetsApplication1.h"
#include <QtWidgets/QApplication>
#include "chat_window.h"
#include "chat_session.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    ChatSession session;
    ChatWindow w0{ "Samy", session };
    ChatWindow w1{ "Ionut", session };
    ChatWindow w2{ "Razvan", session };
    ChatWindow w3{ "Rares", session };
    
    w0.show();
    w1.show();
    w2.show();
    w3.show();

    return a.exec();
}
