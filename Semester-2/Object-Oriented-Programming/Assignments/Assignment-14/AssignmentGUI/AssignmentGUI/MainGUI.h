#pragma once

#include "everything.h"

class MainGUI : public QDialog
{
    Q_OBJECT


public:
    MainGUI();

    std::string getUserMode();

private:

    QPushButton* adminButton;
    QPushButton* userButton;
    std::string mode;


    void initGUI();
    void connectSignalsAndSlots();

    void setAdminMode();
    void setUserMode();

    void handleInput(std::string mode);

signals:
    void buttonPressedSignal(std::string mode);
};