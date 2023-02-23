#pragma once

#include "everything.h"

class RepositoryOptionGUI : public QDialog
{
    Q_OBJECT

public:
    RepositoryOptionGUI();

    std::string getOption();


private:

    QPushButton* htmlButton;
    QPushButton* csvButton;
    std::string option;

    void initGUI();
    void connectSignalsAndSlots();


    void setOptionToHTML();
    void setOptionToCSV();

    void handleOption(std::string option);

signals:
    void buttonPressedSignal(std::string option);

};