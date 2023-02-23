#pragma once

#include <QtWidgets/QMainWindow>
#include "ui_AssignmentGUI.h"

class AssignmentGUI : public QMainWindow
{
    Q_OBJECT

public:
    AssignmentGUI(QWidget *parent = Q_NULLPTR);

private:
    Ui::AssignmentGUIClass ui;
};
