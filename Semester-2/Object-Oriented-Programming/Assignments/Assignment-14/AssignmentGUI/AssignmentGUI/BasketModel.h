#pragma once

#include <QAbstractTableModel>
#include "admin_repository.h"
#include "user_repository.h"

class BasketModel : public QAbstractTableModel
{
private:
    UserRepository& userRepo;
    Repository& adminRepo;

public:
	BasketModel(UserRepository& userRepo, Repository& adminRepo);

	int rowCount(const QModelIndex& parent = QModelIndex()) const override;
	int columnCount(const QModelIndex& parent = QModelIndex()) const override;
	QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;

	QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

	void addCoat(int id);

	UserRepository& getUserRepo();
	Repository& getAdminRepo();
};

