#include "BasketModel.h"
#include <QFont>

BasketModel::BasketModel(UserRepository& userRepo, Repository& adminRepo) : userRepo(userRepo), adminRepo(adminRepo)
{

}

int BasketModel::rowCount(const QModelIndex& parent) const
{
    return this->userRepo.get_vector().size();
}

int BasketModel::columnCount(const QModelIndex& parent) const
{
    return 5;
}

QVariant BasketModel::data(const QModelIndex& index, int role) const
{
	int row = index.row();
	int column = index.column();

	std::pair<int, int> p = this->userRepo.get_vector().at(row);
	Coat c = this->adminRepo.get_coat_by_ID(p.first);

	if (role == Qt::DisplayRole)
	{
		switch (column)
		{
			case 0:
				return QString::number(c.get_ID());

			case 1:
				return QString::number(c.get_size());

			case 2:
				return QString::number(c.get_price());

			case 3:
				return QString::number(p.second);

			case 4:
				return QString::fromStdString(c.get_color());

			default:
				break;
		}
	}

	if (role == Qt::FontRole)
	{
		QFont font{ "Times", 16 };
		return font;
	}

	return QVariant();
}

QVariant BasketModel::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (orientation == Qt::Horizontal)
	{
		if (role == Qt::DisplayRole)
		{
			switch (section)
			{
				case 0:
					return QString{ "ID" };

				case 1:
					return QString{ "Size" };

				case 2:
					return QString{ "Price" };

				case 3:
					return QString{ "Quantity" };

				case 4:
					return QString{ "Color" };
				default:
					break;
			}
		}
	}

	if (role == Qt::FontRole)
	{
		QFont font{ "Times", 18 };
		return font;
	}

	return QVariant();
}

void BasketModel::addCoat(int id)
{
	int n = this->userRepo.get_vector().size();

	this->beginInsertRows(QModelIndex{}, n, n);

	this->userRepo.add(id);

	this->endInsertRows();
}

UserRepository& BasketModel::getUserRepo()
{
	return this->userRepo;
}

Repository& BasketModel::getAdminRepo()
{
	return this->adminRepo;
}
