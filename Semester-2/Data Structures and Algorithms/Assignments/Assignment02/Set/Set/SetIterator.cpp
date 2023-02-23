#include "SetIterator.h"
#include "Set.h"
#include <exception>

// Theta(1)
SetIterator::SetIterator(const Set& m) : set(m)
{
	this->node = this->set.head;
}

// Theta(1)
void SetIterator::first() {
	this->node = this->set.head;
}

// Theta(1)
void SetIterator::next() {
	if (this->node == nullptr)
	{
		throw std::exception();
	}
	this->node = this->node->next;
}

// Theta(1)
TElem SetIterator::getCurrent()
{
	if (!this->valid())
	{
		throw std::exception();
	}

	return this->node->value;
}

// Theta(1)
bool SetIterator::valid() const {
	return this->node != nullptr;
}



