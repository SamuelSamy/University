#include "SMMIterator.h"
#include "SortedMultiMap.h"
#include <algorithm>

// Theta(n^2)
SMMIterator::SMMIterator(const SortedMultiMap& d) : map(d){
	this->index = 0;
	this->sortedElements = new TElem[this->map.nrElements + 10];

	int currentIndex = 0;

	for (int i = 0; i < this->map.capacity; i++)
	{
		if (this->map.elements[i] != NULL_TELEM)
		{
			this->sortedElements[currentIndex++] = this->map.elements[i];
		}
	}



	bool sorted = true;
	do
	{
		sorted = true;

		for (int i = 0; i < this->map.size() - 1; i++)
		{
			if (!map.r(this->sortedElements[i].first, this->sortedElements[i + 1].first))
			{
				std::swap(this->sortedElements[i], this->sortedElements[i + 1]);
				sorted = false;
			}
		}
	} while (!sorted);
}

// Theta(1)
void SMMIterator::first(){
	this->index = 0;
}

// Theta(1)
void SMMIterator::next(){
	if (!this->valid())
	{
		throw exception();
	}

	this->index++;
}

// Theta(1)
bool SMMIterator::valid() const{
	return this->index < this->map.nrElements;
}

// Theta(1)
TElem SMMIterator::getCurrent() const{
	if (!this->valid())
	{
		throw exception();
	}

	return this->sortedElements[this->index];
}

// Theta(1)
SMMIterator::~SMMIterator()
{
	delete[] this->sortedElements;
}


