#include "SMMIterator.h"
#include "SortedMultiMap.h"
#include <iostream>
#include <vector>
#include <exception>
using namespace std;

// Theta(1)
int SortedMultiMap::hash(TKey key) const
{
	return abs(key) % this->capacity;
}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
int SortedMultiMap::getFirstEmpty()
{
	for (int i = this->capacity - 1; i >= 0; i--)
	{
		if (this->elements[i] == NULL_TELEM)
		{
			return i;
		}
	}

	return -1;
}

// Theta(n)
SortedMultiMap::SortedMultiMap(Relation r) {
	this->r = r;
	this->nrElements = 0;
	this->capacity = 8;
	this->elements = new TElem[this->capacity];
	this->next = new int[this->capacity];

	for (int i = 0; i < this->capacity; i++)
	{
		this->next[i] = NULL_TVALUE;
		this->elements[i] = NULL_TELEM;
	}
}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n) // O(1) amortized
void SortedMultiMap::add(TKey c, TValue v) {
	
	if (1.0 * this->nrElements / this->capacity >= 0.7)
	{
		TElem* copyElems = this->elements;
		this->capacity *= 2;
		this->nrElements = 0;
		
		this->elements = new TElem[this->capacity];
		
		delete[] this->next;
		this->next = new int[this->capacity];

		for (int i = 0; i < this->capacity; i++)
		{
			this->next[i] = NULL_TVALUE;
			this->elements[i] = NULL_TELEM;
		}

		for (int i = 0; i < this->capacity / 2; i++)
		{
			if (copyElems[i] != NULL_TELEM)
			{
				this->add(copyElems[i].first, copyElems[i].second);

			}
		}

		delete[] copyElems;
	}

	int pos = this->hash(c);
	if (this->elements[pos] == NULL_TELEM)
	{
		this->elements[pos] = make_pair(c, v);
		this->nrElements++;
		return;
	}

	int prev = pos;
	while (this->next[pos] != NULL_TVALUE)
	{
		pos = this->next[pos];
		prev = pos;
	}

	int firstEmpty = this->getFirstEmpty();

	this->elements[firstEmpty] = make_pair(c, v);
	this->next[firstEmpty] = NULL_TVALUE;
	this->next[pos] = firstEmpty;
	this->nrElements++;
}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
vector<TValue> SortedMultiMap::search(TKey c) const {

	int pos = this->hash(c);

	if (this->elements[pos] == NULL_TELEM)
	{
		return vector<TValue>();
	}

	vector<TValue> values;

	while (pos != NULL_TVALUE)
	{
		if (this->elements[pos].first == c)
		{
			values.push_back(this->elements[pos].second);
		}

		pos = this->next[pos];
	}

	return values;
}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
vector<TValue> SortedMultiMap::removeKey(TKey c)
{
	vector<TValue> values = this->search(c);

	for (TValue value : values)
	{
		this->remove(c, value);
	}

	return values;
}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n) // O(1) amortized
bool SortedMultiMap::remove(TKey c, TValue v) {

	int pos = this->hash(c);
	if (this->elements[pos] == NULL_TELEM)
	{
		return false;
	}

	int prev = NULL_TVALUE;
	while (pos != NULL_TVALUE)
	{
		if (this->elements[pos].first == c && this->elements[pos].second == v)
		{
			this->nrElements--;

			if (this->next[pos] == NULL_TVALUE)
			{
				this->elements[pos] = NULL_TELEM;
				this->next[pos] = NULL_TVALUE;
				return true;
			}

			this->elements[pos] = this->elements[this->next[pos]];
			this->elements[this->next[pos]] = NULL_TELEM;
			this->next[pos] = this->next[this->next[pos]];

			if (this->next[pos] != NULL_TVALUE)
			{
				this->next[this->next[pos]] = NULL_TVALUE;
			}
			return true;
		}

		prev = pos;
		pos = this->next[pos];
	}

	return false;
}

// Theta(1)
int SortedMultiMap::size() const {
	return nrElements;
}

// Theta(1)
bool SortedMultiMap::isEmpty() const {
	return this->nrElements == 0;
}

// Theta(1)
SMMIterator SortedMultiMap::iterator() const {
	return SMMIterator(*this);
}

// Theta(1)
SortedMultiMap::~SortedMultiMap() {
	delete[] elements;
	delete[] next;
}
