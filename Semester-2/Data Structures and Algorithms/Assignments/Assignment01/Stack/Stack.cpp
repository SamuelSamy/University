#include "Stack.h"
#include <exception>
#include <iostream>

#define INITIAL_SIZE 2

using namespace std;


// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1) 
Stack::Stack() {
	this->topIndex = 0;
	this->elements = new TElem[INITIAL_SIZE];
	this->size = INITIAL_SIZE;
}

// Best case: Theta(1)
// Worst case: Theta(n) (when resizing)
// Total complexity: O(n)
void Stack::push(TElem e) {
	
	if (this->topIndex >= this->size)
	{
		TElem* newElements = new TElem[this->size * 2];

		for (int i = 0; i < this->topIndex; i++)
		{
			newElements[i] = this->elements[i];
		}

		delete[] this->elements;
		this->elements = newElements;
		this->size *= 2;
	}

	this->elements[this->topIndex++] = e;
}

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
TElem Stack::top() const {
	
	if (this->topIndex <= 0)
	{
		throw exception();
	}

	return this->elements[this->topIndex - 1];
}

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
TElem Stack::pop() {
	if (this->topIndex <= 0)
	{
		throw exception();
	}

	return this->elements[--this->topIndex];
}

// Best case: Theta(n)
// Worst case: Theta(n)
// Total complexity: Theta(n)
TElem Stack::popMaximum()
{
	if (this->topIndex <= 0)
	{
		throw exception();
	}

	TElem element = -INT_MAX;
	int pos = -1;

	for (int i = 0; i < this->topIndex; i++)
	{
		if (element < this->elements[i])
		{
			element = this->elements[i];
			pos = i;
		}
	}

	for (int i = pos; i < this->topIndex; i++)
	{
		this->elements[i] = this->elements[i + 1];
	}

	this->topIndex--;
	return element;
}

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
bool Stack::isEmpty() const {
	return this->topIndex <= 0;
}

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
Stack::~Stack() {
	delete[] this->elements;
}