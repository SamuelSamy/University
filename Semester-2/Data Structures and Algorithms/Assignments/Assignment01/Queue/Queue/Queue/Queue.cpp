#include "Queue.h"
#include <exception>
#include <iostream>

using namespace std;

#define INITIAL_SIZE 2

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
Queue::Queue() {
	this->front = this->back = -1;
	this->size = INITIAL_SIZE;
	this->noOfElements = 0;
	this->elements = new TElem[this->size];
}

// Best case: Theta(1)
// Worst case: Theta(n) (when resizing)
// Total complexity: O(n)
void Queue::push(TElem elem) {
	
	if (this->noOfElements >= this->size)
	{
		TElem* newElements = new TElem[this->size * 2];

		int j = 0;

		for (int i = this->front; i < this->size; i++)
		{
			newElements[j++] = this->elements[i];
		}

		for (int i = 0; i < this->front; i++)
		{
			newElements[j++] = this->elements[i];
		}

		this->front = 0;
		this->back = j - 1;
		delete[] this->elements;
		this->elements = newElements;
		this->size *= 2;
	}

	if (this->front == -1)
	{
		this->front = this->back = 0;
		elements[this->back] = elem;
	}
	else if (this->front > 0 && this->back == this->size - 1)
	{
		this->back = 0;
		elements[this->back] = elem;
	}
	else
	{
		this->elements[++this->back] = elem;
	}

	this->noOfElements++;
}


// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
TElem Queue::top() const {

	if (this->front == -1)
	{
		throw exception();
	}

	return this->elements[this->front];
}

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
TElem Queue::pop() {
	
	if (this->front == -1)
	{
		throw exception();
	}

	TElem elem = this->elements[this->front];
	
	if (this->front == this->back)
	{
		this->front = this->back = -1;
	}
	else if (this->front == this->size - 1)
	{
		this->front = 0;
	}
	else
	{
		this->front++;
	}

	this->noOfElements--;
	return elem;
}

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
bool Queue::isEmpty() const {
	return (this->front == -1 && this->back == -1);
}

// Best case: Theta(1)
// Worst case: Theta(1)
// Total complexity: Theta(1)
Queue::~Queue() {
	delete[] this->elements;
}

