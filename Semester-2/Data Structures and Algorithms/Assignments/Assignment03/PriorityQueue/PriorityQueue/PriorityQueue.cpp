
#include "PriorityQueue.h"
#include <exception>
using namespace std;

#include <iostream>

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
int PriorityQueue::initNode()
{
	if (this->size == this->capacity - 1)
	{
		Node* temp = new Node[this->capacity * 2];
		for (int i = 0; i < this->capacity; i++)
		{
			temp[i] = this->elements[i];
		}

		int newcapacity = this->capacity * 2;
		for (int i = this->capacity - 1; i < newcapacity - 1; i++)
		{
			temp[i].next = i + 1;
		}
		
		this->firstFree = this->capacity - 1;
		this->capacity = newcapacity;
		delete[] this->elements;
		this->elements = temp;
	}

	int index = this->firstFree;
	this->firstFree = this->elements[firstFree].next;
	return index;
}

// Theta(1)
void PriorityQueue::destroyNode(int index)
{
	this->elements[index].next = firstFree;
	firstFree = index;
}

// Total: Theta(n)
PriorityQueue::PriorityQueue(Relation r) {

	this->relation = r;

	this->size = 0;
	this->capacity = 2;
	this->head = -1;
	this->tail = -1;
	this->firstFree = 0;

	this->elements = new Node[this->capacity];

	for (int i = 0; i < this->capacity - 1; i++)
	{
		this->elements[i].next = i + 1;
	}
	this->elements[this->capacity - 1].next = -1;

}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
void PriorityQueue::push(TElem e, TPriority p) {
	
	int newIndex = initNode();

	this->elements[newIndex].value = e;
	this->elements[newIndex].priority = p;
	
	this->size++;

	if (this->head == -1)
	{
		this->head = newIndex;
		this->tail = newIndex;
		this->elements[this->tail].next = this->firstFree;
		return;
	}

	int prevIndex = -1;
	int currentIndex = this->head;
	
	while (currentIndex != newIndex && !this->relation(p, this->elements[currentIndex].priority))
	{
		prevIndex = currentIndex;
		currentIndex = this->elements[currentIndex].next;
	}

	if (currentIndex == newIndex)
	{
		// last element
		this->tail = newIndex;
		this->elements[prevIndex].next = newIndex;
	}
	else if (prevIndex == -1)
	{
		// first
		this->head = newIndex;
		this->elements[newIndex].next = currentIndex;
		this->elements[this->tail].next = this->firstFree;
	}
	else
	{
		// middle
		this->elements[prevIndex].next = newIndex;
		this->elements[newIndex].next = currentIndex;
		this->elements[this->tail].next = this->firstFree;
	}

}

// Theta(1)
Element PriorityQueue::top() const {
	
	if (this->head == -1)
	{
		throw exception();
	}

	return Element(this->elements[this->head].value, this->elements[this->head].priority);
}

// Theta(1)
Element PriorityQueue::pop() {

	if (this->head == -1)
	{
		throw exception();
	}
	
	Element element = Element(this->elements[this->head].value, this->elements[this->head].priority);

	int tempFree = firstFree;
	int tempHead = this->head;
	this->head = this->elements[this->head].next;
	destroyNode(tempHead);
	

	if (tempFree == this->head)
	{
		this->head = this->tail = -1;
	}
	else
	{
		this->elements[this->tail].next = this->firstFree;
	}

	this->size--;
	return element;
}

// Theta(1)
bool PriorityQueue::isEmpty() const {
	return this->size == 0;
}

// Theta(1)
PriorityQueue::~PriorityQueue() {
	delete[] elements;
}
;

