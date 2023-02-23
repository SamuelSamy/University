#include "Set.h"
#include "SetITerator.h"

// Best case: Theta(1)
// Worst case: Theta(n)
// Total: O(n)
Node* Set::getLast() const
{
	Node* node = this->head;

	while (node != nullptr && node->next != nullptr)
	{
		node = node->next;
	}

	return node;
}

// Best case: Theta(1)
// Worst case: Theta(n)
// Total: O(n)
Set::Set() {
	this->head = nullptr;
	this->_size = 0;
}

// Best case: Theta(1)
// Worst case: Theta(n)
// Total: O(n)
bool Set::add(TElem elem) {
	
	if (this->search(elem))
	{
		return false;
	}

	Node* last = this->getLast();
	
	Node* newNode = new Node;
	newNode->next = nullptr;
	newNode->prev = last;
	newNode->value = elem;

	if (last == nullptr)
	{
		this->head = newNode;
		this->_size++;
		return true;
	}
	
	last->next = newNode;
	this->_size++;
	return true;
}

// Best case: Theta(1)
// Worst case: Theta(n)
// Total: O(n)
bool Set::remove(TElem elem) {
	
	if (this->head == nullptr)
	{
		return false;
	}

	Node* node = this->head;
	while (node != nullptr && node->value != elem)
	{
		node = node->next;
	}

	if (node == nullptr)
	{
		return false;
	}

	if (this->head == node)
	{
		this->head = node->next;
	}

	if (node->prev != nullptr)
	{
		node->prev->next = node->next;
	}
	

	if (node->next != nullptr)
	{
		node->next->prev = node->prev;
	}

	delete node;
	node = nullptr;
	this->_size--;
	return true;
}

// Best case: Theta(1)
// Worst case: Theta(n)
// Total: O(n)
bool Set::search(TElem elem) const {
	
	Node* node = this->head;

	while (node != nullptr)
	{
		if (node->value == elem)
		{
			return true;
		}

		node = node->next;
	}

	return false;
}

// Theta(1)
int Set::size() const {
	return this->_size;
}

// Theta(1)
bool Set::isEmpty() const {
	return this->head == nullptr;
}

// Best case: Theta(n)
// Worst case: Theta(m * n)
// Total: O(m * n)
// m: the size of the Set s; n: the size of the this set
void Set::_union(const Set& s)
{
	SetIterator it = s.iterator();

	while (it.valid())
	{
		this->add(it.getCurrent());
		it.next();
	}
}

// Best case: Theta(1)
// Worst case: Theta(n)
// Total: O(n)
Set::~Set() {
	
	Node* node = this->head;

	while (node != nullptr)
	{
		Node* temp = node->next;

		delete node;

		node = temp;
	}
}

// Theta(1)
SetIterator Set::iterator() const {
	return SetIterator(*this);
}


