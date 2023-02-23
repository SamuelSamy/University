#include "Matrix.h"
#include <exception>
#include <cmath>
#include <iostream>

using namespace std;

// Theta(1)
bool isNull(Node node)
{
	return node.value == NULL_TELEM;
}

// Theta(1)
bool hasNoChildren(Node node)
{
	return node.leftChildIndex == node.rightChildIndex && node.leftChildIndex == -1;
}

// Theta(1)
bool relation(int i0, int j0, int i1, int j1)
{
	return i0 < i1 || i0 == i1 && j0 < j1;
}

// Theta(1)
bool Matrix::validIndex(int i, int j) const
{
	return i >= 0 && j >= 0 && i < this->noLines&& j < this->noCols;
}

// Theta(1)
Matrix::Matrix(int nrLines, int nrCols) {
	this->noLines = nrLines;
	this->noCols = nrCols;

	this->capacity = 3;
	this->size = 0;
	this->rootIndex = 0;

	this->nodes = new Node[this->capacity];

	for (int i = 0; i < this->capacity; i++)
	{
		this->nodes[i] = NULL_TNODE;
	}
}

// Theta(1)
int Matrix::nrLines() const {
	return this->noLines;
}

// Theta(1)
int Matrix::nrColumns() const {
	return this->noCols;
}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
TElem Matrix::element(int i, int j) const {
	
	if (!validIndex(i, j))
	{
		throw exception();
	}

	// search for the element in bst
	int index = this->rootIndex;
	Node node = this->nodes[rootIndex];

	while (!isNull(node))
	{
		if (node.row == i && node.col == j)
		{
			break;
		}

		if (relation(i, j, node.row, node.col))
		{
			index = node.leftChildIndex;
		}
		else
		{
			index = node.rightChildIndex;
		}

		if (index == -1)
		{
			return NULL_TELEM;
		}

		node = this->nodes[index];
	}

	if (isNull(node))
	{
		return NULL_TELEM;
	}

	return node.value;
}

// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
TElem Matrix::modify(int i, int j, TElem e) {

	if (!validIndex(i, j))
	{
		throw exception();
	} 
	int index = rootIndex;
	int prevIndex = -1;
	Node node = this->nodes[this->rootIndex];

	while (!isNull(node))
	{
		if (node.row == i && node.col == j)
		{
			break;
		}

		prevIndex = index;

		if (relation(i, j, node.row, node.col))
		{
			index = node.leftChildIndex;
		}
		else
		{
			index = node.rightChildIndex;
		}

		if (index == -1)
		{
			break;
		}

		node = this->nodes[index];
	}

	TElem value = node.value;

	if (e == NULL_TELEM )
	{
		if (index != -1)
		{
			// remove from bst
			this->remove(prevIndex, index);
		}
		
		return value;
	}

	if (isNull(node) || index == -1)
	{
		// insert into bst
		this->insert(prevIndex, i, j, e);
		return value;
	}

	// modify the value in bst
	this->nodes[index].value = e;
	return value;
}

int Matrix::numberOfNonZeroElemsRec(int index) const
{
	if (isNull(this->nodes[index]))
	{
		return 0;
	}

	if (this->nodes[index].leftChildIndex != -1)
	{
		return 1 + numberOfNonZeroElemsRec(this->nodes[index].leftChildIndex);
	}

	
	if (this->nodes[index].rightChildIndex != -1)
	{
		return 1 + numberOfNonZeroElemsRec(this->nodes[index].rightChildIndex);
	}

	return 0;
}

// Theta(n)
int Matrix::numberOfNonZeroElems() const
{
	return numberOfNonZeroElemsRec(0);
}


// Best: Theta(1)
// Worst: Theta(n)
// Total: O(n)
void Matrix::insert(int parentIndex, int i, int j, TElem e)
{
	if (this->size >= this->capacity)
	{
		Node* temp = this->nodes;
		int newCapacity = this->size * 2;

		this->nodes = new Node[newCapacity];

		for (int k = 0; k < this->capacity; k++)
		{
			this->nodes[k] = temp[k];
		}

		for (int k = this->capacity; k < newCapacity; k++)
		{
			this->nodes[k] = NULL_TNODE;
		}

		this->capacity = newCapacity;
		delete[] temp;
	}

	this->size++;

	int index = this->rootIndex;
	if (parentIndex == -1)
	{
		// there are no nodes in the tree
		this->nodes[index].row = i;
		this->nodes[index].col = j;
		this->nodes[index].value = e;
		return;
	}

	index = getFirstFree();
	Node node = this->nodes[parentIndex];
	if (relation(i, j, node.row, node.col))
	{
		this->nodes[parentIndex].leftChildIndex = index;
	}
	else
	{
		this->nodes[parentIndex].rightChildIndex = index;
	}

	this->nodes[index].row = i;
	this->nodes[index].col = j;
	this->nodes[index].value = e;
	this->nodes[index].parentIndex = parentIndex;
}

void Matrix::remove(int parentIndex, int index)
{
	Node& currentNode = this->nodes[index];

	// has no children
	if ((currentNode.leftChildIndex == -1) + (currentNode.rightChildIndex == -1) == 2)
	{
		// the current node is the root
		if (parentIndex == -1)
		{
			this->rootIndex = 0;
			currentNode.parentIndex = -1;
			currentNode.value = 0;
			return;
		}

		Node& parentNode = this->nodes[parentIndex];
		if (parentNode.leftChildIndex == index)
		{
			parentNode.leftChildIndex = -1;
		}
		else
		{
			parentNode.rightChildIndex = -1;
		}

		currentNode.value = 0;
		return;
	}

	// has one children
	if ((currentNode.leftChildIndex == -1) + (currentNode.rightChildIndex == -1) == 1)
	{
		// has left child
		bool left = true;
		int childIndex = currentNode.leftChildIndex;

		if (this->nodes[parentIndex].rightChildIndex == index)
		{
			left = false;
		}

		// has right child
		if (currentNode.rightChildIndex != -1)
		{
			childIndex = currentNode.rightChildIndex;
		}

		if (parentIndex == -1)
		{
			this->nodes[index].value = NULL_TELEM;
			this->nodes[childIndex].parentIndex = -1;
			this->rootIndex = childIndex;
			return;
		}

		Node& parentNode = this->nodes[parentIndex];
		Node& childNode = this->nodes[childIndex];

		childNode.parentIndex = parentIndex;
		if (left)
		{
			this->nodes[parentIndex].leftChildIndex = childIndex;
		}
		else
		{
			this->nodes[parentIndex].rightChildIndex = childIndex;
		}
		currentNode.value = 0;
		return;
	}


	// has two children  -  get left most leaf in the right sub-tree
	int initialIndex = index;
	index = this->nodes[index].rightChildIndex;
	while (
		this->nodes[index].value != NULL_TELEM &&
		this->nodes[this->nodes[index].leftChildIndex].value != NULL_TELEM &&
		this->nodes[index].leftChildIndex != -1
		)
	{
		index = this->nodes[index].leftChildIndex;
	}

	if (parentIndex == -1)
	{
		this->nodes[index].rightChildIndex = this->nodes[rootIndex].rightChildIndex;

		if (index != this->nodes[rootIndex].leftChildIndex)
		{
			this->nodes[index].leftChildIndex = this->nodes[rootIndex].leftChildIndex;
		}
		
		this->nodes[this->nodes[index].rightChildIndex].parentIndex = index;
		this->nodes[this->nodes[index].leftChildIndex].parentIndex = index;
		this->nodes[this->nodes[index].parentIndex].leftChildIndex = -1;
		this->nodes[index].parentIndex = -1;

		
		this->rootIndex = index;
		currentNode.value = NULL_TELEM;
		return;
	}

	this->nodes[index].parentIndex = parentIndex;
	this->nodes[index].leftChildIndex = this->nodes[initialIndex].leftChildIndex;
	this->nodes[index].rightChildIndex = this->nodes[initialIndex].rightChildIndex;
	this->nodes[this->nodes[index].rightChildIndex].parentIndex = index;
	this->nodes[this->nodes[index].leftChildIndex].parentIndex = index;

	this->nodes[this->nodes[index].parentIndex].leftChildIndex = -1;

	if (this->nodes[parentIndex].leftChildIndex == initialIndex)
	{
		this->nodes[parentIndex].leftChildIndex = index;
	}
	else
	{
		this->nodes[parentIndex].rightChildIndex = index;
	}

	currentNode.value = NULL_TELEM;
	return;
}

int Matrix::getFirstFree()
{
	for (int i = 0; i < this->size; i++)
	{
		if (this->nodes[i].value == NULL_TELEM)
		{
			return i;
		}
	}

	return -1;
}
