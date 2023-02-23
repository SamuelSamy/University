#include "Matrix.h"
#include <exception>
#include <cmath>
#include <iostream>

using namespace std;

// Theta(1)
bool isNull(Node node)
{
	return node.col == node.row && node.col == -1;
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

	this->nodes = new Node[this->capacity];
	this->nonZero = 0;

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
	int index = 0;
	Node node = this->nodes[0];

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

	int index = 0;
	int prevIndex = -1;
	Node node = this->nodes[0];

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

	if (isNull(node) || index == -1)
	{
		// insert into bst
		this->insert(prevIndex, i, j, e);
		return value;
	}

	// modify the value in bst
	if (e == NULL_TELEM)
	{
		this->nonZero--;
	}

	this->nodes[index].value = e;
	return value;
}


// Theta(n)
int Matrix::numberOfNonZeroElemsRec(int index) const
{
	if (isNull(this->nodes[index]))
	{
		return 0;
	}

	if (this->nodes[index].leftChildIndex != -1)
	{
		return (this->nodes[this->nodes[index].leftChildIndex].value != 0) + numberOfNonZeroElemsRec(this->nodes[index].leftChildIndex);
	}

	if (this->nodes[index].rightChildIndex != -1)
	{
		return (this->nodes[this->nodes[index].rightChildIndex].value != 0) + numberOfNonZeroElemsRec(this->nodes[index].rightChildIndex);
	}

	return this->nodes[index].value != 0;
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
	if (e != NULL_TELEM)
	{
		this->nonZero++;
	}

	if (this->size >= this->capacity - 1)
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

	int index = 0;
	if (parentIndex == -1)
	{
		// there are no nodes in the tree
		this->nodes[index].row = i;
		this->nodes[index].col = j;
		this->nodes[index].value = e;
		return;
	}

	index = this->size;
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

