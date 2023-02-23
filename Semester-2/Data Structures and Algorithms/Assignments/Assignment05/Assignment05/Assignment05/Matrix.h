#pragma once

//DO NOT CHANGE THIS PART
typedef int TElem;
#define NULL_TELEM 0


typedef struct _Node {
	int row;
	int col;
	TElem value;
	
	int leftChildIndex;
	int rightChildIndex;
	int parentIndex;


	_Node(int i, int j, TElem e, int left, int right, int parent)
	{
		this->row = i;
		this->col = j;
		this->value = e;
		this->leftChildIndex = left;
		this->rightChildIndex = right;
		this->parentIndex = parent;
	}

	_Node() = default;
} Node;


#define NULL_TNODE Node(-1, -1, 0, -1, -1, -1)


class Matrix {

private:
	Node* nodes;

	int size;
	int capacity;

	int noLines;
	int noCols;
	
	int nonZero;

	bool validIndex(int i, int j) const;
	void insert(int parentIndex, int i, int j, TElem value);
	int numberOfNonZeroElemsRec(int index) const;

public:
	//constructor
	Matrix(int nrLines, int nrCols);

	//returns the number of lines
	int nrLines() const;

	//returns the number of columns
	int nrColumns() const;

	//returns the element from line i and column j (indexing starts from 0)
	//throws exception if (i,j) is not a valid position in the Matrix
	TElem element(int i, int j) const;

	//modifies the value from line i and column j
	//returns the previous value from the position
	//throws exception if (i,j) is not a valid position in the Matrix
	TElem modify(int i, int j, TElem e);

	int numberOfNonZeroElems() const;
};
