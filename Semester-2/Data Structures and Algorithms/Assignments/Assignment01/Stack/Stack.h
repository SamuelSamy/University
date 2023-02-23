#pragma once
//DO NOT CHANGE THIS PART
typedef int TElem;
#define NULL_TELEM -111111

class Stack
{
	private:
		int topIndex;
		TElem* elements;
		int size;


	public:

	//constructor
	Stack();

	//pushes an element to the top of the Stack
	void push(TElem e);

	//returns the element from the top of the Stack
	//throws exception if the stack is empty
	TElem top() const;

	//removes and returns the element from the top of the Stack
	//throws exception if the stack is empty
	TElem pop();

	//returns and removes one occurrence of the maximum element from the Stack
	TElem popMaximum();

	//checks if the stack is empty
	bool isEmpty() const;

	//destructor
	~Stack();
};