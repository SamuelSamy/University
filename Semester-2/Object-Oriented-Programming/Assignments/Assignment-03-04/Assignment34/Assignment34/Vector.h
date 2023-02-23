#pragma once

typedef void* TElem;

typedef void (*destroyFunc)(void*);

typedef struct _Vector {
	TElem* elements;
	int size;
	int capacity;
	destroyFunc destroy;
} Vector;

/// <summary>
/// Creats a Vector*
/// </summary>
/// <param name="capacity"> The initial capacity of the vector </param>
/// <param name="destroy"> A pointer to the destroy function of the elements that will be stored in the vector </param>
/// <returns> A Vector* </returns>
Vector* createVector(int capacity, destroyFunc destroy);

/// <summary>
/// Destorys the given vector 
/// </summary>
/// <param name="vector"> A Vector* </param>
void destroyVector(Vector* vector);

/// <summary>
/// Appends the given element to the vector
/// </summary>
/// <param name="vector"> A Vector* </param>
/// <param name="element"> The address of the element that will be inserted </param>
/// <returns>
///		MemoryIssue - if any of the parameters were NULL;
///		Success - if the element was successfully added
/// </returns>
int vectorInsertTail(Vector* vector, TElem element);

/// <summary>
/// Inserts at index = 0 the given element in the vector
/// </summary>
/// <param name="vector"> A Vector* </param>
/// <param name="element"> The address of the element that will be inserted </param>
/// <returns>
///		MemoryIssue - if any of the parameters were NULL;
///		Success - if the element was successfully added
/// </returns>
//int vectorInsertHead(Vector* vector, TElem element);

/// <summary>
///  Inserts at the specified index the given element in the vector
/// </summary>
/// <param name="vector"> A Vector* </param>
/// <param name="index"> An integer representing an index </param>
/// <param name="element"> The address of the element that will be inserted </param>
/// <returns>
///		MemoryIssue - if any of the parameters were NULL;
///		Success - if the element was successfully added;
///		IndexNotFound - if the index is < 0 or > vectorLen
/// </returns>
int vectorInsetAtIndex(Vector* vector, int index, TElem element);

/// <summary>
/// Gets the element from a specific index
/// </summary>
/// <param name="vector"> A Vector* </param>
/// <param name="index"> An integer representing an index </param>
/// <param name="value"> A pointer to the element that the value will be stored in </param>
/// <returns>
///		MemoryIssue - if any of the parameters were NULL;
///		Success - if the element was successfully added;
///		IndexNotFound - if the index is < 0 or > vectorLen	
/// </returns>
int vectorGetValueByIndex(Vector* vector, int index, TElem* value);

/// <summary>
/// Removes an element from the given index
/// </summary>
/// <param name="vector"> A Vector*</param>
/// <param name="index"> An integer representing an index </param>
/// <returns>
///		MemoryIssue - if any of the parameters were NULL;
///		Success - if the element was successfully added;
///		IndexNotFound - if the index is < 0 or > vectorLen
/// </returns>
int vectorRemoveByIndex(Vector* vector, int index);

/// <summary>
/// Gets the size of the vector
/// </summary>
/// <param name="vector"> A Vector* </param>
/// <returns>
///		-1 - if there were any errors, a positive integer representing the length otherwise
/// </returns>
int vectorGetSize(Vector* vector);

/// <summary>
/// Remvoes all the elements from the given Vector
/// </summary>
/// <param name="vector"> A Vector* </param>
/// <returns>
///		MemoryIssue - if any of the parameters were NULL;
///		Success - if the element was successfully added;
/// </returns>
int vectorClear(Vector* vector);

/// <summary>
/// Swaps 2 elements in the given vector
/// </summary>
/// <param name="vector"> A Vector* </param>
/// <param name="index0"> A Vector* </param>
/// <param name="index1"> A Vector* </param>
void vectorSwap(Vector* vector, int index0, int index1);