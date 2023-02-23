#pragma once
#include <iostream>
#include "errors.h"

template <typename Type> class Vector;
template <typename Type> std::ostream& operator<<(std::ostream& os, const Vector<Type>& vector);

template <typename Type>
class Vector
{
    private:
        Type* elements;
        int _size;
        int capacity;

        void resize();

    public:

        /// <summary>
        /// Constructs a Vector and allocates memory for one element
        /// </summary>
        Vector();

        /// <summary>
        /// Copies the given Vector into the current one
        /// </summary>
        /// <param name="v"> A Vector type object that will be copied </param>
        Vector(const Vector<Type>& v);

        /// <summary>
        /// Gets the element found at the specified index
        /// </summary>
        /// <param name="index"> An integer representing the index </param>
        /// <returns> The element found at the specified position </returns>
        Type& operator[](int index);

        /// <summary>
        /// Override of the = operator
        /// </summary>
        /// <param name="v"> A new object that will be assigned to this </param>
        /// <returns> A pointer to this </returns>
        Vector<Type>& operator=(const Vector<Type>& v);

        /// <summary>
        /// Adds an element in the vector
        /// </summary>
        /// <typeparam name="Type"> The element that will be added </typeparam>
        Vector<Type>& operator+(Type elem);

        /// <summary>
        /// Adds an element in the vector
        /// </summary>
        /// <typeparam name="Type"> The element that will be added </typeparam>
        Vector<Type>& operator+=(Type elem);

        /// <summary>
        /// Adds an element into the vector
        /// </summary>
        /// <param name="element"> The element that will be inserted </param>
        void push_back(Type element);

        /// <summary>
        /// Removes an element from the vector from a given position
        /// </summary>
        /// <param name="index"> The position of the element that will be removed </param>
        void remove(int index);

        /// <summary>
        /// Returns the size of this
        /// </summary>
        /// <returns> The size of this </returns>
        int size() const;

        /// <summary>
        /// Destructor
        /// </summary>
        ~Vector();

        /// <summary>
        /// Creates an ostream for the Vector class
        /// </summary>
        /// <param name="os"> An ostream& type object </param>
        /// <param name="vector"> The vector object </param>
        /// <returns> The os parameter </returns>
        friend std::ostream& operator<< <>(std::ostream& os, const Vector<Type>& vector);

        class Iterator
        {
            private:
                Type* ptr;

            public:

                /// <summary>
                /// Constructor for the Iterator class
                /// </summary>
                /// <param name="t"> The address of an element </param>
                Iterator(Type* t);


                /// <summary>
                /// Overload for operator ++ - pre-incrementing
                /// </summary>
                /// <returns> The iterator after incrementing </returns>
                Iterator operator++();

                
                /// <summary>
                /// Overload for operator ++ - post-incrementing
                /// </summary>
                /// <returns> The iterator before incrementing </returns>
                Iterator operator++(int);

                /// <summary>
                /// Gets the value of the iterator
                /// </summary>
                /// <returns> The value of the iterator </returns>
                Type& operator*();
                
                /// <summary>
                /// Checks if 2 operators are not equal
                /// </summary>
                /// <param name="it"> The Iterator to compare with </param>
                /// <returns> True - if the Iterators are not equal, False - otherwise </returns>
                bool operator!=(const Iterator& it);

                /// <summary>
                /// Checks if 2 operators are equal
                /// </summary>
                /// <param name="it"> The Iterator to compare with </param>
                /// <returns> True - if the Iterators are equal, False - otherwise </returns>
                bool operator==(const Iterator& it);
        };

        /// <summary>
        /// Creates an Iterator for this vector
        /// </summary>
        /// <returns> The begining of this vector </returns>
        Iterator begin()
        {
            return Iterator{ this->elements };
        }

        /// <summary>
        /// Creates an Iterator for this vector
        /// </summary>
        /// <returns> The end of this vector </returns>
        Iterator end()
        {
            return Iterator{ this->elements + this->_size };
        }
};


// Vector Implementation

template <typename Type>
inline std::ostream& operator<<(std::ostream& os, const Vector<Type>& vector)
{
    for (int i = 0; i < vector._size; i++)
    {
        os << vector.elements[i] << '\n';
    }
    return os;
}


template<typename Type>
void Vector<Type>::resize()
{
    Type* temp = new Type[this->capacity * 2];

    for (int i = 0; i < this->size(); i++)
    {
        temp[i] = this->elements[i];
    }

    delete[] this->elements;
    this->elements = temp;
    this->capacity *= 2;
}

template<typename Type>
Vector<Type>::Vector()
{
    this->_size = 0;
    this->capacity = 1;
    this->elements = new Type[this->capacity];
}

template<typename Type>
Vector<Type>::Vector(const Vector<Type>& v)
{
    this->_size = v._size;
    this->capacity = v.capacity;
    this->elements = new Type[this->capacity];

    for (int i = 0; i < v._size; i++)
    {
        this->elements[i] = v.elements[i];
    }
}

template<typename Type>
Type& Vector<Type>::operator[](int index)
{
    return this->elements[index];
}


template<typename Type>
Vector<Type>& Vector<Type>::operator=(const Vector<Type>& v)
{
    if (this == &v)
    {
        return *this;
    }

    this->_size = v._size;
    this->capacity = v.capacity;

    Type* aux = new Type[this->capacity];
    delete[] this->elements;
    this->elements = aux;
    
    for (int i = 0; i < this->_size; i++)
    {
        this->elements[i] = v.elements[i];
    }

    return *this;
}



template<typename Type>
inline Vector<Type>& Vector<Type>::operator+(Type elem)
{
    this->push_back(elem);
    return *this;
}

template<typename Type>
inline Vector<Type>& Vector<Type>::operator+=(Type elem)
{
    this->push_back(elem);
    return *this;
}


template<typename Type>
void Vector<Type>::push_back(Type element)
{
    if (this->_size >= this->capacity)
    {
        this->resize();
    }

    this->elements[this->_size++] = element;
}

template<typename Type>
void Vector<Type>::remove(int index)
{
    if (index < 0 || index >= this->size())
    {
        throw Errors().IndexOutOfBounds;
    }

    for (int i = index; i < this->size() - 1; i++)
    {
        this->elements[i] = this->elements[i + 1];
    }

    this->_size--;
}

template<typename Type>
int Vector<Type>::size() const
{
    return this->_size;
}

template<typename Type>
Vector<Type>::~Vector()
{
    delete[] this->elements;
}


// Vector Iterator implementation


template<typename Type>
Vector<Type>::Iterator::Iterator(Type* ptr)
{
    this->ptr = ptr;
}


template<typename Type>
typename Vector<Type>::Iterator Vector<Type>::Iterator::operator++()
{
    this->ptr++;
    return this->ptr;
}

template<typename Type>
typename Vector<Type>::Iterator Vector<Type>::Iterator::operator++(int)
{
    Type* tempPtr = this->ptr;
    this->ptr++;
    return tempPtr;
}

template<typename Type>
typename  Type& Vector<Type>::Iterator::operator*()
{
    return *this->ptr;
}

template<typename Type>
typename bool Vector<Type>::Iterator::operator!=(const Vector<Type>::Iterator& it)
{
    return this->ptr != it.ptr;
}

template<typename Type>
typename bool Vector<Type>::Iterator::operator==(const Vector<Type>::Iterator& it)
{
    return this->ptr == it.ptr;
}