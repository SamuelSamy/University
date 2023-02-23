#include <assert.h>
#include "Matrix.h"

using namespace std;

void testCustom()
{
	Matrix m(5, 5);

	m.modify(0, 0, 2);
	m.modify(0, 1, 5);
	m.modify(1, 0, 3);
	m.modify(1, 1, 7);
	m.modify(2, 0, 8);
	m.modify(2, 1, 10);

	assert(m.element(0, 0) == 2);
	m.modify(1, 0, 0);
	m.modify(2, 0, 0);
	m.modify(2, 1, 0);


}

void testAll() { 
	Matrix m(4, 4);
	assert(m.nrLines() == 4);
	assert(m.nrColumns() == 4);	
	m.modify(1, 1, 5);
	assert(m.element(1, 1) == 5);
	TElem old = m.modify(1, 1, 6);
	assert(m.element(1, 2) == NULL_TELEM);
	assert(old == 5);

	testCustom();
}

