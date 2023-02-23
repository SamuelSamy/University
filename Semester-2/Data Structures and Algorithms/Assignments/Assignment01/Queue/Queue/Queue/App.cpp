#include "Queue.h"
#include "ExtendedTest.h"
#include "ShortTest.h"
#include "CustomTest.h"

#include <iostream>

#include <crtdbg.h>

using namespace std;

int main() {

	testCustom();
	testAll();
	testAllExtended();

	cout << "Test end" << endl;

	_CrtDumpMemoryLeaks();

	return 0;
}