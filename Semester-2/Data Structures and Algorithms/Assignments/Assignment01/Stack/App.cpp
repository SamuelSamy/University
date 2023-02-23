#include "Stack.h"
#include "ShortTest.h"
#include "ExtendedTest.h"
#include <iostream>

#include <crtdbg.h>

using namespace std;

int main() {

	testAll();
	testAllExtended();
	
	cout << "That's all" << endl;
	
	_CrtDumpMemoryLeaks();
	
	return 0;
}