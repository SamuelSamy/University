#include <iostream>
#include "PriorityQueue.h"
#include "ExtendedTest.h"
#include "ShortTest.h"

using namespace std;

bool relation(TPriority p1, TPriority p2) {
	return p1 <= p2;
}

// O(n^2)
void printQueue(PriorityQueue& pq)
{
	if (pq.isEmpty())
		return;

	TElem e = pq.top().first;
	TPriority p = pq.top().second;
	pq.pop();
	
	std::cout << e << ' ';
	printQueue(pq);

	pq.push(e, p);
}


int main() 
{
	PriorityQueue pq{ relation };

	for (int i = 0; i < 5; i++)
	{
		pq.push(i, i);
	}

	for (int i = -5; i < 5; i++)
	{
		pq.push(i, i);
	}

	printQueue(pq);
	std::cout << '\n';

	customTests();
	testAll();
	testAllExtended();

	cout << "End" << endl;
	system("pause");
	return 0;
}
