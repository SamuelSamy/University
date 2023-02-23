#include "CustomTest.h"
#include "Queue.h"
#include <assert.h>
#include <iostream>

void testCustom()
{
	Queue queue;

	queue.push(1);
	queue.push(2);
	queue.push(3);
	queue.push(4);

	queue.pop();
	queue.pop();
	queue.pop();

	queue.push(5);
	queue.push(6);
	queue.push(7);

	queue.push(10);
}