//// Write a function that receives as parameters 2 memory addresses where are stored values for 2 integer variables and replaces the value from the
//// first address with the product of the 2 initial values and also replaces the value from the second address with the sum of the 2 initial values.
//
//#pragma warning(disable:4996)
//
//#include <stdio.h>
//
//int CustomReplace(int* a, int* b)
//{
//    if (NULL == a || NULL == b)
//    {
//        return -1;
//    }
//
//    int initial_a = *a;
//    int initial_b = *b;
//
//    *a = initial_a * initial_b;
//    *b = initial_a + initial_b;
//
//    return 0;
//}
//
//int main()
//{
//    int a = 5, b = 6;
//    CustomReplace(&a, &b);
//    printf("%d %d", a, b);
//
//}