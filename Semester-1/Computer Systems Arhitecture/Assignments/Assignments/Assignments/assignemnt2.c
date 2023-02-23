// 4. Write a program that, given an integer n, computes the first n values from Fibonacci Series.
// Am presupus ca sirul incepe cu 1 2

#pragma warning(disable:4996)

#include <stdio.h>

/// first way, without using any user defined functions

//int main()
//{
//    int n;
//    int v[1000];
//
//    scanf("%d", &n);
//
//    v[0] = 1;
//    v[1] = 2;
//
//    for (int i = 2; i < n; i++)
//    {
//        v[i] = v[i - 1] + v[i - 2];
//    }
//
//    for (int i = 0; i < n; i++)
//    {
//        printf("%d ", v[i]);
//    }
//
//    return 0;
//}


// second way, using your own defined functions

void ReadInput(int* n)
{
    scanf("%d", n);
}

int ComputeArray(int n, int* v)
{
    if (NULL == v)
    {
        return -1;
    }

    *(v) = 1;
    *(v + 1) = 2;

    for (int i = 2; i < n; i++)
    {
        *(v + i) = *(v + i - 1) + *(v + i - 2);
    }

    return 0;
}

void PrintAnswer(int n, int* v)
{
    for (int i = 0; i < n; i++)
    {
        printf("%d ", v[i]);
    }
}
/*
int main()
{
    int n;
    int v[1000];
    
    ReadInput(&n);
    ComputeArray(n, &v);
    PrintAnswer(n, &v);

    return 0;
}*/