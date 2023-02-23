 //1. Write a program that reads 2 arrays and merges the two arrays into third array. Before merging, sorte the 2 arrays in ascending order.

#pragma warning(disable:4996)

#include <stdio.h>

//void Swap(int* Element1, int* Element2)
//{
//    int temp = *Element2;
//    *Element2 = *Element1;
//    *Element1 = temp;
//}

// without pointers

//int main()
//{
//    int a1[100], a2[100], res[200];
//    int n1, n2;
//
//    printf("n1 = ");
//    scanf("%d", &n1);
//    printf("Enter the array: ");
//    for (int i = 0; i < n1; i++)
//    {
//        scanf("%d", &a1[i]);
//    }
//
//    printf("n2 = ");
//    scanf("%d", &n2);
//    printf("Enter the array: ");
//    for (int i = 0; i < n2; i++)
//    {
//        scanf("%d", &a2[i]);
//    }
//
//
//    for (int i = 0; i < n1; i++)
//    {
//        for (int j = i + 1; j < n1; j++)
//        {
//            if (a1[i] > a1[j])
//            {
//                Swap(&a1[i], &a1[j]);
//            }
//        }
//    }
//
//    for (int i = 0; i < n2; i++)
//    {
//        for (int j = i + 1; j < n2; j++)
//        {
//            if (a2[i] > a2[j])
//            {
//                Swap(&a2[i], &a2[j]);
//            }
//        }
//    }
//
//    int i1 = 0, i2 = 0, iRes = 0;
//    
//    while (i1 < n1 && i2 < n2)
//    {
//        if (a1[i1] < a2[i2])
//        {
//            res[iRes] = a1[i1];
//            i1++;
//            iRes++;
//        }
//        else
//        {
//            res[iRes] = a2[i2];
//            i2++;
//            iRes++;
//        }
//    }
//
//    while (i1 < n1)
//    {
//        res[iRes] = a1[i1];
//        i1++;
//        iRes++;
//    }
//
//    while (i2 < n2)
//    {
//        res[iRes] = a2[i2];
//        i2++;
//        iRes++;
//    }
//
//    for (int i = 0; i < iRes; i++)
//    {
//        printf("%d ", res[i]);
//    }
//
//    return 0;
//}


// with pointers 

int ReadArray(int n, int* v)
{
    if (NULL == v)
    {
        return -1;
    }

    printf("Enter the array: ");
    for (int i = 0; i < n; i++)
    {
        scanf("%d", (v + i));
    }

    return 0;
}


int SortArray(int n, int* v)
{
    if (NULL == v)
    {
        return -1;
    }

    for (int i = 0; i < n; i++)
    {
        for (int j = i + 1; j < n; j++)
        {
            if (*(v + i) > *(v + j))
            {
                Swap((v + i), (v + j));
            }
        }
    }

    return 0;
}

int Merge(int n1, int n2, int* v1, int* v2, int* res, int* iRes)
{
    if (NULL == v1 || NULL == v2 || NULL == res)
    {
        return -1;
    }

    int i1 = 0, i2 = 0;
    *iRes = 0;

    while (i1 < n1 && i2 < n2)
    {
        if (v1[i1] < v2[i2])
        {
            res[(*iRes)] = v1[i1];
            i1++;
            (*iRes)++;
        }
        else
        {
            res[(*iRes)] = v2[i2];
            i2++;
            (*iRes)++;
        }
    }

    while (i1 < n1)
    {
        res[(*iRes)] = v1[i1];
        i1++;
        (*iRes)++;
    }

    while (i2 < n2)
    {
        res[(*iRes)] = v2[i2];
        i2++;
        (*iRes)++;
    }

    return 0;
}
/*
int main()
{
    int a1[100], a2[100], res[200];
    int n1, n2, n3;

    printf("n1 = ");
    scanf("%d", &n1);
    ReadArray(n1, &a1);

    printf("n2 = ");
    scanf("%d", &n2);
    ReadArray(n2, &a2);

    SortArray(n1, &a1);
    SortArray(n2, &a2);

    Merge(n1, n2, a1, a2, &res, &n3);

    for (int i = 0; i < n3; i++)
    {
        printf("%d ", res[i]);
    }

    return 0;
}*/