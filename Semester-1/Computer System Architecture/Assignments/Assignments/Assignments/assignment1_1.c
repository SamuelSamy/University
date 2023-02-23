// 1. Write a program to read a number of units of length (a float) and print out the area of a circle with that radius. Assume that the value of pi is 3.14159. After that, change the type to double and compare the results.

#pragma warning(disable:4996)

#include <stdio.h>

#define PI 3.14159
/*
int main()
{
    float length;
    printf("Enter length: ");
    scanf("%f", &length);
    
    float area = PI * length * length;
    printf("Float area: %f\n", area);
    double doubleArea = (double)area;
    printf("Dobule area: %f\n", doubleArea);

    if (doubleArea == area)
    {
        printf("The areas are equal");
    }
    else if (doubleArea > area)
    {
        printf("The double area is greater");
    }
    else
    {
        printf("The flaot area is greater");
    }

    return 0;
}*/