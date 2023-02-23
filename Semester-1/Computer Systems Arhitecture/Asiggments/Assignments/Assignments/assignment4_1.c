 //1. Multiply 2 matrices of integers.

#pragma warning(disable:4996)

#include <stdio.h>
#include <stdlib.h>


int main()
{
    int r1 = 0, c1 = 0, r2 = 0, c2 = 0;

    // Create and read the first matrix
    printf("Enter r1: ");
    scanf("%d", &r1);
    printf("Enter c1: ");
    scanf("%d", &c1);

    if (r1 < 1 || c1 < 1)
    {
        printf("Matrix size must be positive");
        return -1;
    }

    int** mat1 = (int **)malloc(r1 * sizeof(int*));

    if (NULL == mat1)
    {
        perror("Error while allocating memory");
        return -1;
    }

    for (int i = 0; i < r1; i++)    
    {
        mat1[i] = (int*)malloc(c1 * sizeof(int));
    }

    printf("Enter the first matrix:\n");
    
    for (int i = 0; i < r1; i++)
    {
        for (int j = 0; j < c1; j++)
        {
            scanf("%d", &mat1[i][j]);
        }
    }


    // Create and read the second matrix
    printf("Enter r2: ");
    scanf("%d", &r2);
    printf("Enter c2: ");
    scanf("%d", &c2);

    if (r2 < 1 || c2 < 1)
    {
        perror("Matrix size must be positive");
        return -1;
    }

    int** mat2 = (int**)malloc(r2 * sizeof(int*));

    if (NULL == mat2)
    {
        perror("Error while allocating memory");
        return -1;
    }

    for (int i = 0; i < r2; i++)
    {
        mat2[i] = (int*)malloc(c2 * sizeof(int));
    }

    printf("Enter the second matrix:\n");
    for (int i = 0; i < r2; i++)
    {
        for (int j = 0; j < c2; j++)
        {
            scanf("%d", &mat2[i][j]);
        }
    }


    // Create the result matrix
    int** result = (int**)malloc(r1 * sizeof(int*));

    if (NULL == result)
    {
        perror("Error while allocating memory");
        return -1;
    }

    for (int i = 0; i < r1; i++)    
    {
        result[i] = (int*)malloc(c2 * sizeof(int));
    }

    // Multiply 
    for (int i = 0; i < r1; i++)
    {
        for (int j = 0; j < c2; j++)
        {
            result[i][j] = 0;

            for (int k = 0; k < r2; k++)
            {
                result[i][j] +=  mat1[i][k] * mat2[k][j];
            }
            printf("%d ", result[i][j]);
        }
        printf("\n");
    }


    // Free the memory

    for (int i = 0; i < r1; i++)
    {
        free(result[i]);
        free(mat1[i]); 
    }

    for (int i = 0; i < r2; i++)
    {
        free(mat2[i]);
    }

    free(result);
    free(mat1);
    free(mat2);


    return 0;
}