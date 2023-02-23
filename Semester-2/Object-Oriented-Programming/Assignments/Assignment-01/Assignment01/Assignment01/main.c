#define _CRT_SECURE_NO_WARNINGS
//#define _CRTDBG_MAP_ALLOC

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
//#include <crtdbg.h>


/// <summary>
/// Creates an array of integers where array[i] = 1, if `i` is prime
/// </summary>
/// <param name="Array"> An array of integers in which the function will store the result </param>
/// <param name="MaxValue"> The maximum value to which the function will create the sieve </param>
/// <returns> -1 if there were any erros, 0 otherwise </returns>
int CreateSieve(int* Array, int MaxValue)
{
    if (Array == NULL)
    {
        return -1;
    }

    Array[0] = Array[1] = 1;

    int sqrtMax = (int)sqrt(MaxValue) + 1;

    for (int i = 4; i <= MaxValue; i += 2)
    {
        Array[i] = 1;
    }

    for (int i = 3; i <= sqrtMax; i += 2)
    {
        if (Array[i] == 0)
        {
            for (int j = i * i; j <= MaxValue; j += 2 * i)
            {
                Array[j] = 1;
            }
        }
    }
    

    return 0;
}


/// <summary>
/// Creates an array in which we will mark the composed numbers with 1 and searches a number `x` s.t. x is prime and and n - x is prime
/// </summary>
/// <param name="n"> An integer number </param>
/// <param name="Result"> The adress of an integer number </param>
/// <returns>
///     -1, if malloc returned null
///     -2, if there was a problem creating the sieve
///      0, if the function ran successfully
/// </returns>
int SolveFirstProblem(int n, int* Result)
{

    int* primes = (int*)malloc(sizeof(int) * (n + 2));
    if (primes == NULL)
    {
        return -1;
    }

    memset(primes, 0, sizeof(int) * (n + 2));

    int retVal = CreateSieve(primes, n);
    if (retVal == -1)
    {
        return -2;
    }

    if (*(primes + n - 2) == 0)
    {
        *Result = 2;
        free(primes);
        return 0;
    }

    for (int i = 3; i < n; i++)
    {
        if (*(primes + i) == 0 && *(primes + n - i) == 0)
        {
            *Result = i;
            free(primes);
            return 0;
        }
    }

    free(primes);
    return 0;
}


/// <summary>
/// Creats an array with 10 enties (from 0 to 9) and marks array[i] = 1, if the number contains the digit `i`
/// </summary>
/// <param name="Number"> An integer number </param>
/// <returns> An array with 10 entris (from 0 to 9) where array[i] = 1, if and only if the number has the digit `i` </returns>
int* MarkDigits(int Number)
{
    int* digits = (int*)malloc(sizeof(int) * 10);

    if (digits == NULL)
    {
        return NULL;
    }

    memset(digits, 0, sizeof(int) * 10);

    while (Number)
    {
        *(digits + Number % 10) = 1;
        Number /= 10;
    }

    return digits;
}


/// <summary>
/// Checks if 2 numbers have at leat 2 digits in common.
/// The parameters are int arrays with 10 entris (from 0 to 9) where array[i] = 1, if and only if the number contains the digit `i`
/// </summary>
/// <param name="DigitsNumber0"> An array of 10 entries (from 0 to 9) representing the first number's digits.</param>
/// <param name="DigitsNumber1"> An array of 10 entries (from 0 to 9) representing the second number's digits.</param>
/// <returns> 1, if the the numbers have at least 2 digits in common, 0 otherwise </returns>
int HaveAtLeast2DistinctDigitsInCommon(int* DigitsNumber0, int* DigitsNumber1)
{
    int commonDigitsCount = 0;

    for (int i = 0; i < 10; i++)
    {
        if (DigitsNumber0[i] == 1 && DigitsNumber1[i] == 1)
        {
            commonDigitsCount++;
        }
    }

    return commonDigitsCount > 1;
}


/// <summary>
/// For each number in the array creates an array of 10 entries where v[i] = 1 if the number contains the digit i
/// Iterate the array and find the maximum sequence that meets the given condition
/// </summary>
/// <param name="Array"> The array of numbers </param>
/// <param name="Length"> Number of elements in the array </param>
/// <param name="StartIndex"> The address of the startIndex</param>
/// <param name="StopIndex"> The adress of the stopIndex </param>
/// <returns>
///     -404, if the array is null (or the size is 0)
///     -100, if there was a problem while marking the digits of a number
///        0, if there were no errors
/// </returns>
int SolveSecondProblen(int* Array, int Length, int* StartIndex, int* StopIndex)
{
    if (Array == NULL || Length == 0)
    {
        return -404;
    }

    int* previousDigits = MarkDigits(Array[0]);

    if (previousDigits == NULL)
    {
        return -100;
    }

    int* currentDigits = NULL;
    int currentLen = 1, maxLen = 0;
    *StartIndex = 0;
    *StopIndex = 0;

    for (int i = 1; i < Length; i++)
    {
        currentDigits = MarkDigits(Array[i]);
        if (currentDigits == NULL)
        {
            free(previousDigits);
            return -100;
        }

        if (HaveAtLeast2DistinctDigitsInCommon(previousDigits, currentDigits))
        {
            currentLen++;
        }
        else
        {
            if (currentLen > maxLen)
            {
                *StopIndex = i - 1;
                *StartIndex = i - currentLen;
                maxLen = currentLen;
            }

            currentLen = 1;
        }

        memcpy(previousDigits, currentDigits, sizeof(int) * 10);
        free(currentDigits);
    }

    if (currentLen > maxLen)
    {
        *StopIndex = Length - 1;
        *StartIndex = Length - currentLen;
        maxLen = currentLen;
    }

    free(previousDigits);
    return 0;
}


// UI


/// <summary>
/// Decompose a given even natural number, greater than 2, as a sum of two prime numbers
/// </summary>
void FirstProblem()
{
    int n;

    printf("Enter n: ");
    scanf("%d", &n);

    if (n % 2 == 1 || n < 3)
    {
        printf("The number must be an even integer greater than 2\n\n");
        return;
    }

    int result;
    int retVal = SolveFirstProblem(n, &result);

    if (retVal == 0)
    {
        printf("%d = %d + %d\n\n", n, result, n - result);
    }
    else if (retVal == -1)
    {
        printf("Error while creating the array\n\n");
    }
    else if (retVal == -2)
    {
        printf("Error while creating sieve\n\n");
    }
}

/// <summary>
/// Given a vector of numbers, find the longest contiguous subsequence such that any consecutive elements have at least 2 distinct digits in common.
/// </summary>
/// <param name="Array"> The array </param>
/// <param name="Length"> The number of elements in the array </param>
void SecondProblem(int* Array, int Length)
{
    int startIndex, stopIndex;
    

    int retVal = SolveSecondProblen(Array, Length, &startIndex, &stopIndex);

    if (retVal == 0)
    {

        printf("The longest contiguous subsequence such that any consecutive elements have at least 2 distinct digits in common:\nHas length %d\nStarts at index %d\nEnds at index %d\nThe subsequence: ", stopIndex - startIndex + 1, startIndex, stopIndex);

        for (int i = startIndex; i <= stopIndex; i++)
        {
            printf("%d ", Array[i]);
        }

        printf("\n\n");
    }
    else if (retVal == -404)
    {
        printf("Enter a vector first\n\n");
    }
    else if (retVal == -100)
    {
        printf("Error while marking the digits\n\n");
    }

    return;
}

/// <summary>
/// Reads an array from the keyboard
/// </summary>
/// <param name="Numbers"> The adress of the array </param>
/// <param name="Length"> The address of the variabile in which will be stored the number of elements </param>
/// <returns></returns>
int ReadVector(int** Numbers, int* Length)
{
    if (Numbers == NULL)
    {
        return -1;
    }

    if (*Numbers != NULL)
    {
        free(*Numbers);
    }

    int n = 0;
    int size = 64;
    *Length = 0;

    int* numbers = (int*)malloc(sizeof(int) * size);

    if (numbers == NULL)
    {
        printf("Error while allocing memory\n");
        return -1;
    }

    printf("Enter the vector (stop at 0): ");

    while (scanf("%d", &n) && n != 0)
    {
        if ((*Length) >= size)
        {
            int* temp = (int*)malloc(sizeof(int) * size * 2);

            if (temp == NULL)
            {
                printf("Error while reallocing the memory\n");
                return -1;
            }

            memcpy(numbers, temp, size);
            free(temp);

            size *= 2;
        }

        *(numbers + (*Length)++) = n;
    }

    *Numbers = numbers;
    return 0;
}


/// <summary>
/// Prints on the screen an array
/// </summary>
/// <param name="Array"> The array that will be printed </param>
/// <param name="Length"> The number of elements in the array </param>
void PrintVector(int* Array, int Length)
{
    if (Array == NULL)
    {
        printf("Read a vector first!\n");
        return;
    }

    printf("The last read vector is: ");
    
    for (int i = 0; i < Length; i++)
    {
        printf("%d ", Array[i]);
    }

    printf("\n");
}

/// <summary>
/// Prints on the screen the menu
/// </summary>
void PrintMenu()
{
    printf("1. Read a vector\n");
    printf("2. Print the last read vector\n");
    printf("3. Decompose a given even natural number, greater than 2, as a sum of two prime numbers\n");
    printf("4. Find the longest contiguous subsequence such that any consecutive elements have at least 2 distinct digits in common\n");
    printf("5. Exit\n");
}


int main()
{
    printf("Note: Vectors are indexed from 0!\n\n");

    int* numbers = NULL;
    int length = 0;
    int running = 1;

    while (running)
    {
        PrintMenu();

        int action;
        int retVal = scanf("%d", &action);
        if (retVal == 0)
        {
            char c = '0';
            do
            {
                c = getchar();
            } while (!isdigit(c));
            ungetc(c, stdin);
        }

        if (action == 1)
        {
            int retVal = ReadVector(&numbers, &length);
            if (retVal != 0)
            {
                printf("Error while reading the vector\n");
                continue;
            }
        }
        else if (action == 2)
        {
            PrintVector(numbers, length);
        }
        else if (action == 3)
        {
            FirstProblem();
        }
        else if (action == 4)
        {
            SecondProblem(numbers, length);
        }
        else if (action == 5)
        {
            running = 0;
        }
        else
        {
            printf("Invalid option\n");
        }
    }


    if (numbers != NULL)
    {
        free(numbers);
    }

    //_CrtDumpMemoryLeaks();
    return 0;
}