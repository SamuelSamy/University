#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


int compute_maxim(int*, int);
void read_numbers(char*);
bool is_number(char*);
void split_numbers(char*, int[], int*);
void write_number(int);


int arr[1024], len = 0;
char numbers[1024];


int main()
{
    read_numbers(numbers);
    split_numbers(numbers, arr, &len);
    int maxim = compute_maxim(arr, len);
    write_number(maxim);
        
    return 0;
}

void read_numbers(char* numbers)
{
    scanf("%1023[^\n]", numbers);
}


void write_number(int number)
{
    FILE* fp;
    fp = fopen("data.txt", "w");
    fprintf(fp, "The maximum number is %d", number);
    fclose(fp);
}

bool is_number(char* string)
{
    /// <summary>
    /// Check if a string can be represented as a number
    /// </summary>
    /// <param name="string"></param>
    /// <returns>true if the string is a number, false otherwise </returns>
    bool has_minus_sign = 0;

    for (int i = 0; i < strlen(string); i++)
    {
        if (string[i] == '-')
        {
            if (!has_minus_sign)
                has_minus_sign = true;
            else
                return false;
        }
        else if (!(string[i] >= '0' && string[i] <= '9'))
        {
            return false;
        }
    }

    if (has_minus_sign && strlen(string) == 1)
    {
        return false;
    }

    return true;
}


void split_numbers(char* numbers, int array[], int* len)
{
    /// <summary>
    /// Splits a string into an array of numbers
    /// </summary>
    /// <param name="number"></param>
    /// <param name="array"></param>
    /// <param name="len"></param>
    
    (*len) = 0;

    char* p = strtok(numbers, " ");


    while (p)
    {
        if (!is_number(p))
        {
            printf("Jumped over `%s` because it is not a valid number\n", p);
        }
        else
        {
            array[(*len)++] = atoi(p);
        }

        p = strtok(NULL, " ");
    }
}