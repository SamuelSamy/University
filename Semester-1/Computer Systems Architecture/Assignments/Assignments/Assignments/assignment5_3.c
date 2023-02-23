// 3. Replace even numbers with | in the text read from a given file.
 
#pragma warning(disable:4996)
 
#include <stdio.h>

int GetStringLength(char* String)
{
    int length = 0;

    while (String[length] != '\0')
    {
        length++;
    }

    return length;
}

int IsDigit(char Character)
{
    return Character >= '0' && Character <= '9';
}

int IsNumber(char* String)
{
    int length = GetStringLength(String);

    for (int i = 0; i < length; i++)
    {
        if (1 != IsDigit(String[i]))
        {
            return 0;
        }
    }

    return 1;
}

int IsEven(char* String)
{
    int length = GetStringLength(String);

    return String[length - 1] % 2 == 0;
}

/*
int main()
{
    FILE* file = fopen("file.txt", "r");
    
    if (NULL == file)
    {
        perror("Error in opening file");
        return -1;
    }

    int character, i;
    int max_len = 1000;
    char word[128];
    int totalCount = 0;

    do
    {
        int startIndex = totalCount;

        for (i = 0; (character = fgetc(file)) != EOF && i < max_len - 1 && character != ' ' && character != '\n'; i++)
        {
            word[i] = character;
            totalCount++;
        }
        
        int stopIndex = totalCount -1;
        char endCharacter = character;
        totalCount++;

        word[i++] = '\0';

        if (IsNumber(word) && IsEven(word))
        {
            printf("|");
        }
        else
        {
            printf("%s", word);
        }

        printf("%c", endCharacter);

        if (feof(file))
        {
            break;
        }

    } while (1);

    
    fclose(file);
    return 0;
}*/