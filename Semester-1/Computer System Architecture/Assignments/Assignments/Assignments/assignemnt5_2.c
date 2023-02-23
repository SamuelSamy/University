//2. Replace all vowels with # in the text read from a given file.
 
#pragma warning(disable:4996)

#include <stdio.h>

int IsVowel(char Character)
{
    return (Character == 'a' || Character == 'e' || Character == 'i' || Character == 'o' || Character == 'u' ||
            Character == 'A' || Character == 'E' || Character == 'I' || Character == 'O' || Character == 'U');
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
    char buffer[1000];

    do
    {
        for (i = 0; (character = fgetc(file)) != EOF && i < max_len - 1; i++)
        {
            if (IsVowel(character))
            {
                buffer[i] = '#';
            }
            else
            {
                buffer[i] = character;
            }
        }

        buffer[i++] = '\n';

        for (int j = 0; j < i; j++)
        {
            printf("%c", buffer[j]);
        }

        if (feof(file))
        {
            break;
        }

    } while (1);
    
    fclose(file);
    return 0;
}*/