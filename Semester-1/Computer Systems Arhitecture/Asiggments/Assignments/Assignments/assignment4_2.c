//2. For a list of students, each having name, registration_number and 5 marks for current semester, sort them in descending order of the average of the 5 marks, and display the students with identical average in alphabetical order.

#pragma warning(disable:4996)
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char* Name;
    int RegistrationNumber;
    int* Grades;
    int TotalGrades;
    double Average;
} Student;

void Swap(Student* Student1, Student* Student2)
{
    Student aux = *Student2;
    *Student2 = *Student1;
    *Student1 = aux;
}


/*int GetStringLength(char* String)
{
    int length = 0;

    while (String[length] != '\0')
    {
        length++;
    }

    return length;
}*/

 //Returns:  0 -> The strings are equal
 //          1 -> The first string is greater
 //         -1 -> The second string is greater
int CompareStrings(char* String1, char* String2)
{
    int lengthString1 = GetStringLength(String1);
    int lengthString2 = GetStringLength(String2);
    int minim = lengthString1;

    if (minim > lengthString2)
    {
        minim = lengthString2;
    }

    for (int i = 0; i < minim; i++)
    {
        if (String1[i] > String2[i])
        {
            return -1;
        }
        else if (String1[i] < String2[i])
        {
            return 1;
        }
    }

    if (lengthString1 != lengthString2)
    {
        if (lengthString1 == minim)
        {
            return -1;
        }

        return 1;
    }

    return 0;
}

double GetAverage(Student Student)
{
    int sum = 0;

    for (int i = 0; i < Student.TotalGrades; i++)
    {
        sum += Student.Grades[i];
    }

    return 1.0 * sum / Student.TotalGrades;
}
/*
int main()
{
    int totalStudents = 5;
    char* Names[5] = { "Sam", "Dan", "Andi", "Cata", "Otni" };


    Student* students = (Student*)malloc(totalStudents * sizeof(Student));

    for (int k = 0; k < 3; k++)
    {
        students[k].Name = Names[k];
        students[k].RegistrationNumber = 100 + k;
        students[k].TotalGrades = 5;
        students[k].Grades = (int*)malloc(students[k].TotalGrades * sizeof(int));
        for (int i = 0; i < students[k].TotalGrades; i++)
        {
            students[k].Grades[i] = 6 + i;
        }
        students[k].Average = GetAverage(students[k]);
    }

    for (int k = 3; k < totalStudents; k++)
    {
        students[k].Name = Names[k];
        students[k].RegistrationNumber = 100 + k;
        students[k].TotalGrades = 5;
        students[k].Grades = (int*)malloc(students[k].TotalGrades * sizeof(int));
        for (int i = 0; i < students[k].TotalGrades; i++)
        {
            students[k].Grades[i] = 10;
        }
        students[k].Average = GetAverage(students[k]);
    }
    
     //Order the students by the average
    for (int i = 0; i < totalStudents; i++)
    {
        for (int j = i; j < totalStudents; j++)
        {
            if (students[i].Average < students[j].Average)
            {
                Swap(&students[i], &students[j]);
            }
        }
    }
    
 
    // Order those with the same average grade alphabetical 
    for (int i = 0; i < totalStudents; i++)
    {
        double currentAverage = students[i].Average;

        int startIndex = i;

        while (i < totalStudents && students[i].Average == currentAverage)
        {
            i++;
        }

        int finishedIndex = i;
        i--;

        for (int k = startIndex; k < finishedIndex; k++)
        {
            for (int j = k + 1; j < finishedIndex; j++)
            {
                if (1 == CompareStrings(students[k].Name, students[j].Name))
                {
                    Swap(&students[k], &students[j]);
                }
            }
        }

        if (startIndex != finishedIndex)
        {
            printf("Students with average: %.2f\n", currentAverage);
            for (int j = startIndex; j < finishedIndex; j++)
            {
                printf("Name: %s, Registration Number: %d\n", students[j].Name, students[j].RegistrationNumber);
            }
        }
    }
    
    // Free the memory
    for (int i = 0; i < totalStudents; i++)
    {
        free(students[i].Grades);
    }
    free(students);

    return 0;
}*/