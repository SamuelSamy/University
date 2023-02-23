#define _CRTDBG_MAP_ALLOC
#define _CRT_SECURE_NO_WARNINGS

#define referrence_param(x) x

#include "ErrorLevels.h"
#include <crtdbg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "UI.h"



UI* createUI(Service* service)
{
	UI* ui = malloc(sizeof(UI));

	if (ui == NULL)
	{
		return NULL;
	}

	ui->service = service;

	return ui;
}

void destroyUI(UI* ui)
{
	if (ui == NULL)
	{
		return;
	}

	destroyService(ui->service);
	free(ui);
}


void printEstates(Repository* repository)
{
	int len = getLength(repository);

	if (len == 0)
	{
		printf(">> No estates found\n");
		return;
	}

	for (int i = 0; i < len; i++)
	{
		Estate* estate;
		vectorGetValueByIndex(repository->data, i, &estate);

		char str[500];
		toString(estate, str);
		printf("%4d) %s\n", i, str);
	}

	printf("\n");
}


void printMenu()
{
	printf("\n--------------------------------------------------\n");
	printf("1. Add estate\n");
	printf("2. Delete estate\n");
	printf("3. Update estate\n");
	printf("4. Display estates that contain a given string\n");
	printf("5. See all estates of a given type, having the surface greater than a user provided value\n");
	printf("6. See all estates of a given type, having the price smaller than a user provided value\n");
	printf("7. Undo\n");
	printf("8. Redo\n");
	printf("0. Exit");
	printf("\n--------------------------------------------------\n");
}

int readInteger(char* message)
{
	char value[16] = { 0 };
	int result = 0;
	int r = 0;
	int isInt = 0;

	while (isInt == 0)
	{
		printf("<< %s", message);

		int retValue = scanf("%15s", value);
		if (retValue == 0)
		{
			printf(">> Errow while reading the number!\n");
			continue;
		}

		r = sscanf(value, "%d", &result);
		isInt = (r == 1);

		if (isInt == 0)
		{
			printf(">> Please enter a valid integer!\n");
		}
	}

	return result;
}

double readDouble(char* message)
{
	char value[16] = { 0 };
	double result = 0;
	int r = 0;
	int isInt = 0;

	while (isInt == 0)
	{
		printf("<< %s", message);

		int retValue = scanf("%15s", value);
		if (retValue == 0)
		{
			printf(">> Errow while reading the number!\n");
			continue;
		}

		r = sscanf(value, "%lf", &result);
		isInt = (r == 1);

		if (isInt == 0)
		{
			printf(">> Please enter a valid number!\n");
		}
	}

	return result;
}

int isValidCommand(int command)
{
	return command >= 0 && command <= 8;
}

int addEstateUI(UI* ui)
{
	if (ui == NULL)
	{
		return MemoryIssue;
	}

	char type[100];
	char address[100];
	double surface = -1;
	double price = -1;

	printf("<< Please input the address: ");
	int getchar_result = getchar();
	int scanf_result = scanf("%[^\n]99s", address);

	printf("<< Please input the type: ");
	getchar_result = getchar();
	scanf_result = scanf("%[^\n]99s", type);


	surface = readDouble("Please input the surface: ");
	price = readDouble("Please input the price: ");

	return addEstateService(ui->service, type, address, surface, price);
}

int removeEstateUI(UI* ui)
{
	if (ui == NULL)
	{
		return MemoryIssue;
	}

	printf("<< Please input the address of the estate you want to delete: ");
	char str[100];
	int getchar_result = getchar();
	referrence_param(getchar_result);
	int scanf_result = scanf("%[^\n]99s", str);
	referrence_param(scanf_result);

	int retValue = removeEstateService(ui->service, str);
	return retValue;
}

int updateEstateUI(UI* ui)
{
	if (ui == NULL)
	{
		return MemoryIssue;
	}

	printf("<< Please input the address of the estate you want to update: ");
	char str[100];
	int getchar_result = getchar();
	int scanf_result = scanf("%[^\n]99s", str);
	referrence_param(scanf_result);
	int retValue = checkIfEstateExists(ui->service, str);
	if (retValue != EstateAlreadyExists)
	{
		return EstateNotFound;
	}

	char address[100];
	char type[100];
	double surface = -1;
	double price = -1;

	int changes = 0;

	char answer;

	printf("<< Do you want to change the address? (y/n): ");
	getchar_result = getchar();
	scanf_result = scanf("%c", &answer);
	if (answer == 'y')
	{
		printf("<< Please input the new address: ");
		getchar_result = getchar();
		scanf_result = scanf("%[^\n]99s", address);
		changes |= 1;
	}

	printf("<< Do you want to change the type? (y/n): ");
	getchar_result = getchar();
	scanf_result = scanf("%c", &answer);
	if (answer == 'y')
	{
		printf("<< Please input the new type: ");
		getchar_result = getchar();
		scanf_result = scanf("%[^\n]99s", type);
		changes |= 2;
	}

	printf("<< Do you want to change the surface? (y/n): ");
	getchar_result = getchar();
	scanf_result = scanf("%c", &answer);
	if (answer == 'y')
	{
		surface = readDouble("Please input the new surface: ");
		changes |= 4;
	}

	printf("<< Do you want to change the price? (y/n): ");
	getchar_result = getchar();
	scanf_result = scanf("%c", &answer);
	if (answer == 'y')
	{
		price = readDouble("Please input the new price: ");
		changes |= 8;
	}

	referrence_param(scanf_result);
	retValue = updateEstateService(ui->service, str, address, type, surface, price, changes);
	return retValue;
}

void displayEstatesUI(UI* ui)
{
	if (ui == NULL)
	{
		return;
	}

	char string[100] = { 0 };
	printf("<< Please input a string (enter 0 for all): ");

	int getchar_result = getchar();
	referrence_param(getchar_result);

	int scanf_result = scanf("%[^\n]s", string);
	referrence_param(scanf_result);

	Repository* repository;

	if (strcmp(string, "0") == 0)
	{
		repository = filterByAddress(ui->service, NULL);
	}
	else
	{
		repository = filterByAddress(ui->service, string);
	}

	if (repository == NULL)
	{
		return;
	}

	int number = readInteger("How would you like to sort the estates?\n1. Ascending\n2. Descending\nEnter number: ");
	while (number != 1 && number != 2)
	{
		printf(">> Invalid option\n");
		number = readInteger("How would you like to sort the estates?\n1. Ascending\n2. Descending\nEnter number: ");
	}

	sortEstatesByPrice(repository, number % 2);
	printEstates(repository);
	destroyRepositroy(repository);
}

void filterEstatesByTypeAndSurfaceUI(UI* ui)
{
	if (ui == NULL)
	{
		return;
	}

	char type[100] = { 0 };
	printf("<< Please input a type (house, penthouse, apartment): ");
	int getchar_result = getchar();

	int scanf_result = scanf("%[^\n]s", type);


	while (strcmp(type, "house") != 0 && strcmp(type, "penthouse") != 0 && strcmp(type, "apartment") != 0)
	{
		printf(">> Invalid input.\n");

		printf("<< Please input a type (house, penthouse, apartment): ");
		getchar_result = getchar();

		scanf_result = scanf("%[^\n]s", type);
	}

	double surface = readDouble("Enter a surface: ");

	Repository* repository = filterByTypeAndSurface(ui->service, type, surface);

	if (repository == NULL)
	{
		return;
	}

	printEstates(repository);
	destroyRepositroy(repository);
}


void filterEstatesByTypeAndPriceUI(UI* ui)
{
	if (ui == NULL)
	{
		return;
	}

	char type[100] = { 0 };
	printf("<< Please input a type (house, penthouse, apartment): ");
	int getchar_result = getchar();

	int scanf_result = scanf("%[^\n]s", type);


	while (strcmp(type, "house") != 0 && strcmp(type, "penthouse") != 0 && strcmp(type, "apartment") != 0)
	{
		printf(">> Invalid input.\n");

		printf("<< Please input a type (house, penthouse, apartment): ");
		getchar_result = getchar();

		scanf_result = scanf("%[^\n]s", type);
	}

	double price = readDouble("Enter a price: ");
	Repository* repository = filterByTypeAndPrice(ui->service, type, price);

	if (repository == NULL)
	{
		return;
	}

	sortEstatesByPrice(repository, 1);
	printEstates(repository);
	destroyRepositroy(repository);
}

int undoUI(UI* ui, int undoType)
{
	if (undoType == 0)
	{
		return undo(ui->service);
	}

	return undoUsingCopies(ui->service);
}

int redoUI(UI* ui, int undoType)
{
	if (undoType == 0)
	{
		return redo(ui->service);
	}

	return redoUsingCopies(ui->service);
}

void checkReturnValue(int retValue)
{
	if (retValue == Success)
	{
		return;
	}

	if (retValue == MemoryIssue)
	{
		printf(MemoryIssueMessage);
	}
	else if (retValue == TypeError)
	{
		printf(TypeErrorMessage);
	}
	else if (retValue == SurfaceError)
	{
		printf(SurfaceErrorMessage);
	}
	else if (retValue == PriceError)
	{
		printf(PriceErrorMessage);
	}
	else if (retValue == EstateAlreadyExists)
	{
		printf(EstateAlreadyExistsMessage);
	}
	else if (retValue == EstateNotFound)
	{
		printf(EstateNotFoundMessage);
	}
	else if (retValue == NothingLeftToRedo)
	{
		printf(NothingLeftToRedoMessage);
	}
	else if (retValue == NothingLeftToUndo)
	{
		printf(NothingLeftToUndoMessage);
	}
	else
	{
		printf("Unexpected error occured. Error code: %d", retValue);
	}
}

void startUI(UI* ui, int undoType)
{
	if (ui == NULL)
	{
		return;
	}

	while (1)
	{
		printMenu();
		int command = readInteger("Enter command number: ");

		while (isValidCommand(command) == 0)
		{
			printf(">> Invalid option\n");
			command = readInteger("Enter command number: ");
		}

		printf("\n");

		if (command == 0)
		{
			break;
		}

		int retValue = 0;

		if (command == 1)
		{
			retValue = addEstateUI(ui);

			if (retValue == Success)
			{
				printf("\n>> Estate successfully added\n");
			}
		}
		else if (command == 2)
		{
			retValue = removeEstateUI(ui);

			if (retValue == Success)
			{
				printf("\n>> Estate successfully deleted\n");
			}
		}
		else if (command == 3)
		{
			retValue = updateEstateUI(ui);

			if (retValue == Success)
			{
				printf("\n>> Estate successfully updated\n");
			}
		}
		else if (command == 4)
		{
			displayEstatesUI(ui);
		}
		else if (command == 5)
		{
			filterEstatesByTypeAndSurfaceUI(ui);
		}
		else if (command == 6)
		{
			filterEstatesByTypeAndPriceUI(ui);
		}
		else if (command == 7)
		{
			retValue = undoUI(ui, undoType);

			if (retValue == Success)
			{
				printf("\n>> The last operation was undid\n");
			}
		}
		else if (command == 8)
		{
			retValue = redoUI(ui, undoType);
			
			if (retValue == Success)
			{
				printf("\n>> The last operation was redid\n");
			}
		}

		checkReturnValue(retValue);
	}

	printf("\nExiting UI...\n");
}

