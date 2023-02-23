#define _CRTDBG_MAP_ALLOC
#define _CRT_SECURE_NO_WARNINGS

#include "Estate.h"
#include "ErrorLevels.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <crtdbg.h>


int isValidType(char* type)
{
	return strcmp(type, "house") == 0 || strcmp(type, "apartment") == 0 || strcmp(type, "penthouse") == 0;
}

int isValidSurface(double surface)
{
	return surface > 0;
}

int isValidPrice(double price)
{
	return price > 0;
}


Estate* createEstate(char* type, char* address, double surface, double price, int* errorLevel)
{
	*errorLevel = Success;

	if (!isValidType(type))
	{
		*errorLevel = TypeError;
		return NULL;
	}

	if (!isValidSurface(surface))
	{
		*errorLevel = SurfaceError;
		return NULL;
	}

	if (!isValidPrice(price))
	{
		*errorLevel = PriceError;
		return NULL;
	}

	Estate* estate = malloc(sizeof(Estate));

	if (estate == NULL)
	{
		*errorLevel = MemoryIssue;
		return NULL;
	}

	estate->type = malloc(sizeof(char) * (strlen(type) + 1));
	
	if (estate->type == NULL)
	{
		free(estate);
		*errorLevel = MemoryIssue;
		return NULL;
	}

	estate->address = malloc(sizeof(char) * (strlen(address) + 1));

	if (estate->address == NULL)
	{
		free(estate->type);
		free(estate);
		*errorLevel = MemoryIssue;
		return NULL;
	}
	
	strcpy(estate->type, type);
	strcpy(estate->address, address);
	estate->price = price;
	estate->surface = surface;

	return estate;
}

Estate* copyEstate(Estate* estate)
{
	if (estate == NULL)
	{
		return NULL;
	}

	int errorLevel;
	return createEstate(estate->type, estate->address, estate->surface, estate->price, &errorLevel);
}

void destroyEstate(Estate* estate)
{
	if (estate == NULL)
	{
		return;
	}

	free(estate->type);
	free(estate->address);
	
	free(estate);
}

char* getType(Estate* estate)
{
	if (estate == NULL)
	{
		return NULL;
	}
	
	return estate->type;
}

char* getAddress(Estate* estate)
{
	if (estate == NULL)
	{
		return NULL;
	}

	return estate->address;
}

double getSurface(Estate* estate)
{
	if (estate == NULL)
	{
		return -1.0;
	}

	return estate->surface;
}

double getPrice(Estate* estate)
{
	if (estate == NULL)
	{
		return -1.0;
	}

	return estate->price;
}

int setType(Estate* estate, char* newType)
{
	if (estate == NULL)
	{
		return MemoryIssue;
	}

	if (!isValidType(newType))
	{
		return TypeError;
	}

	free(estate->type);
	estate->type = malloc(sizeof(char) * (strlen(newType) + 1));
	
	if (estate->type == NULL)
	{
		return MemoryIssue;
	}

	strcpy(estate->type, newType);
	return Success;
}

int setAddress(Estate* estate, char* newAddress)
{
	if (estate == NULL)
	{
		return MemoryIssue;
	}

	free(estate->address);
	estate->address = malloc(sizeof(char) * (strlen(newAddress) + 1));

	if (estate->address == NULL)
	{
		return MemoryIssue;
	}

	strcpy(estate->address, newAddress);
	return Success;
}

int setSurface(Estate* estate, double newSurface)
{
	if (estate == NULL)
	{
		return MemoryIssue;
	}

	if (!isValidSurface(newSurface))
	{
		return SurfaceError;
	}

	estate->surface = newSurface;

	return Success;
}

int setPrice(Estate* estate, double newPrice)
{
	if (estate == NULL)
	{
		return MemoryIssue;
	}

	if (!isValidPrice(newPrice))
	{
		return PriceError;
	}

	estate->price = newPrice;

	return Success;
}

void toString(Estate* estate, char str[])
{
	if (estate == NULL)
	{
		return;
	}

	sprintf(str, "Type: %-12s|  Address: %-15s|  Surface: %-12.2lf|  Price: %-12.2lf", getType(estate), getAddress(estate), getSurface(estate), getPrice(estate));
}

