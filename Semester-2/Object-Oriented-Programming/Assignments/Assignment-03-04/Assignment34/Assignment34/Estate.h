#pragma once

typedef struct _Estate {
	char* type;
	char* address;
	double surface;
	double price;
} Estate;

/// <summary>
/// Creates a new estate object if possible
/// </summary>
/// <param name="type"> The type of the estate </param>
/// <param name="adress"> The address of the estate </param>
/// <param name="surface"> The surface of the estate </param>
/// <param name="price"> The price of the estate </param>
/// <param name="errorLevel"> 
///		Success, if there were no errors 
///		SurfaceError, if the surface was not valid
///		PriceError, if the price was not valid
///		TypeError, if the type was not valid
///		MemoryIssue, if there were any problems when initializing the memory
/// </param>
/// <returns> A new Estate object or NULL if there was an error </returns>
Estate* createEstate(char* type, char* adress, double surface, double price, int* errorLevel);




/// <summary>
/// Destroys the estate from memory
/// </summary>
/// <param name="estate"> An estate object </param>
void destroyEstate(Estate* estate);

/// <summary>
/// Get the type of an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <returns> A char* representing the type </returns>
char* getType(Estate* estate);

/// <summary>
/// Get the address of an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <returns> A char* representhing the address </returns>
char* getAddress(Estate* estate);

/// <summary>
/// Get the surface of an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <returns> A double representhing the surface </returns>
double getSurface(Estate* estate);

/// <summary>
/// Get the price of an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <returns> A double representing the price </returns>
double getPrice(Estate* estate);

/// <summary>
/// Sets the type for an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <param name="newType"> The new type of the estate </param>
/// <returns>
///		Success, if the type was successfully set,
///		MemoryIssue, if there was an error regarding the memory (the parameter was NULL or there was not enough space for malloc),
///		TypeError, if the type was not valid
/// </returns>
int setType(Estate* estate, char* newType);

/// <summary>
/// Sets the address of an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <param name="newAddress"> The new address of the estate </param>
/// <returns>
///		Success, if the address was successfully set,
///		MemoryIssue, if there was an error regarding the memory (the parameter was NULL or there was not enough space for malloc),
/// </returns>
int setAddress(Estate* estate, char* newAddress);

/// <summary>
/// Sets the surface of an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <param name="newSurface"> The new surface of the estate </param>
/// <returns>
///		Success, if the surface was successfully set,
///		MemoryIssue, if there was an error regarding the memory (the parameter was NULL or there was not enough space for malloc),
///		SurfaceError, if the surface was not valid
/// </returns>
int setSurface(Estate* estate, double newSurface);

/// <summary>
/// Sets the price of an estate
/// </summary>
/// <param name="estate"> An estate object </param>
/// <param name="newPrice"> The new price of the estate </param>
/// <returns>
///		Success, if the price was successfully set,
///		MemoryIssue, if there was an error regarding the memory (the parameter was NULL or there was not enough space for malloc),
///		PriceError, if the price was not valid
/// </returns>
int setPrice(Estate* estate, double newPrice);

/// <summary>
/// Converts an estate to string
/// </summary>
/// <param name="estate"> An estate object </param>
/// <param name="str"> The char* where the string will be stored </param>
void toString(Estate* estate, char str[]);


Estate* copyEstate(Estate* estate);