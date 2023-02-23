#pragma once

#include "Estate.h"
#include "Vector.h"

typedef struct _Repository {
	Vector* data;
} Repository;

/// <summary>
/// Creates a repository object for estates
/// </summary>
/// <returns> A new repository object</returns>
Repository* createRepository();

/// <summary>
/// Destroys a repository from memory
/// </summary>
/// <param name="repository"> A repository object </param>
void destroyRepositroy(Repository* repository);

/// <summary>
/// Adds an estate to a repository
/// </summary>
/// <param name="repository"> A repository object </param>
/// <param name="parameters"> A vector object representing the parameters </param>
/// <returns>
///		Success, if the estate was successfully added - 
///		MemoryIssue, if there was an error regarding the memory (NULL params / malloc errors)
/// </returns>
int addEstate(Repository* repository, Vector* parameters);

/// <summary>
/// Removes an estate from a repository
/// </summary>
/// <param name="repository"> A repository object </param>
/// <param name="parameters"> A vector object representing the parameters </param>
/// <returns>
///		Success, if the estate was successfully removed from the repository -
///		EstateNotFound, if the estate is not in the repository -
///		MemoryIssue, if there was an error regarding the memory (NULL params / malloc errors)
/// </returns>
int removeEstate(Repository* repository, Vector* parameters);

/// <summary>
/// Updates an estate
/// </summary>
/// <param name="repository"> A repository object </param>
/// <param name="parameters"> A vector object representing the parameters </param>
/// <returns>
///		Success, if the estate was successfully updated - 
///		MemoryIssue, if there was an error regarding the memory (NULL params / malloc errors)
/// </returns>
int updateEstate(Repository* repository, Vector* parameters);

/// <summary>
/// Get the length of the repository
/// </summary>
/// <param name="repository"> A repository object </param>
/// <returns>
///		-1, if there was an error else the length of the repository
/// </returns>
int getLength(Repository* repository);

/// <summary>
/// Sort a repository of estates by price
/// </summary>
/// <param name="repository"> A repository object  </param>
/// <param name="ascending"> 1 for ascending, 0 for descending </param>
void sortEstatesByPrice(Repository* repository, int ascending);

/// <summary>
/// Check if an estate exists in the repository
/// </summary>
/// <param name="repository" A Repository object ></param>
/// <param name="address"> A char* string </param>
/// <returns> MemoryIssue - if any of the parameters was null; EstateNotFound - if the estate is not in the repository; 
/// EstateAlreadyExists - if the estate is in the repository
/// </returns>
int checkIfEstateExistsRepo(Repository* repository, char* address);

/// <summary>
/// Searches and returns the estate with the specified address
/// </summary>
/// <param name="repository"> A Repository object </param>
/// <param name="address"> A char* string representing the address </param>
/// <param name="errorLevel"> An integer number acting as a flag</param>
/// <returns> The estate having the specified address, NULL if there is no such estate </returns>
Estate* getEstateByAddress(Repository* repository, char* address, int* errorLevel);

/// <summary>
/// Creates and returns a repository object containing only the estates which's address match the given string
/// </summary>
/// <param name="repository"> A Repository object containg the elements with are going to iterate </param>
/// <param name="address"> A char* string representing the address </param>
/// <returns> A new repository object containing only the estates which's address match the given string </returns>
Repository* filterByAddressRepo(Repository* repository, char* address);

/// <summary>
/// Creates and returns a repository object object containing only the estates of the given type having the surface greater than the provided value
/// </summary>
/// <param name="repository"> A Repository object containg the elements with are going to iterate </param>
/// <param name="type"> A char* string representing the type </param>
/// <param name="surface"> A double representing the minimum surface </param>
/// <returns> A repository object object containing only the estates of the given type having the surface greater than the provided value</returns>
Repository* filterByTypeAndSurfaceRepo(Repository* repository, char* type, double surface);

/// <summary>
/// Creates and returns a repository object object containing only the estates of the given type having the price smaller than the provided value
/// </summary>
/// <param name="repository"> A Repository object containg the elements with are going to iterate </param>
/// <param name="type"> A char* string representing the type </param>
/// <param name="price"> A double representing the maximum price </param>
/// <returns> A repository object object containing only the estates of the given type having the price smaller than the provided value</returns>
Repository* filterByTypeAndPriceRepo(Repository* repository, char* type, double price);

/// <summary>
/// A deep copy of the given repository
/// </summary>
/// <param name="repository"> A Repository object containg the elements with are going to iterate </param>
/// <returns> A deep copy of the given repository </returns>
Repository* copyRepositoryOfEstates(Repository* repository);