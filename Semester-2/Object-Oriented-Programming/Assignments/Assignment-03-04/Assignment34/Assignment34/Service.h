#pragma once

#include "Repository.h"

typedef struct _Service {
	Repository* repository;

	Vector* undoStack;
	int undoIndex;

	Vector* repoCopies;
	int repoCopiesIndex;
} Service;

/// <summary>
/// Creates a service object
/// </summary>
/// <param name="repository"> The repository linked to the service </param>
/// <returns>
///		The new service object or NULL if there was an error
/// </returns>
Service* createService(Repository* repository);

/// <summary>
/// Destroys a service object
/// </summary>
/// <param name="service"> A service object </param>
void destroyService(Service* service);

/// <summary>
/// Creats an estate object an adds it to the repository that's linked with the service
/// </summary>
/// <param name="service"> A service object </param>
/// <param name="type"> A char* representing the type </param>
/// <param name="address"> A char* representing the address </param>
/// <param name="surface"> A double representing the surface </param>
/// <param name="price"> A double representhing the price </param>
/// <returns>
///		Success, if the estate was successfully added - 
///		EstateAlreadyExists, if the estate already exits - 
///		SurfaceError, if the surface was not valid - 
///		PriceError, if the price was not valid - 
///		TypeError, if the type was not valid -
///		MemoryIssue, if there were any problems when initializing the memory
/// </returns>
int addEstateService(Service* service, char* type, char* address, double surface, double price);

/// <summary>
/// Removes an estate from the linked repository
/// </summary>
/// <param name="service"> A service object </param>
/// <param name="address"> A char* representing the address </param>
/// <returns></returns>
int removeEstateService(Service* service, char* address);

/// <summary>
/// Updates an estate in the linked repository
/// </summary>
/// <param name="service"> A Service* object </param>
/// <param name="oldAddress"> The old address of the estate </param>
/// <param name="newAddress"> The new address of the estate</param>
/// <param name="newType"> The new type of the estae </param>
/// <param name="newSurface"> The new surface of the estate </param>
/// <param name="newPrice"> The new price of the estate </param>
/// <param name="changes"> An integer representing the changes;
///		changes & 1 -> the address was modified;
///		changes & 2 -> the type was modified;
///		changes & 4 -> the surface was modified; 
///		changes & 8 -> the price was modified
/// </param>
/// <returns></returns>
int updateEstateService(Service* service, char* oldAddress, char* newAddress, char* newType, double newSurface, double newPrice, int changes);

/// <summary>
/// Creates a repository of estates which's address match the given string
/// </summary>
/// <param name="service"> A Serivce object </param>
/// <param name="string"> A char* string representing the address </param>
/// <returns> A repository of estates which's address match the given string</returns>
Repository* filterByAddress(Service* service, char* string);

/// <summary>
/// Creates a repository of estates of the specified type and which's surface is greater then the provided value
/// </summary>
/// <param name="service"> A Serivce object </param>
/// <param name="type"> A char* string representing the type </param>
/// <param name="surface"> A double representing the minimum surface </param>
/// <returns> A repository of estates of the specified type and which's surface is greater then the provided value </returns>
Repository* filterByTypeAndSurface(Service* service, char* type, double surface);

/// <summary>
/// Creates a repository of estates of the specified type and which's price is smaller then the provided value
/// </summary>
/// <param name="service"> A Serivce object </param>
/// <param name="type"> A char* string representing the type </param>
/// <param name="price"> A double representing the maximum price </param>
/// <returns> A repository of estates of the specified type and which's price is smaller then the provided value </returns>
Repository* filterByTypeAndPrice(Service* service, char* type, double price);

/// <summary>
/// Get the repository that is linked with the specified service
/// </summary>
/// <param name="service"> A Service* object </param>
/// <returns> The repository that is linked with the specified service </returns>
Repository* getRepo(Service* service);


/// <summary>
/// Check if an estate exists in the repository
/// </summary>
/// <param name="repository" A Repository object ></param>
/// <param name="address"> A char* string </param>
/// <returns> MemoryIssue - if any of the parameters was null; EstateNotFound - if the estate is not in the repository; 
/// EstateAlreadyExists - if the estate is in the repository
/// </returns>
int checkIfEstateExists(Service* service, char* address);

/// <summary>
/// Undo the last performed operation using repository copies
/// </summary>
/// <param name="service"> A Service* </param>
/// <returns> 
///		MemoryIssue - if the parameter was NULL;
///		NothingLeftToUndo - if there is nothing to undo;
///		Success - if the last operation was undid
/// </returns>
int undoUsingCopies(Service* service);

/// <summary>
/// Redo the last udno using repository copies
/// </summary>
/// <param name="service"> A Service* </param>
/// <returns> 
///		MemoryIssue - if the parameter was NULL;
///		NothingLeftToRdo - if there is nothing to undo;
///		Success - if the last operation was redid
/// </returns>
int redoUsingCopies(Service* service);

/// <summary>
/// Undo the last performed operation using function pointers
/// </summary>
/// <param name="service"> A Service* </param>
/// <returns> 
///		MemoryIssue - if the parameter was NULL;
///		NothingLeftToUndo - if there is nothing to undo;
///		Success - if the last operation was undid
/// </returns>
int undo(Service* service);

/// <summary>
/// Redo the last udno using function pointers
/// </summary>
/// <param name="service"> A Service* </param>
/// <returns> 
///		MemoryIssue - if the parameter was NULL;
///		NothingLeftToRdo - if there is nothing to undo;
///		Success - if the last operation was redid
/// </returns>
int redo(Service* service);