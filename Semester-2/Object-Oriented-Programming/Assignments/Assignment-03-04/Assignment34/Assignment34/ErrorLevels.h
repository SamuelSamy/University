#pragma once

#define Success 0
#define MemoryIssue -1

#define TypeError -100
#define SurfaceError -101
#define PriceError -102
#define EstateNotFound -103
#define IndexNotFound -104
#define EstateAlreadyExists -105
#define ErrorUndoStack -106
#define NothingLeftToUndo -107
#define NothingLeftToRedo -108

#define TypeErrorMessage "\n>> The specified type is not valid. Valid types: house, apartment or penthouse\n"
#define SurfaceErrorMessage "\n>> The specified surface is not valid. The surface must be a dobule greater than 0\n"
#define PriceErrorMessage "\n>> The specified price is not valid. The price must be a double greater than 0\n"
#define EstateAlreadyExistsMessage "\n>> There is already an estate with the specified address\n"
#define MemoryIssueMessage "\n>> An unexpected error occured (some data was not initialized correctly\n"
#define EstateNotFoundMessage "\n>> No estate found with the specified address\n"
#define NothingLeftToUndoMessage "\n>> There is nothing to undo\n"
#define NothingLeftToRedoMessage "\n>> There is nothing to redo\n"