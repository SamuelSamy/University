-- Version: 7

USE HousingDatabase;
GO

-- f. add / remove a foreign key;

CREATE OR ALTER PROCEDURE procAddForeignKey
AS
BEGIN
	ALTER TABLE Expenses
	ADD CONSTRAINT FK_Expenses_BID FOREIGN KEY (BID) REFERENCES Buildings(ID) ON DELETE CASCADE
END
GO 


-- undo

CREATE OR ALTER PROCEDURE procRemoveForeignKey
AS
BEGIN
	ALTER TABLE Expenses
	DROP CONSTRAINT FK_Expenses_BID
END