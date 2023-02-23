-- Version: 5
USE HousingDatabase;
GO

-- d. add / remove a primary key;

CREATE OR ALTER PROCEDURE procRemovePrimaryKey
AS
BEGIN
	ALTER TABLE Expenses
	DROP CONSTRAINT PK_Expenses
END
GO


-- undo

CREATE OR ALTER PROCEDURE procAddPrimaryKey
AS
BEGIN
	ALTER TABLE Expenses
	ADD CONSTRAINT PK_Expenses PRIMARY KEY(ID)
END
