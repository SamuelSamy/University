-- Version: 3
USE HousingDatabase;
GO

-- b. add / remove a column;

CREATE OR ALTER PROCEDURE procAddDescrptionColumn
AS
BEGIN
	ALTER TABLE Expenses
	ADD description VARCHAR(128)
END
GO


-- undo
CREATE OR ALTER PROCEDURE procRemoveDescriptionColumn
AS 
BEGIN
	ALTER TABLE Expenses
	DROP COLUMN description
END
GO