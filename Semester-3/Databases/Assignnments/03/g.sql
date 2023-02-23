-- Version: 2

USE HousingDatabase;
GO
-- g. create / drop a table.

CREATE OR ALTER PROCEDURE procCreateExpensesTable
AS
BEGIN
	CREATE TABLE Expenses(
		ID INT NOT NULL IDENTITY(1,1),
		BID INT,
		amount INT,
		date DATE,
		CONSTRAINT PK_Expenses PRIMARY KEY(ID)
	)
END
GO

-- undo
CREATE OR ALTER PROCEDURE procDropExpensesTable
AS
BEGIN
	DROP TABLE Expenses
END
GO