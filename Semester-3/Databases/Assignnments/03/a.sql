-- Version: 1

USE HousingDatabase;
GO
-- a. modify the type of a column;

CREATE OR ALTER PROCEDURE procChangePaymentsAmountToFloat
AS
BEGIN
	ALTER TABLE Payments
	ALTER COLUMN amount FLOAT
END
GO

-- undo
CREATE OR ALTER PROCEDURE procChangePaymentsAmountToInt
AS
BEGIN
	ALTER TABLE Payments
	ALTER COLUMN amount INT
END

