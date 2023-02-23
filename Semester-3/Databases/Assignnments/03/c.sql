-- Version: 4
USE HousingDatabase;
GO

-- c. add / remove a DEFAULT constraint;

CREATE OR ALTER PROCEDURE procAddDefaultPaid
AS
BEGIN
	ALTER TABLE Payments
	ADD CONSTRAINT default_paid DEFAULT 0 FOR paid
END
GO


-- UNDO
CREATE OR ALTER PROCEDURE procRemoveDefaultPaid
AS
BEGIN
	ALTER TABLE Payments
	DROP CONSTRAINT default_paid
END
GO