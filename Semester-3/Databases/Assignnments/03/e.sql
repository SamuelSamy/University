-- Version: 6
USE HousingDatabase;
GO

-- e. add / remove a candidate key;

CREATE OR ALTER PROCEDURE procAddCandidateKey
AS
BEGIN
	ALTER TABLE Payments
	ADD CONSTRAINT CK_Payments_Uniq UNIQUE (SID, due)
END
GO


-- undo

CREATE OR ALTER PROCEDURE procRemoveCandidateKey
AS
BEGIN
	ALTER TABLE Payments
	DROP CONSTRAINT CK_Payments_Uniq
END
