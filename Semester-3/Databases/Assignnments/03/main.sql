USE HousingDatabase;
GO

SELECT *
FROM VersionTable

SELECT *
FROM ProceduresTable

INSERT INTO ProceduresTable (fromVersion, toVersion, procedureName)
VALUES
(0, 1, 'procChangePaymentsAmountToFloat'),
(1, 0, 'procChangePaymentsAmountToInt'),
(1, 2, 'procCreateExpensesTable'),
(2, 1, 'procDropExpensesTable'),
(2, 3, 'procAddDescrptionColumn'),
(3, 2, 'procRemoveDescrptionColumn'),
(3, 4, 'procAddDefaultPaid'),
(4, 3, 'procRemoveDefaultPaid'),
(4, 5, 'procRemovePrimaryKey'),
(5, 4, 'procAddPrimaryKey'),
(5, 6, 'procAddCandidateKey'),
(6, 5, 'procRemoveCandidateKey'),
(6, 7, 'procAddForeignKey'),
(7, 6, 'procRemoveForeignKey')



GO
CREATE OR ALTER PROCEDURE goToVersion (@newVersion INT) 
AS
BEGIN
	DECLARE @currentVersion INT
	DECLARE @minVersion INT
	DECLARE @maxVersion INT
	DECLARE @procedure VARCHAR(1024)
	
	SELECT @currentVersion = version
	FROM VersionTable

	SELECT @minVersion = MIN(fromVersion)
	FROM ProceduresTable

	SELECT @maxVersion = MAX(fromVersion)
	FROM ProceduresTable


	IF @currentVersion = @newVersion
	BEGIN
		DECLARE @newVersionString NCHAR(2)
		SELECT @newVersionString = CAST(@newVersion AS NCHAR)
		RAISERROR (N'You are already on version %s', 18, 1, @newVersionString)
		RETURN
	END


	IF @newVersion < @minVersion
	BEGIN
		DECLARE @minVersionString NCHAR(2)
		SELECT @minVersionString = CAST(@newVersion AS NCHAR)
		RAISERROR (N'Minimum version is %s', 18, 1, @minVersionString)
		RETURN
	END


	IF @newVersion > @maxVersion
	BEGIN
		DECLARE @maxVersionString NCHAR(2)
		SELECT @maxVersionString = CAST(@newVersion AS NCHAR)
		RAISERROR (N'Maximum version is %s', 18, 1, @maxVersionString)
		RETURN
	END	

	DECLARE @step INT
	SET @step = 1 -- go up the versions
	IF @currentVersion > @newVersion
		SET @step = -1 -- go down the versions

	WHILE @currentVersion != @newVersion BEGIN
		SELECT @procedure = procedureName
		FROM ProceduresTable
		WHERE fromVersion = @currentVersion AND toVersion = @currentVersion + @step

		PRINT N'Going from version ' + CAST(@currentVersion AS NCHAR(2)) + N' to version ' + CAST(@currentVersion + @step AS NCHAR(2)) + N' using procedure: ' + CAST(@procedure AS NCHAR(1024));

		EXECUTE (@procedure)
		SET @currentVersion = @currentVersion + @step

		UPDATE VersionTable
		SET version = @currentVersion
	END
END


SELECT *
FROM VersionTable

EXECUTE goToVersion @newVersion = 0
EXECUTE goToVersion @newVersion = 5
EXECUTE goToVersion @newVersion = 7
EXECUTE goToVersion @newVersion = 10
EXECUTE goToVersion @newVersion = -1

EXECUTE goToVersion @newVersion = 7