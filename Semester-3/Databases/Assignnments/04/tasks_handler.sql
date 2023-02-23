
-- add test
CREATE OR ALTER PROCEDURE AddTest (@test VARCHAR(50))
AS
BEGIN
	INSERT INTO Tests (Name) VALUES (@test)
END
GO


-- add table to tests
CREATE OR ALTER PROCEDURE AddTable (@table VARCHAR(50))
AS
BEGIN
	IF NOT EXISTS (
		SELECT *
		FROM sys.tables
		WHERE name = @table
	) BEGIN
		DECLARE @errorMessage VARCHAR(MAX) = 'Table named: `' + @table + '` does not exist.'
		RAISERROR(@errorMessage, 18, 2)
		RETURN
	END

	INSERT INTO Tables (Name) VALUES (@table)
END
GO

-- add view
CREATE OR ALTER PROCEDURE AddView (@view VARCHAR(50))
AS
BEGIN
	IF NOT EXISTS(
		SELECT *
		FROM sys.views
		WHERE name = @view
	) BEGIN
		DECLARE @errorMessage VARCHAR(MAX) = 'View named: `' + @view + '` does not exist.'
		RAISERROR(@errorMessage, 18, 2)
		RETURN
	END

	INSERT INTO Views (Name) Values (@view)
END
GO


-- add table for test
CREATE OR ALTER PROCEDURE AddTableTest (@table VARCHAR(50), @test VARCHAR(50), @numberOfRows INT, @position INT)
AS 
BEGIN
	
	DECLARE @tableID INT = (
		SELECT TableID 
		FROM Tables
		WHERE Name = @table
	)

	DECLARE @testID INT = (
		SELECT testID
		FROM Tests
		WHERE Name = @test
	)

	INSERT INTO TestTables (TestID, TableID, NoOfRows, Position)
	VALUES (@testID, @tableID, @numberOfRows, @position)
END
GO

-- add test for view
CREATE OR ALTER PROCEDURE AddViewTest (@view VARCHAR(50), @test VARCHAR(50))
AS
BEGIN
	DECLARE @viewID INT = (
		SELECT ViewID
		FROM Views
		WHERE Name = @view
	)

	DECLARE @testID INT = (
		SELECT testID
		FROM Tests
		WHERE Name = @test
	)

	INSERT INTO TestViews (TestID, ViewID)
	VALUES (@testID, @viewID)
END
GO



-- run test
CREATE OR ALTER PROCEDURE RunTest (@test VARCHAR(50))
AS
BEGIN
	SET NOCOUNT ON;

	INSERT INTO TestRuns (Description) 
	VALUES (@test)

	DECLARE @query VARCHAR(MAX) = ''
	DECLARE @currentName VARCHAR(MAX) = ''
	DECLARE @currentID INT = 0
	DECLARE @currentNumberOfRows INT = 0
	DECLARE @testRunID INT = (SELECT IDENT_CURRENT('TestRuns'))
	DECLARE @currentStarTime DATETIME
	DECLARE @currentEndTime DATETIME


	DECLARE @testID INT = (
		SELECT testID
		FROM Tests
		WHERE Name = @test
	)

	-- cursor for tables
	-- scroll for all fetch options
	DECLARE TableCursor CURSOR SCROLL
	FOR
		SELECT  Tables.TableID, Tables.Name, TestTables.NoOfRows
		FROM TestTables INNER JOIN Tables ON TestTables.TableID = Tables.TableID
		WHERE TestTables.TestID = @testID
		ORDER BY TestTables.Position ASC


	-- cursor for views
	DECLARE ViewsCursor CURSOR
	FOR 
		SELECT Views.ViewID, Views.Name
		FROM TestViews INNER JOIN Views ON TestViews.ViewID = Views.ViewID
		WHERE TestViews.TestID = @testID

	-- store the start time
	DECLARE @startTime DATETIME = SYSDATETIME()

	
	-- open the table cursor
	OPEN TableCursor

	FETCH FIRST
	FROM TableCursor
	INTO @currentID, @currentName, @currentNumberOfRows

	WHILE @@FETCH_STATUS = 0 BEGIN
		SET @query = 'DELETE FROM ' + @currentName
		EXEC (@query)

		FETCH
		FROM TableCursor
		INTO @currentID, @currentName, @currentNumberOfRows
	END

	-- reset the cursor
	CLOSE TableCursor
	OPEN TableCursor

	FETCH LAST
	FROM TableCursor
	INTO @currentID, @currentName, @currentNumberOfRows

	WHILE @@FETCH_STATUS = 0 BEGIN
		
		SET @currentStarTime = SYSDATETIME()
		
		SET @query = 'PopulateTable ' + '''' + @currentName + ''', ' + CONVERT(VARCHAR(MAX), @currentNumberOfRows)
		EXEC (@query)

		SET @currentEndTime = SYSDATETIME()

		INSERT INTO TestRunTables(TestRunID, TableID, StartAt, EndAt)
		VALUES (@testRunID, @currentID, @currentStarTime, @currentEndTime)

		PRINT ('Test ID: ' + CONVERT(VARCHAR(MAX), @currentID) + ' - Started at: ' + CONVERT(VARCHAR(MAX), @currentStarTime) + ' - Ended at: ' + CONVERT(VARCHAR(MAX), @currentEndTime))


		FETCH PRIOR
		FROM TableCursor
		INTO @currentID, @currentName, @currentNumberOfRows
	END

	-- close and deallocate the table cursor
	CLOSE TableCursor
	DEALLOCATE TableCursor

	-- open the views cursor
	OPEN ViewsCursor

	FETCH 
	FROM ViewsCursor
	INTO @currentID, @currentName

	WHILE @@FETCH_STATUS = 0 BEGIN
		
		SET @currentStarTime = SYSDATETIME()
		
		SET @query = 'SELECT * FROM ' + @currentName
		EXEC (@query)

		SET @currentEndTime = SYSDATETIME()

		INSERT INTO TestRunViews(TestRunID, ViewID, StartAt, EndAt)
		VALUES (@testRunID, @currentID, @currentStarTime, @currentEndTime)

		PRINT ('View ID: ' + CONVERT(VARCHAR(MAX), @currentID) + ' - Started at: ' + CONVERT(VARCHAR(MAX), @currentStarTime) + ' - Ended at: ' + CONVERT(VARCHAR(MAX), @currentEndTime))
		
		FETCH 
		FROM ViewsCursor
		INTO @currentID, @currentName
	END

	-- close and deallocate the view cursor
	CLOSE ViewsCursor
	DEALLOCATE ViewsCursor


	-- set the end time for all tests

	DECLARE @endTime DATETIME = SYSDATETIME()

	UPDATE TestRuns
	SET StartAt = @startTime, EndAt = @endTime
	WHERE TestRunID = @testRunID

	PRINT('Finished all tests. Started at: ' + CONVERT(VARCHAR(MAX), @startTime) + '  -  Ended at: ' + CONVERT(VARCHAR(MAX), @endTime))
END
GO
