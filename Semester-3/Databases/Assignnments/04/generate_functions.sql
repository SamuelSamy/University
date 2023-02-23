CREATE OR ALTER VIEW randNumber AS
	SELECT RAND() AS number

GO

CREATE OR ALTER VIEW randShortStr AS
	SELECT NEWID() AS string

GO

-- max
CREATE OR ALTER FUNCTION maxNumber (@number1 INT, @number2 INT)
RETURNS INT
AS
BEGIN
	IF @number1 > @number2 BEGIN
		RETURN @number2
	END

	RETURN @number1
END

GO

-- random integer
CREATE OR ALTER FUNCTION randInt (@start INT, @stop INT)
RETURNS INT
AS
BEGIN
	RETURN FLOOR(((SELECT number FROM randNumber) * (@stop - @start)) + @start)
END

GO

-- random string
CREATE OR ALTER FUNCTION randStr (@len INT = -1)
RETURNS VARCHAR(MAX) 
AS
BEGIN
	IF @len = -1 BEGIN
		SET @len = dbo.randInt(4, 64)
	END

	DECLARE @string VARCHAR(MAX)
	SET @string = ''

	WHILE LEN(@string) < @len BEGIN
		SET @string = @string + CONVERT(VARCHAR(MAX), (SELECT string FROM randShortStr))
	END	

	RETURN LEFT(@string, @len)
END


GO 

-- random date between 01 Jan 1900 and 06 Jun 2079
CREATE OR ALTER FUNCTION randDate ()
RETURNS DATE
AS 
BEGIN
	RETURN DATEADD(DAY, (ABS(CHECKSUM((SELECT string FROM randShortStr))) % 65530), 0)
END
GO

-- 
-- random datetime between 01 Jan 1900 and 06 Jun 2079
CREATE OR ALTER FUNCTION randDateTime ()
RETURNS DATETIME
AS 
BEGIN
	RETURN DATEADD(MINUTE, (ABS(CHECKSUM((SELECT string FROM randShortStr))) % (65530 * 24 * 60)), 0)
END
GO


-- primary key
CREATE OR ALTER FUNCTION IsPrimaryKey (@table VARCHAR(128), @column VARCHAR(128))
RETURNS INT
AS
BEGIN
	DECLARE @counter INT = 0
	SET @counter = (
		SELECT count(*)
		FROM     INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS C
			JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE  AS K ON C.TABLE_NAME = K.TABLE_NAME
																 AND C.CONSTRAINT_CATALOG = K.CONSTRAINT_CATALOG
																 AND C.CONSTRAINT_SCHEMA = K.CONSTRAINT_SCHEMA
																 AND C.CONSTRAINT_NAME = K.CONSTRAINT_NAME
		WHERE   C.CONSTRAINT_TYPE = 'PRIMARY KEY'
				AND K.COLUMN_NAME = @column
				AND C.TABLE_NAME = @table
	)

	IF @counter = 0 BEGIN
		RETURN 0
	END

	RETURN 1
END


GO 


--SELECT 
--	OBJECT_NAME(F.parent_object_id) AS 'Referencing/Child Table',
--	COL_NAME(FC.parent_object_id, FC.parent_column_id) AS 'Referencing/Child Column',
--	OBJECT_NAME(FC.referenced_object_id) AS 'Referenced/Parent Table',
--	COL_NAME(FC.referenced_object_id, FC.referenced_column_id) AS 'Referenced/Parent Column'
--FROM sys.foreign_keys AS F
--INNER JOIN sys.foreign_key_columns AS FC ON F.OBJECT_ID = FC.constraint_object_id
--WHERE OBJECT_NAME (F.parent_object_id) = 'TestTable2' AND COL_NAME(FC.parent_object_id, FC.parent_column_id) = 'number1'

-- foreign key
CREATE OR ALTER PROCEDURE GetReferenceData (@table VARCHAR(128), @column VARCHAR(128), @referencedTable VARCHAR(128) OUTPUT, @referencedColumn VARCHAR(128) OUTPUT)
AS
BEGIN
	SELECT @referencedTable = OBJECT_NAME(FC.referenced_object_id), @referencedColumn = COL_NAME(FC.referenced_object_id, FC.referenced_column_id)
	FROM sys.foreign_keys AS F INNER JOIN sys.foreign_key_columns AS FC ON F.OBJECT_ID = FC.constraint_object_id
	WHERE OBJECT_NAME (F.parent_object_id) = @table AND COL_NAME(FC.parent_object_id, FC.parent_column_id) = @column
END
GO

-- identity column
CREATE OR ALTER PROCEDURE GetIdentityColumn (@table VARCHAR(128), @column VARCHAR(128) OUTPUT)
AS
BEGIN
	SELECT @column = NAME
	FROM sys.identity_columns
	WHERE OBJECT_NAME(OBJECT_ID) = @table
END
GO

-- unique columns (except primary keys)
CREATE OR ALTER PROCEDURE GetUniqueColumns (@table VARCHAR(128))
AS
BEGIN
	SELECT COL_NAME(Indexes.object_id, IndexColumns.column_id) AS _Column
	FROM sys.index_columns AS IndexColumns
		INNER JOIN sys.indexes AS Indexes ON Indexes.object_id = IndexColumns.object_id AND Indexes.index_id = IndexColumns.index_id
	WHERE 
		OBJECT_NAME(Indexes.object_id) = @table AND
		Indexes.is_unique = 1 AND
		Indexes.is_primary_key = 0
END
GO

-- insert random data into a table (max 1000 rows)
CREATE OR ALTER PROCEDURE InsertRandomRows (@table VARCHAR(128), @rows INT)
AS
BEGIN
	-- do not ouput number of rows affected
	SET NOCOUNT ON;

	IF @rows > 1000 BEGIN
		RAISERROR('You can not insert more than 1000 rows at a time', 18, 1)
		RETURN
	END

	IF @rows < 1 BEGIN 
		RAISERROR('You must insert at least one row', 18, 1)
		RETURN
	END

	DECLARE @referencedTable VARCHAR(MAX) = ''
	DECLARE @referencedColumn VARCHAR(MAX) = ''
	
	DECLARE @columns VARCHAR(MAX) = ''
	DECLARE @values VARCHAR(MAX) = ''

	DECLARE @columnName VARCHAR(MAX) = ''
	DECLARE @dataType VARCHAR(MAX) = ''
	DECLARE @maxLen INT = 0

	DECLARE @currentRow INT = 0

	WHILE @currentRow < @rows BEGIN

		-- declare the cursor
		DECLARE columns_cursor CURSOR 
		FOR
			SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH
			FROM INFORMATION_SCHEMA.COLUMNS
			WHERE TABLE_NAME = @table

		-- open cursor
		OPEN columns_cursor

		-- fetch
		FETCH 
		FROM columns_cursor
		INTO @columnName, @dataType, @maxLen

		SET @values = @values + '('

		-- identity column
		DECLARE @identityColumn VARCHAR(MAX)
		EXEC GetIdentityColumn @table, @identityColumn OUTPUT
		-- read
		WHILE @@FETCH_STATUS = 0 BEGIN

			-- make sure the variables are empty
			SET @referencedTable = NULL
			SET @referencedColumn = NULL

			-- ignore it if it's a primary key AND it has identity
			IF (SELECT dbo.IsPrimaryKey(@table, @columnName)) = 1 AND (@identityColumn = @columnName) BEGIN
				GOTO FETCH_DATA;
			END

			-- add the column to the string statement (if it's the first row)
			IF @currentRow = 0 BEGIN
				SET @columns = @columns + @columnName + ', '
			END

			-- check if it's a foreign key
			EXEC GetReferenceData @table, @columnName, @referencedTable OUTPUT, @referencedColumn OUTPUT
			IF @referencedTable IS NOT NULL BEGIN
				-- it is a foreign key
				DECLARE @foreignValue VARCHAR(MAX)
				DECLARE @query NVARCHAR(MAX)
				
				SET @query = N'SELECT TOP 1 @foreignValue = ' + CONVERT(NVARCHAR(MAX),  @referencedColumn) + N' FROM ' + CONVERT(NVARCHAR(MAX),  @referencedTable) + N' ORDER BY NEWID()'
				EXEC sp_executesql @query, N'@foreignValue VARCHAR(MAX) OUTPUT', @foreignValue OUTPUT
				SET @values = @values + @foreignValue + ', '
			
				GOTO FETCH_DATA;
			END

			

			-- it is neither a primary key nor a foreign key
			IF @dataType = 'int' BEGIN
				SET @values = @values + CONVERT(VARCHAR(MAX), dbo.randInt(0, 999999)) + ', '
			END

			IF @dataType = 'varchar' BEGIN
				SET @values = @values + '''' + dbo.randStr(@maxLen) + ''', '
			END


			IF @dataType = 'datetime' BEGIN
				SET @values = @values + '''' + CONVERT(VARCHAR(MAX), dbo.randDateTime(), 120) + ''', '
			END

			IF @dataType = 'date' BEGIN
				SET @values = @values + '''' + CONVERT(VARCHAR(MAX), dbo.randDate(), 111) + ''', '
			END

			FETCH_DATA:

			FETCH
			FROM columns_cursor
			INTO @columnName, @dataType, @maxLen
		END

		-- close and deallocate
		CLOSE columns_cursor
		DEALLOCATE columns_cursor

		IF LEN(@values) > 0 BEGIN
			SET @values = (SUBSTRING(@values, 1, LEN(@values) - 1)) + '), '
		END ELSE BEGIN
			SET @values = '(), '
		END

		SET @currentRow = @currentRow + 1

	END
	SET @columns = (SUBSTRING(@columns, 1, LEN(@columns) - 1))
	SET @values = (SUBSTRING(@values, 1, LEN(@values) - 1))

	EXEC ('INSERT INTO ' + @table + '(' + @columns + ') VALUES ' + @values) 
END
GO


-- populate a table max(1000) rows of a time
CREATE OR ALTER PROCEDURE PopulateTable (@table VARCHAR(128), @numberOfRows INT)
AS
BEGIN
	SET NOCOUNT ON;

	IF @numberOfRows < 0 BEGIN
		RAISERROR('You must insert at least 1 row', 18, 1)
		RETURN
	END

	DECLARE @currentRow INT

	WHILE @numberOfRows > 0 BEGIN
		DECLARE @rrows INT = (SELECT dbo.maxNumber(@numberOfRows, 1000))
		EXEC InsertRandomRows @table = @table, @rows = @rrows
		SET @numberOfRows = @numberOfRows - 1000
	END
END
GO

--DROP TABLE TestTable2

--CREATE TABLE TestTable2 (
--	ID INT PRIMARY KEY IDENTITY(1, 1),
--	number1 INT FOREIGN KEY REFERENCES Buildings(ID) ON DELETE CASCADE,
--	number2 INT,
--	date1 DATE,
--	datetime1 DATETIME
--)

--SELECT * FROM TestTable2

----EXEC InsertRandomRow @table = 'TestTable2'
--EXEC PopulateTable @table = 'TestTable2', @numberOfRows = 100



--DECLARE @v1 VARCHAR(MAX), @v2 VARCHAR(MAX)
--EXEC GetReferenceData 'TestTable2', 'number1', @v1, @v2
--SELECT @v1 AS 'Table', @V2 as 'Column'



-- https://stackoverflow.com/questions/31468836/use-rand-in-user-defined-function