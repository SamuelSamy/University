CREATE OR ALTER PROCEDURE insertTa (@rows INT) 
AS
BEGIN
	SET NOCOUNT ON;
	DECLARE @a2 INT = (SELECT MAX(a2) FROM Ta)
	IF @a2 IS NULL BEGIN
		SET @a2 = 0
	END

	SET @a2 = @a2 + 1

	WHILE @rows > 0 BEGIN
		INSERT INTO Ta (a2, number, text)
		VALUES (@a2, (SELECT dbo.randInt(0, 9999)), (SELECT dbo.randStr(16)))

		SET @a2 = @a2 + 1
		SET @rows = @rows - 1
	END
END	
GO

-- delete data
DELETE FROM Ta
DELETE FROM Tb
DELETE FROM Tc

-- select data
SELECT * FROM Ta
SELECT * FROM Tb
SELECT * FROM Tc

-- populate tables
EXEC insertTa @rows = 20000
EXEC PopulateTable @table = 'Tb', @numberOfRows = 20000
EXEC PopulateTable @table = 'Tc', @numberOfRows = 1000

-- a. Write queries on Ta such that their execution plans contain the following operators

-- clustered index scan;
SELECT *
FROM Ta

-- clustered index seek;
SELECT *
FROM Ta
WHERE aid = 3000

-- nonclustered index scan;
CREATE NONCLUSTERED INDEX TaIndexNumber ON Ta(number)
DROP INDEX TaIndexNumber ON Ta

SELECT number
FROM Ta

-- nonclustered index seek;
SELECT a2
FROM Ta
WHERE a2 = 24

-- key lookup.
SELECT *
FROM Ta
WHERE a2 = 24


-- b. Write a query on table Tb with a WHERE clause of the form WHERE b2 = value and analyze its execution plan.
-- without index: 0.1926
-- with index: 0.00328

SELECT b2
FROM Tb
WHERE b2 = 389918

-- Create a nonclustered index that can speed up the query. Examine the execution plan again.
CREATE NONCLUSTERED INDEX TbIndex ON Tb(b2)
DROP INDEX TbIndex ON Tb



-- c. Create a view that joins at least 2 tables. Check whether existing indexes are helpful; if not, reassess existing indexes / examine the cardinality of the tables.
-- without index: 0.4651
-- with index: 0.4197

CREATE NONCLUSTERED INDEX TaIndexNumber ON Ta(number)
CREATE NONCLUSTERED INDEX TbIndex ON Tb(b2)

DROP INDEX TaIndexNumber ON Ta
DROP INDEX TbIndex ON Tb

-- create view
GO
CREATE OR ALTER VIEW View1 
AS
	SELECT Ta.number, Tb.b2
	FROM Tc INNER JOIN Ta ON Ta.aid = Tc.aid 
			INNER JOIN Tb ON Tb.bid = Tc.bid
	WHERE Tb.b2 > 100000 AND 
		  Ta.number < 50000
GO


-- select
SELECT * 
FROM View1
