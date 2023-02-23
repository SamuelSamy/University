--a table with a single-column primary key and no foreign keys;  -> Buildings
--a table with a single-column primary key and at least one foreign key; -> Rooms
--a table with a multicolumn primary key; -> Visits

--a view with a SELECT statement operating on one table; -> StudentsView
--a view with a SELECT statement that operates on at least 2 different tables and contains at least one JOIN operator; -> PaymentsView
--a view with a SELECT statement that has a GROUP BY clause, operates on at least 2 different tables and contains at least one JOIN operator. -> VistsView


-- a view with a SELECT statement operating on one table;
CREATE OR ALTER VIEW StudentsView
AS
	SELECT Students.ID, Students.Name, Students.date_of_birth
	FROM Students
	WHERE Students.date_of_birth < '2000-01-01'
GO

	
--a view with a SELECT statement that operates on at least 2 different tables and contains at least one JOIN operator;
CREATE OR ALTER VIEW PaymentsView
AS
	SELECT Students.ID, Students.Name, Payments.amount
	FROM Payments INNER JOIN Students ON Students.ID = Payments.SID
	WHERE amount < 1000
GO


--a view with a SELECT statement that has a GROUP BY clause, operates on at least 2 different tables and contains at least one JOIN operator.
CREATE OR ALTER VIEW VisitsView
AS
	SELECT Visits.SID, count(*) as 'Total Visitors'
	FROM Visits INNER JOIN Students ON Students.ID = Visits.SID
	GROUP BY Visits.SID
GO



-- add tables
EXEC AddTable @table = 'Buildings'
EXEC AddTable @table = 'Rooms'
EXEC AddTable @table = 'Visits'

-- add views
EXEC AddView @view = 'StudentsView'
EXEC AddView @view = 'PaymentsView'
EXEC AddView @view = 'VisitsView'

-- add tests
EXEC AddTest @test = 'Test1'
EXEC AddTest @test = 'Test2'
EXEC AddTest @test = 'Test3'

-- create data for 'Test1'
EXEC AddTableTest @table = 'Rooms', @test = 'Test1', @numberOfRows = 200, @position = 0
EXEC AddTableTest @table = 'Buildings', @test = 'Test1', @numberOfRows = 150, @position = 1
EXEC AddTableTest @table = 'Visits', @test = 'Test1', @numberOfRows = 100, @position = 2

-- create data for 'Test2'
EXEC AddViewTest @view = 'StudentsView', @test = 'Test2'
EXEC AddViewTest @view = 'PaymentsView', @test = 'Test2'
EXEC AddViewTest @view = 'VisitsView', @test = 'Test2'

-- create data for 'Test3
EXEC AddTableTest @table = 'Rooms', @test = 'Test3', @numberOfRows = 2000, @position = 0
EXEC AddTableTest @table = 'Buildings', @test = 'Test3', @numberOfRows = 1000, @position = 1
EXEC AddTableTest @table = 'Visits', @test = 'Test3', @numberOfRows = 3000, @position = 2
EXEC AddViewTest @view = 'StudentsView', @test = 'Test3'
EXEC AddViewTest @view = 'PaymentsView', @test = 'Test3'
EXEC AddViewTest @view = 'VisitsView', @test = 'Test3'

-- restart all tables
DELETE FROM TestTables
DELETE FROM TestViews
DELETE FROM TestRuns
DELETE FROM TestRunTables
DELETE FROM TestRunViews
DELETE FROM Tables
DELETE FROM Views
DELETE FROM Tests



-- reset runs tables
DELETE FROM TestRuns
DELETE FROM TestRunTables
DELETE FROM TestRunViews

-- selects
SELECT * FROM TestRuns
SELECT * FROM TestRunTables
SELECT * FROM TestRunViews

-- data
SELECT * FROM Buildings
SELECT * FROM Rooms
SELECT * FROM Visits

-- main
EXEC RunTest 'Test3'

