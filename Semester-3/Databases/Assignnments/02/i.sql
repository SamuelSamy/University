-- i. 4 queries using ANY and ALL to introduce a subquery in the WHERE clause (2 queries per operator); 
-- rewrite 2 of them with aggregation operators, and the other 2 with IN / [NOT] IN.

-- select all students that had at least one visitor
SELECT *
FROM Students
WHERE Students.ID = ANY(
	SELECT DISTINCT Visits.SID
	FROM Visits
)

-- rewritten
SELECT *
FROM Students
WHERE Students.ID IN (
	SELECT DISTINCT Visits.SID
	FROM Visits
)



-- select all staff that are either security or manager
SELECT *
FROM Staff
WHERE Staff.ID = ANY(
	Select ID
	FROM Staff
	WHERE post = 'Security' or post = 'Manager'
)
ORDER BY post



-- rewritten
SELECT * 
FROM Staff
WHERE post IN ('Security', 'Manager')
ORDER BY post


-- select the oldest student(s)
SELECT Students.name, DATEDIFF(year, Students.date_of_birth, GETDATE()) AS Age
FROM Students
WHERE DATEDIFF(year, Students.date_of_birth, GETDATE()) >= ALL(
	SELECT DATEDIFF(year, S.date_of_birth, GETDATE())
	FROM Students S
)


-- rewritten
SELECT Students.name, DATEDIFF(year, Students.date_of_birth, GETDATE()) AS Age
FROM Students
WHERE DATEDIFF(year, Students.date_of_birth, GETDATE()) = (
	SELECT MAX(DATEDIFF(year, S.date_of_birth, GETDATE()))
	FROM Students S
)


-- show the newest furniture bought that is in use
SELECT *
FROM Furniture INNER JOIN Rooms_Furniture ON Furniture.ID = Rooms_Furniture.FID
WHERE Rooms_Furniture.end_date IS NULL AND DATEDIFF(year, Furniture.buy_date, GETDATE()) >= ALL(
	SELECT DATEDIFF(year, Furniture.buy_date, GETDATE())
	FROM Furniture
)

-- rewritten
SELECT *
FROM Furniture INNER JOIN Rooms_Furniture ON Furniture.ID = Rooms_Furniture.FID
WHERE Rooms_Furniture.end_date IS NULL AND DATEDIFF(year, Furniture.buy_date, GETDATE()) = (
	SELECT MAX(DATEDIFF(year, Furniture.buy_date, GETDATE()))
	FROM Furniture
)