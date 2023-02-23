
--- b. 2 queries with the intersection operation; use INTERSECT and IN;

-- select all staff that are 'Security' or 'Manager' AND are born after 2002-01-01
SELECT *
FROM Staff
WHERE post = 'Security' OR post = 'Manager' 
INTERSECT
SELECT *
FROM Staff
WHERE date_of_birth >= CAST('20020101' AS DATE)


-- select the visitors that visited someone named '%ema%' AND someone named '%cristian%'
SELECT *
FROM Visitors
WHERE ID IN (
	SELECT VID
	FROM Visits
	WHERE SID IN (
		SELECT ID
		FROM Students
		WHERE name LIKE '%ema%'
	)
)
INTERSECT
SELECT *
FROM Visitors
WHERE ID IN (
	SELECT VID
	FROM Visits
	WHERE SID IN (
		SELECT ID
		FROM Students
		WHERE name LIKE '%cristian%'
	)
)

-- select the visitors that visited someone named '%ema%' AND are girls (their CNP starts with 2)
SELECT *
FROM Visitors
WHERE ID IN (
	SELECT VID
	FROM Visits
	WHERE SID IN (
		SELECT ID
		FROM Students
		WHERE name LIKE '%ema%'
	)
)
INTERSECT
SELECT *
FROM Visitors
WHERE cnp LIKE '2%' AND ID IN (
	SELECT VID
	FROM Visits
)
