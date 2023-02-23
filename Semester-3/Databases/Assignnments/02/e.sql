-- e. 2 queries with the IN operator and a subquery in the WHERE clause; 
-- in at least one case, the subquery must include a subquery in its own WHERE clause;

-- select all empty rooms
SELECT *
From Rooms
WHERE ID NOT IN (
	SELECT RID
	FROM Students_Rooms
)


-- select all staff that gave an advertisment to students that still have a contract 'active'
SELECT *
FROM Staff
WHERE ID IN (
	SELECT DISTINCT staff_id
	FROM Advertisments
	WHERE CID IN (
		SELECT ID
		FROM Contracts
		WHERE end_date IS NULL
	)
)