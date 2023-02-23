-- f. 2 queries with the EXISTS operator and a subquery in the WHERE clause;

-- all furniture that is not assigned to a room
SELECT *
FROM Furniture
WHERE NOT EXISTS (
	SELECT Rooms.ID
	FROM Rooms
	WHERE Rooms.ID IN (
		SELECT Rooms_Furniture.RID
		FROM Rooms_Furniture
		WHERE Furniture.ID = Rooms_Furniture.FID
	)
)

-- all students that are not assigned to at least one contract
SELECT *
FROM Students
WHERE NOT EXISTS (
	Select DISTINCT Contracts.SID
	FROM Contracts
	WHERE Contracts.SID = Students.ID
)