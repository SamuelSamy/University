--- c. 2 queries with the difference operation; use EXCEPT and NOT IN;


-- select the students that are not assigned to a roon
SELECT * 
FROM Students
WHERE ID IN (
	SELECT ID
	FROM Students
	EXCEPT
	SELECT SID
	FROM Students_Rooms
)


-- select the students that never had a visitor
SELECT *
FROM Students
WHERE ID NOT IN (
	SELECT SID
	FROM Visits
)


-- select the staff members that never gave an advertisment
SELECT *
FROM Staff
WHERE ID NOT IN (
	SELECT staff_id
	FROM Advertisments
)