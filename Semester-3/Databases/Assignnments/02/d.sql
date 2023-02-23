-- d. 4 queries with INNER JOIN, LEFT JOIN, RIGHT JOIN, and FULL JOIN (one query per operator); 
-- one query will join at least 3 tables, while another one will join at least two many-to-many relationships;


-- select the students that had at least one visitor and the visit's data
SELECT *
FROM Students INNER JOIN Visits ON Students.ID = Visits.SID


-- select all the furniture (and data from Rooms_Furniture) ordered by buy_date
SELECT *
FROM Furniture LEFT JOIN Rooms_Furniture ON Furniture.ID = Rooms_Furniture.FID
ORDER BY buy_date


-- select student's data, contract's data, advertisment's data and staff's data for all the advertisments AND the staff that never gave an advertisment
SELECT *
FROM Students
	RIGHT JOIN Contracts on Students.ID = Contracts.SID
	RIGHT JOIN Advertisments on Advertisments.CID = Contracts.ID
	RIGHT JOIN Staff on Staff.ID = Advertisments.staff_id
ORDER BY Students.ID DESC


-- select data about: students, visits, visitors and rooms  (2 n:m)
SELECT *
FROM Students
	FULL JOIN Visits ON Students.ID = Visits.SID
	FULL JOIN Visitors ON Visitors.ID = Visits.VID
	FULL JOIN Students_Rooms ON Students.ID = Students_Rooms.SID
	FULL JOIN Rooms ON Rooms.ID = Students_Rooms.RID
