-- insert data – for at least 4 tables; at least one statement must violate referential integrity constraints;

-- insert that violates referential integrity constraint
INSERT INTO Contracts (SID, start_date, end_date)
VALUES (3000, '2020-09-29', NULL)


-- {AND, OR, NOT}		+
-- {<,<=,=,>,>=,<>}     +
-- IS [NOT] NULL		+
-- IN					+
-- BETWEEN				+
-- LIKE					+
-- update data – for at least 3 tables;

-- contracts


SELECT *
FROM CONTRACTS
WHERE end_date is NULL AND SID IN (
	SELECT ID
	FROM Students
	WHERE name LIKE 'Ema%mi%'
)


-- end the contract for all students named like 'Ema%mi%'
UPDATE Contracts
SET end_date = GETDATE()
WHERE end_date IS NULL AND SID IN (
	SELECT ID
	FROM Students
	WHERE name LIKE 'Ema%mi%'
)

-- undo for above
UPDATE Contracts
SET end_date = NULL
WHERE SID IN (
	SELECT ID
	FROM Students
	WHERE name LIKE 'Ema%mi%'
)


-- students

SELECT *
FROM Students
WHERE cnp = '5913885895396'

-- update the phone_number for a students
UPDATE Students
SET phone_number = '0712341234'
WHERE cnp = '5913885895396'

-- undo
UPDATE Students
SET phone_number = '0780828555'
WHERE cnp = '5913885895396'


-- rooms - furniture

SELECT *
FROM Rooms_Furniture
WHERE RID IN (
	SELECT ID
	FROM Rooms
	WHERE BID = 1 AND floor between 2 AND 4
) AND FID IN (
	SELECT ID
	FROM Furniture 
	WHERE type = 'Chair'
)

-- remove all chairs from: building with id = 1 and floor between 2 and 4
UPDATE Rooms_Furniture
SET end_date = GETDATE()
WHERE RID IN (
	SELECT ID
	FROM Rooms
	WHERE BID = 1 AND floor between 2 AND 4
) AND FID IN (
	SELECT ID
	FROM Furniture 
	WHERE type = 'Chair'
)

-- undo for above
UPDATE Rooms_Furniture
SET end_date = NULL
WHERE RID IN (
	SELECT ID
	FROM Rooms
	WHERE BID = 1 AND floor >= 2 AND floor <= 4
) AND FID IN (
	SELECT ID
	FROM Furniture 
	WHERE type = 'Chair'
)




-- delete data – for at least 2 tables.

-- contracts
SELECT *
FROM Contracts
WHERE end_date is NULL


-- delete all ended contracts
DELETE
FROM Contracts
WHERE end_date IS NOT NULL


-- payments
SELECT *
FROM Payments
WHERE paid = 0 AND SID IN (
	SELECT ID
	FROM Students
	WHERE ID IN (
		SELECT ID
		FROM Rooms
		WHERE BID = 2
	)
)


-- delete all payment that were not paid yet in bulding with id = 2
DELETE 
FROM Payments
WHERE paid = 0 AND SID IN (
	SELECT ID
	FROM Students
	WHERE ID IN (
		SELECT ID
		FROM Rooms
		WHERE BID = 2
	)
)
