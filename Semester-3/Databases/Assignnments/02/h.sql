-- h. 4 queries with the GROUP BY clause, 3 of which also contain the HAVING clause; 
-- 2 of the latter will also have a subquery in the HAVING clause; 
-- use the aggregation operators: COUNT, SUM, AVG, MIN, MAX;


-- select the total amount paid by each student that is greater than 500; and the amount to restore (10% of the total amount)
SELECT Payments.SID, SUM(Payments.amount) AS 'Total amount paid', (SUM(Payments.amount) / 10) AS 'Amount to restore'
FROM Payments
GROUP BY Payments.SID
HAVING SUM(Payments.amount) >= 500
ORDER BY SUM(Payments.amount) DESC

-- select top 25% from from contracts ordered by the amount of contracts
SELECT TOP 25 PERCENT Contracts.SID, COUNT(Contracts.SID) AS 'Total Contracts'
FROM Contracts
GROUP BY Contracts.SID
ORDER BY COUNT(Contracts.SID) DESC


-- select all payments where the minimum paid amount is less than the average paid amount
SELECT Payments.SID, MIN(Payments.amount) AS 'Minimum amount paid', SUM(Payments.amount) AS 'Total amount paid', SUM(Payments.amount) - MIN(Payments.amount) AS 'Amount after restore'
FROM Payments
GROUP BY Payments.SID
HAVING MIN(Payments.amount) < (
	SELECT AVG(Payments.amount)
	FROM Payments
)
ORDER BY SUM(Payments.amount) DESC


-- select all visistors that visited more than '1% of count(all_visits)' times
SELECT Visits.SID, COUNT(Visits.SID) AS 'Total Visits',  (
	SELECT COUNT(Visits.SID)
	FROM Visits
) / 100 AS '% Visits'
FROM Visits
GROUP BY Visits.SID
HAVING  COUNT(Visits.SID) > (
	SELECT COUNT(Visits.SID)
	FROM Visits
) / 100 
ORDER BY COUNT(Visits.SID) DESC


