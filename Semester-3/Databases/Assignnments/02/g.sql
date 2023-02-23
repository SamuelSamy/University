-- g. 2 queries with a subquery in the FROM clause;                         


-- select top 10% of the staff that are not managers (ordered by salary)
SELECT TOP 10 PERCENT *
FROM (
	SELECT *
	FROM Staff
	WHERE post != 'Manager'
) AS T
ORDER BY T.salary DESC


-- select the top 500 students ordered by age
SELECT TOP 500 T.name, T.Age
FROM (
	SELECT Students.name, DATEDIFF(year, Students.date_of_birth, GETDATE()) as Age
	FROM Students
) AS T
ORDER BY Age DESC