-- a. 2 queries with the union operation; use UNION [ALL] and OR;


-- select all students that were born before 1993-01-01 or after 2002-01-01
SELECT *
FROM Students
WHERE date_of_birth < CAST('19930101' AS DATE)
UNION
SELECT *
FROM Students
WHERE date_of_birth >= CAST('20020101' AS DATE)
ORDER BY date_of_birth

-- same as above
SELECT *
FROM Students
WHERE date_of_birth < CAST('19930101' AS DATE) OR date_of_birth >= CAST('20020101' AS DATE)
ORDER BY date_of_birth

-- select all students that are from 'Cluj' or 'Bitrita - Nasaud' or are born after 2002-01-01
SELECT *
FROM Students
WHERE county = 'Cluj' OR county = 'Bistrita - Nasaud'
UNION
SELECT *
FROM Students
WHERE date_of_birth >= CAST('20020101' AS DATE)
ORDER BY date_of_birth


-- same as above
SELECT *
FROM Students
WHERE county = 'Cluj' OR county = 'Bistrita - Nasaud' OR (date_of_birth >= CAST('20020101' AS DATE))
ORDER BY county
