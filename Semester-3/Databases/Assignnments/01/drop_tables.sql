ALTER TABLE Advertisments
DROP CONSTRAINT Advertisments_CID, Advertisments_Staff

ALTER TABLE Contracts
DROP CONSTRAINT Contracts_SID

ALTER TABLE Payments
DROP CONSTRAINT Payments_SID

ALTER TABLE Rooms
DROP CONSTRAINT Rooms_BID

ALTER TABLE Rooms_Furniture
DROP CONSTRAINT Rooms_Furniture_FID, Rooms_Furniture_RID

ALTER TABLE Students_Rooms
DROP CONSTRAINT Students_Rooms_RID, Students_Rooms_SID

ALTER TABLE Visits
DROP CONSTRAINT Visits_SID, Visits_VID

DROP TABLE Buildings
DROP TABLE Furniture
DROP TABLE Payments
DROP TABLE Rooms
DROP TABLE Rooms_Furniture
DROP TABLE Students
DROP TABLE Students_Rooms
DROP TABLE Visitors
DROP TABLE Staff
DROP TABLE Expenses
DROP TABLE Visits
DROP TABLE Advertisments
DROP TABLE Contracts