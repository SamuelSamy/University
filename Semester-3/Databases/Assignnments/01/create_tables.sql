CREATE TABLE Students (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	name varchar(128) NOT NULL,
	cnp varchar(16) NOT NULL,	
	date_of_birth DATE NOT NULL,
	country varchar(64) NOT NULL,
	county varchar(64) NOT NULL,
	city varchar(64) NOT NULL,
	phone_number varchar(16) NOT NULL,
	mail varchar(64) NOT NULL,
	faculty varchar(128) NOT NULL
)


CREATE TABLE Buildings (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	number_of_floors int NOT NULL,
	number_of_rooms int NOT NULL,
	location varchar(128) NOT NULL,
	fee int NOT NULL
)


CREATE TABLE Rooms (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	BID int NOT NULL,
	number int NOT NULL,
	floor int NOT NULL,
	capacity int NOT NULL,
	CONSTRAINT Rooms_BID FOREIGN KEY (BID) REFERENCES Buildings(ID) ON DELETE CASCADE
)

CREATE TABLE Students_Rooms (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	SID int NOT NULL,
	RID int NOT NULL,
	start_date DATE NOT NULL,
	end_date DATE,
	CONSTRAINT Students_Rooms_SID FOREIGN KEY (SID) REFERENCES Students(ID) ON DELETE CASCADE,
	CONSTRAINT Students_Rooms_RID FOREIGN KEY (RID) REFERENCES Rooms(ID) ON DELETE CASCADE
)

CREATE TABLE Furniture (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	type varchar(32) NOT NULL,
	buy_date DATE NOT NULL
)


CREATE TABLE Rooms_Furniture (
	RID int NOT NULL,
	FID int NOT NULL,
	start_date DATE NOT NULL,
	end_date DATE,
	CONSTRAINT Rooms_Furniture_RID FOREIGN KEY (RID) REFERENCES Rooms(ID) ON DELETE CASCADE,
	CONSTRAINT Rooms_Furniture_FID FOREIGN KEY (FID) REFERENCES Furniture(ID) ON DELETE CASCADE
)


CREATE TABLE Payments (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	SID int NOT NULL,
	amount int NOT NULL,
	due DATE NOT NULL,
	paid bit,
	CONSTRAINT Payments_SID FOREIGN KEY (SID) REFERENCES Students(ID) ON DELETE CASCADE
)


CREATE TABLE Visitors (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	name varchar(128),
	cnp varchar(16),
	phone_number varchar(16),
	mail varchar(128)
)

CREATE TABLE Visits (
	SID int NOT NULL,
	VID int NOT NULL,
	start_time datetime NOT NULL,
	end_time datetime,
	PRIMARY KEY(SID, VID, start_time, end_time),
	CONSTRAINT Visits_SID FOREIGN KEY (SID) REFERENCES Students(ID) ON DELETE CASCADE,
	CONSTRAINT Visits_VID FOREIGN KEY (VID) REFERENCES Visitors(ID) ON DELETE CASCADE

)

CREATE TABLE Staff (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	name varchar(128) NOT NULL,
	phone_number varchar(16) NOT NULL,
	mail varchar(128) NOT NULL,
	date_of_birth DATE NOT NULL,
	post varchar(128) NOT NULL,
	salary int NOT NULL
)	


CREATE TABLE Expenses (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	month varchar(16) NOT NULL,
	type varchar(32) NOT NULL,
	value float(2) NOT NULL
)

CREATE TABLE Contracts (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	SID int NOT NULL,
	start_date DATE NOT NULL,
	end_date DATE DEFAULT NULL,
	CONSTRAINT Contracts_SID FOREIGN KEY (SID) REFERENCES Students(ID) ON DELETE CASCADE
)

CREATE TABLE Advertisments (
	ID int NOT NULL IDENTITY(1,1) PRIMARY KEY,
	CID int NOT NULL,
	staff_id int NOT NULL,
	reason varchar(1024) DEFAULT NULL,
	date DATE NOT NULL,
	CONSTRAINT Advertisments_CID FOREIGN KEY (CID) REFERENCES Contracts(ID) ON DELETE CASCADE,
	CONSTRAINT Advertisments_Staff FOREIGN KEY (staff_id) REFERENCES Staff(ID) ON DELETE CASCADE
)