/*CREATE DATABASE "C:\Users\shishimorov_ds\Documents\Schedule.fdb" user 'SYSDBA' password 'masterkey' DEFAULT CHARACTER SET UTF8;*/
CREATE DATABASE "/var/lib/firebird/2.5/data/Schedule.fdb" user 'SYSDBA' password 'masterkey' DEFAULT CHARACTER SET UTF8;

CREATE TABLE Subjects
(
	ID integer NOT NULL,
	Name varchar(200) NOT NULL,
	CONSTRAINT PK_Subjects PRIMARY KEY(ID)
);

CREATE TABLE Subject_Types
(
	ID integer NOT NULL,
	Name varchar(50) NOT NULL,
	CONSTRAINT PK_Subject_Types PRIMARY KEY(ID)
);

CREATE TABLE Professors
(
	ID integer NOT NULL,
	Name varchar(100) NOT NULL,
	CONSTRAINT PK_Professors PRIMARY KEY(ID)
);

CREATE TABLE Lessons
(
	ID integer NOT NULL,
	"Index" integer NOT NULL,
	"Begin" time NOT NULL,
	"End" time NOT NULL,
	CONSTRAINT PK_Lessons PRIMARY KEY(ID),
	CONSTRAINT UNQ_Lesson_Index UNIQUE("Index")
);

CREATE TABLE Days
(
	ID integer NOT NULL,
	"Index" integer NOT NULL,
	Name varchar(50) NOT NULL,
	CONSTRAINT PK_Days PRIMARY KEY(ID),
	CONSTRAINT UNQ_Day_Index UNIQUE("Index")
);

CREATE TABLE Groups
(
	ID integer NOT NULL,
	Name varchar(50) NOT NULL,
	Group_Size integer NOT NULL,
	CONSTRAINT PK_Groups PRIMARY KEY(ID)
);

CREATE TABLE Rooms
(
	ID integer NOT NULL,
	Name varchar(50) NOT NULL,
	"Size" integer,
	CONSTRAINT PK_Rooms PRIMARY KEY(ID)
);

CREATE TABLE Professors_Subjects
(
	ID integer NOT NULL,
	Professor_ID integer NOT NULL,
	Subject_ID integer NOT NULL,
	CONSTRAINT PK_Professors_Subjects PRIMARY KEY(ID),
	CONSTRAINT FK_PS_Professor_ID FOREIGN KEY(Professor_ID)
		REFERENCES Professors(ID)
		ON DELETE CASCADE
		ON UPDATE CASCADE,
	CONSTRAINT FK_PS_Subject_ID FOREIGN KEY(Subject_ID) REFERENCES Subjects(ID) 
		ON DELETE CASCADE 
		ON UPDATE CASCADE
);

CREATE TABLE Subjects_Groups
(
	ID integer NOT NULL,
	Subject_ID integer NOT NULL,
	Group_ID integer NOT NULL,
	CONSTRAINT PK_Subjects_Groups PRIMARY KEY(ID),
	CONSTRAINT FK_SG_Subject_ID FOREIGN KEY(Subject_ID)
		REFERENCES Subjects(ID)
		ON DELETE CASCADE
		ON UPDATE CASCADE,
	CONSTRAINT FK_SG_Group_ID FOREIGN KEY(Group_ID)
		REFERENCES Groups(ID)
		ON DELETE CASCADE
		ON UPDATE CASCADE
);

CREATE TABLE Schedule_Items
(
	ID integer NOT NULL,
	Subject_ID integer NOT NULL,
	Subject_Type_ID integer,
	Professor_ID integer,
	Lesson_Index integer NOT NULL,
	Day_Index integer NOT NULL,
	Group_ID integer NOT NULL,
	Room_ID integer,
	Week integer NOT NULL, /* 1 - четная, 2 - нечетная, 0 - все */
	CONSTRAINT PK_Items PRIMARY KEY(ID),
	CONSTRAINT FK_SI_Subject_ID FOREIGN KEY(Subject_ID)
		REFERENCES Subjects(ID)
		ON DELETE CASCADE
		ON UPDATE CASCADE,
	CONSTRAINT FK_SI_Subject_Type_ID FOREIGN KEY(Subject_Type_ID)
		REFERENCES Subject_Types(ID),
	CONSTRAINT FK_SI_Professor_ID FOREIGN KEY(Professor_ID)
		REFERENCES Professors(ID)
		ON DELETE SET NULL
		ON UPDATE CASCADE,
	CONSTRAINT FK_SI_Lesson_Index FOREIGN KEY(Lesson_Index)
		REFERENCES Lessons("Index"),
	CONSTRAINT FK_SI_Day_Index FOREIGN KEY(Day_Index)
		REFERENCES Days("Index"),
	CONSTRAINT FK_SI_Group_ID FOREIGN KEY(Group_ID)
		REFERENCES Groups(ID)
		ON DELETE CASCADE
		ON UPDATE CASCADE,
	CONSTRAINT FK_SI_Room_ID FOREIGN KEY(Room_ID)
		REFERENCES Rooms(ID)
		ON DELETE SET NULL
		ON UPDATE CASCADE	
);

COMMIT;
