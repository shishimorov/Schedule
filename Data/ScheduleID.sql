/*CONNECT "C:\Users\shishimorov_ds\Documents\Schedule.fdb" user 'SYSDBA' password 'masterkey';*/
CONNECT "/var/lib/firebird/2.5/data/Schedule.fdb" user 'SYSDBA' password 'masterkey';

CREATE GENERATOR Subjects_ID;
SET TERM ^ ;
CREATE TRIGGER Subject_ID FOR Subjects ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Subjects_ID, 1);
END^
SET TERM ; ^

INSERT INTO Subjects(Name) VALUES('Иностранный язык');
INSERT INTO Subjects(Name) VALUES('Философия');
INSERT INTO Subjects(Name) VALUES('Математический анализ');
INSERT INTO Subjects(Name) VALUES('Теория графов и математическая логика');
INSERT INTO Subjects(Name) VALUES('Линейная алгебра и аналитическая геометрия');
INSERT INTO Subjects(Name) VALUES('Макроэкономика-1');
INSERT INTO Subjects(Name) VALUES('Программные и аппаратные средства информатики');
INSERT INTO Subjects(Name) VALUES('Программирование для ЭВМ');
INSERT INTO Subjects(Name) VALUES('Комбинаторика');
INSERT INTO Subjects(Name) VALUES('Практикум по информатике');
INSERT INTO Subjects(Name) VALUES('Дискретная математика');
INSERT INTO Subjects(Name) VALUES('Дополнительные разделы информатики и программирования');
INSERT INTO Subjects(Name) VALUES('Практикум по программированию');
INSERT INTO Subjects(Name) VALUES('Алгебра и теория чисел');
INSERT INTO Subjects(Name) VALUES('Базы данных');
INSERT INTO Subjects(Name) VALUES('Безопасность жизнедеятельности');
INSERT INTO Subjects(Name) VALUES('Математика');
INSERT INTO Subjects(Name) VALUES('Компьютерная графика');
INSERT INTO Subjects(Name) VALUES('Информатика и программирование');
INSERT INTO Subjects(Name) VALUES('Основы организационно-управленческой деятельности');
INSERT INTO Subjects(Name) VALUES('Языки и методы программирования');
INSERT INTO Subjects(Name) VALUES('Практикум на ЭВМ');
INSERT INTO Subjects(Name) VALUES('Фундаментальная алгебра');
INSERT INTO Subjects(Name) VALUES('Дискр.матем., матем.логика и их прилож.в информ. и комп.науках');
INSERT INTO Subjects(Name) VALUES('Физическая культура');


CREATE GENERATOR Subject_Types_ID;
SET TERM ^ ;
CREATE TRIGGER Subject_Type_ID FOR Subject_Types ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Subject_Types_ID, 1);
END^
SET TERM ; ^

INSERT INTO Subject_Types(Name) VALUES('Лекция');
INSERT INTO Subject_Types(Name) VALUES('Практика');
INSERT INTO Subject_Types(Name) VALUES('ЛБ');


CREATE GENERATOR Professors_ID;
SET TERM ^ ;
CREATE TRIGGER Professor_ID FOR Professors ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Professors_ID, 1);
END^
SET TERM ; ^

INSERT INTO Professors(Name) VALUES('Никольская Т.В.');
INSERT INTO Professors(Name) VALUES('Салтанович М.Н.');
INSERT INTO Professors(Name) VALUES('Демшин И.Н.');
INSERT INTO Professors(Name) VALUES('Свиридова А.Ю.');
INSERT INTO Professors(Name) VALUES('Мишаков А.В.');
INSERT INTO Professors(Name) VALUES('Величко А.С.');
INSERT INTO Professors(Name) VALUES('Воронцова Е.А.');
INSERT INTO Professors(Name) VALUES('Абрамов А.Л.');
INSERT INTO Professors(Name) VALUES('Матачунас Е.В.');
INSERT INTO Professors(Name) VALUES('Рябченко Н.В.');
INSERT INTO Professors(Name) VALUES('Руднева И.В.');
INSERT INTO Professors(Name) VALUES('Антонова Е.И.');
INSERT INTO Professors(Name) VALUES('Москаленко С.В.');
INSERT INTO Professors(Name) VALUES('Лифшиц А.Я.');
INSERT INTO Professors(Name) VALUES('Остроухова С.Н.');
INSERT INTO Professors(Name) VALUES('Хмелева Л.Е.');
INSERT INTO Professors(Name) VALUES('Ларькина Е.В.');
INSERT INTO Professors(Name) VALUES('Никитина Е.Ю.');
INSERT INTO Professors(Name) VALUES('Агапова Т.М.');
INSERT INTO Professors(Name) VALUES('Вакансия');
INSERT INTO Professors(Name) VALUES('Дмух Г.Ю.');
INSERT INTO Professors(Name) VALUES('Малыкина И.А.');
INSERT INTO Professors(Name) VALUES('Кленина Н.В.');
INSERT INTO Professors(Name) VALUES('Клевчихин Ю.А.');
INSERT INTO Professors(Name) VALUES('Туфанов И.Е.');
INSERT INTO Professors(Name) VALUES('Пак Г.К.');
INSERT INTO Professors(Name) VALUES('Пак С.Б.');
INSERT INTO Professors(Name) VALUES('Жуплев А.С.');
INSERT INTO Professors(Name) VALUES('Баранов А.А.');
INSERT INTO Professors(Name) VALUES('Кленин А.С.');
INSERT INTO Professors(Name) VALUES('Храмков И.Н.');
INSERT INTO Professors(Name) VALUES('Прилепкина Е.Г.');
INSERT INTO Professors(Name) VALUES('Сущенко А.А.');
INSERT INTO Professors(Name) VALUES('Чеканов С.Г.');
INSERT INTO Professors(Name) VALUES('Титова О.');
INSERT INTO Professors(Name) VALUES('Прудникова Л.И.');
INSERT INTO Professors(Name) VALUES('Вольнов И.Н.');


CREATE GENERATOR Lessons_ID;
SET TERM ^ ;
CREATE TRIGGER Lesson_ID FOR Lessons ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Lessons_ID, 1);
END^
SET TERM ; ^

INSERT INTO Lessons("Index", "Begin", "End") VALUES(1, '08:30:00', '10:00:00');
INSERT INTO Lessons("Index", "Begin", "End") VALUES(2, '10:10:00', '11:40:00');
INSERT INTO Lessons("Index", "Begin", "End") VALUES(3, '11:50:00', '13:20:00');
INSERT INTO Lessons("Index", "Begin", "End") VALUES(4, '13:30:00', '15:00:00');
INSERT INTO Lessons("Index", "Begin", "End") VALUES(5, '15:10:00', '16:40:00');
INSERT INTO Lessons("Index", "Begin", "End") VALUES(6, '16:50:00', '18:20:00');
INSERT INTO Lessons("Index", "Begin", "End") VALUES(7, '18:30:00', '20:00:00');


CREATE GENERATOR Days_ID;
SET TERM ^ ;
CREATE TRIGGER Day_ID FOR Days ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Days_ID, 1);
END^
SET TERM ; ^

INSERT INTO Days("Index", Name) VALUES(1, 'Понедельник');
INSERT INTO Days("Index", Name) VALUES(2, 'Вторник');
INSERT INTO Days("Index", Name) VALUES(3, 'Среда');
INSERT INTO Days("Index", Name) VALUES(4, 'Четверг');
INSERT INTO Days("Index", Name) VALUES(5, 'Пятница');
INSERT INTO Days("Index", Name) VALUES(6, 'Суббота');
INSERT INTO Days("Index", Name) VALUES(7, 'Воскресенье');


CREATE GENERATOR Groups_ID;
SET TERM ^ ;
CREATE TRIGGER Group_ID FOR Groups ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Groups_ID, 1);
END^
SET TERM ; ^

INSERT INTO Groups(Name, Group_Size) VALUES('Б8120', 14);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8104', 24);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8104-1', 12);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8104-2', 12);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8119а', 15);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8103а', 31);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8103а-1', 15);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8103а-2', 15);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8103б', 31);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8103б-1', 15);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8103б-2', 15);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8102', 25);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8102-1', 12);
INSERT INTO Groups(Name, Group_Size) VALUES('Б8102-2', 13);


CREATE GENERATOR Professors_Subjects_ID;
SET TERM ^ ;
CREATE TRIGGER PS_ID FOR Professors_Subjects ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Professors_Subjects_ID, 1);
END^
SET TERM ; ^

INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(1, 1);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(2, 2);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(3, 3);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(4, 4);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(5, 5);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(6, 6);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(7, 7);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(7, 8);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(8, 4);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(8, 9);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(8, 21);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(9, 9);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(10, 11);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(11, 11);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(11, 23);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(12, 12);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(13, 1);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(14, 13);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(15, 13);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(16, 14);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(17, 3);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(18, 15);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(19, 16);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(20, 11);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(21, 17);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(22, 18);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(23, 19);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(24, 3);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(25, 21);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(26, 5);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(26, 11);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(27, 22);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(28, 22);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(29, 22);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(30, 15);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(31, 15);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(32, 3);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(33, 21);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(33, 22);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(34, 24);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(35, 1);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(36, 21);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(36, 22);
INSERT INTO Professors_Subjects(Professor_ID, Subject_ID) VALUES(37, 15);


CREATE GENERATOR Subjects_Groups_ID;
SET TERM ^ ;
CREATE TRIGGER SG_ID FOR Subjects_Groups ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Subjects_Groups_ID, 1);
END^
SET TERM ; ^

INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(1, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(1, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(1, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(1, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(1, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(1, 12);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(2, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(3, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(3, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(3, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(3, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(3, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(3, 12);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(4, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(5, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(5, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(5, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(6, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(7, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(8, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(9, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(10, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(11, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(11, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(11, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(11, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(12, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(13, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(14, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(15, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(15, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(15, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(15, 12);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(16, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(17, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(18, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(19, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(20, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(21, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(21, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(21, 12);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(22, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(22, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(22, 12);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(23, 12);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(24, 12);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(25, 1);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(25, 2);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(25, 5);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(25, 6);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(25, 9);
INSERT INTO Subjects_Groups(Subject_ID, Group_ID) VALUES(25, 12);

CREATE GENERATOR Rooms_ID;
SET TERM ^ ;
CREATE TRIGGER Room_ID FOR Rooms ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Rooms_ID, 1);
END^
SET TERM ; ^

INSERT INTO Rooms(Name, "Size") VALUES('D944', 51);
INSERT INTO Rooms(Name, "Size") VALUES('D945', 51);
INSERT INTO Rooms(Name, "Size") VALUES('Корпус S/S1/S2', 501);
INSERT INTO Rooms(Name, "Size") VALUES('D942', 61);
INSERT INTO Rooms(Name, "Size") VALUES('D943', 21);
INSERT INTO Rooms(Name, "Size") VALUES('D944', 31);
INSERT INTO Rooms(Name, "Size") VALUES('D936', 51);
INSERT INTO Rooms(Name, "Size") VALUES('D938', 51);
INSERT INTO Rooms(Name, "Size") VALUES('D733', 41);
INSERT INTO Rooms(Name, "Size") VALUES('D941', 15);
INSERT INTO Rooms(Name, "Size") VALUES('D810', 51);
INSERT INTO Rooms(Name, "Size") VALUES('D741', 41);
INSERT INTO Rooms(Name, "Size") VALUES('D534', 201);
INSERT INTO Rooms(Name, "Size") VALUES('D732', 71);
INSERT INTO Rooms(Name, "Size") VALUES('D940', 15);
INSERT INTO Rooms(Name, "Size") VALUES('D542', 31);
INSERT INTO Rooms(Name, "Size") VALUES('D820', 61);
INSERT INTO Rooms(Name, "Size") VALUES('D738', 61);
INSERT INTO Rooms(Name, "Size") VALUES('D549', 31);
INSERT INTO Rooms(Name, "Size") VALUES('D818', 31);

CREATE GENERATOR Weeks_ID;
SET TERM ^ ;
CREATE TRIGGER Weeks_ID FOR Weeks ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Weeks_ID, 1);
END^
SET TERM ; ^

INSERT INTO Weeks(Name) VALUES('Все');
INSERT INTO Weeks(Name) VALUES('Четная');
INSERT INTO Weeks(Name) VALUES('Нечетная');

CREATE GENERATOR Schedule_Items_ID;
SET TERM ^ ;
CREATE TRIGGER Item_ID FOR Schedule_Items ACTIVE
BEFORE INSERT POSITION 1
AS BEGIN
	if (new.id is null) then
	new.id = gen_id(Schedule_Items_ID, 1);
END^
SET TERM ; ^

INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 2, 1, 2, 1, 1, 1, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(2, 1, 2, 3, 1, 1, 2, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(2, 2, 2, 3, 1, 1, 2, 3);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 1, 3, 1, 2, 1, 4, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 3, 2, 2, 1, 4, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(4, 2, 4, 3, 2, 1, 5, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 1, 5, 2, 3, 1, 1, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 2, 5, 3, 3, 1, 1, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 2, 5, 4, 3, 1, 1, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(6, 1, 6, 1, 4, 1, 5, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(6, 2, 6, 2, 4, 1, 5, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 2, 1, 3, 4, 1, 1, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 3, 1, 5, 1, 4, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(7, 1, 7, 2, 5, 1, 5, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(8, 3, 7, 2, 5, 1, 7, 3);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(7, 3, 7, 3, 5, 1, 8, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(4, 1, 8, 2, 6, 1, 4, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(9, 1, 8, 3, 6, 1, 4, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(9, 3, 9, 4, 6, 1, 1, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(9, 2, 9, 4, 6, 1, 1, 3);

INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(10, 3, 10, 1, 2, 3, 8, 2); 
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(10, 3, 10, 1, 2, 4, 8, 3);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(10, 3, 10, 2, 2, 3, 8, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(10, 3, 10, 2, 2, 4, 8, 3);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 1, 11, 3, 2, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 2, 11, 4, 2, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 1, 11, 5, 2, 2, 13, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(12, 1, 12, 1, 3, 2, 14, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(12, 1, 12, 2, 3, 2, 14, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(12, 3, 12, 3, 3, 3, 8, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(12, 3, 12, 3, 3, 4, 8, 3);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 3, 13, 2, 4, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 3, 13, 3, 4, 2, 12, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(13, 3, 14, 1, 5, 3, 8, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(13, 3, 15, 1, 5, 4, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(13, 3, 14, 2, 5, 3, 8, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(13, 3, 15, 2, 5, 4, 6, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(14, 1, 16, 3, 5, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(14, 2, 16, 4, 5, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 1, 17, 1, 6, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 1, 17, 2, 6, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 17, 3, 6, 2, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 17, 4, 6, 2, 12, 2);

INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 1, 18, 2, 1, 5, 9, 1); 
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 18, 3, 1, 5, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(16, 1, 19, 1, 2, 5, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 2, 1, 2, 2, 5, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 1, 20, 3, 2, 5, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 2, 20, 4, 2, 5, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(17, 1, 21, 2, 3, 5, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(17, 2, 21, 3, 3, 5, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(17, 2, 21, 4, 3, 5, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(18, 1, 22, 2, 4, 5, 15, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(18, 3, 22, 2, 4, 5, 15, 3);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(18, 3, 22, 3, 4, 5, 15, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(19, 1, 23, 2, 5, 5, 12, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(19, 3, 23, 3, 5, 5, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(19, 3, 23, 4, 5, 5, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(10, 1, 8, 5, 5, 5, 11, 1);

INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 1, 24, 1, 1, 6, 16, 1); 
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 1, 24, 1, 1, 9, 16, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 24, 2, 1, 6, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 3, 13, 2, 1, 9, 14, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 3, 13, 3, 1, 6, 9, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 24, 3, 1, 9, 14, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 4, 1, 9, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 5, 1, 9, 10, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 1, 26, 2, 2, 6, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 1, 26, 2, 2, 9, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 2, 26, 3, 2, 6, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 2, 26, 3, 2, 9, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 24, 1, 3, 6, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 2, 26, 2, 3, 6, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 24, 2, 3, 9, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(5, 2, 26, 3, 3, 9, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 3, 3, 8, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 4, 3, 8, 7, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 4, 3, 10, 7, 3);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 5, 3, 10, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 27, 4, 3, 11, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 27, 5, 3, 11, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 28, 1, 4, 8, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 28, 2, 4, 8, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 29, 1, 4, 10, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 29, 2, 4, 10, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 1, 25, 3, 4, 6, 18, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 1, 25, 3, 4, 9, 18, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 4, 4, 7, 15, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 25, 5, 4, 7, 15, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 30, 4, 4, 8, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 30, 5, 4, 8, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 1, 26, 2, 5, 6, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 1, 26, 2, 5, 9, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 2, 26, 3, 5, 6, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 2, 26, 3, 5, 9, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 2, 26, 4, 5, 6, 11, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(11, 2, 26, 4, 5, 9, 11, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 31, 5, 5, 10, 8, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 31, 6, 5, 10, 8, 2);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 1, 30, 1, 6, 6, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 1, 30, 1, 6, 9, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 30, 2, 6, 7, 15, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 31, 2, 6, 11, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 30, 4, 6, 7, 15, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 30, 5, 6, 7, 15, 1);

INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(23, 1, 11, 4, 1, 12, 19, 1); 
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(23, 2, 11, 5, 1, 12, 19, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 1, 32, 1, 2, 12, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 32, 2, 2, 12, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 33, 3, 2, 14, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 33, 4, 2, 14, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 33, 5, 2, 14, 7, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(24, 1, 34, 2, 3, 12, 20, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(24, 2, 34, 3, 3, 12, 20, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 1, 32, 1, 4, 12, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(3, 2, 32, 2, 4, 12, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(1, 2, 35, 4, 4, 12, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 1, 36, 3, 5, 12, 17, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(21, 3, 36, 4, 5, 13, 8, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 37, 1, 6, 14, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 1, 37, 2, 6, 12, 11, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(15, 3, 37, 3, 6, 13, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 36, 4, 6, 13, 10, 1);
INSERT INTO Schedule_Items(Subject_ID, Subject_Type_ID, Professor_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(22, 3, 36, 5, 6, 13, 10, 1);

INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 4, 1, 1, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 5, 1, 1, 3, 1); 
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 4, 1, 2, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 5, 1, 2, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 4, 1, 5, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 5, 1, 5, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 3, 1, 12, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 1, 2, 6, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 1, 2, 9, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 4, 4, 1, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 5, 4, 1, 3, 1); 
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 4, 4, 2, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 5, 4, 2, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 4, 4, 5, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 5, 4, 5, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 3, 4, 12, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 1, 5, 6, 3, 1);
INSERT INTO Schedule_Items(Subject_ID, Lesson_Index, Day_Index, Group_ID, Room_ID, Week_ID) VALUES(25, 1, 5, 9, 3, 1);

COMMIT;
