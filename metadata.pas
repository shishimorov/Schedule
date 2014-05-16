unit metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

const
  SELECT_IND = 0;
  FROM_IND = 1;
  WHERE_IND = 2;
  GROUP_BY_IND = 3;
  ORDER_BY_IND = 4;

type
  TDataType = (dtInt, dtStr, dtTime);

  TFieldInfo = class(TObject)
  public
    constructor Create(AVisible: boolean; AName, ACaption: string; AWidth: integer;
      ADataType: TDataType); overload;
    function GetFieldName: string; virtual;
  public
    Visible: boolean;
    Name, Caption: string;
    Width: integer;
    DataType: TDataType;
  end;

  TRefFieldInfo = class(TFieldInfo)
  public
    constructor Create(AVisible: boolean; AName, ACaption, ATable, AKeyField,
      AListField, AOrderByField: string; AWidth: integer; ADataType: TDataType); overload;
    function GetFieldName: string; override;
  public
    RefTableName, KeyFieldName, ListFieldName, OrderByField: string;
  end;

  TFields = array of TFieldInfo;

  TTableInfo = class(TObject)
  public
    constructor Create(AName, ACaption: string);
    function AddField(AVisible: boolean; AName, ACaption: string; AWidth: integer;
      ADataType: TDataType): TFieldInfo overload;
    function AddRefField(AVisible: boolean; AName, ACaption, ATable, AKeyField, AListField,
      AOrderByField: string; AWidth: integer; ADataType: TDataType): TRefFieldInfo; overload;
    function AddRefField(AVisible: boolean; AName, ACaption, ATable, AKeyField,
      AListField: string; AWidth: integer; ADataType: TDataType): TRefFieldInfo; overload;
    function GetSQLQuery: TStringList;
    function GetFullFieldName(AFieldIndex: integer): string;
    function GetColumnName(AFieldIndex: integer): string;
    function GetOrderByFieldName(AFieldIndex: integer): string;
  public
    Name, Caption: string;
    Fields: TFields;
  private
    FSQL: TStringList;
  public
    property SQL: TStringList read GetSQLQuery;
  end;

  TTables = array of TTableInfo;

  TMetaData = class(TObject)
  public
    function AddTable(AName, ACaption: string): TTableInfo;
  public
    Tables: TTables;
  end;

  TConfNode = array of TStringList;

  TConflict = class(TObject)
  public
    constructor Create(AName, ASQL: string; AGroupField: integer);
  public
    Name, SQL: string;
    GroupField: integer;
    Data: array of TConfNode;
  end;

var
  MData: TMetaData;
  TimeTableMData: TMetaData;
  Conflicts: array of TConflict;

implementation

constructor TFieldInfo.Create(AVisible: boolean; AName, ACaption: string;
  AWidth: integer; ADataType: TDataType);
begin
  Visible := AVisible;
  Name := AName;
  Caption := ACaption;
  DataType := ADataType;
  Width := AWidth;
end;

function TFieldInfo.GetFieldName: string;
begin
  Result := Name;
end;

constructor TRefFieldInfo.Create(AVisible: boolean; AName, ACaption, ATable, AKeyField,
  AListField, AOrderByField: string; AWidth: integer; ADataType: TDataType);
begin
  Visible := AVisible;
  Name := AName;
  Caption := ACaption;
  RefTableName := ATable;
  KeyFieldName := AKeyField;
  ListFieldName := AListField;
  OrderByField := AOrderByField;
  Width := AWidth;
  DataType := ADataType;
end;

function TRefFieldInfo.GetFieldName: string;
begin
  Result := ListFieldName
end;

constructor TTableInfo.Create(AName, ACaption: string);
begin
  Name := AName;
  Caption := ACaption;
end;

function TTableInfo.AddField(AVisible: boolean; AName, ACaption: string;
  AWidth: integer; ADataType: TDataType): TFieldInfo;
begin
  SetLength(Fields, Length(Fields) + 1);
  Fields[high(Fields)] := TFieldInfo.Create(AVisible, AName, ACaption, AWidth,
    ADataType);
  Result := Fields[high(Fields)];
end;

function TTableInfo.AddRefField(AVisible: boolean; AName, ACaption, ATable, AKeyField,
  AListField, AOrderByField: string; AWidth: integer; ADataType: TDataType): TRefFieldInfo;
begin
  SetLength(Fields, Length(Fields) + 1);
  Fields[high(Fields)] := TRefFieldInfo.Create(AVisible, AName, ACaption, ATable,
    AKeyField, AListField, AOrderByField, AWidth, ADataType);
  Result := TRefFieldInfo(Fields[high(Fields)]);
end;

function TTableInfo.AddRefField(AVisible: boolean; AName, ACaption, ATable, AKeyField,
  AListField: string; AWidth: integer; ADataType: TDataType): TRefFieldInfo;
begin
  Result :=
    AddRefField(AVisible, AName, ACaption, ATable, AKeyField, AListField, '', AWidth, ADataType);
end;

function TTableInfo.GetSQLQuery: TStringList;
var
  sqlSelect, sqlFrom: string;
  i: integer;
begin
  if FSQL <> nil then Exit(FSQL);

  FSQL := TStringList.Create;
  sqlSelect := 'SELECT ';
  sqlFrom := Format('FROM %s ', [Name]);

  for i := 0 to high(Fields) do begin
    if Fields[i] is TRefFieldInfo then begin
      sqlSelect += Format('%s.%s, %s.%s, ', [(Fields[i] as TRefFieldInfo).RefTableName,
        (Fields[i] as TRefFieldInfo).ListFieldName, Name, Fields[i].Name]);
      sqlFrom += Format('INNER JOIN %s ON %s.%s = %s.%s ',
        [(Fields[i] as TRefFieldInfo).RefTableName, Name, Fields[i].Name,
        (Fields[i] as TRefFieldInfo).RefTableName, (Fields[i] as TRefFieldInfo).KeyFieldName]);
    end
    else sqlSelect += Format('%s.%s, ', [Name, Fields[i].Name]);
  end;
  Delete(sqlSelect, Length(sqlSelect) - 1, 2);

  FSQL.Add(sqlSelect); //SELECT_IND
  FSQL.Add(sqlFrom); //FROM_IND
  FSQL.Add(''); //WHERE_IND
  FSQL.Add(''); //GROUP_BY_IND
  FSQL.Add(''); //ORDER_BY_IND
  Result := FSQL;
end;

function TTableInfo.GetFullFieldName(AFieldIndex: integer): string;
begin
  if Fields[AFieldIndex] is TRefFieldInfo then
    Result := Format('%s.%s', [(Fields[AFieldIndex] as TRefFieldInfo).RefTableName,
      Fields[AFieldIndex].GetFieldName])
  else
    Result := Format('%s.%s', [Name, Fields[AFieldIndex].Name]);
end;

function TTableInfo.GetColumnName(AFieldIndex: integer): string;
var i, j: integer;
begin
  Result := AnsiDequotedStr(Fields[AFieldIndex].GetFieldName, '"');
  j := 0;
  for i := 0 to AFieldIndex-1 do
    if Result = Fields[i].GetFieldName then inc(j);
  if j > 0 then
    Result := Format('%s_%d', [Result, j]);
end;

function TTableInfo.GetOrderByFieldName(AFieldIndex: integer): string;
begin
  if Fields[AFieldIndex] is TRefFieldInfo then
    with Fields[AFieldIndex] as TRefFieldInfo do
      if OrderByField = '' then
        Result := Format('%s.%s', [RefTableName, ListFieldName])
      else
        Result := Format('%s.%s', [RefTableName, OrderByField])
  else
    Result := Format('%s.%s', [Name, Fields[AFieldIndex].Name]);
end;

function TMetaData.AddTable(AName, ACaption: string): TTableInfo;
begin
  SetLength(Tables, Length(Tables) + 1);
  Result := TTableInfo.Create(AName, ACaption);
  Tables[high(Tables)] := Result;
end;

procedure RegisterMetaData;
begin
  MData := TMetaData.Create;
  with MData do begin
    with AddTable('Subjects', 'Предметы') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Name', 'Предмет', 500, dtStr);
    end;
    with AddTable('Subject_Types', 'Типы предметов') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Name', 'Тип', 60, dtStr);
    end;
    with AddTable('Professors', 'Преподаватели') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Name', 'ФИО', 500, dtStr);
    end;
    with AddTable('Lessons', 'Время начала/конца пар') do begin
      AddField(False, 'ID', 'ID', 40, dtInt);
      AddField(True, '"Index"', 'Номер', 80, dtInt);
      AddField(True, '"Begin"', 'Начало', 150, dtTime);
      AddField(True, '"End"', 'Конец', 150, dtTime);
    end;
    with AddTable('Days', 'Дни недели') do begin
      AddField(False, 'ID', 'ID', 40, dtInt);
      AddField(True, '"Index"', 'Номер', 60, dtInt);
      AddField(True, 'Name', 'День недели', 200, dtStr);
    end;
    with AddTable('Groups', 'Группы') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Name', 'Группа', 100, dtStr);
      AddField(True, 'Group_Size', 'Рармер', 100, dtInt);
    end;
    with AddTable('Rooms', 'Аудитории') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Name', 'Аудитория', 120, dtStr);
      AddField(True, '"Size"', 'Размер', 100, dtInt);
    end;
    with AddTable('Weeks', 'Недели') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Name', 'Неделя', 120, dtStr);
    end;
    with AddTable('Professors_Subjects', 'Специализация преподавателей') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddRefField(True, 'Professor_ID', 'Преподаватель', 'Professors', 'ID', 'Name', 120, dtStr);
      AddRefField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 400, dtStr);
    end;
    with AddTable('Subjects_Groups', 'Предметы по группам') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddRefField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 425, dtStr);
      AddRefField(True, 'Group_ID', 'Группа', 'Groups', 'ID', 'Name', 100, dtStr);
    end;
    with AddTable('Schedule_Items', 'Расписание') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddRefField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 200, dtStr);
      AddRefField(True, 'Subject_Type_ID', 'Тип', 'Subject_Types', 'ID', 'Name', 60, dtStr);
      AddRefField(True, 'Professor_ID', 'Преподаватель', 'Professors', 'ID', 'Name', 150, dtStr);
      AddRefField(True, 'Lesson_Index', 'Время начала', 'Lessons', 'ID', '"Begin"', 80, dtTime);
      AddRefField(True, 'Day_Index', 'День недели', 'Days', 'ID', 'Name', '"Index"', 130, dtStr);
      AddRefField(True, 'Group_ID', 'Группа', 'Groups', 'ID', 'Name', 130, dtStr);
      AddRefField(True, 'Room_ID', 'Аудитория', 'Rooms', 'ID', 'Name', 130, dtStr);
      AddRefField(True, 'Week_ID', 'Неделя', 'Weeks', 'ID', 'Name', 130, dtStr);
    end;
  end;

  TimeTableMData  := TMetaData.Create;
  with TimeTableMData do
    with AddTable('Schedule_Items', '') do begin
      AddRefField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 200, dtStr);
      AddRefField(True, 'Professor_ID', 'Преподаватель', 'Professors', 'ID', 'Name', 150, dtStr);
      AddRefField(True, 'Lesson_Index', 'Время начала', 'Lessons', 'ID', '"Begin"', 80, dtTime);
      AddRefField(True, 'Day_Index', 'День недели', 'Days', 'ID', 'Name', '"Index"', 130, dtStr);
      AddRefField(True, 'Group_ID', 'Группа', 'Groups', 'ID', 'Name', 130, dtStr);
      AddRefField(True, 'Room_ID', 'Аудитория', 'Rooms', 'ID', 'Name', 130, dtStr);
    end;
end;

constructor TConflict.Create(AName, ASQL: string; AGroupField: integer);
begin
  Name := AName;
  GroupField := AGroupField;
  SQL := ASQL;
end;

function AddConflict(AName, ASQL: string; AGroupField: integer): TConflict;
begin
  SetLength(Conflicts, Length(Conflicts)+1);
  Conflicts[high(Conflicts)] := TConflict.Create(AName, ASQL, AGroupField);
  Result := Conflicts[high(Conflicts)];
end;

procedure RegisterConflicts;
begin
  AddConflict('Одновременно несколько пар в одной аудитории', 'SELECT DISTINCT s1.ID, SUBJECTS.NAME, SUBJECT_TYPES.NAME, PROFESSORS.NAME, LESSONS."Begin", DAYS.NAME, GROUPS.NAME, ROOMS.NAME, WEEKS.NAME FROM SCHEDULE_ITEMS s1 INNER JOIN SCHEDULE_ITEMS s2 ON s1.WEEK_ID = s2.WEEK_ID and s1.DAY_INDEX = s2.DAY_INDEX and s1.LESSON_INDEX = s2.LESSON_INDEX and s1.ROOM_ID = s2.ROOM_ID and s1.ID <> s2.ID and (s1.SUBJECT_ID <> s2.SUBJECT_ID or s1.SUBJECT_TYPE_ID <> s2.SUBJECT_TYPE_ID) INNER JOIN SUBJECTS ON s1.SUBJECT_ID = SUBJECTS.ID INNER JOIN SUBJECT_TYPES ON s1.SUBJECT_TYPE_ID = SUBJECT_TYPES.ID INNER JOIN PROFESSORS ON s1.PROFESSOR_ID = PROFESSORS.ID INNER JOIN LESSONS ON s1.LESSON_INDEX = LESSONS."Index" INNER JOIN DAYS ON s1.DAY_INDEX = DAYS."Index" INNER JOIN GROUPS ON s1.GROUP_ID = GROUPS.ID INNER JOIN ROOMS ON s1.ROOM_ID = ROOMS.ID INNER JOIN WEEKS on s1.WEEK_ID = WEEKS.ID ORDER BY s1.ROOM_ID', 7);
  AddConflict('Преподаватель одновременно в нескольких аудиториях', 'SELECT DISTINCT s1.ID, SUBJECTS.NAME, SUBJECT_TYPES.NAME, PROFESSORS.NAME, LESSONS."Begin", DAYS.NAME, GROUPS.NAME, ROOMS.NAME, WEEKS.NAME FROM SCHEDULE_ITEMS s1 INNER JOIN SCHEDULE_ITEMS s2 ON s1.WEEK_ID = s2.WEEK_ID and s1.DAY_INDEX = s2.DAY_INDEX and s1.LESSON_INDEX = s2.LESSON_INDEX and s1.PROFESSOR_ID = s2.PROFESSOR_ID and s1.ID <> s2.ID and s1.ROOM_ID <> s2.ROOM_ID INNER JOIN SUBJECTS ON s1.SUBJECT_ID = SUBJECTS.ID INNER JOIN SUBJECT_TYPES ON s1.SUBJECT_TYPE_ID = SUBJECT_TYPES.ID INNER JOIN PROFESSORS ON s1.PROFESSOR_ID = PROFESSORS.ID INNER JOIN LESSONS ON s1.LESSON_INDEX = LESSONS."Index" INNER JOIN DAYS ON s1.DAY_INDEX = DAYS."Index" INNER JOIN GROUPS ON s1.GROUP_ID = GROUPS.ID INNER JOIN ROOMS ON s1.ROOM_ID = ROOMS.ID INNER JOIN WEEKS on s1.WEEK_ID = WEEKS.ID ORDER BY s1.PROFESSOR_ID', 3);
end;

initialization

RegisterMetaData;
RegisterConflicts;

end.
