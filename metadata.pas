unit metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SELECT_IND = 0;
  FROM_IND = 1;
  WHERE_IND = 2;
  GROUP_BY_IND = 3;
  ORDER_BY_IND = 4;

type
  TDataType = (dtInt, dtStr, dtTime, dtList);

  TFieldInfo = class(TObject)
  public
    constructor Create(AVisible: boolean; AName, ACaption: string; AWidth: integer;
      ADataType: TDataType);
    function GetFieldName: string; virtual;
  public
    Visible: boolean;
    Name, Caption: string;
    Width: integer;
    DataType: TDataType;
  end;

  TRefFieldInfo = class(TFieldInfo)
  public
    procedure SetReference(ATable, AKeyField, AListField, AOrderByField: string);
    function GetFieldName: string; override;
  public
    RefTableName, KeyFieldName, ListFieldName, OrderByField: string;
  end;

  TFields = array of TFieldInfo;

  TTableInfo = class(TObject)
  public
    constructor Create(AName, ACaption: string);
    procedure AddField(AVisible: boolean; AName, ACaption: string; AWidth: integer;
      ADataType: TDataType); overload;
    procedure AddField(AVisible: boolean; AName, ACaption, ATable, AKeyField,
      AListField, AOrderByField: string; AWidth: integer; ADataType: TDataType); overload;
    function GetSQLQuery: TStringList;
    function GetFullFieldName(AFieldIndex: integer): string;
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

var
  MData: TMetaData;

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

function TRefFieldInfo.GetFieldName: string;
begin
  Result := ListFieldName
end;

procedure TRefFieldInfo.SetReference(ATable, AKeyField, AListField, AOrderByField: string);
begin
  RefTableName := ATable;
  KeyFieldName := AKeyField;
  ListFieldName := AListField;
  OrderByField := AOrderByField;
end;

constructor TTableInfo.Create(AName, ACaption: string);
begin
  Name := AName;
  Caption := ACaption;
end;

procedure TTableInfo.AddField(AVisible: boolean; AName, ACaption: string;
  AWidth: integer; ADataType: TDataType);
begin
  SetLength(Fields, Length(Fields) + 1);
  Fields[high(Fields)] := TFieldInfo.Create(AVisible, AName, ACaption, AWidth,
    ADataType);
end;

procedure TTableInfo.AddField(AVisible: boolean; AName, ACaption, ATable, AKeyField,
  AListField, AOrderByField: string; AWidth: integer; ADataType: TDataType);
begin
  SetLength(Fields, Length(Fields) + 1);
  Fields[high(Fields)] := TRefFieldInfo.Create(AVisible, AName, ACaption, AWidth, ADataType);
  with Fields[high(Fields)] as TRefFieldInfo do
    SetReference(ATable, AKeyField, AListField, AOrderByField);
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

function TTableInfo.GetOrderByFieldName(AFieldIndex: integer): string;
begin
  if Fields[AFieldIndex] is TRefFieldInfo then
    with Fields[AFieldIndex] as TRefFieldInfo do
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
    with AddTable('Professors_Subjects', 'Специализация преподавателей') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Professor_ID', 'Преподаватель', 'Professors', 'ID', 'Name', 'Name', 120, dtList);
      AddField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 'Name', 400, dtList);
    end;
    with AddTable('Subjects_Groups', 'Предметы по группам') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 'Name', 425, dtList);
      AddField(True, 'Group_ID', 'Группа', 'Groups', 'ID', 'Name', 'Name', 100, dtList);
    end;
    with AddTable('Schedule_Items', 'Расписание') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 'Name', 200, dtList);
      AddField(True, 'Subject_Type_ID', 'Тип', 'Subject_Types', 'ID', 'Name', 'Name', 60, dtList);
      AddField(True, 'Professor_ID', 'Преподаватель', 'Professors', 'ID', 'Name', 'Name', 150, dtList);
      AddField(True, 'Lesson_Index', 'Пара №', 'Lessons', 'ID', '"Index"', '"Index"', 80, dtList);
      AddField(True, 'Day_Index', 'День недели', 'Days', 'ID', 'Name', '"Index"', 130, dtList);
      AddField(True, 'Group_ID', 'Группа', 'Groups', 'ID', 'Name', 'Name', 130, dtList);
      AddField(True, 'Room_ID', 'Аудитория', 'Rooms', 'ID', 'Name', 'Name', 130, dtList);
      AddField(True, 'Week', 'Неделя', 80, dtInt);
    end;
  end;
end;

initialization

RegisterMetaData;

end.
