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
  TDataType = (dtInt, dtStr, dtTime);

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
    procedure SetReference(ATable, ARefField, AField: string);
    function GetFieldName: string; override;
  public
    RefTableName, RefFieldName, FieldName: string;
  end;

  TFields = array of TFieldInfo;

  TTableInfo = class(TObject)
  public
    constructor Create(AName, ACaption: string);
    procedure AddField(AVisible: boolean; AName, ACaption: string; AWidth: integer;
      ADataType: TDataType); overload;
    procedure AddField(AVisible: boolean; AName, ACaption, ATable, ARefField,
      AField: string; AWidth: integer; ADataType: TDataType); overload;
    function GetSQLQuery: TStringList;
    function GetFullFieldName(AFieldIndex: integer): string;
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
  Result := FieldName
end;

procedure TRefFieldInfo.SetReference(ATable, ARefField, AField: string);
begin
  RefTableName := ATable;
  RefFieldName := ARefField;
  FieldName := AField;
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

procedure TTableInfo.AddField(AVisible: boolean; AName, ACaption, ATable, ARefField,
  AField: string; AWidth: integer; ADataType: TDataType);
begin
  SetLength(Fields, Length(Fields) + 1);
  Fields[high(Fields)] := TRefFieldInfo.Create(AVisible, AName, ACaption, AWidth,
    ADataType);
  (Fields[high(Fields)] as TRefFieldInfo).SetReference(ATable, ARefField, AField);
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
      sqlSelect += Format('%s.%s, ', [(Fields[i] as TRefFieldInfo).RefTableName,
        (Fields[i] as TRefFieldInfo).FieldName]);
      sqlFrom += Format('INNER JOIN %s ON %s.%s = %s.%s ',
        [(Fields[i] as TRefFieldInfo).RefTableName, Name, Fields[i].Name,
        (Fields[i] as TRefFieldInfo).RefTableName, (Fields[i] as TRefFieldInfo).RefFieldName]);
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
      AddField(True, 'Professor_ID', 'Преподаватель', 'Professors', 'ID', 'Name', 120, dtInt);
      AddField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 400, dtStr);
    end;
    with AddTable('Subjects_Groups', 'Предметы по группам') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 425, dtStr);
      AddField(True, 'Group_ID', 'Группа', 'Groups', 'ID', 'Name', 100, dtStr);
    end;
    with AddTable('Schedule_Items', 'Расписание') do begin
      AddField(True, 'ID', 'ID', 40, dtInt);
      AddField(True, 'Subject_ID', 'Предмет', 'Subjects', 'ID', 'Name', 200, dtStr);
      AddField(True, 'Subject_Type_ID', 'Тип', 'Subject_Types', 'ID', 'Name', 60, dtStr);
      AddField(True, 'Professor_ID', 'Преподаватель', 'Professors', 'ID', 'Name', 150, dtStr);
      AddField(True, 'Lesson_Index', 'Пара №', 'Lessons', 'ID', '"Index"', 80, dtInt);
      AddField(True, 'Day_Index', 'День недели', 'Days', 'ID', '"Index"', 130, dtInt);
      AddField(True, 'Group_ID', 'Группа', 'Groups', 'ID', 'Name', 130, dtStr);
      AddField(True, 'Room_ID', 'Аудитория', 'Rooms', 'ID', 'Name', 130, dtStr);
      AddField(True, 'Week', 'Неделя', 80, dtInt);
    end;
  end;
end;

initialization

RegisterMetaData;

end.
