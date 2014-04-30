unit record_edit_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, Buttons, ExtCtrls, ButtonPanel, db, sqldb,
  metadata, data, db_edit;

type

  { TEditForm }

  TQueryMode = (qmInsert, qmUpdate);
  TSQLQueryArr = array of TSQLQuery;
  TDataSourceArr = array of TDataSource;

  TEditForm = class(TForm)
    BtnPanel: TButtonPanel;
    Datasource: TDatasource;
    SQLQuery: TSQLQuery;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent; ATable: TTableInfo;
      AfterInsertAction: TNotifyEvent); overload;
    constructor Create(TheOwner: TComponent; ATable: TTableInfo;
      ARecordID: integer; AfterEditAction: TNotifyEvent); overload;
  private
    procedure InitComponents;
  public
    FieldEdits: array of TFieldEdit;
  private
    FMode: TQueryMode;
    FTable: TTableInfo;
    FAfterAction: TNotifyEvent;
    FRefQueryArr: TSQLQueryArr;
    FRefDSArr: TDataSourceArr;
    FRecordID, FDistance: integer;
  public
    property Distance: integer read FDistance write FDistance;
  end;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

{ TEditForm }

constructor TEditForm.Create(TheOwner: TComponent; ATable: TTableInfo;
  AfterInsertAction: TNotifyEvent);
begin
  inherited Create(TheOwner);
  Caption := 'Вставка';
  FMode := qmInsert;
  FTable := ATable;
  FAfterAction := AfterInsertAction;
  InitComponents;

  SQLQuery.SQL.Text :=
    Format('SELECT GEN_ID(%s, 0) FROM RDB$DATABASE', [FTable.Name+'_ID']);
  SQLQuery.Open;
  FieldEdits[0].Value := IntToStr(SQLQuery.Fields[0].AsInteger+1);
  SQLQuery.Close;
end;

constructor TEditForm.Create(TheOwner: TComponent; ATable: TTableInfo;
  ARecordID: integer; AfterEditAction: TNotifyEvent);
var i: integer;
begin
  inherited Create(TheOwner);
  Caption := 'Редактирование';
  FMode := qmUpdate;
  FTable := ATable;
  FRecordID := ARecordID;
  FAfterAction := AfterEditAction;
  InitComponents;

  SQLQuery.SQL := FTable.SQL;
  SQLQuery.SQL.Strings[WHERE_IND] := Format('WHERE %s.ID = %d', [FTable.Name, ARecordID]);
  SQLQuery.Open;
  with FTable do
    for i := 0 to high(Fields) do begin
      if Fields[i] is TRefFieldInfo then
        FieldEdits[i].Value := SQLQuery.FieldByName(Fields[i].Name).Value
      else
        FieldEdits[i].Value := SQLQuery.FieldByName(GetColumnName(i)).Value;
    end;
  SQLQuery.Close;
end;

procedure TEditForm.OKButtonClick(Sender: TObject);
var
  sqlAction, sqlParams: string;
  i: integer;
begin
  for i := 0 to high(FieldEdits) do
    if FieldEdits[i].Value = '' then begin
      ShowMessage('Заполните все поля');
      Show;
      Exit;
    end;

  SQLQuery.SQL.Clear;
  case FMode of
  qmInsert:
    begin
      sqlAction := '';
      sqlParams := '';
      for i := 1 to high(FTable.Fields) do begin
        sqlAction += FTable.Fields[i].Name + ',';
        sqlParams += Format(':Value_%d,', [i]);
      end;
      sqlAction[Length(sqlAction)] := ')';
      sqlParams[Length(sqlParams)] := ')';
      SQLQuery.SQL.Add(Format('INSERT INTO %s(%s', [FTable.Name, sqlAction]));
      SQLQuery.SQL.Add(Format('VALUES(%s', [sqlParams]));
    end;
  qmUpdate:
    begin
      SQLQuery.SQL.Add(Format('UPDATE %s SET', [FTable.Name]));
      for i := 1 to high(FTable.Fields) do
        sqlAction += Format('%s = :Value_%d,', [FTable.Fields[i].Name, i]);
      System.Delete(sqlAction, Length(sqlAction), 1);
      SQLQuery.SQL.Add(sqlAction);
      SQLQuery.SQL.Add(Format('WHERE ID = %d', [FRecordID]));
    end;
  end;
  for i := 1 to high(FTable.Fields) do
    SQLQuery.ParamByName(Format('Value_%d', [i])).Value := FieldEdits[i].Value;

  SQLQuery.ExecSQL;
  FAfterAction(self);
  Close;
end;

procedure TEditForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TEditForm.InitComponents;
var
  Edit: TEdit;
  i: integer;
begin
  SetLength(FieldEdits, Length(FTable.Fields));
  Height := Height + Length(FieldEdits)*32;
  for i := 0 to high(FTable.Fields) do begin
    Edit := TEdit.Create(self);
    with Edit do begin
      Parent := self;
      Width := 180;
      Height := 27;
      Left := 10;
      Top := i*(Height + 5) + 10;
      ReadOnly := True;
      Text := FTable.Fields[i].Caption;
    end;

    if FTable.Fields[i] is TRefFieldInfo then begin
      SetLength(FRefQueryArr, Length(FRefQueryArr)+1);
      FRefQueryArr[high(FRefQueryArr)] := TSQLQuery.Create(self);
      with FRefQueryArr[high(FRefQueryArr)] do begin
        DataBase := DBData.IBConnection;
        Transaction := DBData.SQLTransaction;
        SQL.Text := Format('SELECT t.%s, t.%s FROM %s t',
          [(FTable.Fields[i] as TRefFieldInfo).KeyFieldName,
          (FTable.Fields[i] as TRefFieldInfo).ListFieldName,
          (FTable.Fields[i] as TRefFieldInfo).RefTableName]);
        Open;
      end;
      SetLength(FRefDSArr, Length(FRefDSArr)+1);
      FRefDSArr[high(FRefDSArr)] := TDataSource.Create(self);
      FRefDSArr[high(FRefDSArr)].DataSet := FRefQueryArr[high(FRefQueryArr)];
      FRefQueryArr[high(FRefQueryArr)].Open;

      FieldEdits[i] := TListEdit.Create(self, 200, i*32+10, 180, 27,
          FRefDSArr[high(FRefDSArr)], FTable.Fields[i] as TRefFieldInfo);
    end
    else
      FieldEdits[i] :=
        GetEditClass(FTable.Fields[i].DataType).Create(self, 200, i*32+10, 180, 27);
  end;
  FieldEdits[0].Enabled := False;
end;

end.
