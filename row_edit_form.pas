unit row_edit_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, DBGrids, Buttons, ExtCtrls, ButtonPanel, db, sqldb,
  metadata, db_edit;

type

  { TEditForm }

  TQueryMode = (qmInsert, qmUpdate);
  TSQLQueryArr = array of TSQLQuery;
  TDataSourceArr = array of TDataSource;

  TEditForm = class(TForm)
    BtnPanel: TButtonPanel;
    TempDS: TDatasource;
    TempQuery: TSQLQuery;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  public
    procedure Prepare(ATable: TTableInfo; ADataSource: TDataSource; ASQLQuery: TSQLQuery;
      AColumns: TDBGridColumns; ARefDSArr: TDataSourceArr; AMode: TQueryMode);
  private
    FTable: TTableInfo;
    FDataSource: TDataSource;
    FSQLQuery: TSQLQuery;
    DBEdits: array of TDBEditWBH;
    FMode: TQueryMode;
  end;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

{ TEditForm }

procedure TEditForm.Prepare(ATable: TTableInfo; ADataSource: TDataSource; ASQLQuery: TSQLQuery;
  AColumns: TDBGridColumns; ARefDSArr: TDataSourceArr; AMode: TQueryMode);
var
  Edit: TEdit;
  RFCounter, i: integer;
begin
  FTable := ATable;
  FDataSource := ADataSource;
  FSQLQuery := ASQLQuery;
  FMode := AMode;
  RFCounter := 0;

  Height := Height + Length(FTable.Fields)*32;
  for i := 0 to high(FTable.Fields) do begin
    Edit := TEdit.Create(self);
    with Edit do begin
      Parent := self;
      Width := 180;
      Height := 27;
      Left := 10;
      Top := i*(Height + 5) + 10;
      ReadOnly := True;
      Text := ATable.Fields[i].Caption;
    end;

    SetLength(DBEdits, Length(DBEdits)+1);
    if FTable.Fields[i] is TRefFieldInfo then begin
      DBEdits[high(DBEdits)] := TListEdit.Create(self, 200, i*32+10, 180, 27, nil);
      with DBEdits[high(DBEdits)] as TListEdit do
        Prepare(ARefDSArr[RFCounter], ATable.Fields[i] as TRefFieldInfo);
      inc(RFCounter);
      if FMode = qmUpdate then
        DBEdits[high(DBEdits)].Value :=
          FSQLQuery.FieldByName(ATable.Fields[i].Name).Value;
    end
    else begin
      DBEdits[high(DBEdits)] :=
        GetEditClass(ATable.Fields[i].DataType).Create(self, 200, i*32+10, 180, 27, nil);
      if FMode = qmUpdate then
        DBEdits[high(DBEdits)].Value :=
          FSQLQuery.FieldByName(AColumns[i].FieldName).Value;
    end;
  end;
  if FMode = qmInsert then begin
    TempQuery.SQL.Text :=
      Format('SELECT GEN_ID(%s, 0) FROM RDB$DATABASE', [FTable.Name+'_ID']);
    TempQuery.Open;
    DBEdits[0].Value := IntToStr(TempQuery.Fields[0].AsInteger+1);
    TempQuery.Close;
  end;
  DBEdits[0].Enabled := False;
end;

procedure TEditForm.OKButtonClick(Sender: TObject);
var
  sqlAction, sqlParams: string;
  Distance, i: integer;
begin
  for i := 0 to high(DBEdits) do
    if DBEdits[i].Value = '' then begin
      ShowMessage('Заполните все поля');
      Show;
      Exit;
    end;

  TempQuery.SQL.Clear;
  case FMode of
  qmInsert:
    begin
      Distance := FSQLQuery.RecordCount;
      sqlAction := '';
      sqlParams := '';
      for i := 1 to high(FTable.Fields) do begin
        sqlAction += FTable.Fields[i].Name + ',';
        sqlParams += Format(':Value_%d,', [i]);
      end;
      sqlAction[Length(sqlAction)] := ')';
      sqlParams[Length(sqlParams)] := ')';
      TempQuery.SQL.Add(Format('INSERT INTO %s(%s', [FTable.Name, sqlAction]));
      TempQuery.SQL.Add(Format('VALUES(%s', [sqlParams]));
    end;
  qmUpdate:
    begin
      Distance := FSQLQuery.RecNo-1;
      TempQuery.SQL.Add(Format('UPDATE %s SET', [FTable.Name]));
      for i := 1 to high(FTable.Fields) do
        sqlAction += Format('%s = :Value_%d,', [FTable.Fields[i].Name, i]);
      System.Delete(sqlAction, Length(sqlAction), 1);
      TempQuery.SQL.Add(sqlAction);
      TempQuery.SQL.Add(Format('WHERE ID = %d', [FSQLQuery.FieldByName('ID').AsInteger]));
    end;
  end;

  for i := 1 to high(FTable.Fields) do
    TempQuery.ParamByName(Format('Value_%d', [i])).Value := DBEdits[i].Value;
  TempQuery.ExecSQL;
  FSQLQuery.Refresh;
  FSQLQuery.MoveBy(Distance);

  Close;
end;

procedure TEditForm.CancelButtonClick(Sender: TObject);
begin
  self.Close;
end;

end.
