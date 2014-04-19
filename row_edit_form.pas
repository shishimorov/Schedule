unit row_edit_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, DBGrids, Buttons, ExtCtrls, ComCtrls, ButtonPanel, db, sqldb,
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
    procedure Prepare(ATable: TTableInfo; ADataSource: TDataSource;
      ASQLQuery: TSQLQuery; AColumns: TDBGridColumns;
      AEditDSArr: TDataSourceArr; AMode: TQueryMode);
  private
    function GetCorrectFieldName(const AFieldName: string): string;
    procedure FOnKeyPress(Sender: TObject; var Key: char);
  private
    FTable: TTableInfo;
    FDataSource: TDataSource;
    FSQLQuery: TSQLQuery;
    DBEditArr: array of TDBEditWBH;
    DBComboBoxArr: array of TDBLookupComboBox;
    FMode: TQueryMode;
  end;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

{ TEditForm }

procedure TEditForm.Prepare(ATable: TTableInfo; ADataSource: TDataSource;
  ASQLQuery: TSQLQuery; AColumns: TDBGridColumns; AEditDSArr: TDataSourceArr;
  AMode: TQueryMode);
var
  Edit: TEdit;
  RFCounter, i: integer;
begin
  FTable := ATable;
  FDataSource := ADataSource;
  FSQLQuery := ASQLQuery;
  FMode := AMode;
  RFCounter := 0;

  for i := 0 to high(FTable.Fields) do begin
    self.Height := self.Height + 32;

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

    if ATable.Fields[i] is TRefFieldInfo then begin
      SetLength(DBComboBoxArr, Length(DBComboBoxArr)+1);
      DBComboBoxArr[high(DBComboBoxArr)] := TDBLookupComboBox.Create(self);
      with DBComboBoxArr[high(DBComboBoxArr)] do begin
        Parent := self;
        width := 180;
        Height := 27;
        Left := 200;
        Top := i*32+10;
        ListSource := AEditDSArr[RFCounter];
        ListField :=
          GetCorrectFieldName((ATable.Fields[i] as TRefFieldInfo).FieldName);
        KeyField :=
          GetCorrectFieldName((ATable.Fields[i] as TRefFieldInfo).RefFieldName);
        ItemIndex := 0;
        ReadOnly := True;
        if FMode = qmUpdate then begin
          with (ATable.Fields[i] as TRefFieldInfo) do begin
            TempQuery.SQL.Text :=
              Format('SELECT %s FROM %s WHERE %s = ''%s''',
                [RefFieldName, RefTableName, FieldName,
                FSQLQuery.FieldByName(AColumns[i].FieldName).AsString]);
            TempQuery.Open;
            KeyValue := TempQuery.Fields[0].AsInteger;
          end;
          TempQuery.Close;
        end;
      end;
      inc(RFCounter);
    end
    else begin
      SetLength(DBEditArr, Length(DBEditArr)+1);
      case ATable.Fields[i].DataType of
      dtInt:
        DBEditArr[high(DBEditArr)] :=
          TTextEdit.Create(self, 200, i*32+10, 180, 27, nil);
      dtStr:
        DBEditArr[high(DBEditArr)] :=
          TTextEdit.Create(self, 200, i*32+10, 180, 27, nil);
      dtTIme:
        DBEditArr[high(DBEditArr)] :=
          TTimeEdit.Create(self, 200, i*32+10, 59, 27, nil);
      end;

      if FMode = qmUpdate then
        DBEditArr[high(DBEditArr)].Text :=
          FSQLQuery.FieldByName(AColumns[i].FieldName).AsString;
    end;
  end;
  if FMode = qmInsert then begin
    TempQuery.SQL.Text :=
      Format('SELECT GEN_ID(%s, 0) FROM RDB$DATABASE', [FTable.Name+'_ID']);
    TempQuery.Open;
    DBEditArr[0].Text := IntToStr(TempQuery.Fields[0].AsInteger+1);
    TempQuery.Close;
  end;
  DBEditArr[0].Enabled := False; //защита от пользователя
end;

procedure TEditForm.OKButtonClick(Sender: TObject);
var
  tmpSQL, tmp1, tmp2: string;
  FCounter, RFCounter, FieldID, i: integer;
begin
  with FSQLQuery do begin
    Close;
    tmpSQL := SQL.Text;
    SQL.Clear;

    case FMode of
    qmInsert:
      begin
        for i := 1 to high(FTable.Fields) do begin
          tmp1 += FTable.Fields[i].Name + ',';
          tmp2 += Format(':Value_%d,', [i]);
        end;
        tmp1[Length(tmp1)] := ')';
        tmp2[Length(tmp2)] := ')';
        SQL.Add(Format('INSERT INTO %s(%s', [FTable.Name, tmp1]));
        SQL.Add(Format('VALUES(%s', [tmp2]));
      end;
    qmUpdate:
      begin
        FieldID := FieldByName('ID').Value;
        SQL.Add(Format('UPDATE %s SET', [FTable.Name]));

        for i := 1 to high(FTable.Fields) do
          tmp1 += Format('%s = :Value_%d,', [FTable.Fields[i].Name, i]);

        System.Delete(tmp1, Length(tmp1), 1);
        SQL.Add(tmp1);
        SQL.Add(Format('WHERE ID = %d', [FieldID]));
      end;
    end;

    FCounter := 1; //ID пропускаем
    RFCounter := 0;
    for i := 1 to high(FTable.Fields) do begin
      if FTable.Fields[i] is TRefFieldInfo then begin
        ParamByName(Format('Value_%d', [i])).Value := DBComboBoxArr[RFCounter].KeyValue;
        inc(RFCounter);
      end
      else begin
        ParamByName(Format('Value_%d', [i])).Value := DBEditArr[FCounter].Text;
        inc(FCounter);
      end;
    end;

    ExecSQL;
    SQL.Text := tmpSQL;
    Open;
    ApplyUpdates;
  end;
  self.Close;
end;

procedure TEditForm.CancelButtonClick(Sender: TObject);
begin
  self.Close;
end;

procedure TEditForm.FOnKeyPress(Sender: TObject; var Key: char);
begin
  if (not (Key in ['0'..'9'])) and (Key <> #8) then Key := #0;
end;

function TEditForm.GetCorrectFieldName(const AFieldName: string): string;
begin
  Result := AFieldName;
  if (Pos('"', Result) = 1) and (Result[Length(Result)] = '"') then
  begin
    Delete(Result, 1, 1);
    Delete(Result, Length(Result), 1);
  end;
end;

end.

