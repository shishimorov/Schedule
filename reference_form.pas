unit reference_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, DBGrids, DbCtrls, ActnList, Menus, ExtCtrls, Buttons, EditBtn,
  metadata, search_frame, row_edit_form, data;

type

  { TRefForm }

  TEditForms = array of TEditForm;

  TRefForm = class(TForm)
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    InsertBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    EditBtn: TSpeedButton;
    TitleImgList: TImageList;
    FilterPanel: TPanel;
    SQLQuery: TSQLQuery;
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure InsertBtnClick(Sender: TObject);
  public
    procedure PrepareDBGrid(var ATable: TTableInfo);
    procedure InitSearchFrame;
  private
    procedure RemoveInvalidPointers(var AObjects: TEditForms);
  private
    FEditForms: TEditForms;
    FRefQueryArr: array of TSQLQuery;
    FRefDSArr: array of TDataSource;
    FSearchFrame: TSearchFrame;
    FTable: TTableInfo;
    FSortedColInd: integer;
  const
    CleanFrequency = 2;
    MB_YES = 6;
    MB_YESNO = 4;
    MB_ICONWARNING = 48;
  end;

implementation

{$R *.lfm}

procedure TRefForm.DBGridTitleClick(Column: TColumn);
var FieldName: string;
begin
  SQLQuery.Close;

  if FSortedColInd <> -1 then
    DBGrid.Columns[FSortedColInd].Title.ImageIndex := -1;
  if FSortedColInd <> Column.Index then DBGrid.SortOrder := soAscending;

  FieldName := FTable.GetOrderByFieldName(Column.Index);
  if DBGrid.SortOrder = soAscending then begin
    DBGrid.SortOrder := soDescending;
    SQLQuery.SQL.Strings[ORDER_BY_IND] := Format('ORDER BY %s desc', [FieldName]);
    Column.Title.ImageIndex := 1;
  end
  else begin
    DBGrid.SortOrder := soAscending;
    SQLQuery.SQL.Strings[ORDER_BY_IND] := Format('ORDER BY %s asc', [FieldName]);
    Column.Title.ImageIndex := 0;
  end;

  FSortedColInd := Column.Index;
  //ShowMessage(SQLQuery.SQL.Text);
  SQLQuery.Open;
end;

procedure TRefForm.InsertBtnClick(Sender: TObject);
begin
  if Length(FEditForms) > CleanFrequency then RemoveInvalidPointers(FEditForms);
  SetLength(FEditForms, Length(FEditForms)+1);
  FEditForms[high(FEditForms)] := TEditForm.Create(self);
  with FEditForms[high(FEditForms)] do begin
    Prepare(FTable, DataSource, SQLQuery, DBGrid.Columns, FRefDSArr, qmInsert);
    Tag := -1;
    Show;
  end;
end;

procedure TRefForm.DeleteBtnClick(Sender: TObject);
var
  FieldID: integer;
  SavedSQL: string;
begin
  if Application.MessageBox('Вы действительно хотите удалить данную запись?',
    'Подтверждение', (MB_YESNO + MB_ICONWARNING)) = MB_YES then begin
    with SQLQuery do begin
      FieldID := FieldByName('ID').AsInteger;
      Close;
      SavedSQL := SQL.Text;
      SQL.Text := Format('DELETE FROM %s WHERE ID = %d', [FTable.Name, FieldID]);
      ExecSQL;
      SQL.Text := SavedSQL;
      Open;
    end;
  end;
end;

procedure TRefForm.EditBtnClick(Sender: TObject);
var RecID, i: integer;
begin
  if Length(FEditForms) > CleanFrequency then RemoveInvalidPointers(FEditForms);
  RecID := SQLQuery.FieldByName('ID').AsInteger;
  for i := 0 to high(FEditForms) do
    if FEditForms[i].Tag = RecID then begin
      FEditForms[i].Show;
      Exit;
    end;
  SetLength(FEditForms, Length(FEditForms)+1);
  FEditForms[high(FEditForms)] := nil;
  FEditForms[high(FEditForms)] := TEditForm.Create(self);
  with FEditForms[high(FEditForms)] do begin
    Prepare(FTable, DataSource, SQLQuery, DBGrid.Columns, FRefDSArr, qmUpdate);
    Tag := RecID;
    Show;
  end;
end;

procedure TRefForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  for i := 0 to high(FEditForms) do
    if FEditForms[i] <> nil then FEditForms[i].Free;
end;

procedure TRefForm.DBGridDblClick(Sender: TObject);
begin
  EditBtnClick(Sender);
end;

procedure TRefForm.PrepareDBGrid(var ATable: TTableInfo);
var i: integer;
begin
  FTable := ATable;
  Caption := FTable.Caption;
  SQLQuery.SQL := FTable.SQL;
  ShowMessage(SQLQuery.SQL.Text);
  FSortedColInd := -1;
  for i := 0 to high(FTable.Fields) do begin
    DBGrid.Columns.Add;
    with DBGrid.Columns[DBGrid.Columns.Count - 1] do begin
      Alignment := taLeftJustify;
      Expanded := True;
      Layout := tlCenter;
      Title.Caption := FTable.Fields[i].Caption;
      //Visible := FTable.Fields[i].Visible;
      //Width := FTable.Fields[i].Width;
      FieldName := FTable.GetColumnName(i);
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
    end;
  end;
  SQLQuery.Open;
end;

procedure TRefForm.InitSearchFrame;
begin
  FSearchFrame := TSearchFrame.Create(FilterPanel);
  FSearchFrame.Left := 2;
  FSearchFrame.Top := 2;
  FSearchFrame.Prepare(FTable, SQLQuery);
  FilterPanel.InsertControl(FSearchFrame);
end;

procedure TRefForm.RemoveInvalidPointers(var AObjects: TEditForms);
var i, j, k: integer;
begin
  j := -1;
  k := 0;
  //ShowMessage(IntToStr(Length(AObjects)));
  for i := 0 to high(AObjects) do
    if not Assigned(AObjects[i]) then begin
      j := i;
      inc(k);
    end
    else
      if j >= 0 then begin
        AObjects[j] := AObjects[i];
        AObjects[i] := nil;
        j := -1;
      end;
  SetLength(AObjects, Length(AObjects)-k);
  //ShowMessage(IntToStr(Length(AObjects)));
end;

end.
