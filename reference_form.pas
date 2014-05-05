unit reference_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, DBGrids, DbCtrls, ActnList, Menus, ExtCtrls, Buttons, EditBtn,
  metadata, search_frame, record_edit_form;

type

  { TRefForm }

  TEditForms = array of TEditForm;

  TRefForm = class(TForm)
    Datasource: TDatasource;
    TempDS: TDatasource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    InsertBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    EditBtn: TSpeedButton;
    TempQuery: TSQLQuery;
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
    constructor Create(TheOwner: TComponent; ATable: TTableInfo); overload;
    procedure InitSearchFrame;
  private
    procedure AfterEditAction(Sender: TObject);
    procedure RemoveInvalidPointers;
  public
    SearchFrame: TSearchFrame;
  private
    FTable: TTableInfo;
    FEditForms: TEditForms;
    FSortedColInd: integer;
  const
    MaxEditForms = 10;
    MB_YES = 6;
    MB_YESNO = 4;
    MB_ICONWARNING = 48;
  end;

implementation

{$R *.lfm}

constructor TRefForm.Create(TheOwner: TComponent; ATable: TTableInfo); overload;
var i: integer;
begin
  inherited Create(TheOwner);
  FTable := ATable;
  Caption := FTable.Caption;
  SQLQuery.SQL := FTable.SQL;
  FSortedColInd := -1;
  for i := 0 to high(FTable.Fields) do begin
    DBGrid.Columns.Add;
    with DBGrid.Columns[DBGrid.Columns.Count - 1] do begin
      Alignment := taLeftJustify;
      Expanded := True;
      Layout := tlCenter;
      Title.Caption := FTable.Fields[i].Caption;
      FieldName := FTable.GetColumnName(i);
    end;
  end;
  SQLQuery.Open;
  InitSearchFrame;
end;

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
  SQLQuery.Open;
end;

procedure TRefForm.InsertBtnClick(Sender: TObject);
var InsertForm: TEditForm;
begin
  InsertForm := TEditForm.Create(self, FTable, @AfterEditAction, [0]);
  InsertForm.Show;
end;

procedure TRefForm.DeleteBtnClick(Sender: TObject);
begin
  if Application.MessageBox('Вы действительно хотите удалить данную запись?',
    'Подтверждение', (MB_YESNO + MB_ICONWARNING)) = MB_YES
  then begin
    TempQuery.SQL.Text := Format('DELETE FROM %s WHERE ID = %d',
      [FTable.Name, SQLQuery.FieldByName('ID').AsInteger]);
    TempQuery.ExecSQL;
    SQLQuery.Refresh;
  end;
end;

procedure TRefForm.EditBtnClick(Sender: TObject);
var RecID, i: integer;
begin
  if Length(FEditForms) > MaxEditForms then RemoveInvalidPointers;
  RecID := SQLQuery.FieldByName('ID').AsInteger;
  for i := 0 to high(FEditForms) do
    if FEditForms[i].Tag = RecID then begin
      FEditForms[i].Show;
      Exit;
    end;
  SetLength(FEditForms, Length(FEditForms)+1);
  FEditForms[high(FEditForms)] := TEditForm.Create(self, FTable, RecID, @AfterEditAction, [0]);
  with FEditForms[high(FEditForms)] do begin
    Distance := Datasource.DataSet.RecNo-1;
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

procedure TRefForm.InitSearchFrame;
begin
  SearchFrame := TSearchFrame.Create(FilterPanel, FTable, SQLQuery);
  SearchFrame.Left := 2;
  SearchFrame.Top := 2;
  FilterPanel.InsertControl(SearchFrame);
end;

procedure TRefForm.AfterEditAction(Sender: TObject);
begin
  SQLQuery.Refresh;
  SQLQuery.MoveBy((Sender as TEditForm).Distance);
end;

procedure TRefForm.RemoveInvalidPointers;
var i, j, k: integer;
begin
  j := -1;
  k := 0;
  for i := 0 to high(FEditForms) do begin
    if Assigned(FEditForms[i]) then
      if j >= 0 then begin
        FEditForms[j] := FEditForms[i];
        FEditForms[i] := nil;
        j := -1;
      end
    else begin
      j := i;
      inc(k);
    end;
  end;
  SetLength(FEditForms, Length(FEditForms)-k);
end;

end.
