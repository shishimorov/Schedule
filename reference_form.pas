unit reference_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, DBGrids, DbCtrls, ActnList, Menus, ExtCtrls, Buttons, EditBtn,
  metadata, search_frame, row_edit_form, data, confirm_form;

type

  { TRefForm }

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
    function GetColumnName(AFieldName: string): string;
  private
    FEditForm: TEditForm;
    FEditQueryArr: array of TSQLQuery;
    FEditDSArr: array of TDataSource;
    FSearchFrame: TSearchFrame;
    FTable: TTableInfo;
    FSortedColInd: integer;
  end;

implementation

{$R *.lfm}

procedure TRefForm.DBGridTitleClick(Column: TColumn);
var ColInd, i: integer;
begin
  SQLQuery.Close;

  if FSortedColInd <> -1 then
    DBGrid.Columns[FSortedColInd].Title.ImageIndex := -1;

  for i := 0 to DBGrid.Columns.Count - 1 do
    if DBGrid.Columns[i].FieldName = Column.FieldName then begin
      ColInd := i;
      break;
    end;

  if FSortedColInd <> ColInd then DBGrid.SortOrder := soAscending;

  if DBGrid.SortOrder = soAscending then begin
    DBGrid.SortOrder := soDescending;
    SQLQuery.SQL.Strings[ORDER_BY_IND] := Format('ORDER BY %d desc', [ColInd + 1]);
    Column.Title.ImageIndex := 1;
  end
  else begin
    DBGrid.SortOrder := soAscending;
    SQLQuery.SQL.Strings[ORDER_BY_IND] := Format('ORDER BY %d asc', [ColInd + 1]);
    Column.Title.ImageIndex := 0;
  end;

  FSortedColInd := ColInd;
  SQLQuery.Open;
end;

procedure TRefForm.InsertBtnClick(Sender: TObject);
begin
  FEditForm.Free;
  FEditForm := TEditForm.Create(self);
  FEditForm.Prepare(FTable, DataSource, SQLQuery, DBGrid.Columns, FEditDSArr, qmInsert);
  FEditForm.ShowModal;
end;

procedure TRefForm.DeleteBtnClick(Sender: TObject);
begin
  ConfirmForm := TConfirmForm.Create(self);
  ConfirmForm.Prepare(SQLQuery, FTable.Name);
  ConfirmForm.ShowModal;
end;

procedure TRefForm.EditBtnClick(Sender: TObject);
begin
  FEditForm.Free;
  FEditForm := TEditForm.Create(self);
  FEditForm.Prepare(FTable, DataSource, SQLQuery, DBGrid.Columns, FEditDSArr, qmUpdate);
  FEditForm.ShowModal;
end;

procedure TRefForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FEditForm.Free;
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
      FieldName := GetColumnName(FTable.Fields[i].GetFieldName);
    end;

    if FTable.Fields[i] is TRefFieldInfo then begin
      SetLength(FEditQueryArr, Length(FEditQueryArr)+1);
      FEditQueryArr[high(FEditQueryArr)] := TSQLQuery.Create(self);
      with FEditQueryArr[high(FEditQueryArr)] do begin
        DataBase := DBCon.IBConnection;
        Transaction := DBCon.SQLTransaction;
        SQL.Text := Format('SELECT t.%s, t.%s FROM %s t',
          [(FTable.Fields[i] as TRefFieldInfo).RefFieldName,
          (FTable.Fields[i] as TRefFieldInfo).FieldName,
          (FTable.Fields[i] as TRefFieldInfo).RefTableName]);
        Open;
      end;

      SetLength(FEditDSArr, Length(FEditDSArr)+1);
      FEditDSArr[high(FEditDSArr)] := TDataSource.Create(self);
      FEditDSArr[high(FEditDSArr)].DataSet := FEditQueryArr[high(FEditQueryArr)];

      FEditQueryArr[high(FEditQueryArr)].Open;
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

function TRefForm.GetColumnName(AFieldName: string): string;
var i, j: integer;
begin
  if (Pos('"', AFieldName) = 1) and (AFieldName[Length(AFieldName)] = '"') then
  begin
    Delete(AFieldName, 1, 1);
    Delete(AFieldName, Length(AFieldName), 1);
  end;
  j := 0;
  for i := 0 to DBGrid.Columns.Count - 1 do
    if Pos(AFieldName, DBGrid.Columns[i].FieldName) > 0 then inc(j);
  if j = 0 then
    Result := AFieldName
  else
    Result := Format('%s_%d', [AFieldName, j]);
end;

end.
