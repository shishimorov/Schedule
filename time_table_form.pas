unit time_table_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdCtrls, Menus, Buttons, search_frame, search_filter_frame,
  metadata, math, lazregions, DbCtrls, types, record_edit_form,
  reference_form, comobj;

type

  { TTimeTable }

  TPoints = array of TPoint;
  TIntArray = array of integer;

  TTimeTable = class(TForm)
    AddFilterBtn: TSpeedButton;
    DeleteImage: TImage;
    FiltersBox: TGroupBox;
    EditImage: TImage;
    MainMenu: TMainMenu;
    MExport: TMenuItem;
    MFile: TMenuItem;
    PMRef: TMenuItem;
    PMAdd: TMenuItem;
    PMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    ShowFieldNames: TCheckBox;
    CheckFields: TCheckGroup;
    DrawGrid: TDrawGrid;
    Datasource: TDatasource;
    HorizontalFieldBox: TComboBox;
    SelectBtn: TSpeedButton;
    SQLQuery: TSQLQuery;
    VerticalFieldBox: TComboBox;
    Horizontal: TLabel;
    Vertical: TLabel;
    TopPanel: TPanel;
    FilterPanel: TPanel;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure CheckFieldsItemClick(Sender: TObject; Index: integer);
    procedure CloseFilterClick(AFilterIndex: integer);
    procedure DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure HorizontalFieldBoxChange(Sender: TObject);
    procedure MExportClick(Sender: TObject);
    procedure PMAddClick(Sender: TObject);
    procedure PMRefClick(Sender: TObject);
    procedure SelectBtnClick(Sender: TObject);
    procedure OnPropChange(Sender: TObject);
    procedure OnFilterChange(Sender: TObject);
    procedure VerticalFieldBoxChange(Sender: TObject);
  private
    procedure InitFieldBoxes;
    procedure InitCheckFields;
    procedure DrawTitle(ACol, ARow: integer; ARect: TRect);
    procedure DrawCell(ACol, ARow: integer; ARect: TRect);
    procedure DragCell(AStartCol, AStartRow, ARecordIndex, AEndCol, AEndRow: integer);
    procedure DrawIcons(ACol, ARow, ATop: integer);
    procedure DrawTriangle(ARect: TRect; ACol, ARow: integer);
    procedure OnTriangleClick(Sender: TObject);
    procedure OnEditBtnClick(ARecordID: integer);
    procedure OnDeleteBtnClick(ARecordID: integer);
    function GetTitles(AFieldIndex: integer; var ATitleIDs: TIntArray): TStringList;
    function GetTitlesQuery(AFieldIndex: integer): string;
    function GetTriangle(ARect: TRect; ACol, ARow: integer): TPoints;
    function GetEditBtnRect(ACol, ARow, ARecordIndex: integer): TRect;
    function GetDeleteBtnRect(ACol, ARow, ARecordIndex: integer): TRect;
    procedure GetCellData;
    procedure ClearCellData;
    procedure AfterEditAction(Sender: TObject);
    procedure SaveToHTML(AFile: string);
    procedure SaveToExcel(AFile: string);
  private
    FTable, FMDTable: TTableInfo;
    FCellData: array of array of array of TStringList;
    FRecordIDs: array of array of array of integer;
    FColTitles, FRowTitles: TStringList;
    FColTitleIDs, FRowTitleIDs: TIntArray;
    FCurHFI, FCurVFI, FCurMDHField, FCurMDVField: integer;
    FCurCol, FCurRow, FCurOrd: integer;
    FDragStartPos: TPoint;
    FFieldsCount, FDefaultRowHeight, FTextHeight: integer;
    FFilters: TFilters;
  const
    FieldSelectionError: string = 'Поле по горизонтали совпадает с полем по вертикали';
  end;

var
  TimeTable: TTimeTable;

implementation

{$R *.lfm}

{ TTimeTable }

procedure TTimeTable.FormCreate(Sender: TObject);
begin
  FTable := TimeTableMData.Tables[0];
  FMDTable := MData.Tables[high(MData.Tables)];
  InitFieldBoxes;
  InitCheckFields;
  FCurCol := 0;
  FCurRow := 0;
end;

procedure TTimeTable.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (aCol = 0) and (aRow = 0) then Exit;
  if (aCol = 0) xor (aRow = 0) then begin
    DrawTitle(aCol, aRow, aRect);
    Exit;
  end;
  DrawCell(aCol, aRow, aRect);
end;

procedure TTimeTable.DrawTitle(ACol, ARow: integer; ARect: TRect);
begin
  with DrawGrid do begin
    if aRow = 0 then Canvas.TextOut(ARect.Left+5, ARect.Top+2, FColTitles[aCol-1])
    else Canvas.TextOut(ARect.Left+5, ARect.Top+2, FRowTitles[aRow-1]);
  end;
end;

procedure TTimeTable.DrawCell(ACol, ARow: integer; ARect: TRect);
var
  tmp: string;
  CurTop, i, j, k: integer;
begin
  CurTop := aRect.Top;
  with DrawGrid.Canvas do begin
    for i := 0 to high(FCellData[aCol][aRow]) do begin
      k := 0;
      for j := 0 to FCellData[aCol][aRow][i].Count-1 do begin
        if CheckFields.Checked[j] then begin
          tmp := FCellData[aCol][aRow][i].Strings[j];
          if ShowFieldNames.Checked then
            tmp := Format('%s: %s', [CheckFields.Items[j], tmp]);
          TextOut(aRect.Left+5, CurTop+FTextHeight*k+2, tmp);
          inc(k);
        end;
      end;
      if FFieldsCount > 0 then begin
        DrawIcons(aCol, aRow, CurTop);
        CurTop += FTextHeight*FFieldsCount+1;
        MoveTo(aRect.Left, CurTop);
        LineTo(aRect.Right, CurTop);
      end;
    end;
    DrawTriangle(aRect, aCol, aRow);
  end;
end;

procedure TTimeTable.DrawIcons(ACol, ARow, ATop: integer);
var Rect: TRect;
begin
  if (FCurCol > 0) and (FCurRow > 0) and (FCurCol = aCol) and (FCurRow = aRow) and
     (FCurOrd > ATop) and (FCurOrd < ATop+FTextHeight*FFieldsCount)
  then begin
    Rect := DrawGrid.CellRect(ACol, ARow);
    DrawGrid.Canvas.Draw(Rect.Right-34, ATop+1, EditImage.Picture.Bitmap);
    DrawGrid.Canvas.Draw(Rect.Right-17, ATop, DeleteImage.Picture.Bitmap);
  end;
end;

procedure TTimeTable.DrawTriangle(ARect: TRect; ACol, ARow: integer);
begin
  if (DrawGrid.RowHeights[ARow] < FDefaultRowHeight*Length(FCellData[ACol][ARow])) and
     (Length(FCellData[ACol][ARow]) > 1)
  then begin
    DrawGrid.Canvas.Brush.Style := bsSolid;
    DrawGrid.Canvas.Brush.Color := clBlack;
    DrawGrid.Canvas.Polygon(GetTriangle(ARect, ACol, ARow));
  end;
end;

procedure TTimeTable.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FDragStartPos := Point(X, Y);
end;

procedure TTimeTable.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var Col, Row: integer;
begin
  FCurOrd := Y;
  DrawGrid.MouseToCell(X, Y, Col, Row);
  if (Col > 0) and (Row > 0) then
    if (Col <> FCurCol) or (Row <> FCurRow) then begin
      FCurCol := Col;
      FCurRow := Row;
    end;
  DrawGrid.Repaint;
end;

procedure TTimeTable.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pCol, pRow: integer;
  i: integer;
begin
  if (DrawGrid.Row <= 0) and (DrawGrid.Col <= 0) then Exit;
  with DrawGrid do
    case Button of
    mbLeft:
      begin
        if FFieldsCount = 0 then Exit;
        if RowHeights[Row] < FDefaultRowHeight*Length(FCellData[Col][Row]) then
          if IsPointInPolygon(X, Y, GetTriangle(CellRect(Col, Row), Col, Row)) then begin
            OnTriangleClick(self);
            Exit;
          end;

        for i := 0 to high(FCellData[Col][Row]) do begin
          if PtInRect(GetEditBtnRect(Col, Row, i), Point(X, Y)) then begin
            OnEditBtnClick(FRecordIDs[Col][Row][i]);
            Exit;
          end;
          if PtInRect(GetDeleteBtnRect(Col, Row, i), Point(X, Y)) then begin
            OnDeleteBtnClick(FRecordIDs[Col][Row][i]);
            Exit;
          end;
        end;
      end;
    mbRight:
      begin
        MouseToCell(X, Y, pCol, pRow);
        if (pCol > 0) and (pRow > 0) then begin
          Col := pCol;
          Row := pRow;
          PMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;
      end;
    end;
end;

function TTimeTable.GetEditBtnRect(ACol, ARow, ARecordIndex: integer): TRect;
begin
  Result := DrawGrid.CellRect(ACol, ARow);
  with Result do begin
    Left := Right-34;
    Right := Right-18;
    Top := Top + ARecordIndex*(FTextHeight*FFieldsCount+1);
    Bottom := Top+16;
  end;
end;

function TTimeTable.GetDeleteBtnRect(ACol, ARow, ARecordIndex: integer): TRect;
begin
  Result := DrawGrid.CellRect(ACol, ARow);
  with Result do begin
    Left := Left+17;
    Right := Right+17;
    Top := Top + ARecordIndex*(FTextHeight*FFieldsCount+1);
    Bottom := Top+16;
  end;
end;

procedure TTimeTable.OnTriangleClick(Sender: TObject);
begin
  with DrawGrid do
    RowHeights[Row] := FDefaultRowHeight*Length(FCellData[Col][Row]);
end;

procedure TTimeTable.OnEditBtnClick(ARecordID: integer);
var EditForm: TEditForm;
begin
  EditForm := TEditForm.Create(self, FMDTable, ARecordID, @AfterEditAction,
    [0, FCurMDHField, FCurMDVField]);
  EditForm.ShowModal;
end;

procedure TTimeTAble.OnDeleteBtnClick(ARecordID: integer);
begin
  if Application.MessageBox('Вы действительно хотите удалить данную запись?',
     'Подтверждение', (4+48)) = 6
   then begin
     SQLQuery.SQL.Text :=
       Format('DELETE FROM %s WHERE ID = %d', [FTable.Name, ARecordID]);
     SQLQuery.ExecSQL;
     AfterEditAction(self);
   end;
end;

procedure TTimeTable.PMAddClick(Sender: TObject);
var EditForm: TEditForm;
begin
  EditForm := TEditForm.Create(self, FMDTable, @AfterEditAction, [0, FCurMDHField, FCurMDVField]);
  with EditForm do begin
    FieldEdits[FCurMDHField].Value := FColTitleIDs[DrawGrid.Col-1];
    FieldEdits[FCurMDVField].Value := FRowTitleIDs[DrawGrid.Row-1];
    Show;
  end;
end;

procedure TTimeTable.PMRefClick(Sender: TObject);
var RefForm: TRefForm;
begin
  RefForm := TRefForm.Create(self, FMDTable);
  with RefForm do begin
    SearchFrame.AddFilter(FCurMDHField, FColTitles[DrawGrid.Col-1]);
    SearchFrame.AddFilter(FCurMDVField, FRowTitles[DrawGrid.Row-1]);
    SearchFrame.SearchBtnClick(self);
    Show;
  end;
end;

procedure TTimeTable.DrawGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  if not IsColumn then
    DrawGrid.RowHeights[Index] := FDefaultRowHeight;
end;

procedure TTimeTable.SelectBtnClick(Sender: TObject);
var i: integer;
begin
  InitCheckFields;

  FColTitles.Free;
  FRowTitles.Free;
  FColTitles := GetTitles(FCurHFI, FColTitleIDs);
  FRowTitles := GetTitles(FCurVFI, FRowTitleIDs);
  DrawGrid.ColCount := FColTitles.Count+1;
  DrawGrid.RowCount := FRowTitles.Count+1;

  ClearCellData;
  GetCellData;
  OnPropChange(self);
  SelectBtn.Enabled := False;

  for i := 1 to DrawGrid.RowCount-1 do
    DrawGrid.RowHeights[i] := DrawGrid.DefaultRowHeight;
end;

procedure TTimeTable.DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var Col, Row: integer;
begin
  DrawGrid.MouseToCell(X, Y, Col, Row);
  Accept := (Col > 0) and (Row > 0);
end;

procedure TTimeTable.DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  cRect: TRect;
  sCol, sRow, RecordIndex, i: integer;
begin
  with DrawGrid do begin
    if FFieldsCount > 0 then begin
      RecordIndex := -1;
      cRect := CellRect(Col, Row);
      cRect.Bottom := cRect.Top + FTextHeight*FFieldsCount+1;
      for i := 0 to high(FCellData[Col][Row]) do begin
        if PtInRect(cRect, FDragStartPos) then begin
          RecordIndex := i;
          break;
        end;
        cRect.Top := cRect.Bottom;
        cRect.Bottom += FTextHeight*FFieldsCount+1;
      end;

      if RecordIndex >= 0 then begin
        MouseToCell(X, Y, sCol, sRow);
        DragCell(Col, Row, RecordIndex, sCol, sRow);
      end;
    end;
  end
end;

procedure TTimeTable.DragCell(AStartCol, AStartRow, ARecordIndex, AEndCol, AEndRow: integer);
var
  Cell: TStringList;
  RecID, i: integer;
begin
  RecID := FRecordIDs[AStartCol][AStartRow][ARecordIndex];
  Cell := FCellData[AStartCol][AStartRow][ARecordIndex];

  for i := ARecordIndex to high(FCellData[AStartCol][AStartRow])-1 do begin
    FCellData[AStartCol][AStartRow][i] := FCellData[AStartCol][AStartRow][i+1];
    FRecordIDs[AStartCol][AStartRow][i] := FRecordIDs[AStartCol][AStartRow][i+1];
  end;
  SetLength(FCellData[AStartCol][AStartRow], Length(FCellData[AStartCol][AStartRow])-1);
  SetLength(FRecordIDs[AStartCol][AStartRow], Length(FRecordIDs[AStartCol][AStartRow])-1);

  SetLength(FCellData[AEndCol][AEndRow], Length(FCellData[AEndCol][AEndRow])+1);
  FCellData[AEndCol][AEndRow][high(FCellData[AEndCol][AEndRow])] := Cell;
  SetLength(FRecordIDs[AEndCol][AEndRow], Length(FRecordIDs[AEndCol][AEndRow])+1);
  FRecordIDs[AEndCol][AEndRow][high(FRecordIDs[AEndCol][AEndRow])] := RecID;

  DrawGrid.Repaint;

  SQLQuery.SQL.Text :=
    Format('UPDATE %s SET %s = %d, %s = %d WHERE ID = %d',
      [FTable.Name, FMDTable.Fields[FCurMDHField].Name, FColTitleIDs[AEndCol-1],
       FMDTable.Fields[FCurMDVField].Name, FRowTitleIDs[AEndRow-1], RecID]);
  SQLQuery.ExecSQL;
end;

procedure TTimeTable.InitFieldBoxes;
var i: integer;
begin
  for i := 0 to high(FTable.Fields) do begin
    HorizontalFieldBox.Items.Add(FTable.Fields[i].Caption);
    VerticalFieldBox.Items.Add(FTable.Fields[i].Caption);
  end;
  FCurHFI := 4;
  HorizontalFieldBox.ItemIndex := FCurHFI;
  FCurVFI := 3;
  VerticalFieldBox.ItemIndex := FCurVFI;
end;

procedure TTimeTable.InitCheckFields;
var i: integer;
begin
  CheckFields.Items.Clear;
  for i := 1 to high(FMDTable.Fields) do begin
    if (FTable.Fields[FCurHFI].Name <> FMDTable.Fields[i].Name) and
       (FTable.Fields[FCurVFI].Name <> FMDTable.Fields[i].Name) then
      CheckFields.Checked[CheckFields.Items.Add(FMDTable.Fields[i].Caption)] := True;
  end;
end;

procedure TTimeTable.HorizontalFieldBoxChange(Sender: TObject);
begin
  SelectBtn.Enabled := True;
  if HorizontalFieldBox.ItemIndex = FCurVFI then begin
    ShowMessage(FieldSelectionError);
    HorizontalFieldBox.ItemIndex := FCurHFI;
    Exit;
  end;
  FCurHFI := HorizontalFieldBox.ItemIndex;
end;

procedure TTimeTable.MExportClick(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    case SaveDialog.FilterIndex of
    1: SaveToHTML(SaveDialog.FileName + '.html');
    2: SaveToExcel(SaveDialog.FileName + '.xlsx');
    end;
  end;
end;

procedure TTimeTable.VerticalFieldBoxChange(Sender: TObject);
begin
  SelectBtn.Enabled := True;
  if VerticalFieldBox.ItemIndex = FCurHFI then begin
    ShowMessage(FieldSelectionError);
    VerticalFieldBox.ItemIndex := FCurVFI;
    Exit;
  end;
  FCurVFI := VerticalFieldBox.ItemIndex;
end;

procedure TTimeTable.GetCellData;
var
  PrevCol, PrevRow, i: integer;
  CurCol, CurRow: string;
  CurCell: TStringList;
begin
  SetLength(FCellData, DrawGrid.ColCount);
  SetLength(FRecordIDs, DrawGrid.ColCount);
  for i := 1 to high(FCellData) do begin
    SetLength(FCellData[i], DrawGrid.RowCount);
    SetLength(FRecordIDs[i], DrawGrid.RowCount);
  end;

  with SQLQuery do begin
    SQL := FMDTable.SQL;
    GetFiltersSQL(FFilters, SQLQuery);
    SQL.Strings[ORDER_BY_IND] := Format('ORDER BY %s, %s',
      [FTable.GetOrderByFieldName(FCurHFI), FTable.GetOrderByFieldName(FCurVFI)]);
    Open;

    PrevCol := 0;
    PrevRow := 0;
    while not EOF do begin
      CurCell := TStringList.Create;
      for i := 1 to high(FMDTable.Fields) do begin
        if FMDTable.Fields[i].Name = FTable.Fields[FCurHFI].Name then begin
          FCurMDHField := i;
          CurCol := SQLQuery.FieldByName(FMDTable.GetColumnName(i)).AsString;
          Continue;
        end;
        if FMDTable.Fields[i].Name = FTable.Fields[FCurVFI].Name then begin
          FCurMDVField := i;
          CurRow := SQLQuery.FieldByName(FMDTable.GetColumnName(i)).AsString;
          Continue;
        end;
        CurCell.Add(SQLQuery.FieldByName(FMDTable.GetColumnName(i)).AsString);
      end;

      if FColTitles[PrevCol] <> CurCol then begin
        PrevRow := 0;
        for i := PrevCol+1 to FColTitles.Count-1 do
          if CurCol = FColTitles[i] then begin
            PrevCol := i;
            break;
          end;
      end;
      if FRowTitles[PrevRow] <> CurRow then begin
        for i := PrevRow+1 to FRowTitles.Count-1 do
          if CurRow = FRowTitles[i] then begin
            PrevRow := i;
            break;
          end;
      end;
      SetLength(FCellData[PrevCol+1][PrevRow+1], Length(FCellData[PrevCol+1][PrevRow+1])+1);
      FCellData[PrevCol+1][PrevRow+1][high(FCellData[PrevCol+1][PrevRow+1])] := CurCell;
      SetLength(FRecordIDs[PrevCol+1][PrevRow+1], Length(FRecordIDs[PrevCol+1][PrevRow+1])+1);
      FRecordIDs[PrevCol+1][PrevRow+1][high(FRecordIDs[PrevCol+1][PrevRow+1])] :=
        SQLQuery.FieldByName('ID').AsInteger;
      Next;
    end;
    Close;
  end;
end;

procedure TTimeTable.ClearCellData;
var i, j, k: integer;
begin
  for i := 0 to high(FCellData) do begin
    for j := 0 to high(FCellData[i]) do begin
      for k := 0 to high(FCellData[i][j]) do
        FCellData[i][j][k].Free;
      SetLength(FCellData[i][j], 0);
      SetLength(FRecordIDs[i][j], 0);
    end;
    SetLength(FCellData[i], 0);
    SetLength(FRecordIDs[i], 0);
  end;
end;

function TTimeTable.GetTitles(AFieldIndex: integer; var ATitleIDs: TIntArray): TStringList;
begin
  SetLength(ATitleIDs, 0);
  Result := TStringList.Create;
  SQLQuery.SQL.Text := GetTitlesQuery(AFieldIndex);
  SQLQuery.Open;
  while not SQLQuery.EOF do begin
    SetLength(ATitleIDs, Length(ATitleIDs)+1);
    ATitleIDs[high(ATitleIDs)] := SQLQuery.Fields[0].AsInteger;
    Result.Add(SQLQuery.Fields[1].AsString);
    SQLQuery.Next;
  end;
  SQLQuery.Close;
end;

function TTimeTable.GetTitlesQuery(AFieldIndex: integer): string;
begin
  with FTable.Fields[AFieldIndex] as TRefFieldInfo do
    if OrderByField = '' then
      Result :=
        Format('SELECT ID, %s FROM %s ORDER BY 2', [ListFieldName, RefTableName])
    else
      Result :=
        Format('SELECT ID, %s, %s FROM %s ORDER BY 3', [ListFieldName, OrderByField, RefTableName]);
end;

function TTimeTable.GetTriangle(ARect: TRect; ACol, ARow: integer): TPoints;
begin
  SetLength(Result, 3);
  Result[0].X := ARect.Left+DrawGrid.ColWidths[ACol];
  Result[0].Y := ARect.Top+DrawGrid.RowHeights[ARow]-20;
  Result[1].X := ARect.Left+DrawGrid.ColWidths[ACol];
  Result[1].Y := ARect.Top+DrawGrid.RowHeights[ARow];
  Result[2].X := ARect.Left+DrawGrid.ColWidths[ACol]-20;
  Result[2].Y := ARect.Top+DrawGrid.RowHeights[ARow];
end;

procedure TTimeTable.AddFilterBtnClick(Sender: TObject);
var NewFilter: TFilterFrame;
begin
  NewFilter := AddFilter(FiltersBox, FFilters, FMDTable);
  if NewFilter <> nil then
    with NewFilter do begin
      OnChange := @OnFilterChange;
      OnClose := @CloseFilterClick;
      FiltersBox.Height := FiltersBox.Height+Height;
    end;
end;

procedure TTimeTable.CloseFilterClick(AFilterIndex: integer);
begin
  FiltersBox.Height := FiltersBox.Height-FFilters[AFilterIndex].Height;
  CloseFilter(FiltersBox, FFilters, AFilterIndex);
end;

procedure TTimeTable.AfterEditAction(Sender: TObject);
begin
  ClearCellData;
  GetCellData;
  DrawGrid.Repaint;
end;

procedure TTimeTable.CheckFieldsItemClick(Sender: TObject; Index: integer);
begin
  OnPropChange(self);
end;

procedure TTimeTable.OnFilterChange(Sender: TObject);
begin
  SelectBtn.Enabled := True;
end;

procedure TTimeTable.OnPropChange(Sender: TObject);
var i: integer;
begin
  FFieldsCount := 0;
  for i := 0 to high(FMDTable.Fields)-3 do
    if CheckFields.Checked[i] then inc(FFieldsCount);
  FTextHeight := DrawGrid.Canvas.TextHeight('А')+4;
  FDefaultRowHeight := FTextHeight*FFieldsCount+1;
  DrawGrid.DefaultRowHeight := Max(FDefaultRowHeight, FTextHeight);
  DrawGrid.DefaultColWidth := 250;
  DrawGrid.RowHeights[0] := 30;
  DrawGrid.ColWidths[0] := 150;
  DrawGrid.Repaint;
end;

procedure TTimeTable.SaveToExcel(AFile: string);
var
  Excel: Variant;
  Str: string;
  MaxRowHeight, RowTitleIndex, RowIndex, i, j, k, z: integer;

  function StrToEx(AStr: string): WideString;
  begin
    Result := WideString(UTF8ToSys(AStr));
  end;

  procedure PrepareLine(ALineIndex: integer);
  begin
    Excel.Selection.Borders[ALineIndex].LineStyle := 1;
    Excel.Selection.Borders[ALineIndex].ColorIndex := -4105;
    Excel.Selection.Borders[ALineIndex].TintAndShade := 0;
    Excel.Selection.Borders[ALineIndex].Weight := 4;
  end;

begin
  try
    self.Enabled := False;
    Excel := CreateOleObject('Excel.Application');
    Excel.Workbooks.Add;
    Excel.Visible := False;
    Excel.Application.DisplayAlerts := False;
    Excel.Application.EnableEvents := False;

    Excel.Cells[1,1] := StrToEx(VerticalFieldBox.Items[FCurVFI]);
    RowTitleIndex := 2;
    for i := 0 to FRowTitles.Count-1 do begin
      MaxRowHeight := 1;
      for j := 0 to FColTitles.Count-1 do begin
        RowIndex := RowTitleIndex;
        for k := 0 to high(FCellData[j+1][i+1]) do begin
          if ShowFieldNames.Checked then begin
          Str := '';
          for z := 0 to FCellData[j+1][i+1][k].Count-1 do
            Str += Format('%s: %s',
              [CheckFields.Items[z], FCellData[j+1][i+1][k].Strings[z]]) + #13#10;
            Excel.Cells[RowIndex, j+2] := StrToEx(Str);
          end
          else
            Excel.Cells[RowIndex, j+2] := StrToEx(FCellData[j+1][i+1][k].Text);
          Inc(RowIndex);
        end;
        MaxRowHeight := Max(MaxRowHeight, Length(FCellData[j+1][i+1]));
      end;
      Excel.Cells[RowTitleIndex, 1] := StrToEx(FRowTitles.Strings[i]);
      Excel.Range[Excel.Cells[RowTitleIndex, 1], Excel.Cells[RowTitleIndex, FColTitles.Count+1]].Select;
      PrepareLine(8);
      RowTitleIndex += MaxRowHeight;
    end;

    for i := 0 to FColTitles.Count-1 do begin
      Excel.Cells[1,i+2] := StrToEx(FColTitles.Strings[i]);
      Excel.Range[Excel.Cells[1,i+2], Excel.Cells[RowTitleIndex-1,i+2]].Select;
      PrepareLine(7);
      PrepareLine(9);
    end;

    Excel.ActiveSheet.ListObjects.Add(1, Excel.Range[Excel.Cells[1,1],
      Excel.Cells[RowTitleIndex-1, FColTitles.Count+1]], 0, 1).Name := 'TimeTable';
    Excel.Range[Excel.Cells[1,1], Excel.Cells[RowTitleIndex-1, FColTitles.Count+1]].Select;
    Excel.ActiveSheet.ListObjects['TimeTable'].TableStyle := 'TableStyleMedium12';
    Excel.ActiveSheet.ListObjects['TimeTable'].ShowTableStyleFirstColumn := True;
    Excel.Selection.WrapText := True;
    Excel.Selection.RowHeight := 110;
    Excel.Selection.ColumnWidth := 30;
    Excel.Selection.HorizontalAlignment := -4131;
    Excel.Selection.VerticalAlignment := -4160;
    for i := 7 to 10 do PrepareLine(i);

    Excel.Cells[1,1].Select;
    Excel.ActiveWorkBook.SaveAs(StrToEx(AFile));
    Excel.ActiveWorkbook.Close;
    ShowMessage('Экспорт выполнен успешно');
  finally
    self.Enabled := True;
    Excel.Application.Quit;
  end;
end;

procedure TTimeTable.SaveToHTML(AFile: string);
var
  f: text;
  str: string;
  i, j, k, q: integer;
begin
  System.Assign(f, UTF8ToSys(AFile));
  Rewrite(f);
  writeln(f,
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"><html>' + #13#10 +
    '<head>' +                                                                #13#10 +
    '<meta http-equiv="content-type" content="text/html; charset=utf-8">' +   #13#10 +
    '<title>Расписание</title>' +                                             #13#10 +
    '<style>' +                                                               #13#10 +
    'table {' +                                                               #13#10 +
    '  border-right: 1px solid #000000;' +                                    #13#10 +
    '  border-bottom: 1px solid #000000;' +                                   #13#10 +
    '  margin: 10px;}' +                                                      #13#10 +
    'table td {' +                                                            #13#10 +
    '  min-width: 200px;' +                                                   #13#10 +
    '  max-width: 200px;' +                                                   #13#10 +
    '  border-top: 1px solid #000000;' +                                      #13#10 +
    '  border-left: 1px solid #000000;' +                                     #13#10 +
    '  font-family: Verdana;' +                                               #13#10 +
    '  font-size: 10px;' +                                                    #13#10 +
    '  text-align: left;' +                                                   #13#10 +
    '  padding: 3px;' +                                                       #13#10 +
    '  margin: 0px;' +                                                        #13#10 +
    '  vertical-align: top;}' +                                               #13#10 +
    '.separator {' +                                                          #13#10 +
    '  width: 100%;' +                                                        #13#10 +
    '  height: 1px;' +                                                        #13#10 +
    '  margin: 3px auto 3px auto;' +                                          #13#10 +
    '  background: #bbbbbb;}' +                                               #13#10 +
    '.h {' +                                                                  #13#10 +
    '  font-weight: bold;' +                                                  #13#10 +
    '  text-align: center;' +                                                 #13#10 +
    '  font-size: 12px;' +                                                    #13#10 +
    '  vertical-align: middle;}' +                                            #13#10 +
    'fieldset {' +                                                            #13#10 +
    '  margin: 10px;' +                                                       #13#10 +
    '  font-size: 12px;' +                                                    #13#10 +
    '  border: 1px solid #000;}' +                                            #13#10 +
    'fieldset legend {' +                                                     #13#10 +
    '  font-size: 13px;}' +                                                   #13#10 +
    'ol {' +                                                                  #13#10 +
    '  margin: 5px;}' +                                                       #13#10 +
    'ol li {' +                                                               #13#10 +
    '  margin-top: 5px;}' +                                                   #13#10 +
    '</style>' +                                                              #13#10 +
    '</head>' +                                                               #13#10 +
    '<body>');

  writeln(f,
    '<fieldset><legend><strong>Фильтры</strong></legend><ol>' +               #13#10 +
    '  <li><i>По горизонтали:</i>' +                                          #13#10 +
       HorizontalFieldBox.Items[FCurHFI] +                                    #13#10 +
    '  </li>' +                                                               #13#10 +
    '  <li><i>По вертикали:</i>' +                                            #13#10 +
       VerticalFieldBox.Items[FCurVFI] +                                      #13#10 +
    '  </li>');

  for i := 0 to high(FFilters) do begin
    if FFilters[i].FilterEdit.Value = '' then Continue;
    writeln(f,
      '<li><i>' + FFilters[i].FieldBox.Items[FFilters[i].FieldBox.ItemIndex] +
      ' ' + FFilters[i].OperBox.Items[FFilters[i].OperBox.ItemIndex] +
      '</i> ' + FFilters[i].FilterEdit.Value + '</li>');
  end;

  writeln(f,
    '</ol></fieldset>' +                                                      #13#10 +
    '<table border = "0" cellspacing = "0" cellpadding = "0">' +              #13#10 +
    '<tr valign = "top">' +                                                   #13#10 +
    '<td></td>');

  for i := 0 to FColTitles.Count-1 do
    write(f, #13#10 + '<td class = "h">' + FColTitles.Strings[i] + '</td>');
  writeln(f, #13#10 + '</tr>' + #13#10);

  for i := 0 to FRowTitles.Count-1 do begin
    writeln(f,
      '<tr valign = "top">' + #13#10 +
      '<td class = "h">' + FRowTitles.Strings[i] + '</td>');

    for j := 0 to FColTitles.Count-1 do begin
      writeln(f, '<td>');
      for k := 0 to high(FCellData[j+1][i+1]) do begin
        for q := 0 to FCellData[j+1][i+1][k].Count-1 do begin
          if not CheckFields.Checked[q] then Continue;

          str := FCellData[j+1][i+1][k].Strings[q];
          if ShowFieldNames.Checked then
            str := Format('<b>%s:</b> %s<br />', [CheckFields.Items[q], str]);
          writeln(f, str);
        end;
        writeln(f, '<div class = "separator">&nbsp</div>');
      end;
      writeln(f, '</td>');
    end;
    writeln(f, '</tr>' + #13#10);
  end;

  writeln(f, '</table>' + #13#10 + '</body>');
  System.Close(f);
end;

end.
