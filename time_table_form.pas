unit time_table_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdCtrls, Menus, Buttons, search_frame, search_filter_frame,
  metadata, math, lazregions;

type

  { TTimeTable }

  TPoints = array of TPoint;
  TIntArray = array of integer;

  TTimeTable = class(TForm)
    AddFilterBtn: TSpeedButton;
    FiltersBox: TGroupBox;
    ShowFieldNames: TCheckBox;
    CheckFields: TCheckGroup;
    DrawGrid: TDrawGrid;
    SelectBtn: TButton;
    Datasource: TDatasource;
    HorizontalFieldBox: TComboBox;
    SQLQuery: TSQLQuery;
    VerticalFieldBox: TComboBox;
    Horizontal: TLabel;
    Vertical: TLabel;
    TopPanel: TPanel;
    FilterPanel: TPanel;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure CheckFieldsItemClick(Sender: TObject; Index: integer);
    procedure CloseFilterClick(AFilterIndex: integer);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure HorizontalFieldBoxChange(Sender: TObject);
    procedure SelectBtnClick(Sender: TObject);
    procedure OnPropChange(Sender: TObject);
    procedure VerticalFieldBoxChange(Sender: TObject);
  private
    procedure InitCheckFields;
    function GetTitles(AFieldIndex: integer; var AIDArray: TIntArray): TStringList;
    function GetTitlesQuery(AFieldIndex: integer): string;
    function GetTriangle(ARect: TRect; ACol, ARow: integer): TPoints;
    procedure DrawTitle(ACol, ARow: integer; ARect: TRect);
    procedure ClearCellData;
  private
    FColTitles, FRowTitles: TStringList;
    FCellData: array of array of array of TStringList;
    FHFIDArray, FVFIDArray: TIntArray;
    FCurHFI, FCurVFI: integer;
    FTable, FMDTable: TTableInfo;
    FFilters: TFilters;
    FDefaultRowHeight, FTextHeight: integer;
  const
    FieldSelectionError: string = 'Поле по горизонтали совпадает с полем по вертикали';
  end;

var
  TimeTable: TTimeTable;

implementation

{$R *.lfm}

{ TTimeTable }

procedure TTimeTable.FormCreate(Sender: TObject);
var i: integer;
begin
  FTable := TimeTableMData.Tables[0];
  FMDTable := MData.Tables[high(MData.Tables)];
  for i := 0 to high(FTable.Fields) do begin
    HorizontalFieldBox.Items.Add(FTable.Fields[i].Caption);
    VerticalFieldBox.Items.Add(FTable.Fields[i].Caption);
  end;
  FCurHFI := 4;
  HorizontalFieldBox.ItemIndex := FCurHFI;
  FCurVFI := 3;
  VerticalFieldBox.ItemIndex := FCurVFI;
  InitCheckFields;
end;

procedure TTimeTable.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  tmp: string;
  CurTop, i, j, k: integer;
begin
  with DrawGrid.Canvas do begin
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Color := clBlack;
    Brush.Style := bsClear;
    Font.Color := clBlack;
  end;

  if (aCol = 0) and (aRow = 0) then Exit;
  if (aCol = 0) xor (aRow = 0) then begin
    DrawTitle(aCol, aRow, aRect);
    Exit;
  end;

  CurTop := aRect.Top;
  for i := 0 to high(FCellData[aCol][aRow]) do begin
    k := 0;
    for j := 0 to FCellData[aCol][aRow][i].Count-1 do begin
      if CheckFields.Checked[j] then begin
        tmp := FCellData[aCol][aRow][i].Strings[j];
        if ShowFieldNames.Checked then
          tmp := Format('%s: %s', [FTable.Fields[j].Caption, tmp]);
        DrawGrid.Canvas.TextOut(aRect.Left+5, CurTop+(FTextHeight+4)*k+2, tmp);
        inc(k);
      end;
    end;
    if (k > 0) and (i < high(FCellData[aCol][aRow])) then begin
      CurTop += (FTextHeight+4)*k+1;
      DrawGrid.Canvas.MoveTo(aRect.Left, CurTop);
      DrawGrid.Canvas.LineTo(aRect.Right, CurTop);
    end;
  end;
  if (Length(FCellData[aCol][aRow]) > 1) and
     (DrawGrid.RowHeights[aRow] < FDefaultRowHeight*Length(FCellData[aCol][aRow]))
  then begin
    DrawGrid.Brush.Style := bsHorizontal;
    DrawGrid.Brush.Color := clRed;
    DrawGrid.Canvas.Polygon(GetTriangle(aRect, aCol, aRow));
  end;
end;

procedure TTimeTable.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TTimeTable.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with DrawGrid do
    if DrawGrid.RowHeights[Row] < FDefaultRowHeight*Length(FCellData[Col][Row]) then
      if IsPointInPolygon(X, Y, GetTriangle(CellRect(Col, Row), Col, Row)) then
        RowHeights[Row] := FDefaultRowHeight*Length(FCellData[Col][Row]);
end;

procedure TTimeTable.SelectBtnClick(Sender: TObject);
var
  PrevCol, PrevRow, i: integer;
  CurCol, CurRow: string;
  CurCell: TStringList;
begin
  FColTitles.Free;
  FColTitles := GetTitles(FCurHFI, FHFIDArray);
  DrawGrid.ColCount := FColTitles.Count+1;

  FRowTitles.Free;
  FRowTitles := GetTitles(FCurVFI, FVFIDArray);
  DrawGrid.RowCount := FRowTitles.Count+1;

  ClearCellData;
  SetLength(FCellData, DrawGrid.ColCount);
  for i := 1 to high(FCellData) do
    SetLength(FCellData[i], DrawGrid.RowCount);

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
          CurCol := SQLQuery.FieldByName(FMDTable.GetColumnName(i)).AsString;
          Continue;
        end;
        if FMDTable.Fields[i].Name = FTable.Fields[FCurVFI].Name then begin
          CurRow := SQLQuery.FieldByName(FMDTable.GetColumnName(i)).AsString;
          Continue;
        end;
        CurCell.Add(SQLQuery.FieldByName(FMDTable.GetColumnName(i)).AsString);
      end;

      if FColTitles.Strings[PrevCol] <> CurCol then begin
        PrevRow := 0;
        for i := PrevCol+1 to FColTitles.Count-1 do
          if CurCol = FColTitles.Strings[i] then begin
            PrevCol := i;
            break;
          end;
      end;
      if FRowTitles.Strings[PrevRow] <> CurRow then begin
        for i := PrevRow+1 to FRowTitles.Count-1 do
          if CurRow = FRowTitles.Strings[i] then begin
            PrevRow := i;
            break;
          end;
      end;
      SetLength(FCellData[PrevCol+1][PrevRow+1], Length(FCellData[PrevCol+1][PrevRow+1])+1);
      FCellData[PrevCol+1][PrevRow+1][high(FCellData[PrevCol+1][PrevRow+1])] := CurCell;
      Next;
    end;
    Close;
  end;
  OnPropChange(self);
end;

procedure TTimeTable.HorizontalFieldBoxChange(Sender: TObject);
begin
  if HorizontalFieldBox.ItemIndex = FCurVFI then begin
    ShowMessage(FieldSelectionError);
    HorizontalFieldBox.ItemIndex := FCurHFI;
    Exit;
  end;
  FCurHFI := HorizontalFieldBox.ItemIndex;
  InitCheckFields;
end;

procedure TTimeTable.VerticalFieldBoxChange(Sender: TObject);
begin
  if VerticalFieldBox.ItemIndex = FCurHFI then begin
    ShowMessage(FieldSelectionError);
    VerticalFieldBox.ItemIndex := FCurVFI;
    Exit;
  end;
  FCurVFI := VerticalFieldBox.ItemIndex;
  InitCheckFields;
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

function TTimeTable.GetTitles(AFieldIndex: integer; var AIDArray: TIntArray): TStringList;
begin
  SetLength(AIDArray, 0);
  Result := TStringList.Create;
  SQLQuery.SQL.Text := GetTitlesQuery(AFieldIndex);
  SQLQuery.Open;
  while not SQLQuery.EOF do begin
    SetLength(AIDArray, Length(AIDArray)+1);
    AIDArray[high(AIDArray)] := SQLQuery.Fields[0].AsInteger;
    Result.Add(SQLQuery.Fields[1].AsString);
    SQLQuery.Next;
  end;
  SQLQuery.Close;
end;

procedure TTimeTable.DrawTitle(ACol, ARow: integer; ARect: TRect);
begin
  with DrawGrid do begin
    if aRow = 0 then Canvas.TextOut(ARect.Left+5, ARect.Top+2, FColTitles[aCol-1])
    else Canvas.TextOut(ARect.Left+5, ARect.Top+2, FRowTitles[aRow-1]);
  end;
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

procedure TTimeTable.ClearCellData;
var i, j, k: integer;
begin
  for i := 0 to high(FCellData) do begin
    for j := 0 to high(FCellData[i]) do begin
      for k := 0 to high(FCellData[i][j]) do
        FCellData[i][j][k].Free;
      SetLength(FCellData[i][j], 0);
    end;
    SetLength(FCellData[i], 0);
  end;
end;

procedure TTimeTable.AddFilterBtnClick(Sender: TObject);
var NewFilter: TFilterFrame;
begin
  NewFilter := AddFilter(FiltersBox, FFilters, FMDTable);
  if NewFilter <> nil then
    with NewFilter do begin
      CloseFilter := @CloseFilterClick;
      FiltersBox.Height := FiltersBox.Height+Height;
    end;
end;

procedure TTimeTable.CloseFilterClick(AFilterIndex: integer);
begin
  FiltersBox.Height := FiltersBox.Height-FFilters[AFilterIndex].Height;
  CloseFilter(FiltersBox, FFilters, AFilterIndex);
end;

procedure TTimeTable.CheckFieldsItemClick(Sender: TObject; Index: integer);
begin
  OnPropChange(self);
end;

procedure TTimeTable.OnPropChange(Sender: TObject);
var Count, i: integer;
begin
  Count := 0;
  for i := 0 to high(FMDTable.Fields)-3 do
    if CheckFields.Checked[i] then inc(Count);
  FTextHeight := DrawGrid.Canvas.TextHeight('А');
  FDefaultRowHeight := (FTextHeight+4)*Count+1;
  DrawGrid.DefaultRowHeight := Max(FDefaultRowHeight, FTextHeight+4);
  DrawGrid.DefaultColWidth := 250;
  DrawGrid.RowHeights[0] := 30;
  DrawGrid.ColWidths[0] := 150;
  DrawGrid.Repaint;
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

end.
