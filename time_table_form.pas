unit time_table_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdCtrls, Menus, Buttons, search_frame, search_filter_frame,
  metadata, data, math;

type

  { TTimeTable }

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
    procedure CloseFilterClick(AFilterIndex: integer);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure HorizontalFieldBoxChange(Sender: TObject);
    procedure SelectBtnClick(Sender: TObject);
    procedure VerticalFieldBoxChange(Sender: TObject);
  private
    procedure InitCheckFields;
    function GetTitles(AFieldIndex: integer; var AIDArray: TIntArray): TStringList;
    function GetTitlesQuery(AFieldIndex: integer): string;
    procedure DrawTitles(ACol, ARow: integer; ARect: TRect);
    procedure ClearCellData;
  private
    FColTitles, RowTitles: TStringList;
    FCellData: array of array of array of TStringList;
    FHFIDArray, FVFIDArray: TIntArray;
    FCurHFI, FCurVFI: integer;
    FTable, FMDTable: TTableInfo;
    FFilters: TFilters;
    FRowHeight, FTextHeight: integer;
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
  CurTop: integer;
  i, j: integer;
begin
  if (aCol = 0) and (aRow = 0) then Exit;
  if (aCol = 0) xor (aRow = 0) then begin
    DrawTitles(aCol, aRow, aRect);
    Exit;
  end;

  CurTop := aRect.Top;
  for i := 0 to high(FCellData[aCol][aRow]) do begin
    for j := 0 to FCellData[aCol][aRow][i].Count-1 do begin
      DrawGrid.Canvas.TextOut(
        aRect.Left+5, CurTop+(FTextHeight+4)*j+2, FCellData[aCol][aRow][i].Strings[j]);
    end;
    CurTop += (FTextHeight+4)*FCellData[aCol][aRow][i].Count+1;
    DrawGrid.Canvas.MoveTo(aRect.Left, CurTop);
    DrawGrid.Canvas.LineTo(aRect.Right, CurTop);
  end;
end;

procedure TTimeTable.SelectBtnClick(Sender: TObject);
var
  FCounter, i, j, k: integer;
  tmp: string;
begin
  FColTitles.Free;
  FColTitles := GetTitles(FCurHFI, FHFIDArray);
  DrawGrid.ColCount := FColTitles.Count+1;

  RowTitles.Free;
  RowTitles := GetTitles(FCurVFI, FVFIDArray);
  DrawGrid.RowCount := RowTitles.Count+1;

  ClearCellData;
  SetLength(FCellData, DrawGrid.ColCount);
  SQLQuery.SQL := FMDTable.SQL;
  for i := 1 to DrawGrid.ColCount-1 do begin
    SetLength(FCellData[i], DrawGrid.RowCount);
    for j := 1 to DrawGrid.RowCount-1 do begin
      SQLQuery.SQL.Strings[WHERE_IND] := Format('WHERE %s = %d and %s = %d',
        [FTable.Fields[FCurHFI].Name, FHFIDArray[i-1],
         FTable.Fields[FCurVFI].Name, FVFIDArray[j-1]]);
      GetFiltersSQL(FFilters, SQLQuery);
      //ShowMessage(SQLQuery.SQL.Text);
      SQLQuery.Open;
      while not SQLQuery.EOF do begin
        SetLength(FCellData[i][j], Length(FCellData[i][j])+1);
        FCellData[i][j][high(FCellData[i][j])] := TStringList.Create;
        FCounter := 0;
        for k := 1 to high(FMDTable.Fields) do begin
          if (FMDTable.Fields[k].Name <> FTable.Fields[FCurHFI].Name) and
             (FMDTable.Fields[k].Name <> FTable.Fields[FCurVFI].Name) then begin
            if CheckFields.Checked[FCounter] then begin
              tmp := SQLQuery.FieldByName(FMDTable.GetColumnName(k)).AsString;
              if ShowFieldNames.Checked then
                tmp := Format('%s: %s', [FMDTable.Fields[k].Caption, tmp]);
              FCellData[i][j][high(FCellData[i][j])].Add(tmp);
            end;
            inc(FCounter);
          end;
        end;
        //ShowMessage(FCellData[i][j][high(FCellData[i][j])].Text);
        SQLQuery.Next;
      end;
      FRowHeight := Max(FRowHeight, Length(FCellData[i][j]));
      SQLQuery.Close;
    end;
  end;

  j := 0;
  for i := 0 to high(FMDTable.Fields)-3 do
    if CheckFields.Checked[i] then inc(j);
  FTextHeight := DrawGrid.Canvas.TextHeight('А');
  DrawGrid.DefaultRowHeight := FRowHeight*(FTextHeight+4)*j+j;
  DrawGrid.DefaultColWidth := 250;
  DrawGrid.RowHeights[0] := 30;
  DrawGrid.ColWidths[0] := 150;

  DrawGrid.Repaint;
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

procedure TTimeTable.DrawTitles(ACol, ARow: integer; ARect: TRect);
begin
  with DrawGrid do begin
    if aRow = 0 then Canvas.TextOut(ARect.Left+3, ARect.Top+5, FColTitles[aCol-1])
    else Canvas.TextOut(ARect.Left+3, ARect.Top+5, RowTitles[aRow-1]);
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
  FRowHeight := 0;
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

end.
