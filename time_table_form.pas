unit time_table_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdCtrls, Menus, Buttons, search_frame, search_filter_frame,
  metadata, math, lazregions, DbCtrls, types, record_edit_form;

type

  { TTimeTable }

  TPoints = array of TPoint;
  TIntArray = array of integer;

  TTimeTable = class(TForm)
    AddFilterBtn: TSpeedButton;
    DeleteImage: TImage;
    FiltersBox: TGroupBox;
    EditImage: TImage;
    PMAdd: TMenuItem;
    PMenu: TPopupMenu;
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
    procedure PMAddClick(Sender: TObject);
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
    function GetTitles(AFieldIndex: integer; var ATitleIDs: TIntArray): TStringList;
    function GetTitlesQuery(AFieldIndex: integer): string;
    function GetTriangle(ARect: TRect; ACol, ARow: integer): TPoints;
    procedure GetCellData;
    procedure ClearCellData;
    procedure PrepareCanvas;
    procedure AfterEditAction(Sender: TObject);
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
  PrepareCanvas;
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
        if (FCurCol > 0) and (FCurRow > 0) and (FCurCol = aCol) and (FCurRow = aRow) and
           (FCurOrd > CurTop) and (FCurOrd < CurTop+FTextHeight*FFieldsCount)
        then begin
          Draw(aRect.Right-34, CurTop+1, EditImage.Picture.Bitmap);
          Draw(aRect.Right-17, CurTop, DeleteImage.Picture.Bitmap);
        end;
        CurTop += FTextHeight*FFieldsCount+1;
        MoveTo(aRect.Left, CurTop);
        LineTo(aRect.Right, CurTop);
      end;
    end;

    if (DrawGrid.RowHeights[aRow] < FDefaultRowHeight*Length(FCellData[aCol][aRow])) and
       (Length(FCellData[aCol][aRow]) > 1)
    then begin
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      Polygon(GetTriangle(aRect, aCol, aRow));
    end;
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
  EditForm: TEditForm;
  EditBtnRect, DelBtnRect: TRect;
  pCol, pRow: integer;
  i: integer;
begin
  with DrawGrid do
    if (Row > 0) and (Col > 0) then begin
      if Button = mbLeft then begin
        if RowHeights[Row] < FDefaultRowHeight*Length(FCellData[Col][Row]) then
          if IsPointInPolygon(X, Y, GetTriangle(CellRect(Col, Row), Col, Row)) then begin
            RowHeights[Row] := FDefaultRowHeight*Length(FCellData[Col][Row]);
            Exit;
          end;

        EditBtnRect := CellRect(Col, Row);
        with EditBtnRect do begin
          Left := Right-34;
          Right := Right-18;
          Top := Top+1;
          Bottom := Top+16;
        end;
        DelBtnRect := EditBtnRect;
        with DelBtnRect do begin
          Left := Left+17;
          Right := Right+17;
        end;
        if FFieldsCount > 0 then begin
          for i := 0 to high(FCellData[Col][Row]) do begin
            if PtInRect(EditBtnRect, Point(X, Y)) then begin
              EditForm := TEditForm.Create(self, FMDTable, FRecordIDs[Col][Row][i],
                @AfterEditAction, [0, FCurMDHField, FCurMDVField]);
              EditForm.ShowModal;
              Exit;
            end
            else if PtInRect(DelBtnRect, Point(X, Y)) then begin
              if Application.MessageBox('Вы действительно хотите удалить данную запись?',
                'Подтверждение', (4+48)) = 6
              then begin
                SQLQuery.SQL.Text := Format('DELETE FROM %s WHERE ID = %d',
                  [FTable.Name, FRecordIDs[Col][Row][i]]);
                SQLQuery.ExecSQL;
                AfterEditAction(self);
              end;
              Exit;
            end;
            EditBtnRect.Top += FTextHeight*FFieldsCount+1;
            EditBtnRect.Bottom := EditBtnRect.Top+16;
            DelBtnRect.Top := EditBtnRect.Top;
            DelBtnRect.Bottom := EditBtnRect.Bottom;
          end;
        end;
      end
      else
        if (Button = mbRight) then begin
          MouseToCell(X, Y, pCol, pRow);
          if (pCol > 0) and (pRow > 0) then begin
            Col := pCol;
            Row := pRow;
            PMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
          end;
        end;
    end;
end;

procedure TTimeTable.PMAddClick(Sender: TObject);
var EditForm: TEditForm;
begin
  EditForm := TEditForm.Create(self, FMDTable, @AfterEditAction, [0, FCurMDHField, FCurMDVField]);
  with EditForm do begin
    FieldEdits[0].Value := 'Авто генерация';
    FieldEdits[FCurMDHField].Value := FColTitleIDs[DrawGrid.Col-1];
    FieldEdits[FCurMDVField].Value := FRowTitleIDs[DrawGrid.Row-1];
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
  tmp: TStringList;
  RecID, i: integer;
begin
  RecID := FRecordIDs[AStartCol][AStartRow][ARecordIndex];
  tmp := FCellData[AStartCol][AStartRow][ARecordIndex];

  for i := ARecordIndex to high(FCellData[AStartCol][AStartRow])-1 do begin
    FCellData[AStartCol][AStartRow][i] := FCellData[AStartCol][AStartRow][i+1];
    FRecordIDs[AStartCol][AStartRow][i] := FRecordIDs[AStartCol][AStartRow][i+1];
  end;
  SetLength(FCellData[AStartCol][AStartRow], Length(FCellData[AStartCol][AStartRow])-1);
  SetLength(FRecordIDs[AStartCol][AStartRow], Length(FRecordIDs[AStartCol][AStartRow])-1);

  SetLength(FCellData[AEndCol][AEndRow], Length(FCellData[AEndCol][AEndRow])+1);
  FCellData[AEndCol][AEndRow][high(FCellData[AEndCol][AEndRow])] := tmp;
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

procedure TTimeTable.PrepareCanvas;
begin
  with DrawGrid.Canvas do begin
    Pen.Color := clBlack;
    Brush.Color := clBlack;
    Font.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
  end;
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

end.
