unit search_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, sqldb,
  search_filter_frame, db_edit, metadata, dialogs, ExtCtrls;

type

  { TSearchFrame }

  TSearchFrame = class(TFrame)
    ApplyFilters: TCheckBox;
    FieldBox: TComboBox;
    FiltersBox: TGroupBox;
    SearchBox: TGroupBox;
    AddFilterBtn: TSpeedButton;
    MoveFilterTimer: TTimer;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure MoveFilterTimerTimer(Sender: TObject);
    procedure SearchQueryChange(Sender: TObject);
    procedure FieldBoxChange(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure CloseFilterClick(AFilterIndex: integer);
  public
    procedure Prepare(ATable: TTableInfo; ASQLQuery: TSQLQuery);
  private
    procedure InitFieldBox;
    procedure InitSearchEdit(ADataType: TDataType);
    procedure InitSearchBtn(ALeft, ATop: integer);
    procedure UpdateTable;
  private
    FTable: TTableInfo;
    FSQLQuery: TSQLQuery;
    FSearchEdit: TDBEditWBH;
    FSearchBtn: TSpeedButton;
    FFilters: array of TFilterFrame;
    FClosedFilterIndex, FFinalHeight: integer;
  const
    FILTER_LIMIT = 10;
  end;

implementation

{$R *.lfm}

procedure TSearchFrame.Prepare(ATable: TTableInfo; ASQLQuery: TSQLQuery);
begin
  FTable := ATable;
  FSQLQuery := ASQLQuery;
  InitFieldBox;
  InitSearchEdit(FTable.Fields[FieldBox.ItemIndex].DataType);
end;

procedure TSearchFrame.InitFieldBox;
var i: integer;
begin
  for i := 0 to high(FTable.Fields) do
    FieldBox.Items.Add(FTable.Fields[i].Caption);
  FieldBox.ItemIndex := 0;
  FieldBox.OnChange := @FieldBoxChange;
end;

procedure TSearchFrame.InitSearchEdit(ADataType: TDataType);
begin
  FSearchEdit.Free;
  FSearchEdit := GetEditClass(ADataType).Create(SearchBox, 2, 33, 292, 27, @SearchQueryChange);
  InitSearchBtn(296, 33);
end;

procedure TSearchFrame.InitSearchBtn(ALeft, ATop: integer);
begin
  FSearchBtn.Free;
  FSearchBtn := TSpeedButton.Create(SearchBox);
  with FSearchBtn do begin
    Parent := SearchBox;
    Left := ALeft;
    Top := ATop;
    Width := 30;
    Height := 27;
    Glyph.LoadFromFile('./Data/Icons/search.bmp');
    OnClick := @SearchBtnClick;
  end;
end;

procedure TSearchFrame.UpdateTable;
var
  FieldName, Cond: string;
  ParamQuery: TParamQuery;
  i: integer;
begin
  FSQLQuery.Close;
  FieldName := FTable.GetFullFieldName(FieldBox.ItemIndex);

  if FSearchEdit.Value <> '' then begin
    case FTable.Fields[FieldBox.ItemIndex].DataType of
    dtStr:
      begin
        FSQLQuery.SQL.Strings[WHERE_IND] :=
          Format('WHERE POSITION(UPPER(:SearchValue), UPPER(%s)) = 1', [FieldName]);
        FSQLQuery.ParamByName('SearchValue').AsString := FSearchEdit.Value;
      end;
    else
      begin
        FSQLQuery.SQL.Strings[WHERE_IND] :=
          Format('WHERE %s = :SearchValue', [FieldName]);
        FSQLQuery.ParamByName('SearchValue').Value := FSearchEdit.Value;
      end;
    end;
  end
  else FSQLQuery.SQL.Strings[WHERE_IND] := '';

  if ApplyFilters.Checked then begin
    Cond := 'AND ('; //связка поиска с фильтрами
    for i := 0 to high(FFilters) do begin
      if FFilters[i].ApplyFilter.Checked then begin
        ParamQuery := FFilters[i].ParamQuery;
        if ParamQuery.SQL <> '' then begin
          if FSQLQuery.SQL.Strings[WHERE_IND] = '' then
            FSQLQuery.SQL.Strings[WHERE_IND] := Format('WHERE (%s', [ParamQuery.SQL])
          else
            FSQLQuery.SQL.Strings[WHERE_IND] := Format('%s %s %s',
              [FSQLQuery.SQL.Strings[WHERE_IND], Cond, ParamQuery.SQL]);
          case FFilters[i].CondBtn.Tag of
          0: Cond := 'AND';
          1: Cond := 'OR';
          end;
          FSQLQuery.ParamByName(ParamQuery.ParamName).Value := ParamQuery.ParamValue
        end;
      end;
    end;
    if Length(Cond) <> 5 then
      FSQLQuery.SQL.Strings[WHERE_IND] := FSQLQuery.SQL.Strings[WHERE_IND] + ')';
  end;

  FSQLQuery.Open;
end;

procedure TSearchFrame.FieldBoxChange(Sender: TObject);
begin
  InitSearchEdit(FTable.Fields[FieldBox.ItemIndex].DataType);
  SearchQueryChange(self);
end;

procedure TSearchFrame.SearchQueryChange(Sender: TObject);
begin
  FSearchBtn.Enabled := True;
  FSearchBtn.Flat := False;
end;

procedure TSearchFrame.SearchBtnClick(Sender: TObject);
begin
  UpdateTable;
  FSearchBtn.Flat := True;
  FSearchBtn.Enabled := False;
end;

procedure TSearchFrame.AddFilterBtnClick(Sender: TObject);
begin
  if Length(FFilters) >= FILTER_LIMIT then Exit;
  if Length(FFilters) > 0 then
    FFilters[high(FFilters)].CondBtn.Visible := True;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[high(FFilters)] := TFilterFrame.Create(FiltersBox);
  with FFilters[high(FFilters)] do begin
    Tag := Length(FFilters);
    Top := high(FFilters) * Height;
    Name := Format('FilterFrame_%d', [Tag]);
    //FilterBox.Caption := Format('Фильтр %d', [Tag]);
    SearchQueryChange := @self.SearchQueryChange;
    CloseFilter := @CloseFilterClick;
    Prepare(self.FTable);
  end;
  Height := Height + FFilters[high(FFilters)].Height;
  FiltersBox.InsertControl(FFilters[high(FFilters)]);
end;

procedure TSearchFrame.CloseFilterClick(AFilterIndex: integer);
var i: integer;
begin
  if MoveFilterTimer.Enabled then Exit;
  FClosedFilterIndex := AFilterIndex;
  FFinalHeight := Height - FFilters[AFilterIndex].Height;
  FiltersBox.RemoveControl(FFilters[AFilterIndex]);
  FFilters[AFilterIndex].Free;

  for i := AFilterIndex to high(FFilters) - 1 do begin
    FFilters[i] := FFilters[i+1];
    with FFilters[i] do begin
      Tag := FFilters[i].Tag - 1;
      //Caption := Format('Фильтр %d', [Tag]);
      Name := Format('FilterFrame_%d', [Tag]);
      //FilterBox.Caption := Caption;
    end;
  end;
  SetLength(FFilters, Length(FFilters) - 1);
  if Length(FFilters) > 0 then
    FFilters[high(FFilters)].CondBtn.Visible := False;
  MoveFilterTimer.Enabled := True;
end;

procedure TSearchFrame.MoveFilterTimerTimer(Sender: TObject);
var i: integer;
begin
  if Height = FFinalHeight then begin
    MoveFilterTimer.Enabled := False;
    Exit;
  end;
  Height := Height - 2;
  for i := FClosedFilterIndex to high(FFilters) do
    FFilters[i].Top := FFilters[i].Top - 2;
end;

end.
