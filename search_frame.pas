unit search_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, sqldb,
  search_filter_frame, db_edit, metadata, dialogs, ExtCtrls;

const
  FILTER_LIMIT = 10;

type

  { TSearchFrame }

  TFilters = array of TFilterFrame;

  TSearchFrame = class(TFrame)
    ApplyFilters: TCheckBox;
    FieldBox: TComboBox;
    FiltersBox: TGroupBox;
    SearchBox: TGroupBox;
    AddFilterBtn: TSpeedButton;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure SearchQueryChange(Sender: TObject);
    procedure FieldBoxChange(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure CloseFilterClick(AFilterIndex: integer);
  public
    constructor Create(TheOwner: TComponent; ATable: TTableInfo;
      ASQLQuery: TSQLQuery); overload;
  private
    procedure InitFieldBox;
    procedure InitSearchEdit(ADataType: TDataType);
    procedure InitSearchBtn(ALeft, ATop: integer);
    procedure UpdateTable;
  private
    FTable: TTableInfo;
    FSQLQuery: TSQLQuery;
    FSearchEdit: TFieldEdit;
    FSearchBtn: TSpeedButton;
    FFilters: TFilters;
  end;

function AddFilter(TheOwner: TGroupBox; var AFilters: TFilters; ATable: TTableInfo): TFilterFrame;
procedure CloseFilter(TheOwner: TGroupBox; var AFilters: TFilters; AFilterIndex: integer);
procedure GetFiltersSQL(AFilters: TFilters; ASQLQuery: TSQLQuery);

implementation

{$R *.lfm}

constructor TSearchFrame.Create(TheOwner: TComponent; ATable: TTableInfo;
  ASQLQuery: TSQLQuery);
begin
  inherited Create(TheOwner);
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
  FieldName: string;
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

  if ApplyFilters.Checked then
    GetFiltersSQL(FFilters, FSQLQuery);

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
var NewFilter: TFilterFrame;
begin
  NewFilter := AddFilter(FiltersBox, FFilters, FTable);
  if NewFilter <> nil then
    with NewFilter do begin
      SearchQueryChange := @self.SearchQueryChange;
      CloseFilter := @CloseFilterClick;
      self.Height := self.Height+Height;
    end;
end;

procedure TSearchFrame.CloseFilterClick(AFilterIndex: integer);
begin
  Height := Height-FFilters[AFilterIndex].Height;
  CloseFilter(FiltersBox, FFilters, AFilterIndex);
  SearchQueryChange(self);
end;

function AddFilter(TheOwner: TGroupBox; var AFilters: TFilters; ATable: TTableInfo): TFilterFrame;
begin
  Result := nil;
  if Length(AFilters) >= FILTER_LIMIT then Exit;
  if Length(AFilters) > 0 then
    AFilters[high(AFilters)].CondBtn.Visible := True;
  SetLength(AFilters, Length(AFilters) + 1);
  AFilters[high(AFilters)] := TFilterFrame.Create(TheOwner, ATable);
  with AFilters[high(AFilters)] do begin
    Tag := Length(AFilters);
    Top := high(AFilters)*Height;
    Name := Format('FilterFrame_%d', [Tag]);
  end;
  TheOwner.InsertControl(AFilters[high(AFilters)]);
  Result := AFilters[high(AFilters)];
end;

procedure CloseFilter(TheOwner: TGroupBox; var AFilters: TFilters; AFilterIndex: integer);
var i: integer;
begin
  TheOwner.RemoveControl(AFilters[AFilterIndex]);
  AFilters[AFilterIndex].Free;

  for i := AFilterIndex to high(AFilters) - 1 do begin
    AFilters[i] := AFilters[i+1];
    with AFilters[i] do begin
      Tag := AFilters[i].Tag - 1;
      Top := Top - Height;
      Name := Format('FilterFrame_%d', [Tag]);
    end;
  end;
  SetLength(AFilters, Length(AFilters) - 1);
  if Length(AFilters) > 0 then
    AFilters[high(AFilters)].CondBtn.Visible := False;
end;

procedure GetFiltersSQL(AFilters: TFilters; ASQLQuery: TSQLQuery);
var
  i: integer;
  Cond: string;
  ParamQuery: TParamQuery;
begin
  Cond := 'AND (';
  for i := 0 to high(AFilters) do begin
    if AFilters[i].ApplyFilter.Checked then begin
      ParamQuery := AFilters[i].ParamQuery;
      if ParamQuery.SQL <> '' then begin
        if ASQLQuery.SQL.Strings[WHERE_IND] = '' then
          ASQLQuery.SQL.Strings[WHERE_IND] := Format('WHERE (%s', [ParamQuery.SQL])
        else
          ASQLQuery.SQL.Strings[WHERE_IND] := Format('%s %s %s',
            [ASQLQuery.SQL.Strings[WHERE_IND], Cond, ParamQuery.SQL]);
        case AFilters[i].CondBtn.Tag of
        0: Cond := 'AND';
        1: Cond := 'OR';
        end;
        ASQLQuery.ParamByName(ParamQuery.ParamName).Value := ParamQuery.ParamValue
      end;
    end;
  end;
  if Length(Cond) <> 5 then
    ASQLQuery.SQL.Strings[WHERE_IND] := ASQLQuery.SQL.Strings[WHERE_IND] + ')';
end;

end.
