unit search_filter_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, metadata,
  db_edit, dialogs, Buttons, ExtCtrls;

type

  { TFilterFrame }

  TParamQuery = record
    SQL: string;
    ParamName: string;
    ParamValue: string;
  end;

  TCloseFilterEvent = procedure(AFilterIndex: integer) of Object;

  TFilterFrame = class(TFrame)
    ApplyFilter: TCheckBox;
    FieldBox: TComboBox;
    OperBox: TComboBox;
    FilterBox: TGroupBox;
    CondBtn: TSpeedButton;
    CloseBtn: TSpeedButton;
    procedure CloseBtnClick(Sender: TObject);
    procedure CondBtnClick(Sender: TObject);
    procedure FieldBoxChange(Sender: TObject);
  public
    procedure Prepare(ATable: TTableInfo);
  private
    procedure InitFieldBox;
    procedure InitOperBox(ADataType: TDataType);
    procedure InitFilterEdit(ADataType: TDataType);
    function GetParamQuery: TParamQuery;
  public
    SearchQueryChange: TNotifyEvent;
    CloseFilter: TCloseFilterEvent;
  private
    FTable: TTableInfo;
    FFilterEdit: TDBEditWBH;
  public
    property ParamQuery: TParamQuery read GetParamQuery;
  end;

implementation

{$R *.lfm}

procedure TFilterFrame.Prepare(ATable: TTableInfo);
begin
  FTable := ATable;
  InitFieldBox;
  InitOperBox(FTable.Fields[FieldBox.ItemIndex].DataType);
  InitFilterEdit(FTable.Fields[FieldBox.ItemIndex].DataType);
  ApplyFilter.OnChange := SearchQueryChange;
  OperBox.OnChange := SearchQueryChange;
end;

function TFilterFrame.GetParamQuery: TParamQuery;
var FieldName: string;
begin
  Result.SQL := '';
  if FFilterEdit.Value = '' then Exit;

  FieldName := FTable.GetFullFieldName(FieldBox.ItemIndex);
  Result.ParamName := Format('FilterValue_%d', [Tag]);
  Result.ParamValue := FFilterEdit.Value;

  case FTable.Fields[FieldBox.ItemIndex].DataType of
  dtStr:
    case OperBox.ItemIndex of
    0:
      Result.SQL :=
        Format('POSITION(UPPER(:%s), UPPER(%s)) > 0', [Result.ParamName, FieldName]);
    1:
      Result.SQL :=
        Format('POSITION(UPPER(:%s), UPPER(%s)) = 1', [Result.ParamName, FieldName])
    else
      Result.SQL := Format('UPPER(%s) %s UPPER(:%s)',
        [FieldName, OperBox.Items.Strings[OperBox.ItemIndex], Result.ParamName]);
    end;
  else
    Result.SQL := Format('%s %s :%s',
      [FieldName, OperBox.Items.Strings[OperBox.ItemIndex], Result.ParamName]);
  end;
end;

procedure TFilterFrame.InitFieldBox;
var i: integer;
begin
  for i := 0 to high(FTable.Fields) do
    FieldBox.Items.Add(FTable.Fields[i].Caption);
  FieldBox.ItemIndex := 0;
  FieldBox.OnChange := @FieldBoxChange;
end;

procedure TFilterFrame.InitOperBox(ADataType: TDataType);
begin
  OperBox.Items.Clear;
  if ADataType = dtStr then begin
    OperBox.Items.Add('Cодержит');
    OperBox.Items.Add('Начинается с');
  end;
  OperBox.Items.Add('>');
  OperBox.Items.Add('<');
  OperBox.Items.Add('>=');
  OperBox.Items.Add('<=');
  OperBox.Items.Add('=');
  OperBox.ItemIndex := 0;
end;

procedure TFilterFrame.InitFilterEdit(ADataType: TDataType);
begin
  FFilterEdit.Free;
  FFilterEdit :=
    GetEditClass(ADataType).Create(FilterBox, 2, 31, 265, 27, SearchQueryChange);
end;

procedure TFilterFrame.FieldBoxChange(Sender: TObject);
begin
  InitOperBox(FTable.Fields[FieldBox.ItemIndex].DataType);
  InitFilterEdit(FTable.Fields[FieldBox.ItemIndex].DataType);
  if SearchQueryChange <> nil then SearchQueryChange(self);
end;

procedure TFilterFrame.CondBtnClick(Sender: TObject);
begin
  case CondBtn.Tag of
  0:
    begin
      CondBtn.Tag := 1;
      CondBtn.Caption := 'ИЛИ';
    end;
  1:
    begin
      CondBtn.Tag := 0;
      CondBtn.Caption := 'И';
    end;
  end;
  if SearchQueryChange <> nil then SearchQueryChange(self);
end;

procedure TFilterFrame.CloseBtnClick(Sender: TObject);
begin
  CloseFilter(Tag - 1);
end;

end.

