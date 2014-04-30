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
    constructor Create(TheOwner: TComponent; ATable: TTableInfo); overload;
  private
    procedure InitFieldBox;
    procedure InitOperBox(ADataType: TDataType);
    procedure InitFilterEdit(ADataType: TDataType);
    procedure SetOnChange(AOnChange: TNotifyEvent);
    procedure OnFilterChange(Sender: TObject);
    procedure OnFilterClose(AFilterIndex: integer);
    function GetParamQuery: TParamQuery;
  private
    FOnChange: TNotifyEvent;
    FOnClose: TCloseFilterEvent;
    FTable: TTableInfo;
    FFilterEdit: TFieldEdit;
  public
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property OnClose: TCloseFilterEvent read FOnClose write FOnClose;
    property ParamQuery: TParamQuery read GetParamQuery;
  end;

implementation

{$R *.lfm}

constructor TFilterFrame.Create(TheOwner: TComponent; ATable: TTableInfo); overload;
begin
  inherited Create(TheOwner);
  FTable := ATable;
  InitFieldBox;
  InitOperBox(FTable.Fields[FieldBox.ItemIndex].DataType);
  InitFilterEdit(FTable.Fields[FieldBox.ItemIndex].DataType);
end;

procedure TFilterFrame.SetOnChange(AOnChange: TNotifyEvent);
begin
  FOnChange := AOnChange;
  ApplyFilter.OnChange := FOnChange;
  OperBox.OnChange := FOnChange;
  FFilterEdit.OnChange := FOnChange;
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
  FFilterEdit := GetEditClass(ADataType).Create(FilterBox, 2, 31, 265, 27);
end;

procedure TFilterFrame.FieldBoxChange(Sender: TObject);
begin
  InitOperBox(FTable.Fields[FieldBox.ItemIndex].DataType);
  InitFilterEdit(FTable.Fields[FieldBox.ItemIndex].DataType);
  OnFilterChange(self);
end;

procedure TFilterFrame.OnFilterChange(Sender: TObject);
begin
  if FOnChange <> nil then FOnChange(Sender);
end;

procedure TFilterFrame.OnFilterClose(AFilterIndex: integer);
begin
  if FOnClose <> nil then FOnClose(AFilterIndex);
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
  OnFilterChange(self);
end;

procedure TFilterFrame.CloseBtnClick(Sender: TObject);
begin
  OnFilterChange(self);
  OnFilterClose(Tag-1);
end;

end.

