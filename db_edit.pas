unit db_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Spin, StrUtils, typinfo, db,
  DBCtrls, metadata, dialogs;

type
  TDBEditWBH = class(TPersistent)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent); virtual; abstract;
  protected
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(AValue: Variant); virtual; abstract;
    function GetEnabled: boolean; virtual; abstract;
    procedure SetEnabled(AValue: boolean); virtual; abstract;
  public
    property Value: Variant read GetValue write SetValue;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TStrEdit = class(TDBEditWBH)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent); override;
    destructor Destroy; override;
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
    function GetEnabled: boolean; override;
    procedure SetEnabled(AValue: boolean); override;
  private
    FEdit: TEdit;
  end;

  TIntEdit = class(TStrEdit)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent); override;
  private
    procedure FOnKeyPress(Sender: TObject; var Key: char);
  end;

  TTimeEdit = class(TDBEditWBH)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent); override;
    destructor Destroy; override;
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
    function GetEnabled: boolean; override;
    procedure SetEnabled(AValue: boolean); override;
  private
    FEdit: array [1..3] of TSpinEdit;
  end;

  TListEdit = class(TDBEditWBH)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent); override;
    destructor Destroy; override;
    procedure Prepare(ADataSource: TDataSource; AField: TRefFieldInfo);
  protected
    function GetValue: Variant; override;
    procedure SetValue(AValue: Variant); override;
  private
    FEdit: TDBLookupComboBox;
  end;

  TDBEditWBHClass = class of TDBEditWBH;

function GetEditClass(ADataType: TDataType): TDBEditWBHClass;

implementation

constructor TStrEdit.Create(TheOwner: TComponent; ALeft, ATop, AWidth,
  AHeight: integer; AOnChange: TNotifyEvent);
begin
  FEdit := TEdit.Create(TheOwner);
  with FEdit do begin
    Parent := TheOwner as TWinControl;
    Left := ALeft;
    Top := ATop;
    Width := AWidth;
    Height := AHeight;
    Text := '';
    OnChange := AOnChange;
  end;
end;

destructor TStrEdit.Destroy;
begin
  FEdit.Free;
end;

function TStrEdit.GetValue: Variant;
begin
  Result := FEdit.Text;
end;

procedure TStrEdit.SetValue(AValue: Variant);
begin
  FEdit.Text := AValue;
end;

function TStrEdit.GetEnabled: boolean;
begin
  Result := FEdit.Enabled;
end;

procedure TStrEdit.SetEnabled(AValue: boolean);
begin
  FEdit.Enabled := AValue;
end;

constructor TIntEdit.Create(TheOwner: TComponent; ALeft, ATop, AWidth,
  AHeight: integer; AOnChange: TNotifyEvent);
begin
  inherited Create(TheOwner, ALeft, ATop, AWidth, AHeight, AOnChange);
  FEdit.OnKeyPress := @FOnKeyPress;
end;

procedure TIntEdit.FOnKeyPress(Sender: TObject; var Key: char);
begin
  if (not (Key in ['0'..'9'])) and (Key <> #8) then Key := #0;
end;

constructor TTimeEdit.Create(TheOwner: TComponent; ALeft, ATop, AWidth,
  AHeight: integer; AOnChange: TNotifyEvent);
var
  SpinWidth: integer;
begin
  SpinWidth := Round((AWidth-4)/3);
  FEdit[1] := TSpinEdit.Create(TheOwner);
  with FEdit[1] do begin
    Parent := TheOwner as TWinControl;
    Width := SpinWidth;
    Height := AHeight;
    Left := ALeft;
    Top := ATop;
    MinValue := 0;
    MaxValue := 23;
    Value := 0;
    OnChange := AOnChange;
  end;
  FEdit[2] := TSpinEdit.Create(TheOwner);
  with FEdit[2] do begin
    Parent := TheOwner as TWinControl;
    Width := SpinWidth;
    Height := AHeight;
    Left := ALeft + Width + 2;
    Top := ATop;
    MinValue := 0;
    MaxValue := 60;
    Value := 0;
    OnChange := AOnChange;
  end;
  FEdit[3] := TSpinEdit.Create(TheOwner);
  with FEdit[3] do begin
    Parent := TheOwner as TWinControl;
    Width := SpinWidth;
    Height := AHeight;
    Left := ALeft + Width * 2 + 4;
    Top := ATop;
    MinValue := 0;
    MaxValue := 60;
    Value := 0;
    OnChange := AOnChange;
  end;
end;

destructor TTimeEdit.Destroy;
var i: integer;
begin
  for i := 1 to high(FEdit) do FEdit[i].Free;
end;

function TTimeEdit.GetValue: Variant;
begin
  Result := Format('%d:%d:%d', [FEdit[1].Value, FEdit[2].Value, FEdit[3].Value]);
end;

procedure TTimeEdit.SetValue(AValue: Variant);
var pos1, pos2: integer;
begin
  pos1 := Pos(':', AValue);
  FEdit[1].Value := StrToInt(Copy(AValue, 1, pos1-1));
  pos2 := PosEx(':', AValue, pos1+1);
  FEdit[2].Value := StrToInt(Copy(AValue, pos1+1, pos2-pos1-1));
  FEdit[3].Value := StrToInt(Copy(AValue, pos2+1, Length(AValue)-pos2));
end;

function TTimeEdit.GetEnabled: boolean;
begin
  Result := FEdit[1].Enabled;
end;

procedure TTimeEdit.SetEnabled(AValue: boolean);
begin
  FEdit[1].Enabled := AValue;
  FEdit[2].Enabled := AValue;
  FEdit[3].Enabled := AValue;
end;

constructor TListEdit.Create(TheOwner: TComponent; ALeft, ATop, AWidth,
  AHeight: integer; AOnChange: TNotifyEvent);
begin
  FEdit := TDBLookupComboBox.Create(TheOwner);
  with FEdit do begin
    Parent := TheOwner as TWinControl;
    Left := ALeft;
    Top := ATop;
    Width := AWidth;
    Height := AHeight;
    OnChange := AOnChange;
  end;
end;

destructor TListEdit.Destroy;
begin
  FEdit.Free;
end;

procedure TListEdit.Prepare(ADataSource: TDataSource; AField: TRefFieldInfo);
begin
  with FEdit do begin
    ListSource := ADataSource;
    ListField := AnsiDequotedStr(AField.ListFieldName, '"');
    KeyField := AnsiDequotedStr(AField.KeyFieldName, '"');
  end;
end;

function TListEdit.GetValue: Variant;
begin
  Result := FEdit.KeyValue;
end;

procedure TListEdit.SetValue(AValue: Variant);
begin
  FEdit.KeyValue := AValue;
end;

function GetEditClass(ADataType: TDataType): TDBEditWBHClass;
var tmp: string;
begin
  tmp := GetEnumName(TypeInfo(TDataType), ord(ADataType));
  Delete(tmp, 1, 2);
  Result := TDBEditWBHClass(GetClass(Format('T%sEdit', [tmp])));
end;

initialization

RegisterClasses([TStrEdit, TIntEdit, TTimeEdit, TListEdit]);

end.
