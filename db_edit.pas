unit db_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Spin, StrUtils;

type
  TDBEditWBH = class(TPersistent)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent); virtual; abstract;
  protected
    function GetText: string; virtual; abstract;
    procedure SetText(AValue: string); virtual; abstract;
    function GetEnabled: boolean; virtual; abstract;
    procedure SetEnabled(AValue: boolean); virtual; abstract;
  public
    property Text: string read GetText write SetText;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TTextEdit = class(TDBEditWBH)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent); override;
    destructor Destroy; override;
  protected
    function GetText: string; override;
    procedure SetText(AValue: string); override;
    function GetEnabled: boolean; override;
    procedure SetEnabled(AValue: boolean); override;
  private
    FEdit: TEdit;
  end;

  TIntEdit = class(TTextEdit)
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
    function GetText: string; override;
    procedure SetText(AValue: string); override;
    function GetEnabled: boolean; override;
    procedure SetEnabled(AValue: boolean); override;
  private
    FEdit: array [1..3] of TSpinEdit;
  end;

implementation

constructor TTextEdit.Create(TheOwner: TComponent; ALeft, ATop, AWidth,
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

destructor TTextEdit.Destroy;
begin
  FEdit.Free;
end;

function TTextEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TTextEdit.SetText(AValue: string);
begin
  FEdit.Text := AValue;
end;

function TTextEdit.GetEnabled: boolean;
begin
  Result := FEdit.Enabled;
end;

procedure TTextEdit.SetEnabled(AValue: boolean);
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
begin
  FEdit[1] := TSpinEdit.Create(TheOwner);
  with FEdit[1] do begin
    Parent := TheOwner as TWinControl;
    Width := AWidth;
    Height := AHeight;
    Left := ALeft;
    Top := ATop;
    MinValue := 0;
    MaxValue := 23;
    Value := 0;
    OnChange := AOnChange;
  end;
  //FEdit[2].Assign(FEdit[1]);
  //with FEdit[2] do begin
  //  Left := Left + AWidth + 2;
  //  MaxValue := 60;
  //end;
  //FEdit[3].Assign(FEdit[2]);
  //FEdit[3].Left := Left + AWidth + 2;
  FEdit[2] := TSpinEdit.Create(TheOwner);
  with FEdit[2] do begin
    Parent := TheOwner as TWinControl;
    Width := AWidth;
    Height := AHeight;
    Left := ALeft + AWidth + 2;
    Top := ATop;
    MinValue := 0;
    MaxValue := 60;
    Value := 0;
    OnChange := AOnChange;
  end;
  FEdit[3] := TSpinEdit.Create(TheOwner);
  with FEdit[3] do begin
    Parent := TheOwner as TWinControl;
    Width := AWidth;
    Height := AHeight;
    Left := ALeft + AWidth * 2 + 4;
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

function TTimeEdit.GetText: string;
begin
  Result := Format('%d:%d:%d', [FEdit[1].Value, FEdit[2].Value, FEdit[3].Value]);
end;

procedure TTimeEdit.SetText(AValue: string);
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

end.
