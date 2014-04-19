unit filter_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Spin;

type
  TFilterEdit = class(TObject)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent; AOnKeyPress: TKeyPressEvent); virtual; abstract;
  protected
    function GetText: string; virtual; abstract;
  public
    property Text: string read GetText;
  end;

  TTextEdit = class(TFilterEdit)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent; AOnKeyPress: TKeyPressEvent); override;
    destructor Destroy; override;
  protected
    function GetText: string; override;
  private
    FEdit: TEdit;
  end;

  TTimeEdit = class(TFilterEdit)
  public
    constructor Create(TheOwner: TComponent; ALeft, ATop, AWidth, AHeight: integer;
      AOnChange: TNotifyEvent; AOnKeyPress: TKeyPressEvent); override;
    destructor Destroy; override;
  protected
    function GetText: string; override;
  private
    FEdit: array [1..3] of TSpinEdit;
  end;

implementation

constructor TTextEdit.Create(TheOwner: TComponent; ALeft, ATop, AWidth,
  AHeight: integer; AOnChange: TNotifyEvent; AOnKeyPress: TKeyPressEvent);
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
    OnKeyPress := AOnKeyPress;
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

constructor TTimeEdit.Create(TheOwner: TComponent; ALeft, ATop, AWidth,
  AHeight: integer; AOnChange: TNotifyEvent; AOnKeyPress: TKeyPressEvent);
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
    OnKeyPress := AOnKeyPress;
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
    OnKeyPress := AOnKeyPress;
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
    OnKeyPress := AOnKeyPress;
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

end.
