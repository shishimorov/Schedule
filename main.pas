unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DbCtrls, EditBtn, StdCtrls, reference_form, metadata, data, time_table_form;

type

  { TMainForm }

  TMainForm = class(TForm)
    TimeTableBtn: TButton;
    MainMenu: TMainMenu;
    MTables: TMenuItem;
    MExit: TMenuItem;
    MFile: TMenuItem;
    MHelp: TMenuItem;
    MAbout: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TimeTableBtnClick(Sender: TObject);
  private
    procedure MRefClick(Sender: TObject);
  private
    FReferences: array of TRefForm;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  MenuItem: TMenuItem;
  i: integer;
begin
  SetLength(FReferences, Length(MData.Tables));
  for i := 0 to high(MData.Tables) do begin
    MenuItem := TMenuItem.Create(MainMenu);
    with MenuItem do begin
      Caption := MData.Tables[i].Caption;
      Name := 'M' + MData.Tables[i].Name;
      OnClick := @MRefClick;
      Tag := i;
    end;
    MTables.Insert(i, MenuItem);
  end;
end;

procedure TMainForm.TimeTableBtnClick(Sender: TObject);
begin
  TimeTable.Free;
  TimeTable := TTimeTable.Create(self);
  TimeTable.Show;
end;

procedure TMainForm.MRefClick(Sender: TObject);
begin
  if FReferences[(Sender as TMenuItem).Tag] = nil then
    FReferences[(Sender as TMenuItem).Tag] :=
      TRefForm.Create(self, MData.Tables[(Sender as TMenuItem).Tag]);
  FReferences[(Sender as TMenuItem).Tag].Show;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //DBCon.SQLTransaction.Commit;
end;

end.
