unit confirm_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, BarChart,
  ButtonPanel, StdCtrls, sqldb;

type

  { TConfirmForm }

  TConfirmForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Message: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  public
    procedure Prepare(ASQLQuery: TSQLQuery; ATableName: string);
  private
    FSQLQuery: TSQLQuery;
    FTableName: string;
  end;

var
  ConfirmForm: TConfirmForm;

implementation

{$R *.lfm}

{ TConfirmForm }

procedure TConfirmForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TConfirmForm.OKButtonClick(Sender: TObject);
var
  FieldID: integer;
  tmp: string;
begin
  FieldID := FSQLQuery.FieldByName('ID').AsInteger;
  FSQLQuery.Close;
  tmp := FSQLQuery.SQL.Text;
  FSQLQuery.SQL.Text := Format('DELETE FROM %s WHERE ID = %d', [FTableName, FieldID]);
  FSQLQuery.ExecSQL;
  FSQLQuery.SQL.Text := tmp;
  FSQLQuery.Open;
end;

procedure TConfirmForm.Prepare(ASQLQuery: TSQLQuery; ATableName: string);
begin
  FSQLQuery := ASQLQuery;
  FTableName := ATableName;
end;

end.

