program Schedule;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, reference_form, data, metadata, 
search_frame, db_edit, search_filter_frame,
  row_edit_form, confirm_form;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  //Application.CreateForm(TRefForm, RefForm);
  Application.CreateForm(TDBCon, DBCon);
  Application.CreateForm(TConfirmForm, ConfirmForm);
  //Application.CreateForm(TEditForm, EditForm);
  Application.Run;
end.

