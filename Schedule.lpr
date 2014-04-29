program Schedule;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, reference_form, data, metadata, 
search_frame, db_edit, search_filter_frame,
  record_edit_form, time_table_form;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDBData, DBData);
  Application.CreateForm(TTimeTable, TimeTable);
  //Application.CreateForm(TEditForm, EditForm);
  Application.Run;
end.

