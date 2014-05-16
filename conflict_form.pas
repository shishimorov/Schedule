unit conflict_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  EditBtn, Menus, metadata, db, sqldb, data, record_edit_form, reference_form;

type

  { TConflictForm }

  TConflictForm = class(TForm)
    Datasource: TDatasource;
    PMRef: TMenuItem;
    PMenu: TPopupMenu;
    SQLQuery: TSQLQuery;
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure PMRefClick(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure UpdateConflicts;
    procedure AfterEditAction(Sender: TObject);
  private
    FTable: TTableInfo;
    FPNode: TTreeNode;
    EditForms: array of TEditForm;
  const
    CONFLICT_LEVEL = 2;
    RECORD_LEVEL = 3;
  end;

var
  ConflictForm: TConflictForm;

implementation

{$R *.lfm}

{ TConflictForm }

procedure TConflictForm.FormCreate(Sender: TObject);
begin
  FTable := MData.Tables[high(MData.Tables)];
  TreeView.Items.Add(nil, 'Конфликты');
  UpdateConflicts;
end;

procedure TConflictForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  RecID, i: integer;
  str: string;
begin
  if not (ssDouble in Shift) then Exit;
  Node := TreeView.GetNodeAt(X, Y);
  if Node = nil then Exit;
  if Node.Level <> RECORD_LEVEL then Exit;

  RecID := StrToInt(TStringList(Node.Data^).Strings[0]);
  for i := 0 to high(EditForms) do
    if EditForms[i].Tag = RecID then begin
      EditForms[i].Show;
      Exit;
    end;
  SetLength(EditForms, Length(EditForms)+1);
  EditForms[high(EditForms)] :=
    TEditForm.Create(self, FTable, RecID, @AfterEditAction, [0]);
  EditForms[high(EditForms)].Tag := RecID;
  EditForms[high(EditForms)].Show;
end;

procedure TConflictForm.UpdateConflicts;
var
  MainNode, ConfNode: TTreeNode;
  CurGroupField, PrevGroupField: Variant;
  i, j, k: integer;
begin
  for i := 0 to high(Conflicts) do begin
    for j := 0 to high(Conflicts[i].Data) do begin
      for k := 0 to high(Conflicts[i].Data[j]) do begin
        Conflicts[i].Data[j][k].Free;
      end;
      SetLength(Conflicts[i].Data[j], 0);
    end;
    SetLength(Conflicts[i].Data, 0);
  end;

  for i := 0 to high(Conflicts) do begin
    with Conflicts[i] do begin
      SQLQuery.SQL.Text := SQL;
      SQLQuery.Open;
      while not SQLQuery.EOF do begin
        CurGroupField := SQLQuery.Fields[GroupField].Value;
        if CurGroupField <> PrevGroupField then begin
          PrevGroupField := CurGroupField;
          SetLength(Data, Length(Data)+1);
        end;

        SetLength(Data[high(Data)], Length(Data[high(Data)])+1);
        Data[high(Data)][high(Data[high(Data)])] := TStringList.Create;
        for j := 0 to SQLQuery.FieldCount-1 do
          with Data[high(Data)][high(Data[high(Data)])] do
            Add(SQLQuery.Fields[j].Value);
        SQLQuery.Next;
      end;
      SQLQuery.Close;
    end;
  end;

  with TreeView do begin
    Items[0].Delete;
    Items.Add(nil, 'Конфликты');
    for i := 0 to high(Conflicts) do begin
      MainNode := Items.AddCHild(Items[0], Conflicts[i].Name);
      MainNode.Data := @Conflicts[i];
      for j := 0 to high(Conflicts[i].Data) do begin
        ConfNode :=
          Items.AddChild(MainNode, Conflicts[i].Data[j][0].Strings[Conflicts[i].GroupField]);
        ConfNode.Data := @Conflicts[i].Data[j];
        for k := 0 to high(Conflicts[i].Data[j]) do
          Items.AddChild(ConfNode, Conflicts[i].Data[j][k].Strings[0]).Data :=
            @Conflicts[i].Data[j][k];
      end;
    end;
  end;
end;

procedure TConflictForm.TreeViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Node: TTreeNode;
begin
  Application.CancelHint;
  Node := TreeView.GetNodeAt(X, Y);
  if Node = nil then Exit;
  if Node.Level <> RECORD_LEVEL then Exit;
  Hint := TStringList(Node.Data^).Text;
  Application.ActivateHint(Mouse.CursorPos, True);
end;

procedure TConflictForm.TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Node: TTreeNode;
begin
  if Button <> mbRight then Exit;
  Node := TreeView.GetNodeAt(X, Y);
  if Node = nil then Exit;
  if Node.Level <> CONFLICT_LEVEL then Exit;
  FPNode := Node;
  PMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TConflictForm.PMRefClick(Sender: TObject);
var
  RefForm: TRefForm;
  i: integer;
begin
  RefForm := TRefForm.Create(self, FTable);
  with RefForm.SearchFrame do begin
    for i := 0 to high(TConfNode(FPNode.Data^)) do begin
      AddFilter(0, TConfNode(FPNode.Data^)[i].Strings[0]);
      Filters[high(Filters)].CondBtnClick(self);
    end;
    SearchBtnClick(self);
  end;
  RefForm.Show;
end;

procedure TConflictForm.AfterEditAction(Sender: TObject);
begin
  UpdateConflicts;
end;

initialization

end.

