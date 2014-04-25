unit data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil;

type

  { TDBData }

  TDBData = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DBData: TDBData;

implementation

{$R *.lfm}

{ TDBData }

end.

