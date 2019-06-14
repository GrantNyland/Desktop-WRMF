unit UDBConnection;

interface

uses
  Classes,UAbstractObject;

type

  TDBConnection  = class(TAbstractObject)
  protected
    FConnectionName   : string;
    FConnectionString : string;
    FDatabaseFileName : string;
    function Get_ConnectionName   : string;
    function Get_ConnectionString : string;
  public
    function Initialise : boolean; override;
    procedure Populate(AConnectionName,AConnectionString,ADatabaseFileName: string);
    property ConnectionName   : string read Get_ConnectionName;
    property ConnectionString : string read Get_ConnectionString;
    property DatabaseFileName : string read FDatabaseFileName;
  end;

implementation

uses
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TDBConnection }

function TDBConnection.Get_ConnectionName: string;
const OPNAME = 'TDBConnection.Get_ConnectionName';
begin
  Result := '';
  try
     Result := FConnectionName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDBConnection.Get_ConnectionString: string;
const OPNAME = 'TDBConnection.Get_ConnectionString';
begin
  Result := '';
  try
     Result := FConnectionString;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDBConnection.Initialise: boolean;
const OPNAME = 'TDBConnection.Initialise';
begin
  Result := False;
  try
    FConnectionName   := '';
    FConnectionString := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDBConnection.Populate(AConnectionName,AConnectionString,ADatabaseFileName: string);
const OPNAME = 'TDBConnection.Populate';
begin
  try
    FConnectionName   := AConnectionName;
    FConnectionString := AConnectionString;
    FDatabaseFileName := ADatabaseFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
