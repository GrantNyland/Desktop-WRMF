//
//  UNIT      : Contains TAppModulesWithDatabase Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//

unit UAppModulesWithDatabase;

interface

uses
  UConfiguration,
  UAbstractObject;

type
  TAppModulesWithDatabase = class(TAppModules)
  protected
    FCanProceed : boolean;
    FIniFile: TConfiguration;
    FViewIni: TConfiguration;
    FDatabase: TAbstractDatabaseLayer;
    function CreateMSAccessDatabaseLayer : TAbstractDatabaseLayer;
    function CreateInformixDatabaseLayer  : TAbstractDatabaseLayer;
    function CreateParadoxDatabaseLayer  : TAbstractDatabaseLayer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDatabaseType: TDatabaseType; virtual;
    function CheckForRequiredModules(ADLLNames: array of string): boolean;
  public
    function Initialise: boolean; override;
    function IniFile: TAbstractConfiguration; override;
    function ViewIni: TAbstractConfiguration; override;
    function Database: TAbstractDatabaseLayer; override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
  end;


implementation

uses

  // Delphi
  VCL.Forms,
  SysUtils,
  Windows,

  // DWAF (Arivia)
  USQLDatabaseLayer,
  UDLLOperations,
  UEmptySQLDatasetConstructor,
  UErrorHandlingOperations;

procedure TAppModulesWithDatabase.CreateMemberObjects;
const OPNAME = 'TAppModulesWithDatabase.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIniFile  := TConfiguration.Create(itApplicationConfiguration);
    FViewIni  := TConfiguration.Create(itViewSettings);
    FDatabase := nil;
    case GetDatabaseType of
      dbtAccessODBC : begin
                        FCanProceed := CheckForRequiredModules(['MSAccessDatabaseLayer.dll']);
                        if FCanProceed then
                          FDatabase := CreateMSAccessDatabaseLayer;
                      end;
      dbtInformix   : begin
                        FCanProceed := CheckForRequiredModules(['InformixDatabaseLayer.dll']);
                        if FCanProceed then
                          FDatabase := CreateInformixDatabaseLayer;
                      end;
      dbtParadox    : begin
                        FCanProceed := CheckForRequiredModules(['ParadoxDatabaseLayer.dll']);
                        if FCanProceed then
                          FDatabase := CreateParadoxDatabaseLayer;
                      end;
    end;
    if FCanProceed then
      TSQLDatabaseLayer(FDatabase).AddDataSetConstructor(TEmptySQLDatasetConstructor.Create(self));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModulesWithDatabase.DestroyMemberObjects;
const OPNAME = 'TAppModulesWithDatabase.DestroyMemberObjects';
begin
  try
    if Assigned(FDatabase) then
      TSQLDatabaseLayer(FDatabase).DeleteDataSetConstructorsOfType(TEmptySQLDatasetConstructor);
    FreeAndNil(FDatabase);
    FreeAndNil(FIniFile);
    FreeAndNil(FViewIni);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesWithDatabase.Initialise: boolean;
const OPNAME = 'TAppModulesWithDatabase.Initialise';
begin
//  ProcessFunctionCall(OPNAME);
  Result := False;
  try
    if Assigned(FDatabase) then
    begin
      FDatabase.Connected := True;
      if FDatabase.Connected then
      begin
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
//  ProcessFunctionReturn(OPNAME);
end;

function TAppModulesWithDatabase.GetDatabaseType: TDatabaseType;
const OPNAME = 'TAppModulesWithDatabase.GetDatabaseType';
var LDatabaseType: string;
begin
  Result := dbtUnknown;
  try
    LDatabaseType := UpperCase(FIniFile.ReadString('DatabaseLayer', 'DatabaseType', 'AccessODBC'));
    if (LDatabaseType = 'ACCESSODBC') then Result := dbtAccessODBC else
    if (LDatabaseType = 'INFORMIX')   then Result := dbtInformix   else
    if (LDatabaseType = 'PARADOX')    then Result := dbtParadox    else
    raise Exception.CreateFmt('Unknown database type [%s]. Allowed: AccessODBC, Informix, Paradox.', [LDatabaseType]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesWithDatabase.IniFile: TAbstractConfiguration;
const OPNAME = 'TAppModulesWithDatabase.IniFile';
begin
  Result := nil;
  try
    Result := FIniFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesWithDatabase.ViewIni: TAbstractConfiguration;
const OPNAME = 'TAppModulesWithDatabase.ViewIni';
begin
  Result := nil;
  try
    Result := FViewIni;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesWithDatabase.Database: TAbstractDatabaseLayer;
const OPNAME = 'TAppModulesWithDatabase.Database';
begin
  Result := nil;
  try
    Result := FDatabase;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesWithDatabase.CreateMSAccessDatabaseLayer : TAbstractDatabaseLayer;
const OPNAME = 'TAppModulesWithDatabase.CreateMSAccessDatabaseLayer';
begin
  Result := nil;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\MSAccessDatabaseLayer.dll',
      TAbstractAppObject(Result), self, True, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesWithDatabase.CreateInformixDatabaseLayer: TAbstractDatabaseLayer;
const OPNAME = 'TAppModulesWithDatabase.CreateInformixDatabaseLayer';
begin
  Result := nil;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\InformixDatabaseLayer.dll',
      TAbstractAppObject(Result), self, True, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesWithDatabase.CreateParadoxDatabaseLayer: TAbstractDatabaseLayer;
const OPNAME = 'TAppModulesWithDatabase.CreateParadoxDatabaseLayer';
begin
  Result := nil;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ParadoxDatabaseLayer.dll',
      TAbstractAppObject(Result), self, True, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAppModulesWithDatabase.CheckForRequiredModules(ADLLNames: array of string): boolean;
const OPNAME = 'TAppModulesWithDatabase.CheckForRequiredModules';
{$IFNDEF MERGE_DLLS}
var
  LIndex: integer;
  LFullName: string;
{$ENDIF}
begin
  Result := False;
  try
{$IFNDEF MERGE_DLLS}
    for LIndex := Low(ADLLNames) to High(ADLLNames) do
    begin
      LFullName := ExtractFilePath(ApplicationExeName) + 'Bin\' + ADLLNames[LIndex];
      if (not FileExists(LFullName)) then
      begin
        FCanProceed := False;
        // This dialogue normally appears over the splash so currently there is no need to hide the splash screen first
        // other alternative is to show the errors in the splash screen
        raise Exception.Create('The application can not run without the library module [' + ADLLNames[LIndex] + '].');
      end;
    end;
{$ENDIF}
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModulesWithDatabase.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TAppModulesWithDatabase.ProcessEvent';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
