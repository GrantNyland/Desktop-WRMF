//
//
//  UNIT      : Contains TGlobalData Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGlobalData;

interface

uses
  //Data.Win.ADODB,
  UDWADBComponents,
  UAbstractObject;

type
  TGlobalData = class(TAbstractGlobalData)
  protected
    FLastError: integer;
    FLastErrorMessage,
    FApplicationName,
    FApplicationVersion : string;
    FDatabaseName : TDWAConnection;
    FStopOnFirstErr,
    FAutoSelectStudy: boolean;
    FAutoSelectUser: boolean;
    FIncludeHydrologyFiles: boolean;
    FIncludeDemandFiles: boolean;
    FCOMServerMode: boolean;
  public
    function ApplicationName: string; override;
    function SetApplicationName(AApplicationName: string): boolean; override;
    function ApplicationVersion: string; override;
    function SetApplicationVersion(AApplicationVersion: string): boolean; override;
    function DatabaseName: TDWAConnection; override;
    function SetDatabaseName(ADatabaseName: TDWAConnection): boolean; override;
    function StopOnFirstErr: boolean; override;
    function SetStopOnFirstErr(AStopOnFirstErr: boolean): boolean; override;
    function IncludeHydrologyFiles: boolean; override;
    function SetIncludeHydrologyFiles(AInclude: boolean): boolean; override;
    function IncludeDemandFiles: boolean; override;
    function SetIncludeDemandFiles(AInclude: boolean): boolean; override;
    function COMServerMode: boolean; override;
    function SetCOMServerMode(AMode: boolean): boolean; override;
    function AutoSelectStudy: boolean; override;
    function SetAutoSelectStudy(AAutoSelectStudy: boolean): boolean; override;
    function AutoSelectUser: boolean; override;
    function SetAutoSelectUser(AAutoSelectUser: boolean): boolean; override;
    function Initialise(AIniFile : TAbstractConfiguration): boolean;reintroduce;
    function SaveState(AIniFile : TAbstractConfiguration): boolean;reintroduce;
    procedure SetLastError(AErrorCode: integer); override;
    function GetLastError: integer; override;
    procedure SetLastErrorMessage(AErrorMessage: string); override;
    function GetLastErrorMessage: string; override;
    function SQLReservedWordsInTablesCommaText: string; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

function TGlobalData.ApplicationName: string;
const OPNAME = 'TGlobalData.ApplicationName';
begin
  Result := '';
  try
    Result := FApplicationName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.ApplicationVersion: string;
const OPNAME = 'TGlobalData.ApplicationVersion';
begin
  Result := '';
  try
    Result := FApplicationVersion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.DatabaseName: TDWAConnection;
const OPNAME = 'TGlobalData.DatabaseName';
begin
  Result := nil;
  try
    Result := FDatabaseName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetApplicationName(AApplicationName: string): boolean;
const OPNAME = 'TGlobalData.SetApplicationName';
begin
  Result := False;
  try
    FApplicationName := AApplicationName;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetApplicationVersion(
  AApplicationVersion: string): boolean;
const OPNAME = 'TGlobalData.SetApplicationVersion';
begin
  Result := False;
  try
    FApplicationName := AApplicationVersion;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetDatabaseName(ADatabaseName: TDWAConnection): boolean;
const OPNAME = 'TGlobalData.SetDatabaseName';
begin
  Result := False;
  try
    FDatabaseName := ADatabaseName;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.Initialise(AIniFile: TAbstractConfiguration): boolean;
const OPNAME = 'TGlobalData.Initialise';
var
  LBool: string;

begin
  Result := False;
  if not Assigned(AIniFile) then
    raise Exception.Create('INI file object parameter is not yet assigned.');
  try
    LBool := AIniFile.ReadString('SYSTEM','ResetStopOnFirstError','');
    if(LBool = '') then
    begin
      AIniFile.WriteString('SYSTEM','ResetStopOnFirstError','1');
      AIniFile.WriteString('SYSTEM','StopOnFirstError','N');
    end;
    //LBool := AIniFile.ReadString('SYSTEM','StopOnFirstError','');
    FStopOnFirstErr := False;//(Uppercase(Trim(LBool)) = 'Y');
    LBool := AIniFile.ReadString('SYSTEM','IncludeHydrologyFiles','');
    FIncludeHydrologyFiles := (Uppercase(Trim(LBool)) = 'Y');
    LBool := AIniFile.ReadString('SYSTEM','IncludeDemandFiles','');
    FIncludeDemandFiles := (Uppercase(Trim(LBool)) = 'Y');
    LBool := AIniFile.ReadString('USER','AutoLogon','');
    FAutoSelectUser := (Uppercase(Trim(LBool)) = 'Y');
    LBool := AIniFile.ReadString('STUDY','AutoStudy','');
    FAutoSelectStudy := (Uppercase(Trim(LBool)) = 'Y');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetStopOnFirstErr(AStopOnFirstErr: boolean): boolean;
const OPNAME = 'TGlobalData.SetStopOnFirstErr';
begin
  Result := False;
  try
    FStopOnFirstErr := AStopOnFirstErr;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SQLReservedWordsInTablesCommaText: string;
const OPNAME = 'TGlobalData.SQLReservedWordsInTablesCommaText';
begin
  Result := '';
  try
    Result := 'Month,Password,Position,Section,Year';;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.StopOnFirstErr: boolean;
const OPNAME = 'TGlobalData.StopOnFirstErr';
begin
  Result := False;
  try
    Result := FStopOnFirstErr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.AutoSelectStudy: boolean;
const OPNAME = 'TGlobalData.AutoSelectStudy';
begin
  Result := False;
  try
    Result := FAutoSelectStudy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.AutoSelectUser: boolean;
const OPNAME = 'TGlobalData.AutoSelectUser';
begin
  Result := False;
  try
    Result := FAutoSelectUser;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetAutoSelectStudy(AAutoSelectStudy: boolean): boolean;
const OPNAME = 'TGlobalData.SetAutoSelectStudy';
begin
  Result := False;
  try
    FAutoSelectStudy := AAutoSelectStudy;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetAutoSelectUser(AAutoSelectUser: boolean): boolean;
const OPNAME = 'TGlobalData.SetAutoSelectUser';
begin
  Result := False;
  try
    FAutoSelectUser := AAutoSelectUser;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TGlobalData.SaveState(AIniFile : TAbstractConfiguration): boolean;
const OPNAME = 'TGlobalData.SaveState';
begin
  Result := False;
  if not Assigned(AIniFile) then
    raise Exception.Create('INI file object already destroyed.');
  try
    if FStopOnFirstErr then
      AIniFile.WriteString('SYSTEM','StopOnFirstError','Y')
    else
      AIniFile.WriteString('SYSTEM','StopOnFirstError','N');
    if FIncludeHydrologyFiles then
      AIniFile.WriteString('SYSTEM','IncludeHydrologyFiles','Y')
    else
      AIniFile.WriteString('SYSTEM','IncludeHydrologyFiles','N');
    if FIncludeDemandFiles then
      AIniFile.WriteString('SYSTEM','IncludeDemandFiles','Y')
    else
      AIniFile.WriteString('SYSTEM','IncludeDemandFiles','N');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.GetLastError: integer;
const OPNAME = 'TGlobalData.GetLastError';
begin
  Result := 0;
  try
    Result := FLastError;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.GetLastErrorMessage: string;
const OPNAME = 'TGlobalData.GetLastErrorMessage';
begin
  Result := '';
  try
    Result := FLastErrorMessage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGlobalData.SetLastError(AErrorCode: integer);
const OPNAME = 'TGlobalData.SetLastError';
begin
  try
    FLastError := AErrorCode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGlobalData.SetLastErrorMessage(AErrorMessage: string);
const OPNAME = 'TGlobalData.SetLastErrorMessage';
begin
  try
    FLastErrorMessage := AErrorMessage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.IncludeHydrologyFiles: boolean;
const OPNAME = 'TGlobalData.IncludeHydrologyFiles';
begin
  Result := False;
  try
    Result := FIncludeHydrologyFiles;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.IncludeDemandFiles: boolean;
const OPNAME = 'TGlobalData.IncludeDemandFiles';
begin
  Result := False;
  try
    Result := FIncludeDemandFiles;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetIncludeDemandFiles(AInclude: boolean): boolean;
const OPNAME = 'TGlobalData.SetIncludeDemandFiles';
begin
  Result := False;
  try
    FIncludeDemandFiles := AInclude;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetIncludeHydrologyFiles(AInclude: boolean): boolean;
const OPNAME = 'TGlobalData.SetIncludeHydrologyFiles';
begin
  Result := False;
  try
    FIncludeHydrologyFiles := AInclude;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.COMServerMode: boolean;
const OPNAME = 'TGlobalData.COMServerMode';
begin
  Result := False;
  try
    Result := FCOMServerMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGlobalData.SetCOMServerMode(AMode: boolean): boolean;
const OPNAME = 'TGlobalData.SetCOMServerMode';
begin
  Result := False;
  try
    FCOMServerMode := AMode;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
