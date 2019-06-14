//
//
//  UNIT      : Contains TNetworkElementData Class
//  AUTHOR    : Presley Mudau(Arivia)
//  DATE      : 21/04/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UDataFilePaths;

interface

uses
  Classes,
  UAbstractObject,
  VoaimsCom_TLB;
type
  TDataFilePaths = class(TAbstractAppObject,IDataFilePaths)
  protected
    FDataFilePrefix    : WideString;
    FDataFilePath      : WideString;
    FParamFileName     : WideString;
    FOutputFilePath    : WideString;
    FHydrologyFilePath : WideString;
    FDemandFilePath    : WideString;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Validate (var AErrors: WideString; const AContext: WideString='') : WordBool; safecall;
    function PopulateParamFile(AParamFileName : string): boolean;
    function PopulateDataFilePaths(ADataFilePrefix,ADataFilePath,AOutputFilePath,AHydrologyFilePath,ADemandFilePath: string): boolean;
    function Initialise: boolean; override;
    function Get_DataFilePrefix: WideString; safecall;
    function Get_DataFilePath: WideString; safecall;
    function Get_ParamFileName: WideString; safecall;
    function Get_OutputFilePath: WideString; safecall;
    function Get_HydrologyFilePath: WideString; safecall;
    function Get_DemandFilePath: WideString; safecall;
    property DataFilePrefix    : WideString read Get_DataFilePrefix;
    property DataFilePath      : WideString read Get_DataFilePath;
    property ParamFileName     : WideString read Get_ParamFileName;
    property OutputFilePath    : WideString read Get_OutputFilePath;
    property HydrologyFilePath : WideString read Get_HydrologyFilePath;
    property DemandFilePath    : WideString read Get_DemandFilePath;

  end;

implementation

{ TDataFilePaths }

uses
  SysUtils,
  VCL.Dialogs,
  VCL.Controls,
  UReservoirZoneDataSQLAgent,
  UErrorHandlingOperations;

procedure TDataFilePaths.CreateMemberObjects;
const OPNAME = 'TDataFilePaths.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataFilePaths.DestroyMemberObjects;
const OPNAME = 'TDataFilePaths.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFilePaths._AddRef: Integer;
const OPNAME = 'TDataFilePaths._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths._Release: Integer;
const OPNAME = 'TDataFilePaths._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.Initialise: boolean;
const OPNAME = 'TDataFilePaths.Initialise';
begin
  Result := False;
  try
    FDataFilePrefix := '';
    FDataFilePath   := '';
    FParamFileName  := '';
    FOutputFilePath := '';
    FHydrologyFilePath := '';
    FDemandFilePath    := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFilePaths.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TDataFilePaths.Validate';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.Get_DataFilePath: WideString;
const OPNAME = 'TDataFilePaths.Get_DataFilePath';
begin
  Result := '';
  try
    Result := FDataFilePath;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.Get_DataFilePrefix: WideString;
const OPNAME = 'TDataFilePaths.Get_DataFilePrefix';
begin
  Result := '';
  try
    Result := FDataFilePrefix;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.Get_OutputFilePath: WideString;
const OPNAME = 'TDataFilePaths.Get_OutputFilePath';
begin
  Result := '';
  try
    Result := FOutputFilePath;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.Get_ParamFileName: WideString;
const OPNAME = 'TDataFilePaths.Get_ParamFileName';
begin
  Result := '';
  try
    Result := FParamFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.Get_DemandFilePath: WideString;
const OPNAME = 'TDataFilePaths.Get_DemandFilePath';
begin
  Result := '';
  try
    Result := FDemandFilePath;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.Get_HydrologyFilePath: WideString;
const OPNAME = 'TDataFilePaths.Get_HydrologyFilePath';
begin
  Result := '';
  try
    Result := FHydrologyFilePath;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDataFilePaths.PopulateDataFilePaths(ADataFilePrefix,ADataFilePath,AOutputFilePath,AHydrologyFilePath,ADemandFilePath: string): boolean;
const OPNAME = 'TDataFilePaths.PopulateDataFilePaths';
begin
  Result := False;
  try
    FDataFilePrefix    := Trim(ADataFilePrefix);
    FDataFilePath      := Trim(ADataFilePath);
    FOutputFilePath    := Trim(AOutputFilePath);
    FHydrologyFilePath := Trim(AHydrologyFilePath);
    FDemandFilePath    := Trim(ADemandFilePath);
    if(FDataFilePath <> '') then FDataFilePath := IncludeTrailingPathDelimiter(FDataFilePath);
    if(FOutputFilePath <> '') then FOutputFilePath := IncludeTrailingPathDelimiter(FOutputFilePath);
    if(FHydrologyFilePath <> '') then FHydrologyFilePath := IncludeTrailingPathDelimiter(FHydrologyFilePath);
    if(FDemandFilePath <> '') then FDemandFilePath := IncludeTrailingPathDelimiter(FDemandFilePath);
    Result             := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataFilePaths.PopulateParamFile(AParamFileName: string): boolean;
const OPNAME = 'TDataFilePaths.PopulateParamFile';
begin
  Result := False;
  try
    FParamFileName := AParamFileName;
    Result         := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
