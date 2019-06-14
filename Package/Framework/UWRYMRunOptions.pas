//
//
//  UNIT      : Contains TWRYMRunOptions Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 21/05/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UWRYMRunOptions;

interface

uses
  Classes,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TWRYMRunOptions = class(TAbstractAppObject,IWRYMRunOptions)
  protected
    FRunSilent: boolean;
    FAutoRun: boolean;
    FWRYM_DAT: string;
    FCloseOnComplete: boolean;
    FRunDebugVersion: boolean;
    FOptimiseMemory: boolean;
    FCreateSumOutFile: boolean;
    FSaveOutputAsBinaryFile: boolean;
    FSaveOutputToDB: boolean;
    FFirmYield: Double;
    FSumOutBlobAddress: LongWord;
    FBlobSize: Integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function Get_RunSilent: WordBool; safecall;
    procedure Set_RunSilent(Value: WordBool); safecall;
    function Get_AutoRun: WordBool; safecall;
    procedure Set_AutoRun(Value: WordBool); safecall;
    function Get_CloseOnComplete: WordBool; safecall;
    procedure Set_CloseOnComplete(Value: WordBool); safecall;
    function Get_RunDebugVersion: WordBool; safecall;
    procedure Set_RunDebugVersion(Value: WordBool); safecall;
    function Get_OptimiseMemory: WordBool; safecall;
    procedure Set_OptimiseMemory(Value: WordBool); safecall;
    function Get_WRYM_DAT: WideString; safecall;
    procedure Set_WRYM_DAT(const Value: WideString); safecall;
    function Get_CreateSumOutFile: WordBool; safecall;
    procedure Set_CreateSumOutFile(Value: WordBool); safecall;
    function Get_SaveOutputAsBinaryFile: WordBool; safecall;
    procedure Set_SaveOutputAsBinaryFile(Value: WordBool); safecall;
    function Get_SaveOutputToDB: WordBool; safecall;
    procedure Set_SaveOutputToDB(Value: WordBool); safecall;
    function Get_FirmYield: Double; safecall;
    procedure Set_FirmYield(Value: Double); safecall;
    function Get_SumOutBlobAddress: LongWord; safecall;
    procedure Set_SumOutBlobAddress(Value: LongWord); safecall;
    function Get_BlobSize: Integer; safecall;
    procedure Set_BlobSize(Value: Integer); safecall;
  public
    function Initialise: boolean; override;
    procedure SaveToINI; safecall;
    property RunSilent: WordBool read Get_RunSilent write Set_RunSilent;
    property AutoRun: WordBool read Get_AutoRun write Set_AutoRun;
    property CloseOnComplete: WordBool read Get_CloseOnComplete write Set_CloseOnComplete;
    property RunDebugVersion: WordBool read Get_RunDebugVersion write Set_RunDebugVersion;
    property OptimiseMemory: WordBool read Get_OptimiseMemory write Set_OptimiseMemory;
    property WRYM_DAT: WideString read Get_WRYM_DAT write Set_WRYM_DAT;
    property CreateSumOutFile: WordBool read Get_CreateSumOutFile write Set_CreateSumOutFile;
    property SaveOutputAsBinaryFile: WordBool read Get_SaveOutputAsBinaryFile write Set_SaveOutputAsBinaryFile;
    property SaveOutputToDB: WordBool read Get_SaveOutputToDB write Set_SaveOutputToDB;
    property FirmYield: Double read Get_FirmYield write Set_FirmYield;
    property SumOutBlobAddress: LongWord read Get_SumOutBlobAddress write Set_SumOutBlobAddress;
    property BlobSize: Integer read Get_BlobSize write Set_BlobSize;

  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

  { TWRYMRunOptions }

function TWRYMRunOptions._AddRef: Integer;
const OPNAME = 'TWRYMRunOptions._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRYMRunOptions._Release: Integer;
const OPNAME = 'TWRYMRunOptions._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRYMRunOptions.CreateMemberObjects;
const OPNAME = 'TWRYMRunOptions.CreateMemberObjects';
begin
  inherited;
  try
    FRunSilent              := False;
    FAutoRun                := False;
    FWRYM_DAT               := '';
    FCloseOnComplete        := False;
    FRunDebugVersion        := False;
    FCreateSumOutFile       := False;
    FSaveOutputAsBinaryFile := False;
    FSaveOutputToDB         := False;
    FOptimiseMemory         := False;
    FFirmYield              := 0.0;
    FSumOutBlobAddress      := 0;
    FBlobSize               := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.DestroyMemberObjects;
const OPNAME = 'TWRYMRunOptions.DestroyMemberObjects';
begin
  inherited;
  try
    SaveToINI;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.SaveToINI;
const OPNAME = 'TWRYMRunOptions.SaveToINI';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'RunSilent', Ord(FRunSilent));
    FAppModules.IniFile.WriteInteger(ClassName,'AutoRun', Ord(FAutoRun));
    FAppModules.IniFile.WriteInteger(ClassName,'CloseOnComplete', Ord(FCloseOnComplete));
    FAppModules.IniFile.WriteInteger(ClassName,'RunDebugVersion', Ord(FRunDebugVersion));
    FAppModules.IniFile.WriteInteger(ClassName,'OptimiseMemory', Ord(FOptimiseMemory));    
    FAppModules.IniFile.WriteString(ClassName,'WRYM_DAT', FWRYM_DAT);
    FAppModules.IniFile.WriteInteger(ClassName,'CreateSumOutFile', Ord(FCreateSumOutFile));
    FAppModules.IniFile.WriteInteger(ClassName,'SaveOutputAsBinaryFile', Ord(FSaveOutputAsBinaryFile));
    FAppModules.IniFile.WriteInteger(ClassName,'SaveOutputToDB', Ord(FSaveOutputToDB));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Initialise: boolean;
const OPNAME = 'TWRYMRunOptions.Initialise';
begin
  Result := inherited Initialise;
  try
    FRunSilent       := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'RunSilent', 0));
    FAutoRun         := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'AutoRun', 0));
    FCloseOnComplete := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'CloseOnComplete', 0));
    FRunDebugVersion := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'RunDebugVersion', 0));
    FOptimiseMemory  := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'OptimiseMemory', 0));
    FWRYM_DAT        := FAppModules.IniFile.ReadString(ClassName,'WRYM_DAT', '');
    FCreateSumOutFile       := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'CreateSumOutFile', 0));
    FSaveOutputAsBinaryFile := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'SaveOutputAsBinaryFile', 0));
    FSaveOutputToDB         := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'SaveOutputToDB', 0));
    FFirmYield              := 0.0;
    FSumOutBlobAddress      := 0;
    Result           := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_AutoRun: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_AutoRun';
begin
  Result := False;
  try
    Result := FAutoRun;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_CloseOnComplete: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_CloseOnComplete';
begin
  Result := False;
  try
    Result := FCloseOnComplete;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_RunDebugVersion: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_RunDebugVersion';
begin
  Result := False;
  try
    Result := FRunDebugVersion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_OptimiseMemory: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_OptimiseMemory';
begin
  Result := False;
  try
    Result := FOptimiseMemory;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_RunSilent: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_RunSilent';
begin
  Result := False;
  try
    Result := FRunSilent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_WRYM_DAT: WideString;
const OPNAME = 'TWRYMRunOptions.Get_WRYM_DAT';
begin
  Result := '';
  try
    Result := FWRYM_DAT;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWRYMRunOptions.Set_AutoRun(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_AutoRun';
begin
  try
    FAutoRun := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_CloseOnComplete(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_CloseOnComplete';
begin
  try
    FCloseOnComplete := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_RunDebugVersion(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_RunDebugVersion';
begin
  try
    FRunDebugVersion := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_OptimiseMemory(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_OptimiseMemory';
begin
  try
    FOptimiseMemory := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_RunSilent(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_RunSilent';
begin
  try
    FRunSilent := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_WRYM_DAT(const Value: WideString);
const OPNAME = 'TWRYMRunOptions.Set_WRYM_DAT';
begin
  try
    FWRYM_DAT := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_CreateSumOutFile: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_CreateSumOutFile';
begin
  Result := False;
  try
    Result := FCreateSumOutFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_SaveOutputAsBinaryFile: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_SaveOutputAsBinaryFile';
begin
  Result := False;
  try
    Result := FSaveOutputAsBinaryFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_SaveOutputToDB: WordBool;
const OPNAME = 'TWRYMRunOptions.Get_SaveOutputToDB';
begin
  Result := False;
  try
    Result := FSaveOutputToDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_CreateSumOutFile(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_CreateSumOutFile';
begin
  try
    FCreateSumOutFile := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_SaveOutputAsBinaryFile(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_SaveOutputAsBinaryFile';
begin
  try
    FSaveOutputAsBinaryFile := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_SaveOutputToDB(Value: WordBool);
const OPNAME = 'TWRYMRunOptions.Set_SaveOutputToDB';
begin
  try
    FSaveOutputToDB := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_FirmYield: Double;
const OPNAME = 'TWRYMRunOptions.Get_FirmYield';
begin
  Result := 0.0;
  try
    Result := FFirmYield;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_SumOutBlobAddress: LongWord;
const OPNAME = 'TWRYMRunOptions.Get_SumOutBlobAddress';
begin
  Result := 0;
  try
    Result := FSumOutBlobAddress;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_FirmYield(Value: Double);
const OPNAME = 'TWRYMRunOptions.Set_FirmYield';
begin
  try
    FFirmYield := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_SumOutBlobAddress(Value: LongWord);
const OPNAME = 'TWRYMRunOptions.Set_SumOutBlobAddress';
begin
  try
    FSumOutBlobAddress := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRYMRunOptions.Get_BlobSize: Integer;
const OPNAME = 'TWRYMRunOptions.Get_BlobSize';
begin
  Result := 0;
  try
    Result := FBlobSize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRYMRunOptions.Set_BlobSize(Value: Integer);
const OPNAME = 'TWRYMRunOptions.Set_BlobSize';
begin
  try
    FBlobSize := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
