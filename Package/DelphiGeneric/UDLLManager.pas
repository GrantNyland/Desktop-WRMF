//
//
//  UNIT      : Contains the class TDLLManager.
//  AUTHOR    : Grant Nyland
//  DATE      : 2019/05/31
//  COPYRIGHT : Copyright © 2019 DWS.
//
//
unit UDLLManager;


interface


//
// Interface dependencies.
//
uses

  // Delphi
  Classes;


//
// A class that manages a single DLL. The descendent classes must implement
// the functions that are exported from the DLL by providing an implementation
// GetFunctionPointers();
//
type
  TDLLAgent = class(TObject)
  protected

    // Members
    FBinFolder: String;   // Contains the name of the sub folder that contains DLL's.
    FDllFileName: String; // Contains the DLL file name.
    FDllHandle: Int64;    // Contains the Windows DLL Library Handle.

    // Introduced in this class.
    function GetDLLFunction(var AFunctionPointer: Pointer; AFunctionName: String): Boolean;
    function GetFunctionPointers: Boolean; virtual;
    procedure ClearFunctionPointers; virtual;
  public

    // Construction, destruction.
    constructor Create; virtual;
    destructor Destroy; override;

    // Introduced in this class.
    function LoadDLL: Boolean; virtual;
    function IsDLLLoaded: Boolean; virtual;
    procedure UnLoadDLL; virtual;

    // Properties.
    property BinFolder: String read FBinFolder write FBinFolder;
    property DllFileName: String read FDllFileName write FDllFileName;
    property DllHandle: Int64 read FDllHandle;
  end;


//
// A class that manages a list of DLLs.
//
type
  TDLLManager = class(TObject)
  protected
    FBinFolder: String;   // Contains the name of the sub folder that contains the DLL's.
    FDLL: TStringList;    // Contains the names of the DLLs being managed.

    // Introduced in this class.
    procedure CreateDLLAgents; virtual;
    function CreateDLLAgent(I: Integer): TDLLAgent; virtual;
    procedure DestroyDLLAgents; virtual;
    function GetDLLAgent(AFileName: String): TDLLAgent; virtual;
    function GetFunctionPointers: Boolean; virtual;
    procedure ClearFunctionPointers; virtual;
  public

    // Construction, destruction.
    constructor Create;
    destructor Destroy; override;

    // Introduced in this class.
    function LoadAll: Boolean; virtual;
    procedure UnLoadAll; virtual;
    function AreAllLoaded: Boolean; virtual;

    // Properties.
    property DLL[AFileName: String]: TDLLAgent read GetDLLAgent; default;
  end;


implementation


//
// Implementation dependencies.
//
uses

  // Delphi
  SysUtils, Windows, Forms,

  // DWS
  UErrorHandlingOperations;


{ TDLLAgent }


//
// Constructor.
//
constructor TDLLAgent.Create;
const OPNAME = 'TDLLAgent.Create';
begin
  try

    // Call the ancestor.
    inherited Create;

    // Set the defaults.
    FBinFolder := 'bin\';
    FDllFileName := '';
    FDllHandle := 0;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Destruction.
//
destructor TDLLAgent.Destroy;
const OPNAME = 'TDLLAgent.Destroy';
begin
  try

    // Unload the DLL.
    UnLoadDLL;

    // Call the ancestor.
    inherited Destroy;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the DLL.
//
function TDLLAgent.LoadDLL: Boolean;
const OPNAME = 'TDLLAgent.LoadDLL';
var LFullFileName: String;
begin
  Result := False;
  try

    // Make sure the DLL is not currently loaded.
    UnLoadDLL;

    // Return True if the agent is not currently active.
    if ((LowerCase(FDLLFileName) = 'none') or (Trim(FDLLFileName) = '')) then
    begin
      Result := True;
    end else begin

      // Check that file exists.
      LFullFileName := ExtractFilePath(Application.ExeName) + FBinFolder + ExtractFileName(FDLLFileName);
      if (not FileExists(LFullFileName)) then
      begin
        raise Exception.CreateFmt('Could not locate DLL file [%s].', [LFullFileName]);
      end else begin

        // Attempt to load the DLL.
        FDllHandle := LoadLibrary(PChar(LFullFileName));
        if (FDLLHandle = 0) then
        begin
          raise Exception.CreateFmt('Could not load DLL module [%s]. %s', [LFullFileName, GetLastErrorText]);
        end else begin

          // Call the virtual method to load all the exported functions.
          if GetFunctionPointers then
          begin

            // Done.
            Result := True;
          end;
        end;
      end;
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Returns True if the DLL is loaded.
//
function TDLLAgent.IsDLLLoaded: Boolean;
const OPNAME = 'TDLLAgent.IsDLLLoaded';
begin
  Result := False;
  try
    if (FDllHandle <> 0) then
      Result := True;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Un-Loads the DLL.
//
procedure TDLLAgent.UnLoadDLL;
const OPNAME = 'TDLLAgent.UnLoadDLL';
begin
  try

    // Make sure the DLL is loaded.
    if IsDLLLoaded then
    begin

      // Clear all the exported function pointers.
      ClearFunctionPointers;

      // Unload the DLL.
      FreeLibrary(FDllHandle);
      FDllHandle := 0;
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Gets the address of the function.
//
function TDLLAgent.GetDLLFunction(var AFunctionPointer: Pointer; AFunctionName: String): Boolean;
const OPNAME = 'TDLLAgent.GetDLLFunction';
begin
  Result := False;
  AFunctionPointer := nil;
  try

    // Attempt to obtain the function by name.
    if IsDLLLoaded then
    begin
      AFunctionPointer := GetProcAddress(FDLLHandle, PChar(AFunctionName));

      // Report an error if the function was not found.
      if (AFunctionPointer = nil) then
        raise Exception.CreateFmt('The DLL function [%s] was not found in [%s]. %s',
          [AFunctionName, ExtractFileName(FDLLFileName), GetLastErrorText]);

      // Set the return result.
      Result := True;
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function TDLLAgent.GetFunctionPointers: Boolean;
const OPNAME = 'TDLLAgent.GetFunctionPointers';
begin
  Result := False;
  try

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure TDLLAgent.ClearFunctionPointers;
const OPNAME = 'TDLLAgent.ClearFunctionPointers';
begin
  try

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TDLLManager }


constructor TDLLManager.Create;
const OPNAME = 'TDLLManager.Create';
begin
  try
    inherited Create;
    FBinFolder := 'bin\';
    FDLL := TStringList.Create;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


destructor TDLLManager.Destroy;
const OPNAME = 'TDLLManager.Destroy';
begin
  try
    DestroyDLLAgents;
    FreeAndNil(FDLL);
    inherited Destroy;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDLLManager.CreateDLLAgents;
const OPNAME = 'TDLLManager.CreateDLLAgents';
var
  I: Integer;
  LAgent: TDLLAgent;
begin
  try
    for I := 0 to FDLL.Count - 1 do
    begin
      LAgent := CreateDLLAgent(I);
      FDLL.Objects[I] := LAgent;
      LAgent.BinFolder := FBinFolder;
      LAgent.DllFileName := FDLL[I];
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDLLManager.CreateDLLAgent(I: Integer): TDLLAgent;
begin
  Result := TDLLAgent.Create;
end;


procedure TDLLManager.DestroyDLLAgents;
const OPNAME = 'TDLLManager.DestroyDLLAgents';
var
  I: Integer;
  LAgent: TDLLAgent;
begin
  try
    for I := FDLL.Count - 1 downto 0 do
    begin
      LAgent := TDLLAgent(FDLL.Objects[I]);
      if Assigned(LAgent) then
      begin
        LAgent.UnLoadDLL;
        FDLL.Objects[I] := nil;
        FreeAndNil(LAgent);
      end;
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDLLManager.LoadAll: Boolean;
const OPNAME = 'TDLLManager.LoadAll';
var
  I: Integer;
  LAgent: TDLLAgent;
  LLoadResult: Boolean;
begin
  Result := False;
  try
    LLoadResult := True;
    for I := 0 to FDLL.Count - 1 do
    begin
      LAgent := TDLLAgent(FDLL.Objects[I]);
      if Assigned(LAgent) then
      begin
        if (not LAgent.LoadDLL) then
        begin
          LLoadResult := False;
          UnLoadAll;
          break;
        end;
      end;
    end;
    if LLoadResult then
      Result := GetFunctionPointers;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDLLManager.UnLoadAll;
const OPNAME = 'TDLLManager.UnLoadAll';
var
  I: Integer;
  LAgent: TDLLAgent;
begin
  try
    for I := FDLL.Count - 1 downto 0 do
    begin
      LAgent := TDLLAgent(FDLL.Objects[I]);
      if Assigned(LAgent) then
        LAgent.UnLoadDLL;
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDLLManager.AreAllLoaded: Boolean;
const OPNAME = 'TDLLManager.AreAllLoaded';
var
  I: Integer;
  LAgent: TDLLAgent;
  LLoadResult: Boolean;
begin
  Result := False;
  try
    LLoadResult := True;
    for I := 0 to FDLL.Count - 1 do
    begin
      LAgent := TDLLAgent(FDLL.Objects[I]);
      if Assigned(LAgent) then
      begin
        if (not LAgent.IsDLLLoaded) then
        begin
          LLoadResult := False;
          break;
        end;
      end;
    end;
    Result := LLoadResult;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDLLManager.GetDLLAgent(AFileName: String): TDLLAgent;
const OPNAME = 'TDLLManager.GetDLLAgent';
var I: Integer;
begin
  Result := nil;
  try
    if Assigned(FDLL) then
    begin
      I := FDLL.IndexOf(AFileName);
      if (I >= 0) then
        Result := TDLLAgent(FDLL.Objects[I]);
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLLs.
//
function TDLLManager.GetFunctionPointers: Boolean;
const OPNAME = 'TDLLManager.GetFunctionPointers';
var
  I: Integer;
  LAgent: TDLLAgent;
  LLoadResult: Boolean;
begin
  Result := False;
  try
    LLoadResult := True;
    for I := 0 to FDLL.Count - 1 do
    begin
      LAgent := TDLLAgent(FDLL.Objects[I]);
      if Assigned(LAgent) then
      begin
        if (not LAgent.GetFunctionPointers) then
        begin
          LLoadResult := False;
          ClearFunctionPointers;
          break;
        end;
      end;
    end;
    Result := LLoadResult;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure TDLLManager.ClearFunctionPointers;
const OPNAME = 'TDLLManager.ClearFunctionPointers';
var
  I: Integer;
  LAgent: TDLLAgent;
begin
  try
    for I := FDLL.Count - 1 downto 0 do
    begin
      LAgent := TDLLAgent(FDLL.Objects[I]);
      if Assigned(LAgent) then
        LAgent.ClearFunctionPointers;
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
