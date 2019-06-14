//
// Contains DLL interface declerations for the RainFall Fortran DLL.
//
unit URainFallFortranDLL;


interface


uses
  URainFallDelphiCallBacks;


//
// This class represents a single loaded instance of the RainFall Fortran DLL.
// Most of the functions in this class are direct wrappers for functions
// exported in the Fortran DLL.
//
// This class uses a member class to contain all the DLL function addresses.
// This cumbersome wrapper of a wrapper design is important. The intention is to
// reduce Fortran functionality over time and replace this with Delphi code. As
// functions are deleted from the Fortran DLL they will also be deleted from the
// member class. This wrapper class will then delegate the deleted function to
// another (Delphi) member class. The users of this class should not be affected
// by this change. This wrapper of a wrapper design will allow code to be migrated
// without affecting the larger system.
//
// The constructor of this class does not load the DLL. The DLL must be loaded
// by calling the LoadRainFallFortranDLL() function. The destructor will call the
// UnLoadRainFallFortranDLL() function automatically if it has not been called.
//
type
  TRainFallFortranDLL = class(TObject)
  private
    FRainFallFortranDLLHandle: longword;
    FRainFallFortranDLLFunctions: TObject;
    FCallBacks: TRainFallDelphiCallBacks;

    // Introduced in this class.
    function GetIsLoaded: boolean;
    function GetDLLName(ADebugVersion: boolean): string;
    procedure DoFortranPrint(AMessage: string);
    procedure DoFortranStop(AMessage: string);
    procedure DoFortranPassAddress(ABlockNumber: integer; AAddress: pointer);
  public

    // Construction, destruction.
    constructor Create;
    destructor Destroy; override;

    // DLL loading and unloading functions.
    function LoadRainFallFortranDLL(ADebugVersion: boolean): boolean;
    procedure UnLoadRainFallFortranDLL;

    // Introduced in this class.
    procedure DoFortranRunClassR;
    procedure DoFortranRunPatchR;

    // Properties
    property IsLoaded: boolean read GetIsLoaded;
  end;


implementation


//
// Implementation dependencies.
//
uses
  SysUtils,
  Windows,
  UDLLOperations,
  URainfallCommonBlocks,
  UFrmClassRPatchR,
  UErrorHandlingOperations;


{ TRainFallFortranDLLFunctions }


//
// Exported Fortran Function Declerations
// ======================================
//
// The calling convention for all functions in this project is "stdcall". The project setting
// in the Lahey compiler must be set to "Borland Delphi" which will set the calling convention
// to stdcall in Fortran. In Delphi the stdcall keyword must be placed on every function
// decleration because the default calling convention in Delphi is not stdcall.
//
type
  string10 = string[10];
  string250 = string[250];
  TDoPassGlobalAddresses = procedure (var ASender: integer); stdcall;
  TDoFortranAction = procedure (); stdcall;


//
// This class contains a function pointer for every function in the DLL.
//
type
  TRainFallFortranDLLFunctions = class(TObject)
  private
    FDLLFileName: string;
    FRunClassR: TDoFortranAction;
    FRunPatchR: TDoFortranAction;
    FPassGlobalAddresses: TDoPassGlobalAddresses;
  public

    // Construction and initialisation.
    constructor Create(ADLLFileName: string);
    function AssignFunctionPointers(ADLLHandle: longword): boolean;

    // Properties.
    property RunClassR: TDoFortranAction read FRunClassR write FRunClassR;
    property RunPatchR: TDoFortranAction read FRunPatchR write FRunPatchR;
    property PassGlobalAddresses: TDoPassGlobalAddresses read FPassGlobalAddresses write FPassGlobalAddresses;
  end;


//
// Sets all function pointers to nil.
//
constructor TRainFallFortranDLLFunctions.Create(ADLLFileName: string);
const OPNAME = 'TRainFallFortranDLLFunctions.Create';
begin
  try
    inherited Create;
    FDLLFileName := ADLLFileName;
    FRunClassR := nil;
    FRunPatchR := nil;
    FPassGlobalAddresses := nil;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Loads the addresses of all the function pointers from the DLL.
// Returns False if any function does not exist.
//
function TRainFallFortranDLLFunctions.AssignFunctionPointers(ADLLHandle: longword): boolean;
const OPNAME = 'TRainFallFortranDLLFunctions.AssignFunctionPointers';
begin
  Result := True;
  try
    if (not GetDLLFunction(ADLLHandle, FDLLFileName, 'RunClassR', @FRunClassR, OPNAME)) then
      Result := False;
    if (not GetDLLFunction(ADLLHandle, FDLLFileName, 'RunPatchR', @FRunPatchR, OPNAME)) then
      Result := False;
    if (not GetDLLFunction(ADLLHandle, FDLLFileName, 'PassGlobalAddresses', @FPassGlobalAddresses, OPNAME)) then
      Result := False;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TRainFallFortranDLL }


//
// Creates member objects and initialises member variables.
//
constructor TRainFallFortranDLL.Create;
const OPNAME = 'TRainFallFortranDLL.Create';
begin
  try

    // Call the ancestor.
    inherited Create;

    // Initialise DLL variables.
    FRainFallFortranDLLHandle := 0;
    FRainFallFortranDLLFunctions := nil;

    // Create the call back object that will be passed to the Fortran DLL.
    FCallBacks := TRainFallDelphiCallBacks.Create;
    FCallBacks.OnDelphiPrint := DoFortranPrint;
    FCallBacks.OnDelphiStop := DoFortranStop;
    FCallBacks.OnDelphiPassAddress := DoFortranPassAddress;

    // Create the global common blocks object.
    GFCB := TRainfallCommonBlocks.Create;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Destroys member objects. Unloads the DLL if still loaded.
//
destructor TRainFallFortranDLL.Destroy;
const OPNAME = 'TRainFallFortranDLL.Destroy';
begin
  try
    UnLoadRainFallFortranDLL;
    FreeAndNil(GFCB);
    FreeAndNil(FCallBacks);
    inherited Destroy;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Loads the RainFall Fortran DLL and assigns all function pointers.
//
function TRainFallFortranDLL.LoadRainFallFortranDLL(ADebugVersion: boolean): boolean;
const OPNAME = 'TRainFallFortranDLL.LoadRainFallFortranDLL';
var LBinPath: string;
begin
  Result := False;
  try

    // Determine the bin path. The IF is required because the DLL can be loaded from
    // an EXE which will be above BIN or another DLL which will be in BIN itself.
    // The Fortran DLL will always be in the BIN path so it is appended if necessary.
    LBinPath := UpperCase(ExtractFilePath(CurrentModuleFileName));
    if (Pos('BIN', LBinPath) <> (Length(LBinPath) - 3)) then
      LBinPath := LBinPath + 'Bin\';

    // Load the DLL.
    FRainFallFortranDLLFunctions := TRainFallFortranDLLFunctions.Create(GetDLLName(ADebugVersion));
    if LoadDLL(LBinPath + GetDLLName(ADebugVersion), FRainFallFortranDLLHandle, True, OPNAME) then
    begin
      if TRainFallFortranDLLFunctions(FRainFallFortranDLLFunctions).AssignFunctionPointers(FRainFallFortranDLLHandle) then
      begin

        // Bind the addresses.
        TRainFallFortranDLLFunctions(FRainFallFortranDLLFunctions).PassGlobalAddresses(integer(FCallBacks));
        Result := True;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Unloads the DLL and sets all function pointers to nil.
//
procedure TRainFallFortranDLL.UnLoadRainFallFortranDLL;
const OPNAME = 'TRainFallFortranDLL.UnLoadRainFallFortranDLL';
begin
  try
    GFCB.Reset;
    FreeAndNil(FRainFallFortranDLLFunctions);
    if (FRainFallFortranDLLHandle <> 0) then
    begin
      FreeLibrary(FRainFallFortranDLLHandle);
      FRainFallFortranDLLHandle := 0;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns True if the DLL is loaded.
//
function TRainFallFortranDLL.GetIsLoaded: boolean;
const OPNAME = 'TRainFallFortranDLL.GetIsLoaded';
begin
  Result := False;
  try
    Result := (FRainFallFortranDLLHandle <> 0);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the name of the DLL to load. The RainFall Fortran DLL consists of a single DLL which
// is loaded dynamically with a call to the LoadLibrary() API. The DLL is located relative
// to the application in a folder called "Bin". There are two versions of the DLL - one with
// run time checking and one without.
//
function TRainFallFortranDLL.GetDLLName(ADebugVersion: boolean): string;
const
  OPNAME = 'TRainFallFortranDLL.GetDLLName';
  CRainFallFortranDLLFileName      = 'RainFall.dll';
  CRainFallFortranDLLFileNameDebug = 'RainFall-Slow.dll';
begin
  Result := '';
  try
    if ADebugVersion then
    begin
      Result := CRainFallFortranDLLFileNameDebug;
    end else begin
      Result := CRainFallFortranDLLFileName;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// This event handler is called by the RainFall model every time text was output to the console.
// The text is added to the temp buffer. The temp buffer is transfered to the GUI with a
// timer tick or other periodic mechanism.
//
procedure TRainFallFortranDLL.DoFortranPrint(AMessage: string);
const OPNAME = 'TRainFallFortranDLL.DoFortranPrint';
var LCharIndex: integer;
begin
  try

    // Text sent from FORTRAN is padded with #255 which must be removed.
    for LCharIndex := Length(AMessage) downto 1 do
      if (AMessage[LCharIndex] = #255) then
        Delete(AMessage, LCharIndex, 1);

    FrmClassRPatchR.MmoText.Lines.Add(Copy(AMessage, 1, 10000));

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Sends the message and then throws an unhandled exception to stop
// the Fortran program where it is.
//
type TStopException = class(Exception);
procedure TRainFallFortranDLL.DoFortranStop(AMessage: string);
begin

  // Call the DelphiPrint() function to log the event.
  DoFortranPrint('STOP : ' + AMessage);

  // Raise an exception which will force the DLL to stop where it is.
  raise TStopException.Create('STOP : ' + AMessage);
end;


//
// Sets the global common blocks agent manages this function.
//
procedure TRainFallFortranDLL.DoFortranPassAddress(ABlockNumber: integer; AAddress: pointer);
const OPNAME = 'TRainFallFortranDLL.DoFortranPassAddress';
begin
  try

    // Set the pointer for the correct common block.
    GFCB.SetBlockAddress(ABlockNumber, AAddress);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Runs the target draft method.
//
procedure TRainFallFortranDLL.DoFortranRunClassR;
const OPNAME = 'TRainFallFortranDLL.DoFortranRunClassR';
begin
  try

    // Check that the model is loaded and all is fine.
    if (not (Assigned(FRainFallFortranDLLFunctions) and Assigned(TRainFallFortranDLLFunctions(FRainFallFortranDLLFunctions).RunClassR) )) then
    begin
      raise Exception.Create('This function can not be called if the RainFall Fortran DLL is not loaded. ');
    end else begin

      // Transfer to the FORTRAN DLL.
      try
        TRainFallFortranDLLFunctions(FRainFallFortranDLLFunctions).RunClassR;

      // Catch and discard the "STOP" exception.
      except on E: TStopException do end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Runs the reconciliation of demand method.
//
procedure TRainFallFortranDLL.DoFortranRunPatchR;
const OPNAME = 'TRainFallFortranDLL.DoFortranRunPatchR';
begin
  try

    // Check that the model is loaded and all is fine.
    if (not (Assigned(FRainFallFortranDLLFunctions) and Assigned(TRainFallFortranDLLFunctions(FRainFallFortranDLLFunctions).RunPatchR))) then
    begin
      raise Exception.Create('This function can not be called if the RainFall Fortran DLL is not loaded. ');
    end else begin

      // Transfer to the FORTRAN DLL.
      try
        TRainFallFortranDLLFunctions(FRainFallFortranDLLFunctions).RunPatchR;

      // Catch and discard the "STOP" exception.
      except on E: TStopException do end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
