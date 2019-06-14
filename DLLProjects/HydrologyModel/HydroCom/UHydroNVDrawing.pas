(******************************************************************************)
(*  Contains : Class THydroNVDrawing.
(******************************************************************************)
unit UHydroNVDrawing;


interface

uses
  Classes,
  Contnrs,

  UModule,
  UAbstractObject,
  HydrologyCom_TLB;

type
  THydroNVDrawing = class(TAbstractObject, IHydroNVDrawing)
  protected
    FDrawingID       : Integer;
    FNetworkID       : Integer;
    FDrawingName     : String;
    FGISDrawing      : Integer;
    FReadOnly        : Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_NetworkID: Integer; safecall;
    function Get_DrawingID: Integer; safecall;
    function Get_DrawingName: WideString; safecall;
    procedure Set_DrawingName(const Value: WideString); safecall;
    function Get_GISDrawing: Integer; safecall;
    procedure Set_GISDrawing(Value: Integer); safecall;
    function Get_ReadOnly: Integer; safecall;
    procedure Set_ReadOnly(Value: Integer); safecall;
    function Populate(ANetworkID: Integer; ADrawingID: Integer; const ADrawingName: WideString;
                      AGISDrawing: Integer; AReadOnly: Integer): WordBool; safecall;
    property NetworkID: Integer read Get_NetworkID;
    property DrawingID: Integer read Get_DrawingID;
    property DrawingName: WideString read Get_DrawingName write Set_DrawingName;
    property GISDrawing: Integer read Get_GISDrawing write Set_GISDrawing;
  end;

  THydroNVDrawingAgent = class(TModuleAgent, IHydroNVDrawingAgent)
  protected
//    FList : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
//    function Initialise : Boolean; override;
    function Get_HydroNVDrawingCount: Integer; safecall;
    function Get_HydroNVDrawingByID(ADrawingID: Integer): IHydroNVDrawing; safecall;
    function Get_HydroNVDrawingByIndex(AIndex: Integer): IHydroNVDrawing; safecall;
    function Get_HydroNVDrawingByName(const AName: WideString): IHydroNVDrawing; safecall;
    function AddHydroNVDrawing: THydroNVDrawing;
    function LoadHydroNVDrawings (ANetworkID: Integer): Boolean;
    function Get_DrawingExists(const ADrawingName: WideString): WordBool; safecall;
    function CreateNewDrawing(ANetworkID: Integer; const ADrawingFileName: WideString;
                              AGISDrawing: Integer; const ACopyFrom: WideString): IHydroNVDrawing; safecall;
    function DeleteHydroNVDrawing(ADrawingID: Integer; const ADrawingFileName: WideString): WordBool; safecall;
    property HydroNVDrawingCount: Integer read Get_HydroNVDrawingCount;
    property HydroNVDrawingByID[ADrawingID: Integer]: IHydroNVDrawing read Get_HydroNVDrawingByID;
    property HydroNVDrawingByIndex[AIndex: Integer]: IHydroNVDrawing read Get_HydroNVDrawingByIndex;
    property HydroNVDrawingByName[const AName: WideString]: IHydroNVDrawing read Get_HydroNVDrawingByName;
    property DrawingExists[const ADrawingName: WideString]: WordBool read Get_DrawingExists;
  end;


implementation

uses

  Windows,
  SysUtils,
  VCL.Forms,
  Math,

  UUtilities,
  UModuleDBManager,
  UHydroNVDrawingDBManager,
  UErrorHandlingOperations;

{ THydroNVDrawing *************************************************************}

function THydroNVDrawing._AddRef: Integer;
const OPNAME = 'THydroNVDrawing._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawing._Release: Integer;
const OPNAME = 'THydroNVDrawing._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawing.Get_NetworkID: Integer;
const OPNAME = 'THydroNVDrawing.Get_NetworkID';
begin
  Result := 0;
  try
    Result := FNetworkID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawing.Get_DrawingID: Integer;
const OPNAME = 'THydroNVDrawing.Get_DrawingID';
begin
  Result := 0;
  try
    Result := FDrawingID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawing.Get_DrawingName: WideString;
const OPNAME = 'THydroNVDrawing.Get_DrawingName';
begin
  Result := '';
  try
    Result := FDrawingName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVDrawing.Set_DrawingName(const Value: WideString);
const OPNAME = 'THydroNVDrawing.Set_DrawingName';
begin
  try
    if (GHydroNVDrawingDBManager.UpdateDrawingNameInDB(FDrawingID, Value)) then
      FDrawingName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawing.Get_GISDrawing: Integer;
const OPNAME = 'THydroNVDrawing.Get_GISDrawing';
begin
  Result := 0;
  try
    Result := FGISDrawing;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVDrawing.Set_GISDrawing(Value: Integer);
const OPNAME = 'THydroNVDrawing.Set_GISDrawing';
begin
  try
    if (GHydroNVDrawingDBManager.UpdateGISDrawingInDB(FDrawingID, Value)) then
      FGISDrawing := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawing.Get_ReadOnly: Integer;
const OPNAME = 'THydroNVDrawing.Get_ReadOnly';
begin
  Result := 0;
  try
    Result := FReadOnly;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVDrawing.Set_ReadOnly(Value: Integer);
const OPNAME = 'THydroNVDrawing.Set_ReadOnly';
begin
  try
    if (GHydroNVDrawingDBManager.UpdateReadOnlyInDB(FDrawingID, Value)) then
      FReadOnly := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawing.Populate (ANetworkID         : Integer;
                                   ADrawingID         : Integer;
                                   const ADrawingName : WideString;
                                   AGISDrawing        : Integer;
                                   AReadOnly          : Integer): WordBool;
const OPNAME = 'THydroNVDrawing.Populate';
begin
  try
    FNetworkID   := ANetworkID;
    FDrawingID   := ADrawingID;
    FDrawingName := ADrawingName;
    FGISDrawing  := AGISDrawing;
    FReadOnly    := AReadOnly;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ THydroNVDrawingAgent ********************************************************}

function THydroNVDrawingAgent._AddRef: Integer;
const OPNAME = 'THydroNVDrawingAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent._Release: Integer;
const OPNAME = 'THydroNVDrawingAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVDrawingAgent.CreateMemberObjects;
const OPNAME = 'THydroNVDrawingAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GHydroNVDrawingDBManager := THydroNVDrawingDBManager.Create;
    GHydroNVDrawingDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVDrawingAgent.DestroyMemberObjects;
const OPNAME = 'THydroNVDrawingAgent.DestroyMemberObjects';
begin
  try
    GHydroNVDrawingDBManager.ModuleAgent := nil;
    FreeAndNil(GHydroNVDrawingDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent.AddHydroNVDrawing : THydroNVDrawing;
const OPNAME = 'THydroNVDrawingAgent.AddHydroNVDrawing';
var
  LHydroNVDrawing : THydroNVDrawing;
begin
  Result := nil;
  try
    LHydroNVDrawing := THydroNVDrawing.Create;
    FList.Add(LHydroNVDrawing);
    Result := LHydroNVDrawing;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{$WARN SYMBOL_PLATFORM OFF}
function THydroNVDrawingAgent.CreateNewDrawing (ANetworkID             : Integer;
                                                const ADrawingFileName : WideString;
                                                AGISDrawing            : Integer;
                                                const ACopyFrom        : WideString): IHydroNVDrawing;
const OPNAME = 'THydroNVDrawingAgent.CreateNewDrawing';
var
  LDefaultFile    : String;
  LFileName       : String;
  LPath           : String;
  LAttributes     : word;
  LNVDiagramsPath : String;
  LShortName      : String;
  LPos            : Integer;
begin
  Result := nil;
  try
    LNVDiagramsPath := NetworkDiagramsPath;
    if (ACopyFrom <> '') then
      LDefaultFile := ACopyFrom
    else if (AGISDrawing = 1) then
      LDefaultFile := LNVDiagramsPath + 'DefaultDrawingHydroGIS.VSD'
    else
      LDefaultFile := LNVDiagramsPath + 'DefaultDrawingHydro.VSD';
    if (FileExists(lDefaultFile)) then
    begin
      LFileName  := ADrawingFileName;
      LPath      := ExtractFilePath(LFileName);
      LShortName := ExtractFileName(ADrawingFileName);
      LPos       := Pos('.', LShortName);
      if (LPos > 0) then
        LShortName := Copy(LShortName, 1, LPos-1);
      if (NOT DirectoryExists(LPath)) then
        ForceDirectories(LPath);
      if (CopyFile(PChar(LDefaultFile), PChar(LFileName), TRUE)) then
      begin
        LAttributes := FileGetAttr(LDefaultFile);
        LAttributes := LAttributes AND (NOT SysUtils.faReadOnly);
        FileSetAttr(LFileName, LAttributes);
        Result := GHydroNVDrawingDBManager.CreateNewDrawingInDB(ANetworkID, AGISDrawing, LShortName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{$WARN SYMBOL_PLATFORM ON}

function THydroNVDrawingAgent.DeleteHydroNVDrawing (ADrawingID             : Integer;
                                                    const ADrawingFileName : WideString): WordBool;
const OPNAME = 'THydroNVDrawingAgent.DeleteHydroNVDrawing';
var
  LDrawing : THydroNVDrawing;
  LIndex   : Integer;
begin
  Result := FALSE;
  try
    if (GHydroNVDrawingDBManager.DeleteHydroNVDrawingIdDB(ADrawingID)) then
    begin
      LIndex := 0;
      while ((NOT Result) AND (LIndex < FList.Count)) do
      begin
        LDrawing := THydroNVDrawing(FList.Items[LIndex]);
        if (LDrawing.DrawingID = ADrawingID) then
        begin
          FList.Delete(LIndex);
          Result := TRUE;
        end  
        else
          LIndex := LIndex + 1;
      end;
      if (Result) AND (FileExists(ADrawingFileName)) then
        SysUtils.DeleteFile(ADrawingFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent.Get_DrawingExists(const ADrawingName: WideString): WordBool;
const OPNAME = 'THydroNVDrawingAgent.Get_DrawingExists';
var
  LHydroNVDrawing : THydroNVDrawing;
  LIndex          : Integer;
begin
  Result := FALSE;
  try
    LIndex := 0;
    while ((NOT Result) AND (LIndex < FList.Count)) do
    begin
      LHydroNVDrawing := THydroNVDrawing(FList.Items[LIndex]);
      if (LHydroNVDrawing.DrawingName = ADrawingName) then
        Result := TRUE
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent.Get_HydroNVDrawingCount: Integer;
const OPNAME = 'THydroNVDrawingAgent.Get_HydroNVDrawingCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent.Get_HydroNVDrawingByID (ADrawingID: Integer): IHydroNVDrawing;
const OPNAME = 'THydroNVDrawingAgent.Get_HydroNVDrawingByID';
var
  LHydroNVDrawing : THydroNVDrawing;
  LIndex          : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LHydroNVDrawing := THydroNVDrawing(FList.Items[LIndex]);
      if (LHydroNVDrawing.DrawingID = ADrawingID) then
        Result := LHydroNVDrawing
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent.Get_HydroNVDrawingByName(const AName: WideString): IHydroNVDrawing;
const OPNAME = 'THydroNVDrawingAgent.Get_HydroNVDrawingByName';
var
  LHydroNVDrawing : THydroNVDrawing;
  LIndex          : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LHydroNVDrawing := THydroNVDrawing(FList.Items[LIndex]);
      if (LHydroNVDrawing.DrawingName = AName) then
        Result := LHydroNVDrawing
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent.Get_HydroNVDrawingByIndex(AIndex: Integer): IHydroNVDrawing;
const OPNAME = 'THydroNVDrawingAgent.Get_HydroNVDrawingByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := THydroNVDrawing(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingAgent.LoadHydroNVDrawings (ANetworkID : Integer) : Boolean;
const OPNAME = 'THydroNVDrawingAgent.LoadHydroNVDrawings';
begin
  Result := FALSE;
  try
    Result := GHydroNVDrawingDBManager.LoadHydroNVDrawingsFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
