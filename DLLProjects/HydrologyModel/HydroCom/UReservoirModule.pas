(******************************************************************************)
(*  Contains : Class TReservoirModule.
(******************************************************************************)
unit UReservoirModule;


interface

uses
  Classes,
  Contnrs,
  XMLIntf,

  UModule,
  UFlowRoute,
  UAbstractObject,
  HydrologyCom_TLB;

type
  TYearVolumeAreaData = class(TAbstractObject, IYearVolumeAreaData)
  protected
    FYear   : Integer;
    FVolume : Double;
    FArea   : Double;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_Year: Integer; safecall;
    procedure Set_Year(Value: Integer); safecall;
    function Get_Volume: Double; safecall;
    procedure Set_Volume(Value: Double); safecall;
    function Get_Area: Double; safecall;
    procedure Set_Area(Value: Double); safecall;
    property Year: Integer read Get_Year write Set_Year;
    property Volume: Double read Get_Volume write Set_Volume;
    property Area: Double read Get_Area write Set_Area;
  end;

  TReservoirModule = class(TNetworkModule, IReservoirModule)
  protected
    FReservoirName       : String;
    FMAP                 : Double;
    FRainfallFileName    : String;
    FAreaPower           : Double;
    FSpillageRouteNo     : Integer;
    FInitialStorageState : Double;
    FInflowRoutes        : TObjectList;
    FOutFlowRoutes       : TObjectList;
    FVolumeAreaData      : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ReservoirName: WideString; safecall;
    procedure Set_ReservoirName(const Value: WideString); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_AreaPower: Double; safecall;
    procedure Set_AreaPower(Value: Double); safecall;
    function Get_SpillageRouteNo: Integer; safecall;
    procedure Set_SpillageRouteNo(Value: Integer); safecall;
    function Get_InitialStorageState: Double; safecall;
    procedure Set_InitialStorageState(Value: Double); safecall;
    function Populate (ANetworkID              : Integer;
                       AModuleID               : Integer;
                       const AModuleType       : WideString;
                       AModuleNumber           : Integer;
                       ANetworkSequence        : Integer;
                       const AActive           : WideString;
                       const AReservoirName    : WideString;
                       AMAP                    : Double;
                       const ARainfallFileName : WideString;
                       AAreaPower              : Double;
                       ASpillageRouteNo        : Integer;
                       AInitialStorageState    : Double;
                       ALongitude              : Double;
                       ALatitude               : Double): WordBool; safecall;
    function AddInflowRoute (ARouteNo: Integer; const AFileName: WideString): IInflowRoute; safecall;
    function CreateNewInflowRoute(ARouteNo : Integer; const AFileName: WideString) : Boolean;
    function RemoveInflowRoute(ARouteNo : Integer) : Boolean;
    function DeleteInflowRoute(ARouteNo : Integer) : Boolean;
    function AddOutflowRoute(ARouteNo : Integer; const AFileName: WideString;
                             const AAbstractions: WideString; AStorageState: Double;
                             AReductionFactor: Double): IOutflowRoute; safecall;
    function CreateNewOutflowRoute(ARouteNo : Integer; const AFileName: WideString) : Boolean;
    function RemoveOutflowRoute(ARouteNo : Integer) : Boolean;
    function DeleteOutflowRoute(ARouteNo : Integer) : Boolean;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    function UpdateVolumeAreaData (ARootNode : IXMLNode): Boolean;
    function UpdateInflowRoutesData (ARootNode : IXMLNode): Boolean;
    function UpdateOutflowRoutesData (ARootNode : IXMLNode): Boolean;
    function Get_InFlowRouteByRouteNo(ARouteNo: Integer): IInflowRoute; safecall;
    function FindInFlowRouteByRouteNo(ARouteNo: Integer): TInflowRoute;
    function Get_NoOfInFlowRoutes: Integer; safecall;
    function Get_InflowRouteByIndex(AIndex: Integer): IInflowRoute; safecall;
    function Get_OutFlowRouteByRouteNo(ARouteNo: Integer): IOutflowRoute; safecall;
    function FindOutFlowRouteByRouteNo(ARouteNo: Integer): TOutflowRoute;
    function Get_NoOfOutFlowRoutes: Integer; safecall;
    function Get_OutFlowRouteByIndex(AIndex: Integer): IOutflowRoute; safecall;
    function AddVolumeAreaData (AYear: Integer; AVolume: Double; AArea: Double): IYearVolumeAreaData; safecall;
    function Get_VolumeAreaDataCount: Integer; safecall;
    function Get_VolumeAreaDataByYear(AYear: Integer): IYearVolumeAreaData; safecall;
    function Get_VolumeAreaDataByIndex(AIndex: Integer): IYearVolumeAreaData; safecall;
    procedure ClearVolumeAreaData;
    property ReservoirName: WideString read Get_ReservoirName write Set_ReservoirName;
    property MAP: Double read Get_MAP write Set_MAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property AreaPower: Double read Get_AreaPower write Set_AreaPower;
    property SpillageRouteNo: Integer read Get_SpillageRouteNo write Set_SpillageRouteNo;
    property InitialStorageState: Double read Get_InitialStorageState write Set_InitialStorageState;
    property NoOfInFlowRoutes: Integer read Get_NoOfInFlowRoutes;
    property InFlowRouteByRouteNo[ARouteNo: Integer]: IInflowRoute read Get_InFlowRouteByRouteNo;
    property InflowRouteByIndex[AIndex: Integer]: IInflowRoute read Get_InflowRouteByIndex;
    property NoOfOutFlowRoutes: Integer read Get_NoOfOutFlowRoutes;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IOutflowRoute read Get_OutFlowRouteByRouteNo;
    property OutFlowRouteByIndex[AIndex: Integer]: IOutflowRoute read Get_OutFlowRouteByIndex;
    property VolumeAreaDataCount: Integer read Get_VolumeAreaDataCount;
    property VolumeAreaDataByYear[AYear: Integer]: IYearVolumeAreaData read Get_VolumeAreaDataByYear;
    property VolumeAreaDataByIndex[AIndex: Integer]: IYearVolumeAreaData read Get_VolumeAreaDataByIndex;
  end;

  TReservoirModuleAgent = class(TModuleAgent, IReservoirModuleAgent)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function FindReservoirModuleByID(AModuleID: Integer): TReservoirModule;
    function FindReservoirModuleByNumber(AModuleNumber: Integer): TReservoirModule;
    function LoadReservoirModules(ANetworkID: Integer) : Boolean;
    function AddReservoirModule : TReservoirModule;
    function Get_ReservoirModuleCount: Integer; safecall;
    function Get_ReservoirModuleByID(AModuleID: Integer): IReservoirModule; safecall;
    function Get_ReservoirModuleByNumber(AModuleNumber: Integer): IReservoirModule; safecall;
    function Get_ReservoirModuleByIndex(AIndex: Integer): IReservoirModule; safecall;
    function CreateNewReservoirModule (ANetworkID : Integer): IReservoirModule; safecall;
    function RemoveReservoirModule (AModuleNumber: Integer): WordBool; safecall;
    property ReservoirModuleCount: Integer read Get_ReservoirModuleCount;
    property ReservoirModuleByID[AID: Integer]: IReservoirModule read Get_ReservoirModuleByID;
    property ReservoirModuleByNumber[AModuleNumber: Integer]: IReservoirModule read Get_ReservoirModuleByNumber;
    property ReservoirModuleByIndex[AIndex: Integer]: IReservoirModule read Get_ReservoirModuleByIndex;
  end;


implementation

uses
  System.Types,
  SysUtils,
  Windows,
  VCL.Forms,
  Math,

  UModuleDBManager,
  UReservoirDBManager,
  UErrorHandlingOperations;

{ TYearVolumeAreaData *********************************************************}

function TYearVolumeAreaData._AddRef: Integer;
const OPNAME = 'TYearVolumeAreaData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYearVolumeAreaData._Release: Integer;
const OPNAME = 'TYearVolumeAreaData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYearVolumeAreaData.Get_Year: Integer;
const OPNAME = 'TYearVolumeAreaData.Get_Year';
begin
  Result := 0;
  try
    Result := FYear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYearVolumeAreaData.Set_Year(Value: Integer);
const OPNAME = 'TYearVolumeAreaData.Set_Year';
begin
  try
    FYear := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYearVolumeAreaData.Get_Volume: Double;
const OPNAME = 'TYearVolumeAreaData.Get_Volume';
begin
  Result := 0.0;
  try
    Result := FVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYearVolumeAreaData.Set_Volume(Value: Double);
const OPNAME = 'TYearVolumeAreaData.Set_Volume';
begin
  try
    FVolume := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYearVolumeAreaData.Get_Area: Double;
const OPNAME = 'TYearVolumeAreaData.Get_Area';
begin
  Result := 0.0;
  try
    Result := FArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYearVolumeAreaData.Set_Area(Value: Double);
const OPNAME = 'TYearVolumeAreaData.Set_Area';
begin
  try
    FArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TReservoirModule ************************************************************}

function TReservoirModule._AddRef: Integer;
const OPNAME = 'TReservoirModule._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule._Release: Integer;
const OPNAME = 'TReservoirModule._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirModule.CreateMemberObjects;
const OPNAME = 'TReservoirModule.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FInflowRoutes   := TObjectList.Create(FALSE);
    FOutFlowRoutes  := TObjectList.Create(FALSE);
    FVolumeAreaData := TObjectList.Create(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirModule.DestroyMemberObjects;
const OPNAME = 'TReservoirModule.DestroyMemberObjects';
begin
  try
    while (FInflowRoutes.Count > 0) do
      FInflowRoutes.Delete(0);
    FInflowRoutes.Free;
    while (FOutFlowRoutes.Count > 0) do
      FOutFlowRoutes.Delete(0);
    FOutFlowRoutes.Free;
    ClearVolumeAreaData;
    FVolumeAreaData.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_ReservoirName: WideString;
const OPNAME = 'TReservoirModule.Get_ReservoirName';
begin
  Result := '';
  try
    Result := FReservoirName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirModule.Set_ReservoirName(const Value: WideString);
const OPNAME = 'TReservoirModule.Set_ReservoirName';
begin
  try
    FReservoirName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.Get_MAP: Double;
const OPNAME = 'TReservoirModule.Get_MAP';
begin
  Result := 0.0;
  try
    Result := FMAP;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirModule.Set_MAP(Value: Double);
const OPNAME = 'TReservoirModule.Set_MAP';
begin
  try
    FMAP := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.Get_RainfallFileName: WideString;
const OPNAME = 'TReservoirModule.Get_RainfallFileName';
begin
  Result := '';
  try
    Result := FRainfallFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirModule.Set_RainfallFileName(const Value: WideString);
const OPNAME = 'TReservoirModule.Set_RainfallFileName';
begin
  try
    FRainfallFileName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.Get_AreaPower: Double;
const OPNAME = 'TReservoirModule.Get_AreaPower';
begin
  Result := 0.0;
  try
    Result := FAreaPower;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirModule.Set_AreaPower(Value: Double);
const OPNAME = 'TReservoirModule.Set_AreaPower';
begin
  try
    FAreaPower := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.Get_SpillageRouteNo: Integer;
const OPNAME = 'TReservoirModule.Get_SpillageRouteNo';
begin
  Result := 0;
  try
    Result := FSpillageRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirModule.Set_SpillageRouteNo(Value: Integer);
const OPNAME = 'TReservoirModule.Set_SpillageRouteNo';
begin
  try
    FSpillageRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.Get_InitialStorageState: Double;
const OPNAME = 'TReservoirModule.Get_InitialStorageState';
begin
  Result := 0.0;
  try
    Result := FInitialStorageState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirModule.Set_InitialStorageState(Value: Double);
const OPNAME = 'TReservoirModule.Set_InitialStorageState';
begin
  try
    FInitialStorageState := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.Populate (ANetworkID              : Integer;
                                    AModuleID               : Integer;
                                    const AModuleType       : WideString;
                                    AModuleNumber           : Integer;
                                    ANetworkSequence        : Integer;
                                    const AActive           : WideString;
                                    const AReservoirName    : WideString;
                                    AMAP                    : Double;
                                    const ARainfallFileName : WideString;
                                    AAreaPower              : Double;
                                    ASpillageRouteNo        : Integer;
                                    AInitialStorageState    : Double;
                                    ALongitude              : Double;
                                    ALatitude               : Double): WordBool;
const OPNAME = 'TReservoirModule.Populate';
begin
  Result := FALSE;
  try
    FNetworkID           := ANetworkID;
    FModuleID            := AModuleID;
    FModuleType          := AModuleType;
    FModuleNumber        := AModuleNumber;
    FNetworkSequence     := ANetworkSequence;
    FActive              := AActive;
    FReservoirName       := AReservoirName;
    FMAP                 := AMAP;
    FRainfallFileName    := ARainfallFileName;
    FAreaPower           := AAreaPower;
    FSpillageRouteNo     := ASpillageRouteNo;
    FInitialStorageState := AInitialStorageState;
    FLongitude           := ALongitude;
    FLatitude            := ALatitude;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Creates an instance of a TInflowRoute that already exists in the database and adds it to the FInFlowRoutes list
function TReservoirModule.AddInflowRoute (ARouteNo        : Integer;
                                          const AFileName : WideString): IInflowRoute;
const OPNAME = 'TReservoirModule.AddInflowRoute';
var
  LInflowRoute : TInFlowRoute;
begin
  Result := nil;
  try
    LInflowRoute := TInFlowRoute.Create;
    LInflowRoute.RouteNo := ARouteNo;
    LInflowRoute.FileName := AFileName;
    FInflowRoutes.Add(LInflowRoute);
    Result := LInflowRoute;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Calls AddOutflowRoute to create the instance and add it to the FOutFlowRoutes list.
function TReservoirModule.CreateNewOutflowRoute(ARouteNo : Integer; const AFileName: WideString) : Boolean;
const OPNAME = 'TReservoirModule.CreateNewOutflowRoute';
begin
  Result := FALSE;
  try
    Result := GReservoirDBManager.InsertOutflowRouteIntoDB(FModuleID, ARouteNo, AFileName);
    if (Result) then
      AddOutflowRoute(ARouteNo, AFileName, '', 0, 0);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Removes the instance of a TOutflowRoute from the FOutFlowRoutes list and destroys the instance.
function TReservoirModule.RemoveOutflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TReservoirModule.RemoveOutflowRoute';
var
  LOutflowRoute : TOutFlowRoute;
begin
  Result := FALSE;
  try
    LOutflowRoute := FindOutFlowRouteByRouteNo(ARouteNo);
    if (LOutflowRoute <> nil) then
    begin
      FOutFlowRoutes.Remove(LOutflowRoute);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Deletes the entry for the outflow route in the database.
// Calls RemoveOutflowRoute to remove the instance from the FOutFlowRoutes list.
function TReservoirModule.DeleteOutflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TReservoirModule.DeleteOutflowRoute';
begin
  Result := FALSE;
  try
    Result := GReservoirDBManager.DeleteOutflowRouteFromDB(FModuleID, ARouteNo);
    if (Result) then
      Result := RemoveOutflowRoute(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Creates an instance of a TOutflowRoute that already exists in the database and adds it to the FOutFlowRoutes list
function TReservoirModule.AddOutflowRoute (ARouteNo            : Integer;
                                           const AFileName     : WideString;
                                           const AAbstractions : WideString;
                                           AStorageState       : Double;
                                           AReductionFactor    : Double): IOutflowRoute;
const OPNAME = 'TReservoirModule.AddOutflowRoute';
var
  LOutFlowRoute : TOutFlowRoute;
  LCount        : Integer;
  LTempList     : TStringList;
begin
  Result := nil;
  try
    LOutFlowRoute := TOutFlowRoute.Create;
    LOutFlowRoute.RouteNo         := ARouteNo;
    LOutFlowRoute.FileName        := AFileName;
    LOutFlowRoute.StorageState    := AStorageState;
    LOutFlowRoute.ReductionFactor := AReductionFactor;
    LTempList := TStringList.Create;
    try
      LTempList.CommaText := AAbstractions;
      for LCount := 0 to LTempList.Count - 1 do
        LOutFlowRoute.AbstractionVolumeByMonth[LCount+1] := StrToInt(LTempList.Strings[LCount]);
    finally
      LTempList.Free;
    end;
    FOutFlowRoutes.Add(LOutFlowRoute);
    Result := LOutFlowRoute;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Creates an entry for a new Inflow route in the database.
// Calls AddInflowRoute to create the instance and add it to the FInflowRoutes list.
function TReservoirModule.CreateNewInflowRoute(ARouteNo : Integer; const AFileName: WideString) : Boolean;
const OPNAME = 'TReservoirModule.CreateNewInflowRoute';
begin
  Result := FALSE;
  try
    Result := GReservoirDBManager.InsertInflowRouteIntoDB(FModuleID, ARouteNo, AFileName);
    if (Result) then
      AddInflowRoute(ARouteNo, AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Removes the instance of a TInflowRoute from the FInflowRoutes list and destroys the instance.
function TReservoirModule.RemoveInflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TReservoirModule.CreateNewInflowRoute';
var
  LInflowRoute : TInflowRoute;
begin
  Result := FALSE;
  try
    LInflowRoute := FindInflowRouteByRouteNo(ARouteNo);
    if (LInflowRoute <> nil) then
    begin
      FInflowRoutes.Remove(LInflowRoute);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Deletes the entry for the Inflow route in the database.
// Calls RemoveInflowRoute to remove the instance from the FInflowRoutes list.
function TReservoirModule.DeleteInflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TReservoirModule.DeleteInflowRoute';
begin
  Result := FALSE;
  try
    Result := GReservoirDBManager.DeleteInflowRouteFromDB(FModuleID, ARouteNo);
    if (Result) then
      Result := RemoveInflowRoute(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_NoOfInflowRoutes: Integer;
const OPNAME = 'TReservoirModule.Get_NoOfInflowRoutes';
begin
  Result := 0;
  try
    Result := FInflowRoutes.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_InFlowRouteByRouteNo(ARouteNo: Integer): IInflowRoute;
const OPNAME = 'TReservoirModule.Get_InFlowRouteByRouteNo';
begin
  Result := nil;
  try
    Result := FindInFlowRouteByRouteNo(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirModule.FindInFlowRouteByRouteNo(ARouteNo: Integer): TInflowRoute;
const OPNAME = 'TReservoirModule.FindInFlowRouteByRouteNo';
var
  LIndex       : Integer;
  LInflowRoute : TInflowRoute;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FInflowRoutes.Count)) do
    begin
      LInflowRoute := TInFlowRoute(FInflowRoutes.Items[LIndex]);
      if (LInflowRoute.RouteNo = ARouteNo) then
        Result := LInflowRoute
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_InFlowRouteByIndex(AIndex: Integer): IInflowRoute;
const OPNAME = 'TReservoirModule.Get_InFlowRouteByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FInflowRoutes.Count)) then
      Result := TInFlowRoute(FInflowRoutes.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_NoOfOutFlowRoutes: Integer;
const OPNAME = 'TReservoirModule.Get_NoOfOutFlowRoutes';
begin
  Result := 0;
  try
    Result := FOutFlowRoutes.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_OutFlowRouteByRouteNo(ARouteNo: Integer): IOutflowRoute;
const OPNAME = 'TReservoirModule.Get_OutFlowRouteByRouteNo';
begin
  Result := nil;
  try
    Result := FindOutFlowRouteByRouteNo(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.FindOutFlowRouteByRouteNo(ARouteNo: Integer): TOutflowRoute;
const OPNAME = 'TReservoirModule.FindOutFlowRouteByRouteNo';
var
  LIndex        : Integer;
  LOutFlowRoute : TOutFlowRoute;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FOutFlowRoutes.Count)) do
    begin
      LOutFlowRoute := TOutFlowRoute(FOutFlowRoutes.Items[LIndex]);
      if (LOutFlowRoute.RouteNo = ARouteNo) then
        Result := LOutFlowRoute
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_OutFlowRouteByIndex(AIndex: Integer): IOutflowRoute;
const OPNAME = 'TReservoirModule.Get_OutFlowRouteByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FOutFlowRoutes.Count)) then
      Result := TOutFlowRoute(FOutFlowRoutes.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.AddVolumeAreaData (AYear: Integer; AVolume: Double; AArea: Double): IYearVolumeAreaData;
const OPNAME = 'TReservoirModule.AddVolumeAreaData';
var
  LData : TYearVolumeAreaData;
begin
  Result := nil;
  try
    LData := TYearVolumeAreaData.Create;
    FVolumeAreaData.Add(LData);
    LData.FYear   := AYear;
    LData.FVolume := AVolume;
    LData.FArea   := AArea;
    Result := LData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_VolumeAreaDataCount: Integer;
const OPNAME = 'TReservoirModule.Get_VolumeAreaDataCount';
begin
  Result := 0;
  try
    Result := FVolumeAreaData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_VolumeAreaDataByYear(AYear: Integer): IYearVolumeAreaData;
const OPNAME = 'TReservoirModule.Get_VolumeAreaDataByYear';
var
  LData  : TYearVolumeAreaData;
  LIndex : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FVolumeAreaData.Count)) do
    begin
      LData := TYearVolumeAreaData(FVolumeAreaData.Items[LIndex]);
      if (LData.Year = AYear) then
        Result := LData
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModule.Get_VolumeAreaDataByIndex(AIndex: Integer): IYearVolumeAreaData;
const OPNAME = 'TReservoirModule.Get_VolumeAreaDataByIndex';
begin
  Result := nil;
  try
    if (AIndex < FVolumeAreaData.Count) then
      Result := TYearVolumeAreaData(FVolumeAreaData.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirModule.ClearVolumeAreaData;
const OPNAME = 'TReservoirModule.ClearVolumeAreaData ';
begin
  try
    while (FVolumeAreaData.Count > 0) do
      FVolumeAreaData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TReservoirModule.UpdatePropertiesData';
var
  LActive              : String;
  LLatitude            : Double;
  LLongitude           : Double;
  LReservoirName       : String;
  LMAP                 : Double;
  LRainfallFileName    : String;
  LAreaPower           : Double;
  LSpillageRouteNo     : Integer;
  LInitialStorageState : Double;
  LDataNode            : IXMLNode;
begin
  Result := FALSE;
  try
    LDataNode            := ARootNode.ChildNodes['ReservoirProperties'];
    LActive              := LDataNode.ChildNodes['Active'].Text;
    LLatitude            := StrToFloat(LDataNode.ChildNodes['Latitude'].Text);
    LLongitude           := StrToFloat(LDataNode.ChildNodes['Longitude'].Text);
    LReservoirName       := LDataNode.ChildNodes['ReservoirName'].Text;
    LMAP                 := StrToFloat(LDataNode.ChildNodes['MAP'].Text);
    LRainfallFileName    := LDataNode.ChildNodes['RainfallFileName'].Text;
    LAreaPower           := StrToFloat(LDataNode.ChildNodes['AreaPower'].Text);
    LSpillageRouteNo     := StrToInt(LDataNode.ChildNodes['SpillageRouteNo'].Text);
    LInitialStorageState := StrToFloat(LDataNode.ChildNodes['InitialStorageState'].Text);

    if (GReservoirDBManager.UpdatePropertiesDataInDB
                              (FModuleID, LActive, LLatitude, LLongitude, LReservoirName,
                               LMAP, LRainfallFileName, LAreaPower, LSpillageRouteNo, LInitialStorageState)) then
    begin
      FLatitude            := LLatitude;
      FLongitude           := LLongitude;
      FActive              := LActive;
      FReservoirName       := LReservoirName;
      FMAP                 := LMAP;
      FRainfallFileName    := LRainfallFileName;
      FAreaPower           := LAreaPower;
      FSpillageRouteNo     := LSpillageRouteNo;
      FInitialStorageState := LInitialStorageState;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.UpdateVolumeAreaData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TReservoirModule.UpdateVolumeAreaData';
var
  LListNode  : IXMLNode;
  LDataNode  : IXMLNode;
  LIndex     : Integer;
  LYearList, LVolList, LAreaList : TStringList;
begin
  Result := FALSE;
  try
    LListNode := ARootNode.ChildNodes['ReservoirVolumeArea'];
    LListNode := LListNode.ChildNodes['DataList'];

    LYearList := TStringList.Create;
    LVolList  := TStringList.Create;
    LAreaList := TStringList.Create;
    try
      LIndex := 0;
      while (Result AND (LIndex < LListNode.ChildNodes.Count)) do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LVolList.Add(LDataNode.ChildNodes['FullSupplyVolume'].Text);
        LAreaList.Add(LDataNode.ChildNodes['SurfaceArea'].Text);
        LIndex    := LIndex + 1
      end;
      Result := GReservoirDBManager.UpdateVolumeAreaDataInDB(FModuleID, LYearList, LVolList, LAreaList);
      if (Result) then
        Result := GReservoirDBManager.LoadVolumeAreaDataFromDB(Self);
    finally
      LYearList.Free;
      LVolList.Free;
      LAreaList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.UpdateInflowRoutesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TReservoirModule.UpdateInflowRoutesData';
var
  LListNode       : IXMLNode;
  LDataNode       : IXMLNode;
  LIndex          : Integer;
  LInflowRouteNo  : String;
  LInflowFileName : String;
begin
  Result := TRUE;
  try
    LListNode := ARootNode.ChildNodes['InflowRoutes'];
    LListNode := LListNode.ChildNodes['DataList'];
    for LIndex := 1 to LListNode.ChildNodes.Count do
    begin
      LDataNode       := LListNode.ChildNodes.Get(LIndex-1);
      LInflowRouteNo  := LDataNode.ChildNodes['InflowRouteNo'].Text;
      LInflowFileName := LDataNode.ChildNodes['InflowFileName'].Text;
      if (InFlowRouteByRouteNo[StrToInt(LInflowRouteNo)].FileName <> LInflowFileName) then
      begin
        if (GReservoirDBManager.UpdateInflowRoutesDataInDB(FModuleID, StrToInt(LInflowRouteNo), LInflowFileName)) then
          InFlowRouteByRouteNo[StrToInt(LInflowRouteNo)].FileName := LInflowFileName
        else
          Result := FALSE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirModule.UpdateOutflowRoutesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TReservoirModule.UpdateOutflowRoutesData';
var
  LListNode        : IXMLNode;
  LDataNode        : IXMLNode;
  LIndex           : Integer;
  LOutflowRouteNo  : String;
  LOutflowFileName : String;
begin
  Result := TRUE;
  try
    LListNode := ARootNode.ChildNodes['OutflowRoutes'];
    LListNode := LListNode.ChildNodes['DataList'];
    for LIndex := 1 to LListNode.ChildNodes.Count do
    begin
      LDataNode        := LListNode.ChildNodes.Get(LIndex-1);
      LOutflowRouteNo  := LDataNode.ChildNodes['OutflowRouteNo'].Text;
      LOutflowFileName := LDataNode.ChildNodes['OutflowFileName'].Text;
      if (OutflowRouteByRouteNo[StrToInt(LOutflowRouteNo)].FileName <> LOutflowFileName) then
      begin
        if (GReservoirDBManager.UpdateOutflowRoutesDataInDB(FModuleID, StrToInt(LOutflowRouteNo), LOutflowFileName)) then
          OutflowRouteByRouteNo[StrToInt(LOutflowRouteNo)].FileName := LOutflowFileName
        else
          Result := FALSE;  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirModuleAgent *******************************************************}

function TReservoirModuleAgent._AddRef: Integer;
const OPNAME = 'TReservoirModuleAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent._Release: Integer;
const OPNAME = 'TReservoirModuleAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirModuleAgent.CreateMemberObjects;
const OPNAME = 'TReservoirModuleAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GReservoirDBManager := TReservoirDBManager.Create;
    GReservoirDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirModuleAgent.DestroyMemberObjects;
const OPNAME = 'TReservoirModuleAgent.DestroyMemberObjects';
begin
  try
    GReservoirDBManager.ModuleAgent := nil;
    FreeAndNil(GReservoirDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.CreateNewReservoirModule (ANetworkID : Integer): IReservoirModule;
const OPNAME = 'TReservoirModuleAgent.CreateNewReservoirModule';
begin
  Result := nil;
  try
    Result := GReservoirDBManager.CreateNewReservoirModuleInDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.RemoveReservoirModule (AModuleNumber : Integer): WordBool;
const OPNAME = 'TReservoirModuleAgent.RemoveReservoirModule';
var
  LReservoirModule : TReservoirModule;
  LIndex           : Integer;
begin
  Result := FALSE;
  try
    LReservoirModule := FindReservoirModuleByNumber(AModuleNumber);
    if (LReservoirModule <> nil) then
    begin
      if (GReservoirDBManager.RemoveReservoirModuleFromDB(LReservoirModule.ModuleID)) then
      begin
        Result := TRUE;
        LIndex := FList.IndexOf(LReservoirModule);
        FList.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.AddReservoirModule : TReservoirModule;
const OPNAME = 'TReservoirModuleAgent.AddReservoirModule';
var
  LReservoirModule : TReservoirModule;
begin
  Result := nil;
  try
    LReservoirModule := TReservoirModule.Create;
    FList.Add(LReservoirModule);
    Result := LReservoirModule;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.Get_ReservoirModuleCount: Integer;
const OPNAME = 'TReservoirModuleAgent.Get_ReservoirModuleCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.FindReservoirModuleByID(AModuleID: Integer): TReservoirModule;
const OPNAME = 'TReservoirModuleAgent.FindReservoirModuleByID';
var
  LReservoirModule : TReservoirModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LReservoirModule := TReservoirModule(FList.Items[LIndex]);
      if (LReservoirModule.ModuleID = AModuleID) then
        Result := LReservoirModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.Get_ReservoirModuleByID(AModuleID: Integer): IReservoirModule;
const OPNAME = 'TReservoirModuleAgent.Get_ReservoirModuleByID';
var
  LReservoirModule : TReservoirModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LReservoirModule := TReservoirModule(FList.Items[LIndex]);
      if (LReservoirModule.ModuleID = AModuleID) then
        Result := LReservoirModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.FindReservoirModuleByNumber(AModuleNumber: Integer): TReservoirModule;
const OPNAME = 'TReservoirModuleAgent.FindReservoirModuleByNumber';
var
  LReservoirModule : TReservoirModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LReservoirModule := TReservoirModule(FList.Items[LIndex]);
      if (LReservoirModule.ModuleNumber = AModuleNumber) then
        Result := LReservoirModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.Get_ReservoirModuleByNumber (AModuleNumber : Integer): IReservoirModule;
const OPNAME = 'TReservoirModuleAgent.Get_ReservoirModuleByID';
var
  LReservoirModule : TReservoirModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LReservoirModule := TReservoirModule(FList.Items[LIndex]);
      if (LReservoirModule.ModuleNumber = AModuleNumber) then
        Result := LReservoirModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.Get_ReservoirModuleByIndex(AIndex: Integer): IReservoirModule;
const OPNAME = 'TReservoirModuleAgent.Get_ReservoirModuleByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TReservoirModule(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirModuleAgent.LoadReservoirModules (ANetworkID : Integer) : Boolean;
const OPNAME = 'TReservoirModuleAgent.LoadReservoirModules';
begin
  Result := FALSE;
  try
    Result := GReservoirDBManager.LoadReservoirModulesFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
