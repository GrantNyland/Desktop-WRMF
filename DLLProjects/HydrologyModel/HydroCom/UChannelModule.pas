(******************************************************************************)
(*  Contains : Class TChannelModule.
(******************************************************************************)
unit UChannelModule;


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
  TChannelModule = class(TNetworkModule, IChannelModule)
  protected
    FChannelName                   : String;
    FVersionNo                     : Integer;
    FWetlandMAP                    : Double;
    FRainfallFileName              : String;
    FMonthlyBedLoss                : Double;
    FWetlandStorage                : Double;
    FWetlandArea                   : Double;
    FWetlandRechargeCoefficient    : Double;
    FPrincipalOutflowRouteNo       : Integer;
    FFutureUse                     : Integer;
    FWetlandType                   : Integer;
    FQDiv                          : Double;
    FWetlandsInflowRouteNo         : Integer;
    FWetlandsOutflowRouteNo        : Integer;
    FDiversionRouteNo              : Integer;
    FBankfillCapacity              : Double;
    FDiversionEfficiency           : Double;
    FMaxMonthlyDiversionCapacity   : Double;
    FBankfillArea                  : Double;
    FBankFillVolume                : Double;
    FPowerOfAreaCapCurve           : Double;
    FBankfillCapacityComprehensive : Double;
    FWetlandInflowProportion       : Double;
    FChannelInflowProportion       : Double;
    FInflowRoutes                  : TObjectList;
    FOutFlowRoutes                 : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function DeleteChannelFromDB : Boolean;
    function DeleteCompWetlandParamsFromDB : Boolean;
    function MakeOutflowRoutePrincipal (ARouteNo : Integer) : Boolean;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ChannelName: WideString; safecall;
    procedure Set_ChannelName(const Value: WideString); safecall;
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_WetlandMAP: Double; safecall;
    procedure Set_WetlandMAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_MonthlyBedLoss: Double; safecall;
    procedure Set_MonthlyBedLoss(Value: Double); safecall;
    function Get_WetlandStorage: Double; safecall;
    procedure Set_WetlandStorage(Value: Double); safecall;
    function Get_WetlandArea: Double; safecall;
    procedure Set_WetlandArea(Value: Double); safecall;
    function Get_WetlandRechargeCoefficient: Double; safecall;
    procedure Set_WetlandRechargeCoefficient(Value: Double); safecall;
    function Get_PrincipalOutflowRouteNo: Integer; safecall;
    procedure Set_PrincipalOutflowRouteNo(Value: Integer); safecall;
    function Get_FutureUse: Integer; safecall;
    procedure Set_FutureUse(Value: Integer); safecall;
    function Get_WetlandType: Integer; safecall;
    procedure Set_WetlandType(Value: Integer); safecall;
    function Get_QDiv: Double; safecall;
    procedure Set_QDiv(Value: Double); safecall;
    function Get_WetlandsInflowRouteNo: Integer; safecall;
    procedure Set_WetlandsInflowRouteNo(Value: Integer); safecall;
    function Get_WetlandsOutflowRouteNo: Integer; safecall;
    procedure Set_WetlandsOutflowRouteNo(Value: Integer); safecall;
    function Get_DiversionRouteNo: Integer; safecall;
    procedure Set_DiversionRouteNo(Value: Integer); safecall;
    function Get_BankfillCapacity: Double; safecall;
    procedure Set_BankfillCapacity(Value: Double); safecall;
    function Get_DiversionEfficiency: Double; safecall;
    procedure Set_DiversionEfficiency(Value: Double); safecall;
    function Get_MaxMonthlyDiversionCapacity: Double; safecall;
    procedure Set_MaxMonthlyDiversionCapacity(Value: Double); safecall;
    function Get_BankfillArea: Double; safecall;
    procedure Set_BankfillArea(Value: Double); safecall;
    function Get_BankfillVolume: Double; safecall;
    procedure Set_BankfillVolume(Value: Double); safecall;
    function Get_PowerOfAreaCapCurve: Double; safecall;
    procedure Set_PowerOfAreaCapCurve(Value: Double); safecall;
    function Get_BankfillCapacityComprehensive: Double; safecall;
    procedure Set_BankfillCapacityComprehensive(Value: Double); safecall;
    function Get_WetlandInflowProportion: Double; safecall;
    procedure Set_WetlandInflowProportion(Value: Double); safecall;
    function Get_ChannelInflowProportion: Double; safecall;
    procedure Set_ChannelInflowProportion(Value: Double); safecall;
    function Populate(ANetworkID                   : Integer;
                      AModuleID                    : Integer;
                      const AModuleType            : WideString;
                      AModuleNumber                : Integer;
                      ANetworkSequence             : Integer;
                      const AActive                : WideString;
                      const AChannelName           : WideString;
                      AVersionNo                   : Integer;
                      AWetlandMAP                  : Double;
                      const ARainfallFileName      : WideString;
                      AMonthlyBedLoss              : Double;
                      AWetlandStorage              : Double;
                      AWetlandArea                 : Double;
                      AWetlandRechargeCoefficient  : Double;
                      APrincipalOutflowRouteNo     : Integer;
                      AWetlandType                 : Integer;
                      AQDiv                        : Double;
                      AWetlandsInflowRouteNo       : Integer;
                      AWetlandsOutflowRouteNo      : Integer;
                      ADiversionRouteNo            : Integer;
                      ABankfillCapacity            : Double;
                      ADiversionEfficiency         : Double;
                      AMaxMonthlyDiversionCapacity : Double;
                      ALongitude                   : Double;
                      ALatitude                    : Double): WordBool; safecall;
    function PopulateComprehensiveWetlandParams (ABankfillArea            : Double;
                                                 ABankfillVolume          : Double;
                                                 APowerOfAreaCapCurve     : Double;
                                                 ABankfillCapacity        : Double;
                                                 AWetlandInflowProportion : Double;
                                                 AChannelInflowProportion : Double): WordBool; safecall;
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
    function UpdateInflowRoutesData (ARootNode : IXMLNode): Boolean;
    function UpdateOutflowRoutesData (ARootNode : IXMLNode): Boolean;
    function Get_NoOfInflowRoutes: Integer; safecall;
    function Get_InFlowRouteByRouteNo(ARouteNo: Integer): IInflowRoute; safecall;
    function FindInFlowRouteByRouteNo(ARouteNo: Integer): TInflowRoute;
    function Get_InFlowRouteByIndex(AIndex: Integer): IInflowRoute; safecall;
    function Get_NoOfOutFlowRoutes: Integer; safecall;
    function Get_OutFlowRouteByRouteNo(ARouteNo: Integer): IOutflowRoute; safecall;
    function FindOutFlowRouteByRouteNo(ARouteNo: Integer): TOutflowRoute;
    function Get_OutFlowRouteByIndex(AIndex: Integer): IOutflowRoute; safecall;

    property ChannelName: WideString read Get_ChannelName write Set_ChannelName;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property WetlandMAP: Double read Get_WetlandMAP write Set_WetlandMAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property MonthlyBedLoss: Double read Get_MonthlyBedLoss write Set_MonthlyBedLoss;
    property WetlandStorage: Double read Get_WetlandStorage write Set_WetlandStorage;
    property WetlandArea: Double read Get_WetlandArea write Set_WetlandArea;
    property WetlandRechargeCoefficient: Double read Get_WetlandRechargeCoefficient write Set_WetlandRechargeCoefficient;
    property PrincipalOutflowRouteNo: Integer read Get_PrincipalOutflowRouteNo write Set_PrincipalOutflowRouteNo;
    property FutureUse: Integer read Get_FutureUse write Set_FutureUse;
    property WetlandType: Integer read Get_WetlandType write Set_WetlandType;
    property QDiv: Double read Get_QDiv write Set_QDiv;
    property WetlandsInflowRouteNo: Integer read Get_WetlandsInflowRouteNo write Set_WetlandsInflowRouteNo;
    property WetlandsOutflowRouteNo: Integer read Get_WetlandsOutflowRouteNo write Set_WetlandsOutflowRouteNo;
    property DiversionRouteNo: Integer read Get_DiversionRouteNo write Set_DiversionRouteNo;
    property BankfillCapacity: Double read Get_BankfillCapacity write Set_BankfillCapacity;
    property DiversionEfficiency: Double read Get_DiversionEfficiency write Set_DiversionEfficiency;
    property MaxMonthlyDiversionCapacity: Double read Get_MaxMonthlyDiversionCapacity write Set_MaxMonthlyDiversionCapacity;
    property BankfillArea: Double read Get_BankfillArea write Set_BankfillArea;
    property BankfillVolume: Double read Get_BankfillVolume write Set_BankfillVolume;
    property PowerOfAreaCapCurve: Double read Get_PowerOfAreaCapCurve write Set_PowerOfAreaCapCurve;
    property BankfillCapacityComprehensive: Double read Get_BankfillCapacityComprehensive write Set_BankfillCapacityComprehensive;
    property WetlandInflowProportion: Double read Get_WetlandInflowProportion write Set_WetlandInflowProportion;
    property ChannelInflowProportion: Double read Get_ChannelInflowProportion write Set_ChannelInflowProportion;
    property NoOfInFlowRoutes: Integer read Get_NoOfInFlowRoutes;
    property InFlowRouteByRouteNo[ARouteNo: Integer]: IInflowRoute read Get_InFlowRouteByRouteNo;
    property InflowRouteByIndex[AIndex: Integer]: IInflowRoute read Get_InflowRouteByIndex;
    property NoOfOutFlowRoutes: Integer read Get_NoOfOutFlowRoutes;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IOutflowRoute read Get_OutFlowRouteByRouteNo;
    property OutFlowRouteByIndex[AIndex: Integer]: IOutflowRoute read Get_OutFlowRouteByIndex;
  end;

  TChannelModuleAgent = class(TModuleAgent, IChannelModuleAgent)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ChannelModuleCount: Integer; safecall;
    function FindChannelModuleByNumber(AModuleNumber: Integer): TChannelModule;
    function FindChannelModuleByID(AModuleID: Integer): TChannelModule;
    function Get_ChannelModuleByID(AModuleID: Integer): IChannelModule; safecall;
    function Get_ChannelModuleByNumber(AModuleNumber: Integer): IChannelModule; safecall;
    function Get_ChannelModuleByIndex(AIndex: Integer): IChannelModule; safecall;
    function LoadChannelModules (ANetworkID: Integer): Boolean;
    function AddChannelModule : TChannelModule;
    function CreateNewChannelModule (ANetworkID: Integer): IChannelModule; safecall;
    function RemoveChannelModule (AModuleNumber : Integer): WordBool; safecall;
    property ChannelModuleCount: Integer read Get_ChannelModuleCount;
    property ChannelModuleByID[AID: Integer]: IChannelModule read Get_ChannelModuleByID;
    property ChannelModuleByNumber[AModuleNumber: Integer]: IChannelModule read Get_ChannelModuleByNumber;
    property ChannelModuleByIndex[AIndex: Integer]: IChannelModule read Get_ChannelModuleByIndex;
  end;


implementation

uses

  System.Types,
  SysUtils,
  Windows,
  VCL.Forms,
  Math,

  UModuleDBManager,
  UChannelDBManager,
  UErrorHandlingOperations;

{ TChannelModule **************************************************************}

function TChannelModule._AddRef: Integer;
const OPNAME = 'TChannelModule._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule._Release: Integer;
const OPNAME = 'TChannelModule._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelModule.CreateMemberObjects;
const OPNAME = 'TChannelModule.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FInflowRoutes  := TObjectList.Create(FALSE);
    FOutFlowRoutes := TObjectList.Create(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelModule.DestroyMemberObjects;
const OPNAME = 'TChannelModule.DestroyMemberObjects';
begin
  try
    while (FInflowRoutes.Count > 0) do
      FInflowRoutes.Delete(0);
    FInflowRoutes.Free;
    while (FOutFlowRoutes.Count > 0) do
      FOutFlowRoutes.Delete(0);
    FOutFlowRoutes.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.Get_ChannelName: WideString;
const OPNAME = 'TChannelModule.Get_ChannelName';
begin
  Result := '';
  try
    Result := FChannelName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_ChannelName(const Value: WideString);
const OPNAME = 'TChannelModule.Set_ChannelName';
begin
  try
    FChannelName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_VersionNo: Integer;
const OPNAME = 'TChannelModule.Get_VersionNo';
begin
  Result := 0;
  try
    Result := FVersionNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_VersionNo(Value: Integer);
const OPNAME = 'TChannelModule.Set_VersionNo';
begin
  try
    FVersionNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandMAP: Double;
const OPNAME = 'TChannelModule.Get_WetlandMAP';
begin
  Result := 0.0;
  try
    Result := FWetlandMAP;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandMAP(Value: Double);
const OPNAME = 'TChannelModule.Set_WetlandMAP';
begin
  try
    FWetlandMAP := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_RainfallFileName: WideString;
const OPNAME = 'TChannelModule.Get_RainfallFileName';
begin
  Result := '';
  try
    Result := FRainfallFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_RainfallFileName(const Value: WideString);
const OPNAME = 'TChannelModule.Set_RainfallFileName';
begin
  try
    FRainfallFileName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_MonthlyBedLoss: Double;
const OPNAME = 'TChannelModule.Get_MonthlyBedLoss';
begin
  Result := 0.0;
  try
    Result := FMonthlyBedLoss;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_MonthlyBedLoss(Value: Double);
const OPNAME = 'TChannelModule.Set_MonthlyBedLoss';
begin
  try
    FMonthlyBedLoss := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandStorage: Double;
const OPNAME = 'TChannelModule.Get_WetlandStorage';
begin
  Result := 0.0;
  try
    Result := FWetlandStorage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandStorage(Value: Double);
const OPNAME = 'TChannelModule.Set_WetlandStorage';
begin
  try
    FWetlandStorage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandArea: Double;
const OPNAME = 'TChannelModule.Get_WetlandArea';
begin
  Result := 0.0;
  try
    Result := FWetlandArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandArea(Value: Double);
const OPNAME = 'TChannelModule.Set_WetlandArea';
begin
  try
    FWetlandArea := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandRechargeCoefficient: Double;
const OPNAME = 'TChannelModule.Get_WetlandRechargeCoefficient';
begin
  Result := 0.0;
  try
    Result := FWetlandRechargeCoefficient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandRechargeCoefficient(Value: Double);
const OPNAME = 'TChannelModule.Set_WetlandRechargeCoefficient';
begin
  try
    FWetlandRechargeCoefficient := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_PrincipalOutflowRouteNo: Integer;
const OPNAME = 'TChannelModule.Get_PrincipalOutflowRouteNo';
begin
  Result := 0;
  try
    Result := FPrincipalOutflowRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_PrincipalOutflowRouteNo(Value: Integer);
const OPNAME = 'TChannelModule.Set_PrincipalOutflowRouteNo';
begin
  try
    FPrincipalOutflowRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_FutureUse: Integer;
const OPNAME = 'TChannelModule.Get_FutureUse';
begin
  Result := 0;
  try
    Result := FFutureUse;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_FutureUse(Value: Integer);
const OPNAME = 'TChannelModule.Set_FutureUse';
begin
  try
    FFutureUse := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandType: Integer;
const OPNAME = 'TChannelModule.Get_WetlandType';
begin
  Result := 0;
  try
    Result := FWetlandType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandType(Value: Integer);
const OPNAME = 'TChannelModule.Set_WetlandType';
begin
  try
    FWetlandType := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_QDiv: Double;
const OPNAME = 'TChannelModule.Get_QDiv';
begin
  Result := 0.0;
  try
    Result := FQDiv;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_QDiv(Value: Double);
const OPNAME = 'TChannelModule.Set_QDiv';
begin
  try
    FQDiv := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandsInflowRouteNo: Integer;
const OPNAME = 'TChannelModule.Get_WetlandsInflowRouteNo';
begin
  Result := 0;
  try
    Result := FWetlandsInflowRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandsInflowRouteNo(Value: Integer);
const OPNAME = 'TChannelModule.Set_WetlandsInflowRouteNo';
begin
  try
    FWetlandsInflowRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandsOutflowRouteNo: Integer;
const OPNAME = 'TChannelModule.Get_WetlandsOutflowRouteNo';
begin
  Result := 0;
  try
    Result := FWetlandsOutflowRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandsOutflowRouteNo(Value: Integer);
const OPNAME = 'TChannelModule.Set_WetlandsOutflowRouteNo';
begin
  try
    FWetlandsOutflowRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_DiversionRouteNo: Integer;
const OPNAME = 'TChannelModule.Get_DiversionRouteNo';
begin
  Result := 0;
  try
    Result := FDiversionRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_DiversionRouteNo(Value: Integer);
const OPNAME = 'TChannelModule.Set_DiversionRouteNo';
begin
  try
    FDiversionRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_BankfillCapacity: Double;
const OPNAME = 'TChannelModule.Get_BankfillCapacity';
begin
  Result := 0.0;
  try
    Result := FBankfillCapacity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_BankfillCapacity(Value: Double);
const OPNAME = 'TChannelModule.Set_BankfillCapacity';
begin
  try
    FBankfillCapacity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_DiversionEfficiency: Double;
const OPNAME = 'TChannelModule.Get_DiversionEfficiency';
begin
  Result := 0.0;
  try
    Result := FDiversionEfficiency;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_DiversionEfficiency(Value: Double);
const OPNAME = 'TChannelModule.Set_DiversionEfficiency';
begin
  try
    FDiversionEfficiency := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_MaxMonthlyDiversionCapacity: Double;
const OPNAME = 'TChannelModule.Get_MaxMonthlyDiversionCapacity';
begin
  Result := 0.0;
  try
    Result := FMaxMonthlyDiversionCapacity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_MaxMonthlyDiversionCapacity(Value: Double);
const OPNAME = 'TChannelModule.Set_MaxMonthlyDiversionCapacity';
begin
  try
    FMaxMonthlyDiversionCapacity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_BankfillArea: Double;
const OPNAME = 'TChannelModule.Get_BankfillArea';
begin
  Result := 0.0;
  try
    Result := FBankfillArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_BankfillArea(Value: Double);
const OPNAME = 'TChannelModule.Set_BankfillArea';
begin
  try
    FBankfillArea := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_BankfillVolume: Double;
const OPNAME = 'TChannelModule.Get_BankfillVolume';
begin
  Result := 0.0;
  try
    Result := FBankFillVolume;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_BankfillVolume(Value: Double);
const OPNAME = 'TChannelModule.Set_BankfillVolume';
begin
  try
    FBankFillVolume := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_PowerOfAreaCapCurve: Double;
const OPNAME = 'TChannelModule.Get_PowerOfAreaCapCurve';
begin
  Result := 0.0;
  try
    Result := FPowerOfAreaCapCurve;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_PowerOfAreaCapCurve(Value: Double);
const OPNAME = 'TChannelModule.Set_PowerOfAreaCapCurve';
begin
  try
    FPowerOfAreaCapCurve := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_BankfillCapacityComprehensive: Double;
const OPNAME = 'TChannelModule.Get_BankfillCapacityComprehensive';
begin
  Result := 0.0;
  try
    Result := FBankfillCapacityComprehensive;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_BankfillCapacityComprehensive(Value: Double);
const OPNAME = 'TChannelModule.Set_BankfillCapacityComprehensive';
begin
  try
    FBankfillCapacityComprehensive := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_WetlandInflowProportion: Double;
const OPNAME = 'TChannelModule.Get_WetlandInflowProportion';
begin
  Result := 0.0;
  try
    Result := FWetlandInflowProportion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_WetlandInflowProportion(Value: Double);
const OPNAME = 'TChannelModule.Set_WetlandInflowProportion';
begin
  try
    FWetlandInflowProportion := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Get_ChannelInflowProportion: Double;
const OPNAME = 'TChannelModule.Get_ChannelInflowProportion';
begin
  Result := 0.0;
  try
    Result := FChannelInflowProportion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelModule.Set_ChannelInflowProportion(Value: Double);
const OPNAME = 'TChannelModule.Set_ChannelInflowProportion';
begin
  try
    FChannelInflowProportion := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.Populate (ANetworkID                   : Integer;
                                  AModuleID                    : Integer;
                                  const AModuleType            : WideString;
                                  AModuleNumber                : Integer;
                                  ANetworkSequence             : Integer;
                                  const AActive                : WideString;
                                  const AChannelName           : WideString;
                                  AVersionNo                   : Integer;
                                  AWetlandMAP                  : Double;
                                  const ARainfallFileName      : WideString;
                                  AMonthlyBedLoss              : Double;
                                  AWetlandStorage              : Double;
                                  AWetlandArea                 : Double;
                                  AWetlandRechargeCoefficient  : Double;
                                  APrincipalOutflowRouteNo     : Integer;
                                  AWetlandType                 : Integer;
                                  AQDiv                        : Double;
                                  AWetlandsInflowRouteNo       : Integer;
                                  AWetlandsOutflowRouteNo      : Integer;
                                  ADiversionRouteNo            : Integer;
                                  ABankfillCapacity            : Double;
                                  ADiversionEfficiency         : Double;
                                  AMaxMonthlyDiversionCapacity : Double;
                                  ALongitude                   : Double;
                                  ALatitude                    : Double): WordBool;
const OPNAME = 'TChannelModule.Populate';
begin
  Result := FALSE;
  try
    FNetworkID                   := ANetworkID;
    FModuleID                    := AModuleID;
    FModuleType                  := AModuleType;
    FModuleNumber                := AModuleNumber;
    FNetworkSequence             := ANetworkSequence;
    FActive                      := AActive;
    FChannelName                 := AChannelName;
    FVersionNo                   := AVersionNo;
    FWetlandMAP                  := AWetlandMAP;
    FRainfallFileName            := ARainfallFileName;
    FMonthlyBedLoss              := AMonthlyBedLoss;
    FWetlandStorage              := AWetlandStorage;
    FWetlandArea                 := AWetlandArea;
    FWetlandRechargeCoefficient  := AWetlandRechargeCoefficient;
    FPrincipalOutflowRouteNo     := APrincipalOutflowRouteNo;
    FWetlandType                 := AWetlandType;
    FQDiv                        := AQDiv;
    FWetlandsInflowRouteNo       := AWetlandsInflowRouteNo;
    FWetlandsOutflowRouteNo      := AWetlandsOutflowRouteNo;
    FDiversionRouteNo            := ADiversionRouteNo;
    FBankfillCapacity            := ABankfillCapacity;
    FDiversionEfficiency         := ADiversionEfficiency;
    FMaxMonthlyDiversionCapacity := AMaxMonthlyDiversionCapacity;
    FLongitude                   := ALongitude;
    FLatitude                    := ALatitude;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.PopulateComprehensiveWetlandParams (ABankfillArea            : Double;
                                                            ABankfillVolume          : Double;
                                                            APowerOfAreaCapCurve     : Double;
                                                            ABankfillCapacity        : Double;
                                                            AWetlandInflowProportion : Double;
                                                            AChannelInflowProportion : Double): WordBool;
const OPNAME = 'TChannelModule.PopulateComprehensiveWetlandParams';
begin
  Result := FALSE;
  try
    FBankfillArea                  := ABankfillArea;
    FBankFillVolume                := ABankfillVolume;
    FPowerOfAreaCapCurve           := APowerOfAreaCapCurve;
    FBankfillCapacityComprehensive := ABankfillCapacity;
    FWetlandInflowProportion       := AWetlandInflowProportion;
    FChannelInflowProportion       := AChannelInflowProportion;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Creates an instance of a TInflowRoute that already exists in the database and adds it to the FInFlowRoutes list
function TChannelModule.AddInflowRoute (ARouteNo        : Integer;
                                        const AFileName : WideString): IInflowRoute;
const OPNAME = 'TChannelModule.AddInflowRoute';
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

// Creates an entry for a new Inflow route in the database.
// Calls AddInflowRoute to create the instance and add it to the FInflowRoutes list.
function TChannelModule.CreateNewInflowRoute(ARouteNo : Integer; const AFileName: WideString) : Boolean;
const OPNAME = 'TChannelModule.CreateNewInflowRoute';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.InsertInflowRouteIntoDB(FModuleID, ARouteNo, AFileName);
    if (Result) then
      AddInflowRoute(ARouteNo, AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Removes the instance of a TInflowRoute from the FInflowRoutes list and destroys the instance.
function TChannelModule.RemoveInflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TChannelModule.CreateNewInflowRoute';
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
function TChannelModule.DeleteInflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TChannelModule.CreateNewInflowRoute';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.DeleteInflowRouteFromDB(FModuleID, ARouteNo);
    if (Result) then
      Result := RemoveInflowRoute(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Creates an instance of a TOutflowRoute that already exists in the database and adds it to the FOutFlowRoutes list
function TChannelModule.AddOutflowRoute (ARouteNo            : Integer;
                                         const AFileName     : WideString;
                                         const AAbstractions : WideString;
                                         AStorageState       : Double;
                                         AReductionFactor    : Double): IOutflowRoute;
const OPNAME = 'TChannelModule.AddOutflowRoute';
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

// Calls AddOutflowRoute to create the instance and add it to the FOutFlowRoutes list.
function TChannelModule.CreateNewOutflowRoute(ARouteNo : Integer; const AFileName: WideString) : Boolean;
const OPNAME = 'TChannelModule.CreateNewOutflowRoute';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.InsertOutflowRouteIntoDB(FModuleID, ARouteNo, AFileName);
    if (Result) then
    begin
      AddOutflowRoute(ARouteNo, AFileName, '', 0, 0);
      if (FPrincipalOutflowRouteNo = 0) then
        MakeOutflowRoutePrincipal(ARouteNo);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Sets the principal outflow route of a Channel Module and updates the database.
function TChannelModule.MakeOutflowRoutePrincipal (ARouteNo : Integer) : Boolean;
const OPNAME = 'TChannelModule.MakeOutflowRoutePrincipal';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.MakeOutflowRoutePrincipalInDB(FModuleID, ARouteNo);
    if (Result) then
      FPrincipalOutflowRouteNo := ARouteNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Removes the instance of a TOutflowRoute from the FOutFlowRoutes list and destroys the instance.
function TChannelModule.RemoveOutflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TChannelModule.RemoveOutflowRoute';
var
  LOutflowRoute   : TOutFlowRoute;
  LNewPrincipal   : IOutFlowRoute;
  LNewPrincipalNo : Integer;
begin
  Result := FALSE;
  try
    LOutflowRoute := FindOutFlowRouteByRouteNo(ARouteNo);
    if (LOutflowRoute <> nil) then
    begin
      FOutFlowRoutes.Remove(LOutflowRoute);
      Result := TRUE;
    end;
    if (FPrincipalOutflowRouteNo = ARouteNo) then
    begin
      LNewPrincipalNo := 0;
      if (NoOfOutFlowRoutes > 0) then
      begin
        LNewPrincipal   := OutFlowRouteByIndex[0];
        LNewPrincipalNo := LNewPrincipal.RouteNo;
      end;
      MakeOutflowRoutePrincipal(LNewPrincipalNo);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Deletes the entry for the outflow route in the database.
// Calls RemoveOutflowRoute to remove the instance from the FOutFlowRoutes list.
function TChannelModule.DeleteOutflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TChannelModule.CreateNewOutflowRoute';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.DeleteOutflowRouteFromDB(FModuleID, ARouteNo);
    if (Result) then
      Result := RemoveOutflowRoute(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.Get_NoOfInflowRoutes: Integer;
const OPNAME = 'TChannelModule.Get_NoOfInflowRoutes';
begin
  Result := 0;
  try
    Result := FInflowRoutes.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.Get_InFlowRouteByRouteNo(ARouteNo: Integer): IInflowRoute;
const OPNAME = 'TChannelModule.Get_InFlowRouteByRouteNo';
begin
  Result := nil;
  try
    Result := FindInFlowRouteByRouteNo(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.FindInFlowRouteByRouteNo(ARouteNo: Integer): TInflowRoute;
const OPNAME = 'TChannelModule.FindInFlowRouteByRouteNo';
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

function TChannelModule.Get_InFlowRouteByIndex(AIndex: Integer): IInflowRoute;
const OPNAME = 'TChannelModule.Get_InFlowRouteByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FInflowRoutes.Count)) then
      Result := TInFlowRoute(FInflowRoutes.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.Get_NoOfOutFlowRoutes: Integer;
const OPNAME = 'TChannelModule.Get_NoOfOutFlowRoutes';
begin
  Result := 0;
  try
    Result := FOutFlowRoutes.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.Get_OutFlowRouteByRouteNo(ARouteNo: Integer): IOutflowRoute;
const OPNAME = 'TChannelModule.Get_OutFlowRouteByRouteNo';
begin
  Result := nil;
  try
    Result := FindOutFlowRouteByRouteNo(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.FindOutFlowRouteByRouteNo(ARouteNo: Integer): TOutflowRoute;
const OPNAME = 'TChannelModule.FindOutFlowRouteByRouteNo';
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

function TChannelModule.Get_OutFlowRouteByIndex(AIndex: Integer): IOutflowRoute;
const OPNAME = 'TChannelModule.Get_OutFlowRouteByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FOutFlowRoutes.Count)) then
      Result := TOutFlowRoute(FOutFlowRoutes.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModule.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TChannelModule.UpdatePropertiesData';
var
  LActive                  : String;
  LLatitude                : Double;
  LLongitude               : Double;
  LChannelName             : String;
  LVersionNo               : Integer;
  LWetlandMAP              : Double;
  LRainfallFileName        : String;
  LMonthlyBedLoss          : Double;
  LWetlandStorage          : Double;
  LWetlandArea             : Double;
  LWetlandRechargeCoef     : Double;
  LPrincipalOutflowRouteNo : Integer;
  LWetlandsInflowRouteNo   : Integer;
  LWetlandsOutflowRouteNo  : Integer;
  LDiversionRouteNo        : Integer;
  LBankfillCapacity        : Double;
  LDiversionEfficiency     : Double;
  LMaxMonthlyDivCapacity   : Double;
  LWetlandType             : Integer;
  LBankfillArea            : Double;
  LBankfillVolume          : Double;
  LPowerOfAreaCapCurve     : Double;
  LBankfillCapacityComp    : Double;
  LWetlandInflowProportion : Double;
  LChannelInflowProportion : Double;
  LQDiv                    : Double;
  LDataNode                : IXMLNode;
begin
  Result := FALSE;
  try
    LDataNode := ARootNode.ChildNodes['ChannelProperties'];

    LActive                  := LDataNode.ChildNodes['Active'].Text;
    LLatitude                := StrToFloat(LDataNode.ChildNodes['Latitude'].Text);
    LLongitude               := StrToFloat(LDataNode.ChildNodes['Longitude'].Text);
    LChannelName             := LDataNode.ChildNodes['ChannelName'].Text;
    LVersionNo               := StrToInt(LDataNode.ChildNodes['VersionNo'].Text);
    LWetlandMAP              := StrToFloat(LDataNode.ChildNodes['WetlandMAP'].Text);
    LRainfallFileName        := LDataNode.ChildNodes['RainfallFileName'].Text;
    LMonthlyBedLoss          := StrToFloat(LDataNode.ChildNodes['MonthlyBedLoss'].Text);
    LWetlandStorage          := StrToFloat(LDataNode.ChildNodes['WetlandStorage'].Text);
    LWetlandArea             := StrToFloat(LDataNode.ChildNodes['WetlandArea'].Text);
    LWetlandRechargeCoef     := StrToFloat(LDataNode.ChildNodes['WetlandRechargeCoefficient'].Text);
    LPrincipalOutflowRouteNo := StrToInt(LDataNode.ChildNodes['PrincipalOutflowRouteNo'].Text);
    LWetlandsInflowRouteNo   := StrToInt(LDataNode.ChildNodes['WetlandsInflowRouteNo'].Text);
    LWetlandsOutflowRouteNo  := StrToInt(LDataNode.ChildNodes['WetlandsOutflowRouteNo'].Text);
    LDiversionRouteNo        := StrToInt(LDataNode.ChildNodes['DiversionRouteNo'].Text);
    LBankfillCapacity        := StrToFloat(LDataNode.ChildNodes['BankfillCapacity'].Text);
    LDiversionEfficiency     := StrToFloat(LDataNode.ChildNodes['DiversionEfficiency'].Text);
    LMaxMonthlyDivCapacity   := StrToFloat(LDataNode.ChildNodes['MaxMonthlyDiversionCapacity'].Text);
    LWetlandType             := StrToInt(LDataNode.ChildNodes['WetlandType'].Text);
    LBankfillArea            := StrToFloat(LDataNode.ChildNodes['BankfillArea'].Text);
    LBankfillVolume          := StrToFloat(LDataNode.ChildNodes['BankfillVolume'].Text);
    LPowerOfAreaCapCurve     := StrToFloat(LDataNode.ChildNodes['PowerOfAreaCapCurve'].Text);
    LBankfillCapacityComp    := StrToFloat(LDataNode.ChildNodes['BankfillCapacityComp'].Text);
    LWetlandInflowProportion := StrToFloat(LDataNode.ChildNodes['WetlandInflowProportion'].Text);
    LChannelInflowProportion := StrToFloat(LDataNode.ChildNodes['ChannelInflowProportion'].Text);
    LQDiv                    := StrToFloat(LDataNode.ChildNodes['QDiv'].Text);

    Result := GChannelDBManager.UpdatePropertiesDataInDB
                                  (FModuleID, LActive, LLatitude, LLongitude, LChannelName, LVersionNo,
                                   LWetlandMAP, LRainfallFileName, LMonthlyBedLoss, LWetlandStorage, LWetlandArea,
                                   LWetlandRechargeCoef, LPrincipalOutflowRouteNo, LWetlandsInflowRouteNo,
                                   LWetlandsOutflowRouteNo, LDiversionRouteNo, LBankfillCapacity, LDiversionEfficiency,
                                   LMaxMonthlyDivCapacity, FWetlandType, LWetlandType, LBankfillArea, LBankfillVolume,
                                   LPowerOfAreaCapCurve, LBankfillCapacityComp, LWetlandInflowProportion,
                                   LChannelInflowProportion, LQDiv);

    if (Result) then
    begin
      FLatitude                    := LLatitude;
      FLongitude                   := LLongitude;
      FActive                      := LActive;
      FChannelName                 := LChannelName;
      FVersionNo                   := LVersionNo;
      FWetlandMAP                  := LWetlandMAP;
      FRainfallFileName            := LRainfallFileName;
      FMonthlyBedLoss              := LMonthlyBedLoss;
      FWetlandStorage              := LWetlandStorage;
      FWetlandArea                 := LWetlandArea;
      FWetlandRechargeCoefficient  := LWetlandRechargeCoef;
      FPrincipalOutflowRouteNo     := LPrincipalOutflowRouteNo;
      FWetlandsInflowRouteNo       := LWetlandsInflowRouteNo;
      FWetlandsOutflowRouteNo      := LWetlandsOutflowRouteNo;
      FDiversionRouteNo            := LDiversionRouteNo;
      FBankfillCapacity            := LBankfillCapacity;
      FDiversionEfficiency         := LDiversionEfficiency;
      FMaxMonthlyDiversionCapacity := LMaxMonthlyDivCapacity;
      FWetlandType                 := LWetlandType;
      FQDiv                        := LQDiv;
      FMaxMonthlyDiversionCapacity := LMaxMonthlyDivCapacity;
      if (LWetlandType <> 2) then
      begin
        FBankfillArea                  := 0;
        FBankFillVolume                := 0;
        FPowerOfAreaCapCurve           := 0;
        FBankfillCapacityComprehensive := 0;
        FWetlandInflowProportion       := 0;
        FChannelInflowProportion       := 0;
      end;
      FBankfillArea                  := LBankfillArea;
      FBankFillVolume                := LBankfillVolume;
      FPowerOfAreaCapCurve           := LPowerOfAreaCapCurve;
      FBankfillCapacityComprehensive := LBankfillCapacityComp;
      FWetlandInflowProportion       := LWetlandInflowProportion;
      FChannelInflowProportion       := LChannelInflowProportion;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.UpdateInflowRoutesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TChannelModule.UpdateInflowRoutesData';
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
        if (GChannelDBManager.UpdateInflowRoutesDataInDB(FModuleID, StrToInt(LInflowRouteNo), LInflowFileName)) then
          InFlowRouteByRouteNo[StrToInt(LInflowRouteNo)].FileName := LInflowFileName
        else
          Result := FALSE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.UpdateOutflowRoutesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TChannelModule.UpdateOutflowRoutesData';
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
        if (GChannelDBManager.UpdateOutflowRoutesDataInDB(FModuleID, StrToInt(LOutflowRouteNo), LOutflowFileName)) then
          OutflowRouteByRouteNo[StrToInt(LOutflowRouteNo)].FileName := LOutflowFileName
        else
          Result := FALSE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.DeleteChannelFromDB : Boolean;
const OPNAME = 'TChannelModule.DeleteChannelFromDB';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.DeleteChannelFromDB(FModuleID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelModule.DeleteCompWetlandParamsFromDB : Boolean;
const OPNAME = 'TChannelModule.DeleteCompWetlandParamsFromDB';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.DeleteCompWetlandParamsFromDB(FModuleID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TChannelModuleAgent *********************************************************}

function TChannelModuleAgent._AddRef: Integer;
const OPNAME = 'TChannelModuleAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent._Release: Integer;
const OPNAME = 'TChannelModuleAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelModuleAgent.CreateMemberObjects;
const OPNAME = 'TChannelModuleAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GChannelDBManager := TChannelDBManager.Create;
    GChannelDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelModuleAgent.DestroyMemberObjects;
const OPNAME = 'TChannelModuleAgent.DestroyMemberObjects';
begin
  try
    GChannelDBManager.ModuleAgent := nil;
    FreeAndNil(GChannelDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.AddChannelModule : TChannelModule;
const OPNAME = 'TChannelModuleAgent.AddChannelModule';
var
  LChannelModule : TChannelModule;
begin
  Result := nil;
  try
    LChannelModule := TChannelModule.Create;
    FList.Add(LChannelModule);
    Result := LChannelModule;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.CreateNewChannelModule (ANetworkID: Integer): IChannelModule;
const OPNAME = 'TChannelModuleAgent.CreateNewChannelModule';
begin
  Result := nil;
  try
    Result := GChannelDBManager.CreateNewChannelModuleInDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.RemoveChannelModule (AModuleNumber : Integer): WordBool;
const OPNAME = 'TChannelModuleAgent.RemoveChannelModule';
var
  LChannelModule : TChannelModule;
  LIndex         : Integer;
begin
  Result := FALSE;
  try
    LChannelModule := FindChannelModuleByNumber(AModuleNumber);
    if (LChannelModule <> nil) then
    begin
      if (GChannelDBManager.RemoveChannelModuleInDB(LChannelModule.ModuleID)) then
      begin
        Result := TRUE;
        LIndex := FList.IndexOf(LChannelModule);
        FList.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.Get_ChannelModuleCount: Integer;
const OPNAME = 'TChannelModuleAgent.Get_ChannelModuleCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.FindChannelModuleByID (AModuleID: Integer): TChannelModule;
const OPNAME = 'TChannelModuleAgent.FindChannelModuleByID';
var
  LChannelModule : TChannelModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LChannelModule := TChannelModule(FList.Items[LIndex]);
      if (LChannelModule.ModuleID = AModuleID) then
        Result := LChannelModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.Get_ChannelModuleByID(AModuleID: Integer): IChannelModule;
const OPNAME = 'TChannelModuleAgent.Get_ChannelModuleByID';
var
  LChannelModule : TChannelModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LChannelModule := TChannelModule(FList.Items[LIndex]);
      if (LChannelModule.ModuleID = AModuleID) then
        Result := LChannelModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.FindChannelModuleByNumber (AModuleNumber : Integer): TChannelModule;
const OPNAME = 'TChannelModuleAgent.FindChannelModuleByNumber';
var
  LChannelModule : TChannelModule;
  LIndex         : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LChannelModule := TChannelModule(FList.Items[LIndex]);
      if (LChannelModule.ModuleNumber = AModuleNumber) then
        Result := LChannelModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.Get_ChannelModuleByNumber (AModuleNumber : Integer): IChannelModule;
const OPNAME = 'TChannelModuleAgent.Get_ChannelModuleByNumber';
var
  LChannelModule : TChannelModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LChannelModule := TChannelModule(FList.Items[LIndex]);
      if (LChannelModule.ModuleNumber = AModuleNumber) then
        Result := LChannelModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.Get_ChannelModuleByIndex(AIndex: Integer): IChannelModule;
const OPNAME = 'TChannelModuleAgent.Get_ChannelModuleByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TChannelModule(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelModuleAgent.LoadChannelModules (ANetworkID : Integer) : Boolean;
const OPNAME = 'TChannelModuleAgent.LoadChannelModules';
begin
  Result := FALSE;
  try
    Result := GChannelDBManager.LoadChannelModulesFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
