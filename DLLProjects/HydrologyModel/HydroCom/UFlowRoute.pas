(******************************************************************************)
(*  Contains : Class TInFlowRoute and TOutFlowRoute.
(******************************************************************************)
unit UFlowRoute;


interface

uses
  Classes,
  Contnrs,

  UAbstractObject,
  HydrologyCom_TLB;

type
  TMonthlyDoubleArray = array [1..12] of Double;

  TInFlowRoute = class(TAbstractObject, IInFlowRoute)
  protected
    FRouteNo     : Integer;
    FFileName    : String;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property FileName: WideString read Get_FileName write Set_FileName;
  end;

  TOutFlowRoute = class(TAbstractObject, IOutFlowRoute)
  protected
    FRouteNo                  : Integer;
    FFileName                 : String;
    FMonthlyAbstractionVolume : TMonthlyDoubleArray;
    FStorageState             : Double;
    FReductionFactor          : Double;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_AbstractionVolumeByMonth(AMonth: Integer): Double; safecall;
    procedure Set_AbstractionVolumeByMonth(AMonth: Integer; Value: Double); safecall;
    function Get_StorageState: Double; safecall;
    procedure Set_StorageState(Value: Double); safecall;
    function Get_ReductionFactor: Double; safecall;
    procedure Set_ReductionFactor(Value: Double); safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property FileName: WideString read Get_FileName write Set_FileName;
    property AbstractionVolumeByMonth[AMonth: Integer]: Double read Get_AbstractionVolumeByMonth write Set_AbstractionVolumeByMonth;
    property StorageState: Double read Get_StorageState write Set_StorageState;
    property ReductionFactor: Double read Get_ReductionFactor write Set_ReductionFactor;
  end;

implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils,
  Windows,
  VCL.Forms,
  Math,
  UErrorHandlingOperations;

{ TInFlowRoute ****************************************************************}

function TInFlowRoute._AddRef: Integer;
const OPNAME = 'TInFlowRoute._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInFlowRoute._Release: Integer;
const OPNAME = 'TInFlowRoute._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInFlowRoute.Get_RouteNo: Integer;
const OPNAME = 'TInFlowRoute.Get_RouteNo';
begin
  Result := 0;
  try
    Result := FRouteNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TInFlowRoute.Set_RouteNo(Value: Integer);
const OPNAME = 'TInFlowRoute.Set_RouteNo';
begin
  try
    FRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInFlowRoute.Get_FileName: WideString;
const OPNAME = 'TInFlowRoute.Get_FileName';
begin
  Result := '';
  try
    Result := FFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TInFlowRoute.Set_FileName(const Value: WideString);
const OPNAME = 'TInFlowRoute.Set_FileName';
begin
  try
    FFileName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TOutFlowRoute ***************************************************************}

function TOutFlowRoute._AddRef: Integer;
const OPNAME = 'TOutFlowRoute._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutFlowRoute._Release: Integer;
const OPNAME = 'TOutFlowRoute._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutFlowRoute.Get_RouteNo: Integer;
const OPNAME = 'TOutFlowRoute.Get_RouteNo';
begin
  Result := 0;
  try
    Result := FRouteNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutFlowRoute.Set_RouteNo(Value: Integer);
const OPNAME = 'TOutFlowRoute.Set_RouteNo';
begin
  try
    FRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutFlowRoute.Get_FileName: WideString;
const OPNAME = 'TOutFlowRoute.Get_FileName';
begin
  Result := '';
  try
    Result := FFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutFlowRoute.Set_FileName(const Value: WideString);
const OPNAME = 'TOutFlowRoute.Set_FileName';
begin
  try
    FFileName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutFlowRoute.Get_AbstractionVolumeByMonth(AMonth: Integer): Double;
const OPNAME = 'TOutFlowRoute.Get_AbstractionVolumeByMonth';
begin
  Result := 0;
  try
    if ((AMonth >= Low(FMonthlyAbstractionVolume)) AND (AMonth <= High(FMonthlyAbstractionVolume))) then
      Result := FMonthlyAbstractionVolume[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutFlowRoute.Set_AbstractionVolumeByMonth(AMonth: Integer; Value: Double);
const OPNAME = 'TOutFlowRoute.Set_AbstractionVolumeByMonth';
begin
  try
    if ((AMonth >= Low(FMonthlyAbstractionVolume)) AND (AMonth <= High(FMonthlyAbstractionVolume))) then
      FMonthlyAbstractionVolume[AMonth] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutFlowRoute.Get_StorageState: Double;
const OPNAME = 'TOutFlowRoute.Get_StorageState';
begin
  Result := 0.0;
  try
    Result := FStorageState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutFlowRoute.Set_StorageState(Value: Double);
const OPNAME = 'TOutFlowRoute.Set_StorageState';
begin
  try
    FStorageState := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutFlowRoute.Get_ReductionFactor: Double;
const OPNAME = 'TOutFlowRoute.Get_ReductionFactor';
begin
  Result := 0.0;
  try
    Result := FReductionFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutFlowRoute.Set_ReductionFactor(Value: Double);
const OPNAME = 'TOutFlowRoute.Set_ReductionFactor';
begin
  try
    FReductionFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
