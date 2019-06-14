(******************************************************************************)
(*  Contains : Class TObservationPoint.
(******************************************************************************)
unit UObservationPoint;


interface

uses
  Classes,
  Contnrs,
  XMLIntf,

  UModule,
  UTimeSeries,
  UAbstractObject,
  HydrologyCom_TLB;

type
  TObservationPoint = class(TAbstractObject, IObservationPoint)
  protected
    FNetworkID        : Integer;
    FRouteNo          : Integer;
    FName             : String;
    FFlowDataFileName : String;
    FFlowData         : TTimeSeries;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_FlowDataFileName: WideString; safecall;
    procedure Set_FlowDataFileName(const Value: WideString); safecall;
    function Get_FlowData: ITimeSeries; safecall;
    function Populate (ANetworkID              : Integer;
                       ARouteNo                : Integer;
                       const AName             : WideString;
                       const AFlowDataFileName : WideString): WordBool; safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    procedure ClearFlowData;
    procedure AddFlowDataPeriodValue (AYear : Integer; AValue : Double);
    procedure CalculateFlowDataTotals;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property Name: WideString read Get_Name write Set_Name;
    property FlowDataFileName: WideString read Get_FlowDataFileName write Set_FlowDataFileName;
    property FlowData: ITimeSeries read Get_FlowData;
  end;

  TObservationPointAgent = class(TModuleAgent, IObservationPointAgent)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ObservationPointCount: Integer; safecall;
    function FindObservationPointByRouteNo(ARouteNo: Integer): TObservationPoint;
    function Get_ObservationPointByRouteNo(ARouteNo: Integer): IObservationPoint; safecall;
    function Get_ObservationPointByIndex(AIndex: Integer): IObservationPoint; safecall;
    function LoadObservationPoints (ANetworkID: Integer) : Boolean;
    function AddObservationPoint: TObservationPoint;
    function CreateNewObservationPoint (ANetworkID : Integer;
                                        ARouteNo   : Integer) : IObservationPoint; safecall;
    function RemoveObservationPoint(ANetworkID : Integer;
                                    ARouteNo   : Integer): WordBool; safecall;
    property ObservationPointCount: Integer read Get_ObservationPointCount;
    property ObservationPointByRouteNo[ARouteNo: Integer]: IObservationPoint read Get_ObservationPointByRouteNo;
    property ObservationPointByIndex[AIndex: Integer]: IObservationPoint read Get_ObservationPointByIndex;
  end;


implementation

uses

  SysUtils,
  Windows,
  VCL.Forms,
  Math,

  UModuleDBManager,
  UObsPointDBManager,
  UErrorHandlingOperations;

{ TObservationPoint ***********************************************************}

function TObservationPoint._AddRef: Integer;
const OPNAME = 'TObservationPoint._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPoint._Release: Integer;
const OPNAME = 'TObservationPoint._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TObservationPoint.CreateMemberObjects;
const OPNAME = 'TObservationPoint.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FFlowData := TTimeSeries.Create(12);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPoint.DestroyMemberObjects;
const OPNAME = 'TObservationPoint.DestroyMemberObjects';
begin
  try
    if (FFlowData <> nil) then
    begin
      FreeAndNil(FFlowData);
    end;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPoint.Get_RouteNo: Integer;
const OPNAME = 'TObservationPoint.Get_RouteNo';
begin
  Result := 0;
  try
    Result := FRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPoint.Set_RouteNo(Value: Integer);
const OPNAME = 'TObservationPoint.Set_RouteNo';
begin
  try
    FRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPoint.Get_Name: WideString;
const OPNAME = 'TObservationPoint.Get_Name';
begin
  Result := '';
  try
    Result := FName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPoint.Set_Name(const Value: WideString);
const OPNAME = 'TObservationPoint.Set_Name';
begin
  try
    FName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPoint.Get_FlowDataFileName: WideString;
const OPNAME = 'TObservationPoint.Get_FlowDataFileName';
begin
  Result := '';
  try
    Result := FFlowDataFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPoint.Set_FlowDataFileName(const Value: WideString);
const OPNAME = 'TObservationPoint.Set_FlowDataFileName';
begin
  try
    if ((FFlowDataFileName <> '') AND (NOT GObsPointDBManager.IsFlowFileUsed(FFlowDataFileName))) then
      GObsPointDBManager.DeleteFlowDataFromDB(Self);
    FFlowDataFileName := Value;
    if ((FFlowDataFileName <> '') AND (NOT GObsPointDBManager.IsFlowDataLoaded(FFlowDataFileName))) then
      GObsPointDBManager.LoadFlowFileData(FNetworkID, FFlowDataFileName);
    GObsPointDBManager.LoadFlowData(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPoint.Get_FlowData : ITimeSeries;
const OPNAME = 'TObservationPoint.Get_FlowData';
begin
  Result := nil;
  try
    Result := FFlowData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPoint.Populate (ANetworkID              : Integer;
                                     ARouteNo                : Integer;
                                     const AName             : WideString;
                                     const AFlowDataFileName : WideString): WordBool;
const OPNAME = 'TObservationPoint.Populate';
begin
  Result := FALSE;
  try
    FNetworkID        := ANetworkID;
    FRouteNo          := ARouteNo;
    FName             := AName;
    FFlowDataFileName := AFlowDataFileName;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPoint.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TObservationPoint.UpdatePropertiesData';
var
  LRouteNo          : Integer;
  LName             : String;
  LFlowDataFileName : String;
  LDataNode         : IXMLNode;
begin
  Result := FALSE;
  try
    LDataNode         := ARootNode.ChildNodes['ObservationPoint'];
    LRouteNo          := StrToInt(LDataNode.ChildNodes['RouteNo'].Text);
    LName             := LDataNode.ChildNodes['Name'].Text;
    LFlowDataFileName := LDataNode.ChildNodes['FlowDataFileName'].Text;

    Result := GObsPointDBManager.UpdatePropertiesDataInDB(FNetworkID, LRouteNo, LName, LFlowDataFileName);
    if (Result) then
    begin
      if (FFlowDataFileName <> LFlowDataFileName) then
        FlowDataFileName := LFlowDataFileName;
      FName            := LName;
      FRouteNo         := LRouteNo;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPoint.ClearFlowData;
const OPNAME = 'TObservationPoint.ClearFlowData';
begin
  try
    FFlowData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPoint.AddFlowDataPeriodValue (AYear : Integer; AValue : Double);
const OPNAME = 'TObservationPoint.AddFlowDataPeriodValue';
begin
  try
    FFlowData.AddPeriodValue(AYear, AValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPoint.CalculateFlowDataTotals;
const OPNAME = 'TObservationPoint.CalculateFlowDataTotals';
begin
  try
    FFlowData.CalculateTotals;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



{ TObservationPointAgent ******************************************************}

procedure TObservationPointAgent.CreateMemberObjects;
const OPNAME = 'TObservationPointAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GObsPointDBManager := TObsPointDBManager.Create;
    GObsPointDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TObservationPointAgent.DestroyMemberObjects;
const OPNAME = 'TObservationPointAgent.DestroyMemberObjects';
begin
  try
    GObsPointDBManager.ModuleAgent := nil;
    FreeAndNil(GObsPointDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent._AddRef: Integer;
const OPNAME = 'TObservationPointAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent._Release: Integer;
const OPNAME = 'TObservationPointAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.AddObservationPoint : TObservationPoint;
const OPNAME = 'TObservationPointAgent.AddObservationPoint';
var
  LObservationPoint : TObservationPoint;
begin
  Result := nil;
  try
    LObservationPoint := TObservationPoint.Create;
    FList.Add(LObservationPoint);
    Result := LObservationPoint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.CreateNewObservationPoint (ANetworkID : Integer;
                                                           ARouteNo   : Integer): IObservationPoint;
const OPNAME = 'TObservationPointAgent.CreateNewObservationPoint';
var
  LObsPoint : IObservationPoint;
  LResult   : Boolean;
begin
  Result := nil;
  try
    LResult := GObsPointDBManager.InsertObservationPointIntoDB(ANetworkID, ARouteNo);
    if (LResult) then
    begin
      LObsPoint := AddObservationPoint;
      LObsPoint.Populate(ANetworkID, ARouteNo, 'OBS' + IntToStr(ARouteNo), '');
      Result := LObsPoint;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.RemoveObservationPoint (ANetworkID : Integer;
                                                        ARouteNo   : Integer): WordBool;
const OPNAME = 'TObservationPointAgent.RemoveObservationPoint';
var
  LObsPoint : TObservationPoint;
  LIndex    : Integer;
  LResult   : Boolean;
begin
  Result := FALSE;
  try
    LObsPoint := FindObservationPointByRouteNo(ARouteNo);
    LResult := FALSE;
    if (LObsPoint <> nil) then
    begin
      LResult := GObsPointDBManager.DeleteObservationPointFromDB(ANetworkID, LObsPoint.RouteNo);
      if (LResult) then
      begin
        LIndex := FList.IndexOf(LObsPoint);
        FList.Delete(LIndex);
      end;
    end;
    Result := LResult;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.Get_ObservationPointCount: Integer;
const OPNAME = 'TObservationPointAgent.Get_ObservationPointCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.FindObservationPointByRouteNo (ARouteNo: Integer): TObservationPoint;
const OPNAME = 'TObservationPointAgent.FindObservationPointByRouteNo';
var
  LObservationPoint : TObservationPoint;
  LIndex            : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LObservationPoint := TObservationPoint(FList.Items[LIndex]);
      if (LObservationPoint.RouteNo = ARouteNo) then
        Result := LObservationPoint
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.Get_ObservationPointByRouteNo (ARouteNo: Integer): IObservationPoint;
const OPNAME = 'TObservationPointAgent.Get_ObservationPointByRouteNo';
var
  LObservationPoint : TObservationPoint;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LObservationPoint := TObservationPoint(FList.Items[LIndex]);
      if (LObservationPoint.RouteNo = ARouteNo) then
        Result := LObservationPoint
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.Get_ObservationPointByIndex(AIndex: Integer): IObservationPoint;
const OPNAME = 'TObservationPointAgent.Get_ObservationPointByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TObservationPoint(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObservationPointAgent.LoadObservationPoints (ANetworkID: Integer) : Boolean;
const OPNAME = 'TObservationPointAgent.LoadObservationPoints';
begin
  Result := FALSE;
  try
    Result := GObsPointDBManager.LoadObservationPointsFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
