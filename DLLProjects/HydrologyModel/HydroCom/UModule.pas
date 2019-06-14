(******************************************************************************)
(*  Contains : Class TModule.
(******************************************************************************)
unit UModule;


interface

uses
  Classes,
  Contnrs,
  XMLIntf,

  UAbstractObject,
  HydrologyCom_TLB;

type
  TMonthlyDoubleArray = array [1..12] of Double;

  TYearValuePair = class(TObject)
  protected
    FYear  : Integer;
    FValue : Double;
  public
    property Year  : Integer read FYear  write FYear;
    property Value : Double  read FValue write FValue;
  end;

  TPan = class(TAbstractObject, IPan)
  protected
    FMonth       : Integer;
    FEvaporation : Double;
    FFactor      : Double;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_Month: Integer; safecall;
    procedure Set_Month(Value: Integer); safecall;
    function Get_Evaporation: Double; safecall;
    procedure Set_Evaporation(Value: Double); safecall;
    function Get_Factor: Double; safecall;
    procedure Set_Factor(Value: Double); safecall;
    property Month: Integer read Get_Month write Set_Month;
    property Evaporation: Double read Get_Evaporation write Set_Evaporation;
    property Factor: Double read Get_Factor write Set_Factor;
  end;

  TModule = class(TAbstractObject, IModule)
  protected
    FModuleID    : Integer;
    FModuleType  : String;
    FLongitude   : Double;
    FLatitude    : Double;
    FPanList     : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ModuleID: Integer; safecall;
    function Get_ModuleType: WideString; safecall;
    procedure Set_ModuleType(const Value: WideString); safecall;
    function Get_Longitude: Double; safecall;
    procedure Set_Longitude(Value: Double); safecall;
    function Get_Latitude: Double; safecall;
    procedure Set_Latitude(Value: Double); safecall;
    function Get_PanCount: Integer; safecall;
    function Get_PanByMonth(AMonth: Integer): IPan; safecall;
    function Get_PanByIndex(AIndex: Integer): IPan; safecall;
    function AddPan (AMonth       : Integer;
                     AEvaportaion : Double;
                     AFactor      : Double): IPan; safecall;

    function UpdatePanData (ARootNode : IXMLNode): WordBool;

    property ModuleID: Integer read Get_ModuleID;
    property ModuleType: WideString read Get_ModuleType write Set_ModuleType;
    property Longitude: Double read Get_Longitude write Set_Longitude;
    property Latitude: Double read Get_Latitude write Set_Latitude;
    property PanCount: Integer read Get_PanCount;
    property PanByMonth[AMonth: Integer]: IPan read Get_PanByMonth;
    property PanByIndex[AIndex: Integer]: IPan read Get_PanByIndex;
  end;

  TNetworkModule = class(TModule, INetworkModule)
  protected
    FNetworkID         : Integer;
    FModuleNumber      : Integer;
    FNetworkSequence   : Integer;
    FActive            : String;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_NetworkID: Integer; safecall;
    function Get_ModuleNumber: Integer; safecall;
    procedure Set_ModuleNumber(Value: Integer); safecall;
    function Get_NetworkSequence: Integer; safecall;
    procedure Set_NetworkSequence(Value: Integer); safecall;
    function Get_Active: WideString; safecall;
    procedure Set_Active(const Value: WideString); safecall;
    property NetworkID: Integer read Get_NetworkID;
    property ModuleNumber: Integer read Get_ModuleNumber write Set_ModuleNumber;
    property NetworkSequence: Integer read Get_NetworkSequence write Set_NetworkSequence;
    property Active: WideString read Get_Active write Set_Active;
  end;

  TModuleAgent = class(TAbstractObject)
  protected
    FList : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Initialise : Boolean; override;
  end;

implementation

uses

  SysUtils,
  Windows,
  VCL.Forms,
  Math,

  UModuleDBManager,
  UErrorHandlingOperations;

{ TPan ************************************************************************}

function TPan._AddRef: Integer;
const OPNAME = 'TPan._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPan._Release: Integer;
const OPNAME = 'TPan._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPan.Get_Month: Integer;
const OPNAME = 'TPan.Get_Month';
begin
  Result := 0;
  try
    Result := FMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPan.Set_Month(Value: Integer);
const OPNAME = 'TPan.Set_Month';
begin
  try
    FMonth := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPan.Get_Evaporation: Double;
const OPNAME = 'TPan.Get_Evaporation';
begin
  Result := 0.0;
  try
    Result := FEvaporation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPan.Set_Evaporation(Value: Double);
const OPNAME = 'TPan.Set_Evaporation';
begin
  try
    FEvaporation := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPan.Get_Factor: Double;
const OPNAME = 'TPan.Get_Factor';
begin
  Result := 0.0;
  try
    Result := FFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPan.Set_Factor(Value: Double);
const OPNAME = 'TPan.Set_Factor';
begin
  try
    FFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TModule *********************************************************************}

function TModule._AddRef: Integer;
const OPNAME = 'TModule._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModule._Release: Integer;
const OPNAME = 'TModule._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModule.CreateMemberObjects;
const OPNAME = 'TModule.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanList := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModule.DestroyMemberObjects;
const OPNAME = 'TModule.DestroyMemberObjects';
begin
  try
    FPanList.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModule.Get_ModuleID: Integer;
const OPNAME = 'TModule.Get_ModuleID';
begin
  Result := 0;
  try
    Result := FModuleID;;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.Get_ModuleType: WideString;
const OPNAME = 'TModule.Get_ModuleType';
begin
  Result := '';
  try
    Result := FModuleType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModule.Set_ModuleType(const Value: WideString);
const OPNAME = 'TModule.Set_ModuleType';
begin
  try
    FModuleType := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.Get_Longitude: Double;
const OPNAME = 'TModule.Get_Longitude';
begin
  Result := 0.0;
  try
    Result := FLongitude;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModule.Set_Longitude(Value: Double);
const OPNAME = 'TModule.Set_Longitude';
begin
  try
    if (GModuleDBManager.UpdateLongitudeInDB(FModuleID, Value)) then
      FLongitude := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.Get_Latitude: Double;
const OPNAME = 'TModule.Get_Latitude';
begin
  Result := 0.0;
  try
    Result := FLatitude;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModule.Set_Latitude(Value: Double);
const OPNAME = 'TModule.Set_Latitude';
begin
  try
    if (GModuleDBManager.UpdateLatitudeInDB(FModuleID, Value)) then
      FLatitude := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.Get_PanCount : Integer;
const OPNAME = 'TModule.Get_PanCount';
begin
  Result := 0;
  try
    Result := FPanList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.Get_PanByMonth(AMonth: Integer): IPan;
const OPNAME = 'TModule.Get_PanByMonth';
var
  LPan   : IPan;
  LIndex : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FPanList.Count)) do
    begin
      LPan := TPan(FPanList.Items[LIndex]);
      if (LPan.Month = AMonth) then
        Result := LPan
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.Get_PanByIndex(AIndex: Integer): IPan;
const OPNAME = 'TModule.Get_PanByIndex';
begin
  Result := nil;
  try
    if (AIndex < FPanList.Count) then
      Result := TPan( FPanList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.AddPan (AMonth       : Integer;
                         AEvaportaion : Double;
                         AFactor      : Double): IPan;
const OPNAME = 'TModule.AddPan';
var
  LPan : TPan;
begin
  Result := nil;
  try
    LPan := TPan.Create;
    FPanList.Add(LPan);
    LPan.FMonth       := AMonth;
    LPan.FEvaporation := AEvaportaion;
    LPan.FFactor      := AFactor;
    Result := LPan;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModule.UpdatePanData (ARootNode : IXMLNode): WordBool;
const OPNAME = 'TModule.UpdatePanData';
var
  LListNode        : IXMLNode;
  LDataNode        : IXMLNode;
  LIndex           : Integer;
  LMonth           : Integer;
  LPanEvaporation  : Double;
  LPanFactor       : Double;
  LPan             : IPan;
  LResult          : Boolean;
begin
  Result := FALSE;
  try
    Result := TRUE;
    LListNode := ARootNode.ChildNodes['PanData'];
    LListNode := LListNode.ChildNodes['DataList'];
    for LIndex := 1 to LListNode.ChildNodes.Count do
    begin
      LDataNode       := LListNode.ChildNodes.Get(LIndex-1);
      LMonth          := StrToInt(LDataNode.ChildNodes['Month'].Text);
      LPanEvaporation := StrToFloat(LDataNode.ChildNodes['PanEvaporation'].Text);
      LPanFactor      := StrToFloat(LDataNode.ChildNodes['PanFactor'].Text);
      LPan := PanByMonth[LMonth];
      if ((LPan.Evaporation <> LPanEvaporation) OR (LPan.Factor <> LPanFactor)) then
      begin
        LResult := GModuleDBManager.UpdatePanDataInDB(FModuleID, LMonth, LPanEvaporation, LPanFactor);
        Result  := Result AND LResult;
        if (LResult) then
        begin
          LPan.Evaporation := LPanEvaporation;
          LPan.Factor      := LPanFactor;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TNetworkModule **************************************************************}

function TNetworkModule._AddRef: Integer;
const OPNAME = 'TNetworkModule._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkModule._Release: Integer;
const OPNAME = 'TNetworkModule._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkModule.Get_NetworkID: Integer;
const OPNAME = 'TNetworkModule.Get_NetworkID';
begin
  Result := 0;
  try
    Result := FNetworkID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkModule.Get_ModuleNumber: Integer;
const OPNAME = 'TNetworkModule.Get_NetworkID';
begin
  Result := 0;
  try
    Result := FModuleNumber;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkModule.Set_ModuleNumber(Value: Integer);
const OPNAME = 'TNetworkModule.Get_NetworkID';
begin
  try
    FModuleNumber := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkModule.Get_NetworkSequence: Integer;
const OPNAME = 'TNetworkModule.Get_NetworkID';
begin
  Result := 0;
  try
    Result := FNetworkSequence;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkModule.Set_NetworkSequence(Value: Integer);
const OPNAME = 'TNetworkModule.Set_NetworkSequence';
begin
  try
    if (GModuleDBManager.UpdateNetworkSequenceInDB(FModuleID, Value)) then
      FNetworkSequence := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkModule.Get_Active: WideString;
const OPNAME = 'TNetworkModule.Get_Active';
begin
  Result := '';
  try
    Result := FActive;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkModule.Set_Active(const Value: WideString);
const OPNAME = 'TNetworkModule.Set_Active';
begin
  try
    FActive := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TModuleAgent ****************************************************************}

function TModuleAgent._AddRef: Integer;
const OPNAME = 'TModuleAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModuleAgent._Release: Integer;
const OPNAME = 'TModuleAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModuleAgent.CreateMemberObjects;
const OPNAME = 'TModuleAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FList := TObjectList.Create(FALSE);

//    GModuleDBManager := TModuleDBManager.Create;
//    GModuleDBManager.ModuleAgent := Self;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModuleAgent.DestroyMemberObjects;
const OPNAME = 'TModuleAgent.DestroyMemberObjects';
begin
  try
    Initialise;
//    GModuleDBManager.ModuleAgent := nil;
    FreeAndNil(FList);
//    FreeAndNil(GModuleDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModuleAgent.Initialise : Boolean;
const OPNAME = 'TModuleAgent.Initialise';
begin
  Result := FALSE;
  try
    while (FList.Count > 0) do
      FList.Delete(0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
