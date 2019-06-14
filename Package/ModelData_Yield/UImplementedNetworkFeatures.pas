{******************************************************************************}
{*  UNIT      : Contains the class TImplementedNetworkFeatures.                *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/10/24                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UImplementedNetworkFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{*Implemented Features Object                                                 *}
{******************************************************************************}

  TImplementedNetworkFeatures = class(TAbstractAppObject, IImplementedNetworkFeatures)
  protected
    FPowerPlantFeatureImplemented             : WordBool;
    FIrrigationAreaFeatureImplemented         : WordBool;
    FIrrigationBlockFeatureImplemented        : WordBool;
    FWetlandFeatureImplemented                : WordBool;
    FYMDemandCentreFeatureImplemented         : WordBool;
    FStreamFlowReductionFeatureImplemented    : WordBool;
    FIFRSiteFeatureImplemented                : WordBool;
    FMineFeatureImplemented                   : WordBool;
    FCurtailmentAndDroughtFeatureImplemented  : WordBool;
    FGroundWaterFeatureImplemented            : WordBool;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function  Get_PowerPlantFeatureImplemented: Wordbool; safecall;
    procedure Set_PowerPlantFeatureImplemented(Value: Wordbool);  safecall;
    function  Get_IrrigationAreaFeatureImplemented: Wordbool; safecall;
    procedure Set_IrrigationAreaFeatureImplemented(Value: Wordbool); safecall;
    function  Get_IrrigationBlockFeatureImplemented : Wordbool; safecall;
    procedure Set_IrrigationBlockFeatureImplemented (Value: Wordbool); safecall;
    function  Get_WetlandFeatureImplemented : Wordbool; safecall;
    procedure Set_WetlandFeatureImplemented (Value: Wordbool);  safecall;
    function  Get_YMDemandCentreFeatureImplemented : Wordbool; safecall;
    procedure Set_YMDemandCentreFeatureImplemented (Value: Wordbool); safecall;
    function  Get_StreamFlowReductionFeatureImplemented : Wordbool; safecall;
    procedure Set_StreamFlowReductionFeatureImplemented (Value: Wordbool) safecall;
    function  Get_IFRSiteFeatureImplemented: Wordbool; safecall;
    procedure Set_IFRSiteFeatureImplemented(Value: Wordbool); safecall;
    function  Get_MineFeatureImplemented: Wordbool; safecall;
    procedure Set_MineFeatureImplemented(Value: Wordbool); safecall;
    function  Get_CurtailmentAndDroughtFeatureImplemented: Wordbool; safecall;
    procedure Set_CurtailmentAndDroughtFeatureImplemented(Value: Wordbool); safecall;
    function  Get_GroundWaterFeatureImplemented: Wordbool; safecall;
    procedure Set_GroundWaterFeatureImplemented(Value: Wordbool); safecall;
  public
    function Initialise : Boolean; override;
    property PowerPlantFeatureImplemented: Wordbool            read  Get_PowerPlantFeatureImplemented write Set_PowerPlantFeatureImplemented;
    property IrrigationAreaFeatureImplemented: Wordbool        read  Get_IrrigationAreaFeatureImplemented write Set_IrrigationAreaFeatureImplemented;
    property IrrigationBlockFeatureImplemented: Wordbool       read  Get_IrrigationBlockFeatureImplemented write Set_IrrigationBlockFeatureImplemented;
    property WetlandFeatureImplemented: Wordbool               read  Get_WetlandFeatureImplemented write Set_WetlandFeatureImplemented;
    property YMDemandCentreFeatureImplemented: Wordbool        read  Get_YMDemandCentreFeatureImplemented write Set_YMDemandCentreFeatureImplemented;
    property StreamFlowReductionFeatureImplemented: Wordbool   read  Get_StreamFlowReductionFeatureImplemented write Set_StreamFlowReductionFeatureImplemented;
    property IFRSiteFeatureImplemented: Wordbool               read  Get_IFRSiteFeatureImplemented write Set_IFRSiteFeatureImplemented;
    property MineFeatureImplemented: Wordbool                  read  Get_MineFeatureImplemented write Set_MineFeatureImplemented;
    property CurtailmentAndDroughtFeatureImplemented: Wordbool read  Get_CurtailmentAndDroughtFeatureImplemented write Set_CurtailmentAndDroughtFeatureImplemented;
    property GroundWaterFeatureImplemented: Wordbool           read  Get_GroundWaterFeatureImplemented write Set_GroundWaterFeatureImplemented;
   end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{TImplementedNetworkFeatures}

procedure TImplementedNetworkFeatures.CreateMemberObjects;
const OPNAME = 'TImplementedNetworkFeatures.CreateMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TImplementedNetworkFeatures.DestroyMemberObjects;
const OPNAME = 'TImplementedNetworkFeatures.DestroyMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Initialise: Boolean;
const OPNAME = 'TImplementedNetworkFeatures.Initialise';
begin
  Result := inherited Initialise;
  try
    FPowerPlantFeatureImplemented            := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'PowerPlantFeatureImplemented',1));
    FIrrigationAreaFeatureImplemented        := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'IrrigationAreaFeatureImplemented',1));
    FIrrigationBlockFeatureImplemented       := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'IrrigationBlockFeatureImplemented',1));
    FWetlandFeatureImplemented               := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'WetlandFeatureImplemented',1));
    FYMDemandCentreFeatureImplemented        := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'YMDemandCentreFeatureImplemented',1));
    FStreamFlowReductionFeatureImplemented   := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'StreamFlowReductionFeatureImplemented',1));
    FIFRSiteFeatureImplemented               := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'IFRSiteFeatureImplemented',1));
    FMineFeatureImplemented                  := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'MineFeatureImplemented',1));
    FCurtailmentAndDroughtFeatureImplemented := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'CurtailmentAndDroughtFeatureImplemented',1));
    FGroundWaterFeatureImplemented           := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'GroundWaterFeatureImplemented',0));
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TImplementedNetworkFeatures.Get_PowerPlantFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_PowerPlantFeatureImplemented';
begin
  Result := False;
  try
    Result := FPowerPlantFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_PowerPlantFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_PowerPlantFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'PowerPlantFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_CurtailmentAndDroughtFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_CurtailmentAndDroughtFeatureImplemented';
begin
  Result := False;
  try
    Result := FCurtailmentAndDroughtFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_CurtailmentAndDroughtFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_CurtailmentAndDroughtFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'CurtailmentAndDroughtFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_IrrigationAreaFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_IrrigationAreaFeatureImplemented';
begin
  Result := False;
  try
    Result := FIrrigationAreaFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_IrrigationAreaFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_IrrigationAreaFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'IrrigationAreaFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_IrrigationBlockFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_IrrigationBlockFeatureImplemented';
begin
  Result := False;
  try
    Result := FIrrigationBlockFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_IrrigationBlockFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_IrrigationBlockFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'IrrigationBlockFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_WetlandFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_WetlandFeatureImplemented';
begin
  Result := False;
  try
    Result := FWetlandFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_WetlandFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_WetlandFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'WetlandFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_YMDemandCentreFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_YMDemandCentreFeatureImplemented';
begin
  Result := False;
  try
    Result := FYMDemandCentreFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_YMDemandCentreFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_YMDemandCentreFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'YMDemandCentreFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_StreamFlowReductionFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_StreamFlowReductionFeatureImplemented';
begin
  Result := False;
  try
    Result := FStreamFlowReductionFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_StreamFlowReductionFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_StreamFlowReductionFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'StreamFlowReductionFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_IFRSiteFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_IFRSiteFeatureImplemented';
begin
  Result := False;
  try
    Result := FIFRSiteFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_IFRSiteFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_IFRSiteFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'IFRSiteFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_MineFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_MineFeatureImplemented';
begin
  Result := False;
  try
    Result := FMineFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_MineFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_MineFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'MineFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImplementedNetworkFeatures.Get_GroundWaterFeatureImplemented: Wordbool;
const OPNAME = 'TImplementedNetworkFeatures.Get_GroundWaterFeatureImplemented';
begin
  Result := False;
  try
    Result := FGroundWaterFeatureImplemented;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImplementedNetworkFeatures.Set_GroundWaterFeatureImplemented(Value: Wordbool);
const OPNAME = 'TImplementedNetworkFeatures.Set_GroundWaterFeatureImplemented';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'GroundWaterFeatureImplemented', ord(Value));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
