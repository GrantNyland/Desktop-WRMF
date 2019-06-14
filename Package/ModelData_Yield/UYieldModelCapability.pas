//
//  UNIT      : Contains the class AbstractModelCalendar.
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 2004/01/20
//  COPYRIGHT : Copyright © 2003 Aravia
//
unit UYieldModelCapability;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TYieldModelCapability = class(TAbstractAppObject,IYieldModelCapability)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_MaximumNumberOfChannels: integer; safecall;
    function Get_MinimumNumberOfChannels: integer; safecall;
    function Get_MaximumChannelNumberofLossChannels: integer; safecall;
    function Get_MinimumChannelNumberofLossChannels: integer; safecall;
    function Get_MaximumChannelNumberofMinMaxChannel: integer; safecall;
    function Get_MinimumChannelNumberofMinMaxChannel: integer; safecall;
    function Get_MaximumChannelNumberofPumpingChannels: integer; safecall;
    function Get_MinimumChannelNumberofPumpingChannels: integer; safecall;
    function Get_MaximumChannelsCount: integer; safecall;
    function Get_MinimumChannelsCount: integer; safecall;
    function Get_MaximumConstraintsChannelNumber: integer; safecall;
    function Get_MinimumConstraintsChannelNumber: integer; safecall;
    function Get_MaximumControlStructCount: integer; safecall;
    function Get_MinimumControlStructCount: integer; safecall;
    function Get_MaximumControlStructureCount: integer; safecall;
    function Get_MinimumControlStructureCount: integer; safecall;
    function Get_MaximumDemandCount: integer; safecall;
    function Get_MinimumDemandCount: integer; safecall;
    function Get_MaximumDiversionChannelCount: integer; safecall;
    function Get_MinimumDiversionChannelCount: integer; safecall;
    function Get_MaximumDiversionChannelNumber: integer; safecall;
    function Get_MinimumDiversionChannelNumber: integer; safecall;
    function Get_MaximumDownStreamPowerChannelCount: integer; safecall;
    function Get_MinimumDownStreamPowerChannelCount: integer; safecall;
    function Get_MaximumGeneralCount: integer; safecall;
    function Get_MinimumGeneralCount: integer; safecall;
    function Get_MaximumIFRPointsCount: integer; safecall;
    function Get_MinimumIFRPointsCount: integer; safecall;
    function Get_MaximumInflowCount: integer; safecall;
    function Get_MinimumInflowCount: integer; safecall;
    function Get_MaximumIrrigationCount: integer; safecall;
    function Get_MinimumIrrigationCount: integer; safecall;
    function Get_MaximumLossChannelNumber: integer; safecall;
    function Get_MinimumLossChannelNumber: integer; safecall;
    function Get_MaximumLossCount: integer; safecall;
    function Get_MinimumLossCount: integer; safecall;
    function Get_MaximumMasterControlChannelNumber: integer; safecall;
    function Get_MinimumMasterControlChannelNumber: integer; safecall;
    function Get_MaximumMinFlowChannelNumber: integer; safecall;
    function Get_MinimumMinFlowChannelNumber: integer; safecall;
    function Get_MaximumMinFlowCount: integer; safecall;
    function Get_MinimumMinFlowCount: integer; safecall;
    function Get_MaximumMultiPurposeCount: integer; safecall;
    function Get_MinimumMultiPurposeCount: integer; safecall;
    function Get_MaximumPenaltyCount: integer; safecall;
    function Get_MinimumPenaltyCount: integer; safecall;
    function Get_MaximumPenaltyStructureCount: integer; safecall;
    function Get_MinimumPenaltyStructureCount: integer; safecall;
    function Get_MaximumPowerCount: integer; safecall;
    function Get_MinimumPowerCount: integer; safecall;
    function Get_MaximumPowerGenerationChannelNumber: integer; safecall;
    function Get_MinimumPowerGenerationChannelNumber: integer; safecall;
    function Get_MaximumPumpingCount: integer; safecall;
    function Get_MinimumPumpingCount: integer; safecall;
    function Get_MaximumReferenceNodeCount: integer; safecall;
    function Get_MinimumReferenceNodeCount: integer; safecall;
    function Get_MaximumReservoirCount: integer; safecall;
    function Get_MinimumReservoirCount: integer; safecall;
    function Get_MaximumSpillChannelNumber: integer; safecall;
    function Get_MinimumSpillChannelNumber: integer; safecall;
    function Get_MaximumStorageZoneCount: integer; safecall;
    function Get_MinimumStorageZoneCount: integer; safecall;
    function Get_MaximumTailWaterCount: integer; safecall;
    function Get_MinimumTailWaterCount: integer; safecall;
  end;

implementation

{ TYieldModelCapability }
uses
  SysUtils,
  UErrorHandlingOperations;

function TYieldModelCapability.Get_MaximumNumberOfChannels: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumNumberOfChannels';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumNumberOfChannels: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumNumberOfChannels';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumChannelNumberofLossChannels: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumChannelNumberofLossChannels';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumberofLossChannels');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumChannelNumberofLossChannels: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumChannelNumberofLossChannels';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumberofLossChannels');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumChannelNumberofMinMaxChannel: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumChannelNumberofMinMaxChannel';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumberofMin-MaxChannel');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumChannelNumberofMinMaxChannel: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumChannelNumberofMinMaxChannel';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumberofMin-MaxChannel');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumChannelNumberofPumpingChannels: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumChannelNumberofPumpingChannels';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumberofPumpingChannels');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumChannelNumberofPumpingChannels: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumChannelNumberofPumpingChannels';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelNumberofPumpingChannels');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumChannelsCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumChannelsCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelsCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumChannelsCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumChannelsCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelsCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumConstraintsChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumConstraintsChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintsChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumConstraintsChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumConstraintsChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintsChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumControlStructCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumControlStructCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ControlStructCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumControlStructCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumControlStructCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ControlStructCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumControlStructureCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumControlStructureCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ControlStructureCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumControlStructureCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumControlStructureCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ControlStructureCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumDemandCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumDemandCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DemandCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumDemandCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumDemandCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DemandCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumDiversionChannelCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumDiversionChannelCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionChannelCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumDiversionChannelCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumDiversionChannelCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionChannelCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumDiversionChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumDiversionChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumDiversionChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumDiversionChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumDownStreamPowerChannelCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumDownStreamPowerChannelCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DownStreamPowerChannelCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumDownStreamPowerChannelCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumDownStreamPowerChannelCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('DownStreamPowerChannelCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumGeneralCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumGeneralCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('GeneralCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumGeneralCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumGeneralCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('GeneralCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumIFRPointsCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumIFRPointsCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IFRPointsCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumIFRPointsCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumIFRPointsCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IFRPointsCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumInflowCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumInflowCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('InflowCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumInflowCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumInflowCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('InflowCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumIrrigationCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumIrrigationCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumIrrigationCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumIrrigationCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumLossChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumLossChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('LossFeatureChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumLossChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumLossChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('LossFeatureChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumLossCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumLossCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('LossCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumLossCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumLossCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('LossCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumMasterControlChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumMasterControlChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MasterControlChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumMasterControlChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumMasterControlChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MasterControlChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumMinFlowChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumMinFlowChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MinFlowChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumMinFlowChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumMinFlowChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MinFlowChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumMinFlowCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumMinFlowCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MinFlowCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumMinFlowCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumMinFlowCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MinFlowCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumMultiPurposeCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumMultiPurposeCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MultiPurposeCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumMultiPurposeCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumMultiPurposeCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('MultiPurposeCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumPenaltyCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumPenaltyCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumPenaltyCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumPenaltyCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumPenaltyStructureCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumPenaltyStructureCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStructureCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumPenaltyStructureCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumPenaltyStructureCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStructureCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumPowerCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumPowerCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PowerCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumPowerCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumPowerCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PowerCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumPowerGenerationChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumPowerGenerationChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PowerGenerationChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumPowerGenerationChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumPowerGenerationChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PowerGenerationChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumPumpingCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumPumpingCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PumpingCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumPumpingCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumPumpingCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PumpingCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumReferenceNodeCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumReferenceNodeCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ReferenceNodeCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumReferenceNodeCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumReferenceNodeCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ReferenceNodeCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumReservoirCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumReservoirCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumReservoirCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumReservoirCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumSpillChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumSpillChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('SpillChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumSpillChannelNumber: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumSpillChannelNumber';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('SpillChannelNumber');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumStorageZoneCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumStorageZoneCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('StorageZoneCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumStorageZoneCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumStorageZoneCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('StorageZoneCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MaximumTailWaterCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MaximumTailWaterCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('TailWaterCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability.Get_MinimumTailWaterCount: integer;
const OPNAME = 'TYieldModelCapability.Get_MinimumTailWaterCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('TailWaterCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMinimumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability._AddRef: Integer;
const OPNAME = 'TYieldModelCapability._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCapability._Release: Integer;
const OPNAME = 'TYieldModelCapability._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.


