
//
//
//  UNIT      : Contains TChannelPlanningData Class
//  AUTHOR    : Presley Mudau
//  DATE      : 2006/03/03
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UChannelPlanningData;

interface

uses
  Contnrs,
  Classes,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TChannelSwitchControl = class(TAbstractAppObject, IChannelSwitchControl)
  protected
    FChannelSwitchID       : integer;
    FSwitchDefinitionID    : integer;
    FChannelNumber         : integer;
    FAssociatedNodeNr      : integer;
    FWaterlevel            : double;
    FSwitchType            : integer;
    FInitialStatus         : integer;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateSwitchDefinitionID (AErrorMessages : TStrings) : WordBool;
    function ValidateAssociatedNodeNr (AErrorMessages : TStrings) : WordBool;
    function ValidateWaterLevel (AErrorMessages : TStrings) : WordBool;
    function ValidateSwitchType (AErrorMessages : TStrings) : WordBool;
    function ValidateInitialStatus (AErrorMessages : TStrings) : WordBool;
  public
    function Initialise: boolean; override;
    function Populate (AChannelSwitchID    : integer;
                       AChannelNumber      : integer;
                       ASwitchDefinitionID : integer;
                       AAssociatedNodeNr   : integer;
                       AWaterLevel         : double;
                       ASwitchType         : integer;
                       AInitialStatus      : integer) : Boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Get_ChannelSwitchID: Integer; safecall;
    function Get_SwitchDefinitionID: Integer; safecall;
    procedure Set_SwitchDefinitionID(Value: Integer); safecall;
    function Get_AssociatedNodeNr: Integer; safecall;
    procedure Set_AssociatedNodeNr(Value: Integer); safecall;
    function Get_WaterLevel: Double; safecall;
    procedure Set_WaterLevel(Value: Double); safecall;
    function Get_SwitchType: Integer; safecall;
    procedure Set_SwitchType(Value: Integer); safecall;
    function Get_InitialStatus: Integer; safecall;
    procedure Set_InitialStatus(Value: Integer); safecall;

    property ChannelNumber      : Integer read Get_ChannelNumber;
    property ChannelSwitchID    : Integer read Get_ChannelSwitchID;
    property SwitchDefinitionID : Integer read Get_SwitchDefinitionID  write Set_SwitchDefinitionID;
    property AssociatedNodeNr   : Integer read Get_AssociatedNodeNr write Set_AssociatedNodeNr;
    property WaterLevel         : Double  read Get_WaterLevel       write Set_WaterLevel;
    property SwitchType         : Integer read Get_SwitchType       write Set_SwitchType;
    property InitialStatus      : Integer read Get_InitialStatus    write Set_InitialStatus;
  end;

  TChannelTimeControl = class(TAbstractAppObject,IChannelTimeControl)
  protected
    FIdentifier              : integer;
    FChannelNumber           : integer;
    FChannelStartYear        : integer;
    FChannelStartMonth       : integer;
    FChannelEndYear          : integer;
    FChannelEndMonth         : integer;
    FChannelEconomicLife     : integer;
    FChannelCapitalCost      : double;
    FChannelFixedOMCost      : double;
    FChannelVariableOMCost   : double;
    FChannelCostSchedule     : WideString;
    FChannelEscalationCost   : WideString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateStartYear (AErrorMessages : TStrings) : WordBool;
    function ValidateStartMonth (AErrorMessages : TStrings) : WordBool;
    function ValidateEndYear (AErrorMessages : TStrings) : WordBool;
    function ValidateEndMonth (AErrorMessages : TStrings) : WordBool;
    function ValidateEconomicLife (AErrorMessages : TStrings) : WordBool;
    function ValidateCapitalCost (AErrorMessages : TStrings) : WordBool;
    function ValidateFixedOMCost (AErrorMessages : TStrings) : WordBool;
    function ValidateVariableOMCost (AErrorMessages : TStrings) : WordBool;
    function ValidateYearsToConstruct (AErrorMessages : TStrings) : WordBool;
    function ValidateCostSchedule (AErrorMessages : TStrings): WordBool;
    function ValidateYearsInAnalysis (AErrorMessages : TStrings) : WordBool;
    function ValidateEscalationCost (AErrorMessages : TStrings): WordBool;
  public
    function Initialise: boolean; override;
    function Populate ( AIdentifier                 : integer;
                        AChannelNumber              : integer;
                        AChannelStartYear           : integer;
                        AChannelStartMonth          : integer;
                        AChannelEndYear             : integer;
                        AChannelEndMonth            : integer;
                        AChannelEconomicLife        : integer;
                        AChannelCapitalCost         : double;
                        AChannelFixedOMCost         : double;
                        AChannelVariableOMCost      : double;
                        AChannelChannelCostSchedule : string;
                        AChannelEscalationCost      : string ) : Boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(Value: Integer); safecall;
    function Get_EndYear: Integer; safecall;
    procedure Set_EndYear(Value: Integer); safecall;
    function Get_EndMonth: Integer; safecall;
    procedure Set_EndMonth(Value: Integer); safecall;
    function Get_EconomicLife: Integer; safecall;
    procedure Set_EconomicLife(Value: Integer); safecall;
    function Get_CapitalCost: Double; safecall;
    procedure Set_CapitalCost(Value: Double); safecall;
    function Get_FixedOMCost: Double; safecall;
    procedure Set_FixedOMCost(Value: Double); safecall;
    function Get_VariableOMCost: Double; safecall;
    procedure Set_VariableOMCost(Value: Double); safecall;

    function Get_YearsToConstruct: Integer; safecall;
    procedure Set_YearsToConstruct(Value: Integer); safecall;
    function Get_CostSchedule: WideString; safecall;
    procedure Set_CostSchedule(const Value: WideString); safecall;
    function Get_CostScheduleByIndex (AIndex : integer) : double; safecall;
    procedure Set_CostScheduleByIndex (AIndex : integer;
                                       AValue : double); safecall;

    function Get_YearsInAnalysis: Integer; safecall;
    procedure Set_YearsInAnalysis(Value: Integer); safecall;
    function Get_EscalationCost: WideString; safecall;
    procedure Set_EscalationCost(const Value: WideString); safecall;
    function Get_EscalationCostValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_EscalationCostValueByIndex(AIndex: Integer; AValue: Double); safecall;

    property Identifier: Integer read FIdentifier;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property YearsInAnalysis: Integer read Get_YearsInAnalysis write Set_YearsInAnalysis;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;
    property EndYear: Integer read Get_EndYear write Set_EndYear;
    property EndMonth: Integer read Get_EndMonth write Set_EndMonth;
    property EconomicLife: Integer read Get_EconomicLife write Set_EconomicLife;
    property CapitalCost: Double read Get_CapitalCost write Set_CapitalCost;
    property FixedOMCost: Double read Get_FixedOMCost write Set_FixedOMCost;
    property VariableOMCost: Double read Get_VariableOMCost write Set_VariableOMCost;
    property YearsToConstruct: Integer read Get_YearsToConstruct write Set_YearsToConstruct;
    property CostSchedule: WideString read Get_CostSchedule write Set_CostSchedule;
    property CostScheduleByIndex[AIndex : integer] : double read Get_CostScheduleByIndex write Set_CostScheduleByIndex;
    property EscalationCost: WideString read Get_EscalationCost write Set_EscalationCost;
  end;
implementation

uses
  Math,
  VCL.Controls,
  VCL.Dialogs,
  SysUtils,
  UUtilities,
  UConstants,
  UParameterData,
  UAbstractFileNamesObject,
  UMainMenuEventType,
  UYieldModelDataObject,
  UChannelPlanningLoadAgent,
  UErrorHandlingOperations, UFileNames, DB;

{******************************************************************************}
{* TChannelTimeControl                                                        *}
{******************************************************************************}

procedure TChannelTimeControl.CreateMemberObjects;
const OPNAME = 'TChannelTimeControl.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.DestroyMemberObjects;
const OPNAME = 'TChannelTimeControl.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl._AddRef: Integer;
const OPNAME = 'TChannelTimeControl._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl._Release: Integer;
const OPNAME = 'TChannelTimeControl._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateStartYear(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateStartYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelStartYear',
            IntToStr(FChannelStartYear), lMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelStartYear)+ ':'+lMessage)
    else
    if (StartYear > EndYear) then
      AErrorMessages.Add('ERROR:'+FAppModules.Language.GetString('ContextValidation.InvalidDate'))
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateStartMonth(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateStartMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelStartMonth',
            IntToStr(FChannelStartMonth), lMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelStartMonth)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateEndYear(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateEndYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelEndYear',
            IntToStr(FChannelEndYear), lMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelEndYear)+ ':'+lMessage)
    else
    if (EndYear < StartYear) then
      AErrorMessages.Add('ERROR:'+FAppModules.Language.GetString('ContextValidation.InvalidDate'))
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateEndMonth(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateEndMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelEndMonth',
            IntToStr(FChannelEndMonth), lMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelEndMonth)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateEconomicLife(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateEconomicLife';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelEconomicLife',
            IntToStr(FChannelEconomicLife), lMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelEconomicLife)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateCapitalCost(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateCapitalCost';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelCapitalCost',
            FloatToStr(FChannelCapitalCost), lMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FChannelCapitalCost)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateFixedOMCost(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateFixedOMCost';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelFixedOMCost',
            FloatToStr(FChannelFixedOMCost), lMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FChannelFixedOMCost)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateVariableOMCost(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateVariableOMCost';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelVariableOMCost',
            FloatToStr(FChannelVariableOMCost), lMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FChannelVariableOMCost)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateYearsToConstruct(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateYearsToConstruct';
var
  lMessage          : string;
  LYearsToConstruct : integer;
begin
  Result := FALSE;
  try
    lMessage := '';
    LYearsToConstruct := YearsToConstruct;
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelYearsToConstruct',
            IntToStr(LYearsToConstruct), lMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(LYearsToConstruct)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateCostSchedule(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateCostSchedule';var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelCostSchedule',
            FChannelCostSchedule, lMessage)) then
      AErrorMessages.Add('ERROR:'+FChannelCostSchedule+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateYearsInAnalysis(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateYearsInAnalysis';
var
  lMessage                : string;
  LChannelYearsInAnalysis : integer;
begin
  Result := FALSE;
  try
    lMessage := '';
    LChannelYearsInAnalysis := YearsInAnalysis;
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelYearsInAnalysis',
            IntToStr(LChannelYearsInAnalysis), lMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(LChannelYearsInAnalysis)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.ValidateEscalationCost(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTimeControl.ValidateEscalationCost';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelEscalationCost',
            FChannelEscalationCost, lMessage)) then
      AErrorMessages.Add('ERROR:'+FChannelEscalationCost+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TChannelTimeControl.Initialise: boolean;
const OPNAME = 'TChannelTimeControl.Initialise';
begin
  Result := FALSE;
  try
    FIdentifier              := 0;
    FChannelNumber           := 0;
    FChannelStartYear        := 0;
    FChannelStartMonth       := 0;
    FChannelEndYear          := 0;
    FChannelEndMonth         := 0;
    FChannelEconomicLife     := 0;
    FChannelCapitalCost      := 0.0;
    FChannelFixedOMCost      := 0.0;
    FChannelVariableOMCost   := 0.0;
    FChannelCostSchedule     := '';
    FChannelEscalationCost   := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Populate(AIdentifier                 : integer;
                                      AChannelNumber              : integer;
                                      AChannelStartYear           : integer;
                                      AChannelStartMonth          : integer;
                                      AChannelEndYear             : integer;
                                      AChannelEndMonth            : integer;
                                      AChannelEconomicLife        : integer;
                                      AChannelCapitalCost         : double;
                                      AChannelFixedOMCost         : double;
                                      AChannelVariableOMCost      : double;
                                      AChannelChannelCostSchedule : string;
                                      AChannelEscalationCost      : string): Boolean;
const OPNAME = 'TChannelTimeControl.Populate';
begin
  Result := FALSE;
  try
    FIdentifier              := AIdentifier;
    FChannelNumber           := AChannelNumber;
    FChannelStartYear        := AChannelStartYear;
    FChannelStartMonth       := AChannelStartMonth;
    FChannelEndYear          := AChannelEndYear;
    FChannelEndMonth         := AChannelEndMonth;
    FChannelEconomicLife     := AChannelEconomicLife;
    FChannelCapitalCost      := AChannelCapitalCost;
    FChannelFixedOMCost      := AChannelFixedOMCost;
    FChannelVariableOMCost   := AChannelVariableOMCost;
    FChannelCostSchedule     := AChannelChannelCostSchedule;
    FChannelEscalationCost   := AChannelEscalationCost;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_ChannelNumber: Integer;
const OPNAME = 'TChannelTimeControl.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_YearsInAnalysis: Integer;
const OPNAME = 'TChannelTimeControl.Get_YearsInAnalysis';
begin
  Result := 0;
  try
    Result :=  StringsItemsCount(FChannelEscalationCost);;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_YearsInAnalysis(Value: Integer);
const OPNAME = 'TChannelTimeControl.Set_YearsInAnalysis';
var
  LTempStr     : string;
begin
  try
    if(Value < 0) then Exit;
    LTempStr := FChannelEscalationCost;
    if ChangeSizeCommatextString(Value,'0.0',LTempStr) then
      EscalationCost := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_StartYear: Integer;
const OPNAME = 'TChannelTimeControl.Get_StartYear';
begin
  Result := 0;
  try
    Result := FChannelStartYear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_StartYear(Value: Integer);
const OPNAME = 'TChannelTimeControl.Set_StartYear';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelStartYear', IntToStr(Value),IntToStr(FChannelStartYear), LContextData) then
        begin
          FChannelStartYear := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelStartYear', IntToStr(FChannelStartYear),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_StartMonth: Integer;
const OPNAME = 'TChannelTimeControl.Get_StartMonth';
begin
  Result := 0;
  try
    Result := FChannelStartMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_StartMonth(Value: Integer);
const OPNAME = 'TChannelTimeControl.Set_StartMonth';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelStartMonth', IntToStr(Value),IntToStr(FChannelStartMonth), LContextData) then
        begin
          FChannelStartMonth := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelStartMonth', IntToStr(FChannelStartMonth),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_EndYear: Integer;
const OPNAME = 'TChannelTimeControl.Get_EndYear';
begin
  Result := 0;
  try
    Result := FChannelEndYear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_EndYear(Value: Integer);
const OPNAME = 'TChannelTimeControl.Set_EndYear';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelEndYear', IntToStr(Value),IntToStr(FChannelEndYear), LContextData) then
        begin
          FChannelEndYear := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelEndYear', IntToStr(FChannelEndYear),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_EndMonth: Integer;
const OPNAME = 'TChannelTimeControl.Get_EndMonth';
begin
  Result := 0;
  try
    Result := FChannelEndMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_EndMonth(Value: Integer);
const OPNAME = 'TChannelTimeControl.Set_EndMonth';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelEndMonth', IntToStr(Value),IntToStr(FChannelEndMonth), LContextData) then
        begin
          FChannelEndMonth := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelEndMonth', IntToStr(FChannelEndMonth),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_EconomicLife: Integer;
const OPNAME = 'TChannelTimeControl.Get_EconomicLife';
begin
  Result := 0;
  try
    Result := FChannelEconomicLife;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_EconomicLife(Value: Integer);
const OPNAME = 'TChannelTimeControl.Set_EconomicLife';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelEconomicLife', IntToStr(Value),IntToStr(FChannelEconomicLife), LContextData) then
        begin
          FChannelEconomicLife := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelEconomicLife', IntToStr(FChannelEconomicLife),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_CapitalCost: Double;
const OPNAME = 'TChannelTimeControl.Get_CapitalCost';
begin
  Result := 0.0;
  try
    Result := FChannelCapitalCost;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_CapitalCost(Value: Double);
const OPNAME = 'TChannelTimeControl.Set_CapitalCost';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelCapitalCost', FloatToStr(Value),FloatToStr(FChannelCapitalCost), LContextData) then
        begin
          FChannelCapitalCost := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelCapitalCost', FloatToStr(FChannelCapitalCost),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_FixedOMCost: Double;
const OPNAME = 'TChannelTimeControl.Get_FixedOMCost';
begin
  Result := 0.0;
  try
    Result := FChannelFixedOMCost;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_FixedOMCost(Value: Double);
const OPNAME = 'TChannelTimeControl.Set_FixedOMCost';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelFixedOMCost', FloatToStr(Value),FloatToStr(FChannelFixedOMCost), LContextData) then
        begin
          FChannelFixedOMCost := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelFixedOMCost', FloatToStr(FChannelFixedOMCost),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_VariableOMCost: Double;
const OPNAME = 'TChannelTimeControl.Get_VariableOMCost';
begin
  Result := 0.0;
  try
    Result := FChannelVariableOMCost;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_VariableOMCost(Value: Double);
const OPNAME = 'TChannelTimeControl.Set_VariableOMCost';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelVariableOMCost', FloatToStr(Value),FloatToStr(FChannelVariableOMCost), LContextData) then
        begin
          FChannelVariableOMCost := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelVariableOMCost', FloatToStr(FChannelVariableOMCost),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_YearsToConstruct: Integer;
const OPNAME = 'TChannelTimeControl.Get_YearsToConstruct';
begin
  Result := 0;
  try
    Result := StringsItemsCount(FChannelCostSchedule);;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_YearsToConstruct(Value: Integer);
const OPNAME = 'TChannelTimeControl.Set_YearsToConstruct';
var
  LTempStr     : string;
begin
  try
    if(Value < 0) then Exit;
    LTempStr := FChannelCostSchedule;
    if ChangeSizeCommatextString(Value,'0.0',LTempStr) then
      CostSchedule := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_CostSchedule: WideString;
const OPNAME = 'TChannelTimeControl.Get_CostSchedule';
begin
  Result := '';
  try
    Result := UnCompressCommatextString(FChannelCostSchedule);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_CostSchedule(const Value: WideString);
const OPNAME = 'TChannelTimeControl.Set_CostSchedule';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LTempStr     : string;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LContextData.Clear;
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        begin
          LTempStr := CompressCommatextString(Value);
          if FAppModules.FieldProperties.UpdateFieldValue(
               'ChannelCostSchedule', LTempStr, FChannelCostSchedule, LContextData) then
          begin
            LOldValue := FChannelCostSchedule;
            FChannelCostSchedule := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelCostSchedule',FChannelCostSchedule,LTempStr);
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_CostScheduleByIndex (AIndex : integer) : double;
const OPNAME = 'TChannelTimeControl.Get_CostScheduleByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FChannelCostSchedule);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_CostScheduleByIndex (AIndex : integer;
                                                       AValue : double);
const OPNAME = 'TChannelTimeControl.Set_CostScheduleByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FChannelCostSchedule;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      CostSchedule := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_EscalationCost: WideString;
const OPNAME = 'TChannelTimeControl.Get_EscalationCost';
begin
  Result := '';
  try
    Result := UnCompressCommatextString(FChannelEscalationCost);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_EscalationCost(const Value: WideString);
const OPNAME = 'TChannelTimeControl.Set_EscalationCost';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
  LTempStr     : string;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LContextData.Clear;
        LLoadAgent.LoadContextData_ChannelTimeControl(LContextData, IntToStr(FIdentifier), IntToStr(FChannelNumber));
        begin
          LTempStr := CompressCommatextString(Value);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelEscalationCost', LTempStr, FChannelEscalationCost, LContextData) then
          begin
            FChannelEscalationCost := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelEscalationCost',FChannelEscalationCost,LTempStr);
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Get_EscalationCostValueByIndex (AIndex : integer): Double;
const OPNAME = 'TChannelTimeControl.Get_EscalationCostValueByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FChannelEscalationCost);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTimeControl.Set_EscalationCostValueByIndex (AIndex : integer;
                                                              AValue : Double);
const OPNAME = 'TChannelTimeControl.Set_EscalationCostValueByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FChannelEscalationCost;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      EscalationCost := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelTimeControl.Validate (var AErrors    : WideString;
                                       const AContext : WideString): WordBool;
const OPNAME = 'TChannelTimeControl.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'ChannelStartYear') then
        Result := ValidateStartYear(lErrorList)
      else
      if (AContext = 'ChannelStartMonth') then
        Result := ValidateStartMonth(lErrorList)
      else
      if (AContext = 'ChannelEndYear') then
        Result := ValidateEndYear(lErrorList)
      else
      if (AContext = 'ChannelEndMonth') then
         Result := ValidateEndMonth(lErrorList)
      else
      if (AContext = 'ChannelEconomicLife') then
        Result := ValidateEconomicLife(lErrorList)
      else
      if (AContext = 'ChannelCapitalCost') then
        Result := ValidateCapitalCost(lErrorList)
      else
      if (AContext = 'ChannelFixedOMCost') then
        Result := ValidateFixedOMCost(lErrorList)
      else
      if (AContext = 'ChannelVariableOMCost') then
        Result := ValidateVariableOMCost(lErrorList)
      else
      if (AContext = 'ChannelYearsToConstruct') then
        Result := ValidateYearsToConstruct(lErrorList)
      else
      if (AContext = 'ChannelCostSchedule') then
         Result := ValidateCostSchedule(lErrorList)
      else
      if (AContext = 'ChannelYearsInAnalysis') then
        Result := ValidateYearsInAnalysis(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateStartYear(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateStartMonth(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateEndYear(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateEndMonth(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateEconomicLife(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCapitalCost(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateFixedOMCost(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateVariableOMCost(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateYearsToConstruct(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCostSchedule(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateYearsInAnalysis(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TChannelSwitchControl }

function TChannelSwitchControl._AddRef: Integer;
const OPNAME = 'TChannelSwitchControl._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl._Release: Integer;
const OPNAME = 'TChannelSwitchControl._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Initialise: boolean;
const OPNAME = 'TChannelSwitchControl.Initialise';
begin
  Result := FALSE;
  try
    FChannelSwitchID    := 0;
    FSwitchDefinitionID := 0;
    FChannelNumber      := 0;
    FAssociatedNodeNr   := 0;
    FWaterlevel         := 0.0;
    FSwitchType         := 0;
    FInitialStatus      := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Populate (AChannelSwitchID    : integer;
                                         AChannelNumber      : integer;
                                         ASwitchDefinitionID : integer;
                                         AAssociatedNodeNr   : integer;
                                         AWaterLevel         : double;
                                         ASwitchType         : integer;
                                         AInitialStatus      : integer): Boolean;
const OPNAME = 'TChannelSwitchControl.Populate';
begin
  Result := FALSE;
  try
    FChannelSwitchID    := AChannelSwitchID;
    FSwitchDefinitionID := ASwitchDefinitionID;
    FChannelNumber      := AChannelNumber;
    FAssociatedNodeNr   := AAssociatedNodeNr;
    FWaterlevel         := AWaterLevel;
    FSwitchType         := ASwitchType;
    FInitialStatus      := AInitialStatus;
    Result              := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TChannelSwitchControl.Get_ChannelNumber: Integer;
const OPNAME = 'TChannelSwitchControl.Get_ChannelNumber';
begin
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Get_ChannelSwitchID: Integer;
const OPNAME = 'TChannelSwitchControl.Get_ChannelSwitchID';
begin
  try
    Result := FChannelSwitchID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Get_SwitchDefinitionID: Integer;
const OPNAME = 'TChannelSwitchControl.Get_SwitchDefinitionID';
begin
  try
    Result := FSwitchDefinitionID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelSwitchControl.Set_SwitchDefinitionID(Value: Integer);
const OPNAME = 'TChannelSwitchControl.Set_SwitchDefinitionID';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelSwitchControl(LContextData, IntToStr(FChannelSwitchID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SwitchDefinitionID', IntToStr(Value),IntToStr(FSwitchDefinitionID), LContextData) then
        begin
          FSwitchDefinitionID := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SwitchDefinitionID', IntToStr(FSwitchDefinitionID),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Get_AssociatedNodeNr: Integer;
const OPNAME = 'TChannelSwitchControl.Get_AssociatedNodeNr';
begin
  try
    Result := FAssociatedNodeNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelSwitchControl.Set_AssociatedNodeNr(Value: Integer);
const OPNAME = 'TChannelSwitchControl.Set_AssociatedNodeNr';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelSwitchControl(LContextData, IntToStr(FChannelSwitchID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SwitchAssociatedNodeNr', IntToStr(Value),IntToStr(FAssociatedNodeNr), LContextData) then
        begin
          FAssociatedNodeNr := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SwitchAssociatedNodeNr', IntToStr(FAssociatedNodeNr),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TChannelSwitchControl.Get_WaterLevel: Double;
const OPNAME = 'TChannelSwitchControl.Get_WaterLevel';
begin
  try
    Result := FWaterlevel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelSwitchControl.Set_WaterLevel(Value: Double);
const OPNAME = 'TChannelSwitchControl.Set_WaterLevel';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelSwitchControl(LContextData, IntToStr(FChannelSwitchID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SwitchWaterlevel', FloatToStr(Value), FloatToStr(FWaterlevel), LContextData) then
        begin
          FWaterlevel := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SwitchWaterlevel', FloatToStr(FWaterlevel), FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Get_SwitchType: Integer;
const OPNAME = 'TChannelSwitchControl.Get_SwitchType';
begin
  try
    Result := FSwitchType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelSwitchControl.Set_SwitchType(Value: Integer);
const OPNAME = 'TChannelSwitchControl.Set_SwitchType';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelSwitchControl(LContextData, IntToStr(FChannelSwitchID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SwitchType', IntToStr(Value), IntToStr(FSwitchType), LContextData) then
        begin
          FSwitchType := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SwitchType', IntToStr(FSwitchType), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Get_InitialStatus: Integer;
const OPNAME = 'TChannelSwitchControl.Get_InitialStatus';
begin
  try
    Result := FInitialStatus;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelSwitchControl.Set_InitialStatus(Value: Integer);
const OPNAME = 'TChannelSwitchControl.Set_InitialStatus';
var
  LLoadAgent   : TChannelPlanningLoadAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelSwitchControl(LContextData, IntToStr(FChannelSwitchID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SwitchInitialStatus', IntToStr(Value), IntToStr(FInitialStatus), LContextData) then
        begin
          FInitialStatus := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SwitchInitialStatus', IntToStr(FInitialStatus), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.Validate (var AErrors    : WideString;
                                         const AContext : WideString): WordBool;
const OPNAME = 'TChannelSwitchControl.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'SwitchDefinitionID') then
        Result := ValidateSwitchDefinitionID(lErrorList)
      else
      if (AContext = 'SwitchAssociatedNodeNr') then
        Result := ValidateAssociatedNodeNr(lErrorList)
      else
      if (AContext = 'SwitchWaterlevel') then
        Result := ValidateWaterLevel(lErrorList)
      else
      if (AContext = 'SwitchType') then
         Result := ValidateSwitchType(lErrorList)
      else
      if (AContext = 'SwitchInitialStatus') then
        Result := ValidateInitialStatus(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateSwitchDefinitionID(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAssociatedNodeNr(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateWaterLevel(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSwitchType(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateInitialStatus(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControl.ValidateAssociatedNodeNr(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelSwitchControl.ValidateAssociatedNodeNr';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SwitchAssociatedNodeNr',
            IntToStr(FAssociatedNodeNr), lMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FAssociatedNodeNr) + ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.ValidateInitialStatus(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelSwitchControl.ValidateInitialStatus';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SwitchInitialStatus',
            IntToStr(FInitialStatus), lMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FInitialStatus) + ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.ValidateSwitchDefinitionID(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelSwitchControl.ValidateSwitchDefinitionID';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SwitchDefinitionID',
            IntToStr(FSwitchDefinitionID), lMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FSwitchDefinitionID) + ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.ValidateSwitchType(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelSwitchControl.ValidateSwitchType';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SwitchType',
            IntToStr(FSwitchType), lMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FSwitchType) + ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelSwitchControl.ValidateWaterLevel(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelSwitchControl.ValidateWaterLevel';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SwitchWaterlevel',
            FloatToStr(FWaterlevel), lMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FWaterlevel) + ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

