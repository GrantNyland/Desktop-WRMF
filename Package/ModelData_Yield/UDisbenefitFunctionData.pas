{******************************************************************************}
{*  UNIT      : Contains the class TDisbenefitFunctionData                    *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/06/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UDisbenefitFunctionData;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Disbenefit Function Data                                                   *}
{******************************************************************************}

  TDisbenefitFunctionData = class(TAbstractAppObject, IDisbenefitFunctionDefinition)
  protected
    FFeatureID                   : integer;
    FChannelNumber               : integer;
    FNrOfEconomicYears           : integer;
    FEquationDisbenefitX         : Double;
    FEquationDisbenefitY         : Double;
    FEquationDisbenefitNonSupply : Double;
    FEquationDisbenefitCost      : Double;
    FEscalationRate              : WideString;

    FYearActive                  : integer;
    FMonthActive                 : integer;
    FYearObsolete                : integer;
    FMonthObsolete               : integer;
    FWQConstraint                : double;
    FTDSConcentrationFactors     : array[1..4] of double;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateNrOfEconomicYears(AErrorMessages: TStrings): WordBool;
    function ValidateEquationDisbenefitX(AErrorMessages: TStrings): WordBool;
    function ValidateEquationDisbenefitY(AErrorMessages: TStrings): WordBool;
    function ValidateEquationDisbenefitNonSupply(AErrorMessages: TStrings): WordBool;
    function ValidateEquationDisbenefitCost(AErrorMessages: TStrings): WordBool;

    function ValidateYearDemandChannelActive(AErrorMessages: TStrings): WordBool;
    function ValidateMonthDemandChannelActive(AErrorMessages: TStrings): WordBool;
    function ValidateYearDemandChannelObsolete(AErrorMessages: TStrings): WordBool;
    function ValidateMonthDemandChannelObsolete(AErrorMessages: TStrings): WordBool;
    function ValidateWaterQualityConstraint(AErrorMessages: TStrings): WordBool;
    function ValidateTDSConcentration(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;


  public
    function Initialise : boolean; override;
    function Get_FeatureID: Integer; safecall;
    function Get_NrOfEconomicYears: Integer; safecall;
    procedure Set_NrOfEconomicYears(Value: Integer); safecall;
    function Get_EquationDisbenefitX: Double; safecall;
    procedure Set_EquationDisbenefitX(Value: Double); safecall;
    function Get_EquationDisbenefitY: Double; safecall;
    procedure Set_EquationDisbenefitY(Value: Double); safecall;
    function Get_EquationDisbenefitCost: Double; safecall;
    procedure Set_EquationDisbenefitCost(Value: Double); safecall;
    function Get_EquationDisbenefitNonSupply: Double; safecall;
    procedure Set_EquationDisbenefitNonSupply(Value: Double); safecall;
    function Get_EscalationRate: WideString; safecall;
    procedure Set_EscalationRate(const Value: WideString); safecall;
    function Get_EscalationRateByIndex(AIndex: Integer): Double; safecall;
    procedure Set_EscalationRateByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_YearActive: Integer; safecall;
    procedure Set_YearActive(Value: Integer); safecall;
    function Get_MonthActive: Integer; safecall;
    procedure Set_MonthActive(Value: Integer); safecall;
    function Get_YearObsolete: Integer; safecall;
    procedure Set_YearObsolete(Value: Integer); safecall;
    function Get_MonthObsolete: Integer; safecall;
    procedure Set_MonthObsolete(Value: Integer); safecall;
    function Get_WQConstraint: Double; safecall;
    procedure Set_WQConstraint(Value: Double); safecall;
    function Get_TDSConcentrationByIndex(Aindex: Integer): Double; safecall;
    procedure Set_TDSConcentrationByIndex(Aindex: Integer; Value: Double); safecall;
    function Get_EscalationFactors: WideString; safecall;
    procedure Set_EscalationFactors(const Value: WideString); safecall;

    function Populate (AChannelNumber               : integer;
                       AEquationDisbenefitX         : double;
                       AEquationDisbenefitY         : double;
                       AEquationDisbenefitNonSupply : double;
                       AEquationDisbenefitCost      : double;
                       AEscalationRate              : string;
                       AYearActive,AMonthActive,
                       AYearObsolete,AMonthObsolete : integer;
                       AWQConstraint : double; ATDSConcentrationFactors : array of double): Boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property NrOfEconomicYears: Integer  read Get_NrOfEconomicYears write Set_NrOfEconomicYears;
    property EquationDisbenefitX: Double read Get_EquationDisbenefitX
                                         write Set_EquationDisbenefitX;
    property EquationDisbenefitY: Double read Get_EquationDisbenefitY
                                         write Set_EquationDisbenefitY;
    property EquationDisbenefitNonSupply: Double read Get_EquationDisbenefitNonSupply
                                                 write Set_EquationDisbenefitNonSupply;
    property EquationDisbenefitCost: Double read Get_EquationDisbenefitCost
                                            write Set_EquationDisbenefitCost;
    property EscalationRate : Widestring read Get_EscalationRate
                                         write Set_EscalationRate;
    property FeatureID: Integer read FFeatureID;
    property EscalationRateByIndex[AIndex: Integer]: Double read Get_EscalationRateByIndex
                                                            write Set_EscalationRateByIndex;


  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UUtilities,
  UDisbenefitFunctionDefinitionSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{TUDisbenefitFunctionData}


procedure TDisbenefitFunctionData.CreateMemberObjects;
const OPNAME = 'TDisbenefitFunctionData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.DestroyMemberObjects;
const OPNAME = 'TDisbenefitFunctionData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{ DisbenefitFunctionDefinitionData                                                        }
{******************************************************************************}

function TDisbenefitFunctionData._Release: Integer;
const OPNAME = 'TDisbenefitFunctionData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDisbenefitFunctionData.Get_FeatureID: Integer;
const OPNAME = 'TDisbenefitFunctionData.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.Get_NrOfEconomicYears: Integer;
const OPNAME = 'TDisbenefitFunctionData.Get_NrOfEconomicYears';
begin
  Result := 0;
  try
    FNrOfEconomicYears :=  StringsItemsCount(FEscalationRate);
    Result := FNrOfEconomicYears;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_NrOfEconomicYears(Value: Integer);
const OPNAME = 'TDisbenefitFunctionData.Set_NrOfEconomicYears';
var
  LTempStr     : string;
begin
  try
    if(Value < 0) then Exit;
    LTempStr := FEscalationRate;
    if ChangeSizeCommatextString(Value,'0.0',LTempStr) then
      EscalationRate := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.Get_EquationDisbenefitX: Double;
const OPNAME = 'TDisbenefitFunctionData.Get_EquationDisbenefitX';
begin
  Result := 0.0;
  try
    Result := FEquationDisbenefitX;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_EquationDisbenefitX(Value: Double);
const OPNAME = 'TDisbenefitFunctionData.Set_EquationDisbenefitX';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'EquationFunctionX', FloatToStr(Value), FloatToStr(FEquationDisbenefitX),
               LContextData) then
          begin
            FEquationDisbenefitX := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'EquationFunctionX',FloatToStr(FEquationDisbenefitX),
              FloatToStr(Value));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisbenefitFunctionData.Get_EquationDisbenefitY: Double;
const OPNAME = 'TDisbenefitFunctionData.Get_EquationDisbenefitY';
begin
  Result := 0.0;
  try
    Result := FEquationDisbenefitY;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_EquationDisbenefitY(Value: Double);
const OPNAME = 'TDisbenefitFunctionData.Set_EquationDisbenefitY';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'EquationFunctionY', FloatToStr(Value), FloatToStr(FEquationDisbenefitY),
               LContextData) then
          begin
            FEquationDisbenefitY := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'EquationFunctionY',FloatToStr(FEquationDisbenefitY),
              FloatToStr(Value));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisbenefitFunctionData.Get_EquationDisbenefitNonSupply: Double;
const OPNAME = 'TDisbenefitFunctionData.Get_EquationDisbenefitNonSupply';
begin
  Result := 0.0;
  try
    Result := FEquationDisbenefitNonSupply;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_EquationDisbenefitNonSupply(Value: Double);
const OPNAME = 'TDisbenefitFunctionData.Set_EquationDisbenefitNonSupply';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'EquationFunctionNonSupply', FloatToStr(Value), FloatToStr(FEquationDisbenefitNonSupply),
               LContextData) then
          begin
            FEquationDisbenefitNonSupply := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'EquationFunctionNonSupply',FloatToStr(FEquationDisbenefitNonSupply),
              FloatToStr(Value));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisbenefitFunctionData.Get_EquationDisbenefitCost: Double;
const OPNAME = 'TDisbenefitFunctionData.Get_EquationDisbenefitCost';
begin
  Result := 0.0;
  try
    Result := FEquationDisbenefitCost;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_EquationDisbenefitCost(Value: Double);
const OPNAME = 'TDisbenefitFunctionData.Set_EquationDisbenefitCost';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'EquationFunctionCostY', FloatToStr(Value), FloatToStr(FEquationDisbenefitCost),
               LContextData) then
          begin
            FEquationDisbenefitCost := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'EquationFunctionCostY',FloatToStr(FEquationDisbenefitCost),
              FloatToStr(Value));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisbenefitFunctionData.Get_EscalationRate: WideString;
const OPNAME = 'TDisbenefitFunctionData.Get_EscalationRate';
begin
  Result := '';
  try
     Result := UnCompressCommatextString(FEscalationRate);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_EscalationRate(const Value: WideString);
const OPNAME = 'TDisbenefitFunctionData.Set_EscalationRate';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
  LOldValue,
  LTempStr    : string;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          LTempStr := CompressCommatextString(Value);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'DisbenefitEscalationRate', LTempStr, FEscalationRate, LContextData) then
          begin
            LOldValue       := FEscalationRate;
            FEscalationRate := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DisbenefitEscalationRate',LOldValue,FEscalationRate);
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME ) end;
end;

function TDisbenefitFunctionData.Get_EscalationRateByIndex(AIndex: Integer): Double;
const OPNAME = 'TDisbenefitFunctionData.Get_EscalationRateByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FEscalationRate);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_EscalationRateByIndex(AIndex: Integer;
                                                            Value: Double);
const OPNAME = 'TDisbenefitFunctionData.Set_EscalationRateByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FEscalationRate;
    if UpdateCompressCommatextString(AIndex, FloatToStr(Value), LTempStr) then
      EscalationRate := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.Initialise: boolean;
const OPNAME = 'TDisbenefitFunctionData.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FFeatureID                   := -1;
    FChannelNumber               := 0;
    FNrOfEconomicYears           := 0;
    FEquationDisbenefitX         := 0.0;
    FEquationDisbenefitY         := 0.0;
    FEquationDisbenefitNonSupply := 0.0;
    FEquationDisbenefitCost      := 0.0;
    FEscalationRate              := '';

    FYearActive                  := 0;
    FMonthActive                 := 0;
    FYearObsolete                := 0;
    FMonthObsolete               := 0;
    FWQConstraint                := 0.00;

    for LIndex := 1 to 4 do
      FTDSConcentrationFactors[LIndex] := 0.00;

  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDisbenefitFunctionData.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TDisbenefitFunctionData.Validate';
var
  lErrorMessage     : TStringList;
begin
   Result := FALSE;
   try
     lErrorMessage := TStringList.Create;
     try
       if (AContext = 'NrOfEconomicYears') then
         Result := ValidateNrOfEconomicYears(lErrorMessage)
       else
       if (AContext = 'EquationFunctionCostY') then
         Result := ValidateEquationDisbenefitCost(lErrorMessage)
       else
       if (AContext = 'EquationFunctionNonSupply') then
         Result := ValidateEquationDisbenefitNonSupply(lErrorMessage)
       else
       if (AContext = 'EquationFunctionX') then
         Result := ValidateEquationDisbenefitX(lErrorMessage)
       else
       if (AContext = 'EquationFunctionY') then
         Result := ValidateEquationDisbenefitY(lErrorMessage)
       else

       if (AContext = 'YearDemandChannelActive') then
         Result := ValidateYearDemandChannelActive(lErrorMessage)
       else
       if (AContext = 'MonthDemandChannelActive') then
         Result := ValidateMonthDemandChannelActive(lErrorMessage)
       else
       if (AContext = 'YearDemandChannelObsolete') then
         Result := ValidateYearDemandChannelObsolete(lErrorMessage)
       else
       if (AContext = 'MonthDemandChannelObsolete') then
         Result := ValidateMonthDemandChannelObsolete(lErrorMessage)
       else
       if (AContext = 'WaterQualityConstraint') then
         Result := ValidateWaterQualityConstraint(lErrorMessage)
       else

       begin
         Result := TRUE;
         if (NOT ValidateNrOfEconomicYears(lErrorMessage)) then
           Result := FALSE;
         if (NOT ValidateEquationDisbenefitX(lErrorMessage)) then
           Result := FALSE;
         if (NOT ValidateEquationDisbenefitY(lErrorMessage)) then
           Result := FALSE;
         if (NOT ValidateEquationDisbenefitNonSupply(lErrorMessage)) then
             Result := FALSE;
         if (NOT ValidateEquationDisbenefitCost(lErrorMessage)) then
             Result := FALSE;

         if (NOT ValidateYearDemandChannelActive(lErrorMessage)) then
           Result := FALSE;
         if (NOT ValidateMonthDemandChannelActive(lErrorMessage)) then
           Result := FALSE;
         if (NOT ValidateYearDemandChannelObsolete(lErrorMessage)) then
           Result := FALSE;
         if (NOT ValidateMonthDemandChannelObsolete(lErrorMessage)) then
             Result := FALSE;
         if (NOT ValidateWaterQualityConstraint(lErrorMessage)) then

             Result := FALSE;
       end;
       AErrors := AErrors + lErrorMessage.Text;
     finally
       FreeAndNil(lErrorMessage);
     end;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData._AddRef: Integer;
const OPNAME = 'TDisbenefitFunctionData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDisbenefitFunctionData.Populate(AChannelNumber: integer;
                                          AEquationDisbenefitX : double;
                                          AEquationDisbenefitY : double;
                                          AEquationDisbenefitNonSupply,
                                          AEquationDisbenefitCost: double;
                                          AEscalationRate: string;
                                          AYearActive,AMonthActive,
                                          AYearObsolete,AMonthObsolete : integer;
                                          AWQConstraint : double; ATDSConcentrationFactors : array of double): Boolean;
const OPNAME = 'TDisbenefitFunctionData.Populate';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    FChannelNumber                 := AChannelNumber;
    FEquationDisbenefitX           := AEquationDisbenefitX;
    FEquationDisbenefitY           := AEquationDisbenefitY;
    FEquationDisbenefitNonSupply   := AEquationDisbenefitNonSupply;
    FEquationDisbenefitCost        := AEquationDisbenefitCost;
    FEscalationRate                := AEscalationRate;

    FYearActive                    := AYearActive;
    FMonthActive                   := AMonthActive;
    FYearObsolete                  := AYearObsolete;
    FMonthObsolete                 := AMonthObsolete;
    FWQConstraint                  := AWQConstraint;
    for LIndex := 1 to 4 do
      FTDSConcentrationFactors[LIndex] := ATDSConcentrationFactors[LIndex-1];

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDisbenefitFunctionData.ValidateEquationDisbenefitCost(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateEquationDisbenefitCost';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('EquationFunctionCostY',
            FloatToStr(FEquationDisbenefitCost), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateEquationDisbenefitNonSupply(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateEquationDisbenefitNonSupply';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('EquationFunctionNonSupply',
            FloatToStr(FEquationDisbenefitNonSupply), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateEquationDisbenefitX(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateEquationDisbenefitX';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('EquationFunctionX',
            FloatToStr(FEquationDisbenefitX), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateEquationDisbenefitY(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateEquationDisbenefitY';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('EquationFunctionY',
            FloatToStr(FEquationDisbenefitY), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateNrOfEconomicYears(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateNrOfEconomicYears';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('NrOfEconomicVariableYears',
            IntToStr(FNrOfEconomicYears), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateYearDemandChannelActive(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateYearDemandChannelActive';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('YearDemandChannelActive',
            IntToStr(FYearActive), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateMonthDemandChannelActive(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateMonthDemandChannelActive';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('MonthDemandChannelActive',
            IntToStr(FMonthActive), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateYearDemandChannelObsolete(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateYearDemandChannelObsolete';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('YearDemandChannelObsolete',
            IntToStr(FYearObsolete), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateMonthDemandChannelObsolete(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateMonthDemandChannelObsolete(';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('MonthDemandChannelObsolete',
            IntToStr(FMonthObsolete), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateWaterQualityConstraint(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDisbenefitFunctionData.ValidateWaterQualityConstraint';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('WaterQualityConstraint',
            FloatToStr(FWQConstraint), lMessage)) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.ValidateTDSConcentration(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
const OPNAME = 'TDisbenefitFunctionData.ValidateTDSConcentration';
var
  LResult             : boolean;
  LIndex              : integer;
  LMessage            : string;
  LTDSConcentration   : TAbstractFieldProperty;
  lStopOnFirstError   : boolean;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    LTDSConcentration := FAppModules.FieldProperties.FieldProperty('TDSConcentration');
    if (LTDSConcentration <> nil) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for LIndex := LTDSConcentration.ArrayLow to LTDSConcentration.ArrayHigh do
      begin
        LMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty('TDSConcentration',
                FloatToStr(Get_TDSConcentrationByIndex(LIndex)),LMessage,LIndex)) then
        begin
          LResult := FALSE;
          AErrorMessages.Add(LMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      Result := LResult;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;


function TDisbenefitFunctionData.Get_EscalationFactors: WideString;
const OPNAME = 'TDisbenefitFunctionData.Get_EscalationFactors';
begin

end;

function TDisbenefitFunctionData.Get_MonthActive: Integer;
const OPNAME = 'TDisbenefitFunctionData.Get_MonthActive';
begin
  Result := 0;
  try
    Result := FMonthActive;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.Get_MonthObsolete: Integer;
const OPNAME = 'TDisbenefitFunctionData.Get_MonthObsolete';
begin
  Result := 0;
  try
    Result := FMonthObsolete;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.Get_TDSConcentrationByIndex(
  Aindex: Integer): Double;
const OPNAME = 'TDisbenefitFunctionData.Get_TDSConcentrationByIndex';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := NullFloat;
  try

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('TDSConcentration');
    if (AIndex >= LFieldProperty.ArrayLow) AND (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if FTDSConcentrationFactors[AIndex] = NullFloat then
        Result := 0.
      else
        Result := FTDSConcentrationFactors[AIndex];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDisbenefitFunctionData.Get_WQConstraint: Double;
const OPNAME = 'TDisbenefitFunctionData.Get_WQConstraint';
begin
  Result := 0;
  try
    Result := FWQConstraint;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.Get_YearActive: Integer;
const OPNAME = 'TDisbenefitFunctionData.Get_YearActive';
begin
  Result := 0;
  try
    Result := FYearActive;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDisbenefitFunctionData.Get_YearObsolete: Integer;
const OPNAME = 'TDisbenefitFunctionData.Get_YearObsolete';
begin
  Result := 0;
  try
    Result := FYearObsolete;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDisbenefitFunctionData.Set_EscalationFactors(
  const Value: WideString);
const OPNAME = 'TDisbenefitFunctionData.Set_EscalationFactors';
begin

end;

procedure TDisbenefitFunctionData.Set_MonthActive(Value: Integer);
const OPNAME = 'TDisbenefitFunctionData.Set_MonthActive';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'MonthDemandChannelActive', IntToStr(Value), IntToStr(FMonthActive), LContextData) then
          begin
            LOldValue       := FMonthActive;
            FMonthActive    := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthDemandChannelActive',IntToStr(LOldValue),IntToStr(FMonthActive));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME ) end;
end;

procedure TDisbenefitFunctionData.Set_MonthObsolete(Value: Integer);
const OPNAME = 'TDisbenefitFunctionData.Set_MonthObsolete';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'MonthDemandChannelObsolete', IntToStr(Value), IntToStr(FMonthObsolete), LContextData) then
          begin
            LOldValue       := FYearActive;
            FMonthObsolete := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthDemandChannelObsolete',IntToStr(LOldValue),IntToStr(FMonthObsolete));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME ) end;
end;

procedure TDisbenefitFunctionData.Set_TDSConcentrationByIndex(Aindex: Integer; Value: Double);
const OPNAME = 'TDisbenefitFunctionData.Set_TDSConcentrationByIndex';
var
  LLoadAgent : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
  LNewValue,
  LOldValue : string;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('TDSConcentration');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if FTDSConcentrationFactors[AIndex] <> Value then
      begin
        LLoadAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData_TDSConcentration(LContextData, IntToStr(FChannelNumber), IntToStr(Aindex));
            if (Value = NullFloat) then
              LNewValue := ''
            else
              LNewValue := FloatToStr(Value);
            if (FTDSConcentrationFactors[AIndex] = NullFloat) then
              LOldValue := ''
            else
              LOldValue := FloatToStr(FTDSConcentrationFactors[AIndex]);
            if (FAppModules.FieldProperties.UpdateFieldValue(
               'TDSConcentration', LNewValue, LOldValue, LContextData)) then
            begin
              LOldValue := FloatToStr(FTDSConcentrationFactors[AIndex]);
              FTDSConcentrationFactors[AIndex] := Value;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'TDSConcentration',LOldValue,FloatToStr(Value));
            end;
          finally
            LContextData.Free;
          end;
        finally
          LLoadAgent.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDisbenefitFunctionData.Set_WQConstraint(Value: Double);
const OPNAME = 'TDisbenefitFunctionData.Set_WQConstraint';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
  LOldValue    : Double;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'WaterQualityConstraint', FloatToStr(Value), FloatToStr(FWQConstraint), LContextData) then
          begin
            LOldValue       := FWQConstraint;
            FWQConstraint   := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterQualityConstraint',FloatToStr(LOldValue),FloatToStr(FWQConstraint));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME ) end;
end;

procedure TDisbenefitFunctionData.Set_YearActive(Value: Integer);
const OPNAME = 'TDisbenefitFunctionData.Set_YearActive';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'YearDemandChannelActive', IntToStr(Value), IntToStr(FYearActive), LContextData) then
          begin
            LOldValue       := FYearActive;
            FYearActive := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'YearDemandChannelActive',IntToStr(LOldValue),IntToStr(FYearActive));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME ) end;
end;

procedure TDisbenefitFunctionData.Set_YearObsolete(Value: Integer);
const OPNAME = 'TDisbenefitFunctionData.Set_YearObsolete';
var
  LSQLAgent    : TDisbenefitFunctionDefinitionSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData_ChannelNo(LContextData, IntToStr(FChannelNumber));
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'YearDemandChannelObsolete', IntToStr(Value), IntToStr(FYearObsolete), LContextData) then
          begin
            LOldValue       := FYearObsolete;
            FYearObsolete   := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'YearDemandChannelObsolete',IntToStr(LOldValue),IntToStr(FYearObsolete));
          end;
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME ) end;
end;

end.
