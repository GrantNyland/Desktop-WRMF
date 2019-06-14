{******************************************************************************}
{*  UNIT      : Contains the class TMasterControlFeature.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/29                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMasterControlFeatures;

interface

uses
  Classes,
  Contnrs,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Master Control Features                                                    *}
{******************************************************************************}

  TMasterControlFeature = class(TAbstractAppObject, IMasterControlFeature)
  protected
    FFeatureID         : integer;
    FFeatureName       : string;
    FChannelNr         : integer;
    FFeatureType       : integer;
    FFeatureSubType    : integer;
    FMasterControlType : string;
    FMonthlyFactors    : TMasterMonthlyDoublesArray;

    // F01 Planning
    FDemandCentreType : string;
    FDemandCentreID    : integer;
    FAnnualDemand      : double;
    FMinimumDemand     : double;
    FIncludeInOutput   : Boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateFeatureName (AErrorMessages : TStrings) : WordBool;
    function ValidateAnnualDemand (AErrorMessages : TStrings) : WordBool;
    function ValidateMinimumDemand (AErrorMessages : TStrings) : WordBool;
    function ValidateDemandCentreID (AErrorMessages : TStrings) : WordBool;
    function ValidateMasterControlType (AErrorMessages : TStrings) : WordBool;
    function ValidateDistributionFactors (AErrorMessages : TStrings;AErrorColumns: TStringList) : WordBool;
  public
    function Initialise : boolean; override;
    function Populate (AFeatureID         : integer;
                       AFeatureName       : WideString;
                       AFeatureType       : integer;
                       AFeatureSubType    : integer;
                       AChannelNr         : integer;
                       AMasterControlType : WideString;
                       AFactors           : TMasterMonthlyDoublesArray;
                       ADemandCentreID    : integer;
                       ADemandCentreType  : string;
                       AAnnualDemand      : double;
                       AMinimumDemand     : double;
                       AIncludeInOutput   : boolean): WordBool;
    procedure Assign(AChannelNumber : integer; AMasterControlFeature : TMasterControlFeature);
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_MasterControlType: WideString; safecall;
    procedure Set_MasterControlType(const AType: WideString); safecall;
    function Get_MonthlyFactors : TMasterMonthlyDoublesArray; safecall;
    function Get_FactorByMonth (AMonth : integer): double; safecall;
    procedure Set_FactorByMonth (AMonth: integer; AFactor : double); safecall;

    function Get_DemandCentreType: WideString; safecall;
    procedure Set_DemandCentreType(const Value: WideString); safecall;
    function Get_DemandCentreID: Integer; safecall;
    procedure Set_DemandCentreID(Value: Integer); safecall;
    function Get_AnnualDemand: Double; safecall;
    procedure Set_AnnualDemand(Value: Double); safecall;
    function Get_MinimumDemand: Double; safecall;
    procedure Set_MinimumDemand(Value: Double); safecall;
    function Get_IncludeInOutput: WordBool; safecall;
    procedure Set_IncludeInOutput(Value: WordBool); safecall;

    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex : WideString) : WideString; safecall;

    property FeatureID         : integer read Get_FeatureID;
    property FeatureName       : WideString read Get_FeatureName write Set_FeatureName;
    property Channel           : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType       : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType    : integer read Get_FeatureSubType write Set_FeatureSubType;
    property MasterControlType : WideString read Get_MasterControlType write Set_MasterControlType;
    property MonthlyFactors    : TMasterMonthlyDoublesArray read Get_MonthlyFactors;
    property FactorByMonth[AMonth: integer]: double read Get_FactorByMonth write Set_FactorByMonth;
    property DemandCentreType : WideString read Get_DemandCentreType write Set_DemandCentreType;
    property DemandCentreID   : Integer read Get_DemandCentreID write Set_DemandCentreID;
    property AnnualDemand      : Double read Get_AnnualDemand write Set_AnnualDemand;
    property MinimumDemand     : Double read Get_MinimumDemand write Set_MinimumDemand;
    property IncludeInOutput   : WordBool read Get_IncludeInOutput write Set_IncludeInOutput;

  end;

  TMasterControlFeatureList = class(TAbstractAppObject, IMasterControlFeatureList)
  protected
    FMasterControlFeatureList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddMasterControlFeature (AFeature : TMasterControlFeature): boolean;
    function Get_WaterControlFeature : IMasterControlFeature; safecall;
    function Get_PowerControlFeature : IMasterControlFeature; safecall;
  public
    function Initialise : boolean; override;
    function NewMasterControlFeature : TMasterControlFeature;
    function CreateNewMasterControlFeature (AChannelNumber : integer) : TMasterControlFeature;
    function CastMasterControlFeatureByIndex (AIndex : integer): TMasterControlFeature;
    function CastMasterControlFeatureByID(AFeatureID : integer): TMasterControlFeature;
    function CastMasterControlFeatureByChannelNr(AChannelNr : integer): TMasterControlFeature;
    function CopyMasterControlFeature(ANewChannelNumber : integer;AOldChannelNumber : integer): IMasterControlFeature; safecall;
    function CreateMasterControlFeature(AChannelNumber : integer) : IMasterControlFeature; safecall;
    function RemoveMasterControlFeatureWithID (AFeatureID : integer) : WordBool; safecall;
    function DeleteMasterControlFeatureWithID(AFeatureID : integer) : WordBool;
    function DeleteMasterControlFeatureWithIndex(AIndex : integer) : WordBool;
    function Get_DemandCentreByID(ADemandCentreID : integer): IMasterControlFeature; safecall;
    function Get_ChannelByChannelNumber: Integer; safecall;
    function Get_MasterControlFeatureByID(AFeatureID : integer): IMasterControlFeature; safecall;
    function Get_MasterControlFeatureByIndex(AIndex: integer): IMasterControlFeature; safecall;
    function Get_MasterControlFeatureCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property MasterControlFeatureByIndex[AIndex : integer]: IMasterControlFeature
      read Get_MasterControlFeatureByIndex;
    property MasterControlFeatureByID[AFeatureID: integer]: IMasterControlFeature
      read Get_MasterControlFeatureByID;
    property DemandCentreByID[AFeatureID: integer]: IMasterControlFeature
      read Get_DemandCentreByID;
    property MasterControlFeatureCount: integer read Get_MasterControlFeatureCount;
    property WaterControlFeature : IMasterControlFeature read Get_WaterControlFeature;
    property PowerControlFeature : IMasterControlFeature read Get_PowerControlFeature;
    property ChannelByChannelNumber: Integer read Get_ChannelByChannelNumber;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  Math,
  UConstants,
  UUtilities,
  VCL.Dialogs,
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UGrowthFactorData,
  UGrowthFactorsExcelData,
  UNetworkFeaturesSQLAgent,
  UErrorHandlingOperations;

{******************************************************************************}
{ TMasterControlFeature                                                        }
{******************************************************************************}

function TMasterControlFeature._AddRef: Integer;
const OPNAME = 'TMasterControlFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature._Release: Integer;
const OPNAME = 'TMasterControlFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.CreateMemberObjects;
const OPNAME = 'TMasterControlFeature.CreateMemberObjects';
var
  LMonthlyFactors : TAbstractFieldProperty;
begin
  inherited;
  try
    LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
    if (LMonthlyFactors = nil) then
      raise Exception.Create('Field (MinEnergyDemand) not found in field properties');
    SetLength(FMonthlyFactors,LMonthlyFactors.ArrayLength);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.DestroyMemberObjects;
const OPNAME = 'TMasterControlFeature.DestroyMemberObjects';
begin
  try
    //FChannel := nil;
    Finalize(FMonthlyFactors);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

function TMasterControlFeature.Initialise: boolean;
const OPNAME = 'TMasterControlFeature.Initialise';
var
  nIndex          : integer;
  LMonthlyFactors : TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    //FChannel              := nil;
    FFeatureID            := -1;
    FFeatureName          := '';
    FFeatureType          := -1;
    FFeatureSubType       := -1;
    FMasterControlType    := '';
    FAnnualDemand         := 0.0;
    FMinimumDemand        := 0.0;
    FDemandCentreType    := '';
    FIncludeInOutput      := False;
    FDemandCentreID       := -1;
    LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
    for nIndex := LMonthlyFactors.ArrayLow to LMonthlyFactors.ArrayHigh do
      FMonthlyFactors[nIndex] := NullFloat;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_FeatureID : integer;
const OPNAME = 'TMasterControlFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_FeatureName : WideString;
const OPNAME = 'TMasterControlFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TMasterControlFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
              ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_FeatureType : integer;
const OPNAME = 'TMasterControlFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_FeatureSubType : integer;
const OPNAME = 'TMasterControlFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_MasterControlType : WideString;
const OPNAME = 'TMasterControlFeature.Get_MasterControlType';
begin
  Result := '';
  try
    Result := FMasterControlType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_MonthlyFactors: TMasterMonthlyDoublesArray;
const OPNAME = 'TMasterControlFeature.Get_MonthlyFactors';
begin
  Result := FMonthlyFactors;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_FactorByMonth(AMonth : integer): double;
const OPNAME = 'TMasterControlFeature.Get_FactorByMonth';
begin
  Result := NullFloat;
  try
    Result := FMonthlyFactors[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_FeatureName (const AName : WideString);
const OPNAME = 'TMasterControlFeature.Set_FeatureName';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MasterControlFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MasterControlFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TMasterControlFeature.Set_Channel';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LNewValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LOldValue := IntToStr(FChannelNr);
        if (AChannel <> nil) then
          LNewValue := IntToStr(AChannel.ChannelNumber)
        else
          LNewValue := '0';
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MasterControlChannelNumber', LNewValue, LOldValue, LContextData) then
        begin
          FChannelNr := AChannel.ChannelNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MasterControlChannelNumber',LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TMasterControlFeature.Set_FeatureType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
      {RHS comeback = no DB field to store type of feature}
{
        LLoadAgent.LoadChannelPenaltyContextData(LContextData,
          IntToStr(FPenaltyStructType), IntToStr(FChannelPenaltyArcNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Penalty', FloatToStr(APenaltyValue), FloatToStr(FPenaltyValue), LContextData) then
        begin
}         LOldValue := FFeatureType;
          FFeatureType := AType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FeatureType',IntToStr(LOldValue),IntToStr(AType));
//        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TMasterControlFeature.Set_FeatureSubType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
      {RHS comeback = no DB field to store subtype of feature}
{
        LLoadAgent.LoadChannelPenaltyContextData(LContextData,
          IntToStr(FPenaltyStructType), IntToStr(FChannelPenaltyArcNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Penalty', FloatToStr(APenaltyValue), FloatToStr(FPenaltyValue), LContextData) then
        begin
}         LOldValue := FFeatureSubType;
          FFeatureSubType := ASubType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FeatureSubType',IntToStr(LOldValue),IntToStr(ASubType));
//        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_MasterControlType (const AType: WideString);
const OPNAME = 'TMasterControlFeature.Set_MasterControlType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'MasterChannelType', AType, FMasterControlType, LContextData) then
        begin
          LOldValue := FMasterControlType;
          FMasterControlType := AType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MasterChannelType',LOldValue,AType);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_FactorByMonth (AMonth : integer; AFactor : double);
const OPNAME = 'TMasterControlFeature.Set_FactorByMonth ';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AMonth));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'MinEnergyDemand', FloatToStr(AFactor), FloatToStr(FMonthlyFactors[AMonth]), LContextData) then
        begin
          lOldValue := FMonthlyFactors[AMonth];
          FMonthlyFactors[AMonth] := AFactor;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DistributionFactor',
                                                FloatToStr(lOldValue), FloatToStr(AFactor));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_DemandCentreType: WideString;
const OPNAME = 'TMasterControlFeature.Get_DemandCentreType ';
begin
  Result := '';
  try
    Result := FDemandCentreType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_DemandCentreType(const Value: WideString);
const OPNAME = 'TMasterControlFeature.Set_DemandCentreType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelNr(LContextData, IntToStr(FChannelNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DemandCentreType', Value, FDemandCentreType, LContextData) then
        begin
          LOldValue          := FDemandCentreType;
          FDemandCentreType := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'DemandCentreType', LOldValue, Value);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_DemandCentreID: Integer;
const OPNAME = 'TMasterControlFeature.Get_DemandCentreID ';
begin
  Result := 0;
  try
    Result := FDemandCentreID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_DemandCentreID(Value: Integer);
const OPNAME = 'TMasterControlFeature.Set_DemandCentreID';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelNr(LContextData, IntToStr(FChannelNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DDDemandCentreID', IntToStr(Value),  IntToStr(FChannelNr), LContextData) then
        begin
          LOldValue  := IntToStr(FChannelNr);
          FChannelNr := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'DDDemandCentreID', LOldValue, IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_AnnualDemand: Double;
const OPNAME = 'TMasterControlFeature.Get_AnnualDemand ';
begin
  Result := 0.0;
  try
    Result := FAnnualDemand;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_AnnualDemand(Value: Double);
const OPNAME = 'TMasterControlFeature.Set_AnnualDemand';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelNr(LContextData, IntToStr(FChannelNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AnnualDemand', FloatToStr(Value), FloatToStr(FAnnualDemand) , LContextData) then
        begin
          LOldValue     := FloatToStr(FAnnualDemand);
          FAnnualDemand := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'AnnualDemand', LOldValue, FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_MinimumDemand: Double;
const OPNAME = 'TMasterControlFeature.Get_MinimumDemand ';
begin
  Result := 0.0;
  try
    Result := FMinimumDemand;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_MinimumDemand(Value: Double);
const OPNAME = 'TMasterControlFeature.Set_MinimumDemand';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelNr(LContextData, IntToStr(FChannelNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinimumDemand', FloatToStr(Value), FloatToStr(FMinimumDemand) , LContextData) then
        begin
          LOldValue      := FloatToStr(FMinimumDemand);
          FMinimumDemand := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'MinimumDemand', LOldValue, FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Get_IncludeInOutput: WordBool;
const OPNAME = 'TMasterControlFeature.Get_IncludeInOutput ';
begin
  Result := False;
  try
    Result := FIncludeInOutput;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeature.Set_IncludeInOutput(Value: WordBool);
const OPNAME = 'TMasterControlFeature.Set_IncludeInOutput';
var
  LOldIncludeInOutput : string;
  LNewIncludeInOutput : string;
  LContextData        : TStringList;
  LLoadAgent          : TNetworkFeaturesSQLAgent;
begin
  try
    if FIncludeInOutput then
      LOldIncludeInOutput := '1'
    else
      LOldIncludeInOutput := '0';

    if Value then
      LNewIncludeInOutput := '1'
    else
      LNewIncludeInOutput := '0';

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelNr(LContextData, IntToStr(FChannelNr));
       if FAppModules.FieldProperties.UpdateFieldValue(
             'IncludeInOutput', LNewIncludeInOutput, LOldIncludeInOutput, LContextData) then
        begin
          FIncludeInOutput := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IncludeInOutput',LOldIncludeInOutput,
                                                LNewIncludeInOutput);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Populate (AFeatureID         : integer;
                                         AFeatureName       : WideString;
                                         AFeatureType       : integer;
                                         AFeatureSubType    : integer;
                                         AChannelNr         : integer;
                                         AMasterControlType : WideString;
                                         AFactors           : TMasterMonthlyDoublesArray;
                                         ADemandCentreID    : integer;
                                         ADemandCentreType  : string;
                                         AAnnualDemand      : double;
                                         AMinimumDemand     : double;
                                         AIncludeInOutput   : boolean): WordBool;
const OPNAME = 'TMasterControlFeature.Populate';
var
  lMonth          : integer;
  LMonthlyFactors : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FFeatureID          := AFeatureID;
    FFeatureName        := AFeatureName;
    FFeatureType        := AFeatureType;
    FFeatureSubType     := AFeatureSubType;
    FMasterControlType  := AMasterControlType;
    FChannelNr          := AChannelNr;
    LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
    for lMonth := LMonthlyFactors.ArrayLow to LMonthlyFactors.ArrayHigh do
      FMonthlyFactors[lMonth] := AFactors[lMonth];

    FDemandCentreID     := ADemandCentreID;
    FDemandCentreType   := ADemandCentreType;
    FAnnualDemand       := AAnnualDemand;
    FMinimumDemand      := AMinimumDemand;
    FIncludeInOutput    := AIncludeInOutput;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool;
const OPNAME = 'TMasterControlFeature.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
  lErrorCols        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      if (AContext = 'FeatureName') then
        Result := ValidateFeatureName(lErrorList)
      else
      if (AContext = 'MasterControlType') then
        Result := ValidateMasterControlType(lErrorList)
      else
      if (AContext = 'DistributionFactors') then
      begin
        Result := ValidateDistributionFactors(lErrorList,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorList.Text
          else
            AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          lErrorList.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'AnnualDemand') then
        Result := ValidateAnnualDemand(lErrorList)
      else
      if (AContext = 'MinimumDemand') then
        Result := ValidateMinimumDemand(lErrorList)
      else
      if (AContext = 'DDDemandCentreID') then
        Result := ValidateDemandCentreID(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateFeatureName(lErrorList)) then
          Result := FALSE;
        if (NOT ValidateAnnualDemand(lErrorList)) then
          Result := FALSE;
        if (NOT ValidateMinimumDemand(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMasterControlType(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDistributionFactors(lErrorList, lErrorCols)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlFeature.ValidateFeatureName (AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMasterControlFeature.ValidateFeatureName';
var
  lIndex       : integer;
  lUnique      : Boolean;
  lMessage     : string;
  lFeature     : TMasterControlFeature;
  lFeatureList : TMasterControlFeatureList;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('MasterControlFeatureName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastMasterControlFeatureList;
      lUnique := TRUE;
      lIndex := 0;
      while (lUnique AND (lIndex < lFeatureList.MasterControlFeatureCount)) do
      begin
        lFeature := lFeatureList.CastMasterControlFeatureByIndex(lIndex);
        if ((lFeature.FeatureID <>  FFeatureID) AND
            (UpperCase(Trim(lFeature.FFeatureName)) = UpperCase(Trim(FFeatureName)))) then
        begin
          lMessage := FAppModules.language.GetString('ContextValidation.DuplicateMasterControlFeatureName');
          AErrorMessages.Add('WARNING:' +Format(lMessage,[IntToStr(Channel.ChannelNumber)]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.ValidateAnnualDemand(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMasterControlFeature.ValidateAnnualDemand';
var
  lMessage     : string;
begin
  Result := FALSE;
  try
    if (FAppModules.Model.ModelName = CYield) then
      Result := TRUE
    else
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty('AnnualDemand',
              FloatToStr(FAnnualDemand), lMessage)) then
        AErrorMessages.Add('ERROR:' +lMessage);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.ValidateMinimumDemand(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMasterControlFeature.ValidateMinimumDemand';
var
  lMessage     : string;
begin
  Result := FALSE;
  try
    if (FAppModules.Model.ModelName = CYield) then
      Result := TRUE
    else
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty('MinimumDemand',
              FloatToStr(FMinimumDemand), lMessage)) then
        AErrorMessages.Add('ERROR:' +lMessage);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



function TMasterControlFeature.ValidateMasterControlType (AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMasterControlFeature.ValidateMasterControlType';
var
  lMessage          : string;
  lResult           : Boolean;
  lFeature          : TMasterControlFeature;
  lFeatureList      : TMasterControlFeatureList;
  lIndex            : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
          ('MasterChannelType', FMasterControlType, lMessage)) then
    begin
      lResult := FALSE;
      AErrorMessages.Add('ERROR:' +lMessage);
    end
    else
    begin
      if (FAppModules.Model.ModelName = CYield) then
      begin
        if (FMasterControlType = 'H') then
        begin
          lMessage := FAppModules.language.GetString('ContextValidation.InvalidMasterControlType');
          AErrorMessages.Add('ERROR:' +Format(lMessage,[IntToStr(Channel.ChannelSubType)]));
          lResult := FALSE;
        end
        else
        begin
          lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            CastNetworkFeaturesData.CastMasterControlFeatureList;
          lIndex     := 0;
          while (lResult AND (lIndex < lFeatureList.MasterControlFeatureCount)) do
          begin
            lFeature := lFeatureList.CastMasterControlFeatureByIndex(lIndex);
            if ((lFeature.FeatureID <>  FFeatureID) AND
                (Trim(Uppercase(FMasterControlType)) = Trim(UpperCase(lFeature.FMasterControlType)))) then
            begin
              lMessage := FAppModules.language.GetString('ContextValidation.OneMasterControlPerType');
              AErrorMessages.Add('ERROR:' +Format(lMessage,[IntToStr(Channel.ChannelSubType)]));
              lResult := FALSE;
            end
            else
              lIndex := lIndex + 1;
          end;
        end;
      end
      else
      if (FAppModules.Model.ModelName = CPlanning) then
      begin
        if (FMasterControlType = 'P') then
        begin
          lMessage := FAppModules.language.GetString('ContextValidation.InvalidMasterControlType');
          AErrorMessages.Add('ERROR:' +Format(lMessage,[IntToStr(Channel.ChannelSubType)]));
          lResult := FALSE;
        end
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.ValidateDemandCentreID (AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMasterControlFeature.ValidateDemandCentreID';
var
  lMessage : string;
  lResult  : Boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lMessage := '';
    if (FAppModules.Model.ModelName = CPlanning) then
    begin
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
            ('DDDemandCentreID', IntToStr(FDemandCentreID), lMessage)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +lMessage);
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.ValidateDistributionFactors (AErrorMessages: TStrings;AErrorColumns: TStringList): WordBool;
const OPNAME = 'TMasterControlFeature.ValidateDistributionFactors';
var
  lMonth            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lPropertyName     : string;
  lFieldProperty    : TAbstractFieldProperty;
  lTotal            : double;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
    if (Trim(Uppercase(FMasterControlType)) = 'P') then
      lPropertyName := 'MinEnergyDemand'
    else
      lPropertyName := 'WaterSupplyDistribution';
    lTotal := 0;
    for lMonth := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
              (lPropertyName, FloatToStr(FactorByMonth[lMonth]), lMessage, lMonth)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +lMessage);
        AErrorColumns.Add(IntToStr(lMonth));
        if (lStopOnFirstError) then
          Break;
      end
      else
        lTotal := lTotal + FactorByMonth[lMonth];
    end;
    if (lResult AND (ABS(lTotal - 12) >= 0.05)) then
    begin
      lResult := FALSE;
      AErrorMessages.Add('ERROR:' +FAppModules.Language.GetString('TFieldError.MonthlyFactorSum'));
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeature.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TMasterControlFeature.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FFeatureID)
    else
      Result := Result + ',Identifier=' + IntToStr(FFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TMasterControlFeatureList                                                   *}
{******************************************************************************}

function TMasterControlFeatureList._AddRef: Integer;
const OPNAME = 'TMasterControlFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList._Release: Integer;
const OPNAME = 'TMasterControlFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeatureList.CreateMemberObjects;
const OPNAME = 'TMasterControlFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FMasterControlFeatureList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMasterControlFeatureList.DestroyMemberObjects;
const OPNAME = 'TMasterControlFeatureList.DestroyMemberObjects';
begin
  try
    while (FMasterControlFeatureList.Count > 0) do
      DeleteMasterControlFeatureWithIndex(0);
    FreeAndNil(FMasterControlFeatureList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Initialise: boolean;
const OPNAME = 'TMasterControlFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FMasterControlFeatureList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.AddMasterControlFeature (AFeature : TMasterControlFeature): boolean;
const OPNAME = 'TMasterControlFeatureList.AddMasterControlFeature';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FMasterControlFeatureList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.NewMasterControlFeature : TMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.NewMasterControlFeature';
var
  lFeature : TMasterControlFeature;
begin
  Result := nil;
  try
    lFeature := TMasterControlFeature.Create(FAppModules);
    AddMasterControlFeature(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.CreateNewMasterControlFeature (AChannelNumber : integer) : TMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.CreateNewMasterControlFeature';
var
  LFeatureID      : integer;
  lDemandCentreID : integer;
  LLoadAgent      : TNetworkFeaturesSQLAgent;
  LFeature        : TMasterControlFeature;
  LFactors        : TMasterMonthlyDoublesArray;
  LMonthlyFactors : TAbstractFieldProperty;
  LNumberOfYears,
  lIndex          : integer;
  LTempStr        : TStringList;
  lMasterType     : string;
  LGrowthFactors  : TGrowthFactors;
  LGrowthFactorsInterf  : IGrowthFactors;
  LDMDGTHFactorsInterf  : IDemandCentreGrowthFactors;

begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelCode = CYield) then
    begin
      if (Get_MasterControlFeatureCount < 2) then
      begin
        if (Get_MasterControlFeatureCount > 0) then
        begin
          lFeature := CastMasterControlFeatureByIndex(0);
          if (Trim(UpperCase(LFeature.MasterControlType)) = 'W') then
            lMasterType := 'P'
          else
            lMasterType := 'W';
        end
        else
          lMasterType := 'W';

        LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
        if (LMonthlyFactors = nil) then
          raise Exception.Create('Field (MinEnergyDemand) not found in field properties');
        SetLength(LFactors,LMonthlyFactors.ArrayLength);

        LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
        try
          LFeatureID := 0;
          lDemandCentreID := 0;

          if (LLoadAgent.InsertMasterControlFeature(LFeatureID, lDemandCentreID, AChannelNumber, lMasterType)) then
          begin
            LFeature := NewMasterControlFeature;
            LFeature.Initialise;
            for lIndex := LMonthlyFactors.ArrayLow to LMonthlyFactors.ArrayHigh do
              LFactors[lIndex] := 1;
            LFeature.Populate
              (LFeatureID,
               UpperCase(FAppModules.Language.GetString('NetworkFeatures.MasterControlFeature')) + ' ' + IntToStr(LFeatureID),
               12, 0, AChannelNumber, lMasterType, lFactors, lDemandCentreID, '', 0.0, 0.0, False);
            Result := LFeature;
          end;
        finally
          LLoadAgent.Free;
        end;
      end;
    end
    else
    if (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      lMasterType := 'W';
      LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
      if (LMonthlyFactors = nil) then
        raise Exception.Create('Field (MinEnergyDemand) not found in field properties');
      SetLength(LFactors,LMonthlyFactors.ArrayLength);

      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        LFeatureID := 0;
        lDemandCentreID := 0;
        if (LLoadAgent.InsertMasterControlFeature(LFeatureID, lDemandCentreID, AChannelNumber, lMasterType)) then
        begin
          LFeature := NewMasterControlFeature;
          LFeature.Initialise;
          for lIndex := LMonthlyFactors.ArrayLow to LMonthlyFactors.ArrayHigh do
            LFactors[lIndex] := 1;
          LFeature.Populate
            (LFeatureID,
             UpperCase(FAppModules.Language.GetString('NetworkFeatures.MasterControlFeature')) + ' ' + IntToStr(LFeatureID),
             12, 0, AChannelNumber, lMasterType, lFactors, lDemandCentreID, 'D', 0.0, 0.0, False);
          LGrowthFactorsInterf :=  TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
          if LGrowthFactorsInterf <> nil then
          begin
            LDMDGTHFactorsInterf := LGrowthFactorsInterf.AddDemandCentresGrowthFactor(AChannelNumber);
            if LDMDGTHFactorsInterf <> nil then
            begin
              LTempStr := TStringList.Create;
              try
                LNumberOfYears := LGrowthFactorsInterf.NumberOfYears;
                for LIndex := 0 to LNumberOfYears do
                  LTempStr.Add('0.0000');
                LDMDGTHFactorsInterf.GrowthFactors := LTempStr.CommaText;
              finally
                LTempStr.Free;
              end;
            end;
            LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
            if not LGrowthFactors.Populated then
            begin
              ShowMessage(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG1'));
              Exit;
            end
            else
            begin
              if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG5'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;

              if(LGrowthFactors.DemandCentresGrowthFactorsCount = 0) then
                if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG2'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;

              //if(LGrowthFactors.MinMaxChannelGrowthFactorsCount = 0) then
                //if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG3'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;

              if(LGrowthFactors.HydrologyGrowthFactorsCount = 0) then
                if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG4'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;

              LGrowthFactors.GenerateGrowthProjections;
            end;
          end;
          Result := LFeature;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.CreateMasterControlFeature (AChannelNumber : integer) : IMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.CreateMasterControlFeature';
var
  LFeature : IMasterControlFeature;
begin
  Result := nil;
  try
    lFeature := CreateNewMasterControlFeature(AChannelNumber);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.RemoveMasterControlFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TMasterControlFeatureList.RemoveMasterControlFeatureWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
  LGrowthFactorsInterf  : IGrowthFactors;
  LFeature : IMasterControlFeature;
  LGrowthFactors  : TExelGrowthFactors;
  LHydrologyFactors : TExelHydrologyGrowthFactors;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteMasterControlFeature(AFeatureID) then
        begin
          if (FAppModules.StudyArea.ModelCode = CPlanning) then
          begin
            // remove GTH - growth Factors.....
            LGrowthFactorsInterf :=  TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
            if LGrowthFactorsInterf <> nil then
            begin
              LFeature := CastMasterControlFeatureByID(AFeatureID);
              if LFeature <> nil then
              begin
                LGrowthFactorsInterf.RemoveDemandCentresGrowthFactor(LFeature.Channel.ChannelNumber);
                LGrowthFactorsInterf.RemoveMinMaxChannelGrowthFactor(LFeature.Channel.ChannelNumber);

                LGrowthFactors  := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
                if LGrowthFactors <> nil then
                begin
                  LHydrologyFactors := LGrowthFactors.HydrologyGrowthFactorsByIdentifier[LFeature.Channel.ChannelID];
                  if LHydrologyFactors <> nil then
                    LGrowthFactorsInterf.RemoveHydrologyGrowthFactor(LHydrologyFactors.GaugeNumber);

                  Result := LGrowthFactors.ClearAllProjectionDataFromDB;
                end;
              end;
              // remove BDF - Disbenefit channel Data ....
              if LFeature.Channel.DisbenefitFunction <> nil then
                Result :=  Result and LFeature.Channel.RemoveDisbenefitFunction;
              // remove RTN - Return Flow Channel ....
              if LFeature.Channel.ReturnFlowChannel <> nil then
                Result :=  Result and  LFeature.Channel.RemoveReturnFlowChannel;
            end;
          end;
          DeleteMasterControlFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.DeleteMasterControlFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TMasterControlFeatureList.DeleteMasterControlFeatureWithID';
var
  lFeature : TMasterControlFeature;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastMasterControlFeatureByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FMasterControlFeatureList.IndexOf(lFeature);
      Result := DeleteMasterControlFeatureWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.DeleteMasterControlFeatureWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TMasterControlFeatureList.DeleteMasterControlFeatureWithIndex';
var
  lFeature : TMasterControlFeature;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TMasterControlFeature(FMasterControlFeatureList.Items[AIndex]);
      FMasterControlFeatureList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Get_MasterControlFeatureByIndex(AIndex: integer): IMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.Get_MasterControlFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMasterControlFeatureList.Count) then
      Result := TMasterControlFeature(FMasterControlFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Get_DemandCentreByID (ADemandCentreID : integer): IMasterControlFeature; safecall;
const OPNAME = 'TMasterControlFeatureList.Get_DemandCentreByID';
var
 lIndex   : integer;
 lFeature : TMasterControlFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMasterControlFeatureList.Count)) do
    begin
      lFeature := TMasterControlFeature(FMasterControlFeatureList.Items[lIndex]);
      if (lFeature.DemandCentreID = ADemandCentreID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Get_MasterControlFeatureByID(AFeatureID : integer): IMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.Get_MasterControlFeatureByID';
var
 lIndex   : integer;
 lFeature : TMasterControlFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMasterControlFeatureList.Count)) do
    begin
      lFeature := TMasterControlFeature(FMasterControlFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.CastMasterControlFeatureByIndex (AIndex : integer): TMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.CastMasterControlFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMasterControlFeatureList.Count) then
      Result := TMasterControlFeature(FMasterControlFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.CastMasterControlFeatureByID(AFeatureID : integer): TMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.CastMasterControlFeatureByID';
var
 lIndex   : integer;
 lFeature : TMasterControlFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMasterControlFeatureList.Count)) do
    begin
      lFeature := TMasterControlFeature(FMasterControlFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.CastMasterControlFeatureByChannelNr(AChannelNr : integer): TMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.CastMasterControlFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TMasterControlFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMasterControlFeatureList.Count)) do
    begin
      lFeature := TMasterControlFeature(FMasterControlFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Get_WaterControlFeature : IMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.Get_WaterControlFeature';
var
 lIndex   : integer;
 lFeature : TMasterControlFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMasterControlFeatureList.Count)) do
    begin
      lFeature := TMasterControlFeature(FMasterControlFeatureList.Items[lIndex]);
      if (Trim(Uppercase(lFeature.FMasterControlType)) = 'W') then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Get_PowerControlFeature : IMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.Get_PowerControlFeature';
var
 lIndex   : integer;
 lFeature : TMasterControlFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMasterControlFeatureList.Count)) do
    begin
      lFeature := TMasterControlFeature(FMasterControlFeatureList.Items[lIndex]);
      if (Trim(Uppercase(lFeature.FMasterControlType)) = 'P') then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Get_MasterControlFeatureCount: integer;
const OPNAME = 'TMasterControlFeatureList.Get_MasterControlFeatureCount';
begin
  Result := 0;
  try
    Result := FMasterControlFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TMasterControlFeatureList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FMasterControlFeatureList.Count - 1 do
    begin
      if (NOT MasterControlFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMasterControlFeatureList.Get_ChannelByChannelNumber: Integer;
const OPNAME = 'TMasterControlFeatureList.Get_ChannelByChannelNumber';
begin
  Result := 0;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;
function TMasterControlFeatureList.CopyMasterControlFeature(ANewChannelNumber, AOldChannelNumber: integer): IMasterControlFeature;
const OPNAME = 'TMasterControlFeatureList.CopyMasterControlFeature';
var
  LMasterControlFeature : TMasterControlFeature;
  LMasterControlFeatureCopy : TMasterControlFeature;
begin
  try
    LMasterControlFeature := CastMasterControlFeatureByID(AOldChannelNumber);
    if (LMasterControlFeature <> nil) then
    begin
      LMasterControlFeatureCopy := CreateNewMasterControlFeature(ANewChannelNumber);
      if LMasterControlFeatureCopy <> nil then
      begin
        LMasterControlFeatureCopy.Assign(ANewChannelNumber,LMasterControlFeature);
        Result := LMasterControlFeatureCopy;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlFeature.Assign(AChannelNumber: integer;AMasterControlFeature: TMasterControlFeature);
const OPNAME = 'TMasterControlFeature.Assign';
var
  LChannel : IGeneralFlowChannel;
  LMonth          : integer;
  LMonthlyFactors : TAbstractFieldProperty;
begin
  try
    if (AMasterControlFeature <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+AMasterControlFeature.FeatureName;
      FeatureType := AMasterControlFeature.FeatureType;
      FeatureSubType := AMasterControlFeature.FeatureSubType;
      MasterControlType := AMasterControlFeature.MasterControlType;
      LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
      for LMonth := LMonthlyFactors.ArrayLow to LMonthlyFactors.ArrayHigh do
        FactorByMonth[LMonth] := AMasterControlFeature.FactorByMonth[LMonth];
      DemandCentreType := AMasterControlFeature.DemandCentreType;
      DemandCentreID := AMasterControlFeature.DemandCentreID;
      AnnualDemand := AMasterControlFeature.AnnualDemand;
      MinimumDemand := AMasterControlFeature.MinimumDemand;
      IncludeInOutput := AMasterControlFeature.IncludeInOutput;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
