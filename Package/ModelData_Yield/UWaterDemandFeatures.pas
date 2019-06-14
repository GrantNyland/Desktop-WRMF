{******************************************************************************}
{*  UNIT      : Contains the class TWaterDemandFeature.                       *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/09/14                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UWaterDemandFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Water Demand Configuration                                                 *}
{******************************************************************************}

  TWaterDemandCategory = class(TAbstractAppObject, IWaterDemandCategory)
  protected
    FCategoryID              : integer;
    FCategoryName            : string;
    FPortionTotal            : double;
    FDemandPortion           : TWaterDemandPortion;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateCategoryName (AErrorMessages : TStrings) : Boolean;
    function ValidateDemandPortions(AErrorMessages : TStrings;
                                    AErrorColumns  : TStringList): boolean;
    function ValidateDemandPortionsTotal(AErrorMessages : TStrings): boolean;
  public
    function Initialise : boolean; override;
    function Populate (ACategoryID    : integer;
                       ACategoryDescr : WideString;
                       ADemandPortion : TWaterDemandPortion ): WordBool;
    function PopulateSome (ACategoryID    : integer;
                           ACategoryDescr : WideString): WordBool;
    function Get_CategoryID : integer; safecall;
    function Get_CategoryName : WideString; safecall;
    procedure Set_CategoryName (const AName : WideString); safecall;
    function Get_DemandPortionByIndex(AIndex : integer) : double; safecall;
    procedure Set_DemandPortionByIndex(AIndex : integer;
                                       AValue : double); safecall;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    function Get_PortionTotal(ACategoryID: Integer): Double; safecall;



    property CategoryID      : integer read Get_CategoryID;
    property CategoryName    : WideString read Get_CategoryName write Set_CategoryName;
    property DemandPortionByIndex[AIndex : integer] : double read Get_DemandPortionByIndex write Set_DemandPortionByIndex;
    property PortionTotal[ACategoryID: Integer]: Double read Get_PortionTotal;
  end;


  TWaterUseOutputProportion = class(TAbstractAppObject, IWaterUseOutputProportion)
  protected
    FChannelNumber   : integer;
    FWaterUsePortion : TWaterDemandPortion;
    FWaterUseTotal   : double;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateWaterUsePortions(AErrorMessages : TStrings; AErrorColumns  : TStringList) : boolean;
    function ValidateWaterUsePortionsTotal(AErrorMessages : TStrings) : boolean;

  public
    procedure PopulateSome(AChannelNumber : integer);
    function Populate(AChannelNumber : integer;
                       AUsePortion    : TWaterDemandPortion ): WordBool;
    function Validate(var AErrors    : WideString;
                        const AContext : WideString) : WordBool; safecall;
    function Get_ChannelNumber : Integer; safecall;
    procedure Set_ChannelNumber(Value : Integer); safecall;
    function Get_ProportionByIndex(AIndex : Integer ) : Double; safecall;
    procedure Set_ProportionByIndex(AIndex : Integer; Value : Double ); safecall;
    function Get_Total : Double; safecall;
    function Initialise : Boolean; override;
    property ChannelNumber : Integer read Get_ChannelNumber write Set_ChannelNumber;
    property ProportionByIndex[AIndex: Integer]: Double read Get_ProportionByIndex write Set_ProportionByIndex;
    property Total : Double read Get_Total;
  end;

  TWaterDemandConfiguration = class(TAbstractAppObject, IWaterDemandConfiguration)
  protected
    FRecurrenceInterval           : TWaterDemandPortion;
    FWaterDemandCategoryList      : TObjectList;
    FWaterUseOutputProportionList : TObjectList;
    FImplementReconciliation      : boolean;
    
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateCategoryCount (AErrorMessages : TStrings) : Boolean;
    function ValidateRiskCriteriaCount (AErrorMessages : TStrings) : Boolean;
    function ValidateRecurrenceIntervals(AErrorMessages : TStrings;
                                         AErrorColumns  : TStringList): boolean;
    function CreateRiskCriteriaRecord: boolean;
  public
    function Initialise : boolean; override;
    function Populate (ARecurrenceInterval : TWaterDemandPortion): WordBool;
    function NewWaterDemandCategory : TWaterDemandCategory;
    function CreateNewWaterDemandCategory : TWaterDemandCategory;
    function CreateNewWaterUseOutputProportion(AChannelNumber : integer) : IWaterUseOutputProportion; safecall;
    function NewWaterUseOutputProportion : TWaterUseOutputProportion;
    function DeleteWaterDemandCategoryWithID(ACategoryID : integer) : WordBool;

    function DeleteWaterDemandCategoryWithIndex(AIndex : integer) : WordBool;
    function CastWaterDemandCategoryByIndex(AIndex : integer): TWaterDemandCategory;
    function CastWaterDemandCategoryByID(ACategoryID: integer): TWaterDemandCategory;
    function CastWaterUseOutputProportionByIndex (AIndex : integer): TWaterUseOutputProportion;
    function CastWaterUseOutputProportionByChannelNr (AChannelNr : integer) : TWaterUseOutputProportion;

    function CreateWaterDemandCategory : IWaterDemandCategory; safecall;
    function RemoveWaterDemandCategoryWithID (ACategoryID : integer) : WordBool; safecall;

    function CreateWaterUseOutputProportion : IWaterUseOutputProportion; safecall;
    function DeleteWaterUseOutputProportion (AChannelNumber : integer) : WordBool; safecall;
    procedure CreateYieldWaterUseOutputProportion; safecall;

    function Get_WaterUseOutputProportionByChannelNumber (AChannelNr : Integer) : IWaterUseOutputProportion; safecall;
    function Get_WaterUseOutputProportionByIndex ( AIndex : Integer ) : IWaterUseOutputProportion; safecall;
    function Get_WaterUseOutputProportionCount : Integer; safecall;
    procedure UpdateWaterUseOutputProportions(ANewCount: Integer); safecall;

    function Get_DemandCategoryCount : integer; safecall;
    function Get_RiskCriteriaCount   : integer; safecall;
    procedure Set_RiskCriteriaCount(AValue: Integer); safecall;
    function Get_RecurrenceIntervalByIndex(AIndex : integer) : double; safecall;
    procedure Set_RecurrenceIntervalByIndex(AIndex : integer;
                                            AValue : double); safecall;
    function Get_DemandCategoryByIndex(AIndex : integer) : IWaterDemandCategory; safecall;
    function Get_DemandCategoryByID(ACategoryID : integer) : IWaterDemandCategory; safecall;
    function Get_DemandCategoryByName(const AName: WideString): IWaterDemandCategory; safecall;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    function Get_ImplementReconciliation: WordBool; safecall;
    procedure Set_ImplementReconciliation(Value: WordBool); safecall;
    function InitialiseFileCreate(AImplementReconciliation : integer) : boolean;

    property DemandCategoryCount : integer read Get_DemandCategoryCount;
    property RiskCriteriaCount : integer read Get_RiskCriteriaCount write Set_RiskCriteriaCount;
    property RecurrenceIntervalByIndex[AIndex : integer] : double
             read Get_RecurrenceIntervalByIndex write Set_RecurrenceIntervalByIndex;
    property DemandCategoryByIndex[AIndex : integer] : IWaterDemandCategory read Get_DemandCategoryByIndex;
    property DemandCategoryByID[ACategoryID : integer] : IWaterDemandCategory read Get_DemandCategoryByID;
    property DemandCategoryByName[const AName: WideString]: IWaterDemandCategory read Get_DemandCategoryByName;

    property WaterUseOutputProportionCount: Integer read Get_WaterUseOutputProportionCount;
    property WaterUseOutputProportionByIndex[AIndex: Integer]: IWaterUseOutputProportion read Get_WaterUseOutputProportionByIndex;
    property WaterUseOutputProportionByChannelNumber[AChannelID: Integer]: IWaterUseOutputProportion read Get_WaterUseOutputProportionByChannelNumber;
    property ImplementReconciliation: WordBool read Get_ImplementReconciliation write Set_ImplementReconciliation;
  end;

{******************************************************************************}
{* Water Demand Feature                                                       *}
{******************************************************************************}

  TWaterDemandFeature = class(TAbstractAppObject, IWaterDemandFeature)
  protected
    FFeatureID           : integer;
    FFeatureName         : string;
    FChannelNr           : integer;
    FFeatureType         : integer;
    FFeatureSubType      : integer;
    FWaterDemandCategory : integer;
    FScenarioPortion     : TWaterDemandPortion;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateWaterScenarioPortion(AErrorMessages : TStrings;
                                    AErrorColumns  : TStringList): boolean;
  public
    procedure Assign(AChannelNumber : integer; AWaterDemandFeature: TWaterDemandFeature);
    function Initialise : boolean; override;
    function Populate (AFeatureID       : integer;
                       AFeatureName     : WideString;
                       AChannelNr       : integer;
                       AFeatureType     : integer;
                       AFeatureSubType  : integer;
                       ACategoryID      : integer;
                       AScenarioPortion : TWaterDemandPortion): WordBool;
    function PopulateSome (AFeatureID      : integer;
                           AFeatureName    : WideString;
                           AChannelNr      : integer;
                           AFeatureType    : integer;
                           AFeatureSubType : integer;
                           ACategoryID     : integer): WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_WaterDemandCategory : integer; safecall;
    procedure Set_WaterDemandCategory (ACategoryID : integer); safecall;
    function Get_ScenarioPortionByIndex(AIndex : integer) : double; safecall;
    procedure Set_ScenarioPortionByIndex(AIndex : integer;AValue : double); safecall;

    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property WaterDemandCategory : integer read Get_WaterDemandCategory write Set_WaterDemandCategory;
    property ScenarioPortionByIndex[AIndex : integer] : double read Get_ScenarioPortionByIndex write Set_ScenarioPortionByIndex;
  end;

  TWaterDemandFeatureList = class(TAbstractAppObject, IWaterDemandFeatureList)
  protected
    FScenarioCount           : integer;
    FWaterDemandFeatureList  : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddWaterDemandFeature (AData : TWaterDemandFeature): boolean;
    function CreateScenarioCount: boolean;
    function ValidateWaterDemandScenarioCount(AErrorMessages : TStrings) : Boolean;
  public
    function Initialise: boolean; override;
    function PopulateScenarioCount(AScenarioCount : integer): WordBool;
    function NewWaterDemandFeature : TWaterDemandFeature;
    function CastWaterDemandFeatureByIndex (AIndex : integer): TWaterDemandFeature;
    function CastWaterDemandFeatureByID(AFeatureID: integer): TWaterDemandFeature;
    function CastWaterDemandFeatureByChannelNr(AChannelNr: integer): TWaterDemandFeature;
    function DeleteWaterDemandFeatureWithID(AFeatureID : integer) : WordBool;
    function DeleteWaterDemandFeatureWithIndex(AIndex : integer) : WordBool;
    function CreateNewWaterDemandFeature : TWaterDemandFeature;

    function CreateWaterDemandFeature : IWaterDemandFeature; safecall;
    function CopyWaterDemandFeature(ANewChannelNumber : integer;AOldChannelNumber : integer) : IWaterDemandFeature; safecall;
    function RemoveWaterDemandFeatureWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_ScenarioCount       : integer; safecall;
    procedure Set_ScenarioCount (ACount : integer); safecall;
    function Get_WaterDemandFeatureByIndex(AIndex: integer): IWaterDemandFeature; safecall;
    function Get_WaterDemandFeatureByID(AFeatureID: integer): IWaterDemandFeature; safecall;
    function Get_WaterDemandFeatureCount: integer; safecall;
    function Validate (var AErrors : WideString;
                       const AContext    : WideString) : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property ScenarioCount : integer read Get_ScenarioCount write Set_ScenarioCount;
    property WaterDemandFeatureByIndex[AIndex : integer]: IWaterDemandFeature
      read Get_WaterDemandFeatureByIndex;
    property WaterDemandFeatureByID[AFeatureID: integer]: IWaterDemandFeature
      read Get_WaterDemandFeatureByID;
    property WaterDemandFeatureCount: integer read Get_WaterDemandFeatureCount;
  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UNetworkFeaturesSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{ TWaterDemandCategory                                                         }
{******************************************************************************}

function TWaterDemandCategory._AddRef: Integer;
const OPNAME = 'TWaterDemandCategory._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory._Release: Integer;
const OPNAME = 'TWaterDemandCategory._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandCategory.CreateMemberObjects;
const OPNAME = 'TWaterDemandCategory.CreateMemberObjects';
var
  LRiskCriteria : TAbstractFieldProperty;
begin
  inherited;
  try
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if (LRiskCriteria = nil) then
      raise Exception.Create('Field (RecurrenceInterval) not found in field properties');
    SetLength(FDemandPortion, LRiskCriteria.ArrayLength);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandCategory.DestroyMemberObjects;
const OPNAME = 'TWaterDemandCategory.DestroyMemberObjects';
begin
  try
    Finalize(FDemandPortion);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.Initialise: boolean;
const OPNAME = 'TWaterDemandCategory.Initialise';
var
  lIndex        : integer;
  LRiskCriteria : TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FCategoryID    := 0;
    FCategoryName  := '';
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    for lIndex := LRiskCriteria.ArrayLow to LRiskCriteria.ArrayHigh do
      FDemandPortion[lIndex] := NullFloat;
    FPortionTotal := NullFloat;  
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.Populate (ACategoryID    : integer;
                                        ACategoryDescr : WideString;
                                        ADemandPortion : TWaterDemandPortion): WordBool;
const OPNAME = 'TWaterDemandCategory.Populate';
var
  lIndex    : integer;
  lCriteria : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FCategoryID   := ACategoryID;
    FCategoryName := ACategoryDescr;
    FPortionTotal := PortionTotal [ ACategoryID ];
    lCriteria     := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    for lIndex := lCriteria.ArrayLow to lCriteria.ArrayHigh do
      FDemandPortion[lIndex] := ADemandPortion[lIndex];

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.PopulateSome (ACategoryID    : integer;
                                            ACategoryDescr : WideString): WordBool;
const OPNAME = 'TWaterDemandCategory.PopulateSome';
begin
  Result := FALSE;
  try
    FCategoryID   := ACategoryID;
    FCategoryName := ACategoryDescr;
    FPortionTotal := 0.0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.Get_CategoryID : integer;
const OPNAME = 'TWaterDemandCategory.Get_CategoryID';
begin
  Result := 0;
  try
    Result := FCategoryID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.Get_CategoryName : WideString;
const OPNAME = 'TWaterDemandCategory.Get_CategoryName';
begin
  Result := '';
  try
    Result := FCategoryName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandCategory.Set_CategoryName (const AName : WideString);
const OPNAME = 'TWaterDemandCategory.Set_CategoryName';
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
        LLoadAgent.LoadContextData_WaterDemandCategoryID(LContextData, IntToStr(FCategoryID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'WaterDemandCategoryName', AName, FCategoryName, LContextData) then
        begin
          LOldValue := FCategoryName;
          FCategoryName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterDemandCategoryName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.Get_DemandPortionByIndex(AIndex : integer) : double;
const OPNAME = 'TWaterDemandCategory.Get_DemandPortionByIndex';
var
  LRiskCriteria : TAbstractFieldProperty;
begin
  Result := 0.0;
  try
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if (AIndex >= LRiskCriteria.ArrayLow) AND (AIndex <= LRiskCriteria.ArrayHigh) then
      Result := FDemandPortion[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandCategory.Set_DemandPortionByIndex(AIndex : integer;
                                                        AValue : double);
const OPNAME = 'TWaterDemandCategory.Set_DemandPortionByIndex';
var
  LRiskCriteria : TAbstractFieldProperty;
  LLoadAgent    : TNetworkFeaturesSQLAgent;
  LContextData  : TStringList;
  lOldValue     : string;
  lNewValue     : string;
  LPrevValue    : double;
begin
  try
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if (AIndex >= LRiskCriteria.ArrayLow) and (AIndex <= LRiskCriteria.ArrayHigh) then
    begin
      if (FDemandPortion[AIndex] <> AValue) then
      begin
        LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData_WaterDemandCategoryIDFieldNameID
              (LContextData, IntToStr(FCategoryID), IntToStr(AIndex));

            if (AValue = NullFloat) then
              lNewValue := ''
            else
              lNewValue := FloatToStr(AValue);
            if (FDemandPortion[AIndex] = NullFloat) then
              lOldValue := ''
            else
              lOldValue := FloatToStr(FDemandPortion[AIndex]);
            if FAppModules.FieldProperties.UpdateFieldValue(
              'DemandPortion', lNewValue, lOldValue, LContextData) then
            begin
              LPrevValue := FDemandPortion[AIndex];
              FDemandPortion[AIndex] := AValue;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'DemandPortion',FloatToStr(LPrevValue),FloatToStr(AValue));
            end;
          finally
            LContextData.Free;
          end;
        finally
          LLoadAgent.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.Validate (var AErrors    : WideString;
                                        const AContext : WideString) : WordBool;
const OPNAME = 'TWaterDemandCategory.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorCols        : TStringList;
  lErrorMsgs        : TStringList;
begin
  Result := FALSE;
  try
    lErrorCols := TStringList.Create;
    lErrorMsgs := TStringList.Create;
    try
      if (AContext = 'CategoryName') then
        Result := ValidateCategoryName(lErrorMsgs)
      else
      if (AContext = 'DemandPortionTotal') then
        Result := ValidateDemandPortionsTotal(lErrorMsgs)
      else
      if (AContext = 'DemandPortion') then
      begin
        Result := ValidateDemandPortions(lErrorMsgs,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMsgs.Text
          else
            AErrors := AErrors + CTStringsSeparator + lErrorMsgs.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          lErrorMsgs.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateCategoryName(lErrorMsgs)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDemandPortions(lErrorMsgs,lErrorCols)) then
          begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorMsgs.Text
            else
              AErrors := AErrors + CTStringsSeparator + lErrorMsgs.Text +
                                   CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorMsgs.Clear;
            lErrorCols.Clear;
          end;
        end;
      end;
      AErrors := AErrors + lErrorMsgs.Text
    finally
      FreeAndNil(lErrorCols);
      FreeAndNil(lErrorMsgs);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandCategory.ValidateCategoryName (AErrorMessages : TStrings) : Boolean;
const OPNAME = 'TWaterDemandCategory.ValidateCategoryName';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('WaterDemandCategoryName', Trim(FCategoryName), lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage);
    //else
    //  Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandCategory.ValidateDemandPortions (AErrorMessages : TStrings;
                                                      AErrorColumns  : TStringList): boolean;
const OPNAME = 'TWaterDemandCategory.ValidateDemandPortions';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lIndex            : integer;
  lRiskCriteria     : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lRiskCriteria := FAppModules.FieldProperties.FieldProperty('DemandPortion');
    if (lRiskCriteria <> nil) then
    begin
      AErrorColumns.Clear;
      lResult := TRUE;
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := lRiskCriteria.ArrayLow to lRiskCriteria.ArrayHigh do
      begin
        lMessage := '';
        if (FDemandPortion[lIndex] <> NullFloat) then
        begin
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('DemandPortion', FloatToStr(FDemandPortion[lIndex]),
             lMessage, lIndex)) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +lMessage);
            AErrorColumns.Add(IntToStr(lIndex));
            if (lStopOnFirstError) then
              Break;
          end;
        end;
      end;
      Result := lResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandCategory.ValidateDemandPortionsTotal (AErrorMessages : TStrings): boolean;
const OPNAME = 'TWaterDemandCategory.ValidateDemandPortionsTotal';
var
  lMessage          : string;
  lResult           : Boolean;
  lFieldProperty     : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('DemandPortionTotal');
    if (lFieldProperty <> nil) then
    begin
      lResult := TRUE;
      begin
        lMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('DemandPortionTotal', FloatToStr(FPortionTotal),
             lMessage)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
        end;
      end;
      Result := lResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TWaterDemandCategory.GetKeyValues (const AParamField : WideString;
                                            const AFieldIndex : WideString) : WideString;
const OPNAME = 'TWaterDemandCategory.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'CategoryID=' + IntToStr(FCategoryID)
    else
      Result := Result + ',CategoryID=' + IntToStr(FCategoryID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandCategory.Get_PortionTotal ( ACategoryID : Integer ) : double;
const OPNAME = 'TWaterDemandCategory.Get_PortionTotal';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
begin
  Result := 0;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LLoadAgent.AddWaterDemandPortionTotal ( ACategoryID, FPortionTotal );
    finally
      LLoadAgent.Free;
    end;
    Result := FPortionTotal;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


{******************************************************************************}
{ TWaterDemandConfiguration                                                    }
{******************************************************************************}

function TWaterDemandConfiguration._AddRef: Integer;
const OPNAME = 'TWaterDemandConfiguration._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration._Release: Integer;
const OPNAME = 'TWaterDemandConfiguration._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandConfiguration.CreateMemberObjects;
const OPNAME = 'TWaterDemandConfiguration.CreateMemberObjects';
var
  LRiskCriteria : TAbstractFieldProperty;
begin
  try
    inherited CreateMemberObjects;
    FWaterDemandCategoryList := TObjectList.Create;
    FWaterUseOutputProportionList    := TObjectList.Create;
     
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if (LRiskCriteria = nil) then
      raise Exception.Create('Field (RecurrenceInterval) not found in field properties');
    SetLength(FRecurrenceInterval, LRiskCriteria.ArrayLength);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandConfiguration.DestroyMemberObjects;
const OPNAME = 'TWaterDemandConfiguration.DestroyMemberObjects';
begin
  try
    FreeAndNil(FWaterDemandCategoryList);
    FreeAndNil ( FWaterUseOutputProportionList );
    Finalize(FRecurrenceInterval);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Initialise: boolean;
const OPNAME = 'TWaterDemandConfiguration.Initialise';
var
  lIndex        : integer;
  LRiskCriteria : TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FWaterDemandCategoryList.Clear;
    FWaterUseOutputProportionList.Clear;
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    for lIndex := LRiskCriteria.ArrayLow to LRiskCriteria.ArrayHigh do
      FRecurrenceInterval[lIndex] := NullFloat;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Populate (ARecurrenceInterval : TWaterDemandPortion): WordBool;
const OPNAME = 'TWaterDemandConfiguration.Populate';
var
  lIndex    : integer;
  lCriteria : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    for lIndex := lCriteria.ArrayLow to lCriteria.ArrayHigh do
      FRecurrenceInterval[lIndex] := ARecurrenceInterval[lIndex];
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.NewWaterDemandCategory : TWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.NewWaterDemandCategory';
var
  lCategory : TWaterDemandCategory;
begin
  Result := nil;
  try
    lCategory := TWaterDemandCategory.Create(FAppModules);
    FWaterDemandCategoryList.Add(lCategory);
    Result := lCategory;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CreateNewWaterDemandCategory : TWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.CreateNewWaterDemandCategory';
var
  lIndex              : integer;
  lNewIdentifier      : integer;
  lLoadAgent          : TNetworkFeaturesSQLAgent;
  lCategory           : TWaterDemandCategory;
  lCategoryName       : string;
  lRiskCriteriaCount  : TAbstractFieldProperty;
begin
  Result := nil;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LNewIdentifier := 0;
      lRiskCriteriaCount := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');

      if LLoadAgent.InsertWaterDemandCategory(LNewIdentifier) then
      begin
        lCategory := NewWaterDemandCategory;
        lCategory.Initialise;
        lCategoryName := UpperCase(FAppModules.Language.GetString('TField.WaterDemandCategory')) + ' ' + IntToStr(lNewIdentifier);
        lCategory.PopulateSome(LNewIdentifier, lCategoryName);
        for lIndex := lRiskCriteriaCount.ArrayLow to RiskCriteriaCount do
          lCategory.DemandPortionByIndex[lIndex] := 0;
        Result := lCategory;

        if (LNewIdentifier = 1) then
        begin
          lLoadAgent.InsertImplementReconciliationFile;
          FImplementReconciliation := True;
        end;

      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CreateWaterDemandCategory : IWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.CreateWaterDemandCategory';
var
  lCategory : TWaterDemandCategory;
begin
  Result := nil;
  try
    lCategory := CreateNewWaterDemandCategory;
    Result := lCategory;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CreateNewWaterUseOutputProportion (AChannelNumber : integer) : IWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.CreateNewWaterUseOutputProportion';
var
  LLoadAgent  : TNetworkFeaturesSQLAgent;
  lProportion : TWaterUseOutputProportion;
begin
  Result := nil;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create ( FAppModules );
    try
      if LLoadAgent.InsertWaterUseOutputProportion(AChannelNumber) then
      begin
        lProportion := NewWaterUseOutputProportion;
        lProportion.PopulateSome(AChannelNumber);
        Result := lProportion;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.NewWaterUseOutputProportion : TWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.NewWaterUseOutputProportion';
var
  LWaterUseOutputProportion : TWaterUseOutputProportion;
begin
  Result := nil;
  try
    LWaterUseOutputProportion := TWaterUseOutputProportion.Create(FAppModules);
    LWaterUseOutputProportion.Initialise;
    FWaterUseOutputProportionList.Add ( LWaterUseOutputProportion );
    Result := LWaterUseOutputProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CreateWaterUseOutputProportion : IWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.CreateWaterUseOutputProportion';
var
  LWaterUseOutputProportion : TWaterUseOutputProportion;
begin
  Result := nil;
  try
    LWaterUseOutputProportion := NewWaterUseOutputProportion;
    Result := LWaterUseOutputProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.RemoveWaterDemandCategoryWithID (ACategoryID : integer) : WordBool;
const OPNAME = 'TWaterDemandConfiguration.RemoveWaterDemandCategoryWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (ACategoryID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteWaterDemandCategory(ACategoryID) then
        begin
          DeleteWaterDemandCategoryWithID(ACategoryID);
          Result := TRUE;

          if FWaterDemandCategoryList.Count = 0 then
            lLoadAgent.DeleteImplementReconciliationFile;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function  TWaterDemandConfiguration.DeleteWaterUseOutputProportion (AChannelNumber : integer) : WordBool;
const OPNAME = 'TWaterDemandConfiguration.DeleteWaterUseOutputProportion';
var
  lLoadAgent  : TNetworkFeaturesSQLAgent;
  lProportion : TWaterUseOutputProportion;
  lIndex      : integer;
begin
  Result := False;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      if (LLoadAgent.DeleteWaterUseOutputProportion(AChannelNumber))  then
      begin
        lProportion := CastWaterUseOutputProportionByChannelNr(AChannelNumber);
        if (lProportion <> nil) then
        begin
          lIndex := FWaterUseOutputProportionList.IndexOf(lProportion);
          if (lIndex >= 0) then
          begin
            FWaterUseOutputProportionList.Delete(lIndex);
            Result := TRUE;
          end;  
        end;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.DeleteWaterDemandCategoryWithID(ACategoryID : integer) : WordBool;
const OPNAME = 'TWaterDemandConfiguration.DeleteWaterDemandCategoryWithID';
var
  lCategory : TWaterDemandCategory;
  lIndex    : integer;
begin
  Result := FALSE;
  try
    lCategory := CastWaterDemandCategoryByID(ACategoryID);
    if (lCategory <> nil) then
    begin
      lIndex := FWaterDemandCategoryList.IndexOf(lCategory);
      Result := DeleteWaterDemandCategoryWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.DeleteWaterDemandCategoryWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TWaterDemandConfiguration.DeleteWaterDemandCategoryWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      FWaterDemandCategoryList.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Get_DemandCategoryCount : integer;
const OPNAME = 'TWaterDemandConfiguration.Get_DemandCategoryCount';
begin
  Result := 0;
  try
    Result := FWaterDemandCategoryList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Get_WaterUseOutputProportionCount : Integer; safecall;
const OPNAME = 'TWaterDemandConfiguration.Get_WaterUseOutputProportionCount';
begin
  Result := 0;
  try
    Result := FWaterUseOutputProportionList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TWaterDemandConfiguration.UpdateWaterUseOutputProportions (ANewCount : Integer);
const OPNAME = 'TWaterDemandConfiguration.UpdateWaterUseOutputProportions';
var
  lIndex      : integer;
  lCount      : integer;
  lProportion : TWaterUseOutputProportion;
  lProperty   : TAbstractFieldProperty;
begin
  try
    lProperty := FAppModules.FieldProperties.FieldProperty('WaterUsePortion');
    if not Assigned(lProperty) then
      raise Exception.Create('Field (WaterUsePortion) not found in field properties');
    if (ANewCount > 0) then
    begin
      for lCount := 0 to FWaterUseOutputProportionList.Count - 1 do
      begin
        lProportion := CastWaterUseOutputProportionByIndex(lCount);
        if (lProportion <> nil) then
        begin
          for lIndex := ANewCount + 1 to LProperty.ArrayHigh do
            lProportion.ProportionByIndex[lIndex] := 0;
        end;
      end;  
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



function TWaterDemandConfiguration.Get_RiskCriteriaCount : integer;
const OPNAME = 'TWaterDemandConfiguration.Get_RiskCriteriaCount';
var
  lIndex        : integer;
  LRiskCriteria : TAbstractFieldProperty;
begin
  Result := 0;
  try
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    lIndex := LRiskCriteria.ArrayLow;
    while ((lIndex <= LRiskCriteria.ArrayHigh) AND (FRecurrenceInterval[lIndex] <> NullFloat)) do
      lIndex := lIndex + 1;
    if (lIndex >= 1) then
      Result := lIndex - 1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandConfiguration.Set_RiskCriteriaCount(AValue: Integer);
const OPNAME = 'TWaterDemandConfiguration.Set_RiskCriteriaCount';
var
  lOldCount   : integer;
  lNewCount   : integer;
  lIndex      : integer;
  lCount      : integer;
  lCategory   : IWaterDemandCategory;
begin
  try
    lOldCount := RiskCriteriaCount;
    if lOldCount = AValue then Exit;
    lNewCount := AValue;
    if(lOldCount <= 0) then
    begin
      if not CreateRiskCriteriaRecord then
        Exit;
    end;

    if (lNewCount > lOldCount) then
    begin
      for lIndex := lOldCount + 1 to lNewCount do
      begin
        RecurrenceIntervalByIndex[lIndex] := 0;
        for lCount := 0 to DemandCategoryCount - 1 do
        begin
          lCategory := DemandCategoryByIndex[lCount];
          lCategory.DemandPortionByIndex[lIndex] := 0;
        end;
      end;
    end
    else
    begin
      for lIndex := lNewCount + 1 to lOldCount do
      begin
        RecurrenceIntervalByIndex[lIndex] := NullFloat;
        for lCount := 0 to DemandCategoryCount - 1 do
        begin
          lCategory := DemandCategoryByIndex[lCount];
          lCategory.DemandPortionByIndex[lIndex] := NullFloat;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Get_RecurrenceIntervalByIndex(AIndex : integer) : double;
const OPNAME = 'TWaterDemandConfiguration.Get_RecurrenceIntervalByIndex';
var
  LRiskCriteria : TAbstractFieldProperty;
begin
  Result := 0.0;
  try
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if (AIndex >= LRiskCriteria.ArrayLow) AND (AIndex <= LRiskCriteria.ArrayHigh) then
      Result := FRecurrenceInterval[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandConfiguration.Set_RecurrenceIntervalByIndex(AIndex : integer;
                                                                  AValue : double);
const OPNAME = 'TWaterDemandConfiguration.Set_RecurrenceIntervalByIndex';
var
  LRiskCriteria : TAbstractFieldProperty;
  LLoadAgent    : TNetworkFeaturesSQLAgent;
  LContextData  : TStringList;
  lOldValue     : string;
  lNewValue     : string;
  LPrevValue    : double;
begin
  try
    LRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if (AIndex >= LRiskCriteria.ArrayLow) and (AIndex <= LRiskCriteria.ArrayHigh) then
    begin
      if (FRecurrenceInterval[AIndex] <> AValue) then
      begin
        LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData_FieldNameID
              (LContextData, IntToStr(AIndex));
            if (AValue = NullFloat) then
              lNewValue := ''
            else
              lNewValue := FloatToStr(AValue);
            if (FRecurrenceInterval[AIndex] = NullFloat) then
              lOldValue := ''
            else
              lOldValue := FloatToStr(FRecurrenceInterval[AIndex]);
            if FAppModules.FieldProperties.UpdateFieldValue(
              'RecurrenceInterval', lNewValue, lOldValue, LContextData) then
            begin
              LPrevValue := FRecurrenceInterval[AIndex];
              FRecurrenceInterval[AIndex] := AValue;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'RecurrenceInterval',FloatToStr(LPrevValue),FloatToStr(AValue));
            end;
          finally
            LContextData.Free;
          end;
        finally
          LLoadAgent.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Get_DemandCategoryByIndex(AIndex : integer) : IWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.Get_DemandCategoryByIndex';
var
  lCategory : TWaterDemandCategory;
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FWaterDemandCategoryList.Count)) then
    begin
      lCategory := TWaterDemandcategory(FWaterDemandCategoryList.Items[AIndex]);
      Result := lCategory;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Get_DemandCategoryByID(ACategoryID : integer) : IWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.Get_DemandCategoryByID';
var
  lIndex    : integer;
  lCategory : TWaterDemandCategory;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FWaterDemandCategoryList.Count)) do
    begin
      lCategory := TWaterDemandCategory(FWaterDemandCategoryList.Items[lIndex]);
      if (lCategory.CategoryID = ACategoryID) then
        Result := lCategory
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Get_DemandCategoryByName(const AName: WideString): IWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.Get_DemandCategoryByName';
var
  lIndex    : integer;
  lCategory : TWaterDemandCategory;
begin
  Result := nil;
  try
    for lIndex  := 0 to FWaterDemandCategoryList.Count -1  do
    begin
      lCategory := TWaterDemandCategory(FWaterDemandCategoryList.Items[lIndex]);
      if (lCategory.FCategoryName = AName) then
      begin
        Result := lCategory;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CastWaterDemandCategoryByIndex (AIndex : integer): TWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.CastWaterDemandCategoryByIndex';
begin
  Result := nil;
  try
    Result := TWaterDemandCategory(FWaterDemandCategoryList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CastWaterDemandCategoryByID(ACategoryID: integer): TWaterDemandCategory;
const OPNAME = 'TWaterDemandConfiguration.CastWaterDemandCategoryByID';
var
 lIndex    : integer;
 lCategory : TWaterDemandCategory;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FWaterDemandCategoryList.Count)) do
    begin
      lCategory := TWaterDemandCategory(FWaterDemandCategoryList.Items[lIndex]);
      if (lCategory.CategoryID = ACategoryID) then
        Result := lCategory
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CastWaterUseOutputProportionByIndex ( AIndex : integer ) : TWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.CastWaterUseOutputProportionByIndex';
begin
  Result := nil;
  try
    Result := TWaterUseOutputProportion ( FWaterUseOutputProportionList.Items [ AIndex ] );
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CastWaterUseOutputProportionByChannelNr (AChannelNr : integer) : TWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.CastWaterUseOutputProportionByChannelNr';
var
 LIndex    : integer;
 LCategory : TWaterUseOutputProportion;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (LIndex < FWaterUseOutputProportionList.Count)) do
    begin
      LCategory := TWaterUseOutputProportion(FWaterUseOutputProportionList.Items[LIndex]);
      if (LCategory.ChannelNumber = AChannelNr) then
        Result := LCategory
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.ValidateCategoryCount (AErrorMessages : TStrings) : Boolean;
const OPNAME = 'TWaterDemandConfiguration.ValidateCategoryCount';
var
  lMessage : string;
  lValue   : integer;
begin
  Result := FALSE;
  try
    lMessage := '';
    lValue   := DemandCategoryCount;
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
              ('WaterDemandCategoryCount', IntToStr(lValue), lMessage)) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.ValidateRiskCriteriaCount (AErrorMessages : TStrings) : Boolean;
const OPNAME = 'TWaterDemandConfiguration.ValidateRiskCriteriaCount';
var
  lMessage : string;
  lValue   : integer;
begin
  Result := FALSE;
  try
    lMessage := '';
    lValue   := RiskCriteriaCount;
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
              ('WaterDemandRiskCriteriaCount', IntToStr(lValue), lMessage)) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.ValidateRecurrenceIntervals (AErrorMessages : TStrings;
                                                                AErrorColumns  : TStringList): boolean;
const OPNAME = 'TWaterDemandConfiguration.ValidateRecurrenceIntervals';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lIndex            : integer;
  lRiskCriteria     : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lRiskCriteria := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if (lRiskCriteria <> nil) then
    begin
      AErrorColumns.Clear;
      lResult := TRUE;
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := 1 to RiskCriteriaCount do
      begin
        lMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
           ('RecurrenceInterval', FloatToStr(FRecurrenceInterval[lIndex]),
           lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      Result := lResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandConfiguration.Validate (var AErrors    : WideString;
                                             const AContext : WideString) : WordBool;
const OPNAME = 'TWaterDemandConfiguration.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorCols        : TStringList;
  lErrorMsgs        : TStringList;
begin
  Result := FALSE;
  try
    lErrorCols := TStringList.Create;
    lErrorMsgs := TStringList.Create;
    try
      if (AContext = 'CategoryCount') then
        Result := ValidateCategoryCount(lErrorMsgs)
      else
      if (AContext = 'RiskCriteriaCount') then
        Result := ValidateRiskCriteriaCount(lErrorMsgs)
      else
      if (AContext = 'RecurrenceIntervals') then
      begin
        Result := ValidateRecurrenceIntervals(lErrorMsgs,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMsgs.Text
          else
            AErrors := AErrors + CTStringsSeparator + lErrorMsgs.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          lErrorMsgs.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateCategoryCount(lErrorMsgs)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateRiskCriteriaCount(lErrorMsgs)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateRecurrenceIntervals(lErrorMsgs,lErrorCols)) then
          begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorMsgs.Text
            else
              AErrors := AErrors + CTStringsSeparator + lErrorMsgs.Text +
                                   CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorMsgs.Clear;
            lErrorCols.Clear;
          end;
        end;
      end;
      AErrors := AErrors + lErrorMsgs.Text
    finally
      FreeAndNil(lErrorCols);
      FreeAndNil(lErrorMsgs);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandConfiguration.GetKeyValues (const AParamField : WideString;
                                                 const AFieldIndex : WideString) : WideString;
const OPNAME = 'TWaterDemandConfiguration.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandConfiguration.Get_ImplementReconciliation: WordBool;
const OPNAME = 'TWaterDemandConfiguration.Get_ImplementReconciliation';
begin
  Result := False;
  try
    Result := FImplementReconciliation
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterDemandConfiguration.Set_ImplementReconciliation(Value: WordBool);
const OPNAME = 'TWaterDemandConfiguration.Set_ImplementReconciliation';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LNewValue    : string;
begin
  try
    if FImplementReconciliation then
      LOldValue := '1'
    else
      LOldValue := '0';

    if Value then
      LNewValue := '1'
    else
      LNewValue := '0';

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'WaterDemandFileCreate',LNewValue,LOldValue, LContextData) then
        begin
          FImplementReconciliation := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterDemandFileCreate',LOldValue, LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.InitialiseFileCreate(AImplementReconciliation: integer): boolean;
const OPNAME = 'TWaterDemandConfiguration.InitialiseFileCreate';
begin
  Result := FALSE;
  try
    if AImplementReconciliation = 1 then
      FImplementReconciliation := TRUE
    else
      FImplementReconciliation := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandConfiguration.CreateYieldWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.CreateYieldWaterUseOutputProportion';
var
  lChannelNumber  : integer;
  lFeature        : IMasterControlFeature;
  lYieldModelData : IYieldModelData;
  lYieldList      : IMasterControlFeatureList;
  lIndex          : integer;
  lCount          : integer;
  lExist          : Boolean;
begin
  try
    lYieldModelData := (FAppModules.Model.ModelData  as IYieldModelData);
    lYieldList      := lYieldModelData.NetworkFeaturesData.MasterControlFeatureList;
    for lIndex := 0 to lYieldList.MasterControlFeatureCount - 1 do
    begin
      lFeature       := lYieldList.MasterControlFeatureByIndex[lIndex];
      lChannelNumber := lFeature.Channel.ChannelNumber;
      lExist := FALSE;
      lCount := 0;
      while ((NOT lExist) AND (lCount < FWaterUseOutputProportionList.Count)) do
      begin
        if (TWaterUseOutputProportion(FWaterUseOutputProportionList.Items[lCount]).FChannelNumber = lChannelNumber) then
          lExist := TRUE
        else
          lCount := lCount + 1;
      end;
      if (NOT lExist) then
        CreateNewWaterUseOutputProportion(lChannelNumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TWaterDemandFeature                                                          }
{******************************************************************************}

function TWaterDemandFeature._AddRef: Integer;
const OPNAME = 'TWaterDemandFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature._Release: Integer;
const OPNAME = 'TWaterDemandFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeature.CreateMemberObjects;
const OPNAME = 'TWaterDemandFeature.CreateMemberObjects';
var
  LScenarioCount : TAbstractFieldProperty;
begin
  inherited;
  try
    LScenarioCount := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
    if (LScenarioCount = nil) then
      raise Exception.Create('Field (ScenarioPortion) not found in field properties');
    SetLength(FScenarioPortion, LScenarioCount.ArrayLength);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeature.DestroyMemberObjects;
const OPNAME = 'TWaterDemandFeature.DestroyMemberObjects';
begin
  try
    Finalize(FScenarioPortion);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Initialise: boolean;
const OPNAME = 'TWaterDemandFeature.Initialise';
var
  lIndex         : integer;
  LScenarioCount : TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelNr             := 0;
    FFeatureID             := -1;
    FFeatureName           := '';
    FFeatureType           := -1;
    FFeatureSubType        := -1;
    FWaterDemandCategory   := 0;
    LScenarioCount := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
    for lIndex := LScenarioCount.ArrayLow to LScenarioCount.ArrayHigh do
      FScenarioPortion[lIndex] := NullFloat;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Get_FeatureID : integer;
const OPNAME = 'TWaterDemandFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Get_FeatureName : WideString;
const OPNAME = 'TWaterDemandFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TWaterDemandFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Get_FeatureType : integer;
const OPNAME = 'TWaterDemandFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Get_FeatureSubType : integer;
const OPNAME = 'TWaterDemandFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Get_WaterDemandCategory : integer;
const OPNAME = 'TWaterDemandFeature.Get_WaterDemandCategory';
begin
  Result := 0;
  try
    Result := FWaterDemandCategory;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TWaterDemandFeature.Set_FeatureType';
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

procedure TWaterDemandFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TWaterDemandFeature.Set_FeatureSubType';
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

function TWaterDemandFeature.Get_ScenarioPortionByIndex(AIndex : integer) : double;
const OPNAME = 'TWaterDemandFeature.Get_ScenarioPortionByIndex';
begin
  Result := 0.0;
  try
    Result := FScenarioPortion[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeature.Set_FeatureName (const AName : WideString);
const OPNAME = 'TWaterDemandFeature.Set_FeatureName';
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
        LLoadAgent.LoadContextData_WaterFeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'WaterDemandFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterDemandFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TWaterDemandFeature.Set_Channel';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LNewNr       : integer;
  LOldNr       : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (AChannel <> nil) then
          LNewNr := AChannel.ChannelNumber
        else
          LNewNr := 0;
        LLoadAgent.LoadContextData_WaterFeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'WaterDemandChannelNumber', IntToStr(LNewNr), IntToStr(FChannelNr), LContextData) then
        begin
          LOldNr     := FChannelNr;
          FChannelNr := lNewNr;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'WaterDemandChannelNumber',IntToStr(LOldNr),IntToStr(LNewNr));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeature.Set_WaterDemandCategory (ACategoryID : integer);
const OPNAME = 'TWaterDemandFeature.Set_WaterDemandCategory';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldNr       : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_WaterFeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'WaterDemandCategory', IntToStr(ACategoryID), IntToStr(FWaterDemandCategory), LContextData) then
        begin
          LOldNr               := FWaterDemandCategory;
          FWaterDemandCategory := ACategoryID;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'WaterDemandCategory',IntToStr(LOldNr),IntToStr(ACategoryID));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeature.Set_ScenarioPortionByIndex (AIndex : integer;
                                                          AValue : double);
const OPNAME = 'TWaterDemandFeature.Set_ScenarioPortionByIndex';
var
  LScenarioCount : TAbstractFieldProperty;
  LLoadAgent     : TNetworkFeaturesSQLAgent;
  LContextData   : TStringList;
  lOldValue      : string;
  lNewValue      : string;
  LPrevValue     : double;
begin
  try
    LScenarioCount := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
    if (AIndex >= LScenarioCount.ArrayLow) and (AIndex <= LScenarioCount.ArrayHigh) then
    begin
      if (FScenarioPortion[AIndex] <> AValue) then
      begin
        LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData_WaterFeatureIDFieldNameID
              (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
            if (AValue = NullFloat) then
              lNewValue := ''
            else
              lNewValue := FloatToStr(AValue);
            if (FScenarioPortion[AIndex] = NullFloat) then
              lOldValue := ''
            else
              lOldValue := FloatToStr(FScenarioPortion[AIndex]);
            if FAppModules.FieldProperties.UpdateFieldValue(
              'ScenarioPortion', lNewValue, lOldValue, LContextData) then
            begin
              LPrevValue := FScenarioPortion[AIndex];
              FScenarioPortion[AIndex] := AValue;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'ScenarioPortion',FloatToStr(LPrevValue),FloatToStr(AValue));
            end;
          finally
            LContextData.Free;
          end;
        finally
          LLoadAgent.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Populate (AFeatureID       : integer;
                                       AFeatureName     : WideString;
                                       AChannelNr       : integer;
                                       AFeatureType     : integer;
                                       AFeatureSubType  : integer;
                                       ACategoryID      : integer;
                                       AScenarioPortion : TWaterDemandPortion): WordBool;
const OPNAME = 'TWaterDemandFeature.Populate';
var
  lIndex         : integer;
  LScenarioCount : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FChannelNr           := AChannelNr;
    FFeatureID           := AFeatureID;
    FFeatureName         := AFeatureName;
    FFeatureType         := AFeatureType;
    FFeatureSubType      := AFeatureSubType;
    FWaterDemandCategory := ACategoryID;
    LScenarioCount := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
    for lIndex := LScenarioCount.ArrayLow to LScenarioCount.ArrayHigh do
      FScenarioPortion[lIndex]  := AScenarioPortion[lIndex];
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.PopulateSome (AFeatureID       : integer;
                                           AFeatureName     : WideString;
                                           AChannelNr       : integer;
                                           AFeatureType     : integer;
                                           AFeatureSubType  : integer;
                                           ACategoryID      : integer): WordBool;
const OPNAME = 'TWaterDemandFeature.PopulateSome';
begin
  Result := FALSE;
  try
    FChannelNr           := AChannelNr;
    FFeatureID           := AFeatureID;
    FFeatureName         := AFeatureName;
    FFeatureType         := AFeatureType;
    FFeatureSubType      := AFeatureSubType;
    FWaterDemandCategory := ACategoryID;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.Validate (var AErrors : WideString;
                                       const AContext    : WideString) : WordBool;
const OPNAME = 'TWaterDemandFeature.Validate';
var
  lErrorCols        : TStringList;
  lErrorMsgs        : TStringList;
begin
  Result := FALSE;
  try
    lErrorCols := TStringList.Create;
    lErrorMsgs := TStringList.Create;
    try
      if (AContext = 'ScenarioPortion') then
        Result := ValidateWaterScenarioPortion(lErrorMsgs, lErrorCols);
      begin
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMsgs.Text
          else
            AErrors := AErrors + CTStringsSeparator + lErrorMsgs.Text +
                                 CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorMsgs.Clear;
            lErrorCols.Clear;
          end;
        end;
        AErrors := AErrors + lErrorMsgs.Text;
    finally
      lErrorMsgs.Free;
      lErrorCols.Free;
    end;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.GetKeyValues (const AParamField : WideString;
                                           const AFieldIndex : WideString) : WideString;
const OPNAME = 'TWaterDemandFeature.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'FeatureID=' + IntToStr(FFeatureID)
    else
      Result := Result + ',FeatureID=' + IntToStr(FFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TWaterDemandFeatureList                                                      }
{******************************************************************************}

function TWaterDemandFeatureList._AddRef: Integer;
const OPNAME = 'TWaterDemandFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList._Release: Integer;
const OPNAME = 'TWaterDemandFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeatureList.CreateMemberObjects;
const OPNAME = 'TWaterDemandFeatureList.CreateMemberObjects';
begin
  try
    FScenarioCount := 0;
    inherited CreateMemberObjects;
    FWaterDemandFeatureList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeatureList.DestroyMemberObjects;
const OPNAME = 'TWaterDemandFeatureList.DestroyMemberObjects';
begin
  try
    while (FWaterDemandFeatureList.Count > 0) do
      DeleteWaterDemandFeatureWithIndex(0);
    FreeAndNil(FWaterDemandFeatureList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.Initialise: boolean;
const OPNAME = 'TWaterDemandFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FWaterDemandFeatureList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.AddWaterDemandFeature (AData : TWaterDemandFeature): boolean;
const OPNAME = 'TWaterDemandFeatureList.AddWaterDemandFeature';
begin
  Result := False;
  try
    if (AData <> nil) then
    begin
      FWaterDemandFeatureList.Add(AData);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.NewWaterDemandFeature : TWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.NewWaterDemandFeature';
var
  lFeature : TWaterDemandFeature;
begin
  Result := nil;
  try
    lFeature := TWaterDemandFeature.Create(FAppModules);
    AddWaterDemandFeature(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.CreateNewWaterDemandFeature : TWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.CreateNewWaterDemandFeature';
var
  LFeatureID : integer;
  LLoadAgent : TNetworkFeaturesSQLAgent;
  LFeature   : TWaterDemandFeature;
begin
  Result := nil;
  try
    if (FScenarioCount <= 0) then
    begin
      if not CreateScenarioCount then
        Exit;
    end;

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertWaterDemandFeature(LFeatureID)) then
      begin
        LFeature := NewWaterDemandFeature;
        LFeature.Initialise;
        LFeature.PopulateSome
          (LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.WaterDemandFeature')) + ' ' + IntToStr(LFeatureID),
           0, 7, 0, 0);
        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.CreateWaterDemandFeature : IWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.CreateWaterDemandFeature';
var
  LFeature : IWaterDemandFeature;
begin
  Result := nil;
  try
    lFeature := CreateNewWaterDemandFeature;
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.RemoveWaterDemandFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TWaterDemandFeatureList.RemoveWaterDemandFeatureWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteWaterDemandFeature(AFeatureID) then
        begin
          DeleteWaterDemandFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.DeleteWaterDemandFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TWaterDemandFeatureList.DeleteWaterDemandFeatureWithID';
var
  lIndex   : integer;
begin
  Result := FALSE;
  try
    for lIndex := 0 to FWaterDemandFeatureList.Count -1 do
    begin
      if(TWaterDemandFeature(FWaterDemandFeatureList[lIndex]).FFeatureID = AFeatureID) then
      begin
        FWaterDemandFeatureList.Delete(lIndex);
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.Get_ScenarioCount : integer;
const OPNAME = 'TWaterDemandFeatureList.Get_ScenarioCount';
begin
  Result := 0;
  try
    Result := FScenarioCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterDemandFeatureList.Set_ScenarioCount (ACount : integer);
const OPNAME = 'TWaterDemandFeatureList.Set_ScenarioCount';
var
  LScenarioCountField : TAbstractFieldProperty;
  LFeature            : TWaterDemandFeature;
  LLoadAgent          : TNetworkFeaturesSQLAgent;
  LContextData        : TStringList;
  LFeatureIndex,
  LIndex: integer;
  lNewValue,
  lOldValue : string;
begin
  try
    if(FScenarioCount <= 0) then
    begin
      if not CreateScenarioCount then
        Exit;
    end;

    LScenarioCountField := FAppModules.FieldProperties.FieldProperty('WaterDemandScenarioCount');
    if (ACount >= StrToInt(LScenarioCountField.FieldMinimumValue)) and
       (ACount <= StrToInt(LScenarioCountField.FieldMaximumValue)) then
    begin
      if (ACount <> FScenarioCount) then
      begin
        for LFeatureIndex := 0 to WaterDemandFeatureCount -1 do
        begin
          LFeature := CastWaterDemandFeatureByIndex(LFeatureIndex);
          if(ACount > FScenarioCount) then
          begin
            for LIndex := FScenarioCount + 1 to ACount do
              LFeature.ScenarioPortionByIndex[LIndex] := 0.0;
          end
          else
          begin
            for LIndex := FScenarioCount downto ACount+1 do
              LFeature.ScenarioPortionByIndex[LIndex] := NullFloat;
          end;
        end;
        LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData(LContextData);
            if (ACount = NullInteger) then
              lNewValue := ''
            else
              lNewValue := IntToStr(ACount);
            if (FScenarioCount = NullInteger) then
              lOldValue := ''
            else
              lOldValue := IntToStr(FScenarioCount);
            if FAppModules.FieldProperties.UpdateFieldValue(
              'WaterDemandScenarioCount', lNewValue, lOldValue, LContextData) then
            begin
              FScenarioCount := ACount;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterDemandScenarioCount',lOldValue,lNewValue);
            end;
          finally
            LContextData.Free;
          end;
        finally
          LLoadAgent.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.DeleteWaterDemandFeatureWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TWaterDemandFeatureList.DeleteWaterDemandFeatureWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) and (AIndex < FWaterDemandFeatureList.Count) then
    begin
      FWaterDemandFeatureList.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.Get_WaterDemandFeatureByIndex(AIndex: integer): IWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.Get_WaterDemandFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FWaterDemandFeatureList.Count) then
      Result := TWaterDemandFeature(FWaterDemandFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.Get_WaterDemandFeatureByID(AFeatureID: integer): IWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.Get_WaterDemandFeatureByID';
var
 lIndex   : integer;
 lFeature : TWaterDemandFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FWaterDemandFeatureList.Count)) do
    begin
      lFeature := TWaterDemandFeature(FWaterDemandFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.CastWaterDemandFeatureByIndex (AIndex : integer): TWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.CastWaterDemandFeatureByIndex';
begin
  Result := nil;
  try
    Result := TWaterDemandFeature(FWaterDemandFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.CastWaterDemandFeatureByID(AFeatureID: integer): TWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.CastWaterDemandFeatureByID';
var
 lIndex   : integer;
 lFeature : TWaterDemandFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FWaterDemandFeatureList.Count)) do
    begin
      lFeature := TWaterDemandFeature(FWaterDemandFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.CastWaterDemandFeatureByChannelNr(AChannelNr: integer): TWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.CastWaterDemandFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TWaterDemandFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FWaterDemandFeatureList.Count)) do
    begin
      lFeature := TWaterDemandFeature(FWaterDemandFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TWaterDemandFeatureList.Get_WaterDemandFeatureCount: integer;
const OPNAME = 'TWaterDemandFeatureList.Get_WaterDemandFeatureCount';
begin
  Result := 0;
  try
    Result := FWaterDemandFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TWaterDemandFeatureList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'WaterDemandScenarioCount') then
        Result := ValidateWaterDemandScenarioCount(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        for LIndex := 0 to FWaterDemandFeatureList.Count -1 do
        begin
          if (NOT WaterDemandFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
            Result := False;
          if ((NOT Result ) AND lStopOnFirstError) then
            Break;
        end;
        if (NOT ValidateWaterDemandScenarioCount(lErrorList)) then
          Result := FALSE;
       end;
      AErrors := AErrors + lErrorList.Text;
    finally
        FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.GetKeyValues (const AParamField : WideString;
                                               const AFieldIndex : WideString) : WideString;
const OPNAME = 'TWaterDemandFeatureList.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureList.PopulateScenarioCount(AScenarioCount: integer): WordBool;
const OPNAME = 'TWaterDemandFeatureList.PopulateScenarioCount';
begin
  Result := FALSE;
  try
    FScenarioCount := AScenarioCount;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.CreateScenarioCount: boolean;
const OPNAME = 'TWaterDemandFeatureList.CreateScenarioCount';
var
  LLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      Result := LLoadAgent.CreateScenarioCount
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureList.ValidateWaterDemandScenarioCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWaterDemandFeatureList.ValidateWaterDemandScenarioCount';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('WaterDemandScenarioCount',
            IntToStr(FScenarioCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +LMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeature.ValidateWaterScenarioPortion(AErrorMessages : TStrings;
                                                          AErrorColumns  : TStringList): boolean;
const OPNAME = 'TWaterDemandFeature.ValidateWaterScenarioPortion';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lIndex            : integer;
  LScenarioPortion  : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    LScenarioPortion := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
    if (LScenarioPortion <> nil) then
    begin
      AErrorColumns.Clear;
      lResult := TRUE;
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := LScenarioPortion.ArrayLow to LScenarioPortion.ArrayHigh do
      begin
        lMessage := '';
        if (FScenarioPortion[lIndex] <> NullFloat) then
        begin
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('ScenarioPortion',FloatToStr(ScenarioPortionByIndex[lIndex]),
             lMessage, lIndex)) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +lMessage);
            AErrorColumns.Add(IntToStr(lIndex));
            if (lStopOnFirstError) then
              Break;
          end;
        end;
      end;
      Result := lResult;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.CreateRiskCriteriaRecord: boolean;
const OPNAME = 'TWaterDemandConfiguration.CreateRiskCriteriaRecord';
var
  LLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      Result := LLoadAgent.CreateRiskCriteriaRecord
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TWaterUseOutputProportion }

function TWaterUseOutputProportion._AddRef: Integer;
const OPNAME = 'TWaterUseOutputProportion._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUseOutputProportion._Release: Integer;
const OPNAME = 'TWaterUseOutputProportion._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUseOutputProportion.CreateMemberObjects;
const OPNAME = 'TWaterUseOutputProportion.CreateMemberObjects';
var
  LFieldProperty : TAbstractFieldProperty;
  lIndex         : integer;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty ( 'WaterUsePortion' );
    if ( LFieldProperty = nil ) then
      raise Exception.Create ( 'Field (WaterUsePortion) not found in field properties' );
    SetLength ( FWaterUsePortion, LFieldProperty.ArrayLength );
    for lIndex := 0 to LFieldProperty.ArrayHigh do
      FWaterUsePortion[lIndex] := 0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUseOutputProportion.DestroyMemberObjects;
const OPNAME = 'TWaterUseOutputProportion.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    Finalize ( FWaterUsePortion );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUseOutputProportion.PopulateSome(AChannelNumber : integer);
const OPNAME = 'TWaterUseOutputProportion.PopulateSome';
begin
  try
    FChannelNumber := AChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWaterUseOutputProportion.Get_ChannelNumber : Integer;
const OPNAME = 'TWaterUseOutputProportion.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWaterUseOutputProportion.Get_ProportionByIndex (AIndex : Integer) : Double;
const OPNAME = 'TWaterUseOutputProportion.Get_ProportionByIndex';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := 0.00;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('WaterUsePortion');
    if (AIndex >= LFieldProperty.ArrayLow) AND (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if FWaterUsePortion [AIndex] = NullFloat then
        Result := 0.00
      else
        Result := FWaterUsePortion[AIndex];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWaterUseOutputProportion.Get_Total : Double;
const OPNAME = 'TWaterUseOutputProportion.Get_Total';
begin
  Result := 0.00;
  try
    Result := FWaterUseTotal;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUseOutputProportion.Set_ChannelNumber ( Value : Integer );
const OPNAME = 'TWaterUseOutputProportion.Set_ChannelNumber';
begin
  try
    FChannelNumber := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUseOutputProportion.Set_ProportionByIndex (AIndex : Integer;
                                                           Value  : Double );
const OPNAME = 'TWaterUseOutputProportion.Set_ProportionByIndex';
var
  LLoadAgent    : TNetworkFeaturesSQLAgent;
  LContextData  : TStringList;
  lOldValue     : string;
  lNewValue     : string;
  LPrevValue    : double;
begin
  try
    if (FWaterUsePortion[AIndex] <> Value) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_WaterUseOutputProportion(
            LContextData, IntToStr(FChannelNumber), IntToStr(AIndex));
          if (Value = NullFloat) then
            lNewValue := ''
          else
            lNewValue := FloatToStr(Value);
          if (FWaterUsePortion[AIndex] = NullFloat) then
            lOldValue := ''
          else
            lOldValue := FloatToStr(FWaterUsePortion[AIndex]);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'WaterUsePortion', lNewValue, lOldValue, LContextData) then
          begin
            LPrevValue := FWaterUsePortion[AIndex];
            FWaterUsePortion[AIndex] := Value;
            FWaterUseTotal := FWaterUseTotal - LPrevValue + Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterUsePortion',FloatToStr(LPrevValue),FloatToStr(Value));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUseOutputProportion.Validate ( var AErrors : WideString; const AContext: WideString): WordBool;
const OPNAME = 'TWaterUseOutputProportion.Validate';
var
  LStopOnFirstError : Boolean;
  LErrorCols        : TStringList;
  LErrorMsgs        : TStringList;
begin
  Result := FALSE;
  try
    LErrorCols := TStringList.Create;
    LErrorMsgs := TStringList.Create;
    try
      if ( AContext = 'WaterUsePortionTotal' ) then
        Result := ValidateWaterUsePortionsTotal ( LErrorMsgs )
      else
      if ( AContext = 'WaterUsePortion' ) then
      begin
        Result := ValidateWaterUsePortions ( LErrorMsgs, LErrorCols );
        if ( not Result ) then
        begin
          if ( LErrorCols.Count = 0 ) then
            AErrors := AErrors + LErrorMsgs.Text
          else
            AErrors := AErrors + CTStringsSeparator + LErrorMsgs.Text +
                       CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
          LErrorMsgs.Clear;
          LErrorCols.Clear;
        end;
      end
      else
      begin
        Result := True;
        LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if ( not ValidateWaterUsePortionsTotal ( LErrorMsgs ) ) then
          Result := False;
        if ( Result or ( not LStopOnFirstError ) ) then
        begin
          if ( not ValidateWaterUsePortions ( LErrorMsgs, LErrorCols ) ) then
          begin
            Result := False;
            if ( LErrorCols.Count = 0 ) then
              AErrors := AErrors + LErrorMsgs.Text
            else
              AErrors := AErrors + CTStringsSeparator + LErrorMsgs.Text +
                         CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
            LErrorMsgs.Clear;
            LErrorCols.Clear;
          end;
        end;
      end;
      AErrors := AErrors + LErrorMsgs.Text
    finally
      FreeAndNil ( LErrorCols );
      FreeAndNil ( LErrorMsgs );
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseOutputProportion.Populate (AChannelNumber : integer;
                                             AUsePortion    : TWaterDemandPortion): WordBool;
const OPNAME = 'TWaterUseOutputProportion.Populate';
var
  LIndex         : integer;
  LFieldProperty : TAbstractFieldProperty;
  lTotal         : double;
begin
  Result := False;
  try
    FChannelNumber := AChannelNumber;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('WaterUsePortion');
    lTotal         := 0.0;
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if (AUsePortion[LIndex] <> NullFloat) then
      begin
        FWaterUsePortion[LIndex] := AUsePortion[LIndex];
        lTotal                   := lTotal + AUsePortion[lIndex];
      end;
    end;
    FWaterUseTotal := lTotal;    
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWaterUseOutputProportion.Initialise: Boolean;
const OPNAME = 'TWaterUseOutputProportion.Initialise';
var
  LIndex : integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  LFieldProperty := FAppModules.FieldProperties.FieldProperty ( 'WaterUsePortion' );
  Result := inherited initialise;
  try
    FChannelNumber := 0;
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      FWaterUsePortion [ LIndex ] := 0.00;
    FWaterUseTotal         := 0.00;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWaterDemandConfiguration.Get_WaterUseOutputProportionByChannelNumber (AChannelNr : Integer ) : IWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.Get_WaterUseOutputProportionByChannelNumber';
var
  LIndex    : integer;
  LCategory : TWaterUseOutputProportion;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FWaterUseOutputProportionList.Count)) do
    begin
      LCategory := TWaterUseOutputProportion(FWaterUseOutputProportionList.Items[lIndex]);
      if (LCategory.ChannelNumber = AChannelNr) then
        Result := LCategory
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandConfiguration.Get_WaterUseOutputProportionByIndex ( AIndex : Integer ) : IWaterUseOutputProportion;
const OPNAME = 'TWaterDemandConfiguration.Get_WaterUseOutputProportionByIndex';
var
  LWaterUseOutputProportion : TWaterUseOutputProportion;
begin
  Result := nil;
  try
    if ( ( AIndex >= 0 ) AND ( AIndex < FWaterUseOutputProportionList.Count ) ) then
    begin
      LWaterUseOutputProportion := TWaterUseOutputProportion ( FWaterUseOutputProportionList.Items [ AIndex ] );
      Result := LWaterUseOutputProportion;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUseOutputProportion.ValidateWaterUsePortions ( AErrorMessages : TStrings; AErrorColumns : TStringList ) : boolean;
const OPNAME = 'TWaterUseOutputProportion.ValidateWaterUsePortions';
var
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LIndex            : integer;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('WaterUsePortion');
    if ( LFieldProperty <> nil ) then
    begin
      AErrorColumns.Clear;
      LResult := True;
      LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        lMessage := '';
        if (  FWaterUsePortion [ LIndex ] <> NullFloat ) then
        begin
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('WaterUsePortion', FloatToStr ( FWaterUsePortion [ LIndex ] ),
             LMessage, LIndex)) then
          begin
            LResult := False;
            AErrorMessages.Add ('ERROR:'+ LMessage );
            AErrorColumns.Add ( IntToStr ( LIndex ) );
            if ( LStopOnFirstError ) then
              Break;
          end;
        end;
      end;
      Result := LResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseOutputProportion.ValidateWaterUsePortionsTotal ( AErrorMessages : TStrings ) : boolean;
const OPNAME = 'TWaterUseOutputProportion.ValidateWaterUsePortionsTotal';
var
  LMessage          : string;
  LResult           : Boolean;
  LFieldProperty     : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty ( 'WaterUsePortionTotal' );
    if ( LFieldProperty <> nil) then
    begin
      LResult := True;
      begin
        LMessage := '';
        if ( not FAppModules.FieldProperties.ValidateFieldProperty
             ( 'WaterUsePortionTotal', FloatToStr ( FWaterUseTotal ),
             LMessage ) ) then
        begin
          LResult := False;
          AErrorMessages.Add ( 'ERROR:'+LMessage );
        end;
      end;
      Result := LResult;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWaterDemandFeatureList.CopyWaterDemandFeature(ANewChannelNumber,AOldChannelNumber: integer): IWaterDemandFeature;
const OPNAME = 'TWaterDemandFeatureList.CopyWaterDemandFeature';
var
  LWaterDemandFeature : TWaterDemandFeature;
  LWaterDemandFeatureCopy : TWaterDemandFeature;
begin
  Result := nil;
  try
    LWaterDemandFeature := CastWaterDemandFeatureByChannelNr(AOldChannelNumber);
    if (LWaterDemandFeature <> nil) then
    begin
      LWaterDemandFeatureCopy := CreateNewWaterDemandFeature;
      if LWaterDemandFeatureCopy <> nil then
      begin
        LWaterDemandFeatureCopy.Assign(ANewChannelNumber,LWaterDemandFeature);
        Result := LWaterDemandFeatureCopy;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeature.Assign(AChannelNumber: integer;AWaterDemandFeature: TWaterDemandFeature);
const OPNAME = 'TWaterDemandFeature.Assign';
var
  LChannel : IGeneralFlowChannel;
  LIndex : integer;
  LScenarioCount : TAbstractFieldProperty;
begin
  try
    if (AWaterDemandFeature <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+AWaterDemandFeature.FeatureName;
      FeatureType := AWaterDemandFeature.FeatureType;
      FeatureSubType := AWaterDemandFeature.FeatureSubType;
      WaterDemandCategory := AWaterDemandFeature.WaterDemandCategory;
      LScenarioCount := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
      for LIndex := LScenarioCount.ArrayLow to LScenarioCount.ArrayHigh do
        ScenarioPortionByIndex[LIndex]  := AWaterDemandFeature.ScenarioPortionByIndex[LIndex];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



end.
