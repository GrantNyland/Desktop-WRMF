{******************************************************************************}
{*  UNIT      : Contains the class TIFRFeature.                               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/17                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UIFRFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  UIFRDataObject,
  UStringListOfStringLists,
  VoaimsCom_TLB;{,
  HydrologyCom_TLB; }

type

{******************************************************************************}
{* In-stream Flow Requirements (IFR)                                          *}
{******************************************************************************}

  TIFRFeature = class(TAbstractAppObject, IIFRFeature)
  protected
    FFeatureID               : integer;
    FFeatureName             : string;
    FChannelNr               : integer;
    FFeatureType             : integer;
    FFeatureSubType          : integer;
    FLagMonths               : integer;
    FReferenceNodeNumbers    : TStringList;
    FExceedencePercentages   : TExceedencePercentagesArray;
    FInflows                 : TIFRArray;
    FReleases                : TIFRArray;
    FCalculationOption       : Double;
    FIFRSiteID               : integer;
    FReferenceFlowType       : TIFRFeatureReferenceFlowType;
    FAnnualInflow            : TExceedencePercentagesArray;
    FIFRFeatureExists        : integer;
    FIFRLoss                 : Integer;
    FMonthlyIFRLoss          : TStringList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ExceedencePercentagesCommaText: string;
    procedure ReCalculateReferenceFlows;
    procedure ClearReferenceFlows;
    function GetSingleNodeReferenceFlowData(ANodeNumber: integer;AReferenceFlows:TStrings): boolean;
    function UpdatePercentileReferenceFlows(AReferenceFlows:TStrings): boolean;

    function ValidateFeatureName(AErrorMessages: TStrings): WordBool;
    function ValidateLagInMonths(AErrorMessages : TStrings): WordBool;
    function ValidateReferenceNodes(AErrorMessages : TStrings): WordBool;
    function ValidateInflows (AErrorMessages : TStrings;
                              AErrorColumns  : TStringList): WordBool;
    function ValidateReleases (AErrorMessages : TStrings;
                               AErrorColumns  : TStringList): WordBool;
    function ValidatePointsCount(AErrorMessages: TStrings): WordBool;
    function ValidateAnnualNumberOfClasses(AErrorMessages: TStrings): WordBool;
    function ValidateAnnualInflow(AErrorMessages : TStrings;
                               AErrorColumns  : TStringList): WordBool;
    function ValidateCalcOption(AErrorMessages: TStrings): WordBool;

  public
    function Initialise : boolean; override;
    function Populate (AFeatureID             : integer;
                       AFeatureName           : WideString;
                       AChannelNr             : integer;
                       AFeatureType           : integer;
                       AFeatureSubType        : integer;
                       ALagMonths             : integer;
                       ACalculationOption     : double;
                       AFIFRSiteID            : integer;
                       AReferenceFlowType     : TIFRFeatureReferenceFlowType;
                       AExceedencePercentages : TExceedencePercentagesArray;
                       AReferenceNodeNumbers  : TStringList;
                       AInflows               : TIFRArray;
                       AReleases              : TIFRArray;
                       AAnnualInflow          : TExceedencePercentagesArray;
                       AIFRFeatureExists      : integer;
                       AIFRLoss               : integer;
                       AMonthlyIFRLoss        : WideString ): WordBool;
    function PopulateSome (AFeatureID             : integer;
                           AFeatureName           : WideString;
                           AChannelNr             : integer;
                           AFeatureType           : integer;
                           AFeatureSubType        : integer;
                           ALagMonths             : integer;
                           ACalculationOption     : double;
                           AIFRSiteID             : integer;
                           AReferenceFlowType     : TIFRFeatureReferenceFlowType;
                           AIFRFeatureExists      : integer ): WordBool;

    function PopulateWithSiteData(AIFRSite:TIFRSiteDataObject):boolean;
    procedure Assign(ANewChannelNumber : integer;AIFRFeature: TIFRFeature);
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_LagMonths : integer; safecall;
    procedure Set_LagMonths (AMonths : integer); safecall;
    function Get_CalculationOption : Double; safecall;
    procedure Set_CalculationOption (AOption : Double); safecall;
    function Get_IFRSiteID : integer; safecall;
    procedure Set_IFRSiteID (ASiteID : integer); safecall;
    function Get_AnnualInflow(AIndex : integer) : Double; safecall;
    procedure Set_AnnualInflow(AIndex : integer;AInflow : Double); safecall;
    function Get_ReferenceNodeNumbers : WideString; safecall;
    procedure Set_ReferenceNodeNumbers (const ANumbers  : WideString); safecall;
    function Get_ReferenceNodeNumbersCount : integer; safecall;
    procedure Set_ReferenceNodeNumbersCount (ACount : integer); safecall;
    function Get_ReferenceNodeNumberByIndex (AIndex : integer) : integer; safecall;
    function Get_NrOfInflowIFRPoints : integer; safecall;
    procedure Set_NrOfInflowIFRPoints(ACount : integer); safecall;
    function Get_ExceedencePercentageArray : TExceedencePercentagesArray; safecall;
    function Get_ExceedencePercentageByIndex(AIndex : integer) : double; safecall;
    procedure Set_ExceedencePercentageByIndex(AIndex : integer;AValue : double); safecall;
    function Get_InflowByIndexAndMonth(AIndex : integer;AMonth : integer) : double; safecall;
    procedure Set_InflowByIndexAndMonth(AIndex : integer; AMonth : integer; AValue : double); safecall;
    function Get_ReleaseByIndexAndMonth(AIndex : integer; AMonth : integer) : double; safecall;
    procedure Set_ReleaseByIndexAndMonth(AIndex : integer; AMonth : integer; AValue : double); safecall;
    function Get_ReferenceFlowType: TIFRFeatureReferenceFlowType; safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString) : WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex : WideString) : WideString; safecall;
    function GetNodeReferenceFlowData(AReferenceFlowsData:TStrings): boolean; safecall;
    function GetRequirementFlowFromReferenceFlow(AMonth: integer; AReferenceFlow: double): double; safecall;
    function Get_IFRStatusIndicator : integer; safecall;
    procedure Set_IFRStatusIndicator(Value: Integer); safecall;

    function Get_IFRLoss: Integer; safecall;
    procedure Set_IFRLoss(Value: Integer); safecall;
    function Get_MonthlyIFRLossByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyIFRLossByIndex(AIndex: Integer; Value: Double); safecall;

    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property LagMonths : integer read Get_LagMonths write Set_LagMonths;
    property CalculationOption : Double read Get_CalculationOption write Set_CalculationOption;
    property IFRSiteID : integer read Get_IFRSiteID write Set_IFRSiteID;
    property AnnualInflow[AIndex : integer] : Double read Get_AnnualInflow write Set_AnnualInflow;
    property IFRFeatureExists : integer read Get_IFRStatusIndicator write Set_IFRStatusIndicator;
    
    property ReferenceNodeNumbers : WideString read Get_ReferenceNodeNumbers write Set_ReferenceNodeNumbers;
    property ReferenceNodeNumbersCount : integer read Get_ReferenceNodeNumbersCount write Set_ReferenceNodeNumbersCount;
    property ReferenceNodeNumberByIndex[AIndex : integer] : integer read Get_ReferenceNodeNumberByIndex;
    property NrOfInflowIFRPoints : integer
      read Get_NrOfInflowIFRPoints write Set_NrOfInflowIFRPoints;
    property ExceedencePercentageByIndex[AIndex : integer] : double
      read Get_ExceedencePercentageByIndex write Set_ExceedencePercentageByIndex;
    property InflowByIndexAndMonth[AIndex, AMonth : integer] : double
      read Get_InflowByIndexAndMonth  write Set_InflowByIndexAndMonth;
    property ReleaseByIndexAndMonth[AIndex, AMonth : integer] : double
      read Get_ReleaseByIndexAndMonth write Set_ReleaseByIndexAndMonth;

    property IFRLoss: Integer read Get_IFRLoss write Set_IFRLoss;
    property MonthlyIFRLossByIndex[AIndex: Integer]: Double read Get_MonthlyIFRLossByIndex write Set_MonthlyIFRLossByIndex;


  end;

  TIFRFeatureList = class(TAbstractAppObject, IIFRFeatureList)
  protected
    FMonthlyIFRFeatureList : TObjectList;
    FAnnualIFRFeatureList : TObjectList;
    FInflowOption   : integer;
    FUpdateIFRFromReferenceInflows : boolean;
    FIFRFeatureExists : boolean;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_MaximumAnnualControlStructCount: integer;
    function Get_MaximumMonthlyControlStructCount: integer;
    function ValidateAnnualStructureCount(AErrorMessages: TStrings): WordBool;
    function ValidateMonthlyStructureCount(AErrorMessages: TStrings): WordBool;
  public
    function Initialise: boolean; override;
    function Validate (var AErrors : WideString;const AContext    : WideString) : WordBool; safecall;
    function PopulateInflowOption(AInflowOption : integer): WordBool;

    function CastMonthlyIFRFeatureByIndex(AIndex : integer): TIFRFeature;
    function CastMonthlyIFRFeatureByNumber(AFeatureNumber: integer): TIFRFeature;
    function CastMonthlyIFRFeatureByID(AFeatureID: integer): TIFRFeature;

    function CastAnnualIFRFeatureByIndex(AIndex : integer): TIFRFeature;
    function CastAnnualIFRFeatureByNumber(AFeatureNumber: integer): TIFRFeature;
    function CastAnnualIFRFeatureByID(AFeatureID: integer): TIFRFeature;

    function DeleteIFRFeatureWithID(AFeatureID : integer) : WordBool;
    function RemoveIFRFeatureWithID (AFeatureID : integer) : WordBool; safecall;

    function CreateNewIFRFeature(AIFRType: TIFRFeatureReferenceFlowType): TIFRFeature;
    function CreateIFRFeature(AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature; safecall;
    function CopyIFRFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer;
                            AIFRType: TIFRFeatureReferenceFlowType):IIFRFeature;safecall;
    function NewMonthlyIFRFeature : TIFRFeature;
    function NewAnnualIFRFeature: TIFRFeature;

    function Get_InflowOption : integer; safecall;
    procedure Set_InflowOption (AInflowOption: integer); safecall;

    function Get_MonthlyIFRFeatureByIndex(AIndex: integer): IIFRFeature; safecall;
    function Get_AnnualIFRFeatureByIndex(AIndex: integer): IIFRFeature; safecall;
    function Get_MonthlyIFRFeatureByID(AFeatureID: integer): IIFRFeature; safecall;
    function Get_AnnualIFRFeatureByID(AFeatureID: integer): IIFRFeature; safecall;

    function Get_MonthlyIFRFeatureCount: integer; safecall;
    function Get_AnnualIFRFeatureCount: integer; safecall;

    property MaximumAnnualControlStructCount: integer  read Get_MaximumAnnualControlStructCount;
    property MaximumMonthlyControlStructCount: integer read Get_MaximumMonthlyControlStructCount;

    property InflowOption : integer read Get_InflowOption write Set_InflowOption;
    property MonthlyIFRFeatureByIndex[AIndex : integer]: IIFRFeature read Get_MonthlyIFRFeatureByIndex;
    property MonthlyIFRFeatureByID[AFeatureID: integer]: IIFRFeature read Get_MonthlyIFRFeatureByID;
    property MonthlyIFRFeatureCount: integer read Get_MonthlyIFRFeatureCount;
    property AnnualIFRFeatureByIndex[AIndex : integer]: IIFRFeature read Get_AnnualIFRFeatureByIndex;
    property AnnualIFRFeatureByID[AFeatureID: integer]: IIFRFeature read Get_AnnualIFRFeatureByID;
    property AnnualIFRFeatureCount: integer read Get_AnnualIFRFeatureCount;
    property UpdateIFRFromReferenceInflows : boolean read FUpdateIFRFromReferenceInflows write FUpdateIFRFromReferenceInflows;
    property IFRFeatureExists : boolean read FIFRFeatureExists write FIFRFeatureExists;
  end;

implementation

uses
  System.Types,
  SysUtils,
  Math,
  VCL.Dialogs,
  UDataSetType,
  UConstants,
  UUtilities,
  UDemandHydrologyDataSQLAgent,
  UNetworkFeaturesSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{ TIFRFeature                                                                  }
{******************************************************************************}

function TIFRFeature._AddRef: Integer;
const OPNAME = 'TIFRFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature._Release: Integer;
const OPNAME = 'TIFRFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.CreateMemberObjects;
const OPNAME = 'TIFRFeature.CreateMemberObjects';
var
  LExceedencePercentages,
  LIFRVariables,
  LAnnualInflow,
  LIFRReleaseVariables: TAbstractFieldProperty;
begin
  inherited;
  try
    FReferenceNodeNumbers := TStringList.Create;
    FMonthlyIFRLoss := TStringList.Create;
    if (UpperCase(FAppModules.StudyArea.ModelCode) <> 'DAILYDIVERSION') then
    begin
      LExceedencePercentages := FAppModules.FieldProperties.FieldProperty('ExceedencePercentage');
      if (LExceedencePercentages = nil) then
        raise Exception.Create('Field (ExceedencePercentage) not found in field properties');
      SetLength(FExceedencePercentages,LExceedencePercentages.ArrayLength);

      LIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables');
      if (LIFRVariables = nil) then
        raise Exception.Create('Field (IFRVariables) not found in field properties');
      SetLength(FInflows,LIFRVariables.ArrayLength, LIFRVariables.ArrayLength(1));

      LIFRReleaseVariables := FAppModules.FieldProperties.FieldProperty('IFRReleaseVariables');
      if (LIFRReleaseVariables = nil) then
        raise Exception.Create('Field (IFRReleaseVariables) not found in field properties');
      SetLength(FReleases,LIFRReleaseVariables.ArrayLength, LIFRReleaseVariables.ArrayLength(1));

      LAnnualInflow := FAppModules.FieldProperties.FieldProperty('AnnualInflow');
      if (LAnnualInflow = nil) then
        raise Exception.Create('Field (AnnualInflow) not found in field properties');
      SetLength(FAnnualInflow,LAnnualInflow.ArrayLength);
    end
    else
    begin
      LExceedencePercentages := FAppModules.FieldProperties.FieldProperty('DiversionExceedencePercentage');
      if (LExceedencePercentages = nil) then
        raise Exception.Create('Field (DiversionExceedencePercentage) not found in field properties');
      SetLength(FExceedencePercentages,LExceedencePercentages.ArrayLength);

      LIFRVariables := FAppModules.FieldProperties.FieldProperty('DiversionIFRVariables');
      if (LIFRVariables = nil) then
        raise Exception.Create('Field (DiversionIFRVariables) not found in field properties');
      SetLength(FInflows,LIFRVariables.ArrayLength, LIFRVariables.ArrayLength(1));

      LIFRReleaseVariables := FAppModules.FieldProperties.FieldProperty('DiversionIFRReleaseVariables');
      if (LIFRReleaseVariables = nil) then
        raise Exception.Create('Field (DiversionIFRReleaseVariables) not found in field properties');
      SetLength(FReleases,LIFRReleaseVariables.ArrayLength, LIFRReleaseVariables.ArrayLength(1));

      LAnnualInflow := FAppModules.FieldProperties.FieldProperty('DiversionAnnualInflow');
      if (LAnnualInflow = nil) then
        raise Exception.Create('Field (DiversionAnnualInflow) not found in field properties');
      SetLength(FAnnualInflow,LAnnualInflow.ArrayLength);

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.DestroyMemberObjects;
const OPNAME = 'TIFRFeature.DestroyMemberObjects';
begin
  try
    FReferenceNodeNumbers.Clear;
    FMonthlyIFRLoss.Clear;
    FreeAndNil(FMonthlyIFRLoss);
    FreeAndNil(FReferenceNodeNumbers);
    Finalize(FExceedencePercentages);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Initialise: boolean;
const OPNAME = 'TIFRFeature.Initialise';
var
  lIndex : integer;
  lMonth : integer;
  LIFRVariables,
  LExceedencePercentages: TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelNr             := 0;
    FFeatureID             := -1;
    FFeatureName           := '';
    FFeatureType           := -1;
    FFeatureSubType        := -1;
    FLagMonths             := 0;
    FCalculationOption     := NullFloat;
    FIFRSiteID             := NullInteger;
    FReferenceFlowType     := ifrtNone;
    FIFRFeatureExists      := 0;
    FReferenceNodeNumbers.Clear;
    FMonthlyIFRLoss.Clear;
    if (UpperCase(FAppModules.StudyArea.ModelCode) <> 'DAILYDIVERSION') then
      LExceedencePercentages := FAppModules.FieldProperties.FieldProperty('ExceedencePercentage')
    else
      LExceedencePercentages := FAppModules.FieldProperties.FieldProperty('DiversionExceedencePercentage');

    for lIndex := LExceedencePercentages.ArrayLow to LExceedencePercentages.ArrayHigh do
      FExceedencePercentages[lIndex] := NullFloat;
    if (UpperCase(FAppModules.StudyArea.ModelCode) <> 'DAILYDIVERSION') then
      LIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables')
    else
      LIFRVariables := FAppModules.FieldProperties.FieldProperty('DiversionIFRVariables');

    for lIndex := LIFRVariables.ArrayLow to LIFRVariables.ArrayHigh do
      for lMonth := 1 to 12 do
      begin
        FInflows[lIndex, lMonth]  := NullFloat;
        FReleases[lIndex, lMonth] := NullFloat;
      end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Populate (AFeatureID             : integer;
                               AFeatureName           : WideString;
                               AChannelNr             : integer;
                               AFeatureType           : integer;
                               AFeatureSubType        : integer;
                               ALagMonths             : integer;
                               ACalculationOption     : double;
                               AFIFRSiteID            : integer;
                               AReferenceFlowType     : TIFRFeatureReferenceFlowType;
                               AExceedencePercentages : TExceedencePercentagesArray;
                               AReferenceNodeNumbers  : TStringList;
                               AInflows               : TIFRArray;
                               AReleases              : TIFRArray;
                               AAnnualInflow          : TExceedencePercentagesArray;
                               AIFRFeatureExists      : integer;
                               AIFRLoss               : integer;
                               AMonthlyIFRLoss        : WideString): WordBool;
const OPNAME = 'TIFRFeature.Populate';
var
  lNodeNr : string;
  lIndex  : integer;
  lMonth  : integer;
  LIFRVariables: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FChannelNr         := AChannelNr;
    FFeatureID         := AFeatureID;
    FFeatureName       := AFeatureName;
    FFeatureType       := AFeatureType;
    FFeatureSubType    := AFeatureSubType;
    FLagMonths         := ALagMonths;
    FCalculationOption := ACalculationOption;
    FIFRSiteID         := AFIFRSiteID;
    FReferenceFlowType := AReferenceFlowType;
    FIFRFeatureExists  := AIFRFeatureExists;

    while (AReferenceNodeNumbers.Count > 0) do
    begin
      lNodeNr := AReferenceNodeNumbers.Strings[0];
      FReferenceNodeNumbers.Add(lNodeNr);
      AReferenceNodeNumbers.Delete(0);
    end;
    if (UpperCase(FAppModules.StudyArea.ModelCode) <> 'DAILYDIVERSION') then
      LIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables')
    else
      LIFRVariables := FAppModules.FieldProperties.FieldProperty('DiversionIFRVariables');

    for lIndex := LIFRVariables.ArrayLow to LIFRVariables.ArrayHigh do
    begin
      FExceedencePercentages[lIndex] := AExceedencePercentages[lIndex];
      if LIndex <= 10 then
        FAnnualInflow[LIndex-1] := AAnnualInflow[LIndex-1];
      for lMonth := 1 to 12 do
      begin
        FInflows[lIndex, lMonth]  := AInflows[lIndex, lMonth];
        FReleases[lIndex, lMonth] := AReleases[lIndex, lMonth];
      end;
    end;
    FIFRLoss           := AIFRLoss;
    FMonthlyIFRLoss.CommaText := AMonthlyIFRLoss;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.PopulateSome (AFeatureID             : integer;
                                   AFeatureName           : WideString;
                                   AChannelNr             : integer;
                                   AFeatureType           : integer;
                                   AFeatureSubType        : integer;
                                   ALagMonths             : integer;
                                   ACalculationOption     : double;
                                   AIFRSiteID             : integer;
                                   AReferenceFlowType     : TIFRFeatureReferenceFlowType;
                                   AIFRFeatureExists      : integer): WordBool;
const OPNAME = 'TIFRFeature.PopulateSome';
begin
  Result := FALSE;
  try
    FChannelNr          := AChannelNr;
    FFeatureID          := AFeatureID;
    FFeatureName        := AFeatureName;
    FFeatureType        := AFeatureType;
    FFeatureSubType     := AFeatureSubType;
    FLagMonths          := ALagMonths;
    FCalculationOption  := ACalculationOption;
    FIFRSiteID          := AIFRSiteID;
    FReferenceFlowType  := AReferenceFlowType;
    FIFRFeatureExists   := AIFRFeatureExists;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_FeatureID : integer;
const OPNAME = 'TIFRFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_FeatureName : WideString;
const OPNAME = 'TIFRFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TIFRFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_FeatureType : integer;
const OPNAME = 'TIFRFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_FeatureSubType : integer;
const OPNAME = 'TIFRFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TIFRFeature.Set_FeatureType';
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

procedure TIFRFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TIFRFeature.Set_FeatureSubType';
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

function TIFRFeature.Get_LagMonths : integer;
const OPNAME = 'TIFRFeature.Get_LagMonths';
begin
  Result := 0;
  try
    Result := StrToInt(FAppModules.Changes.GetParameterValue
                          ('LagInMonthsCount',
                          GetKeyValues('LagInMonthsCount', ''),
                          IntToStr(FLagMonths),
                          ''));
//    Result := FLagMonths;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_MonthlyIFRLossByIndex(AIndex: Integer): Double;
const OPNAME = 'TIFRFeature.Get_MonthlyIFRLossByIndex';
begin
  Result := NullFloat;
  try
    if(Trim(FMonthlyIFRLoss.CommaText) = '') and (FIFRLoss <> 1) then  Exit;
      if (AIndex>=0) and (AIndex<=FMonthlyIFRLoss.Count-1) then
        Result := StrToFloat(FMonthlyIFRLoss[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_NrOfInflowIFRPoints : integer;
const OPNAME = 'TIFRFeature.Get_NrOfInflowIFRPoints';
var
  lIndex    : integer;
  lContinue : Boolean;
  LIFRVariables: TAbstractFieldProperty;
begin
  Result := 0;
  try
    lContinue := TRUE;
    LIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables');
    lIndex    := LIFRVariables.ArrayLow;
    while (lContinue AND (lIndex <= LIFRVariables.ArrayHigh)) do
    begin
      if (FInflows[lIndex, 1] = NullFloat) then
        lContinue := FALSE
      else
      begin
        lIndex := lIndex + 1;
        Result := Result + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_ExceedencePercentageArray : TExceedencePercentagesArray;
const OPNAME = 'TIFRFeature.Get_ExceedencePercentageArray';
begin
  Result := FExceedencePercentages;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_ExceedencePercentageByIndex(AIndex : integer) : double;
const OPNAME = 'TIFRFeature.Get_ExceedencePercentageByIndex';
begin
  Result := 0.0;
  try
    Result := FExceedencePercentages[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_ReferenceFlowType: TIFRFeatureReferenceFlowType;
const OPNAME = 'TIFRFeature.Get_ReferenceFlowType';
begin
  Result := ifrtMonthly;
  try
    Result := FReferenceFlowType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_InflowByIndexAndMonth(AIndex : integer;
                                               AMonth : integer) : double;
const OPNAME = 'TIFRFeature.Get_InflowByIndexAndMonth';
begin
  Result := 0.0;
  try
    {Result := StrToFloat(FAppModules.Changes.GetParameterValue
                          ('IFRVariables',
                          GetKeyValues('IFRVariables', IntToStr(AIndex) + ','+IntToStr(AMonth)),
                          FloatToStr(FInflows[AIndex,AMonth]),
                          IntToStr(AIndex) + ',' + IntToStr(AMonth)));}
    Result := FInflows[AIndex, AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_ReleaseByIndexAndMonth(AIndex : integer;
                                                AMonth : integer) : double;
const OPNAME = 'TIFRFeature.Get_ReleaseByIndexAndMonth';
begin
  Result := 0.0;
  try
    {Result := StrToFloat(FAppModules.Changes.GetParameterValue
                          ('IFRReleaseVariables',
                          GetKeyValues('IFRReleaseVariables', IntToStr(AIndex) + ','+IntToStr(AMonth)),
                          FloatToStr(FReleases[AIndex,AMonth]),
                          IntToStr(AIndex) + ',' + IntToStr(AMonth))); }
    Result := FReleases[AIndex, AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_FeatureName (const AName : WideString);
const OPNAME = 'TIFRFeature.Set_FeatureName';
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
           'IFRFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TIFRFeature.Set_Channel';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'IFRChannelNumber', IntToStr(LNewNr), IntToStr(FChannelNr), LContextData) then
        begin
          LOldNr     := FChannelNr;
          FChannelNr := lNewNr;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'IFRChannelNumber',IntToStr(LOldNr),IntToStr(LNewNr));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_ExceedencePercentageByIndex(AIndex : integer;
                                                      AValue : double);
const OPNAME = 'TIFRFeature.Set_ExceedencePercentageByIndex';

var
  LLoadAgent    : TNetworkFeaturesSQLAgent;
  LContextData  : TStringList;
  LPrevValue    : double;
  LNewCommaText,
  LOldCommaText : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LPrevValue                     := FExceedencePercentages[AIndex];
        LOldCommaText                  := ExceedencePercentagesCommaText;
        FExceedencePercentages[AIndex] := AValue;
        LNewCommaText                  := ExceedencePercentagesCommaText;

        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue('ExceedencePercentage', LNewCommaText, LOldCommaText, LContextData) then
        begin
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ExceedencePercentage',FloatToStr(LPrevValue),FloatToStr(AValue));
          if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIFRFeatureList.UpdateIFRFromReferenceInflows then
           ReCalculateReferenceFlows;
        end
        else
        begin
          FExceedencePercentages[AIndex] := LPrevValue;
        end;

      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_LagMonths (AMonths : integer);
const OPNAME = 'TIFRFeature.Set_LagMonths';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'LagInMonthsCount', FloatToStr(AMonths), FloatToStr(FLagMonths), LContextData) then
        begin
          LPrevValue := FLagMonths;
          FLagMonths := AMonths;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'LagInMonthsCount',IntToStr(LPrevValue),IntToStr(AMonths));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_MonthlyIFRLossByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TIFRFeature.Set_MonthlyIFRLossByIndex';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (FMonthlyIFRLoss.Count>0) and (AIndex<=FMonthlyIFRLoss.Count-1) then
        begin

          LPrevValue := FMonthlyIFRLoss.CommaText;
          FMonthlyIFRLoss[AIndex] := FormatFloat('#0.00',Value);
          LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
          if FAppModules.FieldProperties.UpdateFieldValue('MonthlyIFRLossStr',
            FMonthlyIFRLoss.CommaText, LPrevValue, LContextData) then
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthlyIFRLossStr',LPrevValue,FMonthlyIFRLoss.CommaText);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TIFRFeature.Set_NrOfInflowIFRPoints(ACount : integer);
const OPNAME = 'TIFRFeature.Set_NrOfInflowIFRPoints';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldCount    : integer;
  lPoint       : integer;
  lMonth       : integer;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LPrevValue := NrOfInflowIFRPoints;
        LOldCount := NrOfInflowIFRPoints;

        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'IFRPointsCount', IntToStr(ACount), IntToStr(lOldCount), LContextData) then
        begin
          if (ACount > lOldCount) then
          begin

            for lPoint := lOldCount + 1 to ACount do
            begin
              lLoadAgent.AddIFRDetails(FFeatureID, lPoint);
              Set_AnnualInflow(LPoint,0.0);
              for lMonth := 1 to 12 do
              begin
                Set_InflowByIndexAndMonth(lPoint, lMonth, 0.0);
                Set_ReleaseByIndexAndMonth(lPoint, lMonth, 0.0);
              end;
            end;
          end
          else
          if (ACount < lOldCount) then
          begin
            for lPoint := lOldCount downto ACount + 1  do
            begin
              FExceedencePercentages[lPoint] := NullFloat;
              Set_AnnualInflow(LPoint,NullFloat);
              for lMonth := 1 to 12 do
              begin
                FInflows[lPoint, lMonth]  := NullFloat;
                FReleases[lPoint, lMonth] := NullFloat;
              end;
              lLoadAgent.DeleteIFRDetails(FFeatureID, lPoint);
            end;
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRPointsCount',IntToStr(LPrevValue),IntToStr(ACount));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_ReferenceNodeNumbers : WideString;
const OPNAME = 'TIFRFeature.Get_ReferenceNodeNumbers';
begin
  Result := '';
  try
    Result := FReferenceNodeNumbers.CommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_ReferenceNodeNumbersCount (ACount : integer);
const OPNAME = 'TIFRFeature.Set_ReferenceNodeNumbersCount';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LPrevValue := FReferenceNodeNumbers.Count;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'ReferenceNodeCount', IntToStr(ACount), IntToStr(LPrevValue), LContextData) then
        begin
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReferenceNodeCount',IntToStr(LPrevValue),IntToStr(ACount));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_ReferenceNodeNumbers (const ANumbers  : WideString);
const OPNAME = 'TIFRFeature.Set_ReferenceNodeNumbers';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldVal      : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      lOldVal := FReferenceNodeNumbers.CommaText;
      if (ANumbers <> lOldVal) then
      begin
        LContextData.Clear;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if (FAppModules.FieldProperties.UpdateFieldValue
             ('RefNodeNumber', ANumbers, lOldVal, LContextData)) then
        begin
          FReferenceNodeNumbers.Clear;
          FReferenceNodeNumbers.CommaText := ANumbers;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RefNodeNumber', lOldVal, FReferenceNodeNumbers.CommaText);
          ReferenceNodeNumbersCount := FReferenceNodeNumbers.Count;
        end;
      end;
      if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIFRFeatureList.UpdateIFRFromReferenceInflows then
       ReCalculateReferenceFlows;
    finally
      LContextData.Free;
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_ReferenceNodeNumberByIndex (AIndex : integer) : integer;
const OPNAME = 'TIFRFeature.Get_ReferenceNodeNumberByIndex';
begin
  Result := -1;
  try
    Result := StrToInt(FReferenceNodeNumbers.Strings[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_ReferenceNodeNumbersCount : integer;
const OPNAME = 'TIFRFeature.Get_ReferenceNodeNumbersCount';
begin
  Result := 0;
  try
    Result := FReferenceNodeNumbers.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_InflowByIndexAndMonth(AIndex : integer;
                                                AMonth : integer;
                                                AValue : double);
const OPNAME = 'TIFRFeature.Set_InflowByIndexAndMonth';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AMonth));
        LContextData.Add('LineNumber=' + IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'IFRVariables', FloatToStr(AValue), FloatToStr(FInflows[AIndex, AMonth]), LContextData) then
        begin
          LPrevValue := FInflows[AIndex, AMonth];
          FInflows[AIndex, AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRVariables',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_ReleaseByIndexAndMonth(AIndex : integer;
                                                 AMonth : integer;
                                                 AValue : double);
const OPNAME = 'TIFRFeature.Set_ReleaseByIndexAndMonth';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AMonth));
        LContextData.Add('LineNumber=' + IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'IFRReleaseVariables', FloatToStr(AValue), FloatToStr(FReleases[AIndex, AMonth]), LContextData) then
        begin
          LPrevValue := FReleases[AIndex, AMonth];
          FReleases[AIndex, AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRReleaseVariables',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeature.Get_AnnualInflow(AIndex : integer): Double;
const OPNAME = 'TIFRFeature.Get_AnnualInflow';
begin
  Result := NullFloat;
  try
    //Result := Get_InflowByIndexAndMonth(AIndex,1);
    Result := FAnnualInflow[AIndex];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_AnnualInflow(AIndex : integer;AInflow: Double);
const OPNAME = 'TIFRFeature.Set_AnnualInflow';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if AIndex <= 10 then
        begin
          LLoadAgent.LoadContextData_FeatureIDFieldNameID
            (LContextData, IntToStr(FFeatureID), IntToStr(1));
          LContextData.Add('LineNumber=' + IntToStr(AIndex));

          if FAppModules.FieldProperties.UpdateFieldValue(
            'AnnualInflow', FloatToStr(AInflow), FloatToStr(FAnnualInflow[AIndex-1]), LContextData) then
          begin
            LPrevValue := FAnnualInflow[AIndex-1];
            FAnnualInflow[AIndex-1] := AInflow;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'AnnualInflow',FloatToStr(LPrevValue),FloatToStr(AInflow));
          end;
        end;  
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
    //Set_InflowByIndexAndMonth(AIndex,1,AInflow)
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_CalculationOption: Double;
const OPNAME = 'TIFRFeature.Get_CalculationOption';
begin
  Result := NullFloat;
  try
    Result := FCalculationOption;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_CalculationOption(AOption: Double);
const OPNAME = 'TIFRFeature.Set_CalculationOption';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : Double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'IFRCalcOption', FloatToStr(AOption), FloatToStr(FCalculationOption), LContextData) then
        begin
          LPrevValue := FCalculationOption;
          FCalculationOption := AOption;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRCalcOption',FloatToStr(LPrevValue),FloatToStr(AOption));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_IFRLoss: Integer;
const OPNAME = 'TIFRFeature.Get_IFRLoss';
begin
  Result := NullInteger;
  try
    Result := FIFRLoss;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.Get_IFRSiteID: integer;
const OPNAME = 'TIFRFeature.Get_IFRSiteID';
begin
  Result := NullInteger;
  try
    Result := FIFRSiteID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.Set_IFRLoss(Value: Integer);
const OPNAME = 'TIFRFeature.Set_IFRLoss';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LIndex,
  LPrevValue   : Integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'IFRLoss', IntToStr(Value), IntToStr(FIFRLoss), LContextData) then
        begin
          LPrevValue := FIFRLoss;
          FIFRLoss := Value;
          if (FIFRLoss = 1) then
          begin
            for LIndex := 0 to 11 do
              FMonthlyIFRLoss.Add('0.00');
            Set_MonthlyIFRLossByIndex(0,0.00);
          end
          else
          begin
            FMonthlyIFRLoss.Clear;
            FAppModules.FieldProperties.UpdateFieldValue('MonthlyIFRLossStr',
            FMonthlyIFRLoss.CommaText, '', LContextData)
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRLoss',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
procedure TIFRFeature.Set_IFRSiteID(ASiteID: integer);
const OPNAME = 'TIFRFeature.Set_IFRSiteID';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'IFRSiteID', IntToStr(ASiteID), IntToStr(FIFRSiteID), LContextData) then
        begin
          LPrevValue := FIFRSiteID;
          FIFRSiteID := ASiteID;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteID',IntToStr(LPrevValue),IntToStr(ASiteID));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.ValidateReferenceNodes(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TIFRFeature.ValidateReferenceNodes';
var
  lIndex            : integer;
  lMessage          : string;
  lResult           : Boolean;
  lStopOnFirstError : Boolean;
  lNode             : IReservoirData;
  lNodeNr           : integer;
  lValid            : Boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;

    if (FReferenceNodeNumbers.Count < 0) then
    begin
      lResult := FALSE;
      lMessage := FAppModules.Language.GetString('ContextValidation.IFRReferenceNodeNotSpecified');
      AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lIndex := 0;
      while (lIndex < FReferenceNodeNumbers.Count) do
      begin
        lMessage := '';
        lNodeNr  := StrToInt(FReferenceNodeNumbers[lIndex]);
        lValid   := FAppModules.FieldProperties.ValidateFieldProperty
                      ('RefNodeNumber', IntToStr(lNodeNr), lMessage, lIndex+1);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
//          FAppModules.Model.ModelData.
//          LRes.

          lNode := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lNodeNr];
          if (lNode = nil) then
          begin
            lResult := FALSE;
            lMessage := FAppModules.Language.GetString('ContextValidation.InvalidIFRReferenceNodeNumber');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
          end;
        end;
        if ((NOT lResult) AND lStopOnFirstError) then
          Break;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lValid := FAppModules.FieldProperties.ValidateFieldProperty(
                  'ReferenceNodeCount', IntToStr(FReferenceNodeNumbers.Count), lMessage);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add(lMessage);
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeature.ValidateInflows (AErrorMessages : TStrings;
                                      AErrorColumns  : TStringList): WordBool;
const OPNAME = 'TIFRFeature.ValidateInflows';
var
  lIndex            : integer;
  lMonth            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : boolean;
  lResult           : boolean;
  lAscending        : Boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := 1 to NrOfInflowIFRPoints do
    begin
      for lMonth := 1 to 12 do
      begin
        lMessage := '';
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('IFRVariables', FloatToStr(FInflows[lIndex, lMonth]),
                    lMessage, lIndex, lMonth);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
          if (AErrorColumns.IndexOf(IntToStr(lMonth )) < 0 ) then
            AErrorColumns.Add(IntToStr(lMonth));   
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lAscending := TRUE;
      lMonth := 1;
      while (lMonth <= 12) do
      begin
        lValid := TRUE;
        lIndex := 1;
        while (lValid AND (lIndex < NrOfInflowIFRPoints)) do
        begin
          if (FInflows[lIndex+1, lMonth] <> NullFloat) and
             (FInflows[lIndex, lMonth] > FInflows[lIndex+1, lMonth]) then
          begin
            AErrorColumns.Add(IntToStr(lMonth));
            lValid     := FALSE;
            lAscending := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        lMonth := lMonth + 1;
      end;
      if (NOT lAscending) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.IFRValuesNotInAscendingOrder');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeature.ValidateReleases (AErrorMessages : TStrings;
                                       AErrorColumns  : TStringList): WordBool;
const OPNAME = 'TIFRFeature.ValidateReleases';
var
  lIndex            : integer;
  lMonth            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : boolean;
  lResult           : boolean;
  LDiscending,
  lAscending        : Boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := 1 to NrOfInflowIFRPoints do
    begin
      for lMonth := 1 to 12 do
      begin
        lMessage := '';
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('IFRReleaseVariables', FloatToStr(FReleases[lIndex, lMonth]),
                    lMessage, lIndex, lMonth);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
          if (AErrorColumns.IndexOf(IntToStr(lMonth )) < 0 ) then
            AErrorColumns.Add(IntToStr(lMonth));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lAscending := TRUE;
      LDiscending := True;
      lMonth := 1;
      while (lMonth <= 12) do
      begin
        lValid := TRUE;
        lIndex := 1;
        while (lValid AND (lIndex < NrOfInflowIFRPoints)) do
        begin
          if (Get_ReferenceFlowType = ifrtMonthly) and (FReleases[lIndex+1, lMonth] <> NullFloat) and
             (FReleases[lIndex, lMonth] > FReleases[lIndex+1, lMonth]) then
          begin
            AErrorColumns.Add(IntToStr(lMonth));
            lValid     := FALSE;
            lAscending := FALSE;
          end
          else
          if (Get_ReferenceFlowType = ifrrftAnnual) and (FReleases[lIndex+1, lMonth] <> NullFloat) and
             (FReleases[lIndex, lMonth] < FReleases[lIndex+1, lMonth]) then
          begin
            AErrorColumns.Add(IntToStr(lMonth));
            lValid     := FALSE;
            LDiscending := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        lMonth := lMonth + 1;
      end;
      if (NOT lAscending) or not (LDiscending) then
      begin
        lResult := FALSE;
        if Get_ReferenceFlowType = ifrtMonthly then
          lMessage := FAppModules.Language.GetString('ContextValidation.IFRReleaseNotInAscendingOrder')
        else
        if Get_ReferenceFlowType = ifrrftAnnual then
          lMessage := FAppModules.Language.GetString('ContextValidation.IFRAnnualReleaseNotIndiscendingOrder');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeature.ValidateLagInMonths(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIFRFeature.ValidateLagInMonths';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('LagInMonthsCount', IntToStr(FLagMonths), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeature.ValidatePointsCount(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIFRFeature.ValidatePointsCount';
var
  LMessage : string;
  LResult  : Boolean;
  LValid   : Boolean;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('IFRPointsCount');
    if assigned(lFieldProperty) then
    begin
      lMessage := '';
      LValid := FAppModules.FieldProperties.ValidateFieldProperty
                ('IFRPointsCount', IntToStr(NrOfInflowIFRPoints),lMessage);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+LMessage);
      end;
    end;
      Result := lResult;
except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.ValidateAnnualNumberOfClasses(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIFRFeature.ValidateAnnualNumberOfClasses';
var
  LMessage : string;
  LResult  : Boolean;
  LValid   : Boolean;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('AnnualNumberOfClasses');
    if assigned(lFieldProperty) then
    begin
      lMessage := '';
      LValid := FAppModules.FieldProperties.ValidateFieldProperty
                ('AnnualNumberOfClasses', IntToStr(NrOfInflowIFRPoints),lMessage);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+LMessage);
      end;
    end;
      Result := lResult;
except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeature.ValidateCalcOption(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIFRFeature.ValidateCalcOption';
var
  LMessage : string;
  LResult  : Boolean;
  LValid   : Boolean;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('IFRCalcOption');
    if assigned(lFieldProperty) then
    begin
      lMessage := '';
      LValid := FAppModules.FieldProperties.ValidateFieldProperty
                ('IFRCalcOption', IntToStr(NrOfInflowIFRPoints),lMessage);
      if (not lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+LMessage);
      end;
    end;
      Result := lResult;
except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.ValidateAnnualInflow(AErrorMessages : TStrings;AErrorColumns  : TStringList): WordBool;
const OPNAME = 'TIFRFeature.ValidateAnnualInflow';
var
  LMessage : string;
  LResult  : Boolean;
  LValid   : Boolean;
  LIndex   : integer;
  LFieldProperty    : TAbstractFieldProperty;
  LDiscending : boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LDiscending := True;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('AnnualInflow');
    if assigned(lFieldProperty) then
    begin
      lMessage := '';
      for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        if FAnnualInflow[LIndex-1] = NullFloat then Continue;
        LValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('AnnualInflow', FloatToStr(FAnnualInflow[LIndex-1]),LMessage,LIndex);
        if (not lValid) then
        begin
          lResult := False;
          AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+LMessage);
        end;
      end;

      for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        if FAnnualInflow[LIndex-1] = NullFloat then Continue;
        if (FAnnualInflow[LIndex-1] <> NullFloat) and
           (FAnnualInflow[(LIndex-1)+1] > FAnnualInflow[LIndex-1]) then
        begin
          AErrorColumns.Add(IntToStr(LIndex));
          LDiscending := False;
          Break;
        end;
      end;
      if not LDiscending then
      begin
        lResult := FALSE;
        LMessage := FAppModules.Language.GetString('ContextValidation.IFRAnnualInFlowNotIndiscendingOrder');
        AErrorMessages.Add('ERROR:' +Format(LMessage, [Channel.ChannelName]));
      end;
    end;
      Result := LResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeature.Validate (var AErrors : WideString;
                               const AContext    : WideString) : WordBool;
const OPNAME = 'TIFRFeature.Validate';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lErrorCols        : TStringList;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorCols := TStringList.Create;
    lErrorList := TStringList.Create;
    try
      if (AContext = 'FeatureName') then
        Result := ValidateFeatureName(lErrorList)
      else
      if (AContext = 'LagInMonths') then
        Result := ValidateLagInMonths(lErrorList)
      else
      if (AContext = 'ReferenceNodes') then
        Result := ValidateReferenceNodes(lErrorList)
      else
      if (AContext = 'IFRPointsCount') then
        Result := ValidatePointsCount(lErrorList)
      else
      if (AContext = 'AnnualNumberOfClasses') then
        Result := ValidatePointsCount(lErrorList)
      else
      if (AContext = 'IFRCalcOption') then
        Result := ValidateCalcOption(lErrorList)
      else

      if (AContext = 'Inflows') then
      begin
        Result := ValidateInflows(lErrorList,lErrorCols);
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
      if (AContext = 'Releases') then
      begin
        Result := ValidateReleases(lErrorList,lErrorCols);
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
      if (AContext = 'AnnualInflow') then
      begin
        Result := ValidateAnnualInflow(lErrorList,lErrorCols);
        if (not Result) then
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
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateLagInMonths(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateReferenceNodes(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePointsCount(lErrorList)) then
            Result := FALSE;
        end;

        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateInflows(lErrorList,lErrorCols)) then
          begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorList.Text
            else
              AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                   CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorList.Clear;
            lErrorCols.Clear;
          end;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateReleases(lErrorList,lErrorCols)) then
          begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorList.Text
            else
              AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                   CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorList.Clear;
            lErrorCols.Clear;
          end;
        end;
        if (FChannelNr = 0) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.IFRFeatureNotAssignedToChannel');
          lErrorList.Add(Format(lMessage, [IntToStr(FFeatureID)]));
          Result := TRUE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      lErrorList.Free;
      lErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TIFRFeatureList                                                              }
{******************************************************************************}

function TIFRFeatureList._AddRef: Integer;
const OPNAME = 'TIFRFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList._Release: Integer;
const OPNAME = 'TIFRFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeatureList.CreateMemberObjects;
const OPNAME = 'TIFRFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    //FIFRFeatureList := TObjectList.Create(True);
    FMonthlyIFRFeatureList := TObjectList.Create;
    FAnnualIFRFeatureList := TObjectList.Create;
    FInflowOption   := 1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeatureList.DestroyMemberObjects;
const OPNAME = 'TIFRFeatureList.DestroyMemberObjects';
begin
  try
    //FreeAndNil(FIFRFeatureList);
    FreeAndNil(FMonthlyIFRFeatureList);
    FreeAndNil(FAnnualIFRFeatureList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Initialise: boolean;
const OPNAME = 'TIFRFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    //FIFRFeatureList.Clear;
    FMonthlyIFRFeatureList.Clear;
    FAnnualIFRFeatureList.Clear;
    FInflowOption   := 1;
    FUpdateIFRFromReferenceInflows := False;
    FIFRFeatureExists := False;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.NewMonthlyIFRFeature : TIFRFeature;
const OPNAME = 'TIFRFeatureList.NewMonthlyIFRFeature';
var
  lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lFeature := TIFRFeature.Create(FAppModules);
    lFeature.Initialise;
    FMonthlyIFRFeatureList.Add(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.NewAnnualIFRFeature: TIFRFeature;
const OPNAME = 'TIFRFeatureList.NewAnnualIFRFeature';
var
  lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lFeature := TIFRFeature.Create(FAppModules);
    lFeature.Initialise;
    FAnnualIFRFeatureList.Add(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.CreateNewIFRFeature(AIFRType: TIFRFeatureReferenceFlowType): TIFRFeature;
const OPNAME = 'TIFRFeatureList.CreateNewIFRFeature';
var
  LFeatureID : integer;
  LLoadAgent : TNetworkFeaturesSQLAgent;
  LFeature   : TIFRFeature;
begin
  Result := nil;
  try
    if(AIFRType = ifrtNone) then Exit;
    if(FMonthlyIFRFeatureList.Count = 0) and (FAnnualIFRFeatureList.Count = 0) then
      Set_InflowOption(1);

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertIFRFeature(LFeatureID,NullFloat,Ord(AIFRType),0)) then
      begin
        if AIFRType = ifrtMonthly then
          LFeature := NewMonthlyIFRFeature
        else
          LFeature := NewAnnualIFRFeature;



        LFeature.Initialise;
        LFeature.PopulateSome(LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.IFRFeature')) + ' ' + IntToStr(LFeatureID),
           0, 7, 0, 0,NullFloat,NullInteger,AIFRType,0);
        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_MaximumMonthlyControlStructCount: integer;
const OPNAME = 'TIFRFeatureList.Get_MaximumMonthlyControlStructCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IFRMonthlyStructureCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_MaximumAnnualControlStructCount: integer;
const OPNAME = 'TIFRFeatureList.Get_MaximumAnnualControlStructCount';
var
  LFieldProperty:TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IFRAnnualStructureCount');
    if Assigned(LFieldProperty) then
      Result := StrToIntDef(LFieldProperty.FieldMaximumValue,Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeatureList.CreateIFRFeature(AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature;
const OPNAME = 'TIFRFeatureList.CreateIFRFeature';
var
  LFeature : IIFRFeature;
begin
  Result := nil;
  try
    if(AIFRType = ifrtNone) then Exit;
    if(AIFRType = ifrtMonthly) and (MonthlyIFRFeatureCount > MaximumMonthlyControlStructCount) then
    begin
      ShowMessage(FAppModules.Language.GetString('ContextValidation.IFRMonthlyCountErr'));
      Exit;
    end;

    if(AIFRType = ifrrftAnnual) and (AnnualIFRFeatureCount > MaximumAnnualControlStructCount) then
    begin
      ShowMessage(FAppModules.Language.GetString('ContextValidation.IFRAnnualCountErr'));
      Exit;
    end;

    lFeature := CreateNewIFRFeature(AIFRType);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.RemoveIFRFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TIFRFeatureList.RemoveIFRFeatureWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteIFRFeature(AFeatureID) then
        begin
          DeleteIFRFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.DeleteIFRFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TIFRFeatureList.DeleteIFRFeatureWithID';
var
  lFeature : TIFRFeature;
begin
  Result := FALSE;
  try
    lFeature := CastMonthlyIFRFeatureByID(AFeatureID);
    if lFeature = nil then
      lFeature := CastAnnualIFRFeatureByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      if lFeature.FReferenceFlowType = ifrtMonthly then
        FMonthlyIFRFeatureList.Remove(lFeature)
      else
      if lFeature.FReferenceFlowType = ifrrftAnnual then
        FAnnualIFRFeatureList.Remove(lFeature);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_MonthlyIFRFeatureByIndex(AIndex: integer): IIFRFeature;
const OPNAME = 'TIFRFeatureList.Get_MonthlyIFRFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMonthlyIFRFeatureList.Count) then
      Result := TIFRFeature(FMonthlyIFRFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_AnnualIFRFeatureByIndex(AIndex: integer): IIFRFeature;
const OPNAME = 'TIFRFeatureList.Get_AnnualIFRFeatureByIndex';
begin
  Result := nil;
  try
   if (AIndex >= 0) and (AIndex < FAnnualIFRFeatureList.Count) then
     Result := TIFRFeature(FAnnualIFRFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_MonthlyIFRFeatureByID(AFeatureID: integer): IIFRFeature;
const OPNAME = 'TIFRFeatureList.Get_MonthlyIFRFeatureByID';
var
 lIndex   : integer;
 lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMonthlyIFRFeatureList.Count)) do
    begin
      lFeature := TIFRFeature(FMonthlyIFRFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_AnnualIFRFeatureByID(AFeatureID: integer): IIFRFeature;
const OPNAME = 'TIFRFeatureList.Get_AnnualIFRFeatureByID';
var
 lIndex   : integer;
 lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FAnnualIFRFeatureList.Count)) do
    begin
      lFeature := TIFRFeature(FAnnualIFRFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) and (lFeature.Get_ReferenceFlowType = ifrrftAnnual)then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.CastMonthlyIFRFeatureByIndex (AIndex : integer): TIFRFeature;
const OPNAME = 'TIFRFeatureList.CastMonthlyIFRFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMonthlyIFRFeatureList.Count) then
      Result := TIFRFeature(FMonthlyIFRFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.CastAnnualIFRFeatureByIndex (AIndex : integer): TIFRFeature;
const OPNAME = 'TIFRFeatureList.CastAnnualIFRFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FAnnualIFRFeatureList.Count) then
      Result := TIFRFeature(FAnnualIFRFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeatureList.CastMonthlyIFRFeatureByNumber(AFeatureNumber: integer): TIFRFeature;
const OPNAME = 'TIFRFeatureList.CastMonthlyIFRFeatureByNumber';
var
 lIndex   : integer;
 lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMonthlyIFRFeatureList.Count)) do
    begin
      lFeature := TIFRFeature(FMonthlyIFRFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AFeatureNumber) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.CastAnnualIFRFeatureByNumber(AFeatureNumber: integer): TIFRFeature;
const OPNAME = 'TIFRFeatureList.CastAnnualIFRFeatureByNumber';
var
 lIndex   : integer;
 lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FAnnualIFRFeatureList.Count)) do
    begin
      lFeature := TIFRFeature(FAnnualIFRFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AFeatureNumber) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeatureList.CastMonthlyIFRFeatureByID(AFeatureID: integer): TIFRFeature;
const OPNAME = 'TIFRFeatureList.CastMonthlyIFRFeatureByID';
var
 lIndex   : integer;
 lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMonthlyIFRFeatureList.Count)) do
    begin
      lFeature := TIFRFeature(FMonthlyIFRFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.CastAnnualIFRFeatureByID(AFeatureID: integer): TIFRFeature;
const OPNAME = 'TIFRFeatureList.CastAnnualIFRFeatureByID';
var
 lIndex   : integer;
 lFeature : TIFRFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FAnnualIFRFeatureList.Count)) do
    begin
      lFeature := TIFRFeature(FAnnualIFRFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeatureList.Get_MonthlyIFRFeatureCount: integer;
const OPNAME = 'TIFRFeatureList.Get_MonthlyIFRFeatureCount';
begin
  Result := 0;
  try
    Result := FMonthlyIFRFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_AnnualIFRFeatureCount: integer;
const OPNAME = 'TIFRFeatureList.Get_AnnualIFRFeatureCount';
begin
  Result := 0;
  try
    Result := FAnnualIFRFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.ValidateFeatureName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIFRFeature.ValidateFeatureName';
var
  lMessage     : string;
  lUnique      : boolean;
  lIndex       : integer;
  lFeatureList : TIFRFeatureList;
  lFeature     : TIFRFeature;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('IFRFeatureName',FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +Channel.ChannelName +':'+lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastIFRFeatureList;
      lUnique := TRUE;
      lIndex := 0;
      if FReferenceFlowType = ifrtMonthly then
      begin
        while (lUnique AND (lIndex < lFeatureList.MonthlyIFRFeatureCount)) do
        begin
          lFeature := lFeatureList.CastMonthlyIFRFeatureByIndex(lIndex);
          if ((FFeatureID <> lFeature.FeatureID) AND
              (UpperCase(trim(lFeature.FeatureName)) = UpperCase(trim(FFeatureName)))) then
          begin
            lMessage := FAppModules.language.GetString('ContextValidation.DuplicateIFRFeatureName');
            AErrorMessages.Add('WARNING:' +Format(lMessage, [Channel.ChannelName]));
            lUnique := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TIFRFeatureList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
  lErrorList    : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      Result := TRUE;
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (NOT ValidateMonthlyStructureCount(lErrorList)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
      begin
        AErrors := AErrors + lErrorList.Text;
        Exit;
      end;

      if (NOT ValidateAnnualStructureCount(lErrorList)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
      begin
        AErrors := AErrors + lErrorList.Text;
        Exit;
      end;

      for LIndex := 0 to FMonthlyIFRFeatureList.Count -1 do
      begin
        if (NOT MonthlyIFRFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
          Result := False;
        if ((NOT Result ) AND lStopOnFirstError) then
          Break;
      end;

      for LIndex := 0 to FAnnualIFRFeatureList.Count -1 do
      begin
        if (NOT AnnualIFRFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
          Result := False;
        if ((NOT Result ) AND lStopOnFirstError) then
          Break;
      end;

    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.ValidateAnnualStructureCount(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIFRFeatureList.ValidateAnnualStructureCount';
var
  lMessage : string;
begin
  Result := False;
  try
    if(AnnualIFRFeatureCount = 0) then
      Result := True
    else
    begin
      lMessage := '';
      if (not FAppModules.FieldProperties.ValidateFieldProperty('IFRAnnualStructureCount',
          IntToStr(AnnualIFRFeatureCount), lMessage)) then
        AErrorMessages.Add('ERROR:'+lMessage)
      else
        Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.ValidateMonthlyStructureCount(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIFRFeatureList.ValidateMonthlyStructureCount';
var
  lMessage : string;
begin
  Result := False;
  try
    if(MonthlyIFRFeatureCount = 0) then
      Result := True
    else
    begin
      lMessage := '';
      if (not FAppModules.FieldProperties.ValidateFieldProperty('IFRMonthlyStructureCount',
         IntToStr(MonthlyIFRFeatureCount), lMessage)) then
        AErrorMessages.Add('ERROR:'+lMessage)
      else
        Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.Get_InflowOption: integer;
const OPNAME = 'TIFRFeatureList.Get_InflowOption';
begin
  Result := 0;
  try
    Result := FInflowOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeatureList.Set_InflowOption(AInflowOption: integer);
const OPNAME = 'TIFRFeatureList.Set_InflowOption';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if LLoadAgent.InsertIFRDetails(AInflowOption) then
        begin
          LPrevValue := FInflowOption;
          FInflowOption := AInflowOption;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRInflowOption',IntToStr(LPrevValue),IntToStr(AInflowOption));
        end
        else
        begin
          LLoadAgent.LoadContextData(LContextData);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'IFRInflowOption', FloatToStr(AInflowOption), FloatToStr(FInflowOption), LContextData) then
          begin
            LPrevValue := FInflowOption;
            FInflowOption := AInflowOption;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRInflowOption',IntToStr(LPrevValue),IntToStr(AInflowOption));
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

function TIFRFeatureList.PopulateInflowOption(AInflowOption: integer): WordBool;
const OPNAME = 'TIFRFeatureList.PopulateInflowOption';
begin
  Result := False;
  try
    FInflowOption := AInflowOption;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIFRFeature.GetBaseValue (const AParamField : WideString;
                                   const AFieldIndex : WideString): WideString;
const OPNAME = 'TIFRFeature.GetBaseValue';
var
  lFieldProperty : TAbstractFieldProperty;
  lFormatStr     : string;
  lDim1Idx       : integer;
  lDim2Idx       : integer;
begin
  Result := '';
  try
    lFormatStr := '';
    lFieldProperty := FAppModules.FieldProperties.FieldProperty(AParamField);
    if (lFieldProperty <> nil) then
      lFormatStr := lFieldProperty.FormatStringGrid;

    FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
    if (AParamField = 'LagInMonthsCount') then
    begin
      if (lFormatStr = '') then
        Result := IntToStr(FLagMonths)
      else
        Result := Format(lFormatStr, [FLagMonths]);
    end
    else
    if (AParamField = 'IFRVariables') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FInflows[lDim1Idx,lDim2Idx])
      else
        Result := Format(lFormatStr, [FInflows[lDim1Idx,lDim2Idx]]);
    end
    else
    if (AParamField = 'IFRReleaseVariables') then
    begin
      if (lFormatStr = '') then    
        Result := FloatToStr(FReleases[lDim1Idx,lDim2Idx])
      else
        Result := Format(lFormatStr, [FReleases[lDim1Idx,lDim2Idx]]);
    end
    ;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.GetKeyValues (const AParamField : WideString;
                                   const AFieldIndex : WideString) : WideString;
const OPNAME = 'TIFRFeature.GetKeyValues';
var
  lDim1Idx     : integer;
  lDim2Idx     : integer;
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FFeatureID)
    else
      Result := Result + ',Identifier=' + IntToStr(FFeatureID);
    if (AParamField = 'IFRVariables') OR
       (AParamField = 'IFRReleaseVariables') then
    begin
      FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
      Result := Result + ',LineNumber=' + IntToStr(lDim1Idx);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeature.PopulateWithSiteData(AIFRSite: TIFRSiteDataObject): boolean;
const OPNAME = 'TIFRFeature.PopulateWithSiteData';
var
  LRow,
  LCol: integer;
  LValue : double;
  LExceedence: TAbstractFieldProperty;
  LDaysInMonth : array[1..12] of double;
  LExceedencePercentages : TExceedencePercentagesArray;
  LIniLength : integer;
begin
  Result := False;
  try
    if(AIFRSite <> nil) then
    begin
      LExceedence := FAppModules.FieldProperties.FieldProperty('ExceedencePercentage');
      if (LExceedence = nil) then
        raise Exception.Create('Field (ExceedencePercentage) not found in field properties');
      SetLength(LExceedencePercentages,LExceedence.ArrayLength);
      LIniLength := Length(AIFRSite.ExceedencePercentageArray);
      if (LIniLength < 12) then
      begin
        Set_NrOfInflowIFRPoints(LIniLength+2);
        FExceedencePercentages[1] := 100;
        FExceedencePercentages[NrOfInflowIFRPoints] := 0;
      end
      else
        Set_NrOfInflowIFRPoints(Length(AIFRSite.ExceedencePercentageArray));

      Set_IFRSiteID(AIFRSite.SiteIdentifier);
      if(NrOfInflowIFRPoints > 0) then
      begin
        //for LRow := Low(FExceedencePercentages) to High(FExceedencePercentages) do
          //FExceedencePercentages[LRow] := NullFloat;
        if (LIniLength < 12) then
        begin
          for LRow := 2 to NrOfInflowIFRPoints-1 do
            FExceedencePercentages[LRow] := AIFRSite.ExceedencePercentageArray[LRow-2];
        end
        else
          for LRow := 1 to NrOfInflowIFRPoints do
            FExceedencePercentages[LRow] := AIFRSite.ExceedencePercentageArray[LRow-1];

        Set_ExceedencePercentageByIndex(1,FExceedencePercentages[1]);

        for LCol := 1 to 12 do
        begin
          LDaysInMonth[LCol] := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LCol];
        end;

        if LIniLength < 12 then
        begin
          for LCol := 1 to 12 do
            Set_ReleaseByIndexAndMonth(1,LCol,0);
          for LRow := 2 to NrOfInflowIFRPoints-1 do
          begin
            for LCol := 1 to 12 do
            begin
              LValue := AIFRSite.RequiredFlowsArray[LRow-2,LCol-1];
              if(LValue < 0.0) then
                LValue := 0.0;
              LValue := (LValue * 1000000.0)/(LDaysInMonth[LCol] * 24*60*60);
              Set_ReleaseByIndexAndMonth(LRow,LCol,LValue);
            end;
          end;

          for LCol := 1 to 12 do
            Set_ReleaseByIndexAndMonth(NrOfInflowIFRPoints,LCol,Get_ReleaseByIndexAndMonth(NrOfInflowIFRPoints-1,LCol));

        end
        else
        begin
          for LRow := 1 to NrOfInflowIFRPoints do
          begin
            for LCol := 1 to 12 do
            begin
              LValue := AIFRSite.RequiredFlowsArray[LRow-1,LCol-1];
              if(LValue < 0.0) then
                LValue := 0.0;
              LValue := (LValue * 1000000.0)/(LDaysInMonth[LCol] * 24*60*60);
              if (FExceedencePercentages[LRow] >= 100) then
                Set_ReleaseByIndexAndMonth(LRow,LCol,0)
              else
              if (FExceedencePercentages[LRow] <= 0) and (LRow > 1) then
                Set_ReleaseByIndexAndMonth(LRow,LCol,ReleaseByIndexAndMonth[LRow-1,LCol])
              else
                Set_ReleaseByIndexAndMonth(LRow,LCol,LValue);
            end;
          end;
        end;
      end;
      if(Trim(FFeatureName) = '') and (Trim(AIFRSite.SiteName) <> '') then
        FeatureName := AIFRSite.SiteName;
      Result := True;
      Finalize(LExceedencePercentages);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.ExceedencePercentagesCommaText: string;
const OPNAME = 'TIFRFeature.ExceedencePercentagesCommaText';
var
  LValues : TStringList;
  LIndex : integer;
begin
  Result := '';
  try
    LValues := TStringList.Create;
    try
      for LIndex := 1 to High(FExceedencePercentages) do
      begin
        if(FExceedencePercentages[LIndex] = NullFloat) then
          Break;
        LValues.Add(FloatToStr(FExceedencePercentages[LIndex]));
      end;
      Result := LValues.CommaText;
    finally
      FreeAndNil(LValues);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.ReCalculateReferenceFlows;
const OPNAME = 'TIFRFeature.ReCalculateReferenceFlows';
var
  LReferenceFlowData : TStringList;
  LResult : boolean;
begin
  try
    if (FReferenceNodeNumbers.Count = 0) then
    begin
      ClearReferenceFlows;
    end
    else
    begin
      LReferenceFlowData := TStringList.Create;
      try
        LResult := GetNodeReferenceFlowData(LReferenceFlowData);
        if LResult then
        begin
          UpdatePercentileReferenceFlows(LReferenceFlowData);
        end;
      finally
        LReferenceFlowData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.UpdatePercentileReferenceFlows(AReferenceFlows: TStrings): boolean;
const OPNAME = 'TIFRFeature.UpdatePercentileReferenceFlows';
var
  LStringValue: string;
  LFloatValue : double;
  LColumnsData : TStringListOfStringLists;
  LCurrentColumn,
  LLineData   : TStringList;
  LIntLineNo1,
  LIntLineNo2,
  LCol,
  LRow : integer;
  LRealLineNo,
  LPercentile: double;
  LX,
  LX1,
  LX2,
  LY1,
  LY2: double;
begin
  Result := False;
  try
    LColumnsData := TStringListOfStringLists.Create;
    LLineData   := TStringList.Create;
    try
      for LCol := 1 to 12 do
      begin
        LColumnsData.Add(IntToStr(LCol));
        LCurrentColumn  := LColumnsData.Row[LCol-1];
        LCurrentColumn.Sorted := True;
        LCurrentColumn.Duplicates := dupAccept;
      end;

      for LRow := 0 to AReferenceFlows.Count-1 do
      begin
        LLineData.CommaText := AReferenceFlows.Strings[LRow];
        for LCol := 1 to 12 do
        begin
          LStringValue    := LLineData[LCol];
          LFloatValue     := StrToFloat(LStringValue);
          LCurrentColumn  := LColumnsData.Row[LCol-1];
          LCurrentColumn.Add(FormatFloat('00000000000000.000',LFloatValue));
        end;
      end;

      for LCol := 1 to 12 do
      begin
        LLineData.Clear;
        LCurrentColumn  := LColumnsData.Row[LCol-1];
        for LRow := LCurrentColumn.Count-1 downto 0 do
          LLineData.Add(LCurrentColumn[LRow]);
        LCurrentColumn.Sorted := False;
        LCurrentColumn.Assign(LLineData);
      end;

      for LRow := 1 to NrOfInflowIFRPoints do
      begin
        if AReferenceFlows.Count = 0 then Exit;

        LPercentile := FExceedencePercentages[LRow];

        LRealLineNo := Round((LPercentile/ 100) * (AReferenceFlows.Count+1));
       { if(Frac(LRealLineNo) = 0) and (LPercentile <> 0) then
        begin
          LIntLineNo1 := Trunc(LRealLineNo)-1;
          LIntLineNo2 := LIntLineNo1;
        end
        else
        begin}
          if(LRealLineNo > AReferenceFlows.Count) then
          begin
            LIntLineNo1 := AReferenceFlows.Count-2;
            LIntLineNo2 := LIntLineNo1+1;
          end
          else if(LRealLineNo < 1) then
          begin
            LIntLineNo1 := 0;
            LIntLineNo2 := LIntLineNo1+1;
          end
          else
          if(LRealLineNo = AReferenceFlows.Count) then
          begin
            LIntLineNo1 := Trunc(LRealLineNo)-1;
            LIntLineNo2 := LIntLineNo1;
          end
          else
          begin
            LIntLineNo1 := Trunc(LRealLineNo)-1;
            LIntLineNo2 := LIntLineNo1+1;

          end;

        //end;

        for LCol := 1 to 12 do
        begin
          LCurrentColumn := LColumnsData.Row[LCol-1];
          if(LIntLineNo1 = LIntLineNo2) then
          begin
            LFloatValue := StrToFloat(LCurrentColumn[LIntLineNo1-1]);
          end
          else
          begin
            LX  := LPercentile;
            LX1 := ((LIntLineNo1)/LCurrentColumn.Count)* 100;
            LX2 := ((LIntLineNo2)/LCurrentColumn.Count)* 100;
            LY1 := StrToFloat(LCurrentColumn[LIntLineNo1]);
            LY2 := StrToFloat(LCurrentColumn[LIntLineNo2]);
            LFloatValue := (((LX-LX1)/(LX2-LX1))*(LY2-LY1))+ LY1;

          end;
          if LPercentile >= 100 then
            InflowByIndexAndMonth[LRow,LCol] := 0
          else
          if LPercentile <= 0 then
            InflowByIndexAndMonth[LRow,LCol] := 999.999
          else
          InflowByIndexAndMonth[LRow,LCol] := LFloatValue;
        end;
      end;

      Result := True;
    finally
      LColumnsData.Free;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeature.ClearReferenceFlows;
const OPNAME = 'TIFRFeature.ClearReferenceFlows';
var
  LNoOfPoints,
  lIndex : integer;
  lMonth : integer;
  LIFRVariables:TAbstractFieldProperty;
begin
  try
    LNoOfPoints := NrOfInflowIFRPoints;
    LIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables');
    for lIndex := LIFRVariables.ArrayLow to LIFRVariables.ArrayHigh do
    begin
      for lMonth := 1 to 12 do
      begin
        FInflows[lIndex, lMonth]  := NullFloat;
      end;
    end;

    for lIndex := LIFRVariables.ArrayLow to LNoOfPoints do
    begin
      for lMonth := 1 to 12 do
      begin
        InflowByIndexAndMonth[lIndex, lMonth]  := 0.0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.GetNodeReferenceFlowData(AReferenceFlowsData: TStrings): boolean;
const OPNAME = 'TIFRFeature.GetNodeReferenceFlowData';

procedure AddDoubleStringList(ASource,ADestination: TStrings);
const OPNAME = 'AddDoubleStringList';
var
  LCount,
  LIndex: integer;
  LSourceValue,LDestinationValue: double;
  LSourceValues,LDestinationValues,
  LLineData: TStringList;
begin
  if(ADestination.Count = 0) then
  begin
    ADestination.AddStrings(ASource);
  end
  else
  begin
    LLineData      := TStringList.Create;
    LSourceValues      := TStringList.Create;
    LDestinationValues := TStringList.Create;
    try
      for LCount := 0 to ADestination.Count-1 do
      begin
        if(LCount >= ASource.Count) then Break;
         LSourceValues.CommaText := ASource[LCount];
         LDestinationValues.CommaText := ADestination[LCount];
         LLineData.Clear;
         LLineData.Add(LSourceValues[0]);
         for LIndex := 1 to LSourceValues.Count-1 do
         begin
           LSourceValue      := StrToFloat(LSourceValues[LIndex]);
           LDestinationValue := StrToFloat(LDestinationValues[LIndex]);
           LLineData.Add(FormatFloat('##0.000',LSourceValue+LDestinationValue));
         end;
         ADestination[LCount] := LLineData.CommaText;
      end;
    finally
      LLineData.Free;
      LSourceValues.Free;
      LDestinationValues.Free;
    end;
  end;
end;

var
  LIndex: integer;
  LNodeNumber: integer;
  LNodeReferenceFlows  :  TStringList;
begin
  Result := False;
  try
    if(AReferenceFlowsData = nil) then Exit;
    LNodeReferenceFlows  :=  TStringList.Create;
    try
      for LIndex := 0 to FReferenceNodeNumbers.Count-1 do
      begin
        LNodeNumber :=  StrToInt(FReferenceNodeNumbers[LIndex]);
        if GetSingleNodeReferenceFlowData(LNodeNumber,LNodeReferenceFlows) then
        begin
           AddDoubleStringList(LNodeReferenceFlows,AReferenceFlowsData);
        end;
      end;
      Result := True;
    finally
      LNodeReferenceFlows.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.GetSingleNodeReferenceFlowData(ANodeNumber: integer;AReferenceFlows: TStrings): boolean;
const OPNAME = 'TIFRFeature.GetSingleNodeReferenceFlowData';
var
  LAffDataSet: TAbstractModelDataset;
  LIrrDataSet: TAbstractModelDataset;
  LIncDataSet: TAbstractModelDataset;
  LSQLAgent : TDemandHydrologyDataSQLAgent;
  LLineData : TStringList;
  LFlowValue,
  LIncValue,
  LAffValue,
  LIrrValue: double;
  LValue,
  LIncFileName,
  LAffFileName,
  LIrrFileName,
  LFieldName,
  LSQL: string;
  LIndex : integer;
  LSelectedReservoir : IReservoirData;
  LParamReference : IParamReference;
  LDaysInMonth : array[1..12] of double;
  LYearsInAnalysis : integer;
begin
  Result := False;
  try
    AReferenceFlows.Clear;
    LSelectedReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
                          ReservoirOrNodeByIdentifier[ANodeNumber];
    if (LSelectedReservoir <> nil) then
    begin
      LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceDataByCatchNumber[
                         LSelectedReservoir.ReservoirConfigurationData.CatchmentRef];
      if (LParamReference = nil) then Exit;
      LIncFileName := LParamReference.FileReference + '.INC';
      LAffFileName := LParamReference.FileReference + '.AFF';
      LIrrFileName := LParamReference.FileReference + '.IRR';

      LSQLAgent := TDemandHydrologyDataSQLAgent.Create(FAppModules);
      try
        for LIndex := 1 to 12 do
        begin
          LDaysInMonth[LIndex] := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LIndex];
        end;
        if(TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IFRFeatureList.InflowOption = 1) then
        begin
          FAppModules.Database.CreateDataset(integer(dtExecSQL), LIncDataSet);
          try
            if Assigned(LIncDataSet) then
            begin
              LSQL := LSQLAgent.GetNodeReferenceFlowDataSQL(LIncFileName);
              LIncDataSet.SetSQL(LSQL);
              LIncDataSet.DataSet.Open;
              LYearsInAnalysis := 0;
              if LIncDataSet.DataSet.EOF and LIncDataSet.DataSet.Bof then Exit;
              LLineData := TStringList.Create;
              try
                while (NOT LIncDataSet.DataSet.EOF) do
                begin
                  if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis <= LYearsInAnalysis) then
                    Break;
                  LLineData.Clear;
                  LValue := Trim(LIncDataSet.DataSet.FieldByName('HydroYearValue').AsString);
                  LLineData.Add(LValue);
                  for LIndex := 1 to 12 do
                  begin
                    LFieldName := Format('%s%2.2d',['HydroMonthValue',LIndex]);
                    LIncValue  := LIncDataSet.DataSet.FieldByName(LFieldName).AsFloat* LSelectedReservoir.ReservoirConfigurationData.DrainageScale/100.0;
                    LFlowValue := (LIncValue * 1000000.0)/(LDaysInMonth[LIndex]*24*60*60);
                    LValue  := FormatFloat('##0.000',LFlowValue);
                    LLineData.Add(LValue);
                  end;
                  AReferenceFlows.Add(LLineData.CommaText);
                  Inc(LYearsInAnalysis);
                  LIncDataSet.DataSet.Next;
                end;
                Result := True;
              finally
                LLineData.Free;
              end
            end;
          finally
            LIncDataSet.Free;
          end;
        end
        else
        begin
          FAppModules.Database.CreateDataset(integer(dtExecSQL), LIncDataSet);
          FAppModules.Database.CreateDataset(integer(dtExecSQL), LAffDataSet);
          FAppModules.Database.CreateDataset(integer(dtExecSQL), LIrrDataSet);
          try
            if Assigned(LIncDataSet) then
            begin
              LSQL := LSQLAgent.GetNodeReferenceFlowDataSQL(LIncFileName);
              LIncDataSet.SetSQL(LSQL);
              LSQL := LSQLAgent.GetNodeReferenceFlowDataSQL(LIrrFileName);
              LIrrDataSet.SetSQL(LSQL);
              LSQL := LSQLAgent.GetNodeReferenceFlowDataSQL(LAffFileName);
              LAffDataSet.SetSQL(LSQL);

              LIncDataSet.DataSet.Open;
              LIrrDataSet.DataSet.Open;
              LAffDataSet.DataSet.Open;

              if LIncDataSet.DataSet.EOF and LIncDataSet.DataSet.Bof then Exit;
              if LIrrDataSet.DataSet.EOF and LIrrDataSet.DataSet.Bof then Exit;
              if LAffDataSet.DataSet.EOF and LAffDataSet.DataSet.Bof then Exit;
              LLineData := TStringList.Create;
              try
                LYearsInAnalysis := 0;
                while(NOT LIncDataSet.DataSet.EOF) and
                     (NOT LIrrDataSet.DataSet.EOF) and
                     (NOT LAffDataSet.DataSet.EOF) do
                begin
                  if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis <= LYearsInAnalysis) then
                    Break;
                  LLineData.Clear;
                  LValue := Trim(LIncDataSet.DataSet.FieldByName('HydroYearValue').AsString);
                  LLineData.Add(LValue);
                  for LIndex := 1 to 12 do
                  begin
                    LFieldName := Format('%s%2.2d',['HydroMonthValue',LIndex]);
                    LIncValue  := LIncDataSet.DataSet.FieldByName(LFieldName).AsFloat * LSelectedReservoir.ReservoirConfigurationData.DrainageScale/100.0;
                    LAffValue  := LAffDataSet.DataSet.FieldByName(LFieldName).AsFloat * LSelectedReservoir.ReservoirConfigurationData.AfforestationScale/100.0;
                    LIrrValue  := LIrrDataSet.DataSet.FieldByName(LFieldName).AsFloat * LSelectedReservoir.ReservoirConfigurationData.IrrigationScale/100.0;
                    LFlowValue := LIncValue - (LIrrValue+LAffValue);
                    if(LFlowValue < 0.0) then
                      LFlowValue := 0.0;
                    LFlowValue :=  (LFlowValue * 1000000.0)/(LDaysInMonth[LIndex]*24*60*60);
                    LValue     := FormatFloat('##0.000',LFlowValue);
                    LLineData.Add(LValue);
                  end;
                  AReferenceFlows.Add(LLineData.CommaText);
                  Inc(LYearsInAnalysis);
                  LIncDataSet.DataSet.Next;
                end;
                Result := True;
              finally
                LLineData.Free;
              end
            end;
          finally
            LIncDataSet.Free;
            LAffDataSet.Free;
            LIrrDataSet.Free;
          end;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeature.GetRequirementFlowFromReferenceFlow(AMonth: integer; AReferenceFlow: double): double;
const OPNAME = 'TIFRFeature.GetRequirementFlowFromReferenceFlow';
var
  lIndex : integer;
  LValue,
  LX,
  LX1,
  LX2,
  LY1,
  LY2: double;
begin
  Result := NullFloat;
  try
    if(AMonth >= 1) and (AMonth <= 12) then
    begin
      LValue := NullFloat;
      for lIndex := 1 to NrOfInflowIFRPoints do
      begin
        if CompareDouble(FInflows[lIndex,AMonth],AReferenceFlow,3) then
        begin
           LValue := FReleases[lIndex,AMonth];
           Break;
        end;
        if(FInflows[lIndex,AMonth] > AReferenceFlow) then
        begin
          if(lIndex = 1) then
          begin
            LValue := FReleases[lIndex,AMonth];
            Break;
          end
          else
          if(lIndex = NrOfInflowIFRPoints) then
          begin
            LValue := FReleases[lIndex,AMonth];
            Break;
          end
          else
          begin
            LX  := AReferenceFlow;
            LX1 := FInflows[lIndex-1,AMonth];
            LX2 := FInflows[lIndex,AMonth];
            LY1 := FReleases[lIndex-1,AMonth];
            LY2 := FReleases[lIndex,AMonth];
            LValue := (((LX-LX1)/(LX2-LX1))*(LY2-LY1))+ LY1;
          end;
          Break;
        end;
      end;
      Result := LValue;
      if Result = NullFloat then
        Result := FReleases[NrOfInflowIFRPoints,AMonth];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureList.CopyIFRFeature(ANewChannelNumber,AOldChannelNumber: integer;AIFRType: TIFRFeatureReferenceFlowType): IIFRFeature;
const OPNAME = 'TIFRFeatureList.CopyIFRFeature';
var
  LIFRFeature : TIFRFeature;
  LIFRFeatureCopy : TIFRFeature;
begin
  Result := nil;
  try
    if(AIFRType = ifrtNone) then Exit;
    if(AIFRType = ifrtMonthly) and (MonthlyIFRFeatureCount >= 50) then
    begin
      ShowMessage(FAppModules.Language.GetString('ContextValidation.IFRMonthlyCountErr'));
      Exit;
    end;

    if(AIFRType = ifrrftAnnual) and (AnnualIFRFeatureCount >= 20) then
    begin
      ShowMessage(FAppModules.Language.GetString('ContextValidation.IFRAnnualCountErr'));
      Exit;
    end;
    if(AIFRType = ifrtMonthly) then
      LIFRFeature := CastMonthlyIFRFeatureByNumber(AOldChannelNumber)
    else
      LIFRFeature := CastAnnualIFRFeatureByNumber(AOldChannelNumber);

    if (LIFRFeature <> nil) then
    begin
      LIFRFeatureCopy := CreateNewIFRFeature(AIFRType);
      if LIFRFeatureCopy <> nil then
      begin
        LIFRFeatureCopy.Assign(ANewChannelNumber,LIFRFeature);
        Result := LIFRFeatureCopy;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeature.Assign(ANewChannelNumber : integer;AIFRFeature: TIFRFeature);
const OPNAME = 'TIFRFeature.Assign';
var
  Lindex : integer;
  LMonth : integer;
  LChannel : IGeneralFlowChannel;
begin
  try
    if (AIFRFeature <> nil) then
    begin
      FeatureName := 'Copy of '+AIFRFeature.FeatureName;
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[ANewChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureType             := AIFRFeature.FeatureType;
      FeatureSubType          := AIFRFeature.FeatureSubType;
      LagMonths               := AIFRFeature.LagMonths;
      ReferenceNodeNumbers    := AIFRFeature.ReferenceNodeNumbers;
      NrOfInflowIFRPoints     := AIFRFeature.NrOfInflowIFRPoints;
      for LIndex := 1 to NrOfInflowIFRPoints do
      begin
        for LMonth := 1 to 12 do
        begin
          InflowByIndexAndMonth[LIndex,LMonth] := AIFRFeature.InflowByIndexAndMonth[LIndex,LMonth];
          ReleaseByIndexAndMonth[LIndex,LMonth] := AIFRFeature.ReleaseByIndexAndMonth[LIndex,LMonth];
        end;
        if (AIFRFeature.AnnualInflow[LIndex-1] <> NullFloat) and (LIndex <= 10) then
          AnnualInflow[LIndex] := AIFRFeature.AnnualInflow[LIndex-1];
        ExceedencePercentageByIndex[LIndex] := AIFRFeature.ExceedencePercentageByIndex[LIndex];
      end;
      CalculationOption        := AIFRFeature.CalculationOption;
      IFRSiteID                := AIFRFeature.IFRSiteID;
      FReferenceFlowType       := AIFRFeature.FReferenceFlowType;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRFeature.Get_IFRStatusIndicator: integer;
const OPNAME = 'TIFRFeature.Get_IFRStatusIndicator';
begin
  try
    Result := FIFRFeatureExists;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRFeature.Set_IFRStatusIndicator(Value: Integer);
const OPNAME = 'TIFRFeature.Set_IFRStatusIndicator';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'IFRStatusIndicator', IntToStr(Value), IntToStr(FIFRFeatureExists), LContextData) then
        begin
          LOldValue := FIFRFeatureExists;
          FIFRFeatureExists := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRFeatureName',IntToStr(LOldValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
