//
//
//  UNIT      : Contains TGrowthFactors Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 11/05/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UGrowthFactorData;

interface
uses
  SysUtils,
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UGrowthFactorDataSQLAgent,
  UYieldModelDataObject;

type
  TGrowthType = (gtDemand,gtMinMax,gtHydrology);

type

 TDemandCentreGrowthFactors = class(TAbstractAppObject,IDemandCentreGrowthFactors)
  protected
    FChannelNumber : Integer;
    FIdentifier : Integer;
    FGrowthFactor  : WideString;
    FNoOfYears : integer;
    FValidFactors : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function ValidateChannelNumber(AErrorMessages : TStrings) : WordBool;
    function ValidateFGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;

    function Get_ChannelNumber : integer; safecall;
    procedure Set_ChannelNumber(AValue : integer); safecall;
    function Get_GrowthFactors : WideString; safecall;
    procedure Set_GrowthFactors(const AValue : WideString); safecall;
  public

    function Get_GrowthFactorsCount: Integer; safecall;

    procedure Set_GrowthFactorsValueByIndex(AIndex : integer; AValue : double);safecall;
    function Get_GrowthFactorsValueByIndex(AIndex : integer) : double; safecall;
    function Get_ValidFactors : Wordbool; safecall;
    procedure Set_ValidFactors(AValue : Wordbool);safecall;
    function GenerateDemandGrowthProjections(ABaseValue: double; ABaseYear,AYearsCount,ADataStartYear : integer ): string;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    function Populate(AIdentifier,AChannelNumber, ANoOfYears : integer;AGrowthFactor: WideString;AValidFactors : boolean) : boolean;
    property Identifier    : integer read FIdentifier;

    property ChannelNumber : Integer read Get_ChannelNumber write Set_ChannelNumber;
    property GrowthFactors : Widestring read Get_GrowthFactors write Set_GrowthFactors;
    property GrowthFactorsValueByIndex[AIndex: Integer] : double read Get_GrowthFactorsValueByIndex write Set_GrowthFactorsValueByIndex;
    property GrowthFactorsCount: Integer read Get_GrowthFactorsCount;
    property ValidFactors : Wordbool read Get_ValidFactors write Set_ValidFactors;
  end;

  TMinMaxChannelGrowthFactors = class(TAbstractAppObject,IMinMaxChannelGrowthFactors)
  protected
    FMinMaxChannel : Integer;
    FArcNumber : Integer;
    FGrowthFactors : WideString;
    FIdentifier : Integer;
    FNoOfYears : integer;
    FValidFactors : Wordbool;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateBoundChannel(AErrorMessages : TStrings) : WordBool;
    function ValidateMinMaxChannel(AErrorMessages : TStrings) : WordBool;
    function ValidateFGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;

    function Get_MinMaxChannel : integer; safecall;
    procedure Set_MinMaxChannel(AValue : integer); safecall;
    function Get_ArcNumber : integer; safecall;
    procedure Set_ArcNumber(AValue: integer); safecall;
    function Get_GrowthFactors : WideString; safecall;
    procedure Set_GrowthFactors(const AValue : WideString ); safecall;
  public
    function Populate(AIdentifier,AMinMaxChannel,AArcNumber,ANoOfYears : integer;AGrowthFactor: WideString;AValidFactors: boolean) : boolean;
    function Initialise: boolean;override;

    function GenerateMinMaxChannelGrowthProjections(ABaseValue: double; ABaseYear,AYearsCount: integer ): string;
    function Get_GrowthFactorsCount: Integer; safecall;
    function Get_ValidFactors : Wordbool; safecall;
    procedure Set_ValidFactors(AValue : Wordbool);safecall;

    procedure Set_GrowthFactorsValueByIndex(AIndex : integer; AValue : double);safecall;
    function Get_GrowthFactorsValueByIndex(AIndex : integer) : double; safecall;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property MinMaxChannel : Integer read Get_MinMaxChannel write Set_MinMaxChannel;
    property Identifier : integer read FIdentifier;
    property ArcNumber : Integer read Get_ArcNumber write Set_ArcNumber;
    property GrowthFactors : WideString read Get_GrowthFactors write Set_GrowthFactors;
    property GrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_GrowthFactorsValueByIndex write Set_GrowthFactorsValueByIndex;
    property GrowthFactorsCount: Integer read Get_GrowthFactorsCount;
    property ValidFactors : Wordbool read Get_ValidFactors write Set_ValidFactors;

  end;

  THydrologyGrowthFactors = class(TAbstractAppObject,IHydrologyGrowthFactors)
  protected
    FGaugeNumber : Integer;
    FIdentifier : integer;
    FNoOfYears : integer;
    FAFFGrowthFactors : WideString;
    FIRRGrowthFactors : WideString;
    FURBGrowthFactors : WideString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateGaugeNumber(AErrorMessages : TStrings) : WordBool;
    function ValidateAFFGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
    function ValidateIRRGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
    function ValidateURBGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;

    procedure SetGrowthFactorsValueByIndex(AIndex : integer; AValue : double;AGrowthFactors : TStringList);
    function GetGrowthFactorsValueByIndex(AIndex : integer;AGrowthFactors : TStringList) : double;

    function Get_GaugeNumber : integer; safecall;
    procedure Set_GaugeNumber(AValue : integer); safecall;
    function Get_AFFGrowthFactors : WideString;safecall;
    procedure Set_AFFGrowthFactors(const AValue:WideString);safecall;
    function Get_IRRGrowthFactors : WideString;safecall;
    procedure Set_IRRGrowthFactors(const AValue:WideString);safecall;
    function Get_URBGrowthFactors : WideString;safecall;
    procedure Set_URBGrowthFactors(const AValue:WideString);safecall;

  public
    function Initialise: boolean;override;
    function Populate(AIdentifier,AGaugeNumber, ANoOfYears: integer;AAFFGrowthFactors,
                      AIRRGrowthFactors,AURBGrowthFactors: WideString) : boolean;

    function GenerateHydrologyGrowthProjections(AAFFBaseYearDemand,AIRRBaseYearDemand,AURBBaseYearDemand :double;
                                                          AYearsCount: integer; var AAFFProjections,AIRRProjections,AURBProjections: string): boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    procedure Set_AFFGrowthFactorsValueByIndex(AIndex : integer; AValue : double);safecall;
    function Get_AFFGrowthFactorsValueByIndex(AIndex : integer) : double; safecall;

    procedure Set_IRRGrowthFactorsValueByIndex(AIndex : integer; AValue : double);safecall;
    function Get_IRRGrowthFactorsValueByIndex(AIndex : integer) : double; safecall;

    procedure Set_URBGrowthFactorsValueByIndex(AIndex : integer; AValue : double);safecall;
    function Get_URBGrowthFactorsValueByIndex(AIndex : integer) : double; safecall;

    function Get_AFFGrowthFactorsCount: Integer; safecall;
    function Get_IRRGrowthFactorsCount: Integer; safecall;
    function Get_URBGrowthFactorsCount: Integer; safecall;

    property GaugeNumber : Integer read Get_GaugeNumber write Set_GaugeNumber;
    property Identifier : integer read FIdentifier;
    property AFFGrowthFactors : WideString read Get_AFFGrowthFactors write Set_AFFGrowthFactors;
    property IRRGrowthFactors : WideString read Get_IRRGrowthFactors write Set_IRRGrowthFactors;
    property URBGrowthFactors : WideString read Get_URBGrowthFactors write Set_URBGrowthFactors;
    property AFFGrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_AFFGrowthFactorsValueByIndex write Set_AFFGrowthFactorsValueByIndex;
    property IRRGrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_IRRGrowthFactorsValueByIndex write Set_IRRGrowthFactorsValueByIndex;
    property URBGrowthFactorsValueByIndex[AIndex: Integer]: Double read Get_URBGrowthFactorsValueByIndex write Set_URBGrowthFactorsValueByIndex;
    property AFFGrowthFactorsCount: Integer read Get_AFFGrowthFactorsCount;
    property IRRGrowthFactorsCount: Integer read Get_IRRGrowthFactorsCount;
    property URBGrowthFactorsCount: Integer read Get_URBGrowthFactorsCount;

  end;

  TGrowthFactors  = class(TAbstractAppObject,IGrowthFactors)
  protected
    FNumberOfYears                  : Integer;
    FSavedToDB                      : boolean;
    FGFDemandCentresObjectContainer : TObjectList;
    FGFMinMaxChannelObjectContainer : TObjectList;
    FGFHydrologyObjectContainer     : TObjectList;
    fGrowthType                     : TGrowthType;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function SetYearsCountToAllObjects(ANumberOfYears : integer) : boolean;
    function ValidateNumberOfYears(AErrorMessages : TStrings) : WordBool;

    function Get_NumberOfYears: Integer; safecall;
    procedure Set_NumberOfYears(Value: Integer); safecall;
    function Get_DemandCentresGrowthByIndex(AIndex: integer): IDemandCentreGrowthFactors; safecall;
    function Get_MinMaxChannelGrowthFactorByIndex(AIndex: integer): IMinMaxChannelGrowthFactors; safecall;
    function Get_HydrologyGrowthFactorByIndex(AIndex: integer): IHydrologyGrowthFactors; safecall;

    function Get_DemandGrowthFactorsByChannel(AChannelNumber: integer): IDemandCentreGrowthFactors; safecall;
    function Get_MinMaxChannelGrowthFactorsByMinMaxChannel(AMinMaxChannel: integer): IMinMaxChannelGrowthFactors; safecall;
    function Get_HydrologyGrowthFactorsByGaugeNumber(AGaugeNumber: integer): IHydrologyGrowthFactors; safecall;

    function GetDemandChannelGrowthFactorsByIndex(AIndex: integer): TDemandCentreGrowthFactors;
    function GetMinMaxChannelGrowthFactorsByIndex(AIndex: integer): TMinMaxChannelGrowthFactors;
    function GetHydrologyGrowthFactorsByIndex(AIndex: integer)    : THydrologyGrowthFactors;


    function GetDemandChannelGrowthFactorsByIdentifier(AIdentifier: integer): TDemandCentreGrowthFactors;
    function GetMinMaxChannelGrowthFactorsByIdentifier(AIdentifier: integer): TMinMaxChannelGrowthFactors;
    function GetHydrologyGrowthFactorsByIdentifier(AIdentifier: integer)    : THydrologyGrowthFactors;

    function ClearDemandGrowthFactorsDB : boolean;
    function ClearMinMaxChannelGrowthFactorsDB : boolean;
    function ClearHydrologyGrowthFactorsDB : boolean;
   {
    function ClearDemandGrowthProjectionsDB : boolean;
    function ClearMinMaxChannelGrowthProjectionsDB : boolean;
    function ClearHydrologyGrowthProjectionsDB : boolean;
     }
    function AddDemandGrowthFactorsToDB : boolean;
    function AddMinMaxChannelGrowthFactorsToDB : boolean;
    function AddHydrologyGrowthFactorsToDB : boolean;
    function AddGrowthFactorsConfigDataToDB(AYearsCount: Integer) : boolean;
    function GetGrowthType : TGrowthType;
    procedure SetGrowthType(AValue : TGrowthType);

  public
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Populate(ANumberOfYears: Integer):boolean;
    function Initialise: boolean;override;
    function Populated: boolean;

    function ClearDemandChannelGrowthFactors: boolean;

    function GenerateGrowthProjections: boolean;

    function AddDemandCentresGrowthFactor(AChannelNumber: integer) : IDemandCentreGrowthFactors; safecall;
    function AddMinMaxChannelGrowthFactor(AChannelNumber: integer) : IMinMaxChannelGrowthFactors; safecall;
    function AddHydrologyGrowthFactor(AGaugeNumber: integer) : IHydrologyGrowthFactors; safecall;

    function CreateDemandGrowthFactor : TDemandCentreGrowthFactors;
    function CreateMinMaxChannelGrowthFactor: TMinMaxChannelGrowthFactors;
    function CreateHydrologyGrowthFactor: THydrologyGrowthFactors;

    function CastDemandGrowthFactorByIndex(AIndex: integer): TDemandCentreGrowthFactors;
    function CastMinMaxChannelGrowthFactorByIndex(AIndex: integer): TMinMaxChannelGrowthFactors;
    function CastHydrologyGrowthFactorByIndex(AIndex: integer): THydrologyGrowthFactors;

    function CastDemandGrowthFactorByChannel(AChannelNumber: integer): TDemandCentreGrowthFactors;
    function CastMinMaxChannelGrowthFactorByMinMaxChannel(AMinMaxChannel: integer): TMinMaxChannelGrowthFactors;

    function CastMinMaxChannelGrowthFactorsByChannelArc(AChannel, AArc: integer): TMinMaxChannelGrowthFactors;

    function CastHydrologyGrowthFactorByGaugeNumber(AGaugeNumber: integer): THydrologyGrowthFactors;

    function RemoveDemandCentresGrowthFactor(AChannelNumber: integer) : WordBool; safecall;
    function RemoveMinMaxChannelGrowthFactor(AMinMaxChannel: integer) : WordBool; safecall;
    function RemoveHydrologyGrowthFactor(AGaugeNumber:integer) : WordBool; safecall;

    function DeleteDemandGrowthFactorByChannelNumber(AChannelNumber: integer): WordBool;
    function DeleteMinMaxChannelGrowthFactorByChannel(AMinMaxChannel: integer): WordBool;
    function DeleteHydrologyGrowthFactorByGauge(AGaugeNumber: integer): WordBool;

    function DemandCentresGrowthFactorsCount : integer; safecall;
    function MinMaxChannelGrowthFactorsCount : integer; safecall;
    function HydrologyGrowthFactorsCount     : integer; safecall;

    function ClearAllDataFromDB: boolean;
    function SaveAllDataToDB: boolean;

    property  DemandCentresGrowthFactorByIndex[AIndex : integer] : IDemandCentreGrowthFactors
              read Get_DemandCentresGrowthByIndex;
    property  MinMaxChannelGrowthFactorByIndex[AMinMaxChannelNumber: integer] : IMinMaxChannelGrowthFactors
              read Get_MinMaxChannelGrowthFactorByIndex;
    property  HydrologyGrowthFactorByIndex[AGaugeNumber: integer] : IHydrologyGrowthFactors
              read Get_HydrologyGrowthFactorByIndex;
    property  NumberOfYears : Integer read Get_NumberOfYears write Set_NumberOfYears;

    property  DemandGrowthFactorsByChannel[AChannelNumber: Integer]: IDemandCentreGrowthFactors
              read Get_DemandGrowthFactorsByChannel;
    property  MinMaxChannelGrowthFactorsByMinMaxChannel[AMinMaxChannel: Integer]: IMinMaxChannelGrowthFactors
              read Get_MinMaxChannelGrowthFactorsByMinMaxChannel;
    property HydrologyGrowthFactorsByGaugeNumber[AGaugeNumber: Integer]: IHydrologyGrowthFactors
              read Get_HydrologyGrowthFactorsByGaugeNumber;

    property DemandChannelGrowthFactorsByIndex[AIndex: integer]          : TDemandCentreGrowthFactors  read GetDemandChannelGrowthFactorsByIndex;
    property MinMaxChannelGrowthFactorsByIndex[AIndex: integer]          : TMinMaxChannelGrowthFactors read GetMinMaxChannelGrowthFactorsByIndex;
    property HydrologyGrowthFactorsByIndex[AIndex: integer]              : THydrologyGrowthFactors     read GetHydrologyGrowthFactorsByIndex;

    property DemandChannelGrowthFactorsByIdentifier[AIdentifier: integer]: TDemandCentreGrowthFactors  read GetDemandChannelGrowthFactorsByIdentifier;
    property MinMaxChannelGrowthFactorsByIdentifier[AIdentifier: integer]: TMinMaxChannelGrowthFactors read GetMinMaxChannelGrowthFactorsByIdentifier;
    property HydrologyGrowthFactorsByIdentifier[AIdentifier: integer]    : THydrologyGrowthFactors     read GetHydrologyGrowthFactorsByIdentifier;

    property GrowthType : TGrowthType read GetGrowthType write SetGrowthType;

end;

procedure AddErrors (var AErrors : WideString; AErrorList  : TStringList; AErrorCols  : TStringList);
function SetStringListValues(AValue: double; AEOfValues: string; AContainer: TStringList):boolean;

implementation
uses
  System.Types,
  System.UITypes,
  DB,
  Math,
  VCL.Controls,
  VCL.Dialogs,
  UUtilities,
  UConstants,
  UFileNames,
  UGrowthFactorsExcelData,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

procedure AddErrors (var AErrors : WideString;AErrorList  : TStringList;AErrorCols  : TStringList);
const OPNAME = 'UGrowthFactorData.AddErrors';
begin
  try
    if (AErrorCols.Count = 0) then
      AErrors := AErrors + AErrorList.Text
    else
      AErrors := AErrors + CTStringsSeparator + AErrorList.Text +
                           CTStringsSeparator + AErrorCols.Text + CTStringsSeparator;
    AErrorList.Clear;
    AErrorCols.Clear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function SetStringListValues(AValue: double; AEOfValues: string; AContainer: TStringList):boolean;
const OPNAME = 'UGrowthFactorData.SetStringListValues';
var
  LCount : integer;
  LTempStr: string;
begin
  Result := False;
  try
    if Assigned(AContainer) then
    begin
      AEOfValues := Copy(AEOfValues,1,Pos('*',AEOfValues)-1);
      LCount := StrToIntDef(AEOfValues,1);
      LTempStr := Trim(Format('%4.4f', [AValue]));
      if (LCount > 0) and (AValue <> 0) then
      begin
        AContainer.Strings[AContainer.Count -1] := LTempStr;
        AContainer.Add(IntToStr(LCount-1)+'*0');
      end
      else
      if (AValue = 0) then
        AContainer.Strings[AContainer.Count -1] := (IntToStr(LCount+1)+'*0');
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TDemandCentreGrowthFactors }

procedure TDemandCentreGrowthFactors.CreateMemberObjects;
const OPNAME = 'TDemandCentreGrowthFactors.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandCentreGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TDemandCentreGrowthFactors.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandCentreGrowthFactors.Get_ChannelNumber: integer;
const OPNAME = 'TDemandCentreGrowthFactors.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandCentreGrowthFactors.Get_GrowthFactors: WideString;
const OPNAME = 'TDemandCentreGrowthFactors.Get_GrowthFactors';
begin
  Result := '';
  try
    Result := UnCompressCommatextString(FGrowthFactor);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandCentreGrowthFactors.Set_ChannelNumber(AValue: integer);
const OPNAME = 'TDemandCentreGrowthFactors.Set_ChannelNumber';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelNumber', IntToStr(AValue), IntToStr(FChannelNumber), LContextData) then
        begin
          LOldValue := IntToStr(FChannelNumber);
          FChannelNumber := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelNumber',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandCentreGrowthFactors.Set_GrowthFactors(const AValue: WideString);
const OPNAME = 'TDemandCentreGrowthFactors.Set_GrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LTempStr,
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FGrowthFactor;

        if AValue <> LOldValue then
        begin
          LTempStr := CompressCommatextString(AValue);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DemandGrowthFactors', LTempStr,FGrowthFactor, LContextData) then
          begin
            FGrowthFactor := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DemandGrowthFactors',LOldValue,LTempStr);
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

function TDemandCentreGrowthFactors.Get_GrowthFactorsCount:integer;
const OPNAME = 'TDemandCentreGrowthFactors.Get_GrowthFactorsCount';
begin
  Result := 0;
  try
    Result :=  StringsItemsCount(FGrowthFactor);;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDemandCentreGrowthFactors.Get_GrowthFactorsValueByIndex (AIndex : integer): Double;
const OPNAME = 'TDemandCentreGrowthFactors.Get_GrowthFactorsValueByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FGrowthFactor);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDemandCentreGrowthFactors.Set_GrowthFactorsValueByIndex(AIndex : integer; AValue : double);
const OPNAME = 'TDemandCentreGrowthFactors.Set_GrowthFactorsValueByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FGrowthFactor;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      if ChangeSizeCommatextString(FNoOfYears,'0.0',LTempStr) then
        GrowthFactors := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDemandCentreGrowthFactors.Get_ValidFactors : Wordbool; safecall;
const OPNAME = 'TDemandCentreGrowthFactors.Get_ValidFactors';
begin
  Result := True;
  try
    Result := FValidFactors;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDemandCentreGrowthFactors.Set_ValidFactors(AValue : Wordbool);safecall;
const OPNAME = 'TDemandCentreGrowthFactors.Set_ValidFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LNewValue,
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if AValue then
          LNewValue := 'Y'
        else
          LNewValue := 'N';
        if FValidFactors then
          LOldValue := 'Y'
        else
          LOldValue := 'N';
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ValidFactors', LNewValue, LOldValue, LContextData) then
        begin
          FValidFactors := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ValidFactors',LOldValue,LNewValue);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDemandCentreGrowthFactors._AddRef: Integer;
const OPNAME = 'TDemandCentreGrowthFactors._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDemandCentreGrowthFactors._Release: Integer;
const OPNAME = 'TDemandCentreGrowthFactors._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDemandCentreGrowthFactors.ValidateChannelNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDemandCentreGrowthFactors.ValidateChannelNumber';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ChannelNumber',
            IntToStr(FChannelNumber), LMessage)) then
      AErrorMessages.Add( 'ERROR:'+IntToStr(FChannelNumber) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDemandCentreGrowthFactors.ValidateFGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
const OPNAME = 'TDemandCentreGrowthFactors.ValidateFGrowthFactor';
var
  LIndex            : integer;
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LFieldProperties: TAbstractFieldProperty;
begin
  Result := False;
  try
    LResult := True;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('DemandGrowthFactors');

    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := LFieldProperties.ArrayLow to FNoOfYears do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                  ('DemandGrowthFactors', FloatToStr(GrowthFactorsValueByIndex[lIndex]),
                   lMessage, lIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;
      if not FValidFactors then
      begin
        LResult := False;
        LMessage := 'Invalid factor value due to the changes in annual demand value, please generate new factors';
        AErrorMessages.Add('ERROR:'+LMessage);
        AErrorColumns.Add(IntToStr(LIndex));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandCentreGrowthFactors.Populate(AIdentifier, AChannelNumber,ANoOfYears : integer; AGrowthFactor: WideString;AValidFactors : boolean): boolean;
const OPNAME = 'TDemandCentreGrowthFactors.Populate';
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    FChannelNumber := AChannelNumber;
    FNoOfYears := ANoOfYears;
    FGrowthFactor  := AGrowthFactor;
    FValidFactors := AValidFactors;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandCentreGrowthFactors.Initialise: boolean;
const OPNAME = 'TDemandCentreGrowthFactors.Initialise';
begin
  Result := False;
  try
    FIdentifier := 0;
    FChannelNumber := 0;
    FGrowthFactor := '';
    FValidFactors := False;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandCentreGrowthFactors.Validate(var AErrors: WideString;const AContext : WideString): WordBool;
const OPNAME = 'TDemandCentreGrowthFactors.Validate';
var
  LErrorList : TStringList;
  LErrorCols : TStringList;
begin
  Result := False;
  try
    LErrorList := TStringList.Create;
    LErrorCols := TStringList.Create;
    try
      Result := True;
      if AContext = 'ChannelNumber' then
        Result := ValidateChannelNumber(LErrorList)
      else
      if (AContext = 'DemandGrowthFactors') then
      begin
        Result := ValidateFGrowthFactor(LErrorList,LErrorCols);;
        if (not Result) then
          AddErrors(AErrors, LErrorList, LErrorCols);
      end;
      AErrors := AErrors + LErrorList.Text;
    finally
      FreeAndNil(LErrorList);
      FreeAndNil(LErrorCols);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandCentreGrowthFactors.GenerateDemandGrowthProjections(ABaseValue: double; ABaseYear,AYearsCount,ADataStartYear : integer ): string;
const OPNAME = 'TDemandCentreGrowthFactors.GenerateDemandGrowthProjections';
var
  LStartIndex,
  Lindex : integer;
  LProjection,
  LCurrentValue: double;
  LGrowthProjections  : TStringList;

begin
  Result := '';
  try
    LStartIndex := 0;
    LGrowthProjections := TStringList.Create();
    LGrowthProjections.CommaText := GrowthFactors;
    try
      for Lindex := 0 to LGrowthProjections.Count-1 do
      begin
        if(ABaseValue = 0.0) then
          LProjection := 0.00
        else
        if(LStartIndex >= LGrowthProjections.Count-1) then
          LProjection := 0.00
        else
       // if (StrToFloatDef(LGrowthProjections[Lindex],0.0) = 0.0)then
       //   LProjection := 0.00
        //else
        begin
          LCurrentValue := StrToFloatDef(LGrowthProjections[Lindex],0.0);
          LProjection :=  (LCurrentValue + 1)* ABaseValue ;
        end;
        Result  := Result + FormatFloat('#0.00#',LProjection) + ',';
      end;
      if(Length(Result) > 0) then
        Result := Copy(Result,1,Length(Result)-1);
    finally
      FreeAndNil(LGrowthProjections);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TDemandCentreGrowthFactors.GenerateDemandGrowthProjections(ABaseValue: double; ABaseYear,AYearsCount,ADataStartYear : integer ): string;
const OPNAME = 'TDemandCentreGrowthFactors.GenerateDemandGrowthProjections';
var
  LStartIndex,
  Lindex : integer;
  LProjection,
  LCurrentValue: double;
  LGrowthProjections  : TStringList;

begin
  Result := '';
  try
    LStartIndex := 0;
    LGrowthProjections := TStringList.Create();
    LGrowthProjections.CommaText := GrowthFactors;
    try
      if (LStartIndex >= 0) and (LStartIndex < LGrowthProjections.Count) then
      begin
        for Lindex := 1 to AYearsCount do
        begin
          if(ABaseValue = 0.0) then
            LProjection := 0.00
          else
          if(LStartIndex >= LGrowthProjections.Count) then
            LProjection := 0.00
          else
          if ( StrToFloatDef(LGrowthProjections[LStartIndex],0.0) = 0.0)then
            LProjection := 0.00
          else
          begin
            LCurrentValue := StrToFloatDef(LGrowthProjections[LStartIndex],0.0);
            LProjection :=  (LCurrentValue + 1)* ABaseValue ;
          end;
          Result  := Result + FormatFloat('#0.00#',LProjection) + ',';
          LStartIndex := LStartIndex + 1;
        end;
      end;
      if(Length(Result) > 0) then
        Result := Copy(Result,1,Length(Result)-1);
    finally
      FreeAndNil(LGrowthProjections);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}
{ TMinMaxChannelGrowthFactors }

function TMinMaxChannelGrowthFactors._AddRef: Integer;
const OPNAME = 'TMinMaxChannelGrowthFactors._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors._Release: Integer;
const OPNAME = 'TMinMaxChannelGrowthFactors._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxChannelGrowthFactors.CreateMemberObjects;
const OPNAME = 'TMinMaxChannelGrowthFactors.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMinMaxChannelGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TMinMaxChannelGrowthFactors.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.Get_ArcNumber: integer;
const OPNAME = 'TMinMaxChannelGrowthFactors.Get_ArcNumber';
begin
  Result := 0;
  try
    Result := FArcNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.Get_GrowthFactors: WideString;
const OPNAME = 'TMinMaxChannelGrowthFactors.Get_GrowthFactors';
begin
  Result := '';
  try
    Result := UnCompressCommatextString(FGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.Get_MinMaxChannel: integer;
const OPNAME = 'TMinMaxChannelGrowthFactors.Get_MinMaxChannel';
begin
  Result := 0;
  try
    Result := FMinMaxChannel; 
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.Initialise: boolean;
const OPNAME = 'TMinMaxChannelGrowthFactors.Initialise';
begin
  Result := False;
  try
    FMinMaxChannel := 0;
    FArcNumber := 0;
    FGrowthFactors := '';
    FIdentifier := 0;
    FValidFactors := False;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.Get_GrowthFactorsCount : integer;
const OPNAME = 'TMinMaxChannelGrowthFactors.Get_GrowthFactorsCount';
begin
  Result := 0;
  try
    Result :=  StringsItemsCount(FGrowthFactors);;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxChannelGrowthFactors.Set_GrowthFactorsValueByIndex(AIndex : integer; AValue : double);
const OPNAME = 'TMinMaxChannelGrowthFactors.Set_GrowthFactorsValueByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FGrowthFactors;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      GrowthFactors := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxChannelGrowthFactors.Get_GrowthFactorsValueByIndex(AIndex : integer) : double;
const OPNAME = 'TMinMaxChannelGrowthFactors.Get_GrowthFactorsValueByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FGrowthFactors);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxChannelGrowthFactors.Validate(var AErrors: WideString;const AContext : WideString): WordBool;
const OPNAME = 'TMinMaxChannelGrowthFactors.Validate';
var
  LErrorList : TStringList;
  LErrorCols : TStringList;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      Result := True;
      if AContext = 'ArcNumber' then
        Result := ValidateBoundChannel(LErrorList)
      else
      if AContext = 'ChannelNumber' then
        Result := ValidateMinMaxChannel(LErrorList)
      else

      if (AContext = 'MinMaxGrowthFactors') then
      begin
        Result := ValidateFGrowthFactor(LErrorList,LErrorCols);
        if (not Result) then
          AddErrors(AErrors, LErrorList, LErrorCols);
      end;
      AErrors := AErrors + LErrorList.Text;
    finally
      FreeAndNil(LErrorList);
      FreeAndNil(LErrorCols);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.Populate(AIdentifier, AMinMaxChannel,AArcNumber,ANoOfYears: integer; AGrowthFactor: WideString;AValidFactors: boolean): boolean;
const OPNAME = 'TMinMaxChannelGrowthFactors.Populate';
begin
  Result := False;
  try
    FMinMaxChannel := AMinMaxChannel;
    FArcNumber := AArcNumber;
    FGrowthFactors := AGrowthFactor;
    FIdentifier := AIdentifier;
    FNoOfYears := ANoOfYears;
    FValidFactors := AValidFactors;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMinMaxChannelGrowthFactors.Set_ArcNumber(AValue: integer);
const OPNAME = 'TMinMaxChannelGrowthFactors.Set_ArcNumber';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ArcNumber', IntToStr(AValue), IntToStr(FArcNumber), LContextData) then
        begin
          LOldValue := IntToStr(FArcNumber);
          FArcNumber := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ArcNumber',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMinMaxChannelGrowthFactors.Set_GrowthFactors(const AValue: WideString);
const OPNAME = 'TMinMaxChannelGrowthFactors.Set_GrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LTempStr,  
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FGrowthFactors;
        if AValue <> LOldValue then
        begin
          LTempStr := CompressCommatextString(AValue);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'MinMaxGrowthFactors', LTempStr,FGrowthFactors, LContextData) then
          begin
            FGrowthFactors := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinMaxGrowthFactors',LOldValue,LTempStr);
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

procedure TMinMaxChannelGrowthFactors.Set_MinMaxChannel(AValue: integer);
const OPNAME = 'TMinMaxChannelGrowthFactors.Set_MinMaxChannel';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelNumber', IntToStr(AValue), IntToStr(FMinMaxChannel), LContextData) then
        begin
          LOldValue := IntToStr(FMinMaxChannel);
          FMinMaxChannel := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelNumber',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.ValidateBoundChannel(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinMaxChannelGrowthFactors.ValidateBoundChannel';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ArcNumber',
            IntToStr(FArcNumber), LMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FArcNumber) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxChannelGrowthFactors.ValidateFGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
const OPNAME = 'TMinMaxChannelGrowthFactors.ValidateFGrowthFactor';
var
  LIndex            : integer;
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LFieldProperties: TAbstractFieldProperty;
begin
  Result := False;
  try
    lResult := True;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('MinMaxGrowthFactors');

    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := LFieldProperties.ArrayLow to GrowthFactorsCount-1 do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                  ('MinMaxGrowthFactors', FloatToStr(GrowthFactorsValueByIndex[lIndex]),
                   lMessage, lIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;

      if not FValidFactors then
      begin
        LResult := False;
        LMessage := 'Invalid factor value due to the changes in annual demand value, please generate new factors';
        AErrorMessages.Add('ERROR:'+LMessage);
        AErrorColumns.Add(IntToStr(LIndex));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxChannelGrowthFactors.ValidateMinMaxChannel(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinMaxChannelGrowthFactors.ValidateMinMaxChannel';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ChannelNumber',
            IntToStr(FMinMaxChannel), LMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FMinMaxChannel) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxChannelGrowthFactors.GenerateMinMaxChannelGrowthProjections(ABaseValue: double; ABaseYear,AYearsCount: integer ): string;
const OPNAME = 'TMinMaxChannelGrowthFactors.GenerateMinMaxChannelGrowthProjections';
var
  LStartIndex,
  Lindex              : integer;
  LProjection,
  LCurrentValue       : double;
  LGrowthProjections  : TStringList;
begin
  Result := '';
  try
    LStartIndex                  := 0;
    LGrowthProjections           := TStringList.Create;
    LGrowthProjections.CommaText := GrowthFactors;
    try
      for Lindex := 0 to LGrowthProjections.Count-1 do
      begin
        if(ABaseValue = 0.0) then
          LProjection := 0.00
        else
        if((LStartIndex) >= LGrowthProjections.Count-1) then
          LProjection := 0.00
        else
        if(StrToFloatDef(LGrowthProjections[Lindex],0.0) = 0.0) then
          LProjection := 0.00
        else
        begin
          LCurrentValue := StrToFloatDef(LGrowthProjections[Lindex],0.0);
          LProjection :=  (LCurrentValue + 1)* ABaseValue ;
        end;
        Result  := Result +  FormatFloat('#0.00#',(LProjection) )+ ',';
        LStartIndex := LStartIndex + 1;
      end;
      if(Length(Result) > 0) then
        Result := Copy(Result,1,Length(Result)-1);
    finally
      FreeAndNil(LGrowthProjections);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

(*
function TMinMaxChannelGrowthFactors.GenerateMinMaxChannelGrowthProjections(ABaseValue: double; ABaseYear,AYearsCount: integer ): string;
const OPNAME = 'TMinMaxChannelGrowthFactors.GenerateMinMaxChannelGrowthProjections';
var
  LStartIndex,
  Lindex              : integer;
  LProjection,
  LCurrentValue       : double;
  LGrowthProjections  : TStringList;
begin
  Result := '';
  try
    LStartIndex                  := 0;
    LGrowthProjections           := TStringList.Create;
    LGrowthProjections.CommaText := GrowthFactors;
    try
      if (LStartIndex >= 0) and (LStartIndex < LGrowthProjections.Count) then
      begin
        for Lindex := 1 to AYearsCount do
        begin
          if(ABaseValue = 0.0) then
            LProjection := 0.00
          else
          if((LStartIndex) >= LGrowthProjections.Count) then
            LProjection := 0.00
          else
          if(StrToFloatDef(LGrowthProjections[LStartIndex],0.0) = 0.0) then
            LProjection := 0.00
          else
          begin
            LCurrentValue := StrToFloatDef(LGrowthProjections[LStartIndex ],0.0);
            LProjection :=  (LCurrentValue + 1)* ABaseValue ;
          end;
          Result  := Result +  FormatFloat('#0.00#',(LProjection) )+ ',';
          LStartIndex := LStartIndex + 1;
        end;
      end;
      if(Length(Result) > 0) then
        Result := Copy(Result,1,Length(Result)-1);
    finally
      FreeAndNil(LGrowthProjections);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
*)

function TMinMaxChannelGrowthFactors.Get_ValidFactors: Wordbool;
const OPNAME = 'TMinMaxChannelGrowthFactors.Get_ValidFactors';
begin
  Result := True;
  try
    Result := FValidFactors;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxChannelGrowthFactors.Set_ValidFactors(AValue: Wordbool);
const OPNAME = 'TMinMaxChannelGrowthFactors.Set_ValidFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LNewValue,
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if AValue then
          LNewValue := 'Y'
        else
          LNewValue := 'N';
        if FValidFactors then
          LOldValue := 'Y'
        else
          LOldValue := 'N';
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinMaxValidFactors', LNewValue, LOldValue, LContextData) then
        begin
          FValidFactors := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinMaxValidFactors',LOldValue,LNewValue);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ THydrologyGrowthFactors }

function THydrologyGrowthFactors._AddRef: Integer;
const OPNAME = 'THydrologyGrowthFactors._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors._Release: Integer;
const OPNAME = 'THydrologyGrowthFactors._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGrowthFactors.CreateMemberObjects;
const OPNAME = 'THydrologyGrowthFactors.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THydrologyGrowthFactors.DestroyMemberObjects;
const OPNAME = 'THydrologyGrowthFactors.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.Get_AFFGrowthFactors: WideString;
const OPNAME = 'THydrologyGrowthFactors.Get_AFFGrowthFactors';
begin
  Result := '';
  try
    Result := UnCompressCommatextString(FAFFGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function THydrologyGrowthFactors.Get_GaugeNumber: integer;
const OPNAME = 'THydrologyGrowthFactors.Get_GaugeNumber';
begin
  Result := 0;
  try
    Result := FGaugeNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.Get_IRRGrowthFactors: WideString;
const OPNAME = 'THydrologyGrowthFactors.Get_IRRGrowthFactors';
begin
  Result := '';
  try
    Result := UnCompressCommatextString(FIRRGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.Get_URBGrowthFactors: WideString;
const OPNAME = 'THydrologyGrowthFactors.Get_URBGrowthFactors';
begin
  Result := '';
  try
    Result := UnCompressCommatextString(FURBGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.Initialise: boolean;
const OPNAME = 'THydrologyGrowthFactors.Initialise';
begin
  Result := False;
  try
    FGaugeNumber := 0;
    FAFFGrowthFactors := '';
    FIRRGrowthFactors := '';
    FURBGrowthFactors := '';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.Populate(AIdentifier,AGaugeNumber, ANoOfYears: integer;
                                          AAFFGrowthFactors, AIRRGrowthFactors,
                                          AURBGrowthFactors: WideString): boolean;
const OPNAME = 'THydrologyGrowthFactors.Populate';
begin
  Result := False;
  try
    FIdentifier       := AIdentifier;
    FGaugeNumber      := AGaugeNumber;
    FNoOfYears        := ANoOfYears;
    FAFFGrowthFactors := AAFFGrowthFactors;
    FIRRGrowthFactors := AIRRGrowthFactors;
    FURBGrowthFactors := AURBGrowthFactors;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.Validate(var AErrors: WideString;const AContext : WideString): WordBool;
const OPNAME = 'THydrologyGrowthFactors.Validate';
var
  LErrorList : TStringList;
  LErrorCols : TStringList;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      Result := True;
      if AContext = 'GaugeNumber' then
        Result := ValidateGaugeNumber(LErrorList)
      else
      if (AContext = 'AFFGrowthFactors') then
      begin
        Result := ValidateAFFGrowthFactor(LErrorList,lErrorCols);
        if (not Result) then
          AddErrors(AErrors, LErrorList, LErrorCols);
      end;
      if (AContext = 'IRRGrowthFactors') then
      begin
        Result := ValidateIRRGrowthFactor(LErrorList,lErrorCols);
        if (not Result) then
          AddErrors(AErrors, LErrorList, LErrorCols);
      end;
      if (AContext = 'URBGrowthFactors') then
      begin
        Result := ValidateURBGrowthFactor(LErrorList,lErrorCols);
        if (not Result) then
          AddErrors(AErrors, LErrorList, LErrorCols);
      end;

      AErrors := AErrors + LErrorList.Text;
    finally
      FreeAndNil(LErrorList);
      FreeAndNil(LErrorCols);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THydrologyGrowthFactors.Set_AFFGrowthFactorsValueByIndex(AIndex : integer; AValue : double);
const OPNAME = 'THydrologyGrowthFactors.Set_AFFGrowthFactorsValueByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FAFFGrowthFactors;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      AFFGrowthFactors := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGrowthFactors.Get_AFFGrowthFactorsValueByIndex(AIndex : integer) : double;
const OPNAME = 'THydrologyGrowthFactors.Get_AFFGrowthFactorsValueByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FAFFGrowthFactors);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGrowthFactors.Set_IRRGrowthFactorsValueByIndex(AIndex : integer; AValue : double);
const OPNAME = 'THydrologyGrowthFactors.Set_IRRGrowthFactorsValueByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FIRRGrowthFactors;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      IRRGrowthFactors := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGrowthFactors.Get_IRRGrowthFactorsValueByIndex(AIndex : integer) : double;
const OPNAME = 'THydrologyGrowthFactors.Get_IRRGrowthFactorsValueByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FIRRGrowthFactors);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGrowthFactors.Set_URBGrowthFactorsValueByIndex(AIndex : integer; AValue : double);
const OPNAME = 'THydrologyGrowthFactors.Set_URBGrowthFactorsValueByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FURBGrowthFactors;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      URBGrowthFactors := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGrowthFactors.Get_URBGrowthFactorsValueByIndex(AIndex : integer) : double;
const OPNAME = 'THydrologyGrowthFactors.Get_URBGrowthFactorsValueByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FURBGrowthFactors);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGrowthFactors.Set_AFFGrowthFactors(const AValue: WideString);
const OPNAME = 'THydrologyGrowthFactors.Set_AFFGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LTempStr, 
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FAFFGrowthFactors;
        if AValue <> LOldValue then
        begin
          LTempStr := CompressCommatextString(AValue);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'AFFGrowthFactors', LTempStr,FAFFGrowthFactors, LContextData) then
          begin
            FAFFGrowthFactors := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'AFFGrowthFactors',LOldValue,LTempStr);
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

procedure THydrologyGrowthFactors.Set_IRRGrowthFactors(const AValue: WideString);
const OPNAME = 'THydrologyGrowthFactors.Set_IRRGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LTempStr,
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FIRRGrowthFactors;
        if AValue <> LOldValue then
        begin
          LTempStr := CompressCommatextString(AValue);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'IRRGrowthFactors', LTempStr,FIRRGrowthFactors, LContextData) then
          begin
            FIRRGrowthFactors := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'IRRGrowthFactors',LOldValue,LTempStr);
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


procedure THydrologyGrowthFactors.Set_URBGrowthFactors(const AValue: WideString);
const OPNAME = 'THydrologyGrowthFactors.Set_URBGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LTempStr,
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FURBGrowthFactors;
        if AValue <> LOldValue then
        begin
          LTempStr := CompressCommatextString(AValue);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'URBGrowthFactors', LTempStr,FURBGrowthFactors, LContextData) then
          begin
            FURBGrowthFactors := LTempStr;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'URBGrowthFactors',LOldValue,LTempStr);
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


procedure THydrologyGrowthFactors.Set_GaugeNumber(AValue: integer);
const OPNAME = 'THydrologyGrowthFactors.Set_GaugeNumber';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'GaugeNumber', IntToStr(AValue), IntToStr(FGaugeNumber), LContextData) then
        begin
          LOldValue := IntToStr(FGaugeNumber);
          FGaugeNumber := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GaugeNumber',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.ValidateAFFGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
const OPNAME = 'THydrologyGrowthFactors.ValidateAFFGrowthFactor';
var
  LIndex            : integer;
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LFieldProperties: TAbstractFieldProperty;
begin
  Result := False;
  try
    lResult := True;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('AFFGrowthFactors');

    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                  ('AFFGrowthFactors', FloatToStr(AFFGrowthFactorsValueByIndex[lIndex]),
                   lMessage, lIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.ValidateIRRGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
const OPNAME = 'THydrologyGrowthFactors.ValidateIRRGrowthFactor';
var
  LIndex            : integer;
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LFieldProperties: TAbstractFieldProperty;
begin
  Result := False;
  try
    lResult := True;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('IRRGrowthFactors');

    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                  ('IRRGrowthFactors', FloatToStr(IRRGrowthFactorsValueByIndex[lIndex]),
                   lMessage, lIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.ValidateURBGrowthFactor(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
const OPNAME = 'THydrologyGrowthFactors.ValidateURBGrowthFactor';
var
  LIndex            : integer;
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LFieldProperties: TAbstractFieldProperty;
begin
  Result := False;
  try
    lResult := True;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('URBGrowthFactors');

    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                  ('URBGrowthFactors', FloatToStr(URBGrowthFactorsValueByIndex[lIndex]),
                   lMessage, lIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function THydrologyGrowthFactors.ValidateGaugeNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'THydrologyGrowthFactors.ValidateGaugeNumber';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('GaugeNumber',
            IntToStr(FGaugeNumber), LMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FGaugeNumber) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGrowthFactors.GenerateHydrologyGrowthProjections(AAFFBaseYearDemand,AIRRBaseYearDemand,AURBBaseYearDemand :double;
                                                          AYearsCount: integer; var AAFFProjections,AIRRProjections,AURBProjections: string): boolean;
const OPNAME = 'THydrologyGrowthFactors.GenerateHydrologyGrowthProjections';
var
  LStartIndex,
  LPreIndex,
  Lindex              : integer;
  LProjection,
  LCurrentValue,
  LAFFPreValue,
  LIRRPreValue,
  LURBPreValue        : double;
  LAFFFactors,
  LIRRFactors,
  LURBFactors         : TStringList;

begin
  Result      := False;
  LStartIndex := 1;
  try
    AAFFProjections := '';
    AIRRProjections := '';
    AURBProjections := '';

    LAFFFactors := TStringList.Create;
    LIRRFactors := TStringList.Create;
    LURBFactors := TStringList.Create;

    LAFFFactors.CommaText  :=   AFFGrowthFactors;
    LIRRFactors.CommaText  :=   IRRGrowthFactors;
    LURBFactors.CommaText  :=   URBGrowthFactors;
    LAFFPreValue           := 0;
    LIRRPreValue           := 0;
    LURBPreValue           := 0;
    try
      for Lindex := 0 to AYearsCount do
      begin
        //AFF

        if(LStartIndex >= LAFFFactors.Count) then
          LProjection := 0.00
        else
        begin
          if (LStartIndex < 0)then
             LStartIndex := 1;
          LPreIndex := LStartIndex-2;
          if LPreIndex < 0 then
            LAFFPreValue := AAFFBaseYearDemand;
          LCurrentValue := StrToFloatDef(LAFFFactors[LStartIndex-1],0.0);
          if (LAFFPreValue = 0) or (LCurrentValue = 0) then
            LProjection := 0.00
          else
            LProjection  := LAFFPreValue*(LCurrentValue + 1);
          LAFFPreValue := LProjection;
        end;
        AAFFProjections          := AAFFProjections + FormatFloat('#0.00#',LProjection) + ',';
        //IRR
        if(LStartIndex >= LIRRFactors.Count) then
          LProjection := 0.00
        else
        begin
          if (LStartIndex < 0)then
             LStartIndex := 1;
          LPreIndex := LStartIndex-2;
          if LPreIndex < 0 then
            LIRRPreValue := AIRRBaseYearDemand;
          LCurrentValue := StrToFloatDef(LIRRFactors[LStartIndex-1],0.0);
          if (LIRRPreValue = 0) or (LCurrentValue = 0) then
            LProjection := 0.00
          else
            LProjection  := LIRRPreValue*(LCurrentValue + 1);
          LIRRPreValue := LProjection;
        end;
        AIRRProjections          := AIRRProjections + FormatFloat('#0.00#',LProjection) + ',';
        //URB
        if(LStartIndex >= LURBFactors.Count) then
          LProjection := 0.00
        else
        begin
          if (LStartIndex < 0)then
             LStartIndex := 1;
          LPreIndex := LStartIndex-2;
          if LPreIndex < 0 then
            LURBPreValue := AURBBaseYearDemand;
          LCurrentValue := StrToFloatDef(LURBFactors[LStartIndex-1],0.0);
          if (LURBPreValue = 0) or (LCurrentValue = 0) then
            LProjection := 0.00
          else
            LProjection  := LURBPreValue*(LCurrentValue + 1);
          LURBPreValue := LProjection;
        end;
        AURBProjections          := AURBProjections + FormatFloat('#0.00#',LProjection) + ',';
        LStartIndex := LStartIndex + 1;
      end;
      if(Length(AAFFProjections) > 0) then
        AAFFProjections := Copy(AAFFProjections,1,Length(AAFFProjections)-1);
      if(Length(AIRRProjections) > 0) then
        AIRRProjections := Copy(AIRRProjections,1,Length(AIRRProjections)-1);
      if(Length(AURBProjections) > 0) then
        AURBProjections := Copy(AURBProjections,1,Length(AURBProjections)-1);
      Result := True;
    finally
      FreeAndNil(LAFFFactors);
      FreeAndNil(LIRRFactors);
      FreeAndNil(LURBFactors);
   end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function THydrologyGrowthFactors.GenerateHydrologyGrowthProjections(AAFFBaseYearDemand,AIRRBaseYearDemand,AURBBaseYearDemand :double;
                                                          AYearsCount: integer; var AAFFProjections,AIRRProjections,AURBProjections: string): boolean;
const OPNAME = 'THydrologyGrowthFactors.GenerateHydrologyGrowthProjections';
var
  LStartIndex,
  Lindex              : integer;
  LProjection,
  LCurrentValue,
  LBaseValue          : double;
  LAFFFactors,
  LIRRFactors,
  LURBFactors         : TStringList;

begin
  Result      := False;
  LStartIndex := 0;
  try
    AAFFProjections := '';
    AIRRProjections := '';
    AURBProjections := '';
    LAFFFactors := TStringList.Create;
    LIRRFactors := TStringList.Create;
    LURBFactors := TStringList.Create;

    LAFFFactors.CommaText  :=   AFFGrowthFactors;
    LIRRFactors.CommaText  :=   IRRGrowthFactors;
    LURBFactors.CommaText  :=   URBGrowthFactors;

    try
      for Lindex := 1 to AYearsCount do
      begin
        //AFF
        if( LStartIndex >= LAFFFactors.Count ) then
          LProjection := 0.00
        else
          begin
          if (LStartIndex < 0)then
             LStartIndex := 0;
          LBaseValue := AAFFBaseYearDemand;
          LCurrentValue := StrToFloatDef(LAFFFactors[LStartIndex],0.0);
          if(LBaseValue = 0.00) then
            LProjection := 0.00
          else
           LProjection := (LCurrentValue + 1 )* LBaseValue ;
        end;
        AAFFProjections          := AAFFProjections + FormatFloat('#0.00#',LProjection) + ',';
        if (LStartIndex <   (LAFFFactors.Count -1)) then
          LAFFFactors[LStartIndex] := FloatToStr(LProjection);

        //IRR
        if(LStartIndex >= LIRRFactors.Count) then
          LProjection := 0.00
        else
          begin
          if (LStartIndex < 0)then
             LStartIndex := 0;
          LBaseValue := AIRRBaseYearDemand;
          LCurrentValue := StrToFloatDef(LIRRFactors[LStartIndex],0.0);
          if(LBaseValue = 0.00) then
            LProjection := 0.00
          else
           LProjection := (LCurrentValue + 1 )* LBaseValue ;
        end;
        AIRRProjections          := AIRRProjections + FormatFloat('#0.00#',LProjection) + ',';
        if (LStartIndex <   (LIRRFactors.Count -1)) then
          LIRRFactors[LStartIndex] := FloatToStr(LProjection);

        //URB
        if(LStartIndex >= LURBFactors.Count) then
          LProjection := 0.00
        else
          begin
          if (LStartIndex < 0)then
             LStartIndex := 0;
          LBaseValue := AURBBaseYearDemand;
          LCurrentValue := StrToFloatDef(LURBFactors[LStartIndex],0.0);
          if(LBaseValue = 0.00) then
            LProjection := 0.00
          else
             LProjection := (LCurrentValue + 1 )* LBaseValue ;
        end;
        AURBProjections          := AURBProjections + FormatFloat('#0.00#',LProjection) + ',';
        if (LStartIndex <   (LURBFactors.Count -1)) then
          LURBFactors[LStartIndex] := FloatToStr(LProjection);

        LStartIndex := LStartIndex + 1;
      end;

      if(Length(AAFFProjections) > 0) then
        AAFFProjections := Copy(AAFFProjections,1,Length(AAFFProjections)-1);
      if(Length(AIRRProjections) > 0) then
        AIRRProjections := Copy(AIRRProjections,1,Length(AIRRProjections)-1);
      if(Length(AURBProjections) > 0) then
        AURBProjections := Copy(AURBProjections,1,Length(AURBProjections)-1);
      Result := True;
    finally
      FreeAndNil(LAFFFactors);
      FreeAndNil(LIRRFactors);
      FreeAndNil(LURBFactors);
   end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}

{ TGrowthFactors }

function TGrowthFactors._AddRef: Integer;
const OPNAME = 'TGrowthFactors._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors._Release: Integer;
const OPNAME = 'TGrowthFactors._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactors.AddDemandCentresGrowthFactor(AChannelNumber: integer): IDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.AddDemandCentresGrowthFactor';
var
  LDemandCentreGrowthFactors : TDemandCentreGrowthFactors;
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LIdentifier : integer;
begin
  Result := nil;
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if (LSQLAgent.InsertDemandCentreGrowthFactors(LIdentifier,AChannelNumber)) then
      begin
        LDemandCentreGrowthFactors := CreateDemandGrowthFactor;
        LDemandCentreGrowthFactors.FIdentifier := LIdentifier;
        LDemandCentreGrowthFactors.FChannelNumber := AChannelNumber;
        Result := LDemandCentreGrowthFactors;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactors.AddHydrologyGrowthFactor(AGaugeNumber: integer): IHydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.AddHydrologyGrowthFactor';
var
  LHydrologyGrowthFactors : THydrologyGrowthFactors;
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LIdentifier : integer;
begin
  Result := nil;
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if (LSQLAgent.InsertHydrologyGrowthFactors(LIdentifier)) then
      begin
        LHydrologyGrowthFactors := CreateHydrologyGrowthFactor;
        LHydrologyGrowthFactors.FIdentifier := LIdentifier;
        Result := LHydrologyGrowthFactors;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.AddMinMaxChannelGrowthFactor(AChannelNumber: integer): IMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.AddMinMaxChannelGrowthFactor';
var
  LMinMaxChannelGrowthFactors : TMinMaxChannelGrowthFactors;
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LIdentifier : integer;
begin
  Result := nil;
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if (LSQLAgent.InsertMinMaxChannelGrowthFactors(LIdentifier, AChannelNumber)) then
      begin
        LMinMaxChannelGrowthFactors := CreateMinMaxChannelGrowthFactor;
        LMinMaxChannelGrowthFactors.FIdentifier := LIdentifier;
        LMinMaxChannelGrowthFactors.FMinMaxChannel := AChannelNumber; 
        Result := LMinMaxChannelGrowthFactors;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CastDemandGrowthFactorByChannel(AChannelNumber: integer): TDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.CastDemandGrowthFactorByChannel';
var
  LIndex    : integer;
  LDemandCentreGrowthFactors : TDemandCentreGrowthFactors;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (lIndex < FGFDemandCentresObjectContainer.Count)) do
    begin
      LDemandCentreGrowthFactors := TDemandCentreGrowthFactors(FGFDemandCentresObjectContainer.Items[lIndex]);
      if (LDemandCentreGrowthFactors.FchannelNumber = AChannelNumber) then
        Result := LDemandCentreGrowthFactors
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CastDemandGrowthFactorByIndex(AIndex: integer): TDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.CastDemandGrowthFactorByIndex';
begin
  Result := nil;
  try
    Result := TDemandCentreGrowthFactors(FGFDemandCentresObjectContainer.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CastHydrologyGrowthFactorByGaugeNumber(AGaugeNumber: integer): THydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.CastHydrologyGrowthFactorByGaugeNumber';
var
  LIndex    : integer;
  LHydrologyGrowthFactors : THydrologyGrowthFactors;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (lIndex < FGFHydrologyObjectContainer.Count)) do
    begin
      LHydrologyGrowthFactors := THydrologyGrowthFactors(FGFHydrologyObjectContainer.Items[lIndex]);
      if (LHydrologyGrowthFactors.FGaugeNumber = AGaugeNumber) then
        Result := LHydrologyGrowthFactors
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CastHydrologyGrowthFactorByIndex(AIndex: integer): THydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.CastHydrologyGrowthFactorByIndex';
begin
  Result := nil;
  try
    Result := THydrologyGrowthFactors(FGFHydrologyObjectContainer.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CastMinMaxChannelGrowthFactorByIndex(AIndex: integer): TMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.CastMinMaxChannelGrowthFactorByIndex';
begin
  Result := nil;
  try
    Result := TMinMaxChannelGrowthFactors(FGFMinMaxChannelObjectContainer.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CastMinMaxChannelGrowthFactorByMinMaxChannel(AMinMaxChannel: integer): TMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.CastMinMaxChannelGrowthFactorByMinMaxChannel';
var
  LIndex    : integer;
  LMinMaxChannelGrowthFactors : TMinMaxChannelGrowthFactors;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (lIndex < FGFMinMaxChannelObjectContainer.Count)) do
    begin
      LMinMaxChannelGrowthFactors := TMinMaxChannelGrowthFactors(FGFMinMaxChannelObjectContainer.Items[lIndex]);
      if (LMinMaxChannelGrowthFactors.FMinMaxChannel = AMinMaxChannel) then
        Result := LMinMaxChannelGrowthFactors
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CastMinMaxChannelGrowthFactorsByChannelArc(AChannel, AArc: integer): TMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.CastMinMaxChannelGrowthFactorsByChannelArc';
var
  LIndex    : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FGFMinMaxChannelObjectContainer.Count -1 do
    begin
      if (TMinMaxChannelGrowthFactors(FGFMinMaxChannelObjectContainer.Items[LIndex]).FMinMaxChannel = AChannel) and
        (TMinMaxChannelGrowthFactors(FGFMinMaxChannelObjectContainer.Items[LIndex]).FArcNumber = AArc) then
      begin
        Result := TMinMaxChannelGrowthFactors(FGFMinMaxChannelObjectContainer.Items[LIndex]);
        Break;
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.CreateDemandGrowthFactor: TDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.CreateDemandGrowthFactor';
var
  LDemandCentreGrowthFactors : TDemandCentreGrowthFactors;
begin
  Result := nil;
  try
    LDemandCentreGrowthFactors := TDemandCentreGrowthFactors.Create(FAppModules);
    FGFDemandCentresObjectContainer.Add(LDemandCentreGrowthFactors);
    Result := LDemandCentreGrowthFactors;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactors.CreateHydrologyGrowthFactor: THydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.CreateHydrologyGrowthFactor';
var
  LHydrologyGrowthFactors : THydrologyGrowthFactors;
begin
  Result := nil;
  try
    LHydrologyGrowthFactors := THydrologyGrowthFactors.Create(FAppModules);
    FGFHydrologyObjectContainer.Add(LHydrologyGrowthFactors);
    Result := LHydrologyGrowthFactors;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactors.CreateMemberObjects;
const OPNAME = 'TGrowthFactors.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FGFDemandCentresObjectContainer := TObjectList.Create;
    FGFMinMaxChannelObjectContainer := TObjectList.Create;
    FGFHydrologyObjectContainer := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TGrowthFactors.DestroyMemberObjects';
begin
  try
    FreeAndNil(FGFDemandCentresObjectContainer);
    FreeAndNil(FGFMinMaxChannelObjectContainer);
    FreeAndNil(FGFHydrologyObjectContainer);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGrowthFactors.CreateMinMaxChannelGrowthFactor: TMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.CreateMinMaxChannelGrowthFactor';
var
  LMinMaxChannelGrowthFactors : TMinMaxChannelGrowthFactors;
begin
  Result := nil;
  try
    LMinMaxChannelGrowthFactors := TMinMaxChannelGrowthFactors.Create(FAppModules);
    FGFMinMaxChannelObjectContainer.Add(LMinMaxChannelGrowthFactors);
    Result := LMinMaxChannelGrowthFactors;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.DeleteDemandGrowthFactorByChannelNumber(AChannelNumber: integer): WordBool;
const OPNAME = 'TGrowthFactors.DeleteDemandGrowthFactorByChannelNumber';
var
  LIndex: integer;
begin
  Result := False;
  try
    for lIndex := 0 to FGFDemandCentresObjectContainer.Count -1 do
    begin
      if (TDemandCentreGrowthFactors(FGFDemandCentresObjectContainer.Items[lIndex]).ChannelNumber = AChannelNumber) then
      begin
        FGFDemandCentresObjectContainer.Remove(FGFDemandCentresObjectContainer.Items[lIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactors.DeleteHydrologyGrowthFactorByGauge(AGaugeNumber: integer): WordBool;
const OPNAME = 'TGrowthFactors.DeleteHydrologyGrowthFactorByGauge';
var
  LIndex: integer;
begin
  Result := False;
  try
    for lIndex := 0 to FGFHydrologyObjectContainer.Count -1 do
    begin
      if (THydrologyGrowthFactors(FGFHydrologyObjectContainer.Items[lIndex]).GaugeNumber = AGaugeNumber) then
      begin
        FGFHydrologyObjectContainer.Remove(FGFHydrologyObjectContainer.Items[lIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactors.DeleteMinMaxChannelGrowthFactorByChannel(AMinMaxChannel: integer): WordBool;
const OPNAME = 'TGrowthFactors.DeleteMinMaxChannelGrowthFactorByChannel';
var
  lIndex: integer;
  LMinMaxChannelGrowthFactors : TMinMaxChannelGrowthFactors;
begin
  Result := False;
  try
    LIndex := 0;
    while (not(Result) and (lIndex < FGFMinMaxChannelObjectContainer.Count)) do
    begin
      LMinMaxChannelGrowthFactors := CastMinMaxChannelGrowthFactorByMinMaxChannel(AMinMaxChannel);
      if LMinMaxChannelGrowthFactors <> nil then
      begin
        FGFMinMaxChannelObjectContainer.remove(LMinMaxChannelGrowthFactors);
        Result := false;
      end
      else
      begin
        LIndex := LIndex + 1;
        Result := True;
      end;

    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactors.DemandCentresGrowthFactorsCount: integer;
const OPNAME = 'TGrowthFactors.DemandCentresGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGFDemandCentresObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGrowthFactors.Get_DemandGrowthFactorsByChannel(AChannelNumber: integer): IDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.Get_DemandGrowthFactorsByChannel';
begin
  Result := nil;
  try
    Result := CastDemandGrowthFactorByChannel(AChannelNumber);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Get_DemandCentresGrowthByIndex(AIndex: integer): IDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.Get_DemandCentresGrowthByIndex';
begin
  Result := nil;
  try
    Result := CastDemandGrowthFactorByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Get_HydrologyGrowthFactorsByGaugeNumber(AGaugeNumber: integer): IHydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.Get_HydrologyGrowthFactorsByGaugeNumber';
begin
  Result := nil;
  try
    Result := CastHydrologyGrowthFactorByGaugeNumber(AGaugeNumber);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Get_HydrologyGrowthFactorByIndex(AIndex: integer): IHydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.Get_HydrologyGrowthFactorByIndex';
begin
  Result := nil;
  try
    Result := CastHydrologyGrowthFactorByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Get_MinMaxChannelGrowthFactorByIndex(AIndex: integer): IMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.Get_MinMaxChannelGrowthFactorByIndex';
begin
  Result := nil;
  try
    Result := CastMinMaxChannelGrowthFactorByIndex(AIndex); 
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Get_MinMaxChannelGrowthFactorsByMinMaxChannel(AMinMaxChannel: integer): IMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.Get_MinMaxChannelGrowthFactorsByMinMaxChannel';
begin
  Result := nil;
  try
    Result := CastMinMaxChannelGrowthFactorByMinMaxChannel(AMinMaxChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Get_NumberOfYears: integer;
const OPNAME = 'TGrowthFactors.Get_NumberOfYears';
begin
  Result := 0;
  try
    Result := FNumberOfYears;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.HydrologyGrowthFactorsCount: integer;
const OPNAME = 'TGrowthFactors.HydrologyGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGFHydrologyObjectContainer.count;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Initialise: boolean;
const OPNAME = 'TGrowthFactors.Initialise';
begin
  Result := False;
  try
    FNumberOfYears := 0;
    FGFDemandCentresObjectContainer.Clear;
    FGFMinMaxChannelObjectContainer.Clear;
    FGFHydrologyObjectContainer.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.MinMaxChannelGrowthFactorsCount: integer;
const OPNAME = 'TGrowthFactors.MinMaxChannelGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGFMinMaxChannelObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Populate(ANumberOfYears: Integer): boolean;
const OPNAME = 'TGrowthFactors.Populate';
begin
  Result := False;
  try
    FNumberOfYears := ANumberOfYears;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.RemoveDemandCentresGrowthFactor(AChannelNumber: integer) : WordBool;
const OPNAME = 'TGrowthFactors.RemoveDemandCentresGrowthFactor';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LDemandCentreGrowthFactors  : TDemandCentreGrowthFactors;
begin
  Result := False;
  try
    LDemandCentreGrowthFactors := CastDemandGrowthFactorByChannel(AChannelNumber);
    if (LDemandCentreGrowthFactors <> nil) then
    begin
      LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
      try
        if (LSQLAgent.DeleteDemandGrowthFactorByChannel(AChannelNumber)) then
        begin
          DeleteDemandGrowthFactorByChannelNumber(AChannelNumber);
          Result := True;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.RemoveHydrologyGrowthFactor(AGaugeNumber:integer): WordBool;
const OPNAME = 'TGrowthFactors.RemoveHydrologyGrowthFactor';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LHydrologyGrowthFactors  : THydrologyGrowthFactors;
begin
  Result := False;
  try
    LHydrologyGrowthFactors := CastHydrologyGrowthFactorByGaugeNumber(AGaugeNumber);
    if (LHydrologyGrowthFactors <> nil) then
    begin
      LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
      try
        if (LSQLAgent.DeleteHydrologyGrowthFactorsByGauge(AGaugeNumber)) then
        begin
          DeleteHydrologyGrowthFactorByGauge(AGaugeNumber);
          Result := True;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.RemoveMinMaxChannelGrowthFactor(AMinMaxChannel: integer): WordBool;
const OPNAME = 'TGrowthFactors.RemoveMinMaxChannelGrowthFactor';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LMinMaxChannelGrowthFactors  : TMinMaxChannelGrowthFactors;
begin
  Result := False;
  try
    LMinMaxChannelGrowthFactors := CastMinMaxChannelGrowthFactorByMinMaxChannel(AMinMaxChannel);
    if (LMinMaxChannelGrowthFactors <> nil) then
    begin
      LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
      try
        if (LSQLAgent.DeleteMinMaxChannelGrowthFactorByChannel(AMinMaxChannel)) then
        begin
          DeleteMinMaxChannelGrowthFactorByChannel(AMinMaxChannel);
          Result := True;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TGrowthFactors.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
  lErrorCols        : TStringList;
  LIndex            : integer;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (AContext = 'GrowthFactorsYearCount') then
        Result := ValidateNumberOfYears(lErrorList)
      else
      begin
        Result := True;
        if (not ValidateNumberOfYears(lErrorList)) then
          Result := False;
      end;
      for  LIndex := 0 to FGFDemandCentresObjectContainer.Count -1 do
      begin
        if (not CastDemandGrowthFactorByIndex(LIndex).Validate(AErrors,AContext)) then
          Result := False;
        if ((not Result) and LStopOnFirstError) then
          Break;
      end;
      for  LIndex := 0 to FGFMinMaxChannelObjectContainer.Count -1 do
      begin
        if (not CastMinMaxChannelGrowthFactorByIndex(LIndex).Validate(AErrors,AContext)) then
          Result := False;
        if ((not Result) and LStopOnFirstError) then
          Break;
      end;
      for  LIndex := 0 to FGFHydrologyObjectContainer.Count -1 do
      begin
        if (not CastHydrologyGrowthFactorByIndex(LIndex).Validate(AErrors,AContext)) then
          Result := False;
        if ((not Result) and LStopOnFirstError) then
          Break;
      end;
    finally
      lErrorList.Free;
      lErrorCols.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.ValidateNumberOfYears(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TGrowthFactors.ValidateNumberOfYears';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('GrowthFactorsYearCount',
            IntToStr(FNumberOfYears), LMessage)) then
      AErrorMessages.Add('ERROR:'+ IntToStr(FNumberOfYears) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGrowthFactors.Set_NumberOfYears(Value: Integer);
const OPNAME = 'TGrowthFactors.Set_NumberOfYears';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FNumberOfYears <> Value then
        begin
          LSQLAgent.LoadConfigContextData(LContextData);
          if (SetYearsCountToAllObjects(Value)) then
          begin
            if FAppModules.FieldProperties.UpdateFieldValue(
               'GrowthFactorsYearCount', IntToStr(Value), IntToStr(FNumberOfYears), LContextData) then
            begin
              LOldValue := IntToStr(FNumberOfYears);
              FNumberOfYears := Value;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'GrowthFactorsYearCount',LOldValue,IntToStr(Value));
            end;
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

function TGrowthFactors.SetYearsCountToAllObjects(ANumberOfYears : integer) : boolean;
const OPNAME = 'TGrowthFactors.SetYearsCountToAllObjects';
var
  LDemandGrowthFactors : IDemandCentreGrowthFactors;
  LMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;
  LHydrologyGrowthFactors : IHydrologyGrowthFactors;
  LFactorsList : TStringList;
  LIndex : integer;
  LPromptMsg : string;
  LOverwriteDataPrompt : integer;
begin
  Result := False;
  try
    LFactorsList := TStringList.Create;
    try
      LOverwriteDataPrompt := mrNo;;
      for LIndex := 0 to FGFDemandCentresObjectContainer.Count - 1 do
      begin
        LDemandGrowthFactors := TDemandCentreGrowthFactors(FGFDemandCentresObjectContainer.Items[LIndex]);
        if (LDemandGrowthFactors <> nil) then
        begin
          LFactorsList.Clear;
          LFactorsList.CommaText := LDemandGrowthFactors.GrowthFactors;
          if ((LFactorsList.Count-1) <= ANumberOfYears) and ((LFactorsList.Count-1)> 0 ) then
          begin
            LFactorsList.Strings[LFactorsList.Count-1] := IntToStr(ANumberOfYears - (LFactorsList.Count-1))+'*0';
            LDemandGrowthFactors.GrowthFactors := LFactorsList.CommaText;
          end
          else
          if ((LFactorsList.Count-1) > ANumberOfYears)and ((LFactorsList.Count-1)> 0 ) then
          begin
            if (LOverwriteDataPrompt <> mrYes) then
            begin
              LPromptMsg := Format(FAppModules.language.GetString('ContextValidation.NumberOfYears'),[ANumberOfYears]);
              LOverwriteDataPrompt := MessageDlg(LPromptMsg, mtConfirmation, [mbYes, mbNo], 0);
            end;
            if (LOverwriteDataPrompt = mrYes) then
            begin
              while ANumberOfYears < LFactorsList.Count-1 do
                LFactorsList.Delete(LFactorsList.Count-1);
              LDemandGrowthFactors.GrowthFactors := LFactorsList.CommaText;
            end
            else
            if (LOverwriteDataPrompt = mrNo) then
            begin
              Result := False;
              Exit;
            end;
          end;
        end;
      end;

      for LIndex := 0 to FGFMinMaxChannelObjectContainer.Count - 1 do
      begin
        LMinMaxGrowthFactors := TMinMaxChannelGrowthFactors(FGFMinMaxChannelObjectContainer.Items[LIndex]);
        if (LMinMaxGrowthFactors <> nil) then
        begin
          LFactorsList.Clear;
          LFactorsList.CommaText := LMinMaxGrowthFactors.GrowthFactors;
          if ((LFactorsList.Count-1) <= ANumberOfYears) and ((LFactorsList.Count-1)> 0 ) then
          begin
            LFactorsList.Strings[LFactorsList.Count-1] := IntToStr(ANumberOfYears - (LFactorsList.Count-1))+'*0';
            LMinMaxGrowthFactors.GrowthFactors := LFactorsList.CommaText;
          end
          else
          if ((LFactorsList.Count-1) > ANumberOfYears) and ((LFactorsList.Count-1)> 0 ) then
          begin
            if (LOverwriteDataPrompt = mrYes) then
            begin
              while ANumberOfYears < LFactorsList.Count-1 do
                LFactorsList.Delete(LFactorsList.Count-1);
              LMinMaxGrowthFactors.GrowthFactors := LFactorsList.CommaText;
            end
            else
            if (LOverwriteDataPrompt = mrNo) then
            begin
              Result := False;
              Exit;
            end;
          end;  
        end;
      end;

      for LIndex := 0 to FGFHydrologyObjectContainer.Count - 1 do
      begin
        LHydrologyGrowthFactors := THydrologyGrowthFactors(FGFHydrologyObjectContainer.Items[LIndex]);
        if (LHydrologyGrowthFactors <> nil) then
        begin
          LFactorsList.Clear;
          LFactorsList.CommaText := LHydrologyGrowthFactors.AFFGrowthFactors;
          if ((LFactorsList.Count-1) <= ANumberOfYears) and ((LFactorsList.Count-1)> 0 ) then
          begin
            LFactorsList.Strings[LFactorsList.Count-1] := IntToStr(ANumberOfYears - (LFactorsList.Count-1))+'*0';
            LHydrologyGrowthFactors.AFFGrowthFactors := LFactorsList.CommaText;
          end;
          LFactorsList.Clear;
          LFactorsList.CommaText := LHydrologyGrowthFactors.IRRGrowthFactors;
          if ((LFactorsList.Count-1) <= ANumberOfYears) and ((LFactorsList.Count-1)> 0 ) then
          begin
            LFactorsList.Strings[LFactorsList.Count-1] := IntToStr(ANumberOfYears - (LFactorsList.Count-1))+'*0';
            LHydrologyGrowthFactors.IRRGrowthFactors := LFactorsList.CommaText;
          end;
          LFactorsList.Clear;
          LFactorsList.CommaText := LHydrologyGrowthFactors.URBGrowthFactors;
          if ((LFactorsList.Count-1) <= ANumberOfYears) and ((LFactorsList.Count-1)> 0 )then
          begin
            LFactorsList.Strings[LFactorsList.Count-1] := IntToStr(ANumberOfYears - (LFactorsList.Count-1))+'*0';
            LHydrologyGrowthFactors.URBGrowthFactors := LFactorsList.CommaText;
          end;
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LFactorsList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THydrologyGrowthFactors.Get_AFFGrowthFactorsCount: Integer;
const OPNAME = 'THydrologyGrowthFactors.Get_AFFGrowthFactorsCount';
begin
  Result := 0;
  try
    Result :=  StringsItemsCount(FAFFGrowthFactors);;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGrowthFactors.Get_IRRGrowthFactorsCount: Integer;
const OPNAME = 'THydrologyGrowthFactors.Get_IRRGrowthFactorsCount';
begin
  Result := 0;
  try
    Result :=  StringsItemsCount(FIRRGrowthFactors);;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGrowthFactors.Get_URBGrowthFactorsCount: Integer;
const OPNAME = 'THydrologyGrowthFactors.Get_URBGrowthFactorsCount';
begin
  Result := 0;
  try
    Result :=  StringsItemsCount(FURBGrowthFactors);;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGrowthFactors.GetGrowthFactorsValueByIndex(AIndex: integer; AGrowthFactors : TStringList): double;
const OPNAME = 'THydrologyGrowthFactors.GetGrowthFactorsValueByIndex';
var
  LTempStr : string;
  LPos     : integer;
begin
  Result := 0.0;
  try
    if (AIndex >= 0) and (AIndex < AGrowthFactors.Count) then
    begin
      LTempStr := Trim(AGrowthFactors.Strings[AIndex]);
      LPos     := Pos('*', LTempStr);
      if (lPos = 0) then
        Result := StrToFloat(LTempStr)
      else
        Result := 0.0000;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THydrologyGrowthFactors.SetGrowthFactorsValueByIndex( AIndex: integer; AValue: double; AGrowthFactors : TStringList);
const OPNAME = 'THydrologyGrowthFactors.SetGrowthFactorsValueByIndex';
var
  LTempStr  : string;
  LPos      : integer;
  LTempList : TStringList;
begin
  try
    if (AIndex >= 0) and (AIndex < FNoOfYears) then
    begin
      LTempList := TStringList.Create;
      try
        LTempList.CommaText := AGrowthFactors.CommaText;
        if ((LTempList.Count-1) < AIndex) or (AValue = 0) then
          LTempStr := Trim(LTempList.Strings[LTempList.Count -1])
        else
          LTempStr := Trim(LTempList.Strings[AIndex]);
        LPos     := Pos('*', lTempStr);
        if (lPos = 0) and (AValue <> 0) and (AIndex < (LTempList.Count)) then
        begin
          lTempStr := Trim(Format('%4.4f', [AValue]));
          LTempList.Strings[AIndex] := lTempStr;
        end
        else
        if (AValue = 0) and (AIndex < (LTempList.Count-1)) and (AIndex <> (LTempList.Count-1)) then
        begin
          LTempList.Delete(AIndex);
          SetStringListValues(AValue,LTempStr,LTempList);
        end
        else
        if ((LTempList.Count-1) <= AIndex) and (AValue <> 0) then
          SetStringListValues(AValue,LTempStr,LTempList);
        AGrowthFactors.CommaText := lTempList.CommaText;
      finally
        FreeAndNil(lTempList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactors.ClearAllDataFromDB: boolean;
const OPNAME = 'TGrowthFactors.ClearAllDataFromDB';
begin
  Result := False;
  try
    FNumberOfYears := 0;
    FGFDemandCentresObjectContainer.Clear;
    FGFMinMaxChannelObjectContainer.Clear;
    FGFHydrologyObjectContainer.Clear;

    ClearDemandGrowthFactorsDB;
    ClearMinMaxChannelGrowthFactorsDB;
    ClearHydrologyGrowthFactorsDB;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.ClearDemandGrowthFactorsDB: boolean;
const OPNAME = 'TGrowthFactors.ClearDemandGrowthFactorsDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.DeleteAllGrowthFactorDemand;
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGrowthFactors.ClearHydrologyGrowthFactorsDB: boolean;
const OPNAME = 'TGrowthFactors.ClearHydrologyGrowthFactorsDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.DeleteAllGrowthFactorHydrology;
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.ClearMinMaxChannelGrowthFactorsDB: boolean;
const OPNAME = 'TGrowthFactors.ClearMinMaxChannelGrowthFactorsDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.DeleteAllGrowthFactorMinMax;
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.AddDemandGrowthFactorsToDB: boolean;
const OPNAME = 'TGrowthFactors.AddDemandGrowthFactorsToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddDemandGrowthFactorsToDB(FGFDemandCentresObjectContainer);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.AddMinMaxChannelGrowthFactorsToDB: boolean;
const OPNAME = 'TGrowthFactors.AddMinMaxChannelGrowthFactorsToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddMinMaxChannelGrowthFactorsToDB(FGFMinMaxChannelObjectContainer);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.AddHydrologyGrowthFactorsToDB: boolean;
const OPNAME = 'TGrowthFactors.AddHydrologyGrowthFactorsToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddHydrologyGrowthFactorsToDB(FGFHydrologyObjectContainer);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;    

function TGrowthFactors.SaveAllDataToDB: boolean;
const OPNAME = 'TGrowthFactors.SaveAllDataToDB';
begin
  Result := False;
  try
    AddDemandGrowthFactorsToDB;
    AddMinMaxChannelGrowthFactorsToDB;
    AddHydrologyGrowthFactorsToDB;
    AddGrowthFactorsConfigDataToDB(FNumberOfYears);
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGrowthFactors.ClearDemandChannelGrowthFactors: boolean;
const OPNAME = 'TGrowthFactors.ClearDemandChannelGrowthFactors';
begin
  Result := False;
  try
    FGFDemandCentresObjectContainer.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGrowthFactors.Populated: boolean;
const OPNAME = 'TGrowthFactors.Populated';
begin
  Result := False;
  try
    Result := (FGFDemandCentresObjectContainer.Count > 0) or
              (FGFMinMaxChannelObjectContainer.Count > 0) or (FGFHydrologyObjectContainer.Count > 0);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
function TGrowthFactors.GenerateGrowthProjections: boolean;
const OPNAME = 'TGrowthFactors.GenerateGrowthProjections';
 var
  LStartYear,
  LBaseYear,
  LDataStartYear,
  LIndex                          : integer;
  LAFFFactors,
  LIRRFactors,
  LURBFactors,
  LProjection                     : string;
  LAFFBaseYearDemand,
  LIRRBaseYearDemand,
  LURBBaseYearDemand              : double;
  LGrowthProjections              : TExelGrowthFactors;
  LDemandCentreGrowthFactors      : TDemandCentreGrowthFactors;
  LMinMaxChannelGrowthFactors     : TMinMaxChannelGrowthFactors;
  LHydrologyGrowthFactors         : THydrologyGrowthFactors;
  LExelDemandChannelProjections   : TExelDemandChannelGrowthFactors;
  LExelMinMaxChannelProjections   : TExelMinMaxChannelGrowthFactors;
  LExelHydrologyProjections       : TExelHydrologyGrowthFactors;
  LRunConfig                      : IRunConfigurationData;

begin
  Result               := False;
  LBaseYear            := NullInteger;
  LStartYear           := NullInteger;
  LDataStartYear       := NullInteger;
  LAFFBaseYearDemand   := NullInteger;
  LIRRBaseYearDemand   := NullInteger;
  LURBBaseYearDemand   := NullInteger;

  try
    LGrowthProjections  := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    LRunConfig          := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;

    Result              := LGrowthProjections.ClearAllProjectionDataFromDB;

    if (LRunConfig <> nil) and (LGrowthProjections <> nil) then
    begin
      LStartYear     := LRunConfig.HistoricSequenceStartYear;
      LBaseYear      := LStartYear;
      LDataStartYear := LBaseYear;
    end;

    for LIndex := 0 to FGFDemandCentresObjectContainer.Count -1 do
    begin
      LDemandCentreGrowthFactors                  := DemandChannelGrowthFactorsByIndex[LIndex];
      LExelDemandChannelProjections               := LGrowthProjections.CreateDemandGrowthProjection;
      LExelDemandChannelProjections.ChannelNumber := LDemandCentreGrowthFactors.ChannelNumber;

      LProjection := LDemandCentreGrowthFactors.GenerateDemandGrowthProjections( LExelDemandChannelProjections.BaseYearDemand,LBaseYear,FNumberOfYears,LDataStartYear);
      LExelDemandChannelProjections.Populate(LIndex+1,LDemandCentreGrowthFactors.ChannelNumber,FNumberOfYears,LProjection);
    end;

    for LIndex := 0 to FGFMinMaxChannelObjectContainer.Count -1 do
    begin
      LMinMaxChannelGrowthFactors                 := MinMaxChannelGrowthFactorsByIndex[LIndex];
      LExelMinMaxChannelProjections               := LGrowthProjections.CreateMinMaxChannelGrowthProjection;
      LExelMinMaxChannelProjections.ChannelNumber := LMinMaxChannelGrowthFactors.MinMaxChannel;
      LExelMinMaxChannelProjections.ArcNumber     := LMinMaxChannelGrowthFactors.FArcNumber;

      LProjection := LMinMaxChannelGrowthFactors.GenerateMinMaxChannelGrowthProjections(LExelMinMaxChannelProjections.BaseYearDemand,LBaseYear,FNumberOfYears);
      LExelMinMaxChannelProjections.Populate(LIndex+1,LMinMaxChannelGrowthFactors.MinMaxChannel,FNumberOfYears,LProjection);
    end;


    for LIndex := 0 to FGFHydrologyObjectContainer.Count -1 do
    begin
      LHydrologyGrowthFactors               := HydrologyGrowthFactorsByIndex[LIndex];
      LExelHydrologyProjections             := LGrowthProjections.CreateHydrologyGrowthProjection;
      LExelHydrologyProjections.GaugeNumber := LHydrologyGrowthFactors.FGaugeNumber;
      LExelHydrologyProjections.Identifier  := LHydrologyGrowthFactors.FIdentifier;
      if (LExelHydrologyProjections.BaseYearDemand )then
      begin
        LAFFBaseYearDemand :=  LExelHydrologyProjections.AFFBaseYearDemand;
        LIRRBaseYearDemand :=  LExelHydrologyProjections.IRRBaseYearDemand;
        LURBBaseYearDemand :=  LExelHydrologyProjections.URBBaseYearDemand;
      end;

      if LHydrologyGrowthFactors.GenerateHydrologyGrowthProjections(LAFFBaseYearDemand, LIRRBaseYearDemand, LURBBaseYearDemand,
                                                                   FNumberOfYears,LAFFFactors,LIRRFactors,LURBFactors) then
      begin
        LExelHydrologyProjections.Populate(LIndex+1,LHydrologyGrowthFactors.FGaugeNumber,FNumberOfYears,LAFFFactors,LIRRFactors,LURBFactors);
      end;
    end;

    LGrowthProjections.Populate(LBaseYear, LGrowthProjections.BaseYearIndex,LStartYear, LGrowthProjections.StartYearIndex,FNumberOfYears,LDataStartYear);
    Result := Result and LGrowthProjections.SaveAllProjectionDataToDB;
    if Result then
     FAppModules.Model.StudyDataHasChanged(sdccAdd,'GrowthFactors','','');
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TGrowthFactors.GetDemandChannelGrowthFactorsByIdentifier(AIdentifier: integer): TDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.GetDemandChannelGrowthFactorsByIdentifier';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FGFDemandCentresObjectContainer.Count -1 do
    begin
      Result := DemandChannelGrowthFactorsByIndex[LIndex];
      if (Result.FIdentifier = AIdentifier) then Exit;
    end;
    Result := nil;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.GetDemandChannelGrowthFactorsByIndex(AIndex: integer): TDemandCentreGrowthFactors;
const OPNAME = 'TGrowthFactors.GetDemandChannelGrowthFactorsByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FGFDemandCentresObjectContainer.Count) then
    begin
      Result :=TDemandCentreGrowthFactors(FGFDemandCentresObjectContainer.Items[AIndex]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.GetHydrologyGrowthFactorsByIdentifier(AIdentifier: integer): THydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.GetHydrologyGrowthFactorsByIdentifier';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FGFHydrologyObjectContainer.Count -1 do
    begin
      Result := HydrologyGrowthFactorsByIndex[LIndex];
      if (Result.FIdentifier = AIdentifier) then Exit;
    end;
    Result := nil;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.GetHydrologyGrowthFactorsByIndex(AIndex: integer): THydrologyGrowthFactors;
const OPNAME = 'TGrowthFactors.GetHydrologyGrowthFactorsByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FGFHydrologyObjectContainer.Count) then
    begin
      Result :=THydrologyGrowthFactors(FGFHydrologyObjectContainer.Items[AIndex]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.GetMinMaxChannelGrowthFactorsByIdentifier(AIdentifier: integer): TMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.GetMinMaxChannelGrowthFactorsByIdentifier';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FGFMinMaxChannelObjectContainer.Count -1 do
    begin
      Result := MinMaxChannelGrowthFactorsByIndex[LIndex];
      if (Result.FIdentifier = AIdentifier) then Exit;
    end;
    Result := nil;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactors.GetMinMaxChannelGrowthFactorsByIndex(AIndex: integer): TMinMaxChannelGrowthFactors;
const OPNAME = 'TGrowthFactors.GetMinMaxChannelGrowthFactorsByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FGFMinMaxChannelObjectContainer.Count) then
    begin
      Result :=TMinMaxChannelGrowthFactors(FGFMinMaxChannelObjectContainer.Items[AIndex]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TGrowthFactors.ClearDemandGrowthProjectionsDB: boolean;
const OPNAME = 'TGrowthFactors.ClearDemandGrowthProjectionsDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.DeleteAllGrowthProjectionDemand;
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;


end;

function TGrowthFactors.ClearHydrologyGrowthProjectionsDB: boolean;
const OPNAME = 'TGrowthFactors.ClearHydrologyGrowthProjectionsDB';
begin

end;

function TGrowthFactors.ClearMinMaxChannelGrowthProjectionsDB: boolean;
const OPNAME = 'TGrowthFactors.ClearMinMaxChannelGrowthProjectionsDB';
begin

end;
      }
{function TGrowthFactors.AddDemandGrowthProjectionsToDB: boolean;
const OPNAME = 'TGrowthFactors.AddDemandGrowthProjectionsToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddDemandGrowthFactorsToDB(FGFDemandCentresObjectContainer);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;  }

{function TGrowthFactors.AddHydrologyGrowthProjectionsToDB: boolean;
const OPNAME = 'TGrowthFactors.AddHydrologyGrowthProjectionsToDB';
begin

end;

function TGrowthFactors.AddMinMaxChannelGrowthProjectionsToDB: boolean;
const OPNAME = 'TGrowthFactors.AddMinMaxChannelGrowthProjectionsToDB';
begin

end; }


function TGrowthFactors.GetGrowthType : TGrowthType;
const OPNAME = 'TGrowthFactors.GetGrowthType';
begin
  Result := gtDemand;
  try
    result := fGrowthType;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactors.SetGrowthType(AValue : TGrowthType);
const OPNAME = 'TGrowthFactors.SetGrowthType';
begin
  try
    fGrowthType := AValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TGrowthFactors.AddGrowthFactorsConfigDataToDB(AYearsCount: Integer) : boolean;
const OPNAME = 'TGrowthFactors.AddGrowthFactorsConfigDataToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddGrowthFactorsConfigDataToDB(FNumberOfYears);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

end.
