//
//
//  UNIT      : Contains TExelGrowthFactors Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2006/04/06
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UGrowthFactorsExcelData;

interface

uses
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UChannelMinMaxObject,
  UYieldModelDataObject;
type
  TAbstractGrowthFactors = class(TAbstractAppObject)
  protected
    FSavedToDB          : boolean;
    FIdentifier         : integer;
    FInstitution        : String;
    FWaterUser          : String;
  public
    function Initialise     : boolean; override;
//    function BaseYearDemand : double;  virtual; abstract;

    property Institution        : string      read FInstitution   write FInstitution;
    property WaterUser          : string      read FWaterUser     write FWaterUser;
    property Identifier         : integer     read FIdentifier    write FIdentifier;
    property SavedToDB          : boolean     read FSavedToDB     write FSavedToDB;
  end;

  TExelDemandChannelGrowthFactors = class(TAbstractGrowthFactors)
  protected
    FChannelNumber      : integer;
    FGrowthFactors      : TStringList;
    FNoOfYears          : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDemandGrowthFactors: WideString;
    procedure SetDemandGrowthFactors(AFactors : WideString);
    function GetGrowthFactorsCount : integer;
  public
    function Initialise: boolean; override;
    function BaseYearDemand: double; //override;
    function Populate(AIdentifier,AChannelNumber, ANoOfYears : integer;AGrowthFactor: WideString) : boolean;

    function PopulateDemandChannelGrowthFactors(AIdentifier,AChannelNumber: integer; AInstitution, AWaterUser,
             AGrowthFactorsCommaText : String): boolean ;
    function GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer): string; virtual;

    function Validate(var AErrors: WideString): WordBool;

    property GrowthFactorsList : TStringList read FGrowthFactors;
    property ChannelNumber : integer      read FChannelNumber write FChannelNumber;
    property GrowthFactors : widestring   read GetDemandGrowthFactors write SetDemandGrowthFactors;
    property GrowthFactorsCount : integer read GetGrowthFactorsCount;
  end;

  TExelMinMaxChannelGrowthFactors = class(TAbstractGrowthFactors)
  protected
    FArcNumber,
    FChannelNumber,
    FNoOfYears     : integer;
    FGrowthFactors : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMinMaxGrowthFactors: WideString;
    procedure SetMinMaxGrowthFactors(AFactors : WideString);
    function GetMinMaxGrowthFactorsCount : integer;
  public
    function Initialise: boolean; override;
    function BaseYearDemand: double;
    function Populate(AIdentifier,AChannelNumber, ANoOfYears : integer;AGrowthFactor: WideString) : boolean;

    function PopulateMinMaxChannelGrowthFactors(AIdentifier,AChannelNumber,AArcNumber: integer; AInstitution,
             AWaterUser,AGrowthFactorsCommaText : String): boolean ;
    function GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex,AYearsCount,ADataStartYear : integer): string;

    function Validate(var AErrors: WideString): WordBool;

    property ArcNumber : integer               read FArcNumber write FArcNumber;
    property ChannelNumber : integer           read FChannelNumber write FChannelNumber;
    property GrowthFactorsList : TStringList   read FGrowthFactors;
    property GrowthFactors : widestring        read GetMinMaxGrowthFactors write SetMinMaxGrowthFactors;
    property MinMaxGrowthFactorsCount: integer read GetMinMaxGrowthFactorsCount;
  end;

  TExelHydrologyGrowthFactors = class(TAbstractGrowthFactors)
 protected
    FInstitution1     : String;
    FInstitution2     : String;
    FWaterUser1       : String;
    FWaterUser2       : String;
    FGaugeNumber      : integer;
    FAFFGrowthFactors : TStringList;
    FIRRGrowthFactors : TStringList;
    FURBGrowthFactors : TStringList;
    FAFFBaseYearDemand: double;
    FIRRBaseYearDemand: double;
    FURBBaseYearDemand: double;


    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetAFFGrowthFactors: WideString;
    procedure SetAFFGrowthFactors(AFactors : WideString);
    function GetIRRGrowthFactors: WideString;
    procedure SetIRRGrowthFactors(AFactors : WideString);
    function GetURBGrowthFactors: WideString;
    procedure SetURBGrowthFactors(AFactors : WideString);

    function GetAFFGrowthFactorsCount : integer;
    function GetIRRGrowthFactorsCount : integer;
    function GetURBGrowthFactorsCount : integer;
  public
    function Initialise: boolean; override;
    function Populate(AIdentifier,AGaugeNumber, ANoOfYears: integer;AAFFGrowthFactors,
                     AIRRGrowthFactors,AURBGrowthFactors: WideString) : boolean;
    function BaseYearDemand: boolean;
    function PopulateHydrologyGrowthFactors(AIdentifier,AGaugeNumber: integer; AInstitution, AWaterUser,
             AAFFGrowthFactorsCommaText,AIRRGrowthFactorsCommaText,AURBGrowthFactorsCommaText : String): boolean ;
    function GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer;
                                   var AAFFFactors,AIRRFactors,AURBFactors: string): boolean;

    function Validate(var AErrors: WideString): WordBool;

    property Institution1          : string      read FInstitution1        write FInstitution1;
    property Institution2          : string      read FInstitution2        write FInstitution2;
    property WaterUser1            : string      read FWaterUser1          write FWaterUser1;
    property WaterUser2            : string      read FWaterUser2          write FWaterUser2;
    property AFFGrowthFactors      : widestring  read GetAFFGrowthFactors  write SetAFFGrowthFactors;
    property IRRGrowthFactors      : widestring  read GetIRRGrowthFactors  write SetIRRGrowthFactors;
    property URBGrowthFactors      : widestring  read GetURBGrowthFactors  write SetURBGrowthFactors;
    property GaugeNumber           : integer     read FGaugeNumber         write FGaugeNumber;
    property AFFGrowthFactorsCount : integer     read GetAFFGrowthFactorsCount;
    property IRRGrowthFactorsCount : integer     read GetIRRGrowthFactorsCount;
    property URBGrowthFactorsCount : integer     read GetURBGrowthFactorsCount;
    property AFFGrowthFactorsList  : TStringList     read FAFFGrowthFactors;
    property IRRGrowthFactorsList  : TStringList     read FIRRGrowthFactors;
    property URBGrowthFactorsList  : TStringList     read FURBGrowthFactors;

    property AFFBaseYearDemand  : double     read FAFFBaseYearDemand;
    property IRRBaseYearDemand  : double     read FIRRBaseYearDemand;
    property URBBaseYearDemand  : double     read FURBBaseYearDemand;
  end;

  type
  TProjectionType = (ptDemand,ptMinMax,ptHydrology);

  TExelGrowthFactors = class(TAbstractAppObject)
  protected                   
    FProjectionType              : TProjectionType;
    FSavedToDB                   : boolean;
    FBaseYear,
    FStartYear,
    FYearsCount,
    FDataStartYear,
    FBaseYearIndex,
    FStartYearIndex             : Integer;
    FDemandChannelGrowthFactors : TObjectList;
    FMinMaxChannelGrowthFactors : TObjectList;
    FHydrologyGrowthFactors     : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDemandChannelGrowthFactorsByIndex(AIndex: integer): TExelDemandChannelGrowthFactors;
    function GetMinMaxChannelGrowthFactorsByIndex(AIndex: integer): TExelMinMaxChannelGrowthFactors;
    function GetHydrologyGrowthFactorsByIndex(AIndex: integer)    : TExelHydrologyGrowthFactors;
    function GetDemandChannelGrowthFactorsByIdentifier(AIdentifier: integer): TExelDemandChannelGrowthFactors;
    function GetMinMaxChannelGrowthFactorsByIdentifier(AIdentifier: integer): TExelMinMaxChannelGrowthFactors;
    function GetHydrologyGrowthFactorsByIdentifier(AIdentifier: integer)    : TExelHydrologyGrowthFactors;
    function SaveToDB: boolean;
    function TestGenerateFactors: boolean;
    function GetScenarioWhereClause: string;
    procedure SetYearsCount(ACount: integer);
    function GetYearsCount : integer;
    procedure SetStartYear(AValue: integer);
    function GetStartYear : integer;
    procedure SetBaseYear(AValue: integer);
    function GetBaseYear : integer;

    procedure SaveYearsCountToDB(AYearsCount: integer);
    procedure SetSaveDToDB(ASaved: boolean);

    function ClearDemandGrowthProjectionsDB : boolean;
    function ClearMinMaxChannelGrowthProjectionsDB : boolean;
    function ClearHydrologyGrowthProjectionsDB : boolean;

  public
    function Initialise: boolean; override;
    function Populated: boolean;
    function Populate(ABaseYear,ABaseYearIndex, AStartYear,AStartYearIndex, AYearsCount,ADataStartYear: integer): boolean;
    function DemandChannelGrowthFactorsCount: integer;
    function MinMaxChannelGrowthFactorsCount: integer;
    function HydrologyGrowthFactorsCount: integer;
    function GetDemandChannelGrowthFactorsByChannel(AChannelNumber: integer): TExelDemandChannelGrowthFactors;
    function GetExelMinMaxChannelGrowthFactorsByCannelArc(AArc,AChannel : integer) : TExelMinMaxChannelGrowthFactors;
    function AddDemandChannelGrowthFactors : TExelDemandChannelGrowthFactors;
    function AddMinMaxChannelGrowthFactors : TExelMinMaxChannelGrowthFactors;
    function AddHydrologyGrowthFactors     : TExelHydrologyGrowthFactors;

    function ClearDemandChannelGrowthFactors: boolean;
    function ClearMinMaxChannelGrowthFactors: boolean;
    function ClearHydrologyGrowthFactors: boolean;

    function LoadFromDB: boolean;
    function GenerateGrowthFactors: boolean;

    function CreateDemandGrowthProjection        : TExelDemandChannelGrowthFactors;
    function CreateMinMaxChannelGrowthProjection : TExelMinMaxChannelGrowthFactors;
    function CreateHydrologyGrowthProjection     : TExelHydrologyGrowthFactors;
    function RemoveMinMaxChannel(AMinMaxChannel : integer) : boolean;


    function ClearAllProjectionDataFromDB: boolean;
    function SaveAllProjectionDataToDB: boolean;

    function AddDemandGrowthProjectionsToDB : boolean;
    function AddMinMaxChannelGrowthProjectionsToDB : boolean;
    function AddHydrologyGrowthProjectionsToDB : boolean;
    function AddGrowthProjectionConfigurationToDB : Boolean;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool;
    function CastHydrologyGrowthFactorByGaugeNumber(AGaugeNumber: integer): TExelHydrologyGrowthFactors;

    property SavedToDB : boolean read FSavedToDB;
    property DemandChannelGrowthFactorsByIndex[AIndex: integer]    : TExelDemandChannelGrowthFactors  read GetDemandChannelGrowthFactorsByIndex;
    property MinMaxChannelGrowthFactorsByIndex[AIndex: integer]    : TExelMinMaxChannelGrowthFactors  read GetMinMaxChannelGrowthFactorsByIndex;
    property HydrologyGrowthFactorsByIndex[AIndex: integer]        : TExelHydrologyGrowthFactors      read GetHydrologyGrowthFactorsByIndex;

    property DemandChannelGrowthFactorsByIdentifier[AIdentifier: integer]: TExelDemandChannelGrowthFactors read GetDemandChannelGrowthFactorsByIdentifier;
    property MinMaxChannelGrowthFactorsByIdentifier[AIdentifier: integer]: TExelMinMaxChannelGrowthFactors read GetMinMaxChannelGrowthFactorsByIdentifier;
    property HydrologyGrowthFactorsByIdentifier[AIdentifier: integer]    : TExelHydrologyGrowthFactors     read GetHydrologyGrowthFactorsByIdentifier;

    property BaseYear       : integer read GetBaseYear     write SetBaseYear;
    property StartYear      : integer read GetStartYear    write SetStartYear;
    property YearsCount     : integer read GetYearsCount   write SetYearsCount;
    property DataStartYear  : integer read FDataStartYear  write FDataStartYear;
    property BaseYearIndex  : integer read FBaseYearIndex  write FBaseYearIndex;
    property StartYearIndex : integer read FStartYearIndex write FStartYearIndex;

    property ProjectionType : TProjectionType read FProjectionType write FProjectionType;
  end;

implementation

uses
  System.Types,
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UPlanningModelDataObject,
  UGrowthFactorData,
  UGrowthFactorDataSQLAgent,
  UChannelDescriptionObject,
  UErrorHandlingOperations, UAbstractFileNamesObject, DB;

{ TAbstractGrowthFactors }

function TAbstractGrowthFactors.Initialise: boolean;
const OPNAME = 'TAbstractGrowthFactors.Initialise';
begin
  Result := inherited Initialise;
  try
    FIdentifier         := NullInteger;
    FInstitution        := '';
    FWaterUser          := '';
    FSavedToDB          := False;
    Result              := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TExelDemandChannelGrowthFactors }

procedure TExelDemandChannelGrowthFactors.CreateMemberObjects;
const OPNAME = 'TExelDemandChannelGrowthFactors.CreateMemberObjects';
begin
  inherited;
  try
    FGrowthFactors        := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelDemandChannelGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TExelDemandChannelGrowthFactors.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelDemandChannelGrowthFactors.GetDemandGrowthFactors: WideString;
const OPNAME = 'TExelDemandChannelGrowthFactors.GetDemandGrowthFactors';
begin
  Result := '';
  try
    Result := FGrowthFactors.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TExelDemandChannelGrowthFactors.Initialise:boolean;
const OPNAME = 'TExelDemandChannelGrowthFactors.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelNumber  := NullInteger;
    FGrowthFactors.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelDemandChannelGrowthFactors.PopulateDemandChannelGrowthFactors(AIdentifier, AChannelNumber: integer; AInstitution, AWaterUser,
  AGrowthFactorsCommaText: String): boolean;
const OPNAME = 'TExelDemandChannelGrowthFactors.PopulateDemandChannelGrowthFactors';
begin
  Result := False;
  try
    FIdentifier     := AIdentifier;
    FChannelNumber  := AChannelNumber;
    FInstitution    := AInstitution;
    FWaterUser      := AWaterUser;
    FGrowthFactors.CommaText := AGrowthFactorsCommaText;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelDemandChannelGrowthFactors.GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer): string;
const OPNAME = 'TExelDemandChannelGrowthFactors.GenerateGrowthFactors';
var
  Lindex : integer;
  LFactor,
  LCurrentValue,
  LBaseValue: double;
begin
  Result := '';
  try
    LBaseValue := BaseYearDemand;
    for Lindex := 0 to FGrowthFactors.Count-1 do
    begin
      if(LBaseValue = 0.0) then
        LFactor := 0.00
      else
      begin
        LCurrentValue := StrToFloatDef(FGrowthFactors[Lindex],0.0);
        LFactor :=  (LCurrentValue /LBaseValue) -1;
      end;
      Result  := Result + FloatToStr(LFactor) + ',';
    end;
    if(Length(Result) > 0) then
      Result := Copy(Result,1,Length(Result)-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TExelDemandChannelGrowthFactors.GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer): string;
const OPNAME = 'TExelDemandChannelGrowthFactors.GenerateGrowthFactors';
var
  Lindex : integer;
  LFactor,
  LCurrentValue,
  LBaseValue: double;
begin
  Result := '';
  try
    if (ABaseYearIndex >= 0) and (ABaseYearIndex -3 < FGrowthFactors.Count) and
       (AStartYearIndex >= 0) and (AStartYearIndex -3 <= FGrowthFactors.Count) then
    begin
      //LBaseValue := StrToFloatDef(FGrowthFactors[ABaseYearIndex - 3],0.0);
      LBaseValue := BaseYearDemand;
      for Lindex := 1 to AYearsCount do
      begin
        if(LBaseValue = 0.0) then
          LFactor := 0.00
        else
        if((AStartYearIndex-3) >= FGrowthFactors.Count) then
          LFactor := 0.00
        else
        begin
          LCurrentValue := StrToFloatDef(FGrowthFactors[AStartYearIndex - 3 ],0.0);
          LFactor :=  (LCurrentValue /LBaseValue) -1;
        end;
        Result  := Result + FloatToStr(LFactor) + ',';
        AStartYearIndex := AStartYearIndex + 1;
      end;
    end;
    if(Length(Result) > 0) then
      Result := Copy(Result,1,Length(Result)-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}
procedure TExelDemandChannelGrowthFactors.SetDemandGrowthFactors(AFactors: WideString);
const OPNAME = 'TExelDemandChannelGrowthFactors.SetDemandGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
  LGrowthFactors : IGrowthFactors;
  LDemandCentreGrowthFactors : IDemandCentreGrowthFactors;
begin
  try
    if AFactors = FGrowthFactors.CommaText then Exit;
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FGrowthFactors.CommaText;
        if FSavedToDB then
        begin
           FAppModules.FieldProperties.UpdateFieldValue('ExcelDemandGrowthFactors', AFactors,LOldValue, LContextData);
        end;

        FGrowthFactors.CommaText := AFactors;
        LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
        if LGrowthFactors <> nil then
          LDemandCentreGrowthFactors := LGrowthFactors.DemandGrowthFactorsByChannel[FChannelNumber];
        if (LDemandCentreGrowthFactors <> nil) then
          LDemandCentreGrowthFactors.ValidFactors := False;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ExcelDemandGrowthFactors',LOldValue,AFactors);

      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelDemandChannelGrowthFactors.GetGrowthFactorsCount: integer;
const OPNAME = 'TExelDemandChannelGrowthFactors.GetGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGrowthFactors.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TExelDemandChannelGrowthFactors.BaseYearDemand: double;
const OPNAME = 'TExelDemandChannelGrowthFactors.BaseYearDemand';
var
  LFeature : IMasterControlFeature;
  LChannel : IGeneralFlowChannel;
begin
  Result := 0.0;
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FChannelNumber];
    if (lChannel <> nil) then
    begin
      LFeature := lChannel.MasterControlFeature;
      if (LFeature <> nil) then
         Result := LFeature.AnnualDemand;
    end
    else
      Result := 0.0;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TExelDemandChannelGrowthFactors.Populate(AIdentifier,AChannelNumber, ANoOfYears: integer; AGrowthFactor: WideString): boolean;
const OPNAME = 'TExelDemandChannelGrowthFactors.Populate';
begin
    Result := False;
  try
    FIdentifier               := AIdentifier;
    FChannelNumber            := AChannelNumber;
    FNoOfYears                := ANoOfYears;
    FGrowthFactors.CommaText  := AGrowthFactor;
    Result                    := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelDemandChannelGrowthFactors.Validate( var AErrors: WideString): WordBool;
const OPNAME = 'TExelDemandChannelGrowthFactors.Validate';
var
  LChannel       : IGeneralFlowChannel;
  lErrorList     : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      LChannel     := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.ChannelList.
                      ChannelByChannelNumber[FChannelNumber];
      if(LChannel = nil) then
         lErrorList.Add('Demand Channel ' + IntToStr(FChannelNumber) + ' was not found');
      AErrors := AErrors + lErrorList.CommaText;
    finally
      lErrorList.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TExelMinMaxChannelGrowthFactors }

function TExelMinMaxChannelGrowthFactors.GetMinMaxGrowthFactors: WideString;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.GetMinMaxGrowthFactors';
begin
  Result := '';
  try
    Result := FGrowthFactors.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TExelMinMaxChannelGrowthFactors.SetMinMaxGrowthFactors(AFactors: WideString);
const OPNAME = 'TExelMinMaxChannelGrowthFactors.SetMinMaxGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
  LGrowthFactors : IGrowthFactors;
  LMinMaxChannelGrowthFactors : IMinMaxChannelGrowthFactors;
begin
  try
    if AFactors = FGrowthFactors.CommaText then Exit;
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FGrowthFactors.CommaText;
        if FSavedToDB then
        begin
           FAppModules.FieldProperties.UpdateFieldValue('ExcelMinMaxGrowthFactors', AFactors,LOldValue, LContextData);
        end;
        LOldValue := FGrowthFactors.CommaText;
        FGrowthFactors.CommaText := AFactors;
        LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
        if LGrowthFactors <> nil then
          LMinMaxChannelGrowthFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[FChannelNumber];
        if (LMinMaxChannelGrowthFactors <> nil) then
          LMinMaxChannelGrowthFactors.ValidFactors := False;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ExcelMinMaxGrowthFactors',LOldValue,AFactors);

      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelMinMaxChannelGrowthFactors.Initialise: boolean;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.Initialise';
begin
  Result := Inherited Initialise;
  try
    FArcNumber     := NullInteger;
    FChannelNumber := NullInteger;
    FGrowthFactors.Clear;
    Result         := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelMinMaxChannelGrowthFactors.PopulateMinMaxChannelGrowthFactors(AIdentifier,AChannelNumber, AArcNumber: integer;
                                                  AInstitution, AWaterUser, AGrowthFactorsCommaText: String): boolean;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.PopulateMinMaxChannelGrowthFactors';
begin
  Result := False;
  try
    FIdentifier     := AIdentifier;
    FChannelNumber  := AChannelNumber;
    FInstitution    := AInstitution;
    FWaterUser      := AWaterUser;
    FArcNumber      := AArcNumber;
    FGrowthFactors.CommaText := AGrowthFactorsCommaText;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelMinMaxChannelGrowthFactors.GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer): string;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.GenerateGrowthFactors';
var
  Lindex : integer;
  LFactor,
  LCurrentValue,
  LBaseValue: double;
begin
  Result := '';
  try
    LBaseValue := BaseYearDemand;
    for Lindex := 0 to FGrowthFactors.Count-1 do
    begin
      if(LBaseValue = 0.0) then
        LFactor := 0.00
      else
      begin
        LCurrentValue := StrToFloatDef(FGrowthFactors[Lindex],0.0);
        LFactor :=  (Abs(LCurrentValue) / Abs(LBaseValue)) -1;
      end;
      Result  := Result + FloatToStr(LFactor) + ',';
    end;
    if(Length(Result) > 0) then
      Result := Copy(Result,1,Length(Result)-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
(*function TExelMinMaxChannelGrowthFactors.GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer): string;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.GenerateGrowthFactors';
var
  Lindex : integer;
  LFactor,
  LCurrentValue,
  LBaseValue: double;
begin
  Result := '';
  try
    if ((ABaseYearIndex-4) < FGrowthFactors.Count) and ((AStartYearIndex-4) < FGrowthFactors.Count) then
    begin
      {if (ABaseYearIndex < 4) then
         ABaseYearIndex := 4;
      }
      if (AStartYearIndex < 4) then
        AStartYearIndex := 4;
      //LBaseValue := StrToFloatDef(FGrowthFactors[ABaseYearIndex-4],0.0);
      LBaseValue := BaseYearDemand;
      for Lindex := 1 to AYearsCount do
      begin
        if(LBaseValue = 0.0) then
          LFactor := 0.00
        else
        if((AStartYearIndex-4) >= FGrowthFactors.Count) then
          LFactor := 0.00
        else
        begin
          LCurrentValue := StrToFloatDef(FGrowthFactors[AStartYearIndex -4],0.0);
          LFactor :=  (Abs(LCurrentValue) / Abs(LBaseValue)) -1;
        end;
        Result  := Result + FloatToStr(LFactor) + ',';
        AStartYearIndex := AStartYearIndex + 1;
      end;
    end;
    if(Length(Result) > 0) then
      Result := Copy(Result,1,Length(Result)-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
*)

function TExelMinMaxChannelGrowthFactors.GetMinMaxGrowthFactorsCount: integer;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.GetMinMaxGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FGrowthFactors.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelMinMaxChannelGrowthFactors.CreateMemberObjects;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.CreateMemberObjects';
begin
  inherited;
  try
    FGrowthFactors := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelMinMaxChannelGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelMinMaxChannelGrowthFactors.BaseYearDemand: double;
const
  OPNAME = 'TExelMinMaxChannelGrowthFactors.BaseYearDemand';
  LRatio  = (60*60*24*364.25)/ 1000000;
var
  LChannel          : IGeneralFlowChannel;
  LConstraint       : IMinMaxFlowConstraint;
  //LMonthDays        : TMonthDaysArray;
  //  LDaysTotal,
  //  LValues,
  LBaseYearDemand   : double;
  LIndex            : integer;
begin
  Result          := 0.0;
  try
    LChannel          := TYieldModelDataObject(FAppModules.Model.ModelData).
                         NetworkElementData.ChannelList.ChannelByChannelNumber[FChannelNumber];
    if (LChannel <> nil )then
    begin
      LConstraint       := LChannel.MinMaxFlowConstraint;
      if (lConstraint <> nil) then
      begin
        if (FArcNumber <= lConstraint.FlowConstraintCount) then
        begin
          LBaseYearDemand := 0.0;
          //LDaysTotal      := 0.0;
          for LIndex := 1 to 12 do
          begin
            LBaseYearDemand := LBaseYearDemand +  LConstraint.FlowConstraintByArcMonth[FArcNumber,LIndex];
            //     LDaysTotal      := LDaysTotal + LMonthDays[LIndex]*60*60*24;
            //     LBaseYearDemand := LBaseYearDemand + (LConstraint.FlowConstraintByArcMonth[FArcNumber,LIndex]*60*60*24*LMonthDays[LIndex]);
          end;
          Result := (LBaseYearDemand/ 12)* LRatio; ///LDaysTotal;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;


end;

function TExelMinMaxChannelGrowthFactors.Populate(AIdentifier,AChannelNumber, ANoOfYears: integer; AGrowthFactor: WideString): boolean;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.Populate';
begin
    Result := False;
  try
    FIdentifier               := AIdentifier;
    FChannelNumber            := AChannelNumber;
    FNoOfYears                := ANoOfYears;
    FGrowthFactors.CommaText  := AGrowthFactor;
    Result                    := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelMinMaxChannelGrowthFactors.Validate(var AErrors: WideString): WordBool;
const OPNAME = 'TExelMinMaxChannelGrowthFactors.Validate';
var
  LChannel          : IGeneralFlowChannel;
  LErrorList        : TStringList;
  LPenaltyStructure : IChannelPenalty;
begin
  Result := FALSE;
  try
    LErrorList := TStringList.Create;
    try
      LChannel          := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.ChannelList.
                             ChannelByChannelNumber[FChannelNumber];
      LPenaltyStructure := LChannel.ChannelPenalty;

      if(LChannel = nil) then
         LErrorList.Add('Min Max Channel ' + IntToStr(FChannelNumber) + ' was not found');

      if(FArcNumber > lPenaltyStructure.ChannelPenaltyArcCount)
       or(FArcNumber <= 0) then
         LErrorList.Add('Arc Number ' + IntToStr(FArcNumber) + ' was not found');

      AErrors := AErrors + LErrorList.CommaText;
    finally
      LErrorList.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TExelHydrologyGrowthFactors }

procedure TExelHydrologyGrowthFactors.CreateMemberObjects;
const OPNAME = 'TExelHydrologyGrowthFactors.CreateMemberObjects';
begin
  inherited;
  try
    FAFFGrowthFactors := TStringList.Create;
    FIRRGrowthFactors := TStringList.Create;
    FURBGrowthFactors := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelHydrologyGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TExelHydrologyGrowthFactors.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FAFFGrowthFactors);
    FreeAndNil(FIRRGrowthFactors);
    FreeAndNil(FURBGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer;
                                                           var AAFFFactors,AIRRFactors,AURBFactors: string): boolean;
const OPNAME = 'TExelHydrologyGrowthFactors.GenerateGrowthFactors';
var
  LStartYearIndex,
  Lindex : integer;
  LFactor,
  LCurrentValue,
  LAFFPreValue,
  LIRRPreValue,
  LURBPreValue : double;
begin
  Result      := False;
  try
    AAFFFactors := '';
    AIRRFactors := '';
    AURBFactors := '';

    if (AStartYearIndex < 4) then
      AStartYearIndex := 4;
    LAFFPreValue := 0.00;
    LIRRPreValue := 0.00;
    LURBPreValue := 0.00;
    for Lindex := 0 to AYearsCount do
    begin
      //AFF
      if((AStartYearIndex-4) >= FAFFGrowthFactors.Count) then
        LFactor := 0.00
      else
      begin
        LStartYearIndex := (AStartYearIndex-4)-1;
        if LStartYearIndex < 0 then
          LAFFPreValue := FAFFBaseYearDemand;
        LCurrentValue := StrToFloatDef(FAFFGrowthFactors[AStartYearIndex -4],0.0);
        if (LAFFPreValue = 0) or (LCurrentValue = 0) then
          LFactor := 0.00
        else
          LFactor :=  (Abs(LCurrentValue)/Abs(LAFFPreValue))-1;
        LAFFPreValue := StrToFloatDef(FAFFGrowthFactors[AStartYearIndex-4],0.0);
      end;
      AAFFFactors  := AAFFFactors + FloatToStr(LFactor) + ',';
      //IRR
      if((AStartYearIndex-4) >= FIRRGrowthFactors.Count) then
        LFactor := 0.00
      else
      begin
        LStartYearIndex := (AStartYearIndex-4)-1;
        if LStartYearIndex < 0 then
          LIRRPreValue := FIRRBaseYearDemand;
        LCurrentValue := StrToFloatDef(FIRRGrowthFactors[AStartYearIndex -4],0.0);
        if (LIRRPreValue = 0) or (LCurrentValue = 0) then
          LFactor := 0.00
        else
          LFactor :=  (Abs(LCurrentValue)/Abs(LIRRPreValue))-1;
        LIRRPreValue := StrToFloatDef(FIRRGrowthFactors[AStartYearIndex-4],0.0);
      end;
      AIRRFactors  := AIRRFactors + FloatToStr(LFactor) + ',';
      //URB
      if((AStartYearIndex-4) >= FURBGrowthFactors.Count) then
        LFactor := 0.00
      else
      begin
        LStartYearIndex := (AStartYearIndex-4)-1;
        if LStartYearIndex < 0 then
          LURBPreValue := FURBBaseYearDemand;
        LCurrentValue := StrToFloatDef(FURBGrowthFactors[AStartYearIndex -4],0.0);
        if (LURBPreValue = 0) or (LCurrentValue = 0) then
          LFactor := 0.00
        else
          LFactor :=  (Abs(LCurrentValue)/Abs(LURBPreValue))-1;
        LURBPreValue := StrToFloatDef(FURBGrowthFactors[AStartYearIndex-4],0.0);
      end;
      AURBFactors  := AURBFactors + FloatToStr(LFactor) + ',';
      AStartYearIndex := AStartYearIndex + 1;
    end;
    if(Length(AAFFFactors) > 0) then
      AAFFFactors := Copy(AAFFFactors,1,Length(AAFFFactors)-1);
    if(Length(AIRRFactors) > 0) then
      AIRRFactors := Copy(AIRRFactors,1,Length(AIRRFactors)-1);
    if(Length(AURBFactors) > 0) then
      AURBFactors := Copy(AURBFactors,1,Length(AURBFactors)-1);

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


{function TExelHydrologyGrowthFactors.GenerateGrowthFactors(ABaseYearIndex,AStartYearIndex, AYearsCount, ADataStartYear: integer;
                                                           var AAFFFactors,AIRRFactors,AURBFactors: string): boolean;
const OPNAME = 'TExelHydrologyGrowthFactors.GenerateGrowthFactors';
var
  Lindex : integer;
  LFactor,
  LCurrentValue,
  LBaseValue  : double;
begin
  Result      := False;
  try
    AAFFFactors := '';
    AIRRFactors := '';
    AURBFactors := '';

    if (ABaseYearIndex < 4) then
      ABaseYearIndex := 4;

    if (AStartYearIndex < 4) then
      AStartYearIndex := 4;

    for Lindex := 1 to AYearsCount do
    begin
      LBaseValue := StrToFloatDef(FAFFGrowthFactors[ABaseYearIndex-4],0.0);
      if(LBaseValue = 0.0) then
        LFactor := 0.00
      else
      if((AStartYearIndex-4) >= FAFFGrowthFactors.Count) then
        LFactor := 0.00
      else
      begin
        LCurrentValue := StrToFloatDef(FAFFGrowthFactors[AStartYearIndex -4],0.0);
        LFactor :=  (Abs(LCurrentValue) / Abs(LBaseValue)) -1;
      end;
      AAFFFactors  := AAFFFactors + FloatToStr(LFactor) + ',';

      //IRR
      LBaseValue := StrToFloatDef(FIRRGrowthFactors[ABaseYearIndex-4],0.0);
      if(LBaseValue = 0.0) then
        LFactor := 0.00
      else
      if((AStartYearIndex-4) >= FIRRGrowthFactors.Count) then
        LFactor := 0.00
      else
      begin
        LCurrentValue := StrToFloatDef(FIRRGrowthFactors[AStartYearIndex -4],0.0);
        LFactor :=  (Abs(LCurrentValue) / Abs(LBaseValue)) -1;
      end;
      AIRRFactors  := AIRRFactors + FloatToStr(LFactor) + ',';

      //URB
      LBaseValue := StrToFloatDef(FURBGrowthFactors[ABaseYearIndex-4],0.0);
       if(LBaseValue = 0.0) then
        LFactor := 0.00
      else
      if((AStartYearIndex-4) >= FURBGrowthFactors.Count) then
        LFactor := 0.00
      else
      begin
        LCurrentValue := StrToFloatDef(FURBGrowthFactors[AStartYearIndex -4],0.0);
        LFactor :=  (Abs(LCurrentValue) / Abs(LBaseValue)) -1;
      end;
      AURBFactors  := AURBFactors + FloatToStr(LFactor) + ',';

      AStartYearIndex := AStartYearIndex + 1;
    end;

    if(Length(AAFFFactors) > 0) then
      AAFFFactors := Copy(AAFFFactors,1,Length(AAFFFactors)-1);
    if(Length(AIRRFactors) > 0) then
      AIRRFactors := Copy(AIRRFactors,1,Length(AIRRFactors)-1);
    if(Length(AURBFactors) > 0) then
      AURBFactors := Copy(AURBFactors,1,Length(AURBFactors)-1);

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}

function TExelHydrologyGrowthFactors.GetAFFGrowthFactors: WideString;
const OPNAME = 'TExelHydrologyGrowthFactors.GetAFFGrowthFactors';
begin
  Result := '';
  try
    Result := FAFFGrowthFactors.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelHydrologyGrowthFactors.SetAFFGrowthFactors(AFactors: WideString);
const OPNAME = 'TExelHydrologyGrowthFactors.SetAFFGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FSavedToDB then
          FAppModules.FieldProperties.UpdateFieldValue('ExcelAFFGrowthFactors', AFactors,LOldValue, LContextData);
        LOldValue := FAFFGrowthFactors.CommaText;
        FAFFGrowthFactors.CommaText := AFactors;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ExcelAFFGrowthFactors',LOldValue,AFactors);
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.GetIRRGrowthFactors: WideString;
const OPNAME = 'TExelHydrologyGrowthFactors.GetIRRGrowthFactors';
begin
  Result := '';
  try
    Result := FIRRGrowthFactors.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelHydrologyGrowthFactors.SetIRRGrowthFactors(AFactors: WideString);
const OPNAME = 'TExelHydrologyGrowthFactors.SetIRRGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FSavedToDB then
          FAppModules.FieldProperties.UpdateFieldValue('ExcelIRRGrowthFactors', AFactors,LOldValue, LContextData);
        LOldValue := FIRRGrowthFactors.CommaText;
        FIRRGrowthFactors.CommaText := AFactors;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ExcelIRRGrowthFactors',LOldValue,AFactors);
     finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.GetURBGrowthFactors: WideString;
const OPNAME = 'TExelHydrologyGrowthFactors.GetURBGrowthFactors';
begin
  Result := '';
  try
    Result := FURBGrowthFactors.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelHydrologyGrowthFactors.SetURBGrowthFactors(AFactors: WideString);
const OPNAME = 'TExelHydrologyGrowthFactors.SetURBGrowthFactors';
var
  LSQLAgent : TGrowthFactorDataSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    LSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FSavedToDB then
          FAppModules.FieldProperties.UpdateFieldValue('ExcelURBGrowthFactors', AFactors,LOldValue, LContextData);

        LOldValue := FURBGrowthFactors.CommaText;
        FURBGrowthFactors.CommaText := AFactors;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ExcelURBGrowthFactors',LOldValue,AFactors);
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.GetAFFGrowthFactorsCount: integer;
const OPNAME = 'TExelHydrologyGrowthFactors.GetAFFGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FAFFGrowthFactors.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.GetIRRGrowthFactorsCount: integer;
const OPNAME = 'TExelHydrologyGrowthFactors.GetIRRGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FIRRGrowthFactors.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.GetURBGrowthFactorsCount: integer;
const OPNAME = 'TExelHydrologyGrowthFactors.GetURBGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FURBGrowthFactors.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.Initialise: boolean;
const OPNAME = 'TExelHydrologyGrowthFactors.Initialise';
begin
  Result := inherited Initialise;
  try
    FAFFGrowthFactors.Clear;
    FIRRGrowthFactors.Clear;
    FURBGrowthFactors.Clear;
    FGaugeNumber := NullInteger;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.PopulateHydrologyGrowthFactors(AIdentifier, AGaugeNumber: integer; AInstitution, AWaterUser,
                                              AAFFGrowthFactorsCommaText, AIRRGrowthFactorsCommaText,
                                              AURBGrowthFactorsCommaText: String): boolean;
const OPNAME = 'TExelHydrologyGrowthFactors.PopulateHydrologyGrowthFactors';
begin
  Result := False;
  try
    FIdentifier     := AIdentifier;
    FGaugeNumber    := AGaugeNumber;
    FInstitution    := AInstitution;
    FWaterUser      := AWaterUser;
    FAFFGrowthFactors.CommaText := AAFFGrowthFactorsCommaText;
    FIRRGrowthFactors.CommaText := AIRRGrowthFactorsCommaText;
    FURBGrowthFactors.CommaText := AURBGrowthFactorsCommaText;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.BaseYearDemand: boolean;
const
   OPNAME = 'TExelHydrologyGrowthFactors.BaseYearDemand';
   LSQL   = 'SELECT count(*) As NumberOfRec,sum(HydroTotalValue) As TotalValue,sum(HydroTotalValue)/count(*) As Averg'+
            ' from HydrologyFileData where FileName = :FileName '+
            ' AND StudyAreaName = :StudyAreaName ';
var
  LFilePath          : string;
  LParamReference    : IParamReference;
  LDataSet           : TAbstractModelDataset;
  LStudyAreaName     : string;
begin
  Result             := false;
  try
    LParamReference  := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                            ReferenceDataByCatchNumber[FGaugeNumber];
    if(LParamReference <> nil) then
    begin
      LStudyAreaName := FAppModules.StudyArea.StudyAreaCode;
      LFilePath := LParamReference.FileReference;
      LFilePath := UpperCase(ExtractFileName(LFilePath));
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      //****AFF****//
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          LDataSet.SetSQL(LSQL);
          LDataSet.SetParams(['FileName'], [LFilePath + '.AFF']);
          LDataSet.SetParams(['StudyAreaName'], [LStudyAreaName]);
          LDataSet.DataSet.Open;
          FAFFBaseYearDemand       := LDataSet.DataSet.FieldbyName('Averg').AsFloat;
          LDataSet.DataSet.Close;

          //****IRR****//
          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          LDataSet.SetSQL(LSQL);
          LDataSet.SetParams(['FileName'], [LFilePath + '.IRR']);
          LDataSet.SetParams(['StudyAreaName'], [LStudyAreaName]);
          LDataSet.DataSet.Open;
          FIRRBaseYearDemand       := LDataSet.DataSet.FieldbyName('Averg').AsFloat;
          LDataSet.DataSet.Close;

              //****URB****//
          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          LDataSet.SetSQL(LSQL);
          LDataSet.SetParams(['FileName'], [LFilePath + '.URB']);
          LDataSet.SetParams(['StudyAreaName'], [LStudyAreaName]);
          LDataSet.DataSet.Open;
          FURBBaseYearDemand       := LDataSet.DataSet.FieldbyName('Averg').AsFloat;
          LDataSet.DataSet.Close;
        end;
      finally
        LDataSet.Free;
      end;
    end;
    Result := true;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.Populate(AIdentifier, AGaugeNumber, ANoOfYears: integer;
                                         AAFFGrowthFactors, AIRRGrowthFactors, AURBGrowthFactors: WideString): boolean;
const OPNAME = 'TExelHydrologyGrowthFactors.Populate';
begin
  Result := False;
  try
    FGaugeNumber                := AGaugeNumber;
    FAFFGrowthFactors.CommaText := AAFFGrowthFactors;
    FIRRGrowthFactors.CommaText := AIRRGrowthFactors;
    FURBGrowthFactors.CommaText := AURBGrowthFactors;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelHydrologyGrowthFactors.Validate( var AErrors: WideString): WordBool;
const OPNAME = 'TExelHydrologyGrowthFactors.Validate';
var
  LParamReference    : IParamReference;
  LErrorList        : TStringList;
begin
  Result := FALSE;
  try
    LErrorList := TStringList.Create;
    try
      LParamReference  := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                            ReferenceDataByCatchNumber[FGaugeNumber];

      if(LParamReference = nil) then
         LErrorList.Add('Gauge Number ' + IntToStr(FGaugeNumber) + ' was not found');

      AErrors := AErrors + LErrorList.CommaText;
    finally
      LErrorList.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TExelGrowthFactors }

procedure TExelGrowthFactors.CreateMemberObjects;
const OPNAME = 'TExelGrowthFactors.CreateMemberObjects';
begin
  inherited;
  try
    FDemandChannelGrowthFactors := TObjectList.Create(True);
    FMinMaxChannelGrowthFactors := TObjectList.Create(True);
    FHydrologyGrowthFactors     := TObjectList.Create(True);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TExelGrowthFactors.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDemandChannelGrowthFactors);
    FreeAndNil(FMinMaxChannelGrowthFactors);
    FreeAndNil(FHydrologyGrowthFactors);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.Initialise: boolean;
const OPNAME = 'TExelGrowthFactors.Initialise';
begin
  Result := inherited Initialise;
  try
    FSavedToDB      := False;
    FBaseYear       := NullInteger;
    FStartYear      := NullInteger;
    FBaseYearIndex  := -1;
    FStartYearIndex := -1;
    FYearsCount     := NullInteger;
    FDataStartYear  := NullInteger;
    FDemandChannelGrowthFactors.Clear;
    FMinMaxChannelGrowthFactors.Clear;
    FHydrologyGrowthFactors.Clear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.AddDemandChannelGrowthFactors: TExelDemandChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.AddDemandChannelGrowthFactors';
begin
  Result := nil;
  try
    Result := TExelDemandChannelGrowthFactors.Create(FAppModules);
    FDemandChannelGrowthFactors.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.AddMinMaxChannelGrowthFactors: TExelMinMaxChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.AddMinMaxChannelGrowthFactors';
begin
  Result := nil;
  try
    Result := TExelMinMaxChannelGrowthFactors.Create(FAppModules);
    FMinMaxChannelGrowthFactors.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.AddHydrologyGrowthFactors: TExelHydrologyGrowthFactors;
const OPNAME = 'TExelGrowthFactors.AddHydrologyGrowthFactors';
begin
  Result := nil;
  try
    Result := TExelHydrologyGrowthFactors.Create(FAppModules);
    FHydrologyGrowthFactors.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetDemandChannelGrowthFactorsByIndex(AIndex: integer): TExelDemandChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetDemandChannelGrowthFactorsByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FDemandChannelGrowthFactors.Count) then
    begin
      Result := TExelDemandChannelGrowthFactors(FDemandChannelGrowthFactors.Items[AIndex]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetMinMaxChannelGrowthFactorsByIndex(AIndex: integer): TExelMinMaxChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetMinMaxChannelGrowthFactorsByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FMinMaxChannelGrowthFactors.Count) then
    begin
      Result := TExelMinMaxChannelGrowthFactors(FMinMaxChannelGrowthFactors.Items[AIndex]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetHydrologyGrowthFactorsByIndex(AIndex: integer): TExelHydrologyGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetHydrologyGrowthFactorsByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FHydrologyGrowthFactors.Count) then
    begin
      Result := TExelHydrologyGrowthFactors(FHydrologyGrowthFactors.Items[AIndex]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.DemandChannelGrowthFactorsCount: integer;
const OPNAME = 'TExelGrowthFactors.DemandChannelGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FDemandChannelGrowthFactors.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.HydrologyGrowthFactorsCount: integer;
const OPNAME = 'TExelGrowthFactors.HydrologyGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FHydrologyGrowthFactors.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.MinMaxChannelGrowthFactorsCount: integer;
const OPNAME = 'TExelGrowthFactors.MinMaxChannelGrowthFactorsCount';
begin
  Result := 0;
  try
    Result := FMinMaxChannelGrowthFactors.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.ClearDemandChannelGrowthFactors: boolean;
const OPNAME = 'TExelGrowthFactors.ClearDemandChannelGrowthFactors';
begin
  Result := False;
  try
    FDemandChannelGrowthFactors.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.ClearMinMaxChannelGrowthFactors: boolean;
const OPNAME = 'TExelGrowthFactors.ClearMinMaxChannelGrowthFactors';
begin
  Result := False;
  try
    FMinMaxChannelGrowthFactors.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.ClearHydrologyGrowthFactors: boolean;
const OPNAME = 'TExelGrowthFactors.ClearHydrologyGrowthFactors';
begin
  Result := False;
  try
    FHydrologyGrowthFactors.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.Populate(ABaseYear,ABaseYearIndex, AStartYear,AStartYearIndex, AYearsCount,ADataStartYear: integer): boolean;
const OPNAME = 'TExelGrowthFactors.Populate';
begin
  Result := False;
  try
    FBaseYear       := ABaseYear;
    FStartYear      := AStartYear;
    FYearsCount     := AYearsCount;
    FDataStartYear  := ADataStartYear;
    FBaseYearIndex  := ABaseYearIndex;
    FStartYearIndex :=  AStartYearIndex;
    Result      := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.Populated: boolean;
const OPNAME = 'TExelGrowthFactors.Populated';
begin
  Result := False;
  try
    Result := (FDemandChannelGrowthFactors.Count > 0) or
              (FMinMaxChannelGrowthFactors.Count > 0) or (FHydrologyGrowthFactors.Count > 0);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetScenarioWhereClause: string;
const OPNAME = 'TExelGrowthFactors.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TExelGrowthFactors.LoadFromDB: boolean;
const OPNAME = 'TExelGrowthFactors.LoadFromDB';
var
  lSQL              : string;
  LIdentifier       : integer;
  LInstitution      : String;
  LWaterUser        : String;
  LChannelNumber    : integer;
  LGrowthFactors    : String;
  LArcNumber        : integer;
  LIRRGrowthFactors : String;
  LURBGrowthFactors : String;
  LDataSet          : TAbstractModelDataset;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  Result := False;
  try
    Initialise;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := 'SELECT * FROM GrowthFactorExcelConfig WHERE ' +
        GetScenarioWhereClause ;
        LDataSet.SetSQL(lSQL);
        LDataSet.DataSet.Open;
        if not LDataSet.DataSet.Eof then
        begin
          FBaseYear       := LDataSet.DataSet.FieldbyName('BaseYear').AsInteger;
          FStartYear      := LDataset.DataSet.FieldByName('StartYear').AsInteger;
          FYearsCount     := LDataset.DataSet.FieldByName('YearsCount').AsInteger;
          FDataStartYear  := LDataset.DataSet.FieldByName('DataStartYear').AsInteger;

          LDataSet.DataSet.Close;

          lSQL := 'SELECT * FROM GrowthFactorExcelDemand WHERE ' +
          GetScenarioWhereClause +'  ORDER BY Identifier ';

          LDataSet.SetSQL(lSQL);
          LDataSet.DataSet.Open;
          while not LDataSet.DataSet.Eof do
          begin
            LIdentifier       := LDataSet.DataSet.FieldbyName('Identifier').AsInteger;
            LInstitution      := Trim(LDataSet.DataSet.FieldbyName('Institution').AsString);
            LWaterUser        := Trim(LDataSet.DataSet.FieldbyName('WaterUser').AsString);
            LChannelNumber    := LDataSet.DataSet.FieldbyName('ChannelNumber').AsInteger;
            LGrowthFactors    := Trim(LDataSet.DataSet.FieldbyName('Factors').AsString);
            LDemandChannelFactors := AddDemandChannelGrowthFactors;
            LDemandChannelFactors.PopulateDemandChannelGrowthFactors(LIdentifier,LChannelNumber,LInstitution,
              LWaterUser,LGrowthFactors);
            LDataSet.DataSet.Next;
          end;
          LDataSet.DataSet.Close;

          lSQL := 'SELECT * FROM GrowthFactorExcelMinMax WHERE ' +
          GetScenarioWhereClause +'  ORDER BY Identifier ';
          LDataSet.SetSQL(lSQL);
          LDataSet.DataSet.Open;
          while not LDataSet.DataSet.Eof do
          begin
            LIdentifier       := LDataSet.DataSet.FieldbyName('Identifier').AsInteger;
            LInstitution      := Trim(LDataSet.DataSet.FieldbyName('Institution').AsString);
            LWaterUser        := Trim(LDataSet.DataSet.FieldbyName('WaterUser').AsString);
            LChannelNumber    := LDataSet.DataSet.FieldbyName('ChannelNumber').AsInteger;
            LArcNumber        := LDataSet.DataSet.FieldbyName('ArcNumber').AsInteger;
            LGrowthFactors    := Trim(LDataSet.DataSet.FieldbyName('Factors').AsString);
            LMinMaxChannelFactors := AddMinMaxChannelGrowthFactors;
            LMinMaxChannelFactors.PopulateMinMaxChannelGrowthFactors(LIdentifier,LChannelNumber,LArcNumber,LInstitution,LWaterUser,LGrowthFactors);
            LDataSet.DataSet.Next;
          end;
          LDataSet.DataSet.Close;

          lSQL := 'SELECT * FROM GrowthFactorExcelHydrology WHERE ' +
          GetScenarioWhereClause +'  ORDER BY Identifier ';
          LDataSet.SetSQL(lSQL);
          LDataSet.DataSet.Open;
          while not LDataSet.DataSet.Eof do
          begin
            LIdentifier          := LDataSet.DataSet.FieldbyName('Identifier').AsInteger;
            LInstitution         := Trim(LDataSet.DataSet.FieldbyName('Institution').AsString);
            LWaterUser           := Trim(LDataSet.DataSet.FieldbyName('WaterUser').AsString);
            LChannelNumber       := LDataSet.DataSet.FieldbyName('GaugeNumber').AsInteger;
            LGrowthFactors       := Trim(LDataSet.DataSet.FieldbyName('AFFFactors').AsString);
            LIRRGrowthFactors    := Trim(LDataSet.DataSet.FieldbyName('IRRFactors').AsString);
            LURBGrowthFactors    := Trim(LDataSet.DataSet.FieldbyName('URBFactors').AsString);
            LHydrologyFactors    := AddHydrologyGrowthFactors;
            LHydrologyFactors.PopulateHydrologyGrowthFactors(LIdentifier,LChannelNumber,LInstitution,LWaterUser,LGrowthFactors,LIRRGrowthFactors,LURBGrowthFactors);
            LHydrologyFactors.FInstitution1 := Trim(LDataSet.DataSet.FieldbyName('Institution1').AsString);
            LHydrologyFactors.FInstitution2 := Trim(LDataSet.DataSet.FieldbyName('Institution2').AsString);
            LHydrologyFactors.FWaterUser1   := Trim(LDataSet.DataSet.FieldbyName('WaterUser1').AsString);
            LHydrologyFactors.FWaterUser2   := Trim(LDataSet.DataSet.FieldbyName('WaterUser2').AsString);
            LDataSet.DataSet.Next;
          end;
          LDataSet.DataSet.Close;

          SetSaveDToDB(True);
        end;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.SaveToDB: boolean;
const OPNAME = 'TExelGrowthFactors.SaveToDB';
var
  LIndex                : integer;
  lSQL,
  LWhereClause          : string;
  LDataSet              : TAbstractModelDataset;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := 'DELETE FROM GrowthFactorExcelConfig WHERE ' + GetScenarioWhereClause ;
        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;

        lSQL := 'DELETE FROM GrowthFactorExcelDemand WHERE ' + GetScenarioWhereClause ;
        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;


        lSQL := 'DELETE FROM GrowthFactorExcelMinMax WHERE ' + GetScenarioWhereClause ;
        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;


        lSQL := 'DELETE FROM GrowthFactorExcelHydrology WHERE ' + GetScenarioWhereClause ;
        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;


        lSQL := 'INSERT INTO GrowthFactorExcelConfig'+
                '(Model, StudyAreaName, SubArea, Scenario, BaseYear, StartYear, YearsCount, DataStartYear)'+
                ' VALUES'+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :BaseYear, :StartYear, :YearsCount, :DataStartYear)';
        LDataSet.SetSQL(lSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
          [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['BaseYear'], [IntToStr(FBaseYear)]);
        LDataSet.SetParams(['StartYear'], [IntToStr(FStartYear)]);
        LDataSet.SetParams(['YearsCount'], [IntToStr(FYearsCount)]);
        LDataSet.SetParams(['DataStartYear'], [IntToStr(FDataStartYear)]);

        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;

        for LIndex := 0 to FDemandChannelGrowthFactors.Count -1 do
        begin
          LDemandChannelFactors := DemandChannelGrowthFactorsByIndex[LIndex];
          lSQL := 'INSERT INTO GrowthFactorExcelDemand'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber, Institution, WaterUser)'+
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelNumber, :Institution, :WaterUser)';
          LDataSet.SetSQL(lSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
            [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['Identifier'], [IntToStr(LDemandChannelFactors.Identifier)]);
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LDemandChannelFactors.ChannelNumber)]);
          LDataSet.SetParams(['Institution'], [LDemandChannelFactors.Institution]);
          LDataSet.SetParams(['WaterUser'], [LDemandChannelFactors.WaterUser]);
          LDataSet.ExecSQL;

          if (LDemandChannelFactors <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LDemandChannelFactors.Identifier);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelDemand','Factors',LWhereClause,
                                             LDemandChannelFactors.GrowthFactors);
           end;
           LDataSet.DataSet.Close;
        end;

        for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
        begin
          LMinMaxChannelFactors := MinMaxChannelGrowthFactorsByIndex[LIndex];
          lSQL := 'INSERT INTO GrowthFactorExcelMinMax'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber, ArcNumber, Institution, WaterUser)'+
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelNumber,:ArcNumber, :Institution, :WaterUser)';
          LDataSet.SetSQL(lSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
            [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['Identifier'], [IntToStr(LMinMaxChannelFactors.Identifier)]);
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LMinMaxChannelFactors.ChannelNumber)]);
          LDataSet.SetParams(['ArcNumber'], [IntToStr(LMinMaxChannelFactors.ArcNumber)]);
          LDataSet.SetParams(['Institution'], [LMinMaxChannelFactors.Institution]);
          LDataSet.SetParams(['WaterUser'], [LMinMaxChannelFactors.WaterUser]);
          LDataSet.ExecSQL;
          if (LMinMaxChannelFactors <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LMinMaxChannelFactors.Identifier);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelMinMax','Factors',LWhereClause,
                                             LMinMaxChannelFactors.GrowthFactors);
          end;
          LDataSet.DataSet.Close;
        end;

        for LIndex := 0 to FHydrologyGrowthFactors.Count -1 do
        begin
          LHydrologyFactors := HydrologyGrowthFactorsByIndex[LIndex];
          lSQL := 'INSERT INTO GrowthFactorExcelHydrology'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, GaugeNumber, Institution, Institution1, Institution2, WaterUser, WaterUser1, WaterUser2)'+ //, AFFFactors,IRRFactors,URBFactors
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :GaugeNumber, :Institution, :Institution1, :Institution2, :WaterUser, :WaterUser1, :WaterUser2)'; //, :AFFFactors, :IRRFactors, :URBFactors
          LDataSet.SetSQL(lSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
            [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['Identifier'], [IntToStr(LHydrologyFactors.Identifier)]);
          LDataSet.SetParams(['GaugeNumber'], [IntToStr(LHydrologyFactors.GaugeNumber)]);
          LDataSet.SetParams(['Institution'], [LHydrologyFactors.Institution]);
          LDataSet.SetParams(['Institution1'], [LHydrologyFactors.Institution1]);
          LDataSet.SetParams(['Institution2'], [LHydrologyFactors.Institution2]);
          LDataSet.SetParams(['WaterUser'], [LHydrologyFactors.WaterUser]);
          LDataSet.SetParams(['WaterUser1'], [LHydrologyFactors.WaterUser1]);
          LDataSet.SetParams(['WaterUser2'], [LHydrologyFactors.WaterUser2]);
//          LDataSet.SetParams(['AFFFactors'], [LHydrologyFactors.AFFGrowthFactors]);
//          LDataSet.SetParams(['IRRFactors'], [LHydrologyFactors.IRRGrowthFactors]);
//          LDataSet.SetParams(['URBFactors'], [LHydrologyFactors.URBGrowthFactors]);
          LDataSet.ExecSQL;
          if (LHydrologyFactors <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LHydrologyFactors.Identifier);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelHydrology','AFFFactors',LWhereClause,
                                             LHydrologyFactors.AFFGrowthFactors);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelHydrology','IRRFactors',LWhereClause,
                                             LHydrologyFactors.IRRGrowthFactors);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelHydrology','URBFactors',LWhereClause,
                                             LHydrologyFactors.URBGrowthFactors);
          end;
          LDataSet.DataSet.Close;
        end;

        SetSaveDToDB(True);
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelGrowthFactors.SetSaveDToDB(ASaved: boolean);
const OPNAME = 'TExelGrowthFactors.SetSaveDToDB';
var
  LINdex: integer;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  try
    for LIndex := 0 to FDemandChannelGrowthFactors.Count -1 do
    begin
      LDemandChannelFactors := DemandChannelGrowthFactorsByIndex[LIndex];
      LDemandChannelFactors.FSavedToDB := ASaved;
    end;

    for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
    begin
      LMinMaxChannelFactors := MinMaxChannelGrowthFactorsByIndex[LIndex];
      LMinMaxChannelFactors.FSavedToDB := ASaved;
    end;


    for LIndex := 0 to FHydrologyGrowthFactors.Count -1 do
    begin
      LHydrologyFactors := HydrologyGrowthFactorsByIndex[LIndex];
      LHydrologyFactors.FSavedToDB := ASaved;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.TestGenerateFactors: boolean;
const OPNAME = 'TExelGrowthFactors.TestGenerateFactors';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GenerateGrowthFactors: boolean;
const OPNAME = 'TExelGrowthFactors.GenerateGrowthFactors';
var
  LAFFFactors,
  LIRRFactors,
  LURBFactors,
  LFactors                    : string;
  LIndex,
  LBaseDemandIndex,
  LBaseHydrologyIndex,
  LBaseMinMaxIndex,
  LStartDemandIndex,
  LStartMinMaxIndex,
  LStartHydrologyIndex        : integer;
  LGrowthFactors              : TGrowthFactors;
  LDemandCentreGrowthFactors  : TDemandCentreGrowthFactors;
  LMinMaxChannelGrowthFactors : TMinMaxChannelGrowthFactors;
  LHydrologyGrowthFactors     : THydrologyGrowthFactors;
  LExelDemandChannelFactors   : TExelDemandChannelGrowthFactors;
  LExelMinMaxChannelFactors   : TExelMinMaxChannelGrowthFactors;
  LExelHydrologyFactors       : TExelHydrologyGrowthFactors;
  LRunConfig                  : IRunConfigurationData;
begin
  Result := False;
  try
    if not TestGenerateFactors then Exit;
    if not SaveToDB  then Exit;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    LRunConfig          := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;

    Result := LGrowthFactors.ClearAllDataFromDB;

    if (FProjectionType = ptDemand) then
    begin
      LBaseDemandIndex     := FBaseYearIndex;
      LBaseMinMaxIndex     := FBaseYearIndex+1;
      LBaseHydrologyIndex  := FBaseYearIndex+1;
      LStartDemandIndex    := FStartYearIndex;
      LStartMinMaxIndex    := FStartYearIndex+1;
      LStartHydrologyIndex := FStartYearIndex+1;
    end
    else
    begin
      LBaseDemandIndex     := FBaseYearIndex-1;
      LBaseMinMaxIndex     := FBaseYearIndex;
      LBaseHydrologyIndex  := FBaseYearIndex;
      LStartDemandIndex    := FStartYearIndex-1;
      LStartMinMaxIndex    := FStartYearIndex;
      LStartHydrologyIndex := FStartYearIndex;
    end;

    for LIndex := 0 to FDemandChannelGrowthFactors.Count -1 do
    begin
      LExelDemandChannelFactors := DemandChannelGrowthFactorsByIndex[LIndex];
      LDemandCentreGrowthFactors  := LGrowthFactors.CreateDemandGrowthFactor;

      LFactors := LExelDemandChannelFactors.GenerateGrowthFactors(LBaseDemandIndex,LStartDemandIndex, FYearsCount,FDataStartYear);
      LDemandCentreGrowthFactors.Populate(LIndex+1,LExelDemandChannelFactors.ChannelNumber,YearsCount,LFactors,True);
    end;

    for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
    begin
      LExelMinMaxChannelFactors := MinMaxChannelGrowthFactorsByIndex[LIndex];
      LFactors := LExelMinMaxChannelFactors.GenerateGrowthFactors(LBaseMinMaxIndex,LStartMinMaxIndex,FYearsCount,FDataStartYear);

      LMinMaxChannelGrowthFactors  := LGrowthFactors.CreateMinMaxChannelGrowthFactor;
      LMinMaxChannelGrowthFactors.Populate(LIndex+1,LExelMinMaxChannelFactors.ChannelNumber,LExelMinMaxChannelFactors.ArcNumber,YearsCount,LFactors,True);
    end;

    for LIndex := 0 to FHydrologyGrowthFactors.Count -1 do
    begin
      LExelHydrologyFactors := HydrologyGrowthFactorsByIndex[LIndex];
      if LExelHydrologyFactors.BaseYearDemand then
        if LExelHydrologyFactors.GenerateGrowthFactors(LBaseHydrologyIndex,LStartHydrologyIndex,FYearsCount,FDataStartYear,LAFFFactors,LIRRFactors,LURBFactors) then
        begin
          LHydrologyGrowthFactors  := LGrowthFactors.CreateHydrologyGrowthFactor;
          LHydrologyGrowthFactors.Populate(LIndex+1,LExelHydrologyFactors.GaugeNumber,YearsCount,LAFFFactors,LIRRFactors,LURBFactors);
        end;
    end;


    LGrowthFactors.Populate(FYearsCount); //(LRunConfig.YearsInAnalysis);
    Result := Result and LGrowthFactors.SaveAllDataToDB;
    if Result then
     FAppModules.Model.StudyDataHasChanged(sdccAdd,'GrowthFactors','','');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelGrowthFactors.SetYearsCount(ACount: integer);
const OPNAME = 'TExelGrowthFactors.SetYearsCount';
var
  LIndex,
  LCount,
  LDef,
  LIndex2    : integer;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  try
    if(ACount <= 0) then Exit;
    LDef := ACount - FYearsCount;
    if(LDef = 0) then Exit;
    LCount := abs(LDef);
    for LIndex := 0 to FDemandChannelGrowthFactors.Count -1 do
    begin
      LDemandChannelFactors := DemandChannelGrowthFactorsByIndex[LIndex];
      if(LDef < 0) then
      begin
        for LIndex2 := 1 to LCount do
          LDemandChannelFactors.FGrowthFactors.Delete(LDemandChannelFactors.FGrowthFactors.Count-1);
      end
      else
      begin
        for LIndex2 := 1 to LCount do
          LDemandChannelFactors.FGrowthFactors.Add('0.00');
      end;
      LDemandChannelFactors.SetDemandGrowthFactors(LDemandChannelFactors.FGrowthFactors.CommaText);
    end;
    for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
    begin
      LMinMaxChannelFactors := MinMaxChannelGrowthFactorsByIndex[LIndex];
      if(LDef < 0) then
      begin
        for LIndex2 := 1 to LCount do
          LMinMaxChannelFactors.FGrowthFactors.Delete(LMinMaxChannelFactors.FGrowthFactors.Count-1);
      end
      else
      begin
        for LIndex2 := 1 to LCount do
          LMinMaxChannelFactors.FGrowthFactors.Add('0.00');
      end;
     LMinMaxChannelFactors.SetMinMaxGrowthFactors(LMinMaxChannelFactors.FGrowthFactors.CommaText);
    end;
    for LIndex := 0 to FHydrologyGrowthFactors.Count -1 do
    begin
      LHydrologyFactors := HydrologyGrowthFactorsByIndex[LIndex];
      if(LDef < 0) then
      begin
        for LIndex2 := 1 to LCount do
        begin
          LHydrologyFactors.FAFFGrowthFactors.Delete(LHydrologyFactors.FAFFGrowthFactors.Count-1);
          LHydrologyFactors.FIRRGrowthFactors.Delete(LHydrologyFactors.FIRRGrowthFactors.Count-1);
          LHydrologyFactors.FURBGrowthFactors.Delete(LHydrologyFactors.FURBGrowthFactors.Count-1);
        end;
      end
      else
      begin
        for LIndex2 := 1 to LCount do
        begin
          LHydrologyFactors.FAFFGrowthFactors.Add('0.00');
          LHydrologyFactors.FIRRGrowthFactors.Add('0.00');
          LHydrologyFactors.FURBGrowthFactors.Add('0.00');
        end;
      end;
      LHydrologyFactors.SetAFFGrowthFactors(LHydrologyFactors.AFFGrowthFactors);
      LHydrologyFactors.SetIRRGrowthFactors(LHydrologyFactors.IRRGrowthFactors);
      LHydrologyFactors.SetURBGrowthFactors(LHydrologyFactors.URBGrowthFactors);
    end;
    SaveYearsCountToDB(ACount)
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetYearsCount : integer;
const OPNAME = 'TExelGrowthFactors.GetYearsCount';
begin
  Result := 0;
  try
    Result := FYearsCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelGrowthFactors.SetStartYear(AValue: integer);
const OPNAME = 'TExelGrowthFactors.SetStartYear';
var
  LOldValue    : string;
begin
  try
    FStartYear := AValue;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'GrowthProjectionStartYear',LOldValue,IntToStr(AValue));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetStartYear: integer;
const OPNAME = 'TExelGrowthFactors.GetStartYear';
begin
  Result := 0;
  try
    Result := FStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelGrowthFactors.SetBaseYear(AValue: integer);
const OPNAME = 'TExelGrowthFactors.SetBaseYear';
var
  LOldValue    : string;
begin
  try
    FBaseYear := AValue;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'GrowthProjectionBaseYear',LOldValue,IntToStr(AValue));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetBaseYear: integer;
const OPNAME = 'TExelGrowthFactors.GetBaseYear';
begin
  Result := 0;
  try
    Result := FBaseYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TExelGrowthFactors.SaveYearsCountToDB(AYearsCount: integer);
const OPNAME = 'TExelGrowthFactors.SaveYearsCountToDB';
var
  LOldValue    : string;
begin
  try
    FYearsCount := AYearsCount;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'GrowthProjectionYearsCount',LOldValue,IntToStr(AYearsCount));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TExelGrowthFactors.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TExelGrowthFactors.Validate';
var
  LIndex                 : integer;
  LDemandProjections     : TExelDemandChannelGrowthFactors;
  LMinMaxProjections     : TExelMinMaxChannelGrowthFactors;
  LHydrologyProjections  : TExelHydrologyGrowthFactors;
begin
  Result := false;
  try
    Result := True;
    if AContext = '' then
    begin
      for LIndex := 0 to FDemandChannelGrowthFactors.Count -1 do
      begin
        LDemandProjections := DemandChannelGrowthFactorsByIndex[LIndex];
        if not LDemandProjections.Validate(AErrors) then
          Result := False;
      end;

      for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
      begin
        LMinMaxProjections := MinMaxChannelGrowthFactorsByIndex[LIndex];
        if not LMinMaxProjections.Validate(AErrors) then
          Result := False;
      end;

      for LIndex := 0 to FHydrologyGrowthFactors.Count -1 do
      begin
        LHydrologyProjections := HydrologyGrowthFactorsByIndex[LIndex];
        if not LHydrologyProjections.Validate(AErrors) then
          Result := False;
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetDemandChannelGrowthFactorsByIdentifier(AIdentifier: integer): TExelDemandChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetDemandChannelGrowthFactorsByIdentifier';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDemandChannelGrowthFactors.Count -1 do
    begin
      Result := DemandChannelGrowthFactorsByIndex[LIndex];
      if (Result.FIdentifier = AIdentifier) then Exit;
    end;
    Result := nil;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetMinMaxChannelGrowthFactorsByIdentifier(AIdentifier: integer): TExelMinMaxChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetMinMaxChannelGrowthFactorsByIdentifier';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
    begin
      Result := MinMaxChannelGrowthFactorsByIndex[LIndex];
      if (Result.FIdentifier = AIdentifier) then Exit;
    end;
    Result := nil;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.CastHydrologyGrowthFactorByGaugeNumber(AGaugeNumber: integer): TExelHydrologyGrowthFactors;
const OPNAME = 'TExelGrowthFactors.CastHydrologyGrowthFactorByGaugeNumber';
var
  LIndex    : integer;
  LHydrologyGrowthFactors : TExelHydrologyGrowthFactors;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (lIndex < FHydrologyGrowthFactors.Count)) do
    begin
      LHydrologyGrowthFactors := TExelHydrologyGrowthFactors(FHydrologyGrowthFactors.Items[lIndex]);
      if (LHydrologyGrowthFactors.FGaugeNumber = AGaugeNumber) then
        Result := LHydrologyGrowthFactors
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TExelGrowthFactors.GetHydrologyGrowthFactorsByIdentifier(AIdentifier: integer): TExelHydrologyGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetHydrologyGrowthFactorsByIdentifier';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FHydrologyGrowthFactors.Count -1 do
    begin
      Result := HydrologyGrowthFactorsByIndex[LIndex];
      if (Result.FIdentifier = AIdentifier) then Exit;
    end;
    Result := nil;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.CreateDemandGrowthProjection: TExelDemandChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.CreateDemandGrowthProjection';
var
  LDemandCentreGrowthProjections : TExelDemandChannelGrowthFactors;
begin
  Result := nil;
  try
    LDemandCentreGrowthProjections := TExelDemandChannelGrowthFactors.Create(FAppModules);
    FDemandChannelGrowthFactors.Add(LDemandCentreGrowthProjections);
    Result := LDemandCentreGrowthProjections;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TExelGrowthFactors.CreateHydrologyGrowthProjection: TExelHydrologyGrowthFactors;
const OPNAME = 'TExelGrowthFactors.CreateHydrologyGrowthProjection';
var
  LHydrologyGrowthProjections : TExelHydrologyGrowthFactors;
begin
  Result := nil;
  try
    LHydrologyGrowthProjections := TExelHydrologyGrowthFactors.Create(FAppModules);
    FHydrologyGrowthFactors.Add(LHydrologyGrowthProjections);
    Result := LHydrologyGrowthProjections;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TExelGrowthFactors.CreateMinMaxChannelGrowthProjection: TExelMinMaxChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.CreateMinMaxChannelGrowthProjection';
var
  LMinMaxGrowthProjections : TExelMinMaxChannelGrowthFactors;
begin
  Result := nil;
  try
    LMinMaxGrowthProjections := TExelMinMaxChannelGrowthFactors.Create(FAppModules);
    FMinMaxChannelGrowthFactors.Add(LMinMaxGrowthProjections);
    Result := LMinMaxGrowthProjections;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TExelGrowthFactors.ClearAllProjectionDataFromDB: boolean;
const OPNAME = 'TExelGrowthFactors.ClearAllProjectionDataFromDB';
begin
  Result := False;
  try
    FYearsCount := 0;
    FDemandChannelGrowthFactors.Clear;
    FMinMaxChannelGrowthFactors.Clear;
    FHydrologyGrowthFactors.Clear;

    ClearDemandGrowthProjectionsDB;
    ClearMinMaxChannelGrowthProjectionsDB;
    ClearHydrologyGrowthProjectionsDB;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.ClearDemandGrowthProjectionsDB: boolean;
const OPNAME = 'TExelGrowthFactors.ClearDemandGrowthProjectionsDB';
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

function TExelGrowthFactors.ClearHydrologyGrowthProjectionsDB: boolean;
const OPNAME = 'TExelGrowthFactors.ClearHydrologyGrowthProjectionsDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.DeleteAllGrowthProjectionHydrology;
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.ClearMinMaxChannelGrowthProjectionsDB: boolean;
const OPNAME = 'TExelGrowthFactors.ClearMinMaxChannelGrowthProjectionsDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.DeleteAllGrowthProjectionMinMax;
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.SaveAllProjectionDataToDB: boolean;
const OPNAME = 'TExelGrowthFactors.SaveAllProjectionDataToDB';
begin
  Result := False;
  try
    AddDemandGrowthProjectionsToDB;
    AddMinMaxChannelGrowthProjectionsToDB;
    AddHydrologyGrowthProjectionsToDB;
    AddGrowthProjectionConfigurationToDB;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.AddDemandGrowthProjectionsToDB: boolean;
const OPNAME = 'TExelGrowthFactors.AddDemandGrowthProjectionsToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddDemandGrowthProjectionsToDB(FDemandChannelGrowthFactors);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.AddHydrologyGrowthProjectionsToDB: boolean;
const OPNAME = 'TExelGrowthFactors.AddHydrologyGrowthProjectionsToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddHydrologyGrowthProjectionsToDB(FHydrologyGrowthFactors);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TExelGrowthFactors.AddMinMaxChannelGrowthProjectionsToDB: boolean;
const OPNAME = 'TExelGrowthFactors.AddMinMaxChannelGrowthProjectionsToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddMinMaxChannelGrowthProjectionsToDB(FMinMaxChannelGrowthFactors);
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TExelGrowthFactors.AddGrowthProjectionConfigurationToDB: Boolean;
const OPNAME = 'TExelGrowthFactors.AddGrowthProjectionConfigurationToDB';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.AddGrowthProjectionConfDataToDB(BaseYear, StartYear, YearsCount,DataStartYear) ;
      end;
    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetDemandChannelGrowthFactorsByChannel(AChannelNumber: integer): TExelDemandChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetDemandChannelGrowthFactorsByChannel';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDemandChannelGrowthFactors.Count -1 do
    begin
      if (TExelDemandChannelGrowthFactors(FDemandChannelGrowthFactors.Items[LIndex]).FChannelNumber = AChannelNumber) then
      begin
        Result := TExelDemandChannelGrowthFactors(FDemandChannelGrowthFactors.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TExelGrowthFactors.GetExelMinMaxChannelGrowthFactorsByCannelArc(AArc, AChannel: integer): TExelMinMaxChannelGrowthFactors;
const OPNAME = 'TExelGrowthFactors.GetExelMinMaxChannelGrowthFactorsByCannelArc';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
    begin
      if (TExelMinMaxChannelGrowthFactors(FMinMaxChannelGrowthFactors.Items[LIndex]).FChannelNumber = AChannel) and
        (TExelMinMaxChannelGrowthFactors(FMinMaxChannelGrowthFactors.Items[LIndex]).FArcNumber = AArc) then
      begin
        Result := TExelMinMaxChannelGrowthFactors(FMinMaxChannelGrowthFactors.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TExelGrowthFactors.RemoveMinMaxChannel(AMinMaxChannel : integer) : boolean;
const OPNAME = 'TExelGrowthFactors.GetExelMinMaxChannelGrowthFactorsByCannelArc';
var
  lSQLAgent: TGrowthFactorDataSQLAgent;
  LIndex : integer;
begin
  Result := FALSE;
  try
    lSQLAgent := TGrowthFactorDataSQLAgent.Create(FAppModules);
    try
      if Assigned(lSQLAgent) then
      begin
        Result := lSQLAgent.DeleteProjectionMinMaxChannel(AMinMaxChannel) ;
      end;
      for LIndex := 0 to FMinMaxChannelGrowthFactors.Count -1 do
      begin
        if (TExelMinMaxChannelGrowthFactors(FMinMaxChannelGrowthFactors.Items[LIndex]).FChannelNumber = AMinMaxChannel) then
           FMinMaxChannelGrowthFactors.Remove(FMinMaxChannelGrowthFactors.Items[lIndex]);
       end;


    finally
      lSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
