{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentre                            *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/11/02                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentre;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB,
  UConstants,
  VCL.Dialogs,
  UChannelDataSQLAgent;

type
{******************************************************************************}
{* Yield Model Demand Centre Return Flow Feature                              *}
{******************************************************************************}

  TYMDemandCentreReturnFlowFeature = class(TAbstractAppObject, IYMDemandCentreReturnFlowFeature)
  protected
    FFeatureID        : integer;
    FChannelNr        : integer;
    FDemandCentreID   : Integer;
    FTotalReturnFlow  : Double;
    FFlowDiversion    : Double;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateChannelNr(AErrorMessages: TStrings):       Boolean;
    function ValidateTotalReturnFlow(AErrorMessages: TStrings): Boolean;
    function ValidateFlowDiversion(AErrorMessages: TStrings):   Boolean;

  public
    procedure Assign(ASource: TYMDemandCentreReturnFlowFeature);
    function Initialise : boolean; override;
    //function Assign(AReturnFlow: TYMDemandCentreReturnFlowFeature) : Boolean;
    function PopulateIDs(AIdentifier: integer): boolean;
    function Populate (AFeatureID           : integer;
                       AChannelNr           : integer;
                       ADemandCentreID      : integer;
                       ATotalReturnFlow     : double;
                       AFlowDiversion       : double): WordBool;

    function Get_FeatureID        : integer; safecall;
    function Get_Channel          : IGeneralFlowChannel; safecall;
    function Get_DemandCentreID   : Integer; safecall;
    function Get_ChannelNr        : Integer; safecall;
    function Get_TotalReturnFlow  : Double;  safecall;
    function Get_FlowDiversion    : Double;  safecall;

    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    procedure Set_DemandCentreID(Value : Integer);  safecall;
    procedure Set_ChannelNr(Value : Integer);       safecall;
    procedure Set_TotalReturnFlow(Value : Double);  safecall;
    procedure Set_FlowDiversion(Value : Double);    safecall;

    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;

    property FeatureID        : integer read Get_FeatureID;
    property Channel          : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property DemandCentreID   : Integer  read Get_DemandCentreID  write Set_DemandCentreID;
    property ChannelNr        : Integer  read Get_ChannelNr       write Set_ChannelNr;
    property TotalReturnFlow  : Double   read Get_TotalReturnFlow write Set_TotalReturnFlow;
    property FlowDiversion    : Double   read Get_FlowDiversion   write Set_FlowDiversion;
  end;

  TYMDemandCentreReturnFlowFeatureList = class(TAbstractAppObject, IYMDemandCentreReturnFlowFeatureList)
  protected
    FReturnFlowList : TObjectList;

    function _AddRef: Integer;      stdcall;
    function _Release: Integer;     stdcall;
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
    function AddReturnFlowFeature (AFeature : TYMDemandCentreReturnFlowFeature): boolean;
  public
    //function Assign(AReturnFlowList: TYMDemandCentreReturnFlowFeatureList) : Boolean;
    function Initialise                                           : Boolean; override;
    function NewReturnFlowFeature                                 : TYMDemandCentreReturnFlowFeature;
    function CreateNewReturnFlowFeature(ADemandCentreID: Integer) : TYMDemandCentreReturnFlowFeature;
    function DeleteReturnFlowFeatureWithChannelNr(AChannelNr: integer): WordBool;
    function DeleteReturnFlowFeatureWithIndex(AIndex : integer)   : WordBool;
    function CastReturnFlowFeatureByID(AFeatureID : integer)      : TYMDemandCentreReturnFlowFeature;
    function CastReturnFlowFeatureByIndex(AIndex : integer)       : TYMDemandCentreReturnFlowFeature;
    function CastReturnFlowFeatureByChannelNr(AChannelNr : integer)      : TYMDemandCentreReturnFlowFeature;

    function CreateReturnFlowFeature(ADemandCentreID: Integer)                     : IYMDemandCentreReturnFlowFeature; safecall;
    function RemoveReturnFlowFeatureWithNr (ADemandCentreID, AChannelNr : integer) : WordBool; safecall;
    function Get_ReturnFlowFeatureByIndex(AIndex: integer)        : IYMDemandCentreReturnFlowFeature; safecall;
    function Get_ReturnFlowFeatureByID(AFeatureID : integer)      : IYMDemandCentreReturnFlowFeature; safecall;
    function Get_ReturnFlowFeatureCount                           : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property ReturnFlowFeatureByIndex[AIndex : integer] : IYMDemandCentreReturnFlowFeature read Get_ReturnFlowFeatureByIndex;
    property ReturnFlowFeatureByID[AFeatureID: integer] : IYMDemandCentreReturnFlowFeature read Get_ReturnFlowFeatureByID;
    property ReturnFlowFeatureCount                     : Integer read Get_ReturnFlowFeatureCount;
  end;

  TYMDemandCentre = class(TAbstractAppObject, IYMDemandCentre)
  protected
    FIdentifier             : Integer;
    FNodeNumber             : Integer;
    FName                   : string;
    FDescription            : string;
    FNodeRefNr              : Integer;
    FAveReturnFlowFactor    : Double;
    FAveEvaporation         : Double;
    FStdDeviationFactor     : Double;
    FRoutingConstant        : Double;
    FRainfallScalingFactor  : Double;
    FTotalFlowLost          : Double;
    FEvapoTranspiration     : array[MinMonths..MaxMonths] of Double;
    FConsumptiveUseChannelNr  : Integer;
    FReclaimationChannelNr    : Integer;
    FReturnFlowList           : TYMDemandCentreReturnFlowFeatureList;
    FSupplyChannelNrs         : Widestring;

    function Get_AveEvaporation                     : Double;  safecall;
    function Get_AveReturnFlowFactor                : Double;  safecall;
    function Get_Description                        : Widestring;  safecall;
    function Get_EvapoTranspiration(AMonth: Integer): Double;  safecall;
    function Get_Identifier                         : Integer; safecall;
    function Get_Name                               : Widestring;  safecall;
    function Get_NodeNumber                         : Integer; safecall;
    function Get_NodeRefNr                          : Integer; safecall;
    function Get_RainfallScalingFactor              : Double;  safecall;
    function Get_RoutingConstant                    : Double;  safecall;
    function Get_StdDeviationFactor                 : Double;  safecall;
    function Get_TotalFlowLost                      : Double;  safecall;
    function Get_ConsumptiveUseChannel              : IGeneralFlowChannel; safecall;
    function Get_ReclaimationChannel                : IGeneralFlowChannel; safecall;
    function Get_ConsumptiveUseChannelNr            : Integer; safecall;
    function Get_ReclaimationChannelNr              : Integer; safecall;
    function Get_ReclaimationPlantExists            : WordBool; safecall;
    function Get_ReturnFlowFeatureList              : IYMDemandCentreReturnFlowFeatureList; safecall;
    function Get_SupplyChannelNrs                   : WideString; safecall;
    function Get_SupplyChannelByIndex(AIndex: integer): IGeneralFlowChannel; safecall;
    function Get_SupplyChannelCount                 : Integer; safecall;

    procedure Set_Identifier(Value: Integer);             safecall;
    procedure Set_AveEvaporation( Value: Double);         safecall;
    procedure Set_AveReturnFlowFactor( Value: Double);    safecall;
    procedure Set_Description(const Value: Widestring);   safecall;
    procedure Set_EvapoTranspiration(AMonth: Integer;  Value: Double); safecall;
    procedure Set_Name(const Value: Widestring);          safecall;
    procedure Set_NodeRefNr( Value: Integer);             safecall;
    procedure Set_RainfallScalingFactor( Value: Double);  safecall;
    procedure Set_RoutingConstant( Value: Double);        safecall;
    procedure Set_StdDeviationFactor(Value: Double);      safecall;
    procedure Set_TotalFlowLost( Value: Double);          safecall;
    procedure Set_NodeNumber( Value: Integer);            safecall;
    procedure Set_ConsumptiveUseChannelNr(Value: Integer); safecall;
    procedure Set_ReclaimationChannelNr(Value: Integer);  safecall;
    procedure Set_ReclaimationPlantExists(Value: WordBool); safecall;
    //procedure Set_ReturnFlowFeatureList(const Value: IYMDemandCentreReturnFlowFeatureList); safecall;

    function ValidateNodeNumber(AErrorMessages: TStrings)             : Boolean;
    function ValidateName(AErrorMessages: TStrings)                   : Boolean;
    function ValidateDescription(AErrorMessages: TStrings)            : Boolean;
    function ValidateNodeRefNr(AErrorMessages: TStrings)              : Boolean;
    function ValidateAveReturnFlowFactor(AErrorMessages: TStrings)    : Boolean;
    function ValidateAveEvaporation(AErrorMessages: TStrings)         : Boolean;
    function ValidateStdDeviationFactor(AErrorMessages: TStrings)     : Boolean;
    function ValidateRoutingConstant(AErrorMessages: TStrings)        : Boolean;
    function ValidateRainfallScalingFactor(AErrorMessages: TStrings)  : Boolean;
    function ValidateTotalFlowLost(AErrorMessages: TStrings)          : Boolean;
    function ValidateEvapoTranspiration(AErrorMessages: TStrings; AErrorColumns  : TStringList) : Boolean;
    function ValidateConsumptiveChannel(AErrorMessages : TStrings)    : Boolean;
    function ValidateReturnFlowChannel(AErrorMessages : TStrings)    : Boolean;
    function ValidateReclaimationChannel(AErrorMessages : TStrings)   : Boolean;

    function _AddRef: Integer;      stdcall;
    function _Release: Integer;     stdcall;
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(ADemandCentre: TYMDemandCentre);
    function Initialise : Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CastReturnFlowList : TYMDemandCentreReturnFlowFeatureList;
    function DeleteAllSupplyChannels: boolean;
    function DeleteAllReturnFlowChannels: boolean;
    function DeleteConsumptiveChannel: boolean;
    function PopulateIDs(AIdentifier          : Integer;
                      AName                   : string) : Boolean;
    function Populate(AIdentifier             : Integer;
                      ANodeNumber             : Integer;
                      AName                   : string;
                      ANodeRefNr              : Integer;
                      AAveReturnFlowFactor    : Double;
                      AAveEvaporation         : Double;
                      AStdDeviationFactor     : Double;
                      ARoutingConstant        : Double;
                      ARainfallScalingFactor  : Double;
                      ATotalFlowLost          : Double;
                      ADescription            : string;
                      AEvapoTranspiration     : TMonthlyDoubleArray;
                      AConsumptiveUseChannelNr  : Integer;
                      AReclaimationChannelNr    : Integer) : Boolean;

    property Identifier             : Integer  read Get_Identifier            write Set_Identifier;
    property NodeNumber             : Integer  read Get_NodeNumber            write Set_NodeNumber;
    property Name                   : Widestring   read Get_Name                  write Set_Name;
    property Description            : Widestring   read Get_Description           write Set_Description;
    property NodeRefNr              : Integer  read Get_NodeRefNr             write Set_NodeRefNr;
    property AveReturnFlowFactor    : Double   read Get_AveReturnFlowFactor   write Set_AveReturnFlowFactor;
    property AveEvaporation         : Double   read Get_AveEvaporation        write Set_AveEvaporation;
    property StdDeviationFactor     : Double   read Get_StdDeviationFactor    write Set_StdDeviationFactor;
    property RoutingConstant        : Double   read Get_RoutingConstant       write Set_RoutingConstant;
    property RainfallScalingFactor  : Double   read Get_RainfallScalingFactor write Set_RainfallScalingFactor;
    property TotalFlowLost          : Double   read Get_TotalFlowLost         write Set_TotalFlowLost;
    property EvapoTranspiration[AMonth: Integer]  : Double   read Get_EvapoTranspiration write Set_EvapoTranspiration;

    property ConsumptiveUseChannel  : IGeneralFlowChannel read Get_ConsumptiveUseChannel;
    property ReclaimationChannel    : IGeneralFlowChannel read Get_ReclaimationChannel;

    property ConsumptiveUseChannelNr  : Integer read Get_ConsumptiveUseChannelNr write Set_ConsumptiveUseChannelNr;
    property ReclaimationChannelNr    : Integer read Get_ReclaimationChannelNr write Set_ReclaimationChannelNr;
    property ReclaimationPlantExists  : WordBool read Get_ReclaimationPlantExists write Set_ReclaimationPlantExists;
    property ReturnFlowFeatureList    : IYMDemandCentreReturnFlowFeatureList read Get_ReturnFlowFeatureList;// write Set_ReturnFlowFeatureList;
    property SupplyChannelNrs         : Widestring read Get_SupplyChannelNrs;
    property SupplyChannelByIndex[AIndex : Integer] : IGeneralFlowChannel read Get_SupplyChannelByIndex;
    property SupplyChannelCount       : Integer read Get_SupplyChannelCount;
  end;

  TYMDemandCentreList = class(TAbstractAppObject, IYMDemandCentreList)
  protected
    FYMDemandCentreList  : TObjectList;
    FYMDemandCentreCount : Integer;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateYMDemandCentreCount(AErrorMessages : TStrings) : Boolean;
    function AddYMDemandCentre(AFeature : TYMDemandCentre): boolean;

    function CreateNewYMDemandCentre : TYMDemandCentre;

    function DeleteYMDemandCentreWithID(ADemandCentreID : integer) : WordBool;
    function DeleteYMDemandCentreWithIndex(AIndex : integer) : WordBool;
    function Get_YMDemandCentreCount: Integer; safecall;
    function GetConsumptiveChannelID(AYMDemandCentreID : integer): integer;

    function Get_YMDemandCentreByNodeNumber(ANodeNumber: Integer): IYMDemandCentre; safecall;
    function Get_YMDemandCentreByID(ADemandCentreID: integer): IYMDemandCentre; safecall;
    function Get_YMDemandCentreByIndex(AIndex: integer): IYMDemandCentre; safecall;
  public
    property YMDemandCentreCount : integer read Get_YMDemandCentreCount;
    function Initialise : boolean; override;

    function NewYMDemandCentre : TYMDemandCentre;
    function CreateYMDemandCentre : IYMDemandCentre; safecall;
    function CopyCreate(ANodeNumber: integer) : IYMDemandCentre; safecall;
    function RemoveYMDemandCentre (ANodeNumber: integer) : WordBool; safecall;

    function CastYMDemandCentreByID(AYMDemandCentreID: integer): TYMDemandCentre;
    function CastYMDemandCentreByIndex(AIndex : integer): TYMDemandCentre;
    function CastYMDemandCentreByNodeNumber(ANodeNumber: Integer): TYMDemandCentre;

    property YMDemandCentreByIndex[AIndex : Integer]                      : IYMDemandCentre read Get_YMDemandCentreByIndex;
    property YMDemandCentreByID[AYMDemandCentreID : Integer]              : IYMDemandCentre read Get_YMDemandCentreByID;
    property YMDemandCentreByNodeNumber[ANodeNumber : Integer]  : IYMDemandCentre read Get_YMDemandCentreByNodeNumber;
    function YMDemandCentreByName(const AName: WideString)                : IYMDemandCentre; safecall;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
  end;



implementation

uses
  SysUtils,
  Math,
  UChannelData,
  USystemModelManager,
  UYieldModelDataObject,
  UNetworkElementData,
  UYMDemandCentreSQLAgent,
  UNetworkFeaturesSQLAgent,
  UErrorHandlingOperations;

{ TYMDemandCentre }

function TYMDemandCentre._AddRef: Integer;
const OPNAME = 'TYMDemandCentre._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre._Release: Integer;
const OPNAME = 'TYMDemandCentre._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.CreateMemberObjects;
const OPNAME = 'TYMDemandCentre.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReturnFlowList := TYMDemandCentreReturnFlowFeatureList.Create(FAppModules);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentre.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReturnFlowList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Initialise: Boolean;
const OPNAME = 'TYMDemandCentre.Initialise';
var
  LCount : Integer;
begin
  Result := inherited Initialise;
  try
    FIdentifier             := NullInteger;
    FNodeNumber             := 0;
    FName                   := '';
    FDescription            := '';
    FSupplyChannelNrs       := '';
    FNodeRefNr              := 0;
    FAveReturnFlowFactor    := 0.0;
    FAveEvaporation         := 0.0;
    FStdDeviationFactor     := 0.0;
    FRoutingConstant        := 0.0;
    FRainfallScalingFactor  := 0.0;
    FTotalFlowLost          := 0.0;
    for LCount := MinMonths to MaxMonths do
      FEvapoTranspiration[LCount] := 0.0;
    FConsumptiveUseChannelNr  := 0;
    FReclaimationChannelNr    := 0;

    FReturnFlowList.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TYMDemandCentre.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
  lStopOnFirstError : Boolean;  
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'YMDemandCentreNodeNumber') then
        Result := ValidateNodeNumber(lErrorMessage)
      else
      if (AContext = 'YMDemandCentreName') then
        Result := ValidateName(lErrorMessage)
      else
      if (AContext = 'YMDemandCentreDescription') then
        Result := ValidateDescription(lErrorMessage)
      else
      if (AContext = 'NodeRefNr') then
        Result := ValidateNodeRefNr(lErrorMessage)
      else
      if (AContext = 'AveReturnFlowFactor') then
        Result := ValidateAveReturnFlowFactor(lErrorMessage)
      else
      if (AContext = 'AveEvaporation') then
        Result := ValidateAveEvaporation(lErrorMessage)
      else
      if (AContext = 'StdDeviationFactor') then
        Result := ValidateStdDeviationFactor(lErrorMessage)
      else
      if (AContext = 'YMDemandCentreRoutingConstant') then
        Result := ValidateRoutingConstant(lErrorMessage)
      else
      if (AContext = 'RainfallScalingFactor') then
        Result := ValidateRainfallScalingFactor(lErrorMessage)
      else
      if (AContext = 'TotalFlowLost') then
        Result := ValidateTotalFlowLost(lErrorMessage)
      else
      if (AContext = 'DemandCentreConsumptiveChannelNumber') then
        Result := ValidateConsumptiveChannel(lErrorMessage)
      else
      if (AContext = 'DemandCentreReclaimationChannelNumber') then
        Result := ValidateReclaimationChannel(lErrorMessage)
      else
      if (AContext = 'EvapoTranspiration') then
      begin
        Result := ValidateEvapoTranspiration(LErrorMessage,LErrorCols);
        if (not Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMessage.Text
          else
            AErrors := AErrors + CTStringsSeparator + LErrorMessage.Text + CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
        end;
      end else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateNodeNumber(lErrorMessage))            then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateName(lErrorMessage))                  then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateDescription(lErrorMessage))           then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateNodeRefNr(lErrorMessage))             then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateAveReturnFlowFactor(lErrorMessage))   then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateAveEvaporation(lErrorMessage))        then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateStdDeviationFactor(lErrorMessage))    then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateRoutingConstant(lErrorMessage))       then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateRainfallScalingFactor(lErrorMessage)) then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateTotalFlowLost(lErrorMessage))         then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateConsumptiveChannel(lErrorMessage))    then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateReturnFlowChannel(lErrorMessage))    then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateReclaimationChannel(lErrorMessage)) then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateEvapoTranspiration(lErrorMessage, LErrorCols))  then Result := False;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Assign(ADemandCentre: TYMDemandCentre);
const OPNAME = 'TYMDemandCentre.Assign';
var
  LReturnFlowFeature : TYMDemandCentreReturnFlowFeature;
  LCount : Integer;
  LChannelList : IChannelList;
  LChnnelPenalty : IChannelPenalty;
  LDestConsumptive : IGeneralFlowChannel;
  LSourceConsumptive : IGeneralFlowChannel;
begin
  try
    Name                   := 'Copy of '+ADemandCentre.Name;
    Description            := ADemandCentre.Description;
    NodeRefNr              := ADemandCentre.NodeRefNr;
    AveReturnFlowFactor    := ADemandCentre.AveReturnFlowFactor;
    AveEvaporation         := ADemandCentre.AveEvaporation;
    StdDeviationFactor     := ADemandCentre.StdDeviationFactor;
    RoutingConstant        := ADemandCentre.RoutingConstant;
    RainfallScalingFactor  := ADemandCentre.RainfallScalingFactor;
    TotalFlowLost          := ADemandCentre.TotalFlowLost;
    ReclaimationChannelNr  := ADemandCentre.ReclaimationChannelNr;
    ReclaimationPlantExists := ADemandCentre.ReclaimationPlantExists;
    for LCount := MinMonths to MaxMonths do
      EvapoTranspiration[LCount] := ADemandCentre.EvapoTranspiration[LCount];
    if (ADemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount > 0) then
    begin
      for LCount := 0 to ADemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount-1 do
      begin
        if ADemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[LCount] <> nil then
        begin
          ReturnFlowFeatureList.CreateReturnFlowFeature(FIdentifier);
          LReturnFlowFeature := CastReturnFlowList.CastReturnFlowFeatureByIndex(LCount);
          LReturnFlowFeature.Assign(ADemandCentre.CastReturnFlowList.CastReturnFlowFeatureByIndex(LCount));
        end;
      end;
    end;
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    if LChannelList <> nil then
    begin
      LDestConsumptive := LChannelList.ChannelByChannelNumber[FConsumptiveUseChannelNr];
      LSourceConsumptive := LChannelList.ChannelByChannelNumber[ADemandCentre.ConsumptiveUseChannelNr];
      if (LDestConsumptive <> nil) and (LSourceConsumptive <> nil) then
      begin
        LDestConsumptive.ChannelName := 'Copy of '+ LSourceConsumptive.ChannelName;
        LDestConsumptive.ChannelType := LSourceConsumptive.ChannelType;
        LDestConsumptive.ChannelSubType := LSourceConsumptive.ChannelSubType;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceConsumptive.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestConsumptive.ChannelPenalty := LChnnelPenalty;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Populate(AIdentifier,    ANodeNumber: Integer;
                                  AName: string;  ANodeRefNr: Integer; AAveReturnFlowFactor,
                                  AAveEvaporation, AStdDeviationFactor, ARoutingConstant,
                                  ARainfallScalingFactor, ATotalFlowLost: Double; ADescription: string;
                                  AEvapoTranspiration: TMonthlyDoubleArray;
                                  AConsumptiveUseChannelNr  : Integer;
                                  AReclaimationChannelNr    : Integer): Boolean;
const OPNAME = 'TYMDemandCentre.Populate';
var
  LCount : Integer;
begin
  Result := False;
  try
    FIdentifier             := AIdentifier;
    FNodeNumber             := ANodeNumber;
    FName                   := AName;
    FDescription            := ADescription;
    FNodeRefNr              := ANodeRefNr;
    FAveReturnFlowFactor    := AAveReturnFlowFactor;
    FAveEvaporation         := AAveEvaporation;
    FStdDeviationFactor     := AStdDeviationFactor;
    FRoutingConstant        := ARoutingConstant;
    FRainfallScalingFactor  := ARainfallScalingFactor;
    FTotalFlowLost          := ATotalFlowLost;
    for LCount := MinMonths to MaxMonths do
      FEvapoTranspiration[LCount] := AEvapoTranspiration[LCount];
      
    FConsumptiveUseChannelNr  := AConsumptiveUseChannelNr;
    FReclaimationChannelNr    := AReclaimationChannelNr;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_AveEvaporation: Double;
const OPNAME = 'TYMDemandCentre.Get_AveEvaporation';
begin
  try
    Result := FAveEvaporation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_AveReturnFlowFactor: Double;
const OPNAME = 'TYMDemandCentre.Get_AveReturnFlowFactor';
begin
  try
    Result := FAveReturnFlowFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_Description: Widestring;
const OPNAME = 'TYMDemandCentre.Get_Description';
begin
  try
    Result := FDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_EvapoTranspiration(AMonth: Integer): Double;
const OPNAME = 'TYMDemandCentre.Get_EvapoTranspiration';
begin
  try
    Result := FEvapoTranspiration[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_Identifier: Integer;
const OPNAME = 'TYMDemandCentre.Get_Identifier';
begin
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_Name: Widestring;
const OPNAME = 'TYMDemandCentre.Get_Name';
begin
  try
    Result := FName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_NodeNumber: Integer;
const OPNAME = 'TYMDemandCentre.Get_NodeNumber';
begin
  try
    Result := FNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_NodeRefNr: Integer;
const OPNAME = 'TYMDemandCentre.Get_NodeRefNr';
begin
  try
    Result := FNodeRefNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_RainfallScalingFactor: Double;
const OPNAME = 'TYMDemandCentre.Get_RainfallScalingFactor';
begin
  try
    Result := FRainfallScalingFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_RoutingConstant: Double;
const OPNAME = 'TYMDemandCentre.Get_RoutingConstant';
begin
  try
    Result := FRoutingConstant;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_StdDeviationFactor: Double;
const OPNAME = 'TYMDemandCentre.Get_StdDeviationFactor';
begin
  try
    Result := FStdDeviationFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_TotalFlowLost: Double;
const OPNAME = 'TYMDemandCentre.Get_TotalFlowLost';
begin
  try
    Result := FTotalFlowLost;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_AveEvaporation(Value: Double);
const OPNAME = 'TYMDemandCentre.Set_AveEvaporation';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FAveEvaporation <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FAveEvaporation);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('AveEvaporation', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FAveEvaporation := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AveEvaporation', LOldValue, FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_AveReturnFlowFactor(Value: Double);
const OPNAME = 'TYMDemandCentre.Set_AveReturnFlowFactor';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FAveReturnFlowFactor <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FAveReturnFlowFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('AveReturnFlowFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FAveReturnFlowFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AveReturnFlowFactor', LOldValue, FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_Description(const Value: Widestring);
const OPNAME = 'TYMDemandCentre.Set_Description';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FDescription <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue   := FDescription;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('YMDemandCentreDescription', Value, LOldValue, LContextData ) then
        begin
          FDescription := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'YMDemandCentreDescription', LOldValue, Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_EvapoTranspiration(AMonth: Integer; Value: Double);
const OPNAME = 'TYMDemandCentre.Set_EvapoTranspiration';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FEvapoTranspiration[AMonth] <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FEvapoTranspiration[AMonth]);
        LLoadAgent.LoadContextData_EvapoTranspiration(LContextData, IntToStr(FIdentifier), IntToStr(AMonth));
        if FAppModules.FieldProperties.UpdateFieldValue('EvapoTranspiration', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FEvapoTranspiration[AMonth] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'EvapoTranspiration',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_Name(const Value: Widestring);
const OPNAME = 'TYMDemandCentre.Set_Name';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FName <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue   := FName;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('YMDemandCentreName', Value, LOldValue, LContextData ) then
        begin
          FName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'YMDemandCentreName', LOldValue, Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_NodeRefNr(Value: Integer);
const OPNAME = 'TYMDemandCentre.Set_NodeRefNr';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FNodeRefNr <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FNodeRefNr);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('NodeRefNr', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FNodeRefNr := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NodeRefNr', LOldValue, IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_RainfallScalingFactor(Value: Double);
const OPNAME = 'TYMDemandCentre.Set_RainfallScalingFactor';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FRainfallScalingFactor <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FRainfallScalingFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('RainfallScalingFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FRainfallScalingFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RainfallScalingFactor', LOldValue, FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_RoutingConstant(Value: Double);
const OPNAME = 'TYMDemandCentre.Set_RoutingConstant';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FRoutingConstant <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FRoutingConstant);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('YMDemandCentreRoutingConstant', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FRoutingConstant := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'YMDemandCentreRoutingConstant', LOldValue, FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_StdDeviationFactor(Value: Double);
const OPNAME = 'TYMDemandCentre.Set_StdDeviationFactor';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FStdDeviationFactor <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FStdDeviationFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('StdDeviationFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FStdDeviationFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StdDeviationFactor', LOldValue, FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_TotalFlowLost(Value: Double);
const OPNAME = 'TYMDemandCentre.Set_TotalFlowLost';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FTotalFlowLost <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FTotalFlowLost);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('TotalFlowLost', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FTotalFlowLost := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TotalFlowLost', LOldValue, FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_NodeNumber(Value: Integer);
const OPNAME = 'TYMDemandCentre.Set_NodeNumber';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FNodeNumber <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FNodeNumber);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('YMDemandCentreNodeNumber', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FNodeNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'YMDemandCentreNodeNumber', LOldValue, IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateAveEvaporation(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateAveEvaporation';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AveEvaporation', FloatToStr(FAveEvaporation), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FAveEvaporation)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateAveReturnFlowFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateAveReturnFlowFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AveReturnFlowFactor', FloatToStr(FAveReturnFlowFactor), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FAveReturnFlowFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateDescription(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateDescription';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('YMDemandCentreDescription', FDescription, lMessage)) then
      AErrorMessages.Add('WARNING:' +FDescription + ':'+lMessage);
    //else
    //  Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateEvapoTranspiration(AErrorMessages: TStrings; AErrorColumns: TStringList): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateEvapoTranspiration';
var
  LFieldProperty  : TAbstractFieldProperty;
  LMessage        : string;
  LIndex          : integer;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('EvapoTranspiration');
    if (LFieldProperty <> nil) then
    begin
      AErrorColumns.Clear;
      for lIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        lMessage := '';
        if (FEvapoTranspiration[lIndex] <> NullFloat) then
        begin
          if (not FAppModules.FieldProperties.ValidateFieldProperty
             ('EvapoTranspiration', FloatToStr(FEvapoTranspiration[LIndex]),
             lMessage, LIndex)) then
          begin
            AErrorMessages.Add('ERROR:' +LMessage);
            AErrorColumns.Add(IntToStr(LIndex));
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentre.ValidateName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('YMDemandCentreName', FName, lMessage)) then
      AErrorMessages.Add('ERROR:' +FName + ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateNodeNumber(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateNodeNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('YMDemandCentreNodeNumber', IntToStr(FNodeNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FNodeNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateNodeRefNr(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateNodeRefNr';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('NodeRefNr', IntToStr(FNodeRefNr), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FNodeRefNr)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateRainfallScalingFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateRainfallScalingFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('RainfallScalingFactor', FloatToStr(FRainfallScalingFactor), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FRainfallScalingFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateRoutingConstant(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateRoutingConstant';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('YMDemandCentreRoutingConstant', FloatToStr(FRoutingConstant), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FRoutingConstant)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateStdDeviationFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateStdDeviationFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('StdDeviationFactor', FloatToStr(FStdDeviationFactor), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FStdDeviationFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateTotalFlowLost(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateTotalFlowLost';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('TotalFlowLost', FloatToStr(FTotalFlowLost), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FTotalFlowLost)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_Identifier(Value: Integer);
const OPNAME = 'TYMDemandCentre.Set_Identifier';
begin
  FIdentifier := Value;
end;

function TYMDemandCentre.PopulateIDs(AIdentifier :Integer; AName: string): Boolean;
const OPNAME = 'TYMDemandCentre.PopulateIDs';
begin
  Result := False;
  try
    FIdentifier  := AIdentifier;
    FName        := AName;
    FDescription := AName;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_ConsumptiveUseChannel: IGeneralFlowChannel;
const OPNAME = 'TYMDemandCentre.Get_ConsumptiveUseChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FConsumptiveUseChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_ReclaimationChannel: IGeneralFlowChannel;
const OPNAME = 'TYMDemandCentre.Get_ReclaimationChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FReclaimationChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateConsumptiveChannel(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateConsumptiveChannel';
var
  lMessage          : WideString;
  lGeneralFlowChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FConsumptiveUseChannelNr <> 0) then
    begin
      lGeneralFlowChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ChannelList.ChannelByChannelNumber[FConsumptiveUseChannelNr];
      if (lGeneralFlowChannel.UpStreamNodeNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FName]));
        Result := False;
      end
      else
      if (lGeneralFlowChannel.ChannelPenalty <> nil) and ((lGeneralFlowChannel.ChannelPenalty.ChannelPenaltyArcCount <> 2)) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidPenaltyArcCount');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [lGeneralFlowChannel.ChannelPenalty.ChannelPenaltyName,lGeneralFlowChannel.ChannelName]));
        Result := False;
      end
      else
      begin
        if (not lGeneralFlowChannel.Validate(lMessage,'UpNodeNumber')) then
        begin
          Result := False;
          AErrorMessages.Add('ERROR:' +FName+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentre.ValidateReturnFlowChannel(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateReturnFlowChannel';
var
  lMessage             : WideString;
  lGeneralFlowChannel : IGeneralFlowChannel;
  LReturnFlow         : TYMDemandCentreReturnFlowFeature;
  LIndex : integer;
begin
  Result := True;
  try
    for LIndex := 0 to FReturnFlowList.ReturnFlowFeatureCount-1 do
    begin
      LReturnFlow    := FReturnFlowList.CastReturnFlowFeatureByIndex(LIndex);
      lGeneralFlowChannel := LReturnFlow.Channel;
      if (lGeneralFlowChannel <>nil) then
      begin
        if (lGeneralFlowChannel.DownStreamNodeNumber = 0) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDownstreamNodeNumber');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FName]));
          Result := False;
        end
        else
        if (lGeneralFlowChannel.ChannelPenalty <> nil) and ((lGeneralFlowChannel.ChannelPenalty.ChannelPenaltyArcCount <> 2)) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidPenaltyArcCount');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [lGeneralFlowChannel.ChannelPenalty.ChannelPenaltyName,lGeneralFlowChannel.ChannelName]));
          Result := False;
        end
        else
        begin
          if (not lGeneralFlowChannel.Validate(lMessage,'DownNodeNumber')) then
          begin
            Result := False;
            AErrorMessages.Add('ERROR:' +FName+ ':'+lMessage);
          end;
        end;
      end;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentre.Get_ConsumptiveUseChannelNr: Integer;
const OPNAME = 'TYMDemandCentre.Get_ConsumptiveUseChannelNr';
begin
  try
    Result := FConsumptiveUseChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_ConsumptiveUseChannelNr(Value: Integer);
const OPNAME = 'TYMDemandCentre.Set_ConsumptiveUseChannelNr';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
  LChannel      : IGeneralFlowChannel;
begin
  try
    if FConsumptiveUseChannelNr <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FConsumptiveUseChannelNr);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('DemandCentreConsumptiveChannelNumber', IntToStr(Value), LOldValue, LContextData ) then
        begin
          // update old channel number;
          LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ChannelList.ChannelByChannelNumber[FConsumptiveUseChannelNr];
          if LChannel <> nil then
            LChannel.UpStreamNodeNumber := 0;

          // update old channel number;
          LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ChannelList.ChannelByChannelNumber[Value];
          if LChannel <> nil then
            LChannel.UpStreamNodeNumber := FNodeNumber;

          FConsumptiveUseChannelNr := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DemandCentreConsumptiveChannelNumber', LOldValue, IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_ReclaimationPlantExists: WordBool;
const OPNAME = 'TYMDemandCentre.Get_ReclaimationPlantExists';
begin
  try
    Result := (ReclaimationChannel <> nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_ReclaimationPlantExists(Value: WordBool);
const OPNAME = 'TYMDemandCentre.Set_ReclaimationPlantExists';
var
  lGeneralChannel     : IGeneralFlowChannel;
  lChannelID :Integer;
  lNetworkElementData : TNetworkElementData;
begin
  try
    if Value = False then
    begin
      if ReclaimationChannel <> nil then
      begin
        lNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;
        lChannelID := ReclaimationChannel.ChannelID;
        lGeneralChannel := lNetworkElementData.ChannelList.ChannelByIdentifier[lChannelID];

        if (ReclaimationChannel.ChannelType = 21) and (ReclaimationChannel.UpStreamNodeNumber = FNodeNumber) then
        begin
          lGeneralChannel := nil;
          lNetworkElementData.CastChannelList.RemoveChannelWithID(lChannelID);
          //FReclaimationChannelNr := 0;
          ReclaimationChannelNr := 0;
        end;
      end;
    end else
    begin
      lGeneralChannel := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastChannelList.CreateChannel;
      if lGeneralChannel <> nil then
      begin
        lGeneralChannel.ChannelType := 21;
        lGeneralChannel.UpStreamNodeNumber := FNodeNumber;
        ReclaimationChannelNr := lGeneralChannel.ChannelNumber;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_ReclaimationChannelNr: Integer;
const OPNAME = 'TYMDemandCentre.Get_ReclaimationChannelNr';
begin
  try
    Result := FReclaimationChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentre.Set_ReclaimationChannelNr(Value: Integer);
const OPNAME = 'TYMDemandCentre.Set_ReclaimationChannelNr';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
  LChannel      : IGeneralFlowChannel;
begin
  try
    if FReclaimationChannelNr <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FReclaimationChannelNr);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('DemandCentreReclaimationChannelNumber', IntToStr(Value), LOldValue, LContextData ) then
        begin
          // update old channel number;
          LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ChannelList.ChannelByChannelNumber[FReclaimationChannelNr];
          if LChannel <> nil then
            LChannel.UpStreamNodeNumber := 0;

          // update old channel number;
          LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ChannelList.ChannelByChannelNumber[Value];
          if LChannel <> nil then
            LChannel.UpStreamNodeNumber := FNodeNumber;

          FReclaimationChannelNr := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DemandCentreReclaimationChannelNumber', LOldValue, IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.ValidateReclaimationChannel(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentre.ValidateReclaimationChannel';
var
  lMessage          : WideString;
  lGeneralFlowChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FReclaimationChannelNr <> 0) then
    begin
      lGeneralFlowChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ChannelList.ChannelByChannelNumber[FReclaimationChannelNr];
      if (lGeneralFlowChannel.UpStreamNodeNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FName]));
        Result := False;
      end
      else
      begin
        if (not lGeneralFlowChannel.Validate(lMessage,'UpNodeNumber')) then
        begin
          Result := False;
          AErrorMessages.Add('ERROR:' +FName+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentre.Get_ReturnFlowFeatureList: IYMDemandCentreReturnFlowFeatureList;
const OPNAME = 'TYMDemandCentre.Get_ReturnFlowFeatureList';
begin
  try
    Result  := FReturnFlowList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{procedure TYMDemandCentre.Set_ReturnFlowFeatureList(const Value: IYMDemandCentreReturnFlowFeatureList);
const OPNAME = 'TYMDemandCentre.Set_ReturnFlowFeatureList';
var
  LCount : Integer;
begin
  try
//    FReturnFlowList := Value;
    for LCount := 0 to Value.ReturnFlowFeatureCount - 1 do
      Assign(Value.ReturnFlowFeatureByIndex[LCount]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;    }

function TYMDemandCentre.CastReturnFlowList: TYMDemandCentreReturnFlowFeatureList;
const OPNAME = 'TYMDemandCentre.CastReturnFlowList';
begin
  Result := nil;
  try
    Result  := FReturnFlowList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_SupplyChannelNrs: WideString;
const OPNAME = 'TYMDemandCentre.Get_SupplyChannelNrs';
var
  lCount    : Integer;
  LChannel  : IGeneralFlowChannel;
begin
  try
    FSupplyChannelNrs := '';
    for lCount := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelCount -1 do
    begin
      LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByIndex[LCount];
      if (LChannel.DownStreamNodeNumber = FNodeNumber) and (lChannel.ChannelType = 12) then
        FSupplyChannelNrs := FSupplyChannelNrs + ',' + IntToStr(LChannel.ChannelNumber);
    end;
    Result := FSupplyChannelNrs;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_SupplyChannelByIndex(AIndex: integer): IGeneralFlowChannel;
const OPNAME = 'TYMDemandCentre.Get_SupplyChannelByIndex';
var
   LCount,
   LCorrectIndex  : Integer;
   LChannel       : IGeneralFlowChannel;
begin
  Result        := nil;
  LCorrectIndex := -1;
  try
    for LCount := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelCount - 1 do
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
                    ChannelByIndex[LCount];
      if  (LChannel <> nil) and
          (LChannel.ChannelType = 12) and
          (LChannel.DownStreamNodeNumber = FNodeNumber) then
      begin
        if LCorrectIndex = (AIndex - 1) then
        begin
          Result := LChannel;
          Break;
        end;
        Inc(LCorrectIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.Get_SupplyChannelCount: Integer;
const OPNAME = 'TYMDemandCentre.Get_SupplyChannelCount';
var
   LCount   : Integer;
   LResult  : Integer;
   LChannel : IGeneralFlowChannel;
begin
  LResult := 0;
  Result  := 0;
  try
    for LCount := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelCount - 1 do
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
                    ChannelByIndex[LCount];
      if  (LChannel <> nil) and
          (LChannel.ChannelType = 12) and
          (LChannel.DownStreamNodeNumber = FNodeNumber) then
        Inc(LResult);
    end;
    Result  :=  LResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.DeleteAllReturnFlowChannels: boolean;
const OPNAME = 'TYMDemandCentre.DeleteAllReturnFlowChannels';
var
  LCount : integer;
  LChannelNr          : Integer;
  LReturnFlowChannel  : TYMDemandCentreReturnFlowFeature;
begin
  Result := True;
  try
    // Remove return flow channels
    for LCount := ReturnFlowFeatureList.ReturnFlowFeatureCount - 1 downto 0 do
    begin
      LReturnFlowChannel := CastReturnFlowList.CastReturnFlowFeatureByIndex(LCount);
      if (LReturnFlowChannel <> nil) then
        LChannelNr  := LReturnFlowChannel.Channel.ChannelNumber
      else
        LChannelNr  := 0;
      if not ReturnFlowFeatureList.RemoveReturnFlowFeatureWithNr(Self.Identifier, LChannelNr) then
        Result := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.DeleteAllSupplyChannels: boolean;
const OPNAME = 'TYMDemandCentre.DeleteAllSupplyChannels';
var
  LCount   : integer;
  LChannel : TGeneralFlowChannel;
  LNetworkElementData : TNetworkElementData;
  LChannelNumber : integer;
begin
  Result := True;
  try
    LNetworkElementData :=  TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;
    for LCount := LNetworkElementData.ChannelList.ChannelCount - 1 downto 0 do
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastChannelList.CastChannelByIndex[LCount];
      if  (LChannel <> nil) and (LChannel.ChannelType = 12) and
          (LChannel.DownStreamNodeNumber = Self.FNodeNumber) then
      begin
        LChannelNumber := LChannel.ChannelNumber;
        if LNetworkElementData.ChannelList.RemoveChannelWithNumber(LChannelNumber) then
          TSystemModelManager(FAppModules.Model).DeleteTabSheetTreeViewElement('ChannelDetails12','ChannelHeading,ChannelDetails12',LChannelNumber,tvnidChannelDetails12)
        else
          Result := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentre.DeleteConsumptiveChannel: boolean;
const OPNAME = 'TYMDemandCentre.DeleteConsumptiveChannel';
var
  LChannelType : integer;
begin
  Result := True;
  try
    if (FConsumptiveUseChannelNr <> 0) then
    begin
      LChannelType := 0;
      if(ConsumptiveUseChannel  <> nil) then
        LChannelType := ConsumptiveUseChannel.ChannelType;
      Result := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.ChannelList.RemoveChannelWithNumber(FConsumptiveUseChannelNr);
      if Result then
      begin
        case LChannelType of
          ctMinMaxChannel:        TSystemModelManager(FAppModules.Model).DeleteTabSheetTreeViewElement('ChannelDetails8','ChannelHeading,ChannelDetails8',FConsumptiveUseChannelNr,tvnidChannelDetails8);
          ctDemandChannel:        TSystemModelManager(FAppModules.Model).DeleteTabSheetTreeViewElement('ChannelDetails11','ChannelHeading,ChannelDetails11',FConsumptiveUseChannelNr,tvnidChannelDetails11);
          ctMasterControlChannel: TSystemModelManager(FAppModules.Model).DeleteTabSheetTreeViewElement('MasterControlConfiguration','ParametersHeading,MasterControlConfiguration',FConsumptiveUseChannelNr,tvnidMasterControlConfiguration);
          ctGeneralChannel:       TSystemModelManager(FAppModules.Model).DeleteTabSheetTreeViewElement('ChannelDetails12','ChannelHeading,ChannelDetails12',FConsumptiveUseChannelNr,tvnidChannelDetails12);
        end;//case
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TYMDemandCentreList }

function TYMDemandCentreList._AddRef: Integer;
const OPNAME = 'TYMDemandCentreList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList._Release: Integer;
const OPNAME = 'TYMDemandCentreList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.AddYMDemandCentre(AFeature: TYMDemandCentre): boolean;
const OPNAME = 'TYMDemandCentreList.AddYMDemandCentre';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FYMDemandCentreList.Add(AFeature);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.CastYMDemandCentreByID(AYMDemandCentreID: integer): TYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.CastYMDemandCentreByID';
var
 lIndex          : Integer;
 LYMDemandCentre : TYMDemandCentre;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FYMDemandCentreList.Count)) do
    begin
      LYMDemandCentre := TYMDemandCentre(FYMDemandCentreList.Items[lIndex]);
      if (LYMDemandCentre.FIdentifier = AYMDemandCentreID) then
        Result := LYMDemandCentre
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.CastYMDemandCentreByIndex(AIndex: integer): TYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.CastYMDemandCentreByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FYMDemandCentreList.Count) then
      Result := TYMDemandCentre(FYMDemandCentreList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreList.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FYMDemandCentreList := TObjectList.Create(False);
    Initialise;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.CreateNewYMDemandCentre: TYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.CreateNewYMDemandCentre';
var
  LContinue      : boolean;
  LDemandNodeNumber : integer;
  LResevoirData     : IReservoirData;
  LSQLAgent         : TYMDemandCentreSQLAgent;
  LYMDemandCentre   : TYMDemandCentre;
  LSupplyChannel    : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if (FYMDemandCentreList.Count  >= 100) then
      Exit;

    LSQLAgent := TYMDemandCentreSQLAgent.Create(FAppModules);
    try
      LResevoirData := nil;
      LSupplyChannel := nil;
      LContinue := True;


      if LContinue then
      begin
        LResevoirData := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateNodeWithoutInflow(ntDemandCentreNode);
        LContinue := LResevoirData <> nil;
      end;

      //Create one supply channel
      if LContinue then
      begin
        LSupplyChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
        LContinue := LSupplyChannel <> nil;
      end;

      if LContinue then
      begin
        LYMDemandCentre := TYMDemandCentre.Create(FAppModules);
        if (Assigned(LYMDemandCentre)) then
        begin
          LDemandNodeNumber := LResevoirData.ReservoirConfigurationData.ReservoirIdentifier;
          LYMDemandCentre.Initialise;

          LYMDemandCentre.FName := 'DEMAND CENTRE('+IntToStr(LDemandNodeNumber)+')';;
          LYMDemandCentre.FNodeNumber := LDemandNodeNumber;


          if LSQLAgent.InsertYMDemandCentre(LYMDemandCentre) then
          begin
            FYMDemandCentreList.Add(LYMDemandCentre);
            LSupplyChannel.DownStreamNodeNumber := LYMDemandCentre.NodeNumber;
            //Create one return flow channel
            LYMDemandCentre.ReturnFlowFeatureList.CreateReturnFlowFeature(LYMDemandCentre.FIdentifier);
            Result := LYMDemandCentre;
          end
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.RemoveYMDemandCentre(ANodeNumber: integer): WordBool;
const OPNAME = 'TYMDemandCentreList.RemoveYMDemandCentre';
var
  LLoadAgent          : TYMDemandCentreSQLAgent;
  LYMDemandCentre     : TYMDemandCentre;
  LDemandCentreID     : Integer;
  LNetworkElementData : TNetworkElementData;
begin
  Result          := False;
  try
    LLoadAgent := TYMDemandCentreSQLAgent.Create(FAppModules);
    try
      LYMDemandCentre  := CastYMDemandCentreByNodeNumber(ANodeNumber);
      if(LYMDemandCentre = nil) then  Exit;

      LNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;

      LDemandCentreID  := LYMDemandCentre.Identifier;
      LNetworkElementData.CastReservoirList.DeleteNodeWithoutInflow(ANodeNumber);

      LYMDemandCentre.DeleteAllSupplyChannels;
      LYMDemandCentre.DeleteAllReturnFlowChannels;
      //LYMDemandCentre.DeleteConsumptiveChannel;
      LYMDemandCentre.ReclaimationPlantExists := False;

      if LLoadAgent.DeleteYMDemandCentre(ANodeNumber) then
      begin
        DeleteYMDemandCentreWithID(LDemandCentreID);
        Result := True;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.CreateYMDemandCentre: IYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.CreateYMDemandCentre';
begin
  Result := nil;
  try
    Result := CreateNewYMDemandCentre;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.DeleteYMDemandCentreWithID(ADemandCentreID: integer): WordBool;
const OPNAME = 'TYMDemandCentreList.DeleteYMDemandCentreWithID';
var
  lYMDemandCentre : TYMDemandCentre;
  lIndex          : Integer;
begin
  Result := False;
  try
    lYMDemandCentre := CastYMDemandCentreByID(ADemandCentreID);
    if (lYMDemandCentre <> nil) then
    begin
      lIndex := FYMDemandCentreList.IndexOf(lYMDemandCentre);
      Result := DeleteYMDemandCentreWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.DeleteYMDemandCentreWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TYMDemandCentreList.DeleteYMDemandCentreWithIndex';
begin
  Result := False;
  try
    if (AIndex >= 0) then
    begin
      FYMDemandCentreList.Delete(AIndex);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreList.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FYMDemandCentreList);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.Get_YMDemandCentreByID(ADemandCentreID: integer): IYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.Get_YMDemandCentreByID';
var
  lIndex          : integer;
  lYMDemandCentre : TYMDemandCentre;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FYMDemandCentreList.Count)) do
    begin
      lYMDemandCentre := TYMDemandCentre(FYMDemandCentreList.Items[lIndex]);
      if (lYMDemandCentre.FIdentifier = ADemandCentreID) then
        Result := lYMDemandCentre
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.Get_YMDemandCentreByIndex(AIndex: integer): IYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.Get_YMDemandCentreByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) and (AIndex < FYMDemandCentreList.Count)) then
      Result := TYMDemandCentre(FYMDemandCentreList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.Get_YMDemandCentreByNodeNumber(ANodeNumber: Integer): IYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.Get_YMDemandCentreByNodeNumber';
var
  lIndex          : Integer;
  lYMDemandCentre : TYMDemandCentre;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FYMDemandCentreList.Count)) do
    begin
      lYMDemandCentre := TYMDemandCentre(FYMDemandCentreList.Items[lIndex]);
      if (lYMDemandCentre.FNodeNumber = ANodeNumber) then
        Result := lYMDemandCentre
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.Get_YMDemandCentreCount: Integer;
const OPNAME = 'TYMDemandCentreList.Get_YMDemandCentreCount';
begin
  Result := 0;
  try
    Result := FYMDemandCentreList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.Initialise: boolean;
const OPNAME = 'TYMDemandCentreList.Initialise';
begin
  Result := inherited Initialise;
  try
    FYMDemandCentreCount := 0;
    FYMDemandCentreList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.NewYMDemandCentre: TYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.NewYMDemandCentre';
begin
  Result := nil;
  try
    Result := TYMDemandCentre.Create(FAppModules);
    FYMDemandCentreList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TYMDemandCentreList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;
    for LIndex := 0 to FYMDemandCentreList.Count -1 do
    begin
      if not TYMDemandCentre(FYMDemandCentreList[LIndex]).Validate(AErrors,AContext) then
      begin
        Result := False;
        if FAppModules.GlobalData.StopOnFirstErr then
          Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.ValidateYMDemandCentreCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentreList.ValidateYMDemandCentreCount';
var
  lMessage : string;
  lValue   : integer;
begin
  Result := False;
  try
    lMessage := '';
    lValue   := YMDemandCentreCount;
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DemandCentreCountCount', IntToStr(lValue), lMessage)) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.YMDemandCentreByName(const AName: WideString): IYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.YMDemandCentreByName';
var
  lIndex          : integer;
  LYMDemandCentre : TYMDemandCentre;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FYMDemandCentreList.Count)) do
    begin
      LYMDemandCentre := TYMDemandCentre(FYMDemandCentreList.Items[lIndex]);
      if (LYMDemandCentre.FName = AName) then
        Result := LYMDemandCentre
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.GetConsumptiveChannelID(AYMDemandCentreID: integer): integer;
const OPNAME = 'TYMDemandCentreList.GetConsumptiveChannelID';
var
  LYMDemandCentre  : TYMDemandCentre;
  LChannel  : IGeneralFlowChannel;
begin
  Result := NullInteger;
  try
    LYMDemandCentre := CastYMDemandCentreByID(AYMDemandCentreID);
    if(LYMDemandCentre <> nil) then
    begin
      LChannel := LYMDemandCentre.ConsumptiveUseChannel;
      if(LChannel <> nil) then
         Result := LChannel.ChannelID;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{ TYMDemandCentreReturnFlowFeature                                                       }
{******************************************************************************}

function TYMDemandCentreReturnFlowFeature._AddRef: Integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature._Release: Integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeature.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeature.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Initialise: boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelNr        := 0;
    FFeatureID        := -1;
    FDemandCentreID   := 0;
    FTotalReturnFlow  := 0.0;
    FFlowDiversion    := 0.0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Get_FeatureID : integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Get_Channel: IGeneralFlowChannel;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
              ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Get_ChannelNr: Integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Get_ChannelNr';
begin
  Result := 0;
  try
    Result := FChannelNr;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Get_DemandCentreID: Integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Get_DemandCentreID';
begin
  Result := 0;
  try
    Result := FDemandCentreID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Get_FlowDiversion: Double;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Get_FlowDiversion';
begin
  Result := 0.0;
  try
    Result := FFlowDiversion;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Get_TotalReturnFlow: Double;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Get_TotalReturnFlow';
begin
  Result := 0.0;
  try
    Result := FTotalReturnFlow;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeature.Set_Channel(const AChannel: IGeneralFlowChannel);
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Set_Channel';
var
  LLoadAgent   : TYMDemandCentreSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LNewValue    : string;
begin
  try
    LLoadAgent := TYMDemandCentreSQLAgent.Create(FAppModules);
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
           'ReturnFlowChannelNr', LNewValue, LOldValue, LContextData) then
        begin
          FChannelNr := AChannel.ChannelNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'YMDCReturnFlowFeatureName',LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Populate(AFeatureID: integer;
  AChannelNr, ADemandCentreID: integer;
  ATotalReturnFlow, AFlowDiversion: double): WordBool;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Populate';  
begin
  Result := FALSE;
  try
    FFeatureID         := AFeatureID;
    FChannelNr         := AChannelNr;
    FDemandCentreID    := ADemandCentreID;
    FTotalReturnFlow   := ATotalReturnFlow;
    FFlowDiversion     := AFlowDiversion;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeature.Set_ChannelNr(Value: Integer);
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Set_ChannelNr';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FChannelNr <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FChannelNr);
        LLoadAgent.LoadContextData_FeatureIDReturnFlow(LContextData, IntToStr(FDemandCentreID), IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue('ReturnFlowChannelNr', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FChannelNr := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReturnFlowChannelNr', LOldValue, IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeature.Set_DemandCentreID(Value: Integer);
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Set_DemandCentreID';
begin
  try
    FDemandCentreID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TYMDemandCentreReturnFlowFeature.Set_FlowDiversion(Value: Double);
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Set_FlowDiversion';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FFlowDiversion <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FFlowDiversion);
        LLoadAgent.LoadContextData_FeatureIDReturnFlow(LContextData, IntToStr(FDemandCentreID), IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue('FlowDiversion', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FFlowDiversion := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FlowDiversion',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeature.Set_TotalReturnFlow(Value: Double);
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Set_TotalReturnFlow';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FTotalReturnFlow <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FTotalReturnFlow);
        LLoadAgent.LoadContextData_FeatureIDReturnFlow(LContextData, IntToStr(FDemandCentreID), IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue('TotalReturnFlow', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FTotalReturnFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TotalReturnFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
  lStopOnFirstError : Boolean;   
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'ReturnFlowChannelNr') then
        Result := ValidateChannelNr(lErrorMessage)
      else
      if (AContext = 'TotalReturnFlow') then
        Result := ValidateTotalReturnFlow(lErrorMessage)
      else
      if (AContext = 'FlowDiversion') then
        Result := ValidateFlowDiversion(lErrorMessage)
      else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateChannelNr(lErrorMessage))       then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateTotalReturnFlow(lErrorMessage)) then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateFlowDiversion(lErrorMessage))   then Result := False;
      end;
      AErrors := LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.ValidateChannelNr(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.ValidateChannelNr';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ReturnFlowChannelNr', IntToStr(FChannelNr), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FChannelNr)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.ValidateFlowDiversion(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.ValidateFlowDiversion';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('FlowDiversion', FloatToStr(FFlowDiversion), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FFlowDiversion)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.ValidateTotalReturnFlow(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.ValidateTotalReturnFlow';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('TotalReturnFlow', FloatToStr(FTotalReturnFlow), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FTotalReturnFlow)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreList.CastYMDemandCentreByNodeNumber(ANodeNumber: Integer): TYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.CastYMDemandCentreByNodeNumber';
var
  lIndex          : Integer;
  lYMDemandCentre : TYMDemandCentre;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FYMDemandCentreList.Count)) do
    begin
      lYMDemandCentre := TYMDemandCentre(FYMDemandCentreList.Items[lIndex]);
      if (lYMDemandCentre.FNodeNumber = ANodeNumber) then
        Result := lYMDemandCentre
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TYMDemandCentreList.CopyCreate(ANodeNumber: integer): IYMDemandCentre;
const OPNAME = 'TYMDemandCentreList.CopyCreate';
var
  LDestYMDemandCentre : TYMDemandCentre;
  LSourceYMDemandCentre : TYMDemandCentre;
begin
  Result := nil;
  try
    LSourceYMDemandCentre := CastYMDemandCentreByNodeNumber(ANodeNumber);
    if LSourceYMDemandCentre <> nil then
    begin
      LDestYMDemandCentre := CreateNewYMDemandCentre;
      if LDestYMDemandCentre <> nil then
      begin
        LDestYMDemandCentre.Assign(LSourceYMDemandCentre);
        Result := LDestYMDemandCentre;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TYMDemandCentreReturnFlowFeatureList }

function TYMDemandCentreReturnFlowFeatureList._AddRef: Integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList._Release: Integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.AddReturnFlowFeature(AFeature: TYMDemandCentreReturnFlowFeature): boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.AddReturnFlowFeature';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FReturnFlowList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.CastReturnFlowFeatureByID(AFeatureID: integer): TYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.CastReturnFlowFeatureByID';
var
 lIndex   : integer;
 lFeature : TYMDemandCentreReturnFlowFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FReturnFlowList.Count)) do
    begin
      lFeature := TYMDemandCentreReturnFlowFeature(FReturnFlowList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.CastReturnFlowFeatureByIndex(AIndex: integer): TYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.CastReturnFlowFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FReturnFlowList.Count) then
      Result := TYMDemandCentreReturnFlowFeature(FReturnFlowList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeatureList.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReturnFlowList := TObjectList.Create(False);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.CreateNewReturnFlowFeature(ADemandCentreID: Integer): TYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.CreateNewReturnFlowFeature';
var
  LLoadAgent    : TYMDemandCentreSQLAgent;
  LFeature      : TYMDemandCentreReturnFlowFeature;
  LDemandCentre : IYMDemandCentre;
  LChannel      : IGeneralFlowChannel;
begin
  Result := nil;
  if FReturnFlowList.Count >= 20 then
    Exit;
  try
    LLoadAgent := TYMDemandCentreSQLAgent.Create(FAppModules);
    try
      LFeature  := TYMDemandCentreReturnFlowFeature.Create(FAppModules);
      try
        LChannel                    := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                        ChannelList.CreateChannel;
        LDemandCentre               := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                        YMDemandCentreList.YMDemandCentreByID[ADemandCentreID];

        LChannel.ChannelType        := 20;
        LChannel.UpStreamNodeNumber := LDemandCentre.NodeNumber;

        LFeature.Initialise;
        LFeature.FDemandCentreID    := ADemandCentreID;
        LFeature.FChannelNr         := LChannel.ChannelNumber;

        if LLoadAgent.InsertReturnFlowFeature(LFeature) then
        begin
          FReturnFlowList.Add(LFeature);
          Result := LFeature;
        end
        else
        begin
          FreeAndNil(LFeature);
        end;
      except
        FreeAndNil(LFeature);
      end;  
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.CreateReturnFlowFeature(ADemandCentreID: Integer): IYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.CreateReturnFlowFeature';
begin
  Result := nil;
  try
    Result := CreateNewReturnFlowFeature(ADemandCentreID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.DeleteReturnFlowFeatureWithChannelNr(AChannelNr: integer): WordBool;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.DeleteReturnFlowFeatureWithChannelNr';
var
  lFeature : TYMDemandCentreReturnFlowFeature;
  lIndex   : integer;
begin
  Result := False;
  try
    lFeature := CastReturnFlowFeatureByChannelNr(AChannelNr);
    if (lFeature <> nil) then
    begin
      lIndex := FReturnFlowList.IndexOf(lFeature);
      Result := DeleteReturnFlowFeatureWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.DeleteReturnFlowFeatureWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.DeleteReturnFlowFeatureWithIndex';
begin
  Result := False;
  try
    if (AIndex >= 0) then
    begin
      FReturnFlowList.Delete(AIndex);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeatureList.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReturnFlowList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.Get_ReturnFlowFeatureByID(AFeatureID: integer): IYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.Get_ReturnFlowFeatureByID';
var
 lIndex   : integer;
 lFeature : TYMDemandCentreReturnFlowFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FReturnFlowList.Count)) do
    begin
      lFeature := TYMDemandCentreReturnFlowFeature(FReturnFlowList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.Get_ReturnFlowFeatureByIndex(AIndex: integer): IYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.Get_ReturnFlowFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FReturnFlowList.Count) then
      Result := TYMDemandCentreReturnFlowFeature(FReturnFlowList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.Get_ReturnFlowFeatureCount: integer;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.Get_ReturnFlowFeatureCount';
begin
  Result := 0;
  try
    Result := FReturnFlowList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.Initialise: Boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FReturnFlowList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.NewReturnFlowFeature: TYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.NewReturnFlowFeature';
begin
  Result := nil;
  try
    Result := TYMDemandCentreReturnFlowFeature.Create(FAppModules);
    FReturnFlowList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.RemoveReturnFlowFeatureWithNr(ADemandCentreID, AChannelNr: integer): WordBool;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.RemoveReturnFlowFeatureWithNr';
var
  lLoadAgent    : TYMDemandCentreSQLAgent;
  LChannelID    : Integer;
  LChannel      : TGeneralFlowChannel;
begin
  Result := False;
  try
    LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastChannelList.CastChannelByChannelNumber[AChannelNr];
    if (LChannel <> nil) then
    begin
      LLoadAgent := TYMDemandCentreSQLAgent.Create(FAppModules);
      try
        LChannelID  := LChannel.ChannelID;
        if LLoadAgent.DeleteReturnFlowFeature(AChannelNr) then
        begin
          DeleteReturnFlowFeatureWithChannelNr(AChannelNr);
          TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.RemoveChannelWithID(LChannelID);
          Result := True;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;
    for LIndex := 0 to FReturnFlowList.Count -1 do
    begin
      if not TYMDemandCentreReturnFlowFeature(FReturnFlowList[LIndex]).Validate(AErrors,AContext) then
      begin
        Result := False;
        if FAppModules.GlobalData.StopOnFirstErr then
          Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeature.PopulateIDs(AIdentifier: integer): boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.PopulateIDs';
begin
  Result := False;
  try
    FFeatureID := AIdentifier;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TYMDemandCentreReturnFlowFeature.Assign(AReturnFlow: TYMDemandCentreReturnFlowFeature): Boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Assign';
begin
  Result := False;
  try
    FFeatureID        := AReturnFlow.FeatureID;
    FChannelNr        := AReturnFlow.ChannelNr;
    FDemandCentreID   := AReturnFlow.DemandCentreID;
    FTotalReturnFlow  := AReturnFlow.TotalReturnFlow;
    FFlowDiversion    := AReturnFlow.FlowDiversion;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreReturnFlowFeatureList.Assign(AReturnFlowList: TYMDemandCentreReturnFlowFeatureList): Boolean;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.Assign';
var
  LCount      : Integer;
  LReturnFlow : TYMDemandCentreReturnFlowFeature;
begin
  Result := False;
  try
    FReturnFlowList.Clear;
    for LCount := 0 to AReturnFlowList.ReturnFlowFeatureCount - 1 do
    begin
      LReturnFlow                   := TYMDemandCentreReturnFlowFeature.Create(FAppModules);
      LReturnFlow.FFeatureID        := AReturnFlowList.ReturnFlowFeatureByIndex[LCount].FeatureID;
      LReturnFlow.FChannelNr        := AReturnFlowList.ReturnFlowFeatureByIndex[LCount].ChannelNr;
      LReturnFlow.FDemandCentreID   := AReturnFlowList.ReturnFlowFeatureByIndex[LCount].DemandCentreID;
      LReturnFlow.FTotalReturnFlow  := AReturnFlowList.ReturnFlowFeatureByIndex[LCount].TotalReturnFlow;
      LReturnFlow.FFlowDiversion    := AReturnFlowList.ReturnFlowFeatureByIndex[LCount].FlowDiversion;

      AddReturnFlowFeature(LReturnFlow);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
 }
function TYMDemandCentreReturnFlowFeatureList.CastReturnFlowFeatureByChannelNr(AChannelNr: integer): TYMDemandCentreReturnFlowFeature;
const OPNAME = 'TYMDemandCentreReturnFlowFeatureList.CastReturnFlowFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TYMDemandCentreReturnFlowFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FReturnFlowList.Count)) do
    begin
      lFeature := TYMDemandCentreReturnFlowFeature(FReturnFlowList.Items[lIndex]);
      if (lFeature.ChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowFeature.Assign(ASource: TYMDemandCentreReturnFlowFeature);
const OPNAME = 'TYMDemandCentreReturnFlowFeature.Assign';
var
  LChannelList : IChannelList;
  LChnnelPenalty : IChannelPenalty;
  LDestConsumptive : IGeneralFlowChannel;
  LSourceConsumptive : IGeneralFlowChannel;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    if LChannelList <> nil then
    begin
      LDestConsumptive := LChannelList.ChannelByChannelNumber[FChannelNr];
      LSourceConsumptive := LChannelList.ChannelByChannelNumber[ASource.ChannelNr];
      if (LDestConsumptive <> nil) and (LSourceConsumptive <> nil) then
      begin
        LDestConsumptive.ChannelName := 'Copy of '+ LSourceConsumptive.ChannelName;
        LDestConsumptive.ChannelType := LSourceConsumptive.ChannelType;
        LDestConsumptive.ChannelSubType := LSourceConsumptive.ChannelSubType;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceConsumptive.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestConsumptive.ChannelPenalty := LChnnelPenalty;
      end;
    end;

    TotalReturnFlow  := ASource.TotalReturnFlow;
    FlowDiversion    := ASource.FlowDiversion;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.



