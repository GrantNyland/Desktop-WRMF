//
//
//  UNIT      : Contains Channel & Channel Penalty Classes
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 27/08/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UChannelData;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  UChannelPlanningData,
  UDisbenefitFunctionData,
//  UReturnFlowChannelData,
  VoaimsCom_TLB;

type

  TChannelPenalty = class(TAbstractAppObject, IChannelPenalty)
  private
    FChannelPenaltyID : integer;
    FPenaltyValues    : TChannelPenaltyValuesArray;
    FPenaltyName      : string;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function  IndexOfFirstNullPenaltyValue : integer;
    procedure UpdatePenaltyValue (AIndex : integer;
                                  AValue : double);
    procedure UpdateChannelArcCount(ANewCount: integer);

    function ValidateForChannels(AErrorMessages : TStrings) : WordBool;
  public
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function Initialise: Boolean; override;
    function Populate (APenaltyID     : integer;
                       APenaltyName   : WideString;
                       APenaltyValues : TChannelPenaltyValuesArray) : WordBool;
    function Get_ChannelPenaltyID : integer; safecall;
    function Get_ChannelPenaltyName : WideString; safecall;
    procedure Set_ChannelPenaltyName (const AName : WideString); safecall;
    function Get_ChannelPenaltyValues : TChannelPenaltyValuesArray; safecall;
    function Get_ChannelPenaltyArcCount : integer; safecall;
    function Get_ChannelPenaltyValueByIndex(AIndex : integer) : double; safecall;
    procedure Set_ChannelPenaltyValueByIndex(AIndex : integer;
                                             AValue : double); safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property ChannelPenaltyID : integer read Get_ChannelPenaltyID;
    property ChannelPenaltyName : WideString read Get_ChannelPenaltyName write Set_ChannelPenaltyName;
    property ChannelPenaltyArcCount: integer read Get_ChannelPenaltyArcCount;
    property ChannelPenaltyValues: TChannelPenaltyValuesArray read Get_ChannelPenaltyValues;
    property ChannelPenaltyValueByIndex[AIndex: integer]: double
      read Get_ChannelPenaltyValueByIndex write Set_ChannelPenaltyValueByIndex;
  end;

  TChannelPenaltyList = class(TAbstractAppObject, IChannelPenaltyList)
  protected
    FChannelPenaltyList : TObjectList;
    FInflowPenaltyNo    : Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetCastChannelPenaltyByIndex(AIndex: integer): TChannelPenalty;
    function GetCastChannelPenaltyByIdentifier(AIdentifier: integer): TChannelPenalty;
    function AddChannelPenalty (APenalty : TChannelPenalty): boolean;
  public
    function Initialise: Boolean; override;
    function NewChannelPenalty : TChannelPenalty;
    function CreateNewChannelPenalty : TChannelPenalty;
    function DeleteChannelPenaltyWithID (APenaltyID : integer) : WordBool;
    function DeleteChannelPenaltyWithIndex (AIndex : integer) : WordBool;
    function CreateChannelPenalty : IChannelPenalty; safecall;
    function RemoveChannelPenaltyWithID (APenaltyID: integer): WordBool; safecall;
    function Get_ChannelPenaltyByIdentifier(AIdentifier: integer): IChannelPenalty; safecall;
    function Get_ChannelPenaltyByIndex(AIndex: integer): IChannelPenalty; safecall;
    function Get_ChannelPenaltyCount: integer; safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function Get_InflowPenaltyNo: Integer; safecall;
    procedure Set_InflowPenaltyNo(Value: Integer); safecall;
    property CastChannelPenaltyByIndex[AIndex: integer]:TChannelPenalty read GetCastChannelPenaltyByIndex;
    property CastChannelPenaltyByIdentifier[AIdentifier: integer]:TChannelPenalty read GetCastChannelPenaltyByIdentifier;
    property ChannelPenaltyByIndex[AIndex: integer]: IChannelPenalty read Get_ChannelPenaltyByIndex;
    property ChannelPenaltyByIdentifier[AIdentifier: integer]: IChannelPenalty read Get_ChannelPenaltyByIdentifier;
    property ChannelPenaltyCount: integer read Get_ChannelPenaltyCount;
    property InflowPenaltyNo: Integer read Get_InflowPenaltyNo write Set_InflowPenaltyNo;
  end;

  TGeneralFlowChannel = class(TAbstractAppObject, IGeneralFlowChannel)
  protected
    FChannelID                     : integer;
    FChannelNumber                 : integer;
    FChannelName                   : string;
    FChannelType                   : integer;
    FChannelSubType                : integer;
    FUpStreamNodeNumber            : integer;
    FDownStreamNodeNumber          : integer;
    FChannelPenaltyNumber          : integer;
    FSummaryOutputRequired         : string;
    FRequiresFirmYieldAnalysis     : string;
    FFlowOutput                    : string;
    FMinimumFlowConstraintNr       : integer;
    FMinMaxFlowConstraintNr        : integer;
    FPumpingFeatureNr              : integer;
    FLossFeatureNr                 : integer;
    FSpecifiedDemandFeatureNr      : integer;
    FDiversionFeatureNr            : integer;
    FPhysicalFlowConstraintNr      : integer;
    FIFRFeatureNr                  : integer;
    FIrrigationAreaNr              : integer;
    FPowerPlantNr                  : integer;
    FSpecifiedInflowFeatureNr      : integer;
    FWaterDemandFeatureNr          : integer;
    FMasterControlFeatureNr        : integer;
    FChannelAreaID                 : integer;
    FTimeControl                   : TChannelTimeControl;
    FTimeControlList               : TObjectList;
    FDisbenefitFunction            : TDisbenefitFunctionData;
    FSwitchControls                : TObjectList;
    FSelectedSwitchControlID       : integer;
    FMultiResChannelCurtailID      : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function DeleteMinimumFlowConstraint  : Boolean;
    function DeleteLossFeature            : Boolean;
    function DeleteMinMaxFlowConstraint   : Boolean;
    function DeleteSpecifiedDemandFeature : Boolean;
    function DeleteDiversionFeature       : Boolean;
    function ArcCountValid (AArcCount : integer) : Boolean;
    function ValidateChannelName (AErrorMessages : TStrings) : WordBool;
    function ValidateChannelNumber(AErrorMessages : TStrings) : WordBool;
    function ValidateChannelPenalty (AErrorMessages : TStrings) : WordBool;
    function ValidateChannelUpstreamNode (AErrorMessages : TStrings) : WordBool;
    function ValidateChannelDownstreamNode (AErrorMessages : TStrings) : WordBool;
    function ValidateChannelUpDownNotSameNode (AErrorMessages : TStrings) : WordBool;
    function ValidateChannelTariffCalculation (AErrorMessages : TStrings) : WordBool;

  public
    procedure Assign(AGeneralFlowChannel:TGeneralFlowChannel);virtual;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function Initialise: boolean; override;
    function Get_ChannelID: integer; safecall;
    procedure Set_ChannelID(AChannelID: integer); safecall;
    function Get_ChannelNumber: integer; safecall;
    procedure Set_ChannelNumber(AChannelNumber: integer); safecall;
    function Get_ChannelName: WideString; safecall;
    procedure Set_ChannelName(const AChannelName: WideString); safecall;
    function Get_ChannelType: integer; safecall;
    procedure Set_ChannelType (AType : integer); safecall;
    function Get_ChannelSubType: integer; safecall;
    procedure Set_ChannelSubType(ASubType : integer); safecall;
    function Get_UpStreamNodeNumber: integer; safecall;
    procedure Set_UpStreamNodeNumber(ANodeNumber: integer); safecall;
    function Get_DownStreamNodeNumber: integer; safecall;
    procedure Set_DownStreamNodeNumber(ANodeNumber: integer); safecall;
    function Get_ChannelPenalty: IChannelPenalty; safecall;
    procedure Set_ChannelPenalty(const APenalty : IChannelPenalty); safecall;
    function Get_SummaryOutputRequired: WideString; safecall;
    procedure Set_SummaryOutputRequired(const AOutputRequired: WideString); safecall;
    function Get_FlowOutput: WideString; safecall;
    procedure Set_FlowOutput(const Value: WideString); safecall;
    function Get_TariffCalculation: IChannelTariff; safecall;
    function Get_ValidArcCounts : WideString; safecall;
    function Get_RequiresFirmYieldAnalysis: WideString; safecall;
    procedure Set_RequiresFirmYieldAnalysis(const ARequiresAnalysis: WideString); safecall;
    function Get_ChannelPenaltyNumber: integer; safecall;
    function Get_MinimumFlowConstraint : IMinimumFlowConstraint; safecall;
    procedure Set_MinimumFlowConstraint(const Value: IMinimumFlowConstraint); safecall;
    function Get_MinMaxFlowConstraint : IMinMaxFlowConstraint; safecall;
    procedure Set_MinMaxFlowConstraint (const Value: IMinMaxFlowConstraint); safecall;
    function Get_PumpingFeature : IPumpingFeature; safecall;
    procedure Set_PumpingFeature (const Value: IPumpingFeature); safecall;
    function Get_LossFeature : ILossFeature; safecall;
    procedure Set_LossFeature (const Value: ILossFeature); safecall;
    function Get_SpecifiedDemandFeature : ISpecifiedDemandFeature; safecall;
    procedure Set_SpecifiedDemandFeature (const Value: ISpecifiedDemandFeature); safecall;
    function Get_DiversionFeature : IDiversionFeature; safecall;
    procedure Set_DiversionFeature (const Value: IDiversionFeature); safecall;
    function Get_PhysicalFlowConstraint : IPhysicalFlowConstraint; safecall;
    procedure Set_PhysicalFlowConstraint (const Value: IPhysicalFlowConstraint); safecall;
    function Get_IFRFeature : IIFRFeature; safecall;
    procedure Set_IFRFeature (const Value: IIFRFeature); safecall;
    function Get_IrrigationArea : IIrrigationArea; safecall;
    procedure Set_IrrigationArea (const Value: IIrrigationArea); safecall;
    function Get_PowerPlant : IPowerPlant; safecall;
    procedure Set_PowerPlant (const Value: IPowerPlant); safecall;
    function Get_SpecifiedInflowFeature : ISpecifiedInflowFeature; safecall;
    procedure Set_SpecifiedInflowFeature (const Value: ISpecifiedInflowFeature); safecall;
    function Get_MasterControlFeature : IMasterControlFeature; safecall;
    procedure Set_MasterControlFeature (const Value: IMasterControlFeature); safecall;
    function Get_WaterDemandFeature : IWaterDemandFeature; safecall;
    procedure Set_WaterDemandFeature (const Value: IWaterDemandFeature); safecall;
    function Get_SourceName: WideString; safecall;
    function Get_SinkName: WideString; safecall;
    function Get_UpStreamNode : IReservoirData; safecall;
    function Get_DownStreamNode : IReservoirData; safecall;
    function Get_ChannelArea: integer; safecall;
    procedure Set_ChannelArea(AValue: integer); safecall;
    function Get_SelectedSwitchControlID: Integer; safecall;
    procedure Set_SelectedSwitchControlID(Value: Integer); safecall;
    function Get_TimeControl: IChannelTimeControl; safecall;
    function Get_SwitchControlByID (AChannelSwitchID : Integer): IChannelSwitchControl; safecall;
    function Get_SwitchControlByIndex (AIndex : Integer): IChannelSwitchControl; safecall;
    function Get_SwitchControlByChannelNumber(AChannelNumber: Integer): IChannelSwitchControl; safecall;
    function Get_SwitchControlCount: Integer; safecall;
    function Get_DisbenefitFunction: IDisbenefitFunctionDefinition; safecall;
    function CastSwitchControlByID (AChannelSwitchID : integer) : TChannelSwitchControl;
    function CastSwitchControlByIndex (AIndex : integer) : TChannelSwitchControl;
    function CastSwitchControlByChannelNumber(AChannelNumber : integer) : TChannelSwitchControl;
    function CastTimeControlByIndex (AIndex : integer) : TChannelTimeControl;
    function CastChannelTimeControlByNumber (AChannelNumber : integer) : TChannelTimeControl;
    function PopulateGeneralFlowChannel(AChannelID            : integer;
                                        AChannelNumber        : integer;
                                        AChannelType          : integer;
                                        AChannelSubType       : integer;
                                        AChannelName          : WideString;
                                        AUpStreamNodeNumber   : integer;
                                        ADownStreamNodeNumber : integer;
                                        AChannelPenaltyNumber : integer;
                                        ASummaryOutput        : WideString;
                                        AChannelAreaID        : integer;
                                        AFlowOutput,
                                        AFirmYieldAnalysis    : WideString): WordBool;
    function PopulateChannelPenalty (APenalty : IChannelPenalty): WordBool;
    function PopulateOutputChannelData(ASummaryOutputRequired,ARequiresFirmYieldAnalysis: WideString): WordBool;
    function DeleteAllFeatures            : WordBool; safecall;
    function DeletePumpingFeature         : WordBool; safecall;
    function DeletePhysicalFlowConstraint : WordBool; safecall;
    function DeleteIFRFeature             : WordBool; safecall;
    function DeleteMasterControlFeature   : WordBool; safecall;
    function DeleteSpecifiedInflowFeature : WordBool; safecall;
    function DeleteWaterDemandFeature     : WordBool; safecall;
    function CreateTimeControl            : TChannelTimeControl;
    function DeleteTimeControl            : Boolean;
    function NewTimeControl               : IChannelTimeControl; safecall;
    function RemoveTimeControl            : WordBool; safecall;
    function CreateSwitchControl (AChannelSwitchID : Integer) : TChannelSwitchControl;
    function DeleteSwitchControl (AChannelSwitchID : Integer) : Boolean;
    function NewSwitchControl             : IChannelSwitchControl; safecall;
    function RemoveSwitchControl (AChannelSwitchID : Integer) : WordBool; safecall;

    function CreateDisbenefitFunction : TDisbenefitFunctionData;
    function DeleteDisbenefitFunction : Boolean;
    function NewDisbenefitFunction: IDisbenefitFunctionDefinition; safecall;
    function RemoveDisbenefitFunction: WordBool; safecall;

    function Get_MultiResChannelCurtailByChannelNo(AChannelNo: Integer): IMultiResMultiChannelCurtail; safecall;
    function RemoveMultiResChannelCurtail(AID: Integer): WordBool; safecall;

    function NewMultiResCurChannel: IMultiResMultiChannelCurtail; safecall;

    function NewReturnFlowChannel: IReturnFlowChannel; safecall;
    function RemoveReturnFlowChannel: WordBool; safecall;
    function Get_ReturnFlowChannel: IReturnFlowChannel; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property ChannelID: integer read Get_ChannelID;
    property ChannelNumber: integer read Get_ChannelNumber;
    property ChannelName: WideString read Get_ChannelName write Set_ChannelName;
    property ChannelType: integer read Get_ChannelType write Set_ChannelType;
    property ChannelSubType: integer read Get_ChannelSubType write Set_ChannelSubType;
    property UpStreamNodeNumber: integer read Get_UpStreamNodeNumber write Set_UpStreamNodeNumber;
    property DownStreamNodeNumber: integer read Get_DownStreamNodeNumber write Set_DownStreamNodeNumber;
    property ChannelPenaltyNumber: integer read Get_ChannelPenaltyNumber;
    property ChannelPenalty: IChannelPenalty read Get_ChannelPenalty;
    property SummaryOutputRequired: WideString read Get_SummaryOutputRequired write Set_SummaryOutputRequired;
    property RequiresFirmYieldAnalysis: WideString read Get_RequiresFirmYieldAnalysis write Set_RequiresFirmYieldAnalysis;
    property ValidArcCounts : WideString read Get_ValidArcCounts;
    property MinimumFlowConstraint  :IMinimumFlowConstraint read Get_MinimumFlowConstraint write Set_MinimumFlowConstraint;
    property MinMaxFlowConstraint   : IMinMaxFlowConstraint read Get_MinMaxFlowConstraint write Set_MinMaxFlowConstraint;
    property PumpingFeature         : IPumpingFeature read Get_PumpingFeature write Set_PumpingFeature;
    property LossFeature            : ILossFeature read Get_LossFeature write Set_LossFeature;
    property SpecifiedDemandFeature : ISpecifiedDemandFeature read Get_SpecifiedDemandFeature write Set_SpecifiedDemandFeature;
    property DiversionFeature       : IDiversionFeature read Get_DiversionFeature write Set_DiversionFeature;
    property PhysicalFlowConstraint : IPhysicalFlowConstraint read Get_PhysicalFlowConstraint write Set_PhysicalFlowConstraint;
    property IFRFeature             : IIFRFeature read Get_IFRFeature write Set_IFRFeature;
    property IrrigationArea         : IIrrigationArea read Get_IrrigationArea write Set_IrrigationArea;
    property PowerPlant             : IPowerPlant read Get_PowerPlant write Set_PowerPlant;
    property SpecifiedInflowFeature : ISpecifiedInflowFeature read Get_SpecifiedInflowFeature write Set_SpecifiedInflowFeature;
    property MasterControlFeature   : IMasterControlFeature read Get_MasterControlFeature write Set_MasterControlFeature;
    property WaterDemandFeature     : IWaterDemandFeature read Get_WaterDemandFeature write Set_WaterDemandFeature;
    property SourceName: WideString read Get_SourceName;
    property SinkName: WideString read Get_SinkName;
    property UpStreamNode: IReservoirData read Get_UpStreamNode;
    property DownStreamNode: IReservoirData read Get_DownStreamNode;

    property MinimumFlowConstraintID  : integer read FMinimumFlowConstraintNr;
    property MinMaxFlowConstraintID   : integer read FMinMaxFlowConstraintNr;
    property PumpingFeatureID         : integer read FPumpingFeatureNr;
    property LossFeatureID            : integer read FLossFeatureNr;
    property SpecifiedDemandFeatureID : integer read FSpecifiedDemandFeatureNr;
    property DiversionFeatureID       : integer read FDiversionFeatureNr;
    property PhysicalFlowConstraintID : integer read FPhysicalFlowConstraintNr;
    property IFRFeatureID             : integer read FIFRFeatureNr;
    property IrrigationAreaID         : integer read FIrrigationAreaNr;
    property PowerPlantID             : integer read FPowerPlantNr;
    property SpecifiedInflowFeatureID : integer read FSpecifiedInflowFeatureNr;
    property MasterControlFeatureID   : integer read FMasterControlFeatureNr;
    property TimeControl              : IChannelTimeControl read Get_TimeControl;
    property SwitchControlByID [AChannelSwitchID : Integer]: IChannelSwitchControl read Get_SwitchControlByID;
    property SwitchControlByIndex [AIndex : Integer] : IChannelSwitchControl read Get_SwitchControlByIndex;
    property SwitchControlCount: Integer read Get_SwitchControlCount;
    property SelectedSwitchControlID : Integer  read Get_SelectedSwitchControlID write Set_SelectedSwitchControlID;
    property DisbenefitFunction: IDisbenefitFunctionDefinition read Get_DisbenefitFunction;
    property ReturnFlowChannel: IReturnFlowChannel read Get_ReturnFlowChannel;
    property FlowOutput: WideString read Get_FlowOutput write Set_FlowOutput;
    property SwitchControlByChannelNumber[AChannelNumber: Integer]: IChannelSwitchControl read Get_SwitchControlByChannelNumber;

    property MultiResChannelCurtailByChannelNo[AChannelNo: Integer]: IMultiResMultiChannelCurtail read Get_MultiResChannelCurtailByChannelNo;
  end;

  TChannelList = class(TAbstractAppObject, IChannelList)
  protected
    FChannelList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetCastChannelByIndex(AIndex: integer): TGeneralFlowChannel;
    function GetCastChannelByChannelNumber(AChannelNumber: integer): TGeneralFlowChannel;
    function GetCastChannelByChannelAreaID(AChannelAreaID: integer): TGeneralFlowChannel;
    function CastGeneralFlowChannelByID(AChannelID : integer): TGeneralFlowChannel;
    function AddGeneralFlowChannel (AChannel : TGeneralFlowChannel): boolean;
    function AddChannel(AChannel:TGeneralFlowChannel): Boolean;
    function ValidateMaximumZeroUpstreamNode (var AErrors : WideString; AContext : WideString='') : WordBool; safecall;
    function ValidateMaximumZeroDownstreamNode (var AErrors : WideString; AContext : WideString='') : WordBool; safecall;
  public
    function Initialise: Boolean; override;
    function NewGeneralFlowChannel : TGeneralFlowChannel;
    function CreateNewChannel : TGeneralFlowChannel;
    function DeleteGeneralFlowChannelWithID(AChannelID : integer) : WordBool;
    function DeleteGeneralFlowChannelWithIndex(AIndex : integer) : WordBool;
    function CopyChannel(AChannelNumber : integer) : IGeneralFlowChannel; safecall;

    procedure AddMonthlyConstraints(APenaltyNr: integer); safecall;
    procedure DeleteMonthlyConstraints(APenaltyNr : integer;AIndex     : integer); safecall;
    function CreateChannel : IGeneralFlowChannel; safecall;
    function RemoveChannelWithID(AChannelID : integer) : WordBool; safecall;
    function RemoveChannelWithNumber(AChannelNumber : integer) : WordBool; safecall;
    function MayChangePenaltyArcCount (APenaltyNr : integer; AArcCount  : integer) : WordBool; safecall;
    function Get_ChannelCount: integer; safecall;
    function Get_ChannelByIdentifier(AIdentifier: integer): IGeneralFlowChannel; safecall;
    function Get_ChannelByChannelNumber(AChannelNumber: integer): IGeneralFlowChannel; safecall;
    function Get_ChannelByIndex(AIndex: integer): IGeneralFlowChannel; safecall;
    function Get_OutputChannelCount: integer; safecall;
    function Get_ChannelByName(const AName: WideString): IGeneralFlowChannel; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function ChannelNumberFromChannelID(AChannelID : integer): integer;
    function ChannelIDFromChannelNumber(AChannelNumber : integer): integer;

    property ChannelCount: integer read Get_ChannelCount;
    property ChannelByIndex[AIndex: integer]: IGeneralFlowChannel read Get_ChannelByIndex;
    property ChannelByChannelNumber[AChannelNumber: integer]: IGeneralFlowChannel read Get_ChannelByChannelNumber;
    property OutputChannelCount: integer read Get_OutputChannelCount;
    property CastChannelByChannelNumber[AIndex: integer]: TGeneralFlowChannel read GetCastChannelByChannelNumber;
    property CastChannelByIndex[AIndex: integer]: TGeneralFlowChannel read GetCastChannelByIndex;
    property CastChannelNumberFromChannelID[AIndex: integer]: TGeneralFlowChannel read GetCastChannelByChannelAreaID;

  end;

implementation

{ TChannelPenaltyValue }

uses
  System.Types,
  SysUtils,
  UConstants,
  UDataSetType,
  UChannelDataSQLAgent,
  UChannelPlanningLoadAgent,
//  UDisbenefitFunctionDefinitionLoadAgent,
  UDisbenefitFunctionDefinitionSQLAgent,
  UReturnFlowChannelSQLAgent,
  UErrorHandlingOperations,
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UChannelPenaltyStructureDataLoadAgent;

const
  CCreateSpecifiedInflowFeature : array[0..2] of WideString = ('Data','ChannelFeatures','CreateSpecifiedInflowFeature');

{******************************************************************************}
{* TChannelPenalty                                                            *}
{******************************************************************************}

function TChannelPenalty._AddRef: Integer;
const OPNAME = 'TChannelPenalty._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty._Release: Integer;
const OPNAME = 'TChannelPenalty._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.ValidateForChannels(AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TChannelPenalty.ValidateForChannels';
var
  lChannelList : TChannelList;
  lIndex       : integer;
  lChannel     : TGeneralFlowChannel;
  lResultA     : Boolean;
  lResultB     : Boolean;
  lErrorA      : string;
  lErrorB      : string;
  lErrorStr    : string;
  lArcCount    : integer;
begin
  Result := FALSE;
  try
    lResultA := TRUE;
    lResultB := TRUE;
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      CastNetworkElementData.CastChannelList;
    lArcCount  := ChannelPenaltyArcCount;
    lIndex := 0;
    lErrorA := '';
    lErrorB := '';
    while (lIndex < lChannelList.ChannelCount) do
    begin
      lChannel := lChannelList.CastChannelByIndex[lIndex];
      if ((lChannel.ChannelPenalty <> nil) AND (ChannelPenaltyID = lChannel.ChannelPenalty.ChannelPenaltyID)) then
      begin
        if (NOT(lChannel.ArcCountValid(lArcCount))) then
        begin
          lResultA := FALSE;
          if (lErrorA = '') then
            lErrorA := lErrorA + ' ' + IntToStr(lChannel.ChannelNumber)
          else
            lErrorA := lErrorA + ', ' + IntToStr(lChannel.ChannelNumber);
        end
        else
        if ((lChannel.IFRFeature <> nil) AND
            (ChannelPenaltyValueByIndex[1] >= ChannelPenaltyValueByIndex[2])) then
        begin
          lResultB := FALSE;
          if (lErrorB = '') then
            lErrorB := lErrorB + ' ' + IntToStr(lChannel.ChannelNumber)
          else
            lErrorB := lErrorB + ', ' + IntToStr(lChannel.ChannelNumber);
        end;
      end;
      lIndex := lIndex + 1
    end;
    if (NOT lResultA) then
    begin
      lErrorStr := FAppModules.Language.GetString('ContextValidation.InvalidPenaltyArcCount');
      AErrorMessages.Add('ERROR:' +Format(lErrorStr, [IntToStr(ChannelPenaltyID), lErrorA]) + '.');
    end;
    if (NOT lResultB) then
    begin
      lErrorStr := FAppModules.Language.GetString('ContextValidation.InvalidPenaltyValue');
      AErrorMessages.Add('ERROR:' +Format(lErrorStr, [IntToStr(ChannelPenaltyID), lErrorB]) + '.');
    end;
    if (lResultA AND lResultB) then
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenalty.CreateMemberObjects;
const OPNAME = 'TChannelPenalty.CreateMemberObjects';
var
  LPenaltyValues : TAbstractFieldProperty;
begin
  inherited;
  try
    LPenaltyValues := FAppModules.FieldProperties.FieldProperty('Penalty');
    if (LPenaltyValues = nil) then
      raise Exception.Create('Field (Penalty) not found in field properties');
    SetLength(FPenaltyValues, LPenaltyValues.ArrayLength);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenalty.DestroyMemberObjects;
const OPNAME = 'TChannelPenalty.DestroyMemberObjects';
begin
  try
    Finalize(FPenaltyValues);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Initialise: Boolean;
const OPNAME = 'TChannelPenalty.Initialise';
var
  LIndex: integer;
  LPenaltyValuesField: TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelPenaltyID := NullInteger;
    FPenaltyName      := '';
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    for LIndex := LPenaltyValuesField.ArrayLow to LPenaltyValuesField.ArrayHigh do
      FPenaltyValues[LIndex] := NullFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Populate (APenaltyID     : integer;
                                   APenaltyName   : WideString;
                                   APenaltyValues : TChannelPenaltyValuesArray) : WordBool;
const OPNAME = 'TChannelPenalty.Populate';
var
  LIndex: integer;
  LPenaltyValuesField: TAbstractFieldProperty;
begin
  Result := False;
  try
    FChannelPenaltyID := APenaltyID;
    FPenaltyName      := APenaltyName;
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    for LIndex := LPenaltyValuesField.ArrayLow to LPenaltyValuesField.ArrayHigh do
      FPenaltyValues[LIndex] := APenaltyValues[lIndex];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Get_ChannelPenaltyArcCount: integer;
const OPNAME = 'TChannelPenalty.Get_ChannelPenaltyArcCount';
var
  LIndex: integer;
  LPenaltyValuesField: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    for LIndex := LPenaltyValuesField.ArrayLow to LPenaltyValuesField.ArrayHigh do
      if (FPenaltyValues[LIndex] > NullFloat) then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Get_ChannelPenaltyID: integer;
const OPNAME = 'TChannelPenalty.Get_ChannelPenaltyID';
begin
  Result := NullInteger;
  try
    Result := FChannelPenaltyID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Get_ChannelPenaltyName : WideString;
const OPNAME = 'TChannelPenalty.Get_ChannelPenaltyName';
begin
  Result := '';
  try
    Result := FPenaltyName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Get_ChannelPenaltyValueByIndex(AIndex: integer): double;
const OPNAME = 'TChannelPenalty.Get_ChannelPenaltyValueByIndex';
var
  LPenaltyValuesField: TAbstractFieldProperty;
begin
  Result := NullFloat;
  try
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    if (AIndex >= LPenaltyValuesField.ArrayLow) and (AIndex <= LPenaltyValuesField.ArrayHigh) then
    begin
      if (FPenaltyValues[AIndex] = NullFloat) then
        Result := NullFloat
      else
        Result := StrToFloat(FAppModules.Changes.GetParameterValue
                                              ('Penalty',
                                              GetKeyValues('Penalty', IntToStr(AIndex)),
                                              FloatToStr(FPenaltyValues[AIndex]),
                                              IntToStr(AIndex)));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Get_ChannelPenaltyValues : TChannelPenaltyValuesArray;
const OPNAME = 'TChannelPenalty.Get_ChannelPenaltyValues';
begin
  Result := FPenaltyValues;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenalty.Set_ChannelPenaltyValueByIndex (AIndex : integer;
                                                          AValue : double);
const OPNAME = 'TChannelPenalty.Set_ChannelPenaltyValueByIndex';
var
  LArcCount  : integer;
  LNullIndex : integer;
  LIndex     : integer;
  LPenaltyValuesField: TAbstractFieldProperty;
begin
  try
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    if (AIndex >= LPenaltyValuesField.ArrayLow) and (AIndex <= LPenaltyValuesField.ArrayHigh) then
    begin
      LArcCount := ChannelPenaltyArcCount;
      if ((AValue = NullFloat) AND (LArcCount = 1)) then
        AValue := 0;
      if (AValue <> FPenaltyValues[AIndex]) then
      begin
        if (FPenaltyValues[AIndex] = NullFloat) then
        begin
          AIndex    := IndexOfFirstNullPenaltyValue;
          { Add monthly flow constraint for min-max channels when adding a penalty arc. }
          TYieldModelDataObject(FAppModules.Model.ModelData).
            CastNetworkElementData.CastChannelList.AddMonthlyConstraints(FChannelPenaltyID);
        end;
        UpdatePenaltyValue(AIndex, AValue);
        if (AValue = NullFloat) then
        begin
          { Delete monthly flow constraint for min-max channels when deleting a penalty arc.}
          TYieldModelDataObject(FAppModules.Model.ModelData).
            CastNetworkElementData.CastChannelList.
              DeleteMonthlyConstraints(FChannelPenaltyID, AIndex);
        end;

        LNullIndex := IndexOfFirstNullPenaltyValue;
        if (LNullIndex >= LPenaltyValuesField.ArrayLow) AND (LNullIndex <= LPenaltyValuesField.ArrayHigh) then
        begin
          for LIndex := IndexOfFirstNullPenaltyValue + 1 to LPenaltyValuesField.ArrayHigh do
          begin
            if (FPenaltyValues[LIndex] >= 0) then
            begin
              UpdatePenaltyValue(LNullIndex, FPenaltyValues[LIndex]);
              UpdatePenaltyValue(LIndex, NullFloat);
              LNullIndex := LNullIndex + 1;
            end;
          end;
        end;
        if (LArcCount <> ChannelPenaltyArcCount) then
          UpdateChannelArcCount(ChannelPenaltyArcCount);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenalty.Set_ChannelPenaltyName (const AName: WideString);
const OPNAME = 'TChannelPenalty.Set_ChannelPenaltyName';
var
  LLoadAgent   : TChannelPenaltyStructureDataLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TChannelPenaltyStructureDataLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelPenaltyContextData
          (LContextData, IntToStr(FChannelPenaltyID));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelPenaltyName', AName, FPenaltyName, LContextData)) then
        begin
          LOldValue := FPenaltyName;
          FPenaltyName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelPenaltyName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenalty.UpdatePenaltyValue (AIndex : integer;
                                              AValue : double);
const OPNAME = 'TChannelPenalty.UpdatePenaltyValue';
var
  LLoadAgent    : TChannelPenaltyStructureDataLoadAgent;
  LContextData  : TStringList;
  LOldValue     : double;
begin
  try
    LLoadAgent := TChannelPenaltyStructureDataLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelPenaltyValueContextData
          (LContextData, IntToStr(FChannelPenaltyID), IntToStr(AIndex));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'Penalty', FloatToStr(AValue), FloatToStr(FPenaltyValues[AIndex]), LContextData)) then
        begin
          LOldValue :=  FPenaltyValues[AIndex];
          FPenaltyValues[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Penalty',FloatToStr(LOldValue), FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenalty.UpdateChannelArcCount(ANewCount: integer);
const OPNAME = 'TChannelPenalty.UpdateChannelArcCount';
var
  LChanneloadAgent:TChannelDataSQLAgent;
begin
  try
    LChanneloadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LChanneloadAgent.UpdateChannelArcCount(FChannelPenaltyID,ANewCount);
    finally
      LChanneloadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.IndexOfFirstNullPenaltyValue: integer;
const OPNAME = 'TChannelPenalty.IndexOfFirstNullPenaltyValue';
var
  LIndex: integer;
  LPenaltyValuesField: TAbstractFieldProperty;
begin
  Result := NullInteger;
  try
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    for LIndex := LPenaltyValuesField.ArrayLow to LPenaltyValuesField.ArrayHigh do
    begin
      if (FPenaltyValues[LIndex] = NullFloat) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.GetKeyValues (const AParamField : WideString;
                                       const AFieldIndex : WideString) : WideString;
const OPNAME = 'TChannelPenalty.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FChannelPenaltyID)
    else
      Result := Result + ',Identifier=' + IntToStr(FChannelPenaltyID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenalty.GetBaseValue (const AParamField: WideString;
                                       const AFieldIndex: WideString): WideString;
const OPNAME = 'TChannelPenalty.GetBaseValue';
var
  lFieldProperty : TAbstractFieldProperty;
  lFormatStr     : string;
begin
  Result := '';
  try
    lFormatStr := '';
    lFieldProperty := FAppModules.FieldProperties.FieldProperty(AParamField);
    if (lFieldProperty <> nil) then
      lFormatStr := lFieldProperty.FormatStringGrid;

    if (AParamField = 'Penalty') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FPenaltyValues[StrToInt(AFieldIndex)])
      else
        Result := Format(lFormatStr, [FPenaltyValues[StrToInt(AFieldIndex)]]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenalty.Validate(var AErrors: WideString;  const AContext: WideString): WordBool;
const OPNAME = 'TChannelPenalty.Validate';
var
  lErrorList : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'ForChannels') then
        Result := ValidateForChannels(lErrorList)
      else
      begin
        Result := TRUE;
        if (NOT ValidateForChannels(lErrorList)) then
          Result := FALSE;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TChannelPenaltyList                                                        *}
{******************************************************************************}

function TChannelPenaltyList._AddRef: Integer;
const OPNAME = 'TChannelPenaltyList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList._Release: Integer;
const OPNAME = 'TChannelPenaltyList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenaltyList.CreateMemberObjects;
const OPNAME = 'TChannelPenaltyList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FChannelPenaltyList := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenaltyList.DestroyMemberObjects;
const OPNAME = 'TChannelPenaltyList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChannelPenaltyList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.Initialise: boolean;
const OPNAME = 'TChannelPenaltyList.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelPenaltyList.Clear;
    FInflowPenaltyNo := 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.GetCastChannelPenaltyByIndex(AIndex: integer): TChannelPenalty;
const OPNAME = 'TChannelPenaltyList.GetCastChannelPenaltyByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FChannelPenaltyList.Count) then
      Result := TChannelPenalty(FChannelPenaltyList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.Get_ChannelPenaltyByIndex(AIndex : integer): IChannelPenalty;
const OPNAME = 'TChannelPenaltyList.Get_ChannelPenaltyByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FChannelPenaltyList.Count) then
      Result := TChannelPenalty(FChannelPenaltyList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.Get_ChannelPenaltyCount: integer;
const OPNAME = 'TChannelPenaltyList.Get_ChannelPenaltyCount';
begin
  Result := 0;
  try
    Result := FChannelPenaltyList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.Get_InflowPenaltyNo: Integer;
const OPNAME = 'TChannelPenaltyList.Get_InflowPenaltyNo';
begin
  Result := 0;
  try
    Result := FInflowPenaltyNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.AddChannelPenalty (APenalty : TChannelPenalty): boolean;
const OPNAME = 'TChannelPenaltyList.AddChannelPenalty';
begin
  Result := False;
  try
    if (APenalty <> nil) then
    begin
      FChannelPenaltyList.Add(APenalty);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.Get_ChannelPenaltyByIdentifier(AIdentifier: integer): IChannelPenalty;
const OPNAME = 'TChannelPenaltyList.Get_ChannelPenaltyByIdentifier';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FChannelPenaltyList.Count - 1 do
      if (TChannelPenalty(FChannelPenaltyList[LIndex]).ChannelPenaltyID = AIdentifier) then
      begin
        Result := TChannelPenalty(FChannelPenaltyList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.GetCastChannelPenaltyByIdentifier(AIdentifier: integer): TChannelPenalty;
const OPNAME = 'TChannelPenaltyList.GetCastChannelPenaltyByIdentifier';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FChannelPenaltyList.Count - 1 do
      if (TChannelPenalty(FChannelPenaltyList[LIndex]).ChannelPenaltyID = AIdentifier) then
      begin
        Result := TChannelPenalty(FChannelPenaltyList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.NewChannelPenalty : TChannelPenalty;
const OPNAME = 'TChannelPenaltyList.NewChannelPenalty';
var
  lFeature : TChannelPenalty;
begin
  Result := nil;
  try
    lFeature := TChannelPenalty.Create(FAppModules);
    AddChannelPenalty(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.CreateNewChannelPenalty : TChannelPenalty;
const OPNAME = 'TChannelPenaltyList.CreateNewChannelPenalty';
var
  LIndex              : integer;
  LNewIdentifier      : integer;
  LLoadAgent          : TChannelDataSQLAgent;
  LPenaltyStructure   : TChannelPenalty;
  lPenaltyValues      : TChannelPenaltyValuesArray;
  LPenaltyValuesField : TAbstractFieldProperty;
  lPenaltyName        : string;
begin
  Result := nil;
  try
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    if (LPenaltyValuesField = nil) then
      raise Exception.Create('Field (Penalty) not found in field properties');
    SetLength(lPenaltyValues, LPenaltyValuesField.ArrayLength); ;
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LNewIdentifier := 0;
      for LIndex := LPenaltyValuesField.ArrayLow to LPenaltyValuesField.ArrayHigh do
        lPenaltyValues[lIndex] := NullFloat;
      lPenaltyValues[LPenaltyValuesField.ArrayLow] := 0;
      if LLoadAgent.InsertChannelPenalty(LNewIdentifier) then
      begin
        LPenaltyStructure := NewChannelPenalty;
        LPenaltyStructure.Initialise;
        lPenaltyName := Uppercase(FAppModules.Language.GetString('TField.Cpenalty')) + ' ' + IntToStr(LNewIdentifier);
        LPenaltyStructure.Populate(LNewIdentifier, lPenaltyName, lPenaltyValues);
        Result := LPenaltyStructure;
        FAppModules.Model.StudyDataHasChanged(sdccAdd,'ChannelPenalty','',IntToStr(LPenaltyStructure.ChannelPenaltyID));
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.CreateChannelPenalty : IChannelPenalty;
const OPNAME = 'TChannelPenaltyList.CreateChannelPenalty';
var
  LPenaltyStructure   : TChannelPenalty;
begin
  Result := nil;
  try
    LPenaltyStructure := CreateNewChannelPenalty;
    Result := LPenaltyStructure;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.DeleteChannelPenaltyWithID (APenaltyID : integer) : WordBool;
const OPNAME = 'TChannelPenaltyList.DeleteChannelPenaltyWithID';
var
  lPenalty : TChannelPenalty;
begin
  Result := FALSE;
  try
    lPenalty := CastChannelPenaltyByIdentifier[APenaltyID];
    if (lPenalty <> nil) then
    begin
        FChannelPenaltyList.Remove(lPenalty);
        Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.DeleteChannelPenaltyWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TChannelPenaltyList.DeleteChannelPenaltyWithIndex';
var
  lPenalty : TChannelPenalty;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) and(AIndex < FChannelPenaltyList.Count) then
    begin
      lPenalty := TChannelPenalty(FChannelPenaltyList.Items[AIndex]);
      FChannelPenaltyList.Remove(lPenalty);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.RemoveChannelPenaltyWithID(APenaltyID: integer): WordBool;
const OPNAME = 'TChannelPenaltyList.RemoveChannelPenaltyWithID';
var
  LLoadAgent        : TChannelDataSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.DeleteChannelPenalty(APenaltyID) then
      begin
        Result := DeleteChannelPenaltyWithID(APenaltyID);
        FAppModules.Model.StudyDataHasChanged(sdccDelete,'ChannelPenalty',IntToStr(APenaltyID),'');
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelPenaltyList.Set_InflowPenaltyNo(Value: Integer);
const OPNAME = 'TChannelPenaltyList.Set_InflowPenaltyNo';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
  LIndex : integer;
  LChannel : IGeneralFlowChannel;
  LPenalty : IChannelPenalty;
begin
  if FInflowPenaltyNo = Value then
    exit;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelConfigContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'InflowPenaltyNo', IntToStr(Value), IntToStr(FInflowPenaltyNo), LContextData) then
        begin
          LOldValue := IntToStr(FInflowPenaltyNo);
          FInflowPenaltyNo := Value;
          LPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelPenaltyList.ChannelPenaltyByIdentifier[FInflowPenaltyNo];
          for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkElementData.ChannelList.ChannelCount-1 do
          begin
            LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkElementData.ChannelList.ChannelByIndex[LIndex];
            if (LChannel <> nil) and (LChannel.ChannelType = 36) then
              LChannel.ChannelPenalty :=  LPenalty;
          end;

          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InflowPenaltyNo',LOldValue,IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelPenaltyList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TChannelPenaltyList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FChannelPenaltyList.Count - 1 do
    begin
      if (NOT ChannelPenaltyByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) and lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TGeneralFlowChannel                                                        *}
{******************************************************************************}

function TGeneralFlowChannel._AddRef: Integer;
const OPNAME = 'TGeneralFlowChannel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel._Release: Integer;
const OPNAME = 'TGeneralFlowChannel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool;
const OPNAME = 'TGeneralFlowChannel.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'ChannelNumber') then
        Result := ValidateChannelNumber(lErrorList)
      else
      if (AContext = 'ChannelName') then
        Result := ValidateChannelName(lErrorList)
      else
      if (AContext = 'ChannelPenalty') then
        Result := ValidateChannelPenalty(lErrorList)
      else
      if (AContext = 'UpDownNotSameNode') then
         Result := ValidateChannelUpDownNotSameNode(lErrorList)
      else
      if (AContext = 'UpNodeNumber') then
        Result := ValidateChannelUpstreamNode(lErrorList)
      else
      if (AContext = 'DownNodeNumber') then
        Result := ValidateChannelDownstreamNode(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateChannelNumber(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateChannelName(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateChannelPenalty(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateChannelUpstreamNode(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateChannelDownstreamNode(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateChannelUpDownNotSameNode(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateChannelTariffCalculation(lErrorList)) then
            Result := FALSE;
        end;

      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannel.ValidateChannelName (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TGeneralFlowChannel.ValidateChannelName';
var
  lChannelList : TChannelList;
  lMessage     : string;
  lUnique      : Boolean;
  lIndex       : integer;
  lSomeChannel : TGeneralFlowChannel;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelName', FChannelName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FChannelName+ lMessage)
    else
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkElementData.CastChannelList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lChannelList.ChannelCount)) do
      begin
        lSomeChannel := lChannelList.CastChannelByIndex[lIndex];
        if ((FChannelNumber <> lSomeChannel.ChannelNumber) AND
            (UpperCase(Trim(FChannelName)) = UpperCase(Trim(lSomeChannel.ChannelName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateChannelName');
          AErrorMessages.Add('WARNING:' +Format(lMessage, [FChannelName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.ValidateChannelPenalty (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TGeneralFlowChannel.ValidateChannelPenalty';
var
  lMessage        : string;
  lChannelPenalty : IChannelPenalty;
begin
  Result := FALSE;
  try
    lMessage   := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
              ('PenaltyNumber', IntToStr(FChannelPenaltyNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +FChannelName+ ':'+lMessage)
    else
    begin
      if (FChannelPenaltyNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidChannelPenaltyNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
      end
      else
      begin
        lChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ChannelPenaltyList.ChannelPenaltyByIdentifier[FChannelPenaltyNumber];
        if (lChannelPenalty = nil) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidChannelPenaltyNumber');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
        end
        else
        if (NOT(ArcCountValid(lChannelPenalty.ChannelPenaltyArcCount))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidChannelPenaltyArcCount');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
        end
        else
        if (FIFRFeatureNr <> 0) then
        begin
          if (lChannelPenalty.ChannelPenaltyValueByIndex[1] >= lChannelPenalty.ChannelPenaltyValueByIndex[2]) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.IFRFeaturePenaltyRelaxArc');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]{PowerPlant.FeatureName]}));
          end
          else
            Result := TRUE
        end
        else
          Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.ValidateChannelTariffCalculation(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TGeneralFlowChannel.ValidateChannelUpstreamNode';
var
  lMessage     : string;
begin
  Result := FALSE;
  try
    if(FAppModules.Model.ModelName <> CPlanning) then
      Result := True
    else
    begin
      if(FMasterControlFeatureNr <> 0) and (Get_TariffCalculation = nil) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.MasterControlNoTariffCalculation');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
      end
      else
        Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.ValidateChannelUpstreamNode (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TGeneralFlowChannel.ValidateChannelUpstreamNode';
var
  lMessage     : string;
  lNode        : IReservoirData;
  lChannelList : IChannelList;
  lIndex       : integer;
  lFound       : boolean;
begin
  Result := FALSE;
  try
    lMessage  := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
               ('UpNodeNumber', IntToStr(FUpStreamNodeNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +FChannelName+ ':'+lMessage)
    else
    begin
      if (FUpStreamNodeNumber = 0) then
      begin
        if (FMasterControlFeatureNr <> 0) and (FAppModules.Model.ModelName <> CPlanning)then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.MasterControlUpstreamNodeZero');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
        end
        else
          Result := TRUE;
      end
      else
      begin
        lNode := UpStreamNode;
        if (lNode = nil) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamNodeNumber');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
        end
        else
        begin
          { Node without inflow }
          if (lNode.ReservoirConfigurationData.NodeType in (NodesWithoutInflowSet-[ntMineNode])) then
          begin
            lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ChannelList;
            lIndex := 0;
            lFound := FALSE;
            while ((NOT lFound) AND (lIndex < lChannelList.ChannelCount)) do
            begin
              if (FUpStreamNodeNumber = lChannelList.ChannelByIndex[lIndex].DownStreamNodeNumber) then
                lFound := TRUE
              else
                lIndex := lIndex + 1;
            end;
            if (NOT lFound) then
            begin
              lMessage := FAppModules.Language.GetString('ContextValidation.DanglingUpstreamNode');
              AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName,FUpStreamNodeNumber]));
            end;
            Result := lFound;
          end
          else
            Result := TRUE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.ValidateChannelDownstreamNode (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TGeneralFlowChannel.ValidateChannelDownstreamNode';
var
  lMessage     : string;
  lNode        : IReservoirData;
  lChannelList : IChannelList;
  lIndex       : integer;
  lFound       : Boolean;
begin
  Result := FALSE;
  try
    lMessage    := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
               ('DownNodeNumber', IntToStr(FDownStreamNodeNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +FChannelName+ ':'+lMessage)
    else
    begin
      if (FDownStreamNodeNumber = 0) then
      begin
        if (FChannelType = 10) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDownstreamNodeNumber');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
        end
        else
          Result := TRUE;
      end
      else
      begin
        lNode := DownStreamNode;
        if ((lNode = nil) OR
            ((FChannelType = 10) AND (lNode.ReservoirConfigurationData.NodeType in (NodesWithoutInflowSet-[ntMineNode])))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDownstreamNodeNumber');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
        end
        else
        begin
          lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ChannelList;
          lIndex := 0;
          lFound := FALSE;
          while ((NOT lFound) AND (lIndex < lChannelList.ChannelCount)) do
          begin
            if (FDownStreamNodeNumber = lChannelList.ChannelByIndex[lIndex].UpStreamNodeNumber) then
              lFound := TRUE
            else
              lIndex := lIndex + 1;
          end;
          if (NOT lFound) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.DanglingDownstreamNode');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName,FDownStreamNodeNumber]));
          end;
          Result := lFound;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.ValidateChannelUpDownNotSameNode (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TGeneralFlowChannel.ValidateChannelUpDownNotSameNode';
var
  lMessage     : string;
begin
  Result := FALSE;
  try
    lMessage    := '';
    if (FUpStreamNodeNumber = FDownStreamNodeNumber) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.ChannelStartAndStopAtSameNode');
      AErrorMessages.Add('ERROR:' +Format(lMessage, [FChannelName]));
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.ValidateChannelNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TGeneralFlowChannel.ValidateChannelNumber';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelNumber',
            IntToStr(FChannelNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +FChannelName+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.CreateMemberObjects;
const OPNAME = 'TGeneralFlowChannel.CreateMemberObjects';
begin
  inherited;
  try
    FSwitchControls := TObjectList.Create(TRUE);
    FTimeControlList := TObjectList.Create(False);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.DestroyMemberObjects;
const OPNAME = 'TGeneralFlowChannel.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSwitchControls);
    FreeAndNil(FTimeControlList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Initialise: boolean;
const OPNAME = 'TGeneralFlowChannel.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelID                     := NullInteger;
    FChannelNumber                 := NullInteger;
    FChannelName                   := '';
    FChannelType                   := NullInteger;
    FChannelSubType                := NullInteger;
    FUpStreamNodeNumber            := NullInteger;
    FDownStreamNodeNumber          := NullInteger;
    FChannelPenaltyNumber          := 0;
    FSummaryOutputRequired         := 'Y';
    FRequiresFirmYieldAnalysis     := 'N';
    FFlowOutput                    := 'Y';
    FMinimumFlowConstraintNr       := 0;
    FMinMaxFlowConstraintNr        := 0;
    FPumpingFeatureNr              := 0;
    FLossFeatureNr                 := 0;
    FSpecifiedDemandFeatureNr      := 0;
    FDiversionFeatureNr            := 0;
    FPhysicalFlowConstraintNr      := 0;
    FIFRFeatureNr                  := 0;
    FIrrigationAreaNr              := 0;
    FPowerPlantNr                  := 0;
    FSpecifiedInflowFeatureNr      := 0;
    FMasterControlFeatureNr        := 0;
    FSwitchControls.Clear;
    FTimeControlList.Clear;
    FSelectedSwitchControlID       := 0;
    FMultiResChannelCurtailID      := 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ChannelID: integer;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelID';
begin
  Result := NullInteger;
  try
    Result := FChannelID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ChannelNumber: integer;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelNumber';
begin
  Result := NullInteger;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ChannelName: WideString;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelName';
begin
  Result := '';
  try
    Result := FChannelName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ChannelType: integer;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelType';
begin
  Result := NullInteger;
  try
    Result := FChannelType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ChannelSubType: integer;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelSubType';
begin
  Result := NullInteger;
  try
    Result := FChannelSubType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_ChannelType (AType: integer);
const OPNAME = 'TGeneralFlowChannel.Set_ChannelType';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelType', IntToStr(AType), IntToStr(FChannelType), LContextData) then
        begin
          LOldValue    := FChannelType;
          FChannelType := AType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelType',IntToStr(LOldValue),IntToStr(AType));
          if(FRequiresFirmYieldAnalysis = 'Y') and
           (not (ChannelType in [ctMinimumFlowChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel,ctDemandCentreReturnFlowChannel])) then
           RequiresFirmYieldAnalysis := '';
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_ChannelSubType(ASubType: integer);
const OPNAME = 'TGeneralFlowChannel.Set_ChannelSubType';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelSubType', IntToStr(ASubType), IntToStr(FChannelSubType), LContextData) then
        begin
          LOldValue    := FChannelSubType;
          FChannelSubType := ASubType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelSubType',IntToStr(LOldValue),IntToStr(ASubType));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ChannelPenalty : IChannelPenalty;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelPenalty';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelPenaltyList.ChannelPenaltyByIdentifier[FChannelPenaltyNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_DownStreamNodeNumber: integer;
const OPNAME = 'TGeneralFlowChannel.Get_DownStreamNodeNumber';
begin
  Result := NullInteger;
  try
    Result := FDownStreamNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_RequiresFirmYieldAnalysis: WideString;
const OPNAME = 'TGeneralFlowChannel.Get_RequiresFirmYieldAnalysis';
begin
  Result := '';
  try
    Result := FRequiresFirmYieldAnalysis;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_SummaryOutputRequired: WideString;
const OPNAME = 'TGeneralFlowChannel.Get_SummaryOutputRequired';
begin
  Result := '';
  try
    Result := FSummaryOutputRequired;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_UpStreamNodeNumber: integer;
const OPNAME = 'TGeneralFlowChannel.Get_UpStreamNodeNumber';
begin
  Result := NullInteger;
  try
    Result := FUpStreamNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_MinimumFlowConstraint : IMinimumFlowConstraint;
const OPNAME = 'TGeneralFlowChannel.Get_MinimumFlowConstraint';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                MinimumFlowConstraintList.MinimumFlowConstraintByID[FMinimumFlowConstraintNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_MinimumFlowConstraint (const Value : IMinimumFlowConstraint);
const OPNAME = 'TGeneralFlowChannel.Set_MinimumFlowConstraint';
begin
  try
    FMinimumFlowConstraintNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_MinMaxFlowConstraint : IMinMaxFlowConstraint;
const OPNAME = 'TGeneralFlowChannel.Get_MinMaxFlowConstraint';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FMinMaxFlowConstraintNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_MinMaxFlowConstraint (const Value: IMinMaxFlowConstraint);
const OPNAME = 'TGeneralFlowChannel.Set_MinMaxFlowConstraint';
begin
  try
    FMinMaxFlowConstraintNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_PumpingFeature : IPumpingFeature;
const OPNAME = 'TGeneralFlowChannel.Get_PumpingFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                PumpingFeatureList.PumpingFeatureByID[FPumpingFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_PumpingFeature (const Value: IPumpingFeature);
const OPNAME = 'TGeneralFlowChannel.Set_PumpingFeature';
begin
  try
    FPumpingFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_LossFeature : ILossFeature;
const OPNAME = 'TGeneralFlowChannel.Get_LossFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                LossFeatureList.LossFeatureByID[FLossFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_LossFeature (const Value: ILossFeature);
const OPNAME = 'TGeneralFlowChannel.Set_LossFeature';
begin
  try
    FLossFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_MasterControlFeature : IMasterControlFeature;
const OPNAME = 'TGeneralFlowChannel.Get_MasterControlFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                MasterControlFeatureList.MasterControlFeatureByID[FMasterControlFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_MasterControlFeature (const Value: IMasterControlFeature);
const OPNAME = 'TGeneralFlowChannel.Set_MasterControlFeature';
begin
  try
    FMasterControlFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_WaterDemandFeature : IWaterDemandFeature;
const OPNAME = 'TGeneralFlowChannel.Get_WaterDemandFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                WaterDemandFeatureList.WaterDemandFeatureByID[FwaterDemandFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_WaterDemandFeature (const Value: IWaterDemandFeature);
const OPNAME = 'TGeneralFlowChannel.Set_WaterDemandFeature';
begin
  try
    FWaterDemandFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_SpecifiedDemandFeature : ISpecifiedDemandFeature;
const OPNAME = 'TGeneralFlowChannel.Get_SpecifiedDemandFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FSpecifiedDemandFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_SpecifiedDemandFeature (const Value: ISpecifiedDemandFeature);
const OPNAME = 'TGeneralFlowChannel.Set_SpecifiedDemandFeature';
begin
  try
    FSpecifiedDemandFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_DiversionFeature : IDiversionFeature;
const OPNAME = 'TGeneralFlowChannel.Get_DiversionFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                DiversionFeatureList.DiversionFeatureByID[FDiversionFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_DiversionFeature (const Value: IDiversionFeature);
const OPNAME = 'TGeneralFlowChannel.Set_DiversionFeature';
begin
  try
    FDiversionFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_SpecifiedInflowFeature : ISpecifiedInflowFeature;
const OPNAME = 'TGeneralFlowChannel.Get_SpecifiedInflowFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FSpecifiedInflowFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_SourceName: WideString;
const OPNAME = 'TGeneralFlowChannel.Get_SourceName';
begin
  try
    Result := FChannelName + ' SOURCE';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_SinkName: WideString;
const OPNAME = 'TGeneralFlowChannel.Get_SinkName';
begin
  try
    Result := FChannelName + ' SINK';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_SpecifiedInflowFeature (const Value: ISpecifiedInflowFeature);
const OPNAME = 'TGeneralFlowChannel.Set_SpecifiedInflowFeature';
begin
  try
    FSpecifiedInflowFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_PhysicalFlowConstraint : IPhysicalFlowConstraint;
const OPNAME = 'TGeneralFlowChannel.Get_PhysicalFlowConstraint';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FPhysicalFlowConstraintNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_PhysicalFlowConstraint (const Value: IPhysicalFlowConstraint);
const OPNAME = 'TGeneralFlowChannel.Set_PhysicalFlowConstraint';
begin
  try
    FPhysicalFlowConstraintNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_IFRFeature : IIFRFeature;
const OPNAME = 'TGeneralFlowChannel.Get_IFRFeature';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                IFRFeatureList.MonthlyIFRFeatureByID[FIFRFeatureNr];
    if Result = nil then
      Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                IFRFeatureList.AnnualIFRFeatureByID[FIFRFeatureNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_IFRFeature (const Value: IIFRFeature);
const OPNAME = 'TGeneralFlowChannel.Set_IFRFeature';
begin
  try
    FIFRFeatureNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_IrrigationArea : IIrrigationArea;
const OPNAME = 'TGeneralFlowChannel.Get_IrrigationArea';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                IrrigationAreaList.IrrigationAreaByID[FIrrigationAreaNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_IrrigationArea (const Value: IIrrigationArea);
const OPNAME = 'TGeneralFlowChannel.Set_IrrigationArea';
begin
  try
    FIrrigationAreaNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_PowerPlant : IPowerPlant;
const OPNAME = 'TGeneralFlowChannel.Get_PowerPlant';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                PowerPlantList.PowerPlantByID[FPowerPlantNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_PowerPlant (const Value: IPowerPlant);
const OPNAME = 'TGeneralFlowChannel.Set_PowerPlant';
begin
  try
    FPowerPlantNr := Value.FeatureID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.PopulateGeneralFlowChannel
                                       (AChannelID            : integer;
                                        AChannelNumber        : integer;
                                        AChannelType          : integer;
                                        AChannelSubType       : integer;
                                        AChannelName          : WideString;
                                        AUpStreamNodeNumber   : integer;
                                        ADownStreamNodeNumber : integer;
                                        AChannelPenaltyNumber : integer;
                                        ASummaryOutput        : WideString;
                                        AChannelAreaID        : integer;
                                        AFlowOutput,
                                        AFirmYieldAnalysis    : WideString): WordBool;
const OPNAME = 'TGeneralFlowChannel.PopulateGeneralFlowChannel';
begin
  Result := False;
  try
    FChannelID                 := AChannelID;
    FChannelNumber             := AChannelNumber;
    FChannelName               := AChannelName;
    FChannelType               := AChannelType;
    FChannelSubType            := AChannelSubType;
    FUpStreamNodeNumber        := AUpStreamNodeNumber;
    FDownStreamNodeNumber      := ADownStreamNodeNumber;
    FChannelPenaltyNumber      := AChannelPenaltyNumber;
    FSummaryOutputRequired     := ASummaryOutput;
    FRequiresFirmYieldAnalysis := AFirmYieldAnalysis;
    FFlowOutput                := AFlowOutput;
    FChannelAreaID             := AChannelAreaID;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.PopulateChannelPenalty(APenalty : IChannelPenalty): WordBool;
const OPNAME = 'TGeneralFlowChannel.PopulateChannelPenalty';
begin
  Result := False;
  try
    FChannelPenaltyNumber := APenalty.ChannelPenaltyID;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.PopulateOutputChannelData(ASummaryOutputRequired,ARequiresFirmYieldAnalysis: WideString): WordBool;
const OPNAME = 'TGeneralFlowChannel.PopulateOutputChannelData';
begin
  Result := False;
  try
    FSummaryOutputRequired := ASummaryOutputRequired;
    FRequiresFirmYieldAnalysis := ARequiresFirmYieldAnalysis;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_ChannelID(AChannelID: integer);
const OPNAME = 'TGeneralFlowChannel.Set_ChannelID';
begin
  try
    FChannelID := AChannelID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_ChannelNumber(AChannelNumber: integer);
const OPNAME = 'TGeneralFlowChannel.Set_ChannelNumber';
begin
  try
    FChannelNumber := AChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ChannelPenaltyNumber: integer;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelPenaltyNumber';
begin
  Result := 0;
  try
    Result := FChannelPenaltyNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_ChannelName(const AChannelName: WideString);
const OPNAME = 'TGeneralFlowChannel.Set_ChannelName';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelDetailsContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber), IntToStr(FChannelType));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelName', AChannelName, FChannelName, LContextData) then
        begin
          LOldValue := FChannelName;
          FChannelName := AChannelName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelName',LOldValue,AChannelName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_ChannelPenalty (const APenalty : IChannelPenalty);
const OPNAME = 'TGeneralFlowChannel.Set_ChannelPenalty';
var
  LLoadAgent            : TChannelDataSQLAgent;
  LContextData          : TStringList;
  LOldNumber            : integer;
  lNewNumber            : integer;
  lOldArcCount          : integer;
  lNewArcCount          : integer;
  lOldPenalty           : IChannelPenalty;
  lMinMaxFlowConstraint : IMinMaxFlowConstraint;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (APenalty <> nil) then
        begin
          lNewNumber   := APenalty.ChannelPenaltyID;
          lNewArcCount := APenalty.ChannelPenaltyArcCount;
        end
        else
        begin
          lNewNumber   := 0;
          lNewArcCount := 0;
        end;
        if (FChannelPenaltyNumber <> 0) then
        begin
          lOldPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                           ChannelPenaltyList.ChannelPenaltyByIdentifier[FChannelPenaltyNumber];
          LOldNumber   := FChannelPenaltyNumber;
          lOldArcCount := lOldPenalty.ChannelPenaltyArcCount;
        end
        else
        begin
          LOldNumber   := 0;
          lOldArcCount := 0;
        end;
        LLoadAgent.LoadChannelDetailsContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber), IntToStr(FChannelType));
          if FAppModules.FieldProperties.UpdateFieldValue(
             'PenaltyNumber', IntToStr(lNewNumber), IntToStr(lOldNumber), LContextData) then
        begin
          FChannelPenaltyNumber := lNewNumber;
          if (FChannelType in [8,9,22,23,24,25,26,27,28,29,30,31,32,33,34,35]) then {Min-max & Pumping}
          begin
            if (lOldArcCount <> lNewArcCount) then
            begin
              lMinMaxFlowConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                                         NetworkFeaturesData.MinMaxFlowConstraintList.
                                           MinMaxFlowConstraintByID[FMinMaxFlowConstraintNr];
              if (lMinMaxFlowConstraint <> nil) then
              begin
                if (lOldArcCount < lNewArcCount) then
                  lMinMaxFlowConstraint.CreateFlowConstraints
                else
                if (lOldArcCount > lNewArcCount) then
                  lMinMaxFlowConstraint.RemoveFlowConstraints;
              end;
            end;
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PenaltyNumber',IntToStr(LOldNumber),
                                               IntToStr(lNewNumber));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_DownStreamNodeNumber(ANodeNumber: integer);
const OPNAME = 'TGeneralFlowChannel.Set_DownStreamNodeNumber';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
  LNode        : IReservoirData;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelDetailsContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber), IntToStr(FChannelType));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'DownNodeNumber', IntToStr(ANodeNumber), IntToStr(FDownStreamNodeNumber), LContextData) then
        begin
          LOldValue    := FDownStreamNodeNumber;
          FDownStreamNodeNumber := ANodeNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DownNodeNumber',IntToStr(LOldValue),IntToStr(ANodeNumber));
          LNode := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[ANodeNumber];
          if(LNode <> nil) then
          begin
            if(LNode.ReservoirConfigurationData.CatchmentRef <> 0) and (Self.SpecifiedInflowFeature = nil)then
              FAppModules.SetMenuItem(CCreateSpecifiedInflowFeature, msEnable)
            else
              FAppModules.SetMenuItem(CCreateSpecifiedInflowFeature, msDisable);
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

procedure TGeneralFlowChannel.Set_RequiresFirmYieldAnalysis (const ARequiresAnalysis: WideString);
const OPNAME = 'TGeneralFlowChannel.Set_RequiresFirmYieldAnalysis';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelDetailsContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber), IntToStr(FChannelType));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'FirmYieldCalc', ARequiresAnalysis, FRequiresFirmYieldAnalysis, LContextData) then
        begin
          LOldValue := FRequiresFirmYieldAnalysis;
          FRequiresFirmYieldAnalysis := ARequiresAnalysis;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FirmYieldCalc',LOldValue,ARequiresAnalysis);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_SummaryOutputRequired(const AOutputRequired: WideString);
const OPNAME = 'TGeneralFlowChannel.Set_SummaryOutputRequired';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  if FSummaryOutputRequired =  AOutputRequired then
    exit;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelDetailsContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber), IntToStr(FChannelType));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SummaryOutput', AOutputRequired, FSummaryOutputRequired, LContextData) then
        begin
          LOldValue := FSummaryOutputRequired;
          FSummaryOutputRequired := AOutputRequired;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SummaryOutputRequired',LOldValue,AOutputRequired);
        end;
      if ((UpperCase(FSummaryOutputRequired) <> 'Y') AND
          (Uppercase(FRequiresFirmYieldAnalysis) = 'Y')) then
        Set_RequiresFirmYieldAnalysis('');
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_FlowOutput: WideString; safecall;
const OPNAME = 'TGeneralFlowChannel.Get_FlowOutput';
begin
  Result := '';
  try
    Result := FFlowOutput;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_FlowOutput(const Value: WideString); safecall;
const OPNAME = 'TGeneralFlowChannel.Set_SummaryOutputRequired';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  if FFlowOutput = Value then
    exit;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelDetailsContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber), IntToStr(FChannelType));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'FlowOutput', Value, FFlowOutput, LContextData) then
        begin
          LOldValue := FFlowOutput;
          FFlowOutput := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FlowOutput',LOldValue,Value);
        end;
        if (UpperCase(FFlowOutput) = 'Y') then
          LLoadAgent.InsertSummaryOutput(FChannelNumber)
        else
          LLoadAgent.RemoveSummaryOutput(FChannelNumber);

      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TGeneralFlowChannel.Set_UpStreamNodeNumber(ANodeNumber: integer);
const OPNAME = 'TGeneralFlowChannel.Set_UpStreamNodeNumber';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelDetailsContextData(LContextData,
          IntToStr(FChannelID), IntToStr(FChannelNumber), IntToStr(FChannelType));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'UpNodeNumber', IntToStr(ANodeNumber), IntToStr(FUpStreamNodeNumber), LContextData) then
        begin
          LOldValue := FUpStreamNodeNumber;
          FUpStreamNodeNumber := ANodeNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UpNodeNumber',IntToStr(LOldValue), IntToStr(ANodeNumber));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_UpStreamNode : IReservoirData;
const OPNAME = 'TGeneralFlowChannel.Get_UpStreamNode';
var
  LMine             : IMine;
  LIrrigationBlock  : IIrrigationBlock;
  LGroundWater      : IGroundWater;
  LWetland          : IWetland;
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FUpStreamNodeNumber];
    if(Result = nil) then
    begin
      LMine  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                MineList.MineByNodeNumber[FUpStreamNodeNumber];
      if(LMine <> nil) then
        Result := LMine.MineNode;
    end;
    if(Result = nil) then
    begin
      LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                           IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FUpStreamNodeNumber];
      if(LIrrigationBlock <> nil) then
        Result := LIrrigationBlock.BlockNode;
    end;
    if(Result = nil) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                           GroundWaterList.GroundWaterByNodeNumber[FUpStreamNodeNumber];
      if(LGroundWater <> nil) then
        Result := LGroundWater.AquiferNode;
    end;
    if(Result = nil) then
    begin
      LWetland  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.
                   WetlandByNodeNumber[FUpStreamNodeNumber];
      if(LWetland <> nil) then
        Result := LWetland.ReservoirDetails;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_DownStreamNode : IReservoirData;
const OPNAME = 'TGeneralFlowChannel.Get_DownStreamNode';
var
  LMine             : IMine;
  LIrrigationBlock  : IIrrigationBlock;
  LGroundWater      : IGroundWater;
  LWetland          : IWetland;
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FDownStreamNodeNumber];
    if(Result = nil) then
    begin
      LMine  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                MineList.MineByNodeNumber[FUpStreamNodeNumber];
      if(LMine <> nil) then
        Result := LMine.MineNode;
    end;
    if(Result = nil) then
    begin
      LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                           IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FUpStreamNodeNumber];
      if(LIrrigationBlock <> nil) then
        Result := LIrrigationBlock.BlockNode;
    end;
    if(Result = nil) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                           GroundWaterList.GroundWaterByNodeNumber[FUpStreamNodeNumber];
      if(LGroundWater <> nil) then
        Result := LGroundWater.AquiferNode;
    end;
    if(Result = nil) then
    begin
      LWetland  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.
                   WetlandByNodeNumber[FUpStreamNodeNumber];
      if(LWetland <> nil) then
        Result := LWetland.ReservoirDetails;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.ArcCountValid (AArcCount : integer) : Boolean;
const OPNAME = 'TGeneralFlowChannel.ArcCountValid';
begin
  Result := FALSE;
  try
    if (FDiversionFeatureNr <> 0) then
      Result := AArcCount in [2]
    else
    if (FPhysicalFlowConstraintNr <> 0) then
      Result := AArcCount in [1,2,3,4,5]
    else
    if (FIFRFeatureNr <> 0) then
      Result := AArcCount in [1,2,3,4,5]
    else
    if (FSpecifiedInflowFeatureNr <> 0) then
      Result := AArcCount in [2]
    else
    if (FChannelType = 3) then
    begin
      if (FChannelSubType = 1) then
        Result := AArcCount in [3]   { Power channel}
      else
        Result := AArcCount in [1];  { Spill Channel}
    end
    else
    if (FChannelType = 2) then { Master control}
    begin
      Result := AArcCount in [2];
      if(MasterControlFeature <> nil) and (MasterControlFeature.MasterControlType = 'P') then
        Result := AArcCount in [3]
    end
    else
    if (FChannelType = 4) then
    begin
      if ((FChannelSubType = 1) OR (FChannelSubType = 3)) then
        Result := AArcCount in [2]  { Diversion and Return flow channels of irrigation area}
      else
        Result := AArcCount in [1]; { Consumptive channel}
    end
    else
    if (FChannelType = 6) then { Minimum flow channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 7) then { Loss channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 8) then { Min-max channel}
      Result := AArcCount in [1, 2, 3, 4, 5]
    else
    if (FChannelType = 9) then { Pumping channel}
      Result := AArcCount in [1, 2, 3, 4, 5]
    else
    if (FChannelType = 11) then { Specified Demand}
      Result := AArcCount in [2]
    else
    if (FChannelType = 14) then { Irrigation Block inflow}
      Result := AArcCount in [2]
    else
    if (FChannelType = 15) then { Irrigation Block return flow}
      Result := AArcCount in [2]
    else
    if (FChannelType = 16) then { Wetland Inflow}
      Result := AArcCount in [2]
    else
    if (FChannelType = 17) then { Wetland Outflow}
      Result := AArcCount in [2]
    else
    if (FChannelType = 20) then { Demand centre return flow Demand}
      Result := AArcCount in [2]
    else
    if (FChannelType = 21) then { Demand centre Reclaimation Plant Loss}
      Result := AArcCount in [2]
    else
    if (FChannelType = 22) then { Mining channel to PCD}
      Result := AArcCount in [2]
    else
    if (FChannelType = 23) then { Mining channel to River}
      Result := AArcCount in [2]
    else
    if (FChannelType = 24) then { Mining channel to Underground section}
      Result := AArcCount in [2]
    else
    if (FChannelType = 25) then { Aquifer Inflow Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 26) then { Aquifer Excess Interflow Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 27) then { GroundWater Baseflow Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 28) then { Abstraction From Aquifer Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 29) then { Abstraction From BaseFlow Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 30) then { GroundWater BaseFlow Remainder Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 31) then { OutFlow To Downstream Aquifer Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 32) then { Surface Runoff Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 33) then { GroundWater Abstraction Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 34) then {Outflow To Network Channel}
      Result := AArcCount in [2]
    else
    if (FChannelType = 35) then {Inflow From Upstream Aquifer Channel}
      Result := AArcCount in [2]
    else
      Result := AArcCount in [1];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_ValidArcCounts : WideString;
const OPNAME = 'TGeneralFlowChannel.Get_ValidArcCounts';
begin
  Result := '';
  try
    if (FDiversionFeatureNr <> 0) then
      Result := '2'
    else
    if (FPhysicalFlowConstraintNr <> 0) then
      Result := '1,2'
    else
    if (FIFRFeatureNr <> 0) then
      Result := '2'
    else
    if (FSpecifiedInflowFeatureNr <> 0) then
      Result := '2'
    else
    if (FChannelType = 3) then
    begin
      if (FChannelSubType = 1) then
        Result := '3'   {Power channel}
      else
        Result := '1';  { Spill Channel}
    end
    else
    if (FChannelType = 2) then
    begin
      { Master control}
      Result := '2';
      if(MasterControlFeature <> nil) and (MasterControlFeature.MasterControlType = 'P') then
        Result := '3';
    end
    else
    if (FChannelType = 4) then
    begin
      if ((FChannelSubType = 1) OR (FChannelSubType = 3)) then
        Result := '2'  { Diversion and Return flow channels of irrigation area}
      else
        Result := '1'; { Consumptive channel}
    end
    else
    if (FChannelType = 6) then { Minimum flow channel}
      Result := '2'
    else
    if (FChannelType = 7) then { Loss channel}
      Result := '2'
    else
    if (FChannelType = 8) then { Min-max channel}
      Result := '1,2,3,4,5'
    else
    if (FChannelType = 9) then { Pumping channel}
      Result := '1,2,3,4,5'
    else
    if (FChannelType = 11) then { Specified Demand}
      Result := '2'
    else
    if (FChannelType = 14) then { Irrigation Block inflow}
      Result := '2'
    else
    if (FChannelType = 15) then { Irrigation Block return flow}
      Result := '2'
    else
    if (FChannelType = 16) then { Wetland Inflow}
      Result := '2'
    else
    if (FChannelType = 17) then { Wetland Outflow}
      Result := '2'
    else
    if (FChannelType = 20) then { Demand centre return flow}
      Result := '2'
    else
    if (FChannelType = 21) then { Demand centre Reclaimation Plant Loss}
      Result := '2'
    else
    if (FChannelType = 22) then { Mining channel to PCD}
      Result := '2'
    else
    if (FChannelType = 23) then { Mining channel to River}
      Result := '2'
    else
    if (FChannelType = 24) then { Mining channel to Underground section}
      Result := '2'
    else
    if (FChannelType = 25) then { Aquifer Inflow Channel}
      Result := '2'
    else
    if (FChannelType = 26) then { Aquifer Excess Interflow Channel}
      Result := '2'
    else
    if (FChannelType = 27) then { GroundWater Baseflow Channel}
      Result := '2'
    else
    if (FChannelType = 28) then { Abstraction From Aquifer Channel}
      Result := '2'
    else
    if (FChannelType = 29) then { Abstraction From BaseFlow Channel}
      Result := '2'
    else
    if (FChannelType = 30) then { GroundWater BaseFlow Remainder Channel}
      Result := '2'
    else
    if (FChannelType = 31) then { OutFlow To Downstream Aquifer Channel}
      Result := '2'
    else
    if (FChannelType = 32) then { Surface Runoff Channel}
      Result := '2'
    else
    if (FChannelType = 33) then { GroundWater Abstraction Channel}
      Result := '2'
    else
    if (FChannelType = 34) then { Outflow To Network Channel}
      Result := '2'
    else
    if (FChannelType = 35) then { Inflow From Upstream Aquifer}
      Result := '2'
    else
      Result := '1';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteAllFeatures : WordBool;
const OPNAME = 'TGeneralFlowChannel.DeleteAllFeatures';
begin
  Result := TRUE;
  try
    FMinimumFlowConstraintNr  := 0;
    FLossFeatureNr            := 0;
    FPumpingFeatureNr         := 0;
    FMinMaxFlowConstraintNr   := 0;
    FSpecifiedDemandFeatureNr := 0;
    FDiversionFeatureNr       := 0;
    FPhysicalFlowConstraintNr := 0;
    FIFRFeatureNr             := 0;
    FSpecifiedInflowFeatureNr := 0;
    FMultiResChannelCurtailID := 0;
    if (FChannelType <> 12) then
      Set_ChannelType(12);
    if (FChannelSubType > 0) then
      Set_ChannelSubType(0);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteMinimumFlowConstraint : boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteMinimumFlowConstraint';
begin
  Result := FALSE;
  try
    FMinimumFlowConstraintNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteLossFeature : boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteLossFeature';
begin
  Result := FALSE;
  try
    FLossFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteMinMaxFlowConstraint : boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteMinMaxFlowConstraint';
begin
  Result := FALSE;
  try
    FMinMaxFlowConstraintNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteSpecifiedDemandFeature : boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteSpecifiedDemandFeature';
begin
  Result := FALSE;
  try
    FSpecifiedDemandFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteDiversionFeature : boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteDiversionFeature';
begin
  Result := FALSE;
  try
    FDiversionFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeletePumpingFeature : WordBool;
const OPNAME = 'TGeneralFlowChannel.DeletePumpingFeature';
begin
  Result := FALSE;
  try
    FPumpingFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeletePhysicalFlowConstraint : WordBool;
const OPNAME = 'TGeneralFlowChannel.DeletePhysicalFlowConstraint';
begin
  Result := FALSE;
  try
    FPhysicalFlowConstraintNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteIFRFeature : WordBool;
const OPNAME = 'TGeneralFlowChannel.DeleteIFRFeature';
begin
  Result := FALSE;
  try
    FIFRFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteMasterControlFeature : WordBool;
const OPNAME = 'TGeneralFlowChannel.DeleteMasterControlFeature';
begin
  Result := FALSE;
  try
    FMasterControlFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteSpecifiedInflowFeature : WordBool;
const OPNAME = 'TGeneralFlowChannel.DeleteSpecifiedInflowFeature';
begin
  Result := FALSE;
  try
    FSpecifiedInflowFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteWaterDemandFeature: WordBool;
const OPNAME = 'TGeneralFlowChannel.DeleteWaterDemandFeature';
begin
  Result := FALSE;
  try
    FWaterDemandFeatureNr := 0;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.RemoveMultiResChannelCurtail(AID: Integer): WordBool; safecall;
const OPNAME = 'TGeneralFlowChannel.DeleteMultiResChannelCurtail:';
begin
  Result := FALSE;
  try

    Result := TRUE;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

function TGeneralFlowChannel.GetKeyValues (const AParamField : WideString;
                                           const AFieldIndex : WideString) : WideString;
const OPNAME = 'TGeneralFlowChannel.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FChannelID)
    else
      Result := Result + ',Identifier=' + IntToStr(FChannelID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannel.Get_ChannelArea: integer;
const OPNAME = 'TGeneralFlowChannel.Get_ChannelArea';
begin
  Result := NullInteger;
  try
    Result := FChannelAreaID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannel.Set_ChannelArea(AValue: integer);
const OPNAME = 'TGeneralFlowChannel.Set_ChannelArea';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadChannelContextData(LContextData, IntToStr(FChannelID), IntToStr(FChannelNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelAreaID', IntToStr(AValue), IntToStr(FChannelAreaID), LContextData) then
        begin
          LOldValue    := FChannelAreaID;
          FChannelAreaID := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelAreaID',IntToStr(LOldValue),IntToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_TariffCalculation: IChannelTariff;
const OPNAME = 'TGeneralFlowChannel.Get_TimeControl';
begin
  Result := nil;
  try
    if FAppModules.StudyArea.ModelCode = CPlanning then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastTariffCalculationData.ChannelTariffByChannelNumber[FChannelNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_TimeControl: IChannelTimeControl;
const OPNAME = 'TGeneralFlowChannel.Get_TimeControl';
begin
  Result := nil;
  try
    FTimeControl := CastChannelTimeControlByNumber(FChannelNumber);
    Result := FTimeControl;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.CreateTimeControl: TChannelTimeControl;
const OPNAME = 'TGeneralFlowChannel.CreateTimeControl';
begin
  Result := nil;
  try
    FTimeControl := TChannelTimeControl.Create(FAppModules);
    FTimeControl.Populate(FTimeControlList.Count+ 1,FChannelNumber, 0, 0, 0, 0, 0, 0, 0, 0, '','');
    Result := FTimeControl;
    FTimeControlList.Add(FTimeControl);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteTimeControl: Boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteTimeControl';
begin
  Result := False;
  try
    if (FTimeControl <> nil) and (FTimeControlList.Count > 0) then
      FTimeControlList.Remove(FTimeControl);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.NewTimeControl: IChannelTimeControl;
const OPNAME = 'TGeneralFlowChannel.NewTimeControl';
var
  lLoadAgent   : TChannelPlanningLoadAgent;
  lTimeControl : IChannelTimeControl;
begin
  Result := nil;
  try
    lLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertChannelTimeControl(FTimeControlList.Count+ 1,FChannelNumber, FChannelType)) then
      begin
        lTimeControl := CreateTimeControl;
        Result := lTimeControl;
      end;
     finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.RemoveTimeControl : WordBool;
const OPNAME = 'TGeneralFlowChannel.RemoveTimeControl';
var
  lLoadAgent : TChannelPlanningLoadAgent;
begin
  Result := False;
  try
    lLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.DeleteChannelTimeControl(FTimeControl.Identifier,FChannelNumber)) then
      begin
        DeleteTimeControl;
        Result := True;
      end;
     finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.CastSwitchControlByIndex (AIndex : integer) : TChannelSwitchControl;
const OPNAME = 'TGeneralFlowChannel.CastSwitchControlByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) AND (AIndex < FSwitchControls.Count) then
      Result := TChannelSwitchControl(FSwitchControls.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.CastSwitchControlByChannelNumber(AChannelNumber : integer) : TChannelSwitchControl;
const OPNAME = 'TGeneralFlowChannel.CastSwitchControlByIndex';
var
  LIndex        : integer;
  LChannelSwitchControl : TChannelSwitchControl;
begin
  Result := nil;
  try
    LIndex := 0;
    while (Result = nil) and (LIndex < FSwitchControls.Count) do
    begin
      LChannelSwitchControl := CastSwitchControlByIndex(LIndex);
      if (LChannelSwitchControl.ChannelNumber = AChannelNumber) then
        Result := LChannelSwitchControl
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.CastTimeControlByIndex (AIndex : integer) : TChannelTimeControl;
const OPNAME = 'TGeneralFlowChannel.CastTimeControlByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FTimeControlList.Count) then
      Result := TChannelTimeControl(FTimeControlList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.CastChannelTimeControlByNumber (AChannelNumber : integer) : TChannelTimeControl;
const OPNAME = 'TGeneralFlowChannel.CastChannelTimeControlByNumber';
var
  LIndex        : integer;
  LChannelTimeControl : TChannelTimeControl;
begin
  Result := nil;
  try
    LIndex := 0;
    while (Result = nil) and (LIndex < FTimeControlList.Count) do
    begin
      LChannelTimeControl := CastTimeControlByIndex(LIndex);
      if (LChannelTimeControl.ChannelNumber = AChannelNumber) then
        Result := LChannelTimeControl
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGeneralFlowChannel.CastSwitchControlByID (AChannelSwitchID : integer) : TChannelSwitchControl;
const OPNAME = 'TGeneralFlowChannel.CastSwitchControlByID';
var
  lIndex        : integer;
  lSwitchConrol : TChannelSwitchControl;
begin
  Result := nil;
  try
    lIndex := 0;
    while (Result = nil) AND (lIndex < FSwitchControls.Count) do
    begin
      lSwitchConrol := CastSwitchControlByIndex(lIndex);
      if (lSwitchConrol.ChannelSwitchID = AChannelSwitchID) then
        Result := lSwitchConrol
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.Get_SwitchControlByID (AChannelSwitchID : Integer) : IChannelSwitchControl;
const OPNAME = 'TGeneralFlowChannel.Get_SwitchControlByID';
begin
  Result := nil;
  try
    Result := CastSwitchControlByID(AChannelSwitchID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.Get_SwitchControlByIndex (AIndex : Integer) : IChannelSwitchControl;
const OPNAME = 'TGeneralFlowChannel.Get_SwitchControlByIndex';
begin
  Result := nil;
  try
    Result := CastSwitchControlByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.Get_SwitchControlByChannelNumber(AChannelNumber: Integer): IChannelSwitchControl; safecall;
const OPNAME = 'TGeneralFlowChannel.Get_SwitchControlByChannelNumber';
begin
  Result := nil;
  try
    Result := CastSwitchControlByChannelNumber(AChannelNumber);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.Get_SwitchControlCount : Integer;
const OPNAME = 'TGeneralFlowChannel.Get_SwitchControlCount';
begin
  Result := 0;
  try
    Result := FSwitchControls.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.CreateSwitchControl (AChannelSwitchID : Integer) : TChannelSwitchControl;
const OPNAME = 'TGeneralFlowChannel.CreateSwitchControl';
var
  lSwitchControl : TChannelSwitchControl;
begin
  Result := nil;
  try
    lSwitchControl := TChannelSwitchControl.Create(FAppModules);
    FSwitchControls.Add(lSwitchControl);
    lSwitchControl.Populate(AChannelSwitchID, FChannelNumber, 0, 0, 0.0, 0, 0);
    Result := lSwitchControl;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteSwitchControl (AChannelSwitchID : integer) : Boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteSwitchControl';
var
  lIndex: integer;
begin
  Result := FALSE;
  try
    for lIndex := 0 to FSwitchControls.Count -1 do
    begin
      if (TChannelSwitchControl(FSwitchControls.Items[lIndex]).ChannelSwitchID = AChannelSwitchID) then
      begin
        FSwitchControls.Remove(FSwitchControls.Items[lIndex]);
        Break;
        Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.NewSwitchControl : IChannelSwitchControl;
const OPNAME = 'TGeneralFlowChannel.NewSwitchControl';
var
  lLoadAgent       : TChannelPlanningLoadAgent;
  lChannelSwitchID : integer;
begin
  Result := nil;
  try
    lLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      lChannelSwitchID := 0;
      if (lLoadAgent.InsertChannelSwitchControl(FChannelNumber, lChannelSwitchID)) then
      begin
        Result := CreateSwitchControl(lChannelSwitchID);
      end;
     finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.RemoveSwitchControl (AChannelSwitchID : integer) : WordBool;
const OPNAME = 'TGeneralFlowChannel.RemoveSwitchControl';
var
  lSwitchControl : TChannelSwitchControl;
  lLoadAgent     : TChannelPlanningLoadAgent;
begin
  Result := FALSE;
  try
    lSwitchControl := CastSwitchControlByID(AChannelSwitchID);
    if (lSwitchControl <> nil) then
    begin
      lLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
      try
        if lLoadAgent.DeleteChannelSwitchControl(AChannelSwitchID) then
        begin
          DeleteSwitchControl(AChannelSwitchID);
          Result := TRUE;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_SelectedSwitchControlID: Integer;
const OPNAME = 'TGeneralFlowChannel.Get_SelectedSwitchControlID';
begin
  Result := 0;
  try
    Result := FSelectedSwitchControlID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGeneralFlowChannel.Set_SelectedSwitchControlID(Value: Integer);
const OPNAME = 'TGeneralFlowChannel.Set_SelectedSwitchControlID';
begin
  try
    FSelectedSwitchControlID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TChannelList                                                               *}
{******************************************************************************}

function TChannelList.AddChannel(AChannel: TGeneralFlowChannel): Boolean;
const OPNAME = 'TChannelList.AddChannel';
begin
  Result := False;
  try
    if (AChannel <> nil) then
    begin
      FChannelList.Add(AChannel);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList._AddRef: Integer;
const OPNAME = 'TChannelList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList._Release: Integer;
const OPNAME = 'TChannelList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelList.CreateMemberObjects;
const OPNAME = 'TChannelList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FChannelList := TObjectList.Create(False);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelList.DestroyMemberObjects;
const OPNAME = 'TChannelList.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    for LCount := FChannelList.Count - 1 downto 0 do
      FChannelList.Items[LCount].Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.GetCastChannelByIndex(AIndex: integer): TGeneralFlowChannel;
const OPNAME = 'TChannelList.GetCastChannelByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FChannelList.Count) then
      Result := TGeneralFlowChannel(FChannelList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.Get_ChannelByIdentifier(AIdentifier: integer): IGeneralFlowChannel;
const OPNAME = 'TChannelList.Get_ChannelByIdentifier';
var
 LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FChannelList.Count -1 do
      if(TGeneralFlowChannel(FChannelList[LIndex]).ChannelID = AIdentifier) then
      begin
        Result := TGeneralFlowChannel(FChannelList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.Get_ChannelByChannelNumber(AChannelNumber: integer): IGeneralFlowChannel;
const OPNAME = 'TChannelList.Get_ChannelByChannelNumber';
var
 LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FChannelList.Count -1 do
      if(TGeneralFlowChannel(FChannelList[LIndex]).ChannelNumber = AChannelNumber) then
      begin
        Result := TGeneralFlowChannel(FChannelList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.GetCastChannelByChannelNumber(AChannelNumber: integer): TGeneralFlowChannel;
const OPNAME = 'TChannelList.GetCastChannelByChannelNumber';
var
 LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FChannelList.Count -1 do
      if(TGeneralFlowChannel(FChannelList[LIndex]).ChannelNumber = AChannelNumber) then
      begin
        Result := TGeneralFlowChannel(FChannelList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.GetCastChannelByChannelAreaID(AChannelAreaID: integer): TGeneralFlowChannel;
const OPNAME = 'TChannelList.GetCastChannelByChannelAreaID';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FChannelList.Count -1 do
      if(TGeneralFlowChannel(FChannelList[LIndex]).FChannelAreaID = AChannelAreaID) then
      begin
        Result := TGeneralFlowChannel(FChannelList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TChannelList.Get_ChannelByIndex(AIndex: integer): IGeneralFlowChannel;
const OPNAME = 'TChannelList.Get_ChannelByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FChannelList.Count) then
      Result := TGeneralFlowChannel(FChannelList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.Get_ChannelCount: integer;
const OPNAME = 'TChannelList.Get_ChannelCount';
begin
  Result := 0;
  try
    Result := FChannelList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.Initialise: boolean;
const OPNAME = 'TChannelList.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelList.AddMonthlyConstraints(APenaltyNr: integer);
const OPNAME = 'TChannelList.AddMonthlyConstraints';
var
  lChannelIndex : integer;
  lChannel      : TGeneralFlowChannel;
  lMinMaxID     : integer;
  lMinMax       : IMinMaxFlowConstraint;
  lFeatureList  : IMinMaxFlowConstraintList;
begin
  try
    lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      CastNetworkFeaturesData.CastMinMaxFlowConstraintList;

    for lChannelIndex := 0 to FChannelList.Count - 1 do
    begin
      lChannel := GetCastChannelByIndex(lChannelIndex);
      if ((lChannel.ChannelType in [8,9,22,23,24]) {Min-max & Pumping} AND
          (lChannel.ChannelPenaltyNumber = APenaltyNr)) then
      begin
        lMinMaxID := lChannel.MinMaxFlowConstraintID;
        lMinMax   := lFeatureList.MinMaxFlowConstraintByID[lMinMaxID];
        lMinMax.CreateFlowConstraints;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelList.DeleteMonthlyConstraints (APenaltyNr : integer;
                                                 AIndex     : integer);
const OPNAME = 'TChannelList.DeleteMonthlyConstraints';
var
  lChannelIndex : integer;
  lChannel      : TGeneralFlowChannel;
  lMinMaxID     : integer;
  lMinMax       : IMinMaxFlowConstraint;
  lFeatureList  : IMinMaxFlowConstraintList;
begin
  try
    lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.MinMaxFlowConstraintList;
    for lChannelIndex := 0 to FChannelList.Count - 1 do
    begin
      lChannel := CastChannelByIndex[lChannelIndex];
      if ((lChannel.ChannelType in [8,22,23,24]) {Min-max} AND
          (lChannel.ChannelPenaltyNumber = APenaltyNr)) then
      begin
        lMinMaxID := lChannel.MinMaxFlowConstraintID;
        lMinMax   := lFeatureList.MinMaxFlowConstraintByID[lMinMaxID];
        lMinMax.RemoveFlowConstraints;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.Get_OutputChannelCount: integer;
const OPNAME = 'TChannelList.Get_OutputChannelCount';
var
  LCount: integer;
begin
  Result := 0;
  try
    for LCount := 0 to FChannelList.Count - 1 do
    begin
      if (UpperCase(ChannelByIndex[LCount].SummaryOutputRequired) = 'Y') then
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.CastGeneralFlowChannelByID(AChannelID : integer): TGeneralFlowChannel;
const OPNAME = 'TChannelList.CastGeneralFlowChannelByID';
var
 lIndex   : integer;
 lChannel : TGeneralFlowChannel;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FChannelList.Count)) do
    begin
      lChannel := TGeneralFlowChannel(FChannelList.Items[lIndex]);
      if (lChannel.ChannelID = AChannelID) then
        Result := lChannel
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.AddGeneralFlowChannel (AChannel : TGeneralFlowChannel): boolean;
const OPNAME = 'TChannelList.AddGeneralFlowChannel';
begin
  Result := False;
  try
    if (AChannel <> nil) then
    begin
      FChannelList.Add(AChannel);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.NewGeneralFlowChannel : TGeneralFlowChannel;
const OPNAME = 'TChannelList.NewGeneralFlowChannel';
var
  lFeature : TGeneralFlowChannel;
begin
  Result := nil;
  try
    lFeature := TGeneralFlowChannel.Create(FAppModules);
    AddGeneralFlowChannel(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.CreateNewChannel : TGeneralFlowChannel;
const OPNAME = 'TChannelList.CreateNewChannel';
var
  LChannelID : integer;
  LChannelNr : integer;
  LLoadAgent : TChannelDataSQLAgent;
  LChannel   : TGeneralFlowChannel;
begin
  Result := nil;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LChannelID := 0;
      LChannelNr := 0;
      if (LLoadAgent.InsertChannel(LChannelID, LChannelNr)) then
      begin
        if (FChannelList.Count = 0) then
            LLoadAgent.InsertChannelComments;
        LChannel := NewGeneralFlowChannel;
        LChannel.Initialise;
        LChannel.PopulateGeneralFlowChannel(LChannelID, LChannelNr, 12, 0,
           UpperCase(FAppModules.Language.GetString('Channel.Channel')) + ' ' + IntToStr(LChannelNr),
           0, 0, 0, 'Y',0,'','');
        Result := lChannel;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.CreateChannel : IGeneralFlowChannel;
const OPNAME = 'TChannelList.CreateChannel';
var
  LChannel : IGeneralFlowChannel;
begin
  Result := nil;
  try
    lChannel := CreateNewChannel;
    Result   := lChannel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.RemoveChannelWithID(AChannelID : integer) : WordBool;
const OPNAME = 'TChannelList.RemoveChannelWithID';
var
  lLoadAgent : TChannelDataSQLAgent;
  lChannel   : TGeneralFlowChannel;
begin
  Result := False;
  try
    lChannel := CastGeneralFlowChannelByID(AChannelID);
    if (lChannel <> nil) then
    begin
      lChannel.DeleteAllFeatures;
      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteChannel(AChannelID) then
        begin
          DeleteGeneralFlowChannelWithID(AChannelID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.RemoveChannelWithNumber(AChannelNumber : integer) : WordBool;
const OPNAME = 'TChannelList.RemoveChannelWithNumber';
var
  lLoadAgent  : TChannelDataSQLAgent;
  lChannelID    : integer;
begin
  Result := False;
  try
    if (CastChannelByChannelNumber[AChannelNumber] <> nil) then
    begin
      lChannelID     := CastChannelByChannelNumber[AChannelNumber].ChannelID;
      CastChannelByChannelNumber[AChannelNumber].DeleteAllFeatures;
      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteChannel(lChannelID) then
        begin
          DeleteGeneralFlowChannelWithID(lChannelID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.DeleteGeneralFlowChannelWithID(AChannelID : integer) : WordBool;
const OPNAME = 'TChannelList.DeleteGeneralFlowChannelWithID';
var
  LIndex: integer;
begin
  Result := FALSE;
  try
    for lIndex := 0 to FChannelList.Count -1 do
    begin
      if (TGeneralFlowChannel(FChannelList.Items[lIndex]).ChannelID = AChannelID) then
      begin
        FChannelList.Delete(lIndex);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.DeleteGeneralFlowChannelWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TChannelList.DeleteGeneralFlowChannelWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) and (AIndex < FChannelList.Count) then
    begin
      FChannelList.Remove(FChannelList.Items[AIndex]);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.MayChangePenaltyArcCount (APenaltyNr : integer;
                                                AArcCount  : integer) : WordBool;
const OPNAME = 'TChannelList.MayChangePenaltyArcCount';
var
  lIndex   : integer;
  lChannel : TGeneralFlowChannel;
begin
  Result := TRUE;
  try
    lIndex := 0;
    while (Result AND (lIndex < ChannelCount)) do
    begin
      lChannel := CastChannelByIndex[lIndex];
      if ((APenaltyNr = lChannel.ChannelPenalty.ChannelPenaltyID) AND
          (NOT(lChannel.ArcCountValid(AArcCount)))) then
        Result := FALSE
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelList.ValidateMaximumZeroDownstreamNode (var AErrors : WideString;
                                                         AContext    : WideString='') : WordBool;
const OPNAME = 'TChannelList.ValidateMaximumZeroDownstreamNode';
var
  lIndex   : integer;
  lChannel : IGeneralFlowChannel;
  lCount   : integer;
  lErrors  : TStringList;
begin
  Result := TRUE;
  try
    lErrors := TStringList.Create;
    try
      lIndex := 0;
      lCount := 0;
      while (lIndex < ChannelCount) do
      begin
        lChannel := CastChannelByIndex[lIndex];
        if (lChannel.DownStreamNodeNumber = 0) then
          lCount := lCount + 1;
        lIndex := lIndex + 1
      end;
      if (lCount > 2000) then
      begin
        lErrors.Add(FAppModules.Language.GetString('ContextValidation.TooManyZeroDownstreamNodes'));
        Result := FALSE;
      end;
      AErrors := AErrors + lErrors.Text;
    finally
      FreeAndNil(lErrors);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelList.ValidateMaximumZeroUpstreamNode (var AErrors : WideString;
                                                       AContext    : WideString='') : WordBool; safecall;
const OPNAME = 'TChannelList.ValidateMaximumZeroUpstreamNode';
var
  lIndex   : integer;
  lChannel : IGeneralFlowChannel;
  lCount   : integer;
  lErrors  : TStringList;
begin
  Result := TRUE;
  try
    lErrors := TStringList.Create;
    try
      lIndex := 0;
      lCount := 0;
      while (lIndex < ChannelCount) do
      begin
        lChannel := CastChannelByIndex[lIndex];
        if (lChannel.UpStreamNodeNumber = 0) then
          lCount := lCount + 1;
        lIndex := lIndex + 1
      end;
      if (lCount > 2000) then
      begin
        lErrors.Add(FAppModules.Language.GetString('ContextValidation.TooManyZeroUpstreamNodes'));
        Result := FALSE;
      end;
      AErrors := AErrors + lErrors.Text;
    finally
      FreeAndNil(lErrors);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TChannelList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    if (AContext = 'MaximumZeroUpstreamNode') then
      Result := ValidateMaximumZeroUpstreamNode(AErrors,AContext)
    else
    if (AContext = 'MaximumZeroDownstreamNode') then
      Result := ValidateMaximumZeroDownstreamNode(AErrors,AContext)
    else
    begin
      Result := TRUE;
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for LIndex := 0 to FChannelList.Count - 1 do
      begin
        if (NOT ChannelByIndex[LIndex].Validate(AErrors,AContext)) then
          Result := FALSE;
        if ((NOT Result) AND lStopOnFirstError) then
          Break;
      end;
      if (NOT ValidateMaximumZeroUpstreamNode(AErrors,AContext)) then
          Result := FALSE;
      if (Result OR (NOT lStopOnFirstError)) then
      begin
        if (NOT ValidateMaximumZeroDownstreamNode(AErrors,AContext)) then
          Result := FALSE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.CreateDisbenefitFunction: TDisbenefitFunctionData;
const OPNAME = 'TGeneralFlowChannel.CreateDisbenefitFunction';
var
  LIndex : integer;
  LTDSConcentrationFactors : array[1..4] of double;
begin
  Result := nil;
  try
    for LIndex := 1 to 4 do
      LTDSConcentrationFactors[LIndex] := 0.00;

    FDisbenefitFunction := TDisbenefitFunctionData.Create(FAppModules);
    FDisbenefitFunction.Populate(FChannelNumber, 0.0, 0.0, 0.0, 0.0, '',0,0,0,0,0.0,LTDSConcentrationFactors);

    Result := FDisbenefitFunction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.DeleteDisbenefitFunction: Boolean;
const OPNAME = 'TGeneralFlowChannel.DeleteDisbenefitFunction';
begin
  Result := FALSE;
  try
    FreeAndNil(FDisbenefitFunction);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.Get_DisbenefitFunction: IDisbenefitFunctionDefinition;
const OPNAME = 'TGeneralFlowChannel.Get_DisbenefitFunction';
begin
  Result := nil;
  try
    Result := FDisbenefitFunction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGeneralFlowChannel.NewDisbenefitFunction: IDisbenefitFunctionDefinition;
const OPNAME = 'TGeneralFlowChannel.NewDisbenefitFunction';
var
  lLoadAgent          : TDisbenefitFunctionDefinitionSQLAgent;
  LDisbenefitFunction : IDisbenefitFunctionDefinition;
begin
  Result := nil;
  try
    lLoadAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertDisbenefitFunction(FChannelID, FChannelNumber)) then
      begin
        LDisbenefitFunction := CreateDisbenefitFunction;
        Result := LDisbenefitFunction;
      end;
     finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.RemoveDisbenefitFunction: WordBool;
const OPNAME = 'TGeneralFlowChannel.RemoveDisbenefitFunction';
var
  lLoadAgent : TDisbenefitFunctionDefinitionSQLAgent;
begin
  Result := False;
  try
    lLoadAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
    try
      if (lLoadAgent.DeleteDisbenefitFunctionDefinition(FChannelNumber)) then
      begin
        DeleteDisbenefitFunction;
        Result := True;
      end;
     finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.Get_ReturnFlowChannel: IReturnFlowChannel;
const OPNAME = 'TGeneralFlowChannel.Get_ReturnFlowChannel';
begin
  Result := nil;
  try
    if FAppModules.StudyArea.ModelCode = CPlanning then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastReturnFlowChannel.
                ReturnFlowChannelByChannel[FChannelNumber];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.NewReturnFlowChannel: IReturnFlowChannel;
const OPNAME = 'TGeneralFlowChannel.NewReturnFlowChannel';
begin
  Result := nil;
  try
    if FAppModules.StudyArea.ModelCode = CPlanning then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastReturnFlowChannel.
                NewReturnFlowChannel(FChannelNumber);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGeneralFlowChannel.NewMultiResCurChannel: IMultiResMultiChannelCurtail; safecall;
const OPNAME = 'TGeneralFlowChannel.NewMultiResCurChannel';
begin
  Result := nil;
  try

    if FAppModules.StudyArea.ModelCode = CPlanning then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData.NewRestriction(FChannelNumber);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGeneralFlowChannel.RemoveReturnFlowChannel: WordBool;
const OPNAME = 'TGeneralFlowChannel.RemoveReturnFlowChannel';
begin
  Result := False;
  try
    if FAppModules.StudyArea.ModelCode = CPlanning then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastReturnFlowChannel.
                RemoveReturnFlowByChannel(FChannelNumber);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelList.ChannelIDFromChannelNumber(AChannelNumber: integer): integer;
const OPNAME = 'TChannelList.ChannelIDFromChannelNumber';
var
  LChannel: TGeneralFlowChannel;
begin
  Result := NullInteger;
  try
    LChannel :=  GetCastChannelByChannelNumber(AChannelNumber);
    if(LChannel <> nil) then
      Result := LChannel.ChannelID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelList.ChannelNumberFromChannelID(AChannelID: integer): integer;
const OPNAME = 'TChannelList.ChannelNumberFromChannelID';
var
  LChannel: TGeneralFlowChannel;
begin
  Result := NullInteger;
  try
    LChannel :=  CastGeneralFlowChannelByID(AChannelID);
    if(LChannel <> nil) then
      Result := LChannel.ChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGeneralFlowChannel.Assign(AGeneralFlowChannel: TGeneralFlowChannel);
const OPNAME = 'TGeneralFlowChannel.Assign';
var
  LChnnelPenalty : IChannelPenalty;
begin
  try
    if AGeneralFlowChannel <> nil then
    begin
      ChannelName                   := 'Copy of '+AGeneralFlowChannel.FChannelName;
      ChannelType                   := AGeneralFlowChannel.FChannelType;
      ChannelSubType                := AGeneralFlowChannel.FChannelSubType;
      UpStreamNodeNumber            := AGeneralFlowChannel.FUpStreamNodeNumber;
      DownStreamNodeNumber          := AGeneralFlowChannel.FDownStreamNodeNumber;
      SummaryOutputRequired         := AGeneralFlowChannel.FSummaryOutputRequired;
      RequiresFirmYieldAnalysis     := AGeneralFlowChannel.FRequiresFirmYieldAnalysis;
      FlowOutput                    := AGeneralFlowChannel.FFlowOutput;
      FMinimumFlowConstraintNr      := AGeneralFlowChannel.FMinimumFlowConstraintNr;
      FMinMaxFlowConstraintNr       := AGeneralFlowChannel.FMinMaxFlowConstraintNr;
      FLossFeatureNr                := AGeneralFlowChannel.FLossFeatureNr;
      FSpecifiedDemandFeatureNr     := AGeneralFlowChannel.FSpecifiedDemandFeatureNr;
      FDiversionFeatureNr           := AGeneralFlowChannel.FDiversionFeatureNr;
      FPhysicalFlowConstraintNr     := AGeneralFlowChannel.FPhysicalFlowConstraintNr;
      FIFRFeatureNr                 := AGeneralFlowChannel.FIFRFeatureNr;
      FIrrigationAreaNr             := AGeneralFlowChannel.FIrrigationAreaNr;
      FPowerPlantNr                 := AGeneralFlowChannel.FPowerPlantNr;
      FSpecifiedInflowFeatureNr     := AGeneralFlowChannel.FSpecifiedDemandFeatureNr;
      FWaterDemandFeatureNr         := AGeneralFlowChannel.FWaterDemandFeatureNr;
      FMasterControlFeatureNr       := AGeneralFlowChannel.FMasterControlFeatureNr;
      FChannelAreaID                := AGeneralFlowChannel.FChannelAreaID;
      LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                        ChannelPenaltyByIdentifier[AGeneralFlowChannel.FChannelPenaltyNumber];
      if LChnnelPenalty <> nil then
        Set_ChannelPenalty(LChnnelPenalty);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannel.Get_MultiResChannelCurtailByChannelNo(AChannelNo: Integer): IMultiResMultiChannelCurtail;
const OPNAME = 'TGeneralFlowChannel.Get_MultiResChannelCurtailByChannelNo';
begin
  Result := nil;
  try
    if (FAppModules.Model.ModelName = CPlanning) then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).
                CastMultiRestrictionData.RestrictionByChannelNo[AChannelNo];

  except on E: Exception do HandleError(E,OPNAME)  end;
end;

//===============================================TChannelList==================================

function TChannelList.CopyChannel(AChannelNumber : integer): IGeneralFlowChannel;
const OPNAME = 'TChannelList.CopyChannel';
var
  LChannel : TGeneralFlowChannel;
  LChannelCopy : TGeneralFlowChannel;
  LMasterControlFeatureCopy : IMasterControlFeature;
  LLossFeatureCopy : ILossFeature;
  LMinimumFlowConstraintCopy : IMinimumFlowConstraint;
  LMinMaxConstraintCopy : IMinMaxFlowConstraint;
  LPumpingFeatureCopy : IPumpingFeature;
  LDemandFeatureCopy : ISpecifiedDemandFeature;
  LDivFeatureCopy : IDiversionFeature;
  LPhysConstraintCopy : IPhysicalFlowConstraint;
  LIFRFeatureCopy : IIFRFeature;
  LSpecInflowCopy : ISpecifiedInflowFeature;
  LWaterDemandFeatureCopy : IWaterDemandFeature;
  LChannelNumber        : integer;
begin
  Result := nil;
  try
    LChannel := CastChannelByChannelNumber[AChannelNumber];
    if LChannel <> nil then
    begin
      LChannelCopy := CreateNewChannel;
      LChannelCopy.Assign(LChannel);
      LChannelNumber := LChannelCopy.FChannelNumber;
      Result := LChannelCopy;
      if LChannel.MasterControlFeature <> nil then
      begin
        LMasterControlFeatureCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MasterControlFeatureList.
                                 CopyMasterControlFeature(LChannelNumber,AChannelNumber);
        if LMasterControlFeatureCopy <> nil then
          Result.MasterControlFeature := LMasterControlFeatureCopy;
      end;
      if LChannel.MinimumFlowConstraint <> nil then
      begin
        LMinimumFlowConstraintCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MinimumFlowConstraintList.
                                 CopyMinimumFlowConstraint(LChannelNumber,AChannelNumber);
        if LMinimumFlowConstraintCopy <> nil then
          Result.MinimumFlowConstraint := LMinimumFlowConstraintCopy;
      end;
      if LChannel.LossFeature <> nil then
      begin
        LLossFeatureCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.LossFeatureList.
                             CopyLossFeature(LChannelNumber,AChannelNumber);
        if LLossFeatureCopy <> nil then
          Result.LossFeature := LLossFeatureCopy;
      end;
      if LChannel.MinMaxFlowConstraint <> nil then
      begin
        LMinMaxConstraintCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MinMaxFlowConstraintList.
                                 CopyMinMaxFlowConstraint(LChannelNumber,AChannelNumber);
        if LMinMaxConstraintCopy <> nil then
          Result.MinMaxFlowConstraint := LMinMaxConstraintCopy;
      end;
      if LChannel.PumpingFeature <> nil then
      begin
        LPumpingFeatureCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.PumpingFeatureList.
                                 CopyPumpingFeature(LChannelNumber,AChannelNumber);
        if LPumpingFeatureCopy <> nil then
          Result.PumpingFeature := LPumpingFeatureCopy;
      end;
      if LChannel.SpecifiedDemandFeature <> nil then
      begin
        LDemandFeatureCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.SpecifiedDemandFeatureList.
                                 CopySpecifiedDemandFeature(LChannelNumber,AChannelNumber);
        if LDemandFeatureCopy <> nil then
          Result.SpecifiedDemandFeature := LDemandFeatureCopy;
      end;
      if LChannel.DiversionFeature <> nil then
      begin
        LDivFeatureCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.DiversionFeatureList.
                           CopyDiversionFeature(LChannelNumber,AChannelNumber);
        if LDivFeatureCopy <> nil then
          Result.DiversionFeature := LDivFeatureCopy;
      end;
      if LChannel.PhysicalFlowConstraint <> nil then
      begin
        LPhysConstraintCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.PhysicalFlowConstraintList.
                           CopyPhysicalFlowConstraint(LChannelNumber,AChannelNumber);
        if LPhysConstraintCopy <> nil then
          Result.PhysicalFlowConstraint := LPhysConstraintCopy;
      end;
      if LChannel.IFRFeature <> nil then
      begin
        LIFRFeatureCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                           IFRFeatureList.CopyIFRFeature(LChannelNumber,AChannelNumber,
                           LChannel.IFRFeature.ReferenceFlowType);
        if LIFRFeatureCopy <> nil then
          Result.IFRFeature := LIFRFeatureCopy;
      end;
      if LChannel.SpecifiedInflowFeature <> nil then
      begin
        LSpecInflowCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.SpecifiedInflowFeatureList.
                           CopySpecifiedInflowFeature(LChannelNumber,AChannelNumber);
        if LSpecInflowCopy <> nil then
          Result.SpecifiedInflowFeature := LSpecInflowCopy;
      end;
      if LChannel.WaterDemandFeature <> nil then
      begin
        LWaterDemandFeatureCopy := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WaterDemandFeatureList.
                                   CopyWaterDemandFeature(LChannelNumber,AChannelNumber);
        if LWaterDemandFeatureCopy <> nil then
          Result.WaterDemandFeature := LWaterDemandFeatureCopy;
      end;


    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelList.Get_ChannelByName(const AName: WideString): IGeneralFlowChannel;
const OPNAME = 'TChannelList.Get_ChannelByName';
var
 LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FChannelList.Count -1 do
      if(TGeneralFlowChannel(FChannelList[LIndex]).ChannelName = AName) then
      begin
        Result := TGeneralFlowChannel(FChannelList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
