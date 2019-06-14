//
//
//  UNIT      : Contains TAllocationDefinitionFileDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 02/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UAllocationDefinitionFileDataObjects;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UDataFileObjects,
  UAbstractObject;

type
  TTwentyIntArray = array[1..20] of TInteger;
  TTenIntArray = array[1..10] of TInteger;
  TTenDoubleArray = array[1..10] of TDouble;
  TCoefArray = array[1..4] of TDouble;
  TAnualValuesArray = array[1..12] of TInteger;
  TClassValuesArray = array[1..5] of TDouble;
  TFiveIntArray = array[1..6] of TInteger;
  TSixIntArray = array[1..6] of TInteger;
  TRILabelArray = array[1..5] of TString;

  TClassValuesArrayObject = class(TAbstractDataObject)
  protected
    FClassValuesArray :TClassValuesArray;
    FComment          :TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property ClassValuesArray : TClassValuesArray read FClassValuesArray;
    property Comment : TString read FComment;
  end;

  TRoutingSupportChannelNumbers = class(TAbstractDataObject)
  protected
    FChannelNumbers: TFiveIntArray;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property ChannelNumbers : TFiveIntArray read FChannelNumbers;
  end;

  // Line 7
  TDemandSupportDefinition = class(TAbstractDataObject)
  protected

    // Line 7a
    FSubsystemNo1      : TInteger;

    // Line 7b
    FGrowthFlag       : TDouble;

    // Line 7c
    FTargetDemand     : TDouble;

    // Line 7d
    FDemandDefName    : TString;

    // Line 7e
    FDemandCentreID   : TInteger;

    // Line 7f
    FUserCategory     : TInteger;

    // Line 7g
    FSupportArc1      : TInteger;

    // Line 7h
    FSupportArc2      : TInteger;

    // Line 7i
    FSupportSubsystemsCount : TInteger;

    // Line 7j
    FSupportSystemNr  : TInteger;

    // Line 7 Other
    FComment  : TString;

    // Line 7k
    FRoutingSupportChannelNumbers  : TObjectList;

    // Line 7l
    FSupportSubsystemsNumbers : TTenIntArray;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetRoutingSupportChannelNumbersByIndex(AIndex: integer):TRoutingSupportChannelNumbers;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddRoutingSupportChannelNumbers:TRoutingSupportChannelNumbers;
    function RoutingSupportChannelNumbersCount:integer;
    property SubsystemNo1     : TInteger read FSubsystemNo1;
    property GrowthFlag       : TDouble  read FGrowthFlag;
    property TargetDemand     : TDouble  read FTargetDemand;
    property DemandDefName    : TString  read FDemandDefName;
    property DemandCentreID   : TInteger read FDemandCentreID;
    property UserCategory     : TInteger read FUserCategory;
    property SupportArc1      : TInteger read FSupportArc1;
    property SupportArc2      : TInteger read FSupportArc2;
    property Comment          : TString  read FComment;
    property SupportSubsystemsCount   : TInteger read FSupportSubsystemsCount;
    property SupportSystemNr          : TInteger read FSupportSystemNr;
    property RoutingSupportChannelNumbersByIndex[AIndex: integer]  : TRoutingSupportChannelNumbers  read GetRoutingSupportChannelNumbersByIndex;
    property SupportSubsystemsNumbers : TTenIntArray read FSupportSubsystemsNumbers;
  end;


  // Line 10
  TNonFirmSubsystem      = class(TAbstractDataObject)
  protected
    FNonFirmSubsystemNo    : TInteger;
    FRoutingChannelNoArray : TSixIntArray;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property NonFirmSubsystemNo    : TInteger      read FNonFirmSubsystemNo;
    property RoutingChannelNoArray : TSixIntArray read FRoutingChannelNoArray;
  end;

  // Line 11
  TSubsystem      = class(TAbstractDataObject)
  protected
    FSubsystemName          : TString;
    FSubsystemNo2           : TInteger;
    FSupportSubsystemNo     : TInteger;
    FChannelNo              : TInteger;
    FShortTermFirmYield     : TDouble;
    FStreamFlow             : TDouble;
    FLongTermFirmYield      : TDouble;
    FFamilyCurveStartYear   : TInteger;
    FFamilyCurveStartMonth  : TInteger;
    FFamilyCurveEndYear     : TInteger;
    FFamilyCurveEndMonth    : TInteger;
    FFirmFlag               : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    procedure Assign(ASubsystem:TSubsystem);
    property SubsystemName          : TString  read FSubsystemName;
    property SubsystemNo2           : TInteger read FSubsystemNo2;
    property SupportSubsystemNo     : TInteger read FSupportSubsystemNo;
    property ChannelNo              : TInteger read FChannelNo;
    property ShortTermFirmYield     : TDouble  read FShortTermFirmYield;
    property StreamFlow             : TDouble  read FStreamFlow;
    property LongTermFirmYield      : TDouble  read FLongTermFirmYield;
    property FamilyCurveStartYear   : TInteger read FFamilyCurveStartYear;
    property FamilyCurveStartMonth  : TInteger read FFamilyCurveStartMonth;
    property FamilyCurveEndYear     : TInteger read FFamilyCurveEndYear;
    property FamilyCurveEndMonth    : TInteger read FFamilyCurveEndMonth;
    property FirmFlag               : TString  read FFirmFlag;
  end;

  // Line 12
  TStartVolume      = class(TAbstractDataObject)
  protected
    FStartVolume      : TDouble;
    FCurveSetNo       : TInteger;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property StartVolume      : TDouble  read FStartVolume;
    property CurveSetNo       : TInteger read FCurveSetNo;
  end;

  // Line 13
  TScenarioCoefficients      = class(TAbstractDataObject)
  protected
    FLineTargetDraft      : TDouble;
    FCoefArray            : TCoefArray;
    FRiskProp             : TDouble;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property LineTargetDraft      : TDouble    read FLineTargetDraft;
    property CoefArray            : TCoefArray read FCoefArray;
    property RiskProp             : TDouble    read FRiskProp;
  end;

  // Line 12 and 13
  TStartVolumeAndScenarioCoefficients      = class(TAbstractDataObject)
  protected
    FStartVolume            : TStartVolume;
    FScenarioCoefficients   : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetScenarioCoefficientsByIndex(AIndex: integer):TScenarioCoefficients;
  public
    procedure Reset;override;
    function AddScenarioCoefficients:TScenarioCoefficients;
    function ScenarioCoefficientCount: integer;
    function Initialise: boolean;override;
    property StartVolume            : TStartVolume          read FStartVolume;
    property ScenarioCoefficientsByIndex[AIndex: integer]   : TScenarioCoefficients read GetScenarioCoefficientsByIndex;
  end;


  // Line 11 and 12 and 13
  TCoefficients = class(TAbstractDataObject)
  protected
    FSubsystem  : TSubsystem;
    FStartVolumeAndScenarioCoefficients: TObjectList;
    FCurveSetIndex: integer;
    FSubsystemIndex: integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetStartVolumeAndScenarioCoefficientsByIndex(AIndex: integer):TStartVolumeAndScenarioCoefficients;
  public
    function AddStartVolumeAndScenarioCoefficients:TStartVolumeAndScenarioCoefficients;
    function StartVolumeAndScenarioCoefficientCount: integer;
    procedure Reset;override;
    function Initialise: boolean;override;
    property CurveSetIndex  : integer  read FCurveSetIndex  write FCurveSetIndex;
    property SubsystemIndex : integer  read FSubsystemIndex write FSubsystemIndex;
    property Subsystem            : TSubsystem          read FSubsystem;
    property StartVolumeAndScenarioCoefficientsByIndex[AIndex: integer]   : TStartVolumeAndScenarioCoefficients read GetStartVolumeAndScenarioCoefficientsByIndex;
  end;

  // Line 14
  TSystemFullPerNodes      = class(TAbstractDataObject)
  protected
    FSubsystemNo3   : TInteger;
    FNodeNoArray    : TTwentyIntArray;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property SubsystemNo3 : TInteger        read FSubsystemNo3;
    property NodeNoArray  : TTwentyIntArray read FNodeNoArray;
  end;

  // Line 17 & 19
  TFixedSubSystem      = class(TAbstractDataObject)
  protected
    FSubsystemNo4    : TInteger;
    FSubsystemPos    : TInteger;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property SubsystemNo4   : TInteger      read FSubsystemNo4;
    property SubsystemPos   : TInteger      read FSubsystemPos;
  end;

  // Line 21
  TSupportChannel      = class(TAbstractDataObject)
  protected
    FSupportChannelNo    : TInteger;
    FSubsystemNo5        : TInteger;
    FInfluencedSubSytem  : TTenIntArray;
    FInfluenceFactor     : TTenDoubleArray;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property SupportChannelNo   : TInteger      read FSupportChannelNo;
    property SubsystemNo5       : TInteger      read FSubsystemNo5;
    property InfluencedSubSytem : TTenIntArray  read FInfluencedSubSytem;
    property InfluenceFactor    : TTenDoubleArray  read FInfluenceFactor;
  end;


  TAllocationDefinitionFileDataObject = class(TAbstractDataObject)
  protected
    FFileNumber    : integer;

    // Line 1
    FClassCount    : TInteger;
    FLevelCount    : TInteger;
    FCategoryCount : TInteger;
    FPeriodLength  : TInteger;
    FImplemntationDecision  : TString;

    // Line 2
    FRIValueArray  :TFiveIntArray;

    // Line 3
    FRILabelArray  :TRILabelArray;

    // Line 4
    FCurtailments  : TObjectList;

    // Line 5
    FDistributions  :   TObjectList;

    // Line 6
    FDemandSupportDefCount    : TInteger;

    // Line 7
    FDemandSupportDefinitions : TObjectList;

    // Line 8
    FYieldCurveSubsCount    : TInteger;
    FYieldCurveSetsCount    : TInteger;
    FFamilyLoadCaseCount    : TInteger;
    FCurveSetsPerMonthCount : TInteger;

    // Line 9
    FDecisionIndicatorArray  : TAnualValuesArray;

    // Line 10
    FNonFirmSubsystems  : TObjectList;

    // Line 11 and 12 and 13
    FCoefficients : TObjectList;

    // Line 14
    FSystemFullPerNode : TObjectList;

    // Line 15
    FSupportStrategyType    : TInteger;

    // Line 16
    FFixedPosSubsystemCount    : TInteger;

    // Line 17
    FFixedSubSystems : TObjectList;

    // Line 18
    FSequentialPosSubsystemCount    : TInteger;

    // Line 19
    FSequentialSubSystems : TObjectList;

    // Line 20
    FSupportStructureCount    : TInteger;

    // Line 21
    FSupportChannels : TObjectList;

    // Line 22
    FBalancingOption    : TInteger;

    //Line 23... : Extra useless lines
    FFMExtraLines: TStringList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDemandSupportDefinitionByIndex(AIndex: integer): TDemandSupportDefinition;
    function GetNonFirmSubsystemByIndex(AIndex: integer):TNonFirmSubsystem;
    function GetCoefficientByIndex(AIndex: integer):TCoefficients;
    function GetSystemFullPerNodeByIndex(AIndex: integer):TSystemFullPerNodes;
    function GetCurtailmentByIndex(AIndex: integer):TClassValuesArrayObject;
    function GetDistributionByIndex(AIndex: integer):TClassValuesArrayObject;
    function GetFixedSubSystemByIndex(AIndex: integer):TFixedSubSystem;
    function GetSequentialSubSystemByIndex(AIndex: integer):TFixedSubSystem;
    function GetSupportChannelByIndex(AIndex: integer):TSupportChannel;

  public
    constructor Create(AFileNumber: integer); reintroduce;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddDemandSupportDefinition:TDemandSupportDefinition;
    function AddNonFirmSubsystem:TNonFirmSubsystem;
    function AddCoefficient:TCoefficients;
    function AddSystemFullPerNode:TSystemFullPerNodes;
    function AddCurtailment:TClassValuesArrayObject;
    function AddDistribution:TClassValuesArrayObject;
    function AddFixedSubSystem:TFixedSubSystem;
    function AddSequentialSubSystem:TFixedSubSystem;
    function AddSupportChannel:TSupportChannel;

    function DemandSupportDefinitionCount:integer;
    function NonFirmSubsystemCount:integer;
    function CoefficientCount:integer;
    function SystemFullPerNodeCount:integer;
    function CurtailmentCount:integer;
    function DistributionCount:integer;
    function FixedSubSystemCount:integer;
    function SequentialSubSystemCount:integer;
    function SupportChannelCount:integer;

    property FileNumber             : integer           read FFileNumber;
    property ClassCount             : TInteger          read FClassCount             ;
    property LevelCount             : TInteger          read FLevelCount             ;
    property CategoryCount          : TInteger          read FCategoryCount          ;
    property PeriodLength           : TInteger          read FPeriodLength           ;
    property ImplemntationDecision  : TString           read FImplemntationDecision  ;
    property RIValueArray           : TFiveIntArray     read FRIValueArray           ;
    property RILabelArray           : TRILabelArray     read FRILabelArray           ;
    property Curtailments           : TObjectList       read FCurtailments           ;
    property DemandSupportDefCount  : TInteger          read FDemandSupportDefCount  ;
    property YieldCurveSubsCount    : TInteger          read FYieldCurveSubsCount    ;
    property YieldCurveSetsCount    : TInteger          read FYieldCurveSetsCount    ;
    property FamilyLoadCaseCount    : TInteger          read FFamilyLoadCaseCount    ;
    property CurveSetsPerMonthCount : TInteger          read FCurveSetsPerMonthCount ;
    property DecisionIndicatorArray : TAnualValuesArray read FDecisionIndicatorArray ;
    property SupportStrategyType    : TInteger          read FSupportStrategyType    ;
    property FixedPosSubsystemCount : TInteger          read FFixedPosSubsystemCount ;
    property SequentialPosSubsystemCount : TInteger     read FSequentialPosSubsystemCount ;
    property SupportStructureCount  : TInteger          read FSupportStructureCount ;
    property BalancingOption        : TInteger          read FBalancingOption ;

    property DemandSupportDefinitionByIndex[AIndex: integer] : TDemandSupportDefinition read GetDemandSupportDefinitionByIndex;
    property NonFirmSubsystemByIndex[AIndex: integer]        : TNonFirmSubsystem        read GetNonFirmSubsystemByIndex;
    property CoefficientByIndex[AIndex: integer]             : TCoefficients            read GetCoefficientByIndex;
    property SystemFullPerNodeByIndex[AIndex: integer]       : TSystemFullPerNodes      read GetSystemFullPerNodeByIndex;
    property CurtailmentByIndex[AIndex: integer]             : TClassValuesArrayObject  read GetCurtailmentByIndex;
    property DistributionByIndex[AIndex: integer]            : TClassValuesArrayObject  read GetDistributionByIndex;
    property FixedSubSystemsByIndex[AIndex: integer]         : TFixedSubSystem          read GetFixedSubSystemByIndex;
    property SequentialSubSytemByIndex[AIndex: integer]      : TFixedSubSystem          read GetSequentialSubSystemByIndex;
    property SupportChannelByIndex[AIndex: integer]          : TSupportChannel          read GetSupportChannelByIndex;
    property FMExtraLines                                    : TStringList              read FFMExtraLines ;
  end;

  TSubsystemList = class(TObjectList)
  public
    function AddSubsystem:TSubsystem;
    function SubsystemByIndex(AIndex: integer):TSubsystem;
  end;
implementation


{ TAllocationDefinitionFileDataObject }
uses
  UErrorHandlingOperations;

constructor TAllocationDefinitionFileDataObject.Create(AFileNumber: integer);
const OPNAME = 'TAllocationDefinitionFileDataObject.Create';
begin
  try
    FFileNumber := AFileNumber;
    inherited Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAllocationDefinitionFileDataObject.CreateMemberObjects;
const OPNAME = 'TAllocationDefinitionFileDataObject.CreateMemberObjects';
var
  LInteger: TInteger;
  LString : TString;             
  LIndex  : integer;
begin
  inherited CreateMemberObjects;
  try
    // Line 1
    FClassCount               := TInteger.Create;
    FLevelCount               := TInteger.Create;
    FCategoryCount            := TInteger.Create;
    FPeriodLength             := TInteger.Create;
    FImplemntationDecision    := TString.Create;

    // Line 2
    for LIndex := Low(FRIValueArray) to High(FRIValueArray) do
    begin
      LInteger := TInteger.Create;
      FRIValueArray[LIndex] := LInteger;
    end;

    // Line 3
    for LIndex := Low(FRILabelArray) to High(FRILabelArray) do
    begin
      LString := TString.Create;
      FRILabelArray[LIndex] := LString;
    end;

    // Line 4
    FCurtailments             := TObjectList.Create(True);

    // Line 5
    FDistributions            := TObjectList.Create(True);

    // Line 6
    FDemandSupportDefCount    := TInteger.Create;

    // Line 7
    FDemandSupportDefinitions := TObjectList.Create(True);

    // Line 8
    FYieldCurveSubsCount      := TInteger.Create;
    FYieldCurveSetsCount      := TInteger.Create;
    FFamilyLoadCaseCount      := TInteger.Create;
    FCurveSetsPerMonthCount   := TInteger.Create;

    // Line 9
    for LIndex := Low(FDecisionIndicatorArray) to High(FDecisionIndicatorArray) do
    begin
      LInteger := TInteger.Create;
      FDecisionIndicatorArray[LIndex] := LInteger;
    end;

    // Line 10
    FNonFirmSubsystems        := TObjectList.Create(True);

    // Line 11 and 12 and 13
    FCoefficients             := TObjectList.Create(True);

    // Line 14
    FSystemFullPerNode        := TObjectList.Create(True);

    // Line 15
    FSupportStrategyType      := TInteger.Create;

    // Line 16
    FFixedPosSubsystemCount   := TInteger.Create;

    //Line 17
    FFixedSubSystems          := TObjectList.Create(True);

    // Line 18
    FSequentialPosSubsystemCount    := TInteger.Create;

    // Line 19
    FSequentialSubSystems := TObjectList.Create(True);

    // Line 20
    FSupportStructureCount    := TInteger.Create;

    // Line 21
    FSupportChannels          := TObjectList.Create(True);

    // Line 22
    FBalancingOption := TInteger.Create;


    //Line 23... : Extra useless lines
    FFMExtraLines             := TStringList.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAllocationDefinitionFileDataObject.DestroyMemberObjects;
const OPNAME = 'TAllocationDefinitionFileDataObject.DestroyMemberObjects';
var
  LIndex  : integer;
begin
  inherited DestroyMemberObjects;
  try
    // Line 1
    FClassCount.Free;
    FLevelCount.Free;
    FCategoryCount.Free;
    FPeriodLength.Free;
    FImplemntationDecision.Free;
    
    // Line 2
    for LIndex := Low(FRIValueArray) to High(FRIValueArray) do
    begin
      FRIValueArray[LIndex].Free;
      FRIValueArray[LIndex] := nil;
    end;

    // Line 3
    for LIndex := Low(FRILabelArray) to High(FRILabelArray) do
    begin
      FRILabelArray[LIndex].Free;
      FRILabelArray[LIndex] := nil;
    end;

    // Line 4
    FCurtailments.Clear;
    FCurtailments.Free;

    // Line 5
    FDistributions.Clear;
    FDistributions.Free;
    // Line 6
    FDemandSupportDefCount.Free;

    // Line 7
    FDemandSupportDefinitions.Clear;
    FDemandSupportDefinitions.Free;

    // Line 8
    FYieldCurveSubsCount.Free;
    FYieldCurveSetsCount.Free;
    FFamilyLoadCaseCount.Free;
    FCurveSetsPerMonthCount.Free;

    // Line 9
    for LIndex := Low(FDecisionIndicatorArray) to High(FDecisionIndicatorArray) do
    begin
      FDecisionIndicatorArray[LIndex].Free;
      FDecisionIndicatorArray[LIndex] := nil;
    end;

    // Line 10
    FNonFirmSubsystems.Clear;
    FNonFirmSubsystems.Free;

    // Line 11 and 12 and 13
    FCoefficients.Clear;
    FCoefficients.Free;

    // Line 14
    FSystemFullPerNode.Clear;
    FSystemFullPerNode.Free;

    // Line 15
    FSupportStrategyType.Free;

    // Line 16
    FFixedPosSubsystemCount.Free;

    //Line 17
    FFixedSubSystems.Clear;
    FFixedSubSystems.Free;

    // Line 18
    FSequentialPosSubsystemCount.Free;

    // Line 19
    FSequentialSubSystems.Clear;
    FSequentialSubSystems.Free;

    // Line 20
    FSupportStructureCount.Free;

    // Line 21
    FSupportChannels.Clear;
    FSupportChannels.Free;

    // Line 22
    FBalancingOption.Free;

    //Line 23... : Extra useless lines
    FFMExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.Initialise: boolean;
const OPNAME = 'TAllocationDefinitionFileDataObject.Initialise';
var
  LIndex  : integer;
begin
  Result := inherited Initialise;
  try
    // Line 1
    FClassCount.FData := 0;
    FClassCount.FInitalised := False;
    FClassCount.FLength := 5;
    FClassCount.FDecimal := 0;
    FClassCount.FDefaultPadding := True;

    FLevelCount.FData := 0;
    FLevelCount.FInitalised := False;
    FLevelCount.FLength := 5;
    FLevelCount.FDecimal := 0;
    FLevelCount.FDefaultPadding := True;

    FCategoryCount.FData := 0;
    FCategoryCount.FInitalised := False;
    FCategoryCount.FLength := 5;
    FCategoryCount.FDecimal := 0;
    FCategoryCount.FDefaultPadding := True;

    FPeriodLength.FData := 0;
    FPeriodLength.FInitalised := False;
    FPeriodLength.FLength := 5;
    FPeriodLength.FDecimal := 0;
    FPeriodLength.FDefaultPadding := True;

    FImplemntationDecision.FData := '';
    FImplemntationDecision.FInitalised := False;
    FImplemntationDecision.FLength := 5;
    FImplemntationDecision.FDecimal := 0;
    FImplemntationDecision.FDefaultPadding := True;

    // Line 2
    for LIndex := Low(FRIValueArray) to High(FRIValueArray) do
    begin
      FRIValueArray[LIndex].FData := 0;
      FRIValueArray[LIndex].FInitalised := False;
      FRIValueArray[LIndex].FLength := 10;
      FRIValueArray[LIndex].FDecimal := 0;
      FRIValueArray[LIndex].FDefaultPadding := True;
    end;

    // Line 3
    for LIndex := Low(FRILabelArray) to High(FRILabelArray) do
    begin
      FRILabelArray[LIndex].FData := '';
      FRILabelArray[LIndex].FInitalised := False;
      FRILabelArray[LIndex].FLength := 10;
      FRILabelArray[LIndex].FDecimal := 0;
      FRILabelArray[LIndex].FDefaultPadding := True;
    end;

    // Line 4
    FCurtailments.Clear;

    // Line 5
    FDistributions.Clear;

    // Line 6
    FDemandSupportDefCount.FData := 0;
    FDemandSupportDefCount.FInitalised := False;
    FDemandSupportDefCount.FLength := 5;
    FDemandSupportDefCount.FDecimal := 0;
    FDemandSupportDefCount.FDefaultPadding := True;

    // Line 7
    FDemandSupportDefinitions.Clear;

    // Line 8
    FYieldCurveSubsCount.FData := 0;
    FYieldCurveSubsCount.FInitalised := False;
    FYieldCurveSubsCount.FLength := 3;
    FYieldCurveSubsCount.FDecimal := 0;
    FYieldCurveSubsCount.FDefaultPadding := True;

    FYieldCurveSetsCount.FData := 0;
    FYieldCurveSetsCount.FInitalised := False;
    FYieldCurveSetsCount.FLength := 2;
    FYieldCurveSetsCount.FDecimal := 0;
    FYieldCurveSetsCount.FDefaultPadding := True;

    FFamilyLoadCaseCount.FData := 0;
    FFamilyLoadCaseCount.FInitalised := False;
    FFamilyLoadCaseCount.FLength := 4;
    FFamilyLoadCaseCount.FDecimal := 0;
    FFamilyLoadCaseCount.FDefaultPadding := True;

    FCurveSetsPerMonthCount.FData := 0;
    FCurveSetsPerMonthCount.FInitalised := False;
    FCurveSetsPerMonthCount.FLength := 3;
    FCurveSetsPerMonthCount.FDecimal := 0;
    FCurveSetsPerMonthCount.FDefaultPadding := True;

    // Line 9
    for LIndex := Low(FDecisionIndicatorArray) to High(FDecisionIndicatorArray) do
    begin
      FDecisionIndicatorArray[LIndex].FData := 0;
      FDecisionIndicatorArray[LIndex].FInitalised := False;
      FDecisionIndicatorArray[LIndex].FLength := 2;
      FDecisionIndicatorArray[LIndex].FDecimal := 0;
      FDecisionIndicatorArray[LIndex].FDefaultPadding := True;
    end;

    // Line 10
    FNonFirmSubsystems.Clear;

    // Line 11 and 12 and 13
    FCoefficients.Clear;

    // Line 14
    FSystemFullPerNode.Clear;

    // Line 15
    FSupportStrategyType.FData := 0;
    FSupportStrategyType.FInitalised := False;
    FSupportStrategyType.FLength := 2;
    FSupportStrategyType.FDecimal := 0;
    FSupportStrategyType.FDefaultPadding := True;

    // Line 16
    FFixedPosSubsystemCount.FData := 0;
    FFixedPosSubsystemCount.FInitalised := False;
    FFixedPosSubsystemCount.FLength := 2;
    FFixedPosSubsystemCount.FDecimal := 0;
    FFixedPosSubsystemCount.FDefaultPadding := True;

    // Line 17
    FFixedSubSystems.Clear;

    // Line 18
    FSequentialPosSubsystemCount.FData := 0;
    FSequentialPosSubsystemCount.FInitalised := False;
    FSequentialPosSubsystemCount.FLength := 2;
    FSequentialPosSubsystemCount.FDecimal := 0;
    FSequentialPosSubsystemCount.FDefaultPadding := True;

    // Line 19
    FSequentialSubSystems.Clear;

    // Line 20
    FSupportStructureCount.FData := 0;
    FSupportStructureCount.FInitalised := False;
    FSupportStructureCount.FLength := 2;
    FSupportStructureCount.FDecimal := 0;
    FSupportStructureCount.FDefaultPadding := True;

    // Line 21
    FSupportChannels.Clear;

    // Line 22
    FBalancingOption.FData := 0;
    FBalancingOption.FInitalised := False;
    FBalancingOption.FLength := 2;
    FBalancingOption.FDecimal := 0;
    FBalancingOption.FDefaultPadding := True;

    //Line 23... : Extra useless lines
    FFMExtraLines.Clear;

    Result :=  True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAllocationDefinitionFileDataObject.Reset;
const OPNAME = 'TAllocationDefinitionFileDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddDemandSupportDefinition: TDemandSupportDefinition;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddDemandSupportDefinition';
begin
  Result := nil;
  try
    Result := TDemandSupportDefinition.Create;
    FDemandSupportDefinitions.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddCoefficient: TCoefficients;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddCoefficient';
begin
  Result := nil;
  try
    Result := TCoefficients.Create;
    FCoefficients.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddNonFirmSubsystem: TNonFirmSubsystem;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddNonFirmSubsystem';
begin
  Result := nil;
  try
    Result := TNonFirmSubsystem.Create;
    FNonFirmSubsystems.Add(Result);
    Result.FNonFirmSubsystemNo.FData := FNonFirmSubsystems.Count;
    Result.FNonFirmSubsystemNo.FInitalised := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddSystemFullPerNode: TSystemFullPerNodes;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddSystemFullPerNode';
begin
  Result := nil;
  try
    Result := TSystemFullPerNodes.Create;
    FSystemFullPerNode.Add(Result);
    Result.FSubsystemNo3.FData := FSystemFullPerNode.Count;
    Result.FSubsystemNo3.FInitalised := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddCurtailment: TClassValuesArrayObject;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddCurtailment';
begin
  Result := nil;
  try
    Result := TClassValuesArrayObject.Create;
    FCurtailments.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddDistribution: TClassValuesArrayObject;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddDistribution';
begin
  Result := nil;
  try
    Result := TClassValuesArrayObject.Create;
    FDistributions.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddFixedSubSystem: TFixedSubSystem;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddFixedSubSystem';
begin
  Result := nil;
  try
    Result := TFixedSubSystem.Create;
    FFixedSubSystems.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddSequentialSubSystem: TFixedSubSystem;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddSequentialSubSystem';
begin
  Result := nil;
  try
    Result := TFixedSubSystem.Create;
    FSequentialSubSystems.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.AddSupportChannel: TSupportChannel;
const OPNAME = 'TAllocationDefinitionFileDataObject.AddSupportChannel';
begin
  Result := nil;
  try
    Result := TSupportChannel.Create;
    FSupportChannels.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetCoefficientByIndex(AIndex: integer): TCoefficients;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetCoefficientByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FCoefficients.Count) then
      Result := TCoefficients(FCoefficients[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetDemandSupportDefinitionByIndex(AIndex: integer): TDemandSupportDefinition;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetDemandSupportDefinitionByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDemandSupportDefinitions.Count) then
      Result := TDemandSupportDefinition(FDemandSupportDefinitions[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetNonFirmSubsystemByIndex(AIndex: integer): TNonFirmSubsystem;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetNonFirmSubsystemByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FNonFirmSubsystems.Count) then
      Result := TNonFirmSubsystem(FNonFirmSubsystems[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetSystemFullPerNodeByIndex(AIndex: integer): TSystemFullPerNodes;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetSystemFullPerNodeByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FSystemFullPerNode.Count) then
      Result := TSystemFullPerNodes(FSystemFullPerNode[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetCurtailmentByIndex(AIndex: integer): TClassValuesArrayObject;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetCurtailmentByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FCurtailments.Count) then
      Result := TClassValuesArrayObject(FCurtailments[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetDistributionByIndex(AIndex: integer): TClassValuesArrayObject;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetDistributionByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDistributions.Count) then
      Result := TClassValuesArrayObject(FDistributions[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetFixedSubSystemByIndex(AIndex: integer): TFixedSubSystem;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetFixedSubSystemByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FFixedSubSystems.Count) then
      Result := TFixedSubSystem(FFixedSubSystems[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetSequentialSubSystemByIndex(AIndex: integer): TFixedSubSystem;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetSequentialSubSystemByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FSequentialSubSystems.Count) then
      Result := TFixedSubSystem(FSequentialSubSystems[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.GetSupportChannelByIndex(AIndex: integer): TSupportChannel;
const OPNAME = 'TAllocationDefinitionFileDataObject.GetSupportChannelByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FSupportChannels.Count) then
      Result := TSupportChannel(FSupportChannels[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.CoefficientCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.CoefficientCount';
begin
  Result := 0;
  try
    Result := FCoefficients.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.CurtailmentCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.CurtailmentCount';
begin
  Result := 0;
  try
    Result := FCurtailments.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.DemandSupportDefinitionCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.DemandSupportDefinitionCount';
begin
  Result := 0;
  try
    Result := FDemandSupportDefinitions.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.DistributionCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.DistributionCount';
begin
  Result := 0;
  try
    Result := FDistributions.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.NonFirmSubsystemCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.NonFirmSubsystemCount';
begin
  Result := 0;
  try
    Result := FNonFirmSubsystems.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.SystemFullPerNodeCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.SystemFullPerNodeCount';
begin
  Result := 0;
  try
    Result := FSystemFullPerNode.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.FixedSubSystemCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.FixedSubSystemCount';
begin
  Result := 0;
  try
    Result := FFixedSubSystems.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.SequentialSubSystemCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.SequentialSubSystemCount';
begin
  Result := 0;
  try
    Result := FSequentialSubSystems.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinitionFileDataObject.SupportChannelCount: integer;
const OPNAME = 'TAllocationDefinitionFileDataObject.SupportChannelCount';
begin
  Result := 0;
  try
    Result := FSupportChannels.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandSupportDefinition }

procedure TDemandSupportDefinition.CreateMemberObjects;
const OPNAME = 'TDemandSupportDefinition.CreateMemberObjects';
var
  LIndex : integer;
  LInteger: TInteger;
begin
  inherited CreateMemberObjects;
  try
    // Line 7a
    FSubsystemNo1      := TInteger.Create;
    // Line 7b
    FGrowthFlag       := TDouble.Create;
    // Line 7c
    FTargetDemand     := TDouble.Create;
    // Line 7d
    FDemandDefName    := TString.Create;
    // Line 7e
    FDemandCentreID   := TInteger.Create;
    // Line 7f
    FUserCategory     := TInteger.Create;
    // Line 7g
    FSupportArc1      := TInteger.Create;
    // Line 7h
    FSupportArc2      := TInteger.Create;
    // Line 7i
    FSupportSubsystemsCount := TInteger.Create;
    // Line 7j
    FSupportSystemNr  := TInteger.Create;
    // Line 7 Other
    FComment          := TString.Create;
    // Line 7k
    FRoutingSupportChannelNumbers := TObjectList.Create(True);
    // Line 7l
    for LIndex := Low(FSupportSubsystemsNumbers) to High(FSupportSubsystemsNumbers) do
    begin
      LInteger := TInteger.Create;
      FSupportSubsystemsNumbers[LIndex] := LInteger;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandSupportDefinition.DestroyMemberObjects;
const OPNAME = 'TDemandSupportDefinition.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited DestroyMemberObjects;
  try
    // Line 7a
    FSubsystemNo1.Free;
    // Line 7b
    FGrowthFlag.Free;
    // Line 7c
    FTargetDemand.Free;
    // Line 7d
    FDemandDefName.Free;
    // Line 7e
    FDemandCentreID.Free;
    // Line 7f
    FUserCategory.Free;
    // Line 7g
    FSupportArc1.Free;
    // Line 7h
    FSupportArc2.Free;
    // Line 7i
    FSupportSubsystemsCount.Free;
    // Line 7j
    FSupportSystemNr.Free;
    // Line 7 Other
    FComment.Free;
    // Line 7k
    FRoutingSupportChannelNumbers.Clear;
    FRoutingSupportChannelNumbers.Free;
    // Line 7l
    for LIndex := Low(FSupportSubsystemsNumbers) to High(FSupportSubsystemsNumbers) do
    begin
      FSupportSubsystemsNumbers[LIndex].Free;
      FSupportSubsystemsNumbers[LIndex] := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandSupportDefinition.Initialise: boolean;
const OPNAME = 'TDemandSupportDefinition.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    // Line 7a
    FSubsystemNo1.FData := 0;
    FSubsystemNo1.FInitalised := False;
    FSubsystemNo1.FLength := 5;
    FSubsystemNo1.FDecimal := 0;
    FSubsystemNo1.FDefaultPadding := True;

    // Line 7b
    FGrowthFlag.FData := 0;
    FGrowthFlag.FInitalised := False;
    FGrowthFlag.FLength := 6;
    FGrowthFlag.FDecimal := 2;
    FGrowthFlag.FDefaultPadding := True;
    FGrowthFlag.ShowDecimalPoint := True;

    // Line 7c
    FTargetDemand.FData := 0;
    FTargetDemand.FInitalised := False;
    FTargetDemand.FLength := 8;
    FTargetDemand.FDecimal := 2;
    FTargetDemand.FDefaultPadding := True;
    FTargetDemand.ShowDecimalPoint := True;

    // Line 7d
    FDemandDefName.FData := '';
    FDemandDefName.FInitalised := False;
    FDemandDefName.FLength := 10;
    FDemandDefName.FDecimal := 0;
    FDemandDefName.FDefaultPadding := True;

    // Line 7e
    FDemandCentreID.FData := 0;
    FDemandCentreID.FInitalised := False;
    FDemandCentreID.FLength := 5;
    FDemandCentreID.FDecimal := 0;
    FDemandCentreID.FDefaultPadding := True;

    // Line 7f
    FUserCategory.FData := 0;
    FUserCategory.FInitalised := False;
    FUserCategory.FLength := 5;
    FUserCategory.FDecimal := 0;
    FUserCategory.FDefaultPadding := True;

    // Line 7g
    FSupportArc1.FData := 0;
    FSupportArc1.FInitalised := False;
    FSupportArc1.FLength := 5;
    FSupportArc1.FDecimal := 0;
    FSupportArc1.FDefaultPadding := True;

    // Line 7h
    FSupportArc2.FData := 0;
    FSupportArc2.FInitalised := False;
    FSupportArc2.FLength := 5;
    FSupportArc2.FDecimal := 0;
    FSupportArc2.FDefaultPadding := True;

    // Line 7i
    FSupportSubsystemsCount.FData := 0;
    FSupportSubsystemsCount.FInitalised := False;
    FSupportSubsystemsCount.FLength := 5;
    FSupportSubsystemsCount.FDecimal := 0;
    FSupportSubsystemsCount.FDefaultPadding := True;

    // Line 7j
    FSupportSystemNr.FData := 0;
    FSupportSystemNr.FInitalised := False;
    FSupportSystemNr.FLength := 3;
    FSupportSystemNr.FDecimal := 0;
    FSupportSystemNr.FDefaultPadding := True;

    // Line 7 Other
    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength := 0;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := False;

    // Line 7k
    FRoutingSupportChannelNumbers.Clear;

    // Line 7l
    for LIndex := Low(FSupportSubsystemsNumbers) to High(FSupportSubsystemsNumbers) do
    begin
      FSupportSubsystemsNumbers[LIndex].FData := 0;
      FSupportSubsystemsNumbers[LIndex].FInitalised := False;
      FSupportSubsystemsNumbers[LIndex].FLength := 3;
      FSupportSubsystemsNumbers[LIndex].FDecimal := 0;
      FSupportSubsystemsNumbers[LIndex].FDefaultPadding := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandSupportDefinition.Reset;
const OPNAME = 'TDemandSupportDefinition.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandSupportDefinition.AddRoutingSupportChannelNumbers: TRoutingSupportChannelNumbers;
const OPNAME = 'TDemandSupportDefinition.AddRoutingSupportChannelNumbers';
begin
  Result := nil;
  try
    Result := TRoutingSupportChannelNumbers.Create;
    FRoutingSupportChannelNumbers.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandSupportDefinition.GetRoutingSupportChannelNumbersByIndex(AIndex: integer): TRoutingSupportChannelNumbers;
const OPNAME = 'TDemandSupportDefinition.GetRoutingSupportChannelNumbersByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FRoutingSupportChannelNumbers.Count) then
      Result := TRoutingSupportChannelNumbers(FRoutingSupportChannelNumbers[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandSupportDefinition.RoutingSupportChannelNumbersCount: integer;
const OPNAME = 'TDemandSupportDefinition.RoutingSupportChannelNumbersCount';
begin
  Result := 0;
  try
    Result := FRoutingSupportChannelNumbers.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TNonFirmSubsystem }

procedure TNonFirmSubsystem.CreateMemberObjects;
const OPNAME = 'TNonFirmSubsystem.CreateMemberObjects';
var
  LIndex: integer;
  LInteger: TInteger;
begin
  inherited CreateMemberObjects;
  try
    FNonFirmSubsystemNo    := TInteger.Create;
    for LIndex := Low(FRoutingChannelNoArray) to High(FRoutingChannelNoArray) do
    begin
      LInteger := TInteger.Create;
      FRoutingChannelNoArray[LIndex] := LInteger;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNonFirmSubsystem.DestroyMemberObjects;
const OPNAME = 'TNonFirmSubsystem.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited DestroyMemberObjects;
  try
    FNonFirmSubsystemNo.Free;
    for LIndex := Low(FRoutingChannelNoArray) to High(FRoutingChannelNoArray) do
    begin
      FRoutingChannelNoArray[LIndex].Free;
      FRoutingChannelNoArray[LIndex] := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNonFirmSubsystem.Initialise: boolean;
const OPNAME = 'TNonFirmSubsystem.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FNonFirmSubsystemNo.FData := 0;
    FNonFirmSubsystemNo.FInitalised := False;
    FNonFirmSubsystemNo.FLength := 3;
    FNonFirmSubsystemNo.FDecimal := 0;
    FNonFirmSubsystemNo.FDefaultPadding := True;
    for LIndex := Low(FRoutingChannelNoArray) to High(FRoutingChannelNoArray) do
    begin
      FRoutingChannelNoArray[LIndex].FData := 0;
      FRoutingChannelNoArray[LIndex].FInitalised := False;
      FRoutingChannelNoArray[LIndex].FLength := 4;
      FRoutingChannelNoArray[LIndex].FDecimal := 0;
      FRoutingChannelNoArray[LIndex].FDefaultPadding := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNonFirmSubsystem.Reset;
const OPNAME = 'TNonFirmSubsystem.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSubsystem }

procedure TSubsystem.CreateMemberObjects;
const OPNAME = 'TSubsystem.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSubsystemName          := TString.Create;
    FSubsystemNo2           := TInteger.Create;
    FSupportSubsystemNo     := TInteger.Create;
    FChannelNo              := TInteger.Create;
    FShortTermFirmYield     := TDouble.Create;
    FStreamFlow             := TDouble.Create;
    FLongTermFirmYield      := TDouble.Create;
    FFamilyCurveStartYear   := TInteger.Create;
    FFamilyCurveStartMonth  := TInteger.Create;
    FFamilyCurveEndYear     := TInteger.Create;
    FFamilyCurveEndMonth    := TInteger.Create;
    FFirmFlag               := TString.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubsystem.DestroyMemberObjects;
const OPNAME = 'TSubsystem.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FSubsystemName.Free;
    FSubsystemNo2.Free;
    FSupportSubsystemNo.Free;
    FChannelNo.Free;
    FShortTermFirmYield.Free;
    FStreamFlow.Free;
    FLongTermFirmYield.Free;
    FFamilyCurveStartYear.Free;
    FFamilyCurveStartMonth.Free;
    FFamilyCurveEndYear.Free;
    FFamilyCurveEndMonth.Free;
    FFirmFlag.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSubsystem.Initialise: boolean;
const OPNAME = 'TSubsystem.Initialise';
begin
  Result := inherited Initialise;
  try
    FSubsystemName.FData := '';
    FSubsystemName.FInitalised := False;
    //FSubsystemName.FLength := 10;
    FSubsystemName.FDecimal := 0;
    FSubsystemName.FDefaultPadding := True;

    FSubsystemNo2.FData := 0;
    FSubsystemNo2.FInitalised := False;
    FSubsystemNo2.FLength := 4;
    FSubsystemNo2.FDecimal := 0;
    FSubsystemNo2.FDefaultPadding := True;


    FSupportSubsystemNo.FData := 0;
    FSupportSubsystemNo.FInitalised := False;
    FSupportSubsystemNo.FLength := 4;
    FSupportSubsystemNo.FDecimal := 0;
    FSupportSubsystemNo.FDefaultPadding := True;

    FChannelNo.FData := 0;
    FChannelNo.FInitalised := False;
    FChannelNo.FLength := 4;
    FChannelNo.FDecimal := 0;
    FChannelNo.FDefaultPadding := True;

    FShortTermFirmYield.FData := 0;
    FShortTermFirmYield.FInitalised := False;
    FShortTermFirmYield.FLength := 8;
    FShortTermFirmYield.FDecimal := 1;
    FShortTermFirmYield.FDefaultPadding := True;
    FShortTermFirmYield.ShowDecimalPoint := True;

    FStreamFlow.FData := 0;
    FStreamFlow.FInitalised := False;
    FStreamFlow.FLength := 8;
    FStreamFlow.FDecimal := 1;
    FStreamFlow.FDefaultPadding := True;
    FStreamFlow.ShowDecimalPoint := True;

    FLongTermFirmYield.FData := 0;
    FLongTermFirmYield.FInitalised := False;
    FLongTermFirmYield.FLength := 8;
    FLongTermFirmYield.FDecimal := 1;
    FLongTermFirmYield.FDefaultPadding := True;
    FLongTermFirmYield.ShowDecimalPoint := True;

    FFamilyCurveStartYear.FData := 0;
    FFamilyCurveStartYear.FInitalised := False;
    FFamilyCurveStartYear.FLength := 6;
    FFamilyCurveStartYear.FDecimal := 0;
    FFamilyCurveStartYear.FDefaultPadding := True;

    FFamilyCurveStartMonth.FData := 0;
    FFamilyCurveStartMonth.FInitalised := False;
    FFamilyCurveStartMonth.FLength := 4;
    FFamilyCurveStartMonth.FDecimal := 0;
    FFamilyCurveStartMonth.FDefaultPadding := True;

    FFamilyCurveEndYear.FData := 0;
    FFamilyCurveEndYear.FInitalised := False;
    FFamilyCurveEndYear.FLength := 6;
    FFamilyCurveEndYear.FDecimal := 0;
    FFamilyCurveEndYear.FDefaultPadding := True;

    FFamilyCurveEndMonth.FData := 0;
    FFamilyCurveEndMonth.FInitalised := False;
    FFamilyCurveEndMonth.FLength := 4;
    FFamilyCurveEndMonth.FDecimal := 0;
    FFamilyCurveEndMonth.FDefaultPadding := True;

    FFirmFlag.FData := '';
    FFirmFlag.FInitalised := False;
    FFirmFlag.FLength := 10;
    FFirmFlag.FDecimal := 0;
    FFirmFlag.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubsystem.Reset;
const OPNAME = 'TSubsystem.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubsystem.Assign(ASubsystem:TSubsystem);
const OPNAME = 'TSubsystem.Assign';
begin
  try
    if Assigned(ASubsystem) then
    begin
      FSubsystemName.FData               := ASubsystem.SubsystemName.FData;
      FSubsystemName.FInitalised         := ASubsystem.SubsystemName.FInitalised;
      FSubsystemName.FLength             := ASubsystem.SubsystemName.FLength;
      FSubsystemNo2.FData                := ASubsystem.SubsystemNo2.FData;
      FSubsystemNo2.FInitalised          := ASubsystem.SubsystemNo2.FInitalised;
      FSupportSubsystemNo.FData          := ASubsystem.SupportSubsystemNo.FData;
      FSupportSubsystemNo.FInitalised    := ASubsystem.SupportSubsystemNo.FInitalised;
      FChannelNo.FData                   := ASubsystem.ChannelNo.FData;
      FChannelNo.FInitalised             := ASubsystem.ChannelNo.FInitalised;
      FShortTermFirmYield.FData          := ASubsystem.ShortTermFirmYield.FData;
      FShortTermFirmYield.FInitalised    := ASubsystem.ShortTermFirmYield.FInitalised;
      FStreamFlow.FData                  := ASubsystem.StreamFlow.FData;
      FStreamFlow.FInitalised            := ASubsystem.StreamFlow.FInitalised;
      FLongTermFirmYield.FData           := ASubsystem.LongTermFirmYield.FData;
      FLongTermFirmYield.FInitalised     := ASubsystem.LongTermFirmYield.FInitalised;
      FFamilyCurveStartYear.FData        := ASubsystem.FamilyCurveStartYear.FData;
      FFamilyCurveStartYear.FInitalised  := ASubsystem.FamilyCurveStartYear.FInitalised;
      FFamilyCurveStartMonth.FData       := ASubsystem.FamilyCurveStartMonth.FData;
      FFamilyCurveStartMonth.FInitalised := ASubsystem.FamilyCurveStartMonth.FInitalised;
      FFamilyCurveEndYear.FData          := ASubsystem.FamilyCurveEndYear.FData;
      FFamilyCurveEndYear.FInitalised    := ASubsystem.FamilyCurveEndYear.FInitalised;
      FFamilyCurveEndMonth.FData         := ASubsystem.FamilyCurveEndMonth.FData;
      FFamilyCurveEndMonth.FInitalised   := ASubsystem.FamilyCurveEndMonth.FInitalised;
      FFirmFlag.FData                    := ASubsystem.FirmFlag.FData;
      FFirmFlag.FInitalised              := ASubsystem.FirmFlag.FInitalised;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStartVolume }

procedure TStartVolume.CreateMemberObjects;
const OPNAME = 'TStartVolume.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FStartVolume := TDouble.Create;
    FCurveSetNo  := TInteger.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStartVolume.DestroyMemberObjects;
const OPNAME = 'TStartVolume.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FStartVolume.Free;
    FCurveSetNo.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStartVolume.Initialise: boolean;
const OPNAME = 'TStartVolume.Initialise';
begin
  Result := inherited Initialise;
  try

    FStartVolume.FData := 0;
    FStartVolume.FInitalised := False;
    FStartVolume.FLength := 4;
    FStartVolume.FDecimal := 2;
    FStartVolume.FDefaultPadding := True;
    FStartVolume.ShowDecimalPoint := True;

    FCurveSetNo.FData := 0;
    FCurveSetNo.FInitalised := False;
    FCurveSetNo.FLength := 2;
    FCurveSetNo.FDecimal := 0;
    FCurveSetNo.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStartVolume.Reset;
const OPNAME = 'TStartVolume.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TScenarioCoefficients }

procedure TScenarioCoefficients.CreateMemberObjects;
const OPNAME = 'TScenarioCoefficients.CreateMemberObjects';
var
  LIndex: integer;
  LDouble: TDouble;
begin
  inherited CreateMemberObjects;
  try
    FLineTargetDraft := TDouble.Create;
    FRiskProp        := TDouble.Create;
    for LIndex := Low(FCoefArray) to High(FCoefArray) do
    begin
      LDouble := TDouble.Create;
      FCoefArray[LIndex] := LDouble;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TScenarioCoefficients.DestroyMemberObjects;
const OPNAME = 'TScenarioCoefficients.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited DestroyMemberObjects;
  try
    FLineTargetDraft.Free;
    FRiskProp.Free;
    for LIndex := Low(FCoefArray) to High(FCoefArray) do
    begin
      FCoefArray[LIndex].Free;
      FCoefArray[LIndex] := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioCoefficients.Initialise: boolean;
const OPNAME = 'TScenarioCoefficients.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FLineTargetDraft.FData := 0;
    FLineTargetDraft.FInitalised := False;
    FLineTargetDraft.FLength := 10;
    FLineTargetDraft.FDecimal := 6;
    FLineTargetDraft.FDefaultPadding := True;
    FLineTargetDraft.ShowDecimalPoint := True;

    FRiskProp.FData := 0;
    FRiskProp.FInitalised := False;
    FRiskProp.FLength := 10;
    FRiskProp.FDecimal := 6;
    FRiskProp.FDefaultPadding := True;
    FRiskProp.ShowDecimalPoint := True;
    for LIndex := Low(FCoefArray) to High(FCoefArray) do
    begin
      FCoefArray[LIndex].FData := 0;
      FCoefArray[LIndex].FInitalised := False;
      FCoefArray[LIndex].FLength := 10;
      FCoefArray[LIndex].FDecimal := 6;
      FCoefArray[LIndex].FDefaultPadding := True;
      FCoefArray[LIndex].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TScenarioCoefficients.Reset;
const OPNAME = 'TScenarioCoefficients.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStartVolumeAndScenarioCoefficients }

procedure TStartVolumeAndScenarioCoefficients.CreateMemberObjects;
const OPNAME = 'TStartVolumeAndScenarioCoefficients.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FStartVolume            := TStartVolume.Create;
    FScenarioCoefficients   := TObjectList.Create(True);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStartVolumeAndScenarioCoefficients.DestroyMemberObjects;
const OPNAME = 'TStartVolumeAndScenarioCoefficients.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FStartVolume.Free;
    FScenarioCoefficients.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStartVolumeAndScenarioCoefficients.AddScenarioCoefficients: TScenarioCoefficients;
const OPNAME = 'TStartVolumeAndScenarioCoefficients.AddScenarioCoefficients';
begin
  Result := nil;
  try
    Result  := TScenarioCoefficients.Create;
    FScenarioCoefficients.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStartVolumeAndScenarioCoefficients.GetScenarioCoefficientsByIndex(AIndex: integer): TScenarioCoefficients;
const OPNAME = 'TStartVolumeAndScenarioCoefficients.GetScenarioCoefficientsByIndex';
begin
  Result  := nil;
  try
    if(AIndex >= 0) and (AIndex < FScenarioCoefficients.Count) then
      Result := TScenarioCoefficients(FScenarioCoefficients[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStartVolumeAndScenarioCoefficients.Initialise: boolean;
const OPNAME = 'TStartVolumeAndScenarioCoefficients.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := FStartVolume.Initialise;
    FScenarioCoefficients.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStartVolumeAndScenarioCoefficients.Reset;
const OPNAME = 'TStartVolumeAndScenarioCoefficients.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStartVolumeAndScenarioCoefficients.ScenarioCoefficientCount: integer;
const OPNAME = 'TStartVolumeAndScenarioCoefficients.ScenarioCoefficientCount';
begin
  Result := 0;
  try
    Result := FScenarioCoefficients.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TCoefficients }

procedure TCoefficients.CreateMemberObjects;
const OPNAME = 'TCoefficients.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSubsystem            := TSubsystem.Create;
    FStartVolumeAndScenarioCoefficients   := TObjectList.Create(True);

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCoefficients.DestroyMemberObjects;
const OPNAME = 'TCoefficients.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FSubsystem.Free;
    FStartVolumeAndScenarioCoefficients.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficients.Initialise: boolean;
const OPNAME = 'TCoefficients.Initialise';
begin
  Result := inherited Initialise;
  try
    FStartVolumeAndScenarioCoefficients.Clear;
    Result := FSubsystem.Initialise;
    FCurveSetIndex := 0;
    FSubsystemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCoefficients.Reset;
const OPNAME = 'TCoefficients.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficients.AddStartVolumeAndScenarioCoefficients: TStartVolumeAndScenarioCoefficients;
const OPNAME = 'TCoefficients.AddStartVolumeAndScenarioCoefficients';
begin
  Result  := nil;
  try
    Result  := TStartVolumeAndScenarioCoefficients.Create;
    FStartVolumeAndScenarioCoefficients.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficients.GetStartVolumeAndScenarioCoefficientsByIndex(AIndex: integer): TStartVolumeAndScenarioCoefficients;
const OPNAME = 'TCoefficients.GetStartVolumeAndScenarioCoefficientsByIndex';
begin
  Result  := nil;
  try
    if(AIndex >= 0) and (AIndex < FStartVolumeAndScenarioCoefficients.Count) then
      Result := TStartVolumeAndScenarioCoefficients(FStartVolumeAndScenarioCoefficients[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficients.StartVolumeAndScenarioCoefficientCount: integer;
const OPNAME = 'TCoefficients.StartVolumeAndScenarioCoefficientCount';
begin
  Result := 0;
  try
    Result := FStartVolumeAndScenarioCoefficients.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSystemFullPerNodes }

procedure TSystemFullPerNodes.CreateMemberObjects;
const OPNAME = 'TSystemFullPerNodes.CreateMemberObjects';
var
  LIndex: integer;
  LInteger: TInteger;
begin
  inherited CreateMemberObjects;
  try
    FSubsystemNo3    := TInteger.Create;
    for LIndex := Low(FNodeNoArray) to High(FNodeNoArray) do
    begin
      LInteger := TInteger.Create;
      FNodeNoArray[LIndex] := LInteger;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemFullPerNodes.DestroyMemberObjects;
const OPNAME = 'TSystemFullPerNodes.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited DestroyMemberObjects;
  try
    FSubsystemNo3.Free;
    for LIndex := Low(FNodeNoArray) to High(FNodeNoArray) do
    begin
      FNodeNoArray[LIndex].Free;
      FNodeNoArray[LIndex] := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemFullPerNodes.Initialise: boolean;
const OPNAME = 'TSystemFullPerNodes.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FSubsystemNo3.FData := 0;
    FSubsystemNo3.FInitalised := False;
    FSubsystemNo3.FLength := 3;
    FSubsystemNo3.FDecimal := 0;
    FSubsystemNo3.FDefaultPadding := True;
    for LIndex := Low(FNodeNoArray) to High(FNodeNoArray) do
    begin
      FNodeNoArray[LIndex].FData := 0;
      FNodeNoArray[LIndex].FInitalised := False;
      FNodeNoArray[LIndex].FLength := 4;
      FNodeNoArray[LIndex].FDecimal := 0;
      FNodeNoArray[LIndex].FDefaultPadding := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemFullPerNodes.Reset;
const OPNAME = 'TSystemFullPerNodes.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TClassValuesArrayObject }

procedure TClassValuesArrayObject.CreateMemberObjects;
const OPNAME = 'TClassValuesArrayObject.CreateMemberObjects';
var
  LIndex: integer;
  LDouble: TDouble;
begin
  inherited CreateMemberObjects;
  try
    for LIndex := Low(FClassValuesArray) to High(FClassValuesArray) do
    begin
      LDouble := TDouble.Create;
      FClassValuesArray[LIndex] := LDouble;
    end;
    FComment := TString.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TClassValuesArrayObject.DestroyMemberObjects;
const OPNAME = 'TClassValuesArrayObject.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited DestroyMemberObjects;
  try
    for LIndex := Low(FClassValuesArray) to High(FClassValuesArray) do
    begin
      FClassValuesArray[LIndex].Free;
      FClassValuesArray[LIndex] := nil;
    end;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TClassValuesArrayObject.Initialise: boolean;
const OPNAME = 'TClassValuesArrayObject.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    for LIndex := Low(FClassValuesArray) to High(FClassValuesArray) do
    begin
      FClassValuesArray[LIndex].FData := 0;
      FClassValuesArray[LIndex].FInitalised := False;
      FClassValuesArray[LIndex].FLength := 7;
      FClassValuesArray[LIndex].FDecimal := 2;
      FClassValuesArray[LIndex].FDefaultPadding := True;
    end;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength := 0;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TClassValuesArrayObject.Reset;
const OPNAME = 'TClassValuesArrayObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFixedSubSystem }

procedure TFixedSubSystem.CreateMemberObjects;
const OPNAME = 'TFixedSubSystem.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSubsystemNo4 := TInteger.Create;
    FSubsystemPos := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFixedSubSystem.DestroyMemberObjects;
const OPNAME = 'TFixedSubSystem.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FSubsystemNo4.Free;
    FSubsystemPos.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedSubSystem.Initialise: boolean;
const OPNAME = 'TFixedSubSystem.Initialise';
begin
  Result := False;
  try
    FSubsystemNo4.FData           := 0;
    FSubsystemNo4.FInitalised     := False;
    FSubsystemNo4.FLength         := 4;
    FSubsystemNo4.FDecimal        := 0;
    FSubsystemNo4.FDefaultPadding := True;

    FSubsystemPos.FData           := 0;
    FSubsystemPos.FInitalised     := False;
    FSubsystemPos.FLength         := 4;
    FSubsystemPos.FDecimal        := 0;
    FSubsystemPos.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFixedSubSystem.Reset;
const OPNAME = 'TFixedSubSystem.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSupportChannel }

procedure TSupportChannel.CreateMemberObjects;
const OPNAME = 'TSupportChannel.CreateMemberObjects';
var
  LIndex: integer;
  LInteger: TInteger;
  LDouble: TDouble;
begin
  inherited CreateMemberObjects;
  try
    FSupportChannelNo := TInteger.Create;
    FSubsystemNo5     := TInteger.Create;
    for LIndex := Low(FInfluencedSubSytem) to High(FInfluencedSubSytem) do
    begin
      LInteger := TInteger.Create;
      FInfluencedSubSytem[LIndex] := LInteger;
    end;
    for LIndex := Low(FInfluenceFactor) to High(FInfluenceFactor) do
    begin
      LDouble := TDouble.Create;
      FInfluenceFactor[LIndex] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupportChannel.DestroyMemberObjects;
const OPNAME = 'TSupportChannel.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited DestroyMemberObjects;
  try
    FSupportChannelNo.Free;
    FSubsystemNo5.Free;
    for LIndex := Low(FInfluencedSubSytem) to High(FInfluencedSubSytem) do
    begin
      FInfluencedSubSytem[LIndex].Free;
      FInfluencedSubSytem[LIndex] := nil;
    end;
    for LIndex := Low(FInfluenceFactor) to High(FInfluenceFactor) do
    begin
      FInfluenceFactor[LIndex].Free;
      FInfluenceFactor[LIndex] := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupportChannel.Initialise: boolean;
const OPNAME = 'TSupportChannel.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FSupportChannelNo.FData := 0;
    FSupportChannelNo.FInitalised := False;
    FSupportChannelNo.FLength := 3;
    FSupportChannelNo.FDecimal := 0;
    FSupportChannelNo.FDefaultPadding := True;

    FSubsystemNo5.FData := 0;
    FSubsystemNo5.FInitalised := False;
    FSubsystemNo5.FLength := 3;
    FSubsystemNo5.FDecimal := 0;
    FSubsystemNo5.FDefaultPadding := True;

    for LIndex := Low(FInfluencedSubSytem) to High(FInfluencedSubSytem) do
    begin
      FInfluencedSubSytem[LIndex].FData := 0;
      FInfluencedSubSytem[LIndex].FInitalised := False;
      FInfluencedSubSytem[LIndex].FLength := 3;
      FInfluencedSubSytem[LIndex].FDecimal := 0;
      FInfluencedSubSytem[LIndex].FDefaultPadding := True;
    end;

    for LIndex := Low(FInfluencedSubSytem) to High(FInfluencedSubSytem) do
    begin
      FInfluenceFactor[LIndex].FData := 0;
      FInfluenceFactor[LIndex].FInitalised := False;
      FInfluenceFactor[LIndex].FLength := 6;
      FInfluenceFactor[LIndex].FDecimal := 1;
      FInfluenceFactor[LIndex].FDefaultPadding := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupportChannel.Reset;
const OPNAME = 'TSupportChannel.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRoutingSupportChannelNumbers }

procedure TRoutingSupportChannelNumbers.CreateMemberObjects;
const OPNAME = 'TRoutingSupportChannelNumbers.CreateMemberObjects';
var
  LIndex: integer;
  LInteger: TInteger;
begin
  inherited CreateMemberObjects;
  try
    for LIndex := Low(FChannelNumbers) to High(FChannelNumbers) do
    begin
      LInteger := TInteger.Create;
      FChannelNumbers[LIndex] := LInteger;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRoutingSupportChannelNumbers.DestroyMemberObjects;
const OPNAME = 'TRoutingSupportChannelNumbers.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited DestroyMemberObjects;
  try
    for LIndex := Low(FChannelNumbers) to High(FChannelNumbers) do
    begin
      FChannelNumbers[LIndex].Free;
      FChannelNumbers[LIndex] := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRoutingSupportChannelNumbers.Initialise: boolean;
const OPNAME = 'TRoutingSupportChannelNumbers.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    for LIndex := Low(FChannelNumbers) to High(FChannelNumbers) do
    begin
      FChannelNumbers[LIndex].FData := 0;
      FChannelNumbers[LIndex].FInitalised := False;
      FChannelNumbers[LIndex].FLength := 4;
      FChannelNumbers[LIndex].FDecimal := 0;
      FChannelNumbers[LIndex].FDefaultPadding := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRoutingSupportChannelNumbers.Reset;
const OPNAME = 'TRoutingSupportChannelNumbers.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSubsystemList }

function TSubsystemList.AddSubsystem:TSubsystem;
const OPNAME = 'TSubsystemList.AddSubsystem';
begin
  Result := nil;
  try
    Result := TSubsystem.Create;
    Self.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSubsystemList.SubsystemByIndex(AIndex: integer): TSubsystem;
const OPNAME = 'TSubsystemList.SubsystemByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < Self.Count) then
      Result := TSubsystem(Self.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


