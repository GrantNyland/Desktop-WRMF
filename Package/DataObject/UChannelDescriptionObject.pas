
//
//
//  UNIT      : Contains TChannelDescrObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UChannelDescriptionObject;

interface

uses
  Classes, sysutils,contnrs, VCL.dialogs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TPenaltyChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FElementID      : TInteger;
    FIdentifier     : TInteger;
    FPenaltyName    : TString;
    FPenaltyType    : TInteger;
    FArcCount       : TInteger;
    FArcPenalty: array[MinArcs..MaxArcs] of TDouble;
    FComment        : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMasterChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FUpNodeNumber      : TInteger;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    FChannelType       : TChar;
    FComment           : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TPowerChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier                  : TInteger;
    FChannelNumber               : TInteger;
    FChannelName                 : TString;
    FUpNodeNumber                : TInteger;
    FDownNodeNumber              : TInteger;
    FPenaltyStructType           : TInteger;
    FSpillChannelNumber          : TInteger;
    FSpillChannelName            : TString;
    FSpillUpNodeNumber           : TInteger;
    FSpillDownNodeNumber         : TInteger;
    FSpillPenaltyStructType      : TInteger;
    FDownStreamPowerChannelCount : TInteger;
    FDownStreamPowerChannels: array[MinDownStreamPowerChannels..MaxDownStreamPowerChannels] of TInteger;
    FComment                     : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TIrrigationChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier                   : TInteger;
    FIrrigationNodeNumber         : TInteger;
    FUpstreamNodeNumber           : TInteger;
    FDiversionChannelNumber       : TInteger;
    FDiversionChannelName         : TString;
    FIrrigationPenaltyStructType  : TInteger;
    FDownStreamNodeNumber         : TInteger;
    FReturnChannelNumber          : TInteger;
    FReturnChannelName            : TString;
    FReturnPenaltyStructType      : TInteger;
    FConsumptiveChannelNumber     : TInteger;
    FConsumptiveChannelName       : TString;
    FConsumptivePenaltyStructType : TInteger;
    FRelaxationDemand             : TInteger;
    FComment                      : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDiversionChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier         : TInteger;
    FChannelNumber      : TInteger;
    FChannelName        : TString;
    FUpNodeNumber       : TInteger;
    FDownNodeNumber     : TInteger;
    FPenaltyStructType  : TInteger;
    FChannelType        : TInteger;
    FComment            : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMinFlowChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FUpNodeNumber      : TInteger;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    FComment           : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TLossChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FUpNodeNumber      : TInteger;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    FChannelType       : TInteger;
    FReference         : TInteger;
    FComment           : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMultiPurposeChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FChannelType       : integer;
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FUpNodeNumber      : TInteger;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    FComment           : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TPumpingChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FUpNodeNumber      : TInteger;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    FPumpingHead       : TDouble;
    FEfficiency        : TDouble;
    FComment: TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TReturnFlowChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier           : TInteger;
    FChannelNr            : TInteger;
    FChannelName          : TString;
    FDemandCentreNodeNr   : TInteger;
    FDownStreamNodeNr     : TInteger;
    FUpstreamNodeNr       : TInteger;
    FPenaltyStructureType : TInteger;
    function Initialise : Boolean; override;
    procedure Reset; override;
  end;

  TReclaimationChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier           : TInteger;
    FChannelNr            : TInteger;
    FChannelName          : TString;
    FDemandCentreNodeNr   : TInteger;
    FDownStreamNodeNr     : TInteger;
    FUpstreamNodeNr       : TInteger;
    FPenaltyStructureType : TInteger;
    function Initialise : Boolean; override;
    procedure Reset; override;
  end;

  TInflowChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FUpNodeNumber      : TInteger;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    FComment           : TString;
    FInflowFileName    : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDemandChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier       : TInteger;
    FChannelNumber    : TInteger;
    FChannelName      : TString;
    FUpNodeNumber     : TInteger;
    FDownNodeNumber   : TInteger;
    FPenaltyStructType: TInteger;
    FGaugeNumber      : TInteger;
    FStochastic       : TChar;
    FFullname         : TString;
    FComment          : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TGeneralChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FUpNodeNumber      : TInteger;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    FComment           : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TNaturalInflowChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier        : TInteger;
    FChannelNumber     : TInteger;
    FChannelName       : TString;
    FDownNodeNumber    : TInteger;
    FPenaltyStructType : TInteger;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TSummaryChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier         : TInteger;
    FChannelNumber      : TInteger;
    FCalculateFirmYield : TString;
    FFlowOutput         : TString;
    FComment            : TString;
    FChannelName        : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TIrrigationBlockChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier             : TInteger;
    FUpNodeNumber           : TInteger;
    FDownNodeNumber         : TInteger;
    FAbstractChannelNr      : TInteger;
    FAbstractChannelName    : TString;
    FAbstractPenaltyType    : TInteger;
    FReturnFlowChannelNr    : TInteger;
    FReturnFlowChannelName  : TString;
    FReturnFlowPenaltyType  : TInteger;
    FBlockNumber            : TInteger;
    FChannelType            : TInteger;
    FComment                : TString;
    function Initialise     : Boolean; override;
    procedure Reset; override;
  end;

  TWetlandChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier         : TInteger;
    FNodeNumber         : TInteger;
    FUpNodeNumber       : TInteger;
    FDownNodeNumber     : TInteger;
    FInflowChannelNr    : TInteger;
    FInflowChannelName  : TString;
    FInflowPenaltyType  : TInteger;
    FOutflowChannelNr   : TInteger;
    FOutflowChannelName : TString;
    FOutflowPenaltyType : TInteger;
    FChannelType        : TInteger;
    function Initialise : Boolean; override;
    procedure Reset; override;
  end;

  TDemandCentreObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier                : TInteger;
    FNodeNumber                : TInteger;
    FConsumptiveChannelNr      : TInteger;
    FConsumptiveChannelName    : TString;
    FNoOfReturnFlowChannels    : TInteger;
    FNoOfReclaimationChannels  : TInteger;
    function Initialise : Boolean; override;
    procedure Reset; override;
  end;

  TGroundWaterObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier                               : TInteger;
    FReferenceNodeNr                          : TInteger;

    // Line 16a.
    FAquiferNodeNumber                        : TInteger;
    FAbstractionNodeNumber                    : TInteger;
    FCollectionNodeNumber                     : TInteger;
    FBaseflowNodeNumber                       : TInteger;

    FUpNodeNumber                             : TInteger;
    FDownNodeNumber                           : TInteger;

    // Line 16b.
    FAquiferInflowChannelNr                   : TInteger;
    FAquiferInflowChannelName                 : TString;
    FAquiferInflowPenaltyType                 : TInteger;

    // Line 16c.
    FAquiferExcessInterflowChannelNr          : TInteger;
    FAquiferExcessInterflowChannelName        : TString;
    FAquiferExcessInterflowPenaltyType        : TInteger;

    // Line 16d.
    FGroundWaterBaseflowChannelNr             : TInteger;
    FGroundWaterBaseflowChannelName           : TString;
    FGroundWaterBaseflowPenaltyType           : TInteger;

    // Line 16e.
    FAbstractionFromAquiferChannelNr          : TInteger;
    FAbstractionFromAquiferChannelName        : TString;
    FAbstractionFromAquiferPenaltyType        : TInteger;

    // Line 16f.
    FAbstractionFromBaseflowChannelNr         : TInteger;
    FAbstractionFromBaseflowChannelName       : TString;
    FAbstractionFromBaseflowPenaltyType       : TInteger;

    // Line 16g.
    FGroundWaterBaseFlowRemainderChannelNr    : TInteger;
    FGroundWaterBaseFlowRemainderChannelName  : TString;
    FGroundWaterBaseFlowRemainderPenaltyType  : TInteger;

    // Line 16h.
    FSurfaceRunoffAndSoilInterflowChannelNr   : TInteger;
    FSurfaceRunoffAndSoilInterflowChannelName : TString;
    FSurfaceRunoffAndSoilInterflowPenaltyType : TInteger;

    // Line 16i.
    FInflowFromUpStreamAquiferChannelNr       : TInteger;
    FInflowFromUpStreamAquiferChannelName     : TString;
    FInflowFromUpStreamAquiferPenaltyType     : TInteger;
    FInflowFromUpStreamAquiferAquiferNumber   : TInteger;


    function Initialise : Boolean; override;
    procedure Reset; override;
  end;


  TChannelDescrObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F03.dat
    FPenaltyChannelCount      : TInteger;
    FMasterChannelCount       : TInteger;
    FPowerChannelCount        : TInteger;
    FIrrigationChannelCount   : TInteger;
    FDiversionChannelCount    : TInteger;
    FMinFlowChannelCount      : TInteger;
    FLossChannelCount         : TInteger;
    FMultiPurposeChannelCount : TInteger;
    FPumpingChannelCount      : TInteger;
    FReturnFlowChannelCount   : TInteger;
    FInflowChannelCount       : TInteger;
    FDemandChannelCount       : TInteger;
    FGeneralChannelCount      : TInteger;
    FNaturalInflowChannelCount: TInteger;
    FSummaryChannelCount      : TInteger;
    FFirmYieldAnalysesCount   : TInteger;
    FIrrigationBlockCount     : TInteger;
    FWetlandCount             : TInteger;
    FDemandCentreCount        : TInteger;
    FReclaimationChannelCount : TInteger;
    FGroundWaterCount         : TInteger;

    FInflowPenaltyNo          : TInteger;

    FComment01: TString;
    FComment02: TString;
    FComment03: TString;
    FComment04: TString;
    FComment05: TString;
    FComment06: TString;
    FComment07: TString;
    FComment08: TString;
    FComment09: TString;
    FComment10: TString;
    FComment11: TString;
    FComment12: TString;
    FComment13: TString;
    FComment14: TString;
    FComment15: TString;
    FComment16: TString;
    FComment17: TString;
    FComment22: TString;

    FPenaltyChannelList      : TObjectList;
    FMasterChannelList       : TObjectList;
    FPowerChannelList        : TObjectList;
    FIrrigationChannelList   : TObjectList;
    FDiversionChannelList    : TObjectList;
    FMinFlowChannelList      : TObjectList;
    FLossChannelList         : TObjectList;
    FMultiPurposeChannelList : TObjectList;
    FPumpingChannelList      : TObjectList;
    FReturnFLowChannelList   : TObjectList;
    FInflowChannelList       : TObjectList;
    FDemandChannelList       : TObjectList;
    FGeneralChannelList      : TObjectList;
    FNaturalInflowChannelList: TObjectList;
    FSummaryChannelList      : TObjectList;
    FIrrigationBlockList     : TObjectList;
    FWetlandList             : TObjectList;
    FDemandCentreList        : TObjectList;
    FReclaimationChannelList : TObjectList;
    FGroundWaterList         : TObjectList;
    //Line ... : Extra useless lines
    FF03ExtraLines: TStringList;

    function AddPenaltyChannels: boolean;
    function AddMasterChannels: boolean;
    function AddPowerChannels: boolean;
    function AddIrrigationChannels: boolean;
    function AddDiversionChannels: boolean;
    function AddMinFlowChannels: boolean;
    function AddLossChannels: boolean;
    function AddMultiPurposeChannels: boolean;
    function AddPumpingChannels: boolean;
    function AddReturnFlowChannels: boolean;
    function AddInflowChannels: boolean;
    function AddDemandChannels: boolean;
    function AddGeneralChannels: boolean;
    function AddNaturalInflowChannels: TNaturalInflowChannelObject;
    function AddSummaryChannels: boolean;
    function AddIrrigationBlockChannels: Boolean;
    function AddWetlandChannels: Boolean;
    function AddDemandCentreChannels: Boolean;
    function AddReclaimationChannels: Boolean;
    function AddGroundWaterChannels: Boolean;

    procedure Reset;override;
    function FindPenaltyStructure(Apenalty: Integer):TPenaltyChannelObject;

    function FindMasterChannel(AChannelNumber: Integer):TMasterChannelObject;
    function FindPowerChannels(AChannelNumber: Integer):TPowerChannelObject;
    function FindIrrigationChannel(AChannelNumber: Integer):TIrrigationChannelObject;
    function FindDiversionChannel(AChannelNumber: Integer):TDiversionChannelObject;
    function FindMinFlowChannel(AChannelNumber: Integer):TMinFlowChannelObject;
    function FindLossChannels(AChannelNumber: Integer):TLossChannelObject;
    function FindMultiPurposeChannel(AChannelNumber: Integer):TMultiPurposeChannelObject;
    function FindPumpingChannel(AChannelNumber: Integer):TPumpingChannelObject;
    function FindReturnFlowChannel(AChannelNumber: Integer):TReturnFlowChannelObject;
    function FindInflowChannel(AChannelNumber: Integer):TInflowChannelObject;
    function FindDemandChannel(AChannelNumber: Integer):TDemandChannelObject;
    function FindGeneralChannel(AChannelNumber: Integer):TGeneralChannelObject;
    function FindNaturalInflowChannel(AChannelNumber: Integer):TNaturalInflowChannelObject;
    function FindIrrigationBlockAbstractChannels(AChannelNumber: Integer): TIrrigationBlockChannelObject;
    function FindIrrigationBlockReturnChannels(AChannelNumber: Integer): TIrrigationBlockChannelObject;
    function FindIrrigationBlockByNodeNumber(ANodeNumber: Integer): TIrrigationBlockChannelObject;

    function FindWetlandInflowChannels(AChannelNumber: Integer): TWetlandChannelObject;
    function FindWetlandOutflowChannels(AChannelNumber: Integer): TWetlandChannelObject;
    function FindWetlandByNodeNumber(ANodeNumber: Integer): TWetlandChannelObject;
    function FindDemandCentreConsumptiveChannel(AChannelNumber: Integer): TDemandCentreObject;
    function FindDemandCentreReclaimationChannel(AChannelNumber: Integer): TReclaimationChannelObject;

    function Initialise: boolean; override;
  end;

implementation


{ TChannelDescrObject }
uses
  VoaimsCom_TLB,
  UErrorHandlingOperations;

procedure TChannelDescrObject.CreateMemberObjects;
const OPNAME = 'TChannelDescrObject.CreateMemberObjects';
begin
  try
    //File F03.dat
    FPenaltyChannelCount      := TInteger.Create;
    FMasterChannelCount       := TInteger.Create;
    FPowerChannelCount        := TInteger.Create;
    FIrrigationChannelCount   := TInteger.Create;
    FDiversionChannelCount    := TInteger.Create;
    FMinFlowChannelCount      := TInteger.Create;
    FLossChannelCount         := TInteger.Create;
    FMultiPurposeChannelCount := TInteger.Create;
    FPumpingChannelCount      := TInteger.Create;
    FReturnFlowChannelCount   := TInteger.Create;
    FInflowChannelCount       := TInteger.Create;
    FDemandChannelCount       := TInteger.Create;
    FGeneralChannelCount      := TInteger.Create;
    FNaturalInflowChannelCount:= TInteger.Create;
    FSummaryChannelCount      := TInteger.Create;
    FFirmYieldAnalysesCount   := TInteger.Create;
    FIrrigationBlockCount     := TInteger.Create;
    FWetlandCount             := TInteger.Create;
    FDemandCentreCount        := TInteger.Create;
    FReclaimationChannelCount := TInteger.Create;
    FGroundWaterCount         := TInteger.Create;
    FInflowPenaltyNo          := TInteger.Create;

    FComment01                := TString.Create;
    FComment02                := TString.Create;
    FComment03                := TString.Create;
    FComment04                := TString.Create;
    FComment05                := TString.Create;
    FComment06                := TString.Create;
    FComment07                := TString.Create;
    FComment08                := TString.Create;
    FComment09                := TString.Create;
    FComment10                := TString.Create;
    FComment11                := TString.Create;
    FComment12                := TString.Create;
    FComment13                := TString.Create;
    FComment14                := TString.Create;
    FComment15                := TString.Create;
    FComment16                := TString.Create;
    FComment17                := TString.Create;
    FComment22                := TString.Create;

    FF03ExtraLines            := TStringList.Create;

    FPenaltyChannelList       := TObjectList.Create(True);
    FMasterChannelList        := TObjectList.Create(True);
    FPowerChannelList         := TObjectList.Create(True);
    FIrrigationChannelList    := TObjectList.Create(True);
    FDiversionChannelList     := TObjectList.Create(True);
    FMinFlowChannelList       := TObjectList.Create(True);
    FLossChannelList          := TObjectList.Create(True);
    FMultiPurposeChannelList  := TObjectList.Create(True);
    FPumpingChannelList       := TObjectList.Create(True);
    FReturnFLowChannelList    := TObjectList.Create(True);
    FInflowChannelList        := TObjectList.Create(True);
    FDemandChannelList        := TObjectList.Create(True);
    FGeneralChannelList       := TObjectList.Create(True);
    FNaturalInflowChannelList := TObjectList.Create(True);
    FSummaryChannelList       := TObjectList.Create(True);
    FIrrigationBlockList      := TObjectList.Create(True);
    FWetlandList              := TObjectList.Create(True);
    FDemandCentreList         := TObjectList.Create(True);
    FReclaimationChannelList  := TObjectList.Create(True);
    FGroundWaterList          := TObjectList.Create(True);

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TChannelDescrObject.DestroyMemberObjects;
const OPNAME = 'TChannelDescrObject.DestroyMemberObjects';
begin
  try
    //File F03.dat
    FPenaltyChannelCount.Free;
    FMasterChannelCount.Free;
    FPowerChannelCount.Free;
    FIrrigationChannelCount.Free;
    FDiversionChannelCount.Free;
    FMinFlowChannelCount.Free;
    FLossChannelCount.Free;
    FMultiPurposeChannelCount.Free;
    FPumpingChannelCount.Free;
    FReturnFlowChannelCount.Free;
    FInflowChannelCount.Free;
    FDemandChannelCount.Free;
    FGeneralChannelCount.Free;
    FNaturalInflowChannelCount.Free;
    FSummaryChannelCount.Free;
    FFirmYieldAnalysesCount.Free;
    FIrrigationBlockCount.Free;
    FWetlandCount.Free;
    FDemandCentreCount.Free;
    FReclaimationChannelCount.Free;
    FGroundWaterCount.Free;
    FInflowPenaltyNo.Free;

    FComment01.Free;
    FComment02.Free;
    FComment03.Free;
    FComment04.Free;
    FComment05.Free;
    FComment06.Free;
    FComment07.Free;
    FComment08.Free;
    FComment09.Free;
    FComment10.Free;
    FComment11.Free;
    FComment12.Free;
    FComment13.Free;
    FComment14.Free;
    FComment15.Free;
    FComment16.Free;
    FComment17.Free;
    FComment22.Free;

    FPenaltyChannelList.Free;
    FMasterChannelList.Free;
    FPowerChannelList.Free;
    FIrrigationChannelList.Free;
    FDiversionChannelList.Free;
    FMinFlowChannelList.Free;
    FLossChannelList.Free;
    FMultiPurposeChannelList.Free;
    FPumpingChannelList.Free;
    FReturnFLowChannelList.Free;
    FInflowChannelList.Free;
    FDemandChannelList.Free;
    FGeneralChannelList.Free;
    FNaturalInflowChannelList.Free;
    FSummaryChannelList.Free;
    FIrrigationBlockList.Free;
    FWetlandList.Free;
    FDemandCentreList.Free;
    FReclaimationChannelList.Free;
    FGroundWaterList.Free;
    FF03ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.Initialise: boolean;
const OPNAME = 'TChannelDescrObject.Initialise';
Begin
  Result := False;
  try

    FPenaltyChannelCount.FInitalised := False;
    FPenaltyChannelCount.FData := 0;
    FPenaltyChannelCount.FLength := 5;
    FPenaltyChannelCount.FDecimal := 0;
    FPenaltyChannelCount.FDefaultPadding := True;

    FMasterChannelCount.FInitalised := False;
    FMasterChannelCount.FData := 0;
    FMasterChannelCount.FLength := 5;
    FMasterChannelCount.FDecimal := 0;
    FMasterChannelCount.FDefaultPadding := True;

    FPowerChannelCount.FInitalised := False;
    FPowerChannelCount.FData := 0;
    FPowerChannelCount.FLength := 5;
    FPowerChannelCount.FDecimal := 0;
    FPowerChannelCount.FDefaultPadding := True;

    FIrrigationChannelCount.FInitalised := False;
    FIrrigationChannelCount.FData := 0;
    FIrrigationChannelCount.FLength := 5;
    FIrrigationChannelCount.FDecimal := 0;
    FIrrigationChannelCount.FDefaultPadding := True;

    FDiversionChannelCount.FInitalised := False;
    FDiversionChannelCount.FData := 0;
    FDiversionChannelCount.FLength := 5;
    FDiversionChannelCount.FDecimal := 0;
    FDiversionChannelCount.FDefaultPadding := True;

    FMinFlowChannelCount.FInitalised := False;
    FMinFlowChannelCount.FData := 0;
    FMinFlowChannelCount.FLength := 5;
    FMinFlowChannelCount.FDecimal := 0;
    FMinFlowChannelCount.FDefaultPadding := True;

    FLossChannelCount.FInitalised := False;
    FLossChannelCount.FData := 0;
    FLossChannelCount.FLength := 5;
    FLossChannelCount.FDecimal := 0;
    FLossChannelCount.FDefaultPadding := True;

    FMultiPurposeChannelCount.FInitalised := False;
    FMultiPurposeChannelCount.FData := 0;
    FMultiPurposeChannelCount.FLength := 5;
    FMultiPurposeChannelCount.FDecimal := 0;
    FMultiPurposeChannelCount.FDefaultPadding := True;

    FPumpingChannelCount.FInitalised := False;
    FPumpingChannelCount.FData := 0;
    FPumpingChannelCount.FLength := 5;
    FPumpingChannelCount.FDecimal := 0;
    FPumpingChannelCount.FDefaultPadding := True;

    FReturnFlowChannelCount.FInitalised := False;
    FReturnFlowChannelCount.FData := 0;
    FReturnFlowChannelCount.FLength := 5;
    FReturnFlowChannelCount.FDecimal := 0;
    FReturnFlowChannelCount.FDefaultPadding := True;

    FInflowChannelCount.FInitalised := False;
    FInflowChannelCount.FData := 0;
    FInflowChannelCount.FLength := 5;
    FInflowChannelCount.FDecimal := 0;
    FInflowChannelCount.FDefaultPadding := True;

    FDemandChannelCount.FInitalised := False;
    FDemandChannelCount.FData := 0;
    FDemandChannelCount.FLength := 5;
    FDemandChannelCount.FDecimal := 0;
    FDemandChannelCount.FDefaultPadding := True;

    FGeneralChannelCount.FInitalised := False;
    FGeneralChannelCount.FData := 0;
    FGeneralChannelCount.FLength := 5;
    FGeneralChannelCount.FDecimal := 0;
    FGeneralChannelCount.FDefaultPadding := True;

    FNaturalInflowChannelCount.FInitalised := False;
    FNaturalInflowChannelCount.FData := 0;
    FNaturalInflowChannelCount.FLength := 5;
    FNaturalInflowChannelCount.FDecimal := 0;
    FNaturalInflowChannelCount.FDefaultPadding := True;


    FSummaryChannelCount.FInitalised := False;
    FSummaryChannelCount.FData := 0;
    FSummaryChannelCount.FLength := 5;
    FSummaryChannelCount.FDecimal := 0;
    FSummaryChannelCount.FDefaultPadding := True;

    FFirmYieldAnalysesCount.FInitalised := False;
    FFirmYieldAnalysesCount.FData := 0;
    FFirmYieldAnalysesCount.FLength := 5;
    FFirmYieldAnalysesCount.FDecimal := 0;
    FFirmYieldAnalysesCount.FDefaultPadding := True;

    FIrrigationBlockCount.FInitalised     := False;
    FIrrigationBlockCount.FData           := 0;
    FIrrigationBlockCount.FLength         := 5;
    FIrrigationBlockCount.FDecimal        := 0;
    FIrrigationBlockCount.FDefaultPadding := True;

    FWetlandCount.FInitalised     := False;
    FWetlandCount.FData           := 0;
    FWetlandCount.FLength         := 5;
    FWetlandCount.FDecimal        := 0;
    FWetlandCount.FDefaultPadding := True;

    FDemandCentreCount.FInitalised     := False;
    FDemandCentreCount.FData           := 0;
    FDemandCentreCount.FLength         := 5;
    FDemandCentreCount.FDecimal        := 0;
    FDemandCentreCount.FDefaultPadding := True;

    FReclaimationChannelCount.FInitalised     := False;
    FReclaimationChannelCount.FData           := 0;
    FReclaimationChannelCount.FLength         := 5;
    FReclaimationChannelCount.FDecimal        := 0;
    FReclaimationChannelCount.FDefaultPadding := True;

    FGroundWaterCount.FInitalised     := False;
    FGroundWaterCount.FData           := 0;
    FGroundWaterCount.FLength         := 5;
    FGroundWaterCount.FDecimal        := 0;
    FGroundWaterCount.FDefaultPadding := True;

    FInflowPenaltyNo.FInitalised     := False;
    FInflowPenaltyNo.FData           := 0;
    FInflowPenaltyNo.FLength         := 5;
    FInflowPenaltyNo.FDecimal        := 0;
    FInflowPenaltyNo.FDefaultPadding := True;


    FComment01.FInitalised := False;
    FComment01.FData := '';
    FComment01.FDefaultPadding := True;

    FComment02.FInitalised := False;
    FComment02.FData := '';
    FComment02.FDefaultPadding := True;

    FComment03.FInitalised := False;
    FComment03.FData := '';
    FComment03.FDefaultPadding := True;

    FComment04.FInitalised := False;
    FComment04.FData := '';
    FComment04.FDefaultPadding := True;

    FComment05.FInitalised := False;
    FComment05.FData := '';
    FComment05.FDefaultPadding := True;

    FComment06.FInitalised := False;
    FComment06.FData := '';
    FComment06.FDefaultPadding := True;

    FComment07.FInitalised := False;
    FComment07.FData := '';
    FComment07.FDefaultPadding := True;

    FComment08.FInitalised := False;
    FComment08.FData := '';
    FComment08.FDefaultPadding := True;

    FComment09.FInitalised := False;
    FComment09.FData := '';
    FComment09.FDefaultPadding := True;

    FComment10.FInitalised := False;
    FComment10.FData := '';
    FComment10.FDefaultPadding := True;

    FComment11.FInitalised := False;
    FComment11.FData := '';
    FComment11.FDefaultPadding := True;

    FComment12.FInitalised := False;
    FComment12.FData := '';
    FComment12.FDefaultPadding := True;

    FComment13.FInitalised := False;
    FComment13.FData := '';
    FComment13.FDefaultPadding := True;

    FComment14.FInitalised := False;
    FComment14.FData := '';
    FComment14.FDefaultPadding := True;

    FComment15.FInitalised := False;
    FComment15.FData := '';
    FComment15.FDefaultPadding := True;

    FComment16.FInitalised := False;
    FComment16.FData := '';
    FComment16.FDefaultPadding := True;

    FComment17.FInitalised := False;
    FComment17.FData := '';
    FComment17.FDefaultPadding := True;

    FComment22.FInitalised := False;
    FComment22.FData := '';
    FComment22.FDefaultPadding := True;

    FPenaltyChannelList.Clear;
    FMasterChannelList.Clear;
    FPowerChannelList.Clear;
    FIrrigationChannelList.Clear;
    FDiversionChannelList.Clear;
    FMinFlowChannelList.Clear;
    FLossChannelList.Clear;
    FMultiPurposeChannelList.Clear;
    FPumpingChannelList.Clear;
    FReturnFLowChannelList.Clear;
    FInflowChannelList.Clear;
    FDemandChannelList.Clear;
    FGeneralChannelList.Clear;
    FNaturalInflowChannelList.Clear;
    FSummaryChannelList.Clear;
    FIrrigationBlockList.Clear;
    FWetlandList.Clear;
    FDemandCentreList.Clear;
    FReclaimationChannelList.Clear;
    FGroundWaterList.Clear;
    FF03ExtraLines.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddDemandChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddDemandChannels';
var
  LCount : Integer;
  LObject: TDemandChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FDemandChannelCount.FData do
    begin
      LObject := TDemandChannelObject.Create;
      FDemandChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddDiversionChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddDiversionChannels';
var
  LCount : Integer;
  LObject: TDiversionChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FDiversionChannelCount.FData do
    begin
      LObject := TDiversionChannelObject.Create;
      FDiversionChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddGeneralChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddGeneralChannels';
var
  LCount : Integer;
  LObject: TGeneralChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FGeneralChannelCount.FData do
    begin
      LObject := TGeneralChannelObject.Create;
      FGeneralChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDescrObject.AddNaturalInflowChannels: TNaturalInflowChannelObject;
const OPNAME = 'TChannelDescrObject.AddNaturalInflowChannels';
var
  //LCount : Integer;
  LObject: TNaturalInflowChannelObject ;
Begin
  Result := nil;
  try
    //for LCount := 0 to FNaturalInflowChannelCount.FData do
    //begin
      LObject := TNaturalInflowChannelObject.Create;
      FNaturalInflowChannelList.Add(LObject);
    //end;
    Result := LObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddInflowChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddInflowChannels';
var
  LCount : Integer;
  LObject: TInflowChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FInflowChannelCount.FData do
    begin
      LObject := TInflowChannelObject.Create;
      FInflowChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddIrrigationChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddIrrigationChannels';
var
  LCount : Integer;
  LObject: TIrrigationChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FIrrigationChannelCount.FData do
    begin
      LObject := TIrrigationChannelObject.Create;
      FIrrigationChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddLossChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddLossChannels';
var
  LCount : Integer;
  LObject: TLossChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FLossChannelCount.FData do
    begin
      LObject := TLossChannelObject.Create;
      FLossChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddMasterChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddMasterChannels';
var
  LCount : Integer;
  LObject: TMasterChannelObject;
Begin
  Result := False;
  try
    for LCount := 0 to FMasterChannelCount.FData do
    begin
      LObject := TMasterChannelObject.Create;
      FMasterChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddMinFlowChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddMinFlowChannels';
var
  LCount : Integer;
  LObject: TMinFlowChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FMinFlowChannelCount.FData do
    begin
      LObject := TMinFlowChannelObject.Create;
      FMinFlowChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddMultiPurposeChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddMultiPurposeChannels';
var
  LCount : Integer;
  LObject: TMultiPurposeChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FMultiPurposeChannelCount.FData do
    begin
      LObject := TMultiPurposeChannelObject.Create;
      FMultiPurposeChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddPenaltyChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddPenaltyChannels';
var
  LCount : Integer;
  LObject:  TPenaltyChannelObject;
Begin
  Result := False;
  try
    for LCount := 0 to  FPenaltyChannelCount.FData do
    begin
      LObject := TPenaltyChannelObject.Create;
      FPenaltyChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddPowerChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddPowerChannels';
var
  LCount : Integer;
  LObject: TPowerChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FPowerChannelCount.FData do
    begin
      LObject := TPowerChannelObject.Create;
      FPowerChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddPumpingChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddPumpingChannels';
var
  LCount : Integer;
  LObject: TPumpingChannelObject ;
Begin
  Result := False;
  try
    for LCount := 0 to FPumpingChannelCount.FData do
    begin
      LObject := TPumpingChannelObject.Create;
      FPumpingChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddSummaryChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddSummaryChannels';
var
  LCount   : Integer;
  LObject  : TSummaryChannelObject;
Begin
  Result := False;
  try
    for LCount := 0 to FSummaryChannelCount.FData do
    begin
      LObject := TSummaryChannelObject.Create;
      FSummaryChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDescrObject.Reset;
const OPNAME = 'TChannelDescrObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindPenaltyStructure(Apenalty: Integer): TPenaltyChannelObject;
const OPNAME = 'TChannelDescrObject.FindPenaltyStructure';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FPenaltyChannelList.Count - 1 do
    begin
      if(TPenaltyChannelObject(FPenaltyChannelList[LCount]).FPenaltyType.FData = Apenalty) then
      begin
        Result := TPenaltyChannelObject(FPenaltyChannelList[LCount]);
        Break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindDiversionChannel(AChannelNumber: Integer): TDiversionChannelObject;
const OPNAME = 'TChannelDescrObject.FindDiversionChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FDiversionChannelList.Count - 1 do
    begin
      if(TDiversionChannelObject(FDiversionChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TDiversionChannelObject(FDiversionChannelList[LCount]);
        Break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindDemandChannel(AChannelNumber: Integer): TDemandChannelObject;
const OPNAME = 'TChannelDescrObject.FindDemandChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FDemandChannelList.Count - 1 do
    begin
      if(TDemandChannelObject(FDemandChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TDemandChannelObject(FDemandChannelList[LCount]);
        Break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindGeneralChannel(AChannelNumber: Integer): TGeneralChannelObject;
const OPNAME = 'TChannelDescrObject.FindGeneralChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FGeneralChannelList.Count - 1 do
    begin
      if(TGeneralChannelObject(FGeneralChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TGeneralChannelObject(FGeneralChannelList[LCount]);
        Break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindNaturalInflowChannel(AChannelNumber: Integer):TNaturalInflowChannelObject;
const OPNAME = 'TChannelDescrObject.FindNaturalInflowChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FNaturalInflowChannelList.Count - 1 do
    begin
      if(TNaturalInflowChannelObject(FNaturalInflowChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TNaturalInflowChannelObject(FNaturalInflowChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindInflowChannel(AChannelNumber: Integer): TInflowChannelObject;
const OPNAME = 'TChannelDescrObject.FindInflowChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FInflowChannelList.Count - 1 do
    begin
      if(TInflowChannelObject(FInflowChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TInflowChannelObject(FInflowChannelList[LCount]);
        Break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindIrrigationChannel(AChannelNumber: Integer): TIrrigationChannelObject;
const OPNAME = 'TChannelDescrObject.FindIrrigationChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FIrrigationChannelList.Count - 1 do
    begin
      if(TIrrigationChannelObject(FIrrigationChannelList[LCount]).FIrrigationNodeNumber.FData = AChannelNumber) then
      begin
        Result := TIrrigationChannelObject(FIrrigationChannelList[LCount]);
        Break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindLossChannels(AChannelNumber: Integer): TLossChannelObject;
const OPNAME = 'TChannelDescrObject.FindLossChannels';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FLossChannelList.Count - 1 do
    begin
      if(TLossChannelObject(FLossChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TLossChannelObject(FLossChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindMasterChannel(AChannelNumber: Integer): TMasterChannelObject;
const OPNAME = 'TChannelDescrObject.FindMasterChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FMasterChannelList.Count - 1 do
    begin
      if(TMasterChannelObject(FMasterChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TMasterChannelObject(FMasterChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindMinFlowChannel(AChannelNumber: Integer): TMinFlowChannelObject;
const OPNAME = 'TChannelDescrObject.FindMinFlowChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FMinFlowChannelList.Count - 1 do
    begin
      if(TMinFlowChannelObject(FMinFlowChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TMinFlowChannelObject(FMinFlowChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindMultiPurposeChannel(AChannelNumber: Integer): TMultiPurposeChannelObject;
const OPNAME = 'TChannelDescrObject.FindMultiPurposeChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FMultiPurposeChannelList.Count - 1 do
    begin
      if(TMultiPurposeChannelObject(FMultiPurposeChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TMultiPurposeChannelObject(FMultiPurposeChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindPowerChannels(AChannelNumber: Integer): TPowerChannelObject;
const OPNAME = 'TChannelDescrObject.FindPowerChannels';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FPowerChannelList.Count - 1 do
    begin
      if(TPowerChannelObject(FPowerChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TPowerChannelObject(FPowerChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindPumpingChannel(AChannelNumber: Integer): TPumpingChannelObject;
const OPNAME = 'TChannelDescrObject.FindPumpingChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FPumpingChannelList.Count - 1 do
    begin
      if(TPumpingChannelObject(FPumpingChannelList[LCount]).FChannelNumber.FData = AChannelNumber) then
      begin
        Result := TPumpingChannelObject(FPumpingChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddIrrigationBlockChannels: Boolean;
const OPNAME = 'TChannelDescrObject.AddIrrigationBlockChannels';
var
  LCount   : Integer;
  LObject  : TIrrigationBlockChannelObject;
Begin
  Result := False;
  try
    for LCount := 0 to FIrrigationBlockCount.FData do
    begin
      LObject := TIrrigationBlockChannelObject.Create;
      FIrrigationBlockList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddWetlandChannels: Boolean;
const OPNAME = 'TChannelDescrObject.AddWetlandChannels';
var
  LCount   : Integer;
  LObject  : TWetlandChannelObject;
Begin
  Result := False;
  try
    for LCount := 0 to FWetlandCount.FData do
    begin
      LObject := TWetlandChannelObject.Create;
      FWetlandList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindIrrigationBlockAbstractChannels(AChannelNumber: Integer): TIrrigationBlockChannelObject;
const OPNAME = 'TChannelDescrObject.FindIrrigationBlockAbstractChannels';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FIrrigationBlockList.Count - 1 do
    begin
      if(TIrrigationBlockChannelObject(FIrrigationBlockList[LCount]).FAbstractChannelNr.FData = AChannelNumber) then
      begin
        Result := TIrrigationBlockChannelObject(FIrrigationBlockList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindIrrigationBlockReturnChannels(AChannelNumber: Integer): TIrrigationBlockChannelObject;
const OPNAME = 'TChannelDescrObject.FindIrrigationBlockReturnChannels';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FIrrigationBlockList.Count - 1 do
    begin
      if(TIrrigationBlockChannelObject(FIrrigationBlockList[LCount]).FReturnFlowChannelNr.FData = AChannelNumber) then
      begin
        Result := TIrrigationBlockChannelObject(FIrrigationBlockList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindIrrigationBlockByNodeNumber(ANodeNumber: Integer): TIrrigationBlockChannelObject;
const OPNAME = 'TChannelDescrObject.FindIrrigationBlockByNodeNumber';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FIrrigationBlockList.Count - 1 do
    begin
      if(TIrrigationBlockChannelObject(FIrrigationBlockList[LCount]).FBlockNumber.FData = ANodeNumber) then
      begin
        Result := TIrrigationBlockChannelObject(FIrrigationBlockList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindWetlandInflowChannels(AChannelNumber: Integer): TWetlandChannelObject;
const OPNAME = 'TChannelDescrObject.FindWetlandInflowChannels';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FWetlandList.Count - 1 do
    begin
      if(TWetlandChannelObject(FWetlandList[LCount]).FInflowChannelNr.FData = AChannelNumber) then
      begin
        Result := TWetlandChannelObject(FWetlandList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindWetlandOutflowChannels(AChannelNumber: Integer): TWetlandChannelObject;
const OPNAME = 'TChannelDescrObject.FindWetlandOutflowChannels';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FWetlandList.Count - 1 do
    begin
      if(TWetlandChannelObject(FWetlandList[LCount]).FOutflowChannelNr.FData = AChannelNumber) then
      begin
        Result := TWetlandChannelObject(FWetlandList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindWetlandByNodeNumber(ANodeNumber: Integer): TWetlandChannelObject;
const OPNAME = 'TChannelDescrObject.FindWetlandByNodeNumber';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FWetlandList.Count - 1 do
    begin
      if(TWetlandChannelObject(FWetlandList[LCount]).FNodeNumber.FData = ANodeNumber) then
      begin
        Result := TWetlandChannelObject(FWetlandList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddDemandCentreChannels: Boolean;
const OPNAME = 'TChannelDescrObject.AddDemandCentreChannels';
var
  LCount   : Integer;
  LObject  : TDemandCentreObject;
Begin
  Result := False;
  try
    for LCount := 0 to FDemandCentreCount.FData do
    begin
      LObject := TDemandCentreObject.Create;
      FDemandCentreList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindDemandCentreConsumptiveChannel(AChannelNumber: Integer): TDemandCentreObject;
const OPNAME = 'TChannelDescrObject.FindDemandCentreConsumptiveChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FDemandCentreList.Count - 1 do
    begin
      if(TDemandCentreObject(FDemandCentreList[LCount]).FConsumptiveChannelNr.FData = AChannelNumber) then
      begin
        Result := TDemandCentreObject(FDemandCentreList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindDemandCentreReclaimationChannel(AChannelNumber: Integer): TReclaimationChannelObject;
const OPNAME = 'TChannelDescrObject.FindDemandCentreReclaimationChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FReclaimationChannelList.Count - 1 do
    begin
      if(TReclaimationChannelObject(FReclaimationChannelList[LCount]).FChannelNr.FData = AChannelNumber) then
      begin
        Result := TReclaimationChannelObject(FReclaimationChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddReturnFlowChannels: boolean;
const OPNAME = 'TChannelDescrObject.AddReturnFlowChannels';
var
  LCount : Integer;
  LObject: TReturnFlowChannelObject;
Begin
  Result := False;
  try
    for LCount := 0 to FReturnFlowChannelCount.FData do
    begin
      LObject := TReturnFlowChannelObject.Create;
      FReturnFLowChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.FindReturnFlowChannel(AChannelNumber: Integer): TReturnFlowChannelObject;
const OPNAME = 'TChannelDescrObject.FindReturnFlowChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FReturnFLowChannelList.Count - 1 do
    begin
      if(TReturnFlowChannelObject(FReturnFLowChannelList[LCount]).FChannelNr.FData = AChannelNumber) then
      begin
        Result := TReturnFlowChannelObject(FReturnFLowChannelList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddReclaimationChannels: Boolean;
const OPNAME = 'TChannelDescrObject.AddReclaimationChannels';
var
  LCount   : Integer;
  LObject  : TReclaimationChannelObject;
Begin
  Result := False;
  try
    for LCount := 0 to FReclaimationChannelCount.FData do
    begin
      LObject := TReclaimationChannelObject.Create;
      FReclaimationChannelList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDescrObject.AddGroundWaterChannels: Boolean;
const OPNAME = 'TChannelDescrObject.AddGroundWaterChannels';
var
  LCount  : Integer;
  LObject : TGroundWaterObject;
Begin
  Result := False;
  try
    for LCount := 0 to FGroundWaterCount.FData do
    begin
      LObject := TGroundWaterObject.Create;
      FGroundWaterList.Add(LObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPenaltyChannelObject }

procedure TPenaltyChannelObject.CreateMemberObjects;
const OPNAME = 'TPenaltyChannelObject.CreateMemberObjects';
var
  LCount: integer;
  LDouble: TDouble;
begin
  try
    FIdentifier  := TInteger.Create;
    FPenaltyName := TString.Create;

    FPenaltyType := TInteger.Create;
    FArcCount := TInteger.Create;
    FComment := TString.Create;
    for LCount :=  MinArcs to MaxArcs do
    begin
      LDouble := TDouble.Create;
      FArcPenalty[LCount] := LDouble;
    end;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPenaltyChannelObject.DestroyMemberObjects;
const OPNAME = 'TPenaltyChannelObject.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    FIdentifier.Free;
    FPenaltyName.Free;

    FPenaltyType.Free;
    FArcCount.Free;
    for LCount :=  MinArcs to MaxArcs do
      TDouble(FArcPenalty[LCount]).Free;
    FComment.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPenaltyChannelObject.Initialise: boolean;
const OPNAME = 'TPenaltyChannelObject.Initialise';
var
  LCount: integer;
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FPenaltyName.FInitalised := False;
    FPenaltyName.FData := '';
    FPenaltyName.FLength := 0;
    FPenaltyName.FDecimal := 0;
    FPenaltyName.FDefaultPadding := True;


    FPenaltyType.FInitalised := False;
    FPenaltyType.FData := 0;
    FPenaltyType.FLength := 5;
    FPenaltyType.FDecimal := 0;
    FPenaltyType.FDefaultPadding := True;

    FArcCount.FInitalised := False;
    FArcCount.FData := 0;
    FArcCount.FLength := 5;
    FArcCount.FDecimal := 0;
    FArcCount.FDefaultPadding := True;

    for LCount :=  MinArcs to MaxArcs do
    begin
      TDouble(FArcPenalty[LCount]).FInitalised := False;
      TDouble(FArcPenalty[LCount]).FData := 0.0;
      TDouble(FArcPenalty[LCount]).FLength := 10;
      TDouble(FArcPenalty[LCount]).FDecimal := 0;
      TDouble(FArcPenalty[LCount]).FDefaultPadding := True;
      TDouble(FArcPenalty[LCount]).ShowDecimalPoint := True;
    end;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPenaltyChannelObject.Reset;
const OPNAME = 'TPenaltyChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMasterChannelObject }

procedure TMasterChannelObject.CreateMemberObjects;
const OPNAME = 'TMasterChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FChannelType := TChar.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterChannelObject.DestroyMemberObjects;
const OPNAME = 'TMasterChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FChannelType.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterChannelObject.Initialise: boolean;
const OPNAME = 'TMasterChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FChannelType.FInitalised := False;
    FChannelType.FData := ' ';
    FChannelType.FLength := 5;
    FChannelType.FDecimal := 0;
    FChannelType.FDefaultPadding := False;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterChannelObject.Reset;
const OPNAME = 'TMasterChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPowerChannelObject }

procedure TPowerChannelObject.CreateMemberObjects;
const OPNAME = 'TPowerChannelObject.CreateMemberObjects';
var
  LCount: integer;
  LInteger: Tinteger;
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FSpillChannelNumber := TInteger.Create;
    FSpillChannelName   := TString.Create;
    FSpillUpNodeNumber := TInteger.Create;
    FSpillDownNodeNumber := TInteger.Create;
    FSpillPenaltyStructType := TInteger.Create;
    FDownStreamPowerChannelCount := TInteger.Create;
    for LCount := MinDownStreamPowerChannels to MaxDownStreamPowerChannels do
    begin
      LInteger := Tinteger.Create;
      FDownStreamPowerChannels[LCount] := LInteger;
    end;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerChannelObject.DestroyMemberObjects;
const OPNAME = 'TPowerChannelObject.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FSpillChannelNumber.Free;
    FSpillChannelName.Free;
    FSpillUpNodeNumber.Free;
    FSpillDownNodeNumber.Free;
    FSpillPenaltyStructType.Free;
    FDownStreamPowerChannelCount.Free;
    for LCount := MinDownStreamPowerChannels to MaxDownStreamPowerChannels do
      FDownStreamPowerChannels[LCount].Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerChannelObject.Initialise: boolean;
const OPNAME = 'TPowerChannelObject.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FSpillChannelNumber.FInitalised := False;
    FSpillChannelNumber.FData := 0;
    FSpillChannelNumber.FLength := 5;
    FSpillChannelNumber.FDecimal := 0;
    FSpillChannelNumber.FDefaultPadding := True;

    FSpillChannelName.FInitalised := False;
    FSpillChannelName.FData := '';
    FSpillChannelName.FLength := 0;
    FSpillChannelName.FDecimal := 0;
    FSpillChannelName.FDefaultPadding := True;

    FSpillUpNodeNumber.FInitalised := False;
    FSpillUpNodeNumber.FData := 0;
    FSpillUpNodeNumber.FLength := 5;
    FSpillUpNodeNumber.FDecimal := 0;
    FSpillUpNodeNumber.FDefaultPadding := True;

    FSpillDownNodeNumber.FInitalised := False;
    FSpillDownNodeNumber.FData := 0;
    FSpillDownNodeNumber.FLength := 5;
    FSpillDownNodeNumber.FDecimal := 0;
    FSpillDownNodeNumber.FDefaultPadding := True;

    FSpillPenaltyStructType.FInitalised := False;
    FSpillPenaltyStructType.FData := 0;
    FSpillPenaltyStructType.FLength := 5;
    FSpillPenaltyStructType.FDecimal := 0;
    FSpillPenaltyStructType.FDefaultPadding := True;

    FDownStreamPowerChannelCount.FInitalised := False;
    FDownStreamPowerChannelCount.FData := 0;
    FDownStreamPowerChannelCount.FLength := 5;
    FDownStreamPowerChannelCount.FDecimal := 0;
    FDownStreamPowerChannelCount.FDefaultPadding := True;

    for LCount := MinDownStreamPowerChannels to MaxDownStreamPowerChannels do
    begin
      FDownStreamPowerChannels[LCount].FInitalised := False;
      FDownStreamPowerChannels[LCount].FData := 0;
      FDownStreamPowerChannels[LCount].FLength := 5;
      FDownStreamPowerChannels[LCount].FDecimal := 0;
      FDownStreamPowerChannels[LCount].FDefaultPadding := True;
    end;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerChannelObject.Reset;
const OPNAME = 'TPowerChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIrrigationChannelObject }

procedure TIrrigationChannelObject.CreateMemberObjects;
const OPNAME = 'TIrrigationChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FIrrigationNodeNumber := TInteger.Create;
    FUpstreamNodeNumber := TInteger.Create;
    FDiversionChannelNumber := TInteger.Create;
    FDiversionChannelName   := TString.Create;
    FIrrigationPenaltyStructType := TInteger.Create;
    FDownStreamNodeNumber := TInteger.Create;
    FReturnChannelNumber := TInteger.Create;
    FReturnChannelName   := TString.Create;
    FReturnPenaltyStructType := TInteger.Create;
    FConsumptiveChannelNumber := TInteger.Create;
    FConsumptiveChannelName   := TString.Create;
    FConsumptivePenaltyStructType := TInteger.Create;
    FRelaxationDemand := TInteger.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationChannelObject.DestroyMemberObjects;
const OPNAME = 'TIrrigationChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;

    FIrrigationNodeNumber.Free;
    FUpstreamNodeNumber.Free;
    FDiversionChannelNumber.Free;
    FDiversionChannelName.Free;
    FIrrigationPenaltyStructType.Free;
    FDownStreamNodeNumber.Free;
    FReturnChannelNumber.Free;
    FReturnChannelName.Free;
    FReturnPenaltyStructType.Free;
    FConsumptiveChannelNumber.Free;
    FConsumptiveChannelName.Free;
    FConsumptivePenaltyStructType.Free;
    FRelaxationDemand.Free;
    FComment.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationChannelObject.Initialise: boolean;
const OPNAME = 'TIrrigationChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FIrrigationNodeNumber.FInitalised := False;
    FIrrigationNodeNumber.FData := 0;
    FIrrigationNodeNumber.FLength := 5;
    FIrrigationNodeNumber.FDecimal := 0;
    FIrrigationNodeNumber.FDefaultPadding := True;

    FUpstreamNodeNumber.FInitalised := False;
    FUpstreamNodeNumber.FData := 0;
    FUpstreamNodeNumber.FLength := 5;
    FUpstreamNodeNumber.FDecimal := 0;
    FUpstreamNodeNumber.FDefaultPadding := True;

    FDiversionChannelNumber.FInitalised := False;
    FDiversionChannelNumber.FData := 0;
    FDiversionChannelNumber.FLength := 5;
    FDiversionChannelNumber.FDecimal := 0;
    FDiversionChannelNumber.FDefaultPadding := True;

    FDiversionChannelName.FInitalised := False;
    FDiversionChannelName.FData := '';
    FDiversionChannelName.FLength := 0;
    FDiversionChannelName.FDecimal := 0;
    FDiversionChannelName.FDefaultPadding := True;

    FIrrigationPenaltyStructType.FInitalised := False;
    FIrrigationPenaltyStructType.FData := 0;
    FIrrigationPenaltyStructType.FLength := 5;
    FIrrigationPenaltyStructType.FDecimal := 0;
    FIrrigationPenaltyStructType.FDefaultPadding := True;

    FDownStreamNodeNumber.FInitalised := False;
    FDownStreamNodeNumber.FData := 0;
    FDownStreamNodeNumber.FLength := 5;
    FDownStreamNodeNumber.FDecimal := 0;
    FDownStreamNodeNumber.FDefaultPadding := True;

    FReturnChannelNumber.FInitalised := False;
    FReturnChannelNumber.FData := 0;
    FReturnChannelNumber.FLength := 5;
    FReturnChannelNumber.FDecimal := 0;
    FReturnChannelNumber.FDefaultPadding := True;

    FReturnChannelName.FInitalised := False;
    FReturnChannelName.FData := '';
    FReturnChannelName.FLength := 0;
    FReturnChannelName.FDecimal := 0;
    FReturnChannelName.FDefaultPadding := True;

    FReturnPenaltyStructType.FInitalised := False;
    FReturnPenaltyStructType.FData := 0;
    FReturnPenaltyStructType.FLength := 5;
    FReturnPenaltyStructType.FDecimal := 0;
    FReturnPenaltyStructType.FDefaultPadding := True;

    FConsumptiveChannelNumber.FInitalised := False;
    FConsumptiveChannelNumber.FData := 0;
    FConsumptiveChannelNumber.FLength := 5;
    FConsumptiveChannelNumber.FDecimal := 0;
    FConsumptiveChannelNumber.FDefaultPadding := True;

    FConsumptiveChannelName.FInitalised := False;
    FConsumptiveChannelName.FData := '';
    FConsumptiveChannelName.FLength := 0;
    FConsumptiveChannelName.FDecimal := 0;
    FConsumptiveChannelName.FDefaultPadding := True;

    FConsumptivePenaltyStructType.FInitalised := False;
    FConsumptivePenaltyStructType.FData := 0;
    FConsumptivePenaltyStructType.FLength := 5;
    FConsumptivePenaltyStructType.FDecimal := 0;
    FConsumptivePenaltyStructType.FDefaultPadding := True;

    FRelaxationDemand.FInitalised := False;
    FRelaxationDemand.FData := 0;
    FRelaxationDemand.FLength := 5;
    FRelaxationDemand.FDecimal := 0;
    FRelaxationDemand.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationChannelObject.Reset;
const OPNAME = 'TIrrigationChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDiversionChannelObject }

procedure TDiversionChannelObject.CreateMemberObjects;
const OPNAME = 'TDiversionChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;

    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FChannelType := TInteger.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelObject.DestroyMemberObjects;
const OPNAME = 'TDiversionChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;

    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FChannelType.Free;
    FComment.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelObject.Initialise: boolean;
const OPNAME = 'TDiversionChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FChannelType.FInitalised := False;
    FChannelType.FData := 0;
    FChannelType.FLength := 5;
    FChannelType.FDecimal := 0;
    FChannelType.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelObject.Reset;
const OPNAME = 'TDiversionChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMinFlowChannelObject }

procedure TMinFlowChannelObject.CreateMemberObjects;
const OPNAME = 'TMinFlowChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinFlowChannelObject.DestroyMemberObjects;
const OPNAME = 'TMinFlowChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinFlowChannelObject.Initialise: boolean;
const OPNAME = 'TMinFlowChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinFlowChannelObject.Reset;
const OPNAME = 'TMinFlowChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TLossChannelObject }

procedure TLossChannelObject.CreateMemberObjects;
const OPNAME = 'TLossChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;

    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FChannelType := TInteger.Create;
    FReference := TInteger.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelObject.DestroyMemberObjects;
const OPNAME = 'TLossChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;

    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FChannelType.Free;
    FReference.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelObject.Initialise: boolean;
const OPNAME = 'TLossChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FChannelType.FInitalised := False;
    FChannelType.FData := 0;
    FChannelType.FLength := 5;
    FChannelType.FDecimal := 0;
    FChannelType.FDefaultPadding := True;

    FReference.FInitalised := False;
    FReference.FData := 0;
    FReference.FLength := 5;
    FReference.FDecimal := 0;
    FReference.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelObject.Reset;
const OPNAME = 'TLossChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMultiPurposeChannelObject }

procedure TMultiPurposeChannelObject.CreateMemberObjects;
const OPNAME = 'TMultiPurposeChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiPurposeChannelObject.DestroyMemberObjects;
const OPNAME = 'TMultiPurposeChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FComment.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiPurposeChannelObject.Initialise: boolean;
const OPNAME = 'TMultiPurposeChannelObject.Initialise';
Begin
  Result := False;
  try
    FChannelType := ctMinMaxChannel;

    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiPurposeChannelObject.Reset;
const OPNAME = 'TMultiPurposeChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPumpingChannelObject }

procedure TPumpingChannelObject.CreateMemberObjects;
const OPNAME = 'TPumpingChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FPumpingHead := TDouble.Create;
    FEfficiency := TDouble.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingChannelObject.DestroyMemberObjects;
const OPNAME = 'TPumpingChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FPumpingHead.Free;
    FEfficiency.Free;
    FComment.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingChannelObject.Initialise: boolean;
const OPNAME = 'TPumpingChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FPumpingHead.FInitalised := False;
    FPumpingHead.FData := 0.0;
    FPumpingHead.FLength := 10;
    FPumpingHead.FDecimal := 1;
    FPumpingHead.FDefaultPadding := True;
    FPumpingHead.ShowDecimalPoint := True;

    FEfficiency.FInitalised := False;
    FEfficiency.FData := 0.0;
    FEfficiency.FLength := 10;
    FEfficiency.FDecimal := 2;
    FEfficiency.FDefaultPadding := True;
    FEfficiency.ShowDecimalPoint := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingChannelObject.Reset;
const OPNAME = 'TPumpingChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TInflowChannelObject }

procedure TInflowChannelObject.CreateMemberObjects;
const OPNAME = 'TInflowChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FComment := TString.Create;
    FInflowFileName := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInflowChannelObject.DestroyMemberObjects;
const OPNAME = 'TInflowChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FComment.Free;
    FInflowFileName.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInflowChannelObject.Initialise: boolean;
const OPNAME = 'TInflowChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    FInflowFileName.FInitalised := False;
    FInflowFileName.FData := '';
    FInflowFileName.FLength := 255;
    FInflowFileName.FDecimal := 0;
    FInflowFileName.FDefaultPadding := False;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInflowChannelObject.Reset;
const OPNAME = 'TInflowChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandChannelObject }

procedure TDemandChannelObject.CreateMemberObjects;
const OPNAME = 'TDemandChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FGaugeNumber := TInteger.Create;
    FStochastic := TChar.Create;
    FFullname := TString.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandChannelObject.DestroyMemberObjects;
const OPNAME = 'TDemandChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FGaugeNumber.Free;
    FStochastic.Free;
    FFullname.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandChannelObject.Initialise: boolean;
const OPNAME = 'TDemandChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FGaugeNumber.FInitalised := False;
    FGaugeNumber.FData := 0;
    FGaugeNumber.FLength := 5;
    FGaugeNumber.FDecimal := 0;
    FGaugeNumber.FDefaultPadding := True;

    FStochastic.FInitalised := False;
    FStochastic.FData := ' ';
    FStochastic.FLength := 5;
    FStochastic.FDecimal := 0;
    FStochastic.FDefaultPadding := False;

    FFullname.FInitalised := False;
    FFullname.FData := '';
    FFullname.FLength := 50;
    FFullname.FDecimal := 0;
    FFullname.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandChannelObject.Reset;
const OPNAME = 'TDemandChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGeneralChannelObject }

procedure TGeneralChannelObject.CreateMemberObjects;
const OPNAME = 'TGeneralChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FUpNodeNumber := TInteger.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralChannelObject.DestroyMemberObjects;
const OPNAME = 'TGeneralChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;
    FComment.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralChannelObject.Initialise: boolean;
const OPNAME = 'TGeneralChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FUpNodeNumber.FInitalised := False;
    FUpNodeNumber.FData := 0;
    FUpNodeNumber.FLength := 5;
    FUpNodeNumber.FDecimal := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralChannelObject.Reset;
const OPNAME = 'TGeneralChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TNaturalInflowChannelObject }

procedure TNaturalInflowChannelObject.CreateMemberObjects;
const OPNAME = 'TGeneralChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;
    FChannelNumber := TInteger.Create;
    FChannelName   := TString.Create;
    FDownNodeNumber := TInteger.Create;
    FPenaltyStructType := TInteger.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNaturalInflowChannelObject.DestroyMemberObjects;
const OPNAME = 'TGeneralChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FChannelNumber.Free;
    FChannelName.Free;
    FDownNodeNumber.Free;
    FPenaltyStructType.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNaturalInflowChannelObject.Reset;
const OPNAME = 'TGeneralChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TNaturalInflowChannelObject.Initialise: boolean;
const OPNAME = 'TNaturalInflowChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FDownNodeNumber.FInitalised := False;
    FDownNodeNumber.FData := 0;
    FDownNodeNumber.FLength := 5;
    FDownNodeNumber.FDecimal := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FPenaltyStructType.FInitalised := False;
    FPenaltyStructType.FData := 0;
    FPenaltyStructType.FLength := 5;
    FPenaltyStructType.FDecimal := 0;
    FPenaltyStructType.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSummaryChannelObject }

procedure TSummaryChannelObject.CreateMemberObjects;
const OPNAME = 'TSummaryChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier  := TInteger.Create;

    FChannelNumber := TInteger.Create;
    FCalculateFirmYield := TString.Create;
    FFlowOutput      := TString.Create;
    FChannelName := TString.Create;
    FComment := TString.Create;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryChannelObject.DestroyMemberObjects;
const OPNAME = 'TSummaryChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;

    FChannelNumber.Free;
    FCalculateFirmYield.Free;
    FFlowOutput.Free;
    FChannelName.Free;
    FComment.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryChannelObject.Initialise: boolean;
const OPNAME = 'TSummaryChannelObject.Initialise';
Begin
  Result := False;
  try
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FChannelNumber.FInitalised := False;
    FChannelNumber.FData := 0;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FCalculateFirmYield.FInitalised := False;
    FCalculateFirmYield.FData := ' ';
    FCalculateFirmYield.FLength := 1;
    FCalculateFirmYield.FDecimal := 0;
    FCalculateFirmYield.FDefaultPadding := True;

    FFlowOutput.FInitalised := False;
    FFlowOutput.FData := ' ';
    FFlowOutput.FLength := 1;
    FFlowOutput.FDecimal := 0;
    FFlowOutput.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryChannelObject.Reset;
const OPNAME = 'TSummaryChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIrrigationBlockChannelObject }

procedure TIrrigationBlockChannelObject.CreateMemberObjects;
const OPNAME = 'TIrrigationBlockChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier             := TInteger.Create;
    FUpNodeNumber           := TInteger.Create;
    FDownNodeNumber         := TInteger.Create;
    FAbstractChannelNr      := TInteger.Create;
    FAbstractChannelName    := TString.Create;
    FAbstractPenaltyType    := TInteger.Create;
    FReturnFlowChannelNr    := TInteger.Create;
    FReturnFlowChannelName  := TString.Create;
    FReturnFlowPenaltyType  := TInteger.Create;
    FBlockNumber            := TInteger.Create;
    FChannelType            := TInteger.Create;
    FComment                := TString.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockChannelObject.DestroyMemberObjects;
const OPNAME = 'TIrrigationBlockChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FAbstractChannelNr.Free;
    FAbstractChannelName.Free;
    FAbstractPenaltyType.Free;
    FReturnFlowChannelNr.Free;
    FReturnFlowChannelName.Free;
    FReturnFlowPenaltyType.Free;
    FBlockNumber.Free;
    FChannelType.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockChannelObject.Initialise: Boolean;
const OPNAME = 'TIrrigationBlockChannelObject.Initialise';
begin
  Result := False;
  try
    FIdentifier.FInitalised     := False;
    FIdentifier.FData           := 0;
    FIdentifier.FLength         := 5;
    FIdentifier.FDecimal        := 0;
    FIdentifier.FDefaultPadding := True;

    FUpNodeNumber.FInitalised     := False;
    FUpNodeNumber.FData           := 0;
    FUpNodeNumber.FLength         := 5;
    FUpNodeNumber.FDecimal        := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised     := False;
    FDownNodeNumber.FData           := 0;
    FDownNodeNumber.FLength         := 5;
    FDownNodeNumber.FDecimal        := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FAbstractChannelNr.FInitalised     := False;
    FAbstractChannelNr.FData           := 0;
    FAbstractChannelNr.FLength         := 5;
    FAbstractChannelNr.FDecimal        := 0;
    FAbstractChannelNr.FDefaultPadding := True;

    FAbstractChannelName.FInitalised := False;
    FAbstractChannelName.FData := '';
    FAbstractChannelName.FLength := 0;
    FAbstractChannelName.FDecimal := 0;
    FAbstractChannelName.FDefaultPadding := True;

    FAbstractPenaltyType.FInitalised     := False;
    FAbstractPenaltyType.FData           := 0;
    FAbstractPenaltyType.FLength         := 5;
    FAbstractPenaltyType.FDecimal        := 0;
    FAbstractPenaltyType.FDefaultPadding := True;

    FReturnFlowChannelNr.FInitalised     := False;
    FReturnFlowChannelNr.FData           := 0;
    FReturnFlowChannelNr.FLength         := 5;
    FReturnFlowChannelNr.FDecimal        := 0;
    FReturnFlowChannelNr.FDefaultPadding := True;

    FReturnFlowChannelName.FInitalised := False;
    FReturnFlowChannelName.FData := '';
    FReturnFlowChannelName.FLength := 0;
    FReturnFlowChannelName.FDecimal := 0;
    FReturnFlowChannelName.FDefaultPadding := True;

    FReturnFlowPenaltyType.FInitalised     := False;
    FReturnFlowPenaltyType.FData           := 0;
    FReturnFlowPenaltyType.FLength         := 5;
    FReturnFlowPenaltyType.FDecimal        := 0;
    FReturnFlowPenaltyType.FDefaultPadding := True;

    FBlockNumber.FInitalised     := False;
    FBlockNumber.FData           := 0;
    FBlockNumber.FLength         := 5;
    FBlockNumber.FDecimal        := 0;
    FBlockNumber.FDefaultPadding := True;

    FChannelType.FInitalised     := False;
    FChannelType.FData           := 0;
    FChannelType.FLength         := 5;
    FChannelType.FDecimal        := 0;
    FChannelType.FDefaultPadding := True;

    FComment.FInitalised          := False;
    FComment.FData                := '';
    FComment.FDecimal             := 0;
    FComment.FDefaultPadding      := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockChannelObject.Reset;
const OPNAME = 'TIrrigationBlockChannelObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TWetlandObject }

procedure TWetlandChannelObject.CreateMemberObjects;
const OPNAME = 'TWetlandChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier         := TInteger.Create;
    FNodeNumber         := TInteger.Create;
    FUpNodeNumber       := TInteger.Create;
    FDownNodeNumber     := TInteger.Create;
    FInflowChannelNr    := TInteger.Create;
    FInflowChannelName  := TString.Create;
    FInflowPenaltyType  := TInteger.Create;
    FOutflowChannelNr   := TInteger.Create;
    FOutflowChannelName  := TString.Create;
    FOutflowPenaltyType := TInteger.Create;
    FChannelType        := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandChannelObject.DestroyMemberObjects;
const OPNAME = 'TWetlandChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FNodeNumber.Free;
    FUpNodeNumber.Free;
    FDownNodeNumber.Free;
    FInflowChannelNr.Free;
    FInflowChannelName.Free;
    FInflowPenaltyType.Free;
    FOutflowChannelNr.Free;
    FOutflowChannelName.Free;
    FOutflowPenaltyType.Free;
    FChannelType.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandChannelObject.Initialise: Boolean;
const OPNAME = 'TWetlandChannelObject.Initialise';
begin
  Result := False;
  try
    FIdentifier.FInitalised     := False;
    FIdentifier.FData           := 0;
    FIdentifier.FLength         := 5;
    FIdentifier.FDecimal        := 0;
    FIdentifier.FDefaultPadding := True;

    FNodeNumber.FInitalised     := False;
    FNodeNumber.FData           := 0;
    FNodeNumber.FLength         := 5;
    FNodeNumber.FDecimal        := 0;
    FNodeNumber.FDefaultPadding := True;

    FUpNodeNumber.FInitalised     := False;
    FUpNodeNumber.FData           := 0;
    FUpNodeNumber.FLength         := 5;
    FUpNodeNumber.FDecimal        := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised     := False;
    FDownNodeNumber.FData           := 0;
    FDownNodeNumber.FLength         := 5;
    FDownNodeNumber.FDecimal        := 0;
    FDownNodeNumber.FDefaultPadding := True;

    FInflowChannelNr.FInitalised     := False;
    FInflowChannelNr.FData           := 0;
    FInflowChannelNr.FLength         := 5;
    FInflowChannelNr.FDecimal        := 0;
    FInflowChannelNr.FDefaultPadding := True;

    FInflowChannelName.FInitalised := False;
    FInflowChannelName.FData := '';
    FInflowChannelName.FLength := 0;
    FInflowChannelName.FDecimal := 0;
    FInflowChannelName.FDefaultPadding := True;

    FInflowPenaltyType.FInitalised     := False;
    FInflowPenaltyType.FData           := 0;
    FInflowPenaltyType.FLength         := 5;
    FInflowPenaltyType.FDecimal        := 0;
    FInflowPenaltyType.FDefaultPadding := True;

    FOutflowChannelNr.FInitalised     := False;
    FOutflowChannelNr.FData           := 0;
    FOutflowChannelNr.FLength         := 5;
    FOutflowChannelNr.FDecimal        := 0;
    FOutflowChannelNr.FDefaultPadding := True;

    FOutflowChannelName.FInitalised := False;
    FOutflowChannelName.FData := '';
    FOutflowChannelName.FLength := 0;
    FOutflowChannelName.FDecimal := 0;
    FOutflowChannelName.FDefaultPadding := True;

    FOutflowPenaltyType.FInitalised     := False;
    FOutflowPenaltyType.FData           := 0;
    FOutflowPenaltyType.FLength         := 5;
    FOutflowPenaltyType.FDecimal        := 0;
    FOutflowPenaltyType.FDefaultPadding := True;

    FChannelType.FInitalised     := False;
    FChannelType.FData           := 0;
    FChannelType.FLength         := 5;
    FChannelType.FDecimal        := 0;
    FChannelType.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandChannelObject.Reset;
const OPNAME = 'TWetlandChannelObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandCentreObject }

procedure TDemandCentreObject.CreateMemberObjects;
const OPNAME = 'TDemandCentreObject.CreateMemberObjects';
begin
  try
    FIdentifier                   := TInteger.Create;
    FConsumptiveChannelNr         := TInteger.Create;
    FConsumptiveChannelName       := TString.Create;
    FNodeNumber                   := TInteger.Create;
    FNoOfReturnFlowChannels       := TInteger.Create;
    FNoOfReclaimationChannels     := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandCentreObject.DestroyMemberObjects;
const OPNAME = 'TDemandCentreObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FNodeNumber.Free;
    FConsumptiveChannelNr.Free;
    FConsumptiveChannelName.Free;
    FNoOfReturnFlowChannels.Free;
    FNoOfReclaimationChannels.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandCentreObject.Initialise: Boolean;
const OPNAME = 'TDemandCentreObject.Initialise';
begin
  Result := False;
  try
    FIdentifier.FInitalised     := False;
    FIdentifier.FData           := 0;
    FIdentifier.FLength         := 5;
    FIdentifier.FDecimal        := 0;
    FIdentifier.FDefaultPadding := True;

    FNodeNumber.FInitalised     := False;
    FNodeNumber.FData           := 0;
    FNodeNumber.FLength         := 5;
    FNodeNumber.FDecimal        := 0;
    FNodeNumber.FDefaultPadding := True;

    FConsumptiveChannelNr.FInitalised     := False;
    FConsumptiveChannelNr.FData           := 0;
    FConsumptiveChannelNr.FLength         := 5;
    FConsumptiveChannelNr.FDecimal        := 0;
    FConsumptiveChannelNr.FDefaultPadding := True;

    FConsumptiveChannelName.FInitalised := False;
    FConsumptiveChannelName.FData := '';
    FConsumptiveChannelName.FLength := 0;
    FConsumptiveChannelName.FDecimal := 0;
    FConsumptiveChannelName.FDefaultPadding := True;

    FNoOfReturnFlowChannels.FInitalised     := False;
    FNoOfReturnFlowChannels.FData           := 0;
    FNoOfReturnFlowChannels.FLength         := 5;
    FNoOfReturnFlowChannels.FDecimal        := 0;
    FNoOfReturnFlowChannels.FDefaultPadding := True;

    FNoOfReclaimationChannels.FInitalised     := False;
    FNoOfReclaimationChannels.FData           := 0;
    FNoOfReclaimationChannels.FLength         := 5;
    FNoOfReclaimationChannels.FDecimal        := 0;
    FNoOfReclaimationChannels.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandCentreObject.Reset;
const OPNAME = 'TDemandCentreObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReturnFlowChannelObject }

procedure TReturnFlowChannelObject.CreateMemberObjects;
const OPNAME = 'TReturnFlowChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier           := TInteger.Create;
    FDemandCentreNodeNr   := TInteger.CReate;
    FChannelNr            := TInteger.Create;
    FChannelName          := TString.Create;
    FDownStreamNodeNr     := TInteger.Create;
    FUpstreamNodeNr       := TInteger.Create;
    FPenaltyStructureType := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReturnFlowChannelObject.DestroyMemberObjects;
const OPNAME = 'TReturnFlowChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FDemandCentreNodeNr.Free;
    FChannelNr.Free;
    FChannelName.Free;
    FDownStreamNodeNr.Free;
    FUpstreamNodeNr.Free;
    FPenaltyStructureType.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReturnFlowChannelObject.Initialise: Boolean;
const OPNAME = 'TReturnFlowChannelObject.Initialise';
begin
  Result := False;
  try
    FIdentifier.FInitalised     := False;
    FIdentifier.FData           := 0;
    FIdentifier.FLength         := 5;
    FIdentifier.FDecimal        := 0;
    FIdentifier.FDefaultPadding := True;

    FDemandCentreNodeNr.FInitalised     := False;
    FDemandCentreNodeNr.FData           := 0;
    FDemandCentreNodeNr.FLength         := 5;
    FDemandCentreNodeNr.FDecimal        := 0;
    FDemandCentreNodeNr.FDefaultPadding := True;

    FChannelNr.FInitalised     := False;
    FChannelNr.FData           := 0;
    FChannelNr.FLength         := 5;
    FChannelNr.FDecimal        := 0;
    FChannelNr.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FDownStreamNodeNr.FInitalised     := False;
    FDownStreamNodeNr.FData           := 0;
    FDownStreamNodeNr.FLength         := 5;
    FDownStreamNodeNr.FDecimal        := 0;
    FDownStreamNodeNr.FDefaultPadding := True;

    FUpstreamNodeNr.FInitalised     := False;
    FUpstreamNodeNr.FData           := 0;
    FUpstreamNodeNr.FLength         := 5;
    FUpstreamNodeNr.FDecimal        := 0;
    FUpstreamNodeNr.FDefaultPadding := True;

    FPenaltyStructureType.FInitalised     := False;
    FPenaltyStructureType.FData           := 0;
    FPenaltyStructureType.FLength         := 5;
    FPenaltyStructureType.FDecimal        := 0;
    FPenaltyStructureType.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReturnFlowChannelObject.Reset;
const OPNAME = 'TReturnFlowChannelObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReclaimationChannelObject }

procedure TReclaimationChannelObject.CreateMemberObjects;
const OPNAME = 'TReclaimationChannelObject.CreateMemberObjects';
begin
  try
    FIdentifier           := TInteger.Create;
    FDemandCentreNodeNr   := TInteger.CReate;
    FChannelNr            := TInteger.Create;
    FChannelName          := TString.Create;
    FDownStreamNodeNr     := TInteger.Create;
    FUpstreamNodeNr       := TInteger.Create;
    FPenaltyStructureType := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReclaimationChannelObject.DestroyMemberObjects;
const OPNAME = 'TReclaimationChannelObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FDemandCentreNodeNr.Free;
    FChannelNr.Free;
    FChannelName.Free;
    FDownStreamNodeNr.Free;
    FUpstreamNodeNr.Free;
    FPenaltyStructureType.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReclaimationChannelObject.Initialise: Boolean;
const OPNAME = 'TReclaimationChannelObject.Initialise';
begin
  Result := False;
  try
    FIdentifier.FInitalised     := False;
    FIdentifier.FData           := 0;
    FIdentifier.FLength         := 5;
    FIdentifier.FDecimal        := 0;
    FIdentifier.FDefaultPadding := True;

    FDemandCentreNodeNr.FInitalised     := False;
    FDemandCentreNodeNr.FData           := 0;
    FDemandCentreNodeNr.FLength         := 5;
    FDemandCentreNodeNr.FDecimal        := 0;
    FDemandCentreNodeNr.FDefaultPadding := True;

    FChannelNr.FInitalised     := False;
    FChannelNr.FData           := 0;
    FChannelNr.FLength         := 5;
    FChannelNr.FDecimal        := 0;
    FChannelNr.FDefaultPadding := True;

    FChannelName.FInitalised := False;
    FChannelName.FData := '';
    FChannelName.FLength := 0;
    FChannelName.FDecimal := 0;
    FChannelName.FDefaultPadding := True;

    FDownStreamNodeNr.FInitalised     := False;
    FDownStreamNodeNr.FData           := 0;
    FDownStreamNodeNr.FLength         := 5;
    FDownStreamNodeNr.FDecimal        := 0;
    FDownStreamNodeNr.FDefaultPadding := True;

    FUpstreamNodeNr.FInitalised     := False;
    FUpstreamNodeNr.FData           := 0;
    FUpstreamNodeNr.FLength         := 5;
    FUpstreamNodeNr.FDecimal        := 0;
    FUpstreamNodeNr.FDefaultPadding := True;

    FPenaltyStructureType.FInitalised     := False;
    FPenaltyStructureType.FData           := 0;
    FPenaltyStructureType.FLength         := 5;
    FPenaltyStructureType.FDecimal        := 0;
    FPenaltyStructureType.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReclaimationChannelObject.Reset;
const OPNAME = 'TReclaimationChannelObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGroundWaterObject }

procedure TGroundWaterObject.CreateMemberObjects;
const OPNAME = 'TGroundWaterObject.CreateMemberObjects';
begin
  try
    FIdentifier                             := TInteger.Create;
    FReferenceNodeNr                        := TInteger.Create;
    FAquiferNodeNumber                      := TInteger.Create;
    FAbstractionNodeNumber                  := TInteger.Create;
    FCollectionNodeNumber                   := TInteger.Create;
    FBaseflowNodeNumber                     := TInteger.Create;

    FUpNodeNumber                           := TInteger.Create;
    FDownNodeNumber                         := TInteger.Create;

    FAquiferInflowChannelNr                 := TInteger.Create;
    FAquiferExcessInterflowChannelNr        := TInteger.Create;
    FGroundWaterBaseflowChannelNr           := TInteger.Create;
    FAbstractionFromAquiferChannelNr        := TInteger.Create;
    FAbstractionFromBaseflowChannelNr       := TInteger.Create;
    FInflowFromUpStreamAquiferChannelNr     := TInteger.Create;
    FSurfaceRunoffAndSoilInterflowChannelNr := TInteger.Create;
    FGroundWaterBaseFlowRemainderChannelNr  := TInteger.Create;

    FAquiferInflowChannelName                 := TString.Create;
    FAquiferExcessInterflowChannelName        := TString.Create;
    FGroundWaterBaseflowChannelName           := TString.Create;
    FAbstractionFromAquiferChannelName        := TString.Create;
    FAbstractionFromBaseflowChannelName       := TString.Create;
    FInflowFromUpStreamAquiferChannelName     := TString.Create;
    FSurfaceRunoffAndSoilInterflowChannelName := TString.Create;
    FGroundWaterBaseFlowRemainderChannelName  := TString.Create;

    FAquiferInflowPenaltyType                 := TInteger.Create;
    FAquiferExcessInterflowPenaltyType        := TInteger.Create;
    FGroundWaterBaseflowPenaltyType           := TInteger.Create;
    FAbstractionFromAquiferPenaltyType        := TInteger.Create;
    FAbstractionFromBaseflowPenaltyType       := TInteger.Create;
    FInflowFromUpStreamAquiferPenaltyType     := TInteger.Create;
    FInflowFromUpStreamAquiferAquiferNumber   := TInteger.Create;
    FSurfaceRunoffAndSoilInterflowPenaltyType := TInteger.Create;
    FGroundWaterBaseFlowRemainderPenaltyType  := TInteger.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterObject.DestroyMemberObjects;
const OPNAME = 'TGroundWaterObject.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FReferenceNodeNr.Free;
    FAquiferNodeNumber.Free;
    FAbstractionNodeNumber.Free;
    FCollectionNodeNumber.Free;
    FBaseflowNodeNumber.Free;

    FUpNodeNumber.Free;
    FDownNodeNumber.Free;

    FAquiferInflowChannelName.Free;
    FAquiferExcessInterflowChannelName.Free;
    FGroundWaterBaseflowChannelName.Free;
    FAbstractionFromAquiferChannelName.Free;
    FAbstractionFromBaseflowChannelName.Free;
    FInflowFromUpStreamAquiferChannelName.Free;
    FSurfaceRunoffAndSoilInterflowChannelName.Free;
    FGroundWaterBaseFlowRemainderChannelName.Free;

    FAquiferInflowChannelNr.Free;
    FAquiferInflowPenaltyType.Free;
    FAquiferExcessInterflowChannelNr.Free;
    FAquiferExcessInterflowPenaltyType.Free;
    FGroundWaterBaseflowChannelNr.Free;
    FGroundWaterBaseflowPenaltyType.Free;
    FAbstractionFromAquiferChannelNr.Free;
    FAbstractionFromAquiferPenaltyType.Free;
    FAbstractionFromBaseflowChannelNr.Free;
    FAbstractionFromBaseflowPenaltyType.Free;
    FInflowFromUpStreamAquiferChannelNr.Free;
    FInflowFromUpstreamAquiferPenaltyType.Free;
    FInflowFromUpStreamAquiferAquiferNumber.Free;
    FSurfaceRunoffAndSoilInterflowChannelNr.Free;
    FSurfaceRunoffAndSoilInterflowPenaltyType.Free;
    FGroundWaterBaseFlowRemainderChannelNr.Free;
    FGroundWaterBaseFlowRemainderPenaltyType.Free;

 except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterObject.Initialise: Boolean;
const OPNAME = 'TGroundWaterObject.Initialise';
begin
  Result := False;
  try
    FIdentifier.FInitalised     := False;
    FIdentifier.FData           := 0;
    FIdentifier.FLength         := 5;
    FIdentifier.FDecimal        := 0;
    FIdentifier.FDefaultPadding := True;

    FReferenceNodeNr.FInitalised     := False;
    FReferenceNodeNr.FData           := 0;
    FReferenceNodeNr.FLength         := 5;
    FReferenceNodeNr.FDecimal        := 0;
    FReferenceNodeNr.FDefaultPadding := True;

    FAquiferNodeNumber.FInitalised     := False;
    FAquiferNodeNumber.FData           := 0;
    FAquiferNodeNumber.FLength         := 5;
    FAquiferNodeNumber.FDecimal        := 0;
    FAquiferNodeNumber.FDefaultPadding := True;

    FAbstractionNodeNumber.FInitalised     := False;
    FAbstractionNodeNumber.FData           := 0;
    FAbstractionNodeNumber.FLength         := 5;
    FAbstractionNodeNumber.FDecimal        := 0;
    FAbstractionNodeNumber.FDefaultPadding := True;

    FCollectionNodeNumber.FInitalised     := False;
    FCollectionNodeNumber.FData           := 0;
    FCollectionNodeNumber.FLength         := 5;
    FCollectionNodeNumber.FDecimal        := 0;
    FCollectionNodeNumber.FDefaultPadding := True;

    FBaseflowNodeNumber.FInitalised     := False;
    FBaseflowNodeNumber.FData           := 0;
    FBaseflowNodeNumber.FLength         := 5;
    FBaseflowNodeNumber.FDecimal        := 0;
    FBaseflowNodeNumber.FDefaultPadding := True;

    FUpNodeNumber.FInitalised     := False;
    FUpNodeNumber.FData           := 0;
    FUpNodeNumber.FLength         := 5;
    FUpNodeNumber.FDecimal        := 0;
    FUpNodeNumber.FDefaultPadding := True;

    FDownNodeNumber.FInitalised     := False;
    FDownNodeNumber.FData           := 0;
    FDownNodeNumber.FLength         := 5;
    FDownNodeNumber.FDecimal        := 0;
    FDownNodeNumber.FDefaultPadding := True;
           
    FAquiferInflowChannelNr.FInitalised     := False;
    FAquiferInflowChannelNr.FData           := 0;
    FAquiferInflowChannelNr.FLength         := 5;
    FAquiferInflowChannelNr.FDecimal        := 0;
    FAquiferInflowChannelNr.FDefaultPadding := True;

    FAquiferInflowChannelName.FInitalised := False;
    FAquiferInflowChannelName.FData := '';
    FAquiferInflowChannelName.FLength := 0;
    FAquiferInflowChannelName.FDecimal := 0;
    FAquiferInflowChannelName.FDefaultPadding := True;

    FAquiferInflowPenaltyType.FInitalised     := False;
    FAquiferInflowPenaltyType.FData           := 0;
    FAquiferInflowPenaltyType.FLength         := 5;
    FAquiferInflowPenaltyType.FDecimal        := 0;
    FAquiferInflowPenaltyType.FDefaultPadding := True;

    FAquiferExcessInterflowChannelNr.FInitalised     := False;
    FAquiferExcessInterflowChannelNr.FData           := 0;
    FAquiferExcessInterflowChannelNr.FLength         := 5;
    FAquiferExcessInterflowChannelNr.FDecimal        := 0;
    FAquiferExcessInterflowChannelNr.FDefaultPadding := True;

    FAquiferExcessInterflowChannelName.FInitalised := False;
    FAquiferExcessInterflowChannelName.FData := '';
    FAquiferExcessInterflowChannelName.FLength := 0;
    FAquiferExcessInterflowChannelName.FDecimal := 0;
    FAquiferExcessInterflowChannelName.FDefaultPadding := True;

    FAquiferExcessInterflowPenaltyType.FInitalised     := False;
    FAquiferExcessInterflowPenaltyType.FData           := 0;
    FAquiferExcessInterflowPenaltyType.FLength         := 5;
    FAquiferExcessInterflowPenaltyType.FDecimal        := 0;
    FAquiferExcessInterflowPenaltyType.FDefaultPadding := True;

    FGroundWaterBaseflowChannelNr.FInitalised     := False;
    FGroundWaterBaseflowChannelNr.FData           := 0;
    FGroundWaterBaseflowChannelNr.FLength         := 5;
    FGroundWaterBaseflowChannelNr.FDecimal        := 0;
    FGroundWaterBaseflowChannelNr.FDefaultPadding := True;

    FGroundWaterBaseflowPenaltyType.FInitalised     := False;
    FGroundWaterBaseflowPenaltyType.FData           := 0;
    FGroundWaterBaseflowPenaltyType.FLength         := 5;
    FGroundWaterBaseflowPenaltyType.FDecimal        := 0;
    FGroundWaterBaseflowPenaltyType.FDefaultPadding := True;

    FAbstractionFromAquiferChannelNr.FInitalised     := False;
    FAbstractionFromAquiferChannelNr.FData           := 0;
    FAbstractionFromAquiferChannelNr.FLength         := 5;
    FAbstractionFromAquiferChannelNr.FDecimal        := 0;
    FAbstractionFromAquiferChannelNr.FDefaultPadding := True;

    FAbstractionFromAquiferChannelName.FInitalised := False;
    FAbstractionFromAquiferChannelName.FData := '';
    FAbstractionFromAquiferChannelName.FLength := 0;
    FAbstractionFromAquiferChannelName.FDecimal := 0;
    FAbstractionFromAquiferChannelName.FDefaultPadding := True;

    FAbstractionFromAquiferPenaltyType.FInitalised     := False;
    FAbstractionFromAquiferPenaltyType.FData           := 0;
    FAbstractionFromAquiferPenaltyType.FLength         := 5;
    FAbstractionFromAquiferPenaltyType.FDecimal        := 0;
    FAbstractionFromAquiferPenaltyType.FDefaultPadding := True;

    FAbstractionFromBaseflowChannelNr.FInitalised     := False;
    FAbstractionFromBaseflowChannelNr.FData           := 0;
    FAbstractionFromBaseflowChannelNr.FLength         := 5;
    FAbstractionFromBaseflowChannelNr.FDecimal        := 0;
    FAbstractionFromBaseflowChannelNr.FDefaultPadding := True;

    FAbstractionFromBaseflowChannelName.FInitalised := False;
    FAbstractionFromBaseflowChannelName.FData := '';
    FAbstractionFromBaseflowChannelName.FLength := 0;
    FAbstractionFromBaseflowChannelName.FDecimal := 0;
    FAbstractionFromBaseflowChannelName.FDefaultPadding := True;

    FAbstractionFromBaseflowPenaltyType.FInitalised     := False;
    FAbstractionFromBaseflowPenaltyType.FData           := 0;
    FAbstractionFromBaseflowPenaltyType.FLength         := 5;
    FAbstractionFromBaseflowPenaltyType.FDecimal        := 0;
    FAbstractionFromBaseflowPenaltyType.FDefaultPadding := True;

    FInflowFromUpstreamAquiferChannelNr.FInitalised     := False;
    FInflowFromUpstreamAquiferChannelNr.FData           := 0;
    FInflowFromUpstreamAquiferChannelNr.FLength         := 5;
    FInflowFromUpstreamAquiferChannelNr.FDecimal        := 0;
    FInflowFromUpstreamAquiferChannelNr.FDefaultPadding := True;

    FInflowFromUpStreamAquiferChannelName.FInitalised := False;
    FInflowFromUpStreamAquiferChannelName.FData := '';
    FInflowFromUpStreamAquiferChannelName.FLength := 0;
    FInflowFromUpStreamAquiferChannelName.FDecimal := 0;
    FInflowFromUpStreamAquiferChannelName.FDefaultPadding := True;

    FInflowFromUpstreamAquiferPenaltyType.FInitalised     := False;
    FInflowFromUpstreamAquiferPenaltyType.FData           := 0;
    FInflowFromUpstreamAquiferPenaltyType.FLength         := 5;
    FInflowFromUpstreamAquiferPenaltyType.FDecimal        := 0;
    FInflowFromUpstreamAquiferPenaltyType.FDefaultPadding := True;

    FInflowFromUpStreamAquiferAquiferNumber.FInitalised     := False;
    FInflowFromUpStreamAquiferAquiferNumber.FData           := 0;
    FInflowFromUpStreamAquiferAquiferNumber.FLength         := 5;
    FInflowFromUpStreamAquiferAquiferNumber.FDecimal        := 0;
    FInflowFromUpStreamAquiferAquiferNumber.FDefaultPadding := True;

    FSurfaceRunoffAndSoilInterflowChannelNr.FInitalised     := False;
    FSurfaceRunoffAndSoilInterflowChannelNr.FData           := 0;
    FSurfaceRunoffAndSoilInterflowChannelNr.FLength         := 5;
    FSurfaceRunoffAndSoilInterflowChannelNr.FDecimal        := 0;
    FSurfaceRunoffAndSoilInterflowChannelNr.FDefaultPadding := True;

    FSurfaceRunoffAndSoilInterflowChannelName.FInitalised := False;
    FSurfaceRunoffAndSoilInterflowChannelName.FData := '';
    FSurfaceRunoffAndSoilInterflowChannelName.FLength := 0;
    FSurfaceRunoffAndSoilInterflowChannelName.FDecimal := 0;
    FSurfaceRunoffAndSoilInterflowChannelName.FDefaultPadding := True;

    FSurfaceRunoffAndSoilInterflowPenaltyType.FInitalised     := False;
    FSurfaceRunoffAndSoilInterflowPenaltyType.FData           := 0;
    FSurfaceRunoffAndSoilInterflowPenaltyType.FLength         := 5;
    FSurfaceRunoffAndSoilInterflowPenaltyType.FDecimal        := 0;
    FSurfaceRunoffAndSoilInterflowPenaltyType.FDefaultPadding := True;

    FGroundWaterBaseFlowRemainderChannelNr.FInitalised     := False;
    FGroundWaterBaseFlowRemainderChannelNr.FData           := 0;
    FGroundWaterBaseFlowRemainderChannelNr.FLength         := 5;
    FGroundWaterBaseFlowRemainderChannelNr.FDecimal        := 0;
    FGroundWaterBaseFlowRemainderChannelNr.FDefaultPadding := True;

    FGroundWaterBaseFlowRemainderChannelName.FInitalised := False;
    FGroundWaterBaseFlowRemainderChannelName.FData := '';
    FGroundWaterBaseFlowRemainderChannelName.FLength := 0;
    FGroundWaterBaseFlowRemainderChannelName.FDecimal := 0;
    FGroundWaterBaseFlowRemainderChannelName.FDefaultPadding := True;

    FGroundWaterBaseFlowRemainderPenaltyType.FInitalised     := False;
    FGroundWaterBaseFlowRemainderPenaltyType.FData           := 0;
    FGroundWaterBaseFlowRemainderPenaltyType.FLength         := 5;
    FGroundWaterBaseFlowRemainderPenaltyType.FDecimal        := 0;
    FGroundWaterBaseFlowRemainderPenaltyType.FDefaultPadding := True;

    FGroundWaterBaseflowChannelName.FInitalised := False;
    FGroundWaterBaseflowChannelName.FData := '';
    FGroundWaterBaseflowChannelName.FLength := 0;
    FGroundWaterBaseflowChannelName.FDecimal := 0;
    FGroundWaterBaseflowChannelName.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterObject.Reset;
const OPNAME = 'TGroundWaterObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
