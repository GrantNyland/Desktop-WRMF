//
//
//  UNIT      : Contains TMinesFileObject Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 12/03/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UMineFileObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TMonthlyDouble  = array[MinMonths..MaxMonths] of TDouble;

  TMineOpenCastFileObject = class(TAbstractDataObject)
  protected
    FPitName                     : TString;
    FCoalReserveArea             : TDouble;
    FWorkingsArea                : TDouble;
    FDisturbedWorkingsArea       : TDouble;
    FDisturbedArea               : TDouble;
    FWaterSurfaceEvapArea        : TDouble;
    FDisturbedAreaRunOff         : TDouble;
    FDisturbedWorkingsAreaRunOff : TDouble;
    FDecantVolume                : TDouble;
    FSeepageVolume               : TDouble;
    FAnalysisStartVolume         : TDouble;
    FMaximumSeepageRate          : TDouble;
    FSeepageExponent             : TDouble;
    FPCDSurfaceArea              : TDouble;
    FPCDStorageCapacity          : TDouble;
    FPCDAnalysisStartVolume      : TDouble;
    FDisturbedRechargeFactor     : TMonthlyDouble;
    FWorkingAreaRechargeFactor   : TMonthlyDouble;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property PitName                     : TString  read FPitName                     write FPitName;
    property CoalReserveArea             : TDouble  read FCoalReserveArea             write FCoalReserveArea            ;
    property WorkingsArea                : TDouble  read FWorkingsArea                write FWorkingsArea               ;
    property DisturbedWorkingsArea       : TDouble  read FDisturbedWorkingsArea       write FDisturbedWorkingsArea      ;
    property DisturbedArea               : TDouble  read FDisturbedArea               write FDisturbedArea              ;
    property WaterSurfaceEvapArea        : TDouble  read FWaterSurfaceEvapArea        write FWaterSurfaceEvapArea       ;
    property DisturbedAreaRunOff         : TDouble  read FDisturbedAreaRunOff         write FDisturbedAreaRunOff        ;
    property DisturbedWorkingsAreaRunOff : TDouble  read FDisturbedWorkingsAreaRunOff write FDisturbedWorkingsAreaRunOff;
    property DecantVolume                : TDouble  read FDecantVolume                write FDecantVolume               ;
    property SeepageVolume               : TDouble  read FSeepageVolume               write FSeepageVolume              ;
    property AnalysisStartVolume         : TDouble  read FAnalysisStartVolume         write FAnalysisStartVolume        ;
    property MaximumSeepageRate          : TDouble  read FMaximumSeepageRate          write FMaximumSeepageRate         ;
    property SeepageExponent             : TDouble  read FSeepageExponent             write FSeepageExponent            ;
    property PCDSurfaceArea              : TDouble  read FPCDSurfaceArea              write FPCDSurfaceArea             ;
    property PCDStorageCapacity          : TDouble  read FPCDStorageCapacity          write FPCDStorageCapacity         ;
    property PCDAnalysisStartVolume      : TDouble  read FPCDAnalysisStartVolume      write FPCDAnalysisStartVolume     ;
    property DisturbedRechargeFactor     : TMonthlyDouble read FDisturbedRechargeFactor   write FDisturbedRechargeFactor;
    property WorkingAreaRechargeFactor   : TMonthlyDouble read FWorkingAreaRechargeFactor write FWorkingAreaRechargeFactor;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMineUndegroundFileObject = class(TAbstractDataObject)
  protected
    FUnderGroundSectionName         : TString;
    FChannelNumberToUGDam           : TInteger;
    FUpstreamCatchmentArea          : TDouble;
    FBoardPillarCatchmentArea       : TDouble;
    FHighExtractionCatchmentArea    : TDouble;
    FHighExtractionAreaRunoffFactor : TDouble;
    FUpstreamRunoffPortion          : TMonthlyDouble;
    FBoardAndPilarRechargeFactor    : TMonthlyDouble;
    FHighExtractionRechargeFactor   : TMonthlyDouble;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property UnderGroundSectionName         : TString   read FUnderGroundSectionName       write FUnderGroundSectionName;
    property ChannelNumberToUGDam           : TInteger  read FChannelNumberToUGDam         write FChannelNumberToUGDam;
    property UpstreamCatchmentArea          : TDouble   read FUpstreamCatchmentArea        write FUpstreamCatchmentArea;
    property BoardPillarCatchmentArea       : TDouble   read FBoardPillarCatchmentArea     write FBoardPillarCatchmentArea;
    property HighExtractionCatchmentArea    : TDouble   read FHighExtractionCatchmentArea  write FHighExtractionCatchmentArea;
    property HighExtractionAreaRunoffFactor : TDouble   read FHighExtractionAreaRunoffFactor     write FHighExtractionAreaRunoffFactor;
    property UpstreamRunoffPortion          : TMonthlyDouble read FUpstreamRunoffPortion         write FUpstreamRunoffPortion;
    property BoardAndPilarRechargeFactor    : TMonthlyDouble read FBoardAndPilarRechargeFactor   write FBoardAndPilarRechargeFactor;
    property HighExtractionRechargeFactor   : TMonthlyDouble read FHighExtractionRechargeFactor  write FHighExtractionRechargeFactor;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMineSlurryDumpFileObject = class(TAbstractDataObject)
  protected
    FDumpName                : TString;
    FDumpSurfaceArea         : TDouble;
    FRunoffFactorToPCD       : TDouble;
    FSeepageSplitFactor      : TDouble;
    FPCDStorageCapacity      : TDouble;
    FPCDSurfaceArea          : TDouble;
    FPCDAnalysisStartVolume  : TDouble;
    FRechargeFactor          : TMonthlyDouble;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property DumpName                : TString   read FDumpName               write FDumpName;
    property DumpSurfaceArea         : TDouble   read FDumpSurfaceArea        write FDumpSurfaceArea;
    property RunoffFactorToPCD       : TDouble   read FRunoffFactorToPCD      write FRunoffFactorToPCD;
    property SeepageSplitFactor      : TDouble   read FSeepageSplitFactor     write FSeepageSplitFactor;
    property PCDStorageCapacity      : TDouble   read FPCDStorageCapacity     write FPCDStorageCapacity;
    property PCDSurfaceArea          : TDouble   read FPCDSurfaceArea         write FPCDSurfaceArea;
    property PCDAnalysisStartVolume  : TDouble    read FPCDAnalysisStartVolume write FPCDAnalysisStartVolume;
    property RechargeFactor          : TMonthlyDouble read FRechargeFactor     write FRechargeFactor;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMineSubCatchmentFileObject = class(TAbstractDataObject)
  protected
    FCatchmentRefNumber          : TInteger;
    FProportionAntecedentFlow    : TDouble;
    FGroundwaterFlowVolume       : TDouble;
    FAntecedentRunoffDecayFactor : TDouble;
    FCatchmentRefUsed            : TString;
    FMineSubCatchmentFlowVolume  : TMonthlyDouble;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;

    property CatchmentRefNumber          : TInteger       read FCatchmentRefNumber          write FCatchmentRefNumber;
    property ProportionAntecedentFlow    : TDouble        read FProportionAntecedentFlow    write FProportionAntecedentFlow;
    property GroundwaterFlowVolume       : TDouble        read FGroundwaterFlowVolume       write FGroundwaterFlowVolume;
    property AntecedentRunoffDecayFactor : TDouble        read FAntecedentRunoffDecayFactor write FAntecedentRunoffDecayFactor;
    property CatchmentRefUsed            : TString        read FCatchmentRefUsed            write FCatchmentRefUsed;
    property MineSubCatchmentFlowVolume  : TMonthlyDouble read FMineSubCatchmentFlowVolume  write FMineSubCatchmentFlowVolume;
  end;

  TMineSubCatchmentListFileObject  = class(TAbstractDataObject)
  protected
    FMineSubCatchmentFileContainer : TObjectList;
    FComment: TStringlist;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMineSubCatchmentListByIndex(AIndex: integer):TMineSubCatchmentFileObject;
  public
    procedure Reset;override;
    function MineSubCatchmentObjectCount: integer;virtual;
    function Initialise: boolean;override;
    function AddMineSubCatchmentFileObject:TMineSubCatchmentFileObject;

    property MineSubCatchmentObjectByIndex[AIndex: integer] :TMineSubCatchmentFileObject read GetMineSubCatchmentListByIndex;
    property Comment :TStringlist read FComment;
  end;

  TMineFileObject = class(TAbstractDataObject)
  protected
    FOpenCastObjectContainer   : TObjectList;
    FUnderGroundObjectContainer: TObjectList;
    FSlurryDumpObjectContainer : TObjectList;

    FNodeNumber                : TInteger;
    FMineName                  : TString;
    FRiverChannelNumber        : TInteger;
    FPCDChannelNumber          : TInteger;
    FHydrologyNodeNumber       : TInteger;
    FBeneficiationPlantArea    : TDouble;
    FBeneficiationRunOffFactor : TDouble;
    FPanEvaporation            : TMonthlyDouble;
    FLakeEvaporation           : TMonthlyDouble;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetOpenCastByIndex(AIndex: integer): TMineOpenCastFileObject;
    function GetSlurryDumpByIndex(AIndex: integer): TMineSlurryDumpFileObject;
    function GetUndegroundByIndex(AIndex: integer): TMineUndegroundFileObject;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function OpenCastCount: integer;virtual;
    function AddOpenCast:TMineOpenCastFileObject;
    function UndegroundCount: integer;virtual;
    function AddUndeground:TMineUndegroundFileObject;
    function SlurryDumpCount: integer;virtual;
    function AddSlurryDump:TMineSlurryDumpFileObject;

    property NodeNumber                : TInteger read FNodeNumber                write FNodeNumber;
    property MineName                  : TString  read FMineName                  write FMineName;
    property RiverChannelNumber        : TInteger read FRiverChannelNumber        write FRiverChannelNumber;
    property PCDChannelNumber          : TInteger read FPCDChannelNumber          write FPCDChannelNumber;
    property HydrologyNodeNumber       : TInteger read FHydrologyNodeNumber       write FHydrologyNodeNumber;
    property BeneficiationPlantArea    : TDouble  read FBeneficiationPlantArea    write FBeneficiationPlantArea;
    property BeneficiationRunOffFactor : TDouble  read FBeneficiationRunOffFactor write FBeneficiationRunOffFactor;
    property PanEvaporation            : TMonthlyDouble  read FPanEvaporation  write FPanEvaporation;
    property LakeEvaporation           : TMonthlyDouble  read FLakeEvaporation write FLakeEvaporation;

    property OpenCastByIndex[AIndex: integer] :TMineOpenCastFileObject read GetOpenCastByIndex;
    property UndegroundByIndex[AIndex: integer] :TMineUndegroundFileObject read GetUndegroundByIndex;
    property SlurryDumpByIndex[AIndex: integer] :TMineSlurryDumpFileObject read GetSlurryDumpByIndex;
  end;

  TMineListFileObject  = class(TAbstractDataObject)
  protected
    FMineFileObjectContainer : TObjectList;
    FComment: TStringlist;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMineFileObjectByIndex(AIndex: integer):TMineFileObject;
  public
    procedure Reset;override;
    function MineFileObjectCount: integer;virtual;
    function Initialise: boolean;override;
    function AddMineFileObject:TMineFileObject;
    property MineFileObjectByIndex[AIndex: integer] :TMineFileObject read GetMineFileObjectByIndex;
    property Comment :TStringlist read FComment;
  end;

implementation


uses UErrorHandlingOperations;

{ TMineOpenCastFileObject }

procedure TMineOpenCastFileObject.CreateMemberObjects;
const OPNAME = 'TMineOpenCastFileObject.CreateMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FPitName                     := TString.Create;
    FCoalReserveArea             := TDouble.Create;
    FWorkingsArea                := TDouble.Create;
    FDisturbedWorkingsArea       := TDouble.Create;
    FDisturbedArea               := TDouble.Create;
    FWaterSurfaceEvapArea        := TDouble.Create;
    FDisturbedAreaRunOff         := TDouble.Create;
    FDisturbedWorkingsAreaRunOff := TDouble.Create;
    FDecantVolume                := TDouble.Create;
    FSeepageVolume               := TDouble.Create;
    FAnalysisStartVolume         := TDouble.Create;
    FMaximumSeepageRate          := TDouble.Create;
    FSeepageExponent             := TDouble.Create;
    FPCDSurfaceArea              := TDouble.Create;
    FPCDStorageCapacity          := TDouble.Create;
    FPCDAnalysisStartVolume      := TDouble.Create;
    for LIndex := Low(FWorkingAreaRechargeFactor) to High(FWorkingAreaRechargeFactor) do
    begin
      FWorkingAreaRechargeFactor[LIndex] := TDouble.Create;
    end;
    for LIndex := Low(FDisturbedRechargeFactor) to High(FDisturbedRechargeFactor) do
    begin
      FDisturbedRechargeFactor[LIndex] := TDouble.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpenCastFileObject.DestroyMemberObjects;
const OPNAME = 'TMineOpenCastFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FPitName.Free;
    FCoalReserveArea.Free;
    FWorkingsArea.Free;
    FDisturbedWorkingsArea.Free;
    FDisturbedArea.Free;
    FWaterSurfaceEvapArea.Free;
    FDisturbedAreaRunOff.Free;
    FDisturbedWorkingsAreaRunOff.Free;
    FDecantVolume.Free;
    FSeepageVolume.Free;
    FAnalysisStartVolume.Free;
    FMaximumSeepageRate.Free;
    FSeepageExponent.Free;
    FPCDSurfaceArea.Free;
    FPCDStorageCapacity.Free;
    FPCDAnalysisStartVolume.Free;
    for LIndex := Low(FWorkingAreaRechargeFactor) to High(FWorkingAreaRechargeFactor) do
    begin
      FWorkingAreaRechargeFactor[LIndex].Free;
    end;
    for LIndex := Low(FDisturbedRechargeFactor) to High(FDisturbedRechargeFactor) do
    begin
      FDisturbedRechargeFactor[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpenCastFileObject.Reset;
const OPNAME = 'TMineOpenCastFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpenCastFileObject.Initialise: boolean;
const OPNAME = 'TMineOpenCastFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FPitName.FData := '';
    FPitName.FInitalised := False;
    FPitName.FLength := 255;
    FPitName.FDecimal := 0;
    FPitName.FDefaultPadding := True;

    FCoalReserveArea.FData := 0.0;
    FCoalReserveArea.FInitalised := False;
    FCoalReserveArea.FLength := 10;
    FCoalReserveArea.FDecimal := 2;
    FCoalReserveArea.FDefaultPadding := True;
    FCoalReserveArea.ShowDecimalPoint := True;

    FWorkingsArea.FData := 0.0;
    FWorkingsArea.FInitalised := False;
    FWorkingsArea.FLength := 10;
    FWorkingsArea.FDecimal := 2;
    FWorkingsArea.FDefaultPadding := True;
    FWorkingsArea.ShowDecimalPoint := True;

    FDisturbedWorkingsArea.FData := 0.0;
    FDisturbedWorkingsArea.FInitalised := False;
    FDisturbedWorkingsArea.FLength := 10;
    FDisturbedWorkingsArea.FDecimal := 2;
    FDisturbedWorkingsArea.FDefaultPadding := True;
    FDisturbedWorkingsArea.ShowDecimalPoint := True;

    FDisturbedArea.FData := 0.0;
    FDisturbedArea.FInitalised := False;
    FDisturbedArea.FLength := 10;
    FDisturbedArea.FDecimal := 2;
    FDisturbedArea.FDefaultPadding := True;
    FDisturbedArea.ShowDecimalPoint := True;

    FWaterSurfaceEvapArea.FData := 0.0;
    FWaterSurfaceEvapArea.FInitalised := False;
    FWaterSurfaceEvapArea.FLength := 10;
    FWaterSurfaceEvapArea.FDecimal := 2;
    FWaterSurfaceEvapArea.FDefaultPadding := True;
    FWaterSurfaceEvapArea.ShowDecimalPoint := True;

    FDisturbedAreaRunOff.FData := 0.0;
    FDisturbedAreaRunOff.FInitalised := False;
    FDisturbedAreaRunOff.FLength := 10;
    FDisturbedAreaRunOff.FDecimal := 3;
    FDisturbedAreaRunOff.FDefaultPadding := True;
    FDisturbedAreaRunOff.ShowDecimalPoint := True;

    FDisturbedWorkingsAreaRunOff.FData := 0.0;
    FDisturbedWorkingsAreaRunOff.FInitalised := False;
    FDisturbedWorkingsAreaRunOff.FLength := 10;
    FDisturbedWorkingsAreaRunOff.FDecimal := 3;
    FDisturbedWorkingsAreaRunOff.FDefaultPadding := True;
    FDisturbedWorkingsAreaRunOff.ShowDecimalPoint := True;

    FDecantVolume.FData := 0.0;
    FDecantVolume.FInitalised := False;
    FDecantVolume.FLength := 10;
    FDecantVolume.FDecimal := 2;
    FDecantVolume.FDefaultPadding := True;
    FDecantVolume.ShowDecimalPoint := True;

    FSeepageVolume.FData := 0.0;
    FSeepageVolume.FInitalised := False;
    FSeepageVolume.FLength := 10;
    FSeepageVolume.FDecimal := 2;
    FSeepageVolume.FDefaultPadding := True;
    FSeepageVolume.ShowDecimalPoint := True;

    FAnalysisStartVolume.FData := 0.0;
    FAnalysisStartVolume.FInitalised := False;
    FAnalysisStartVolume.FLength := 10;
    FAnalysisStartVolume.FDecimal := 2;
    FAnalysisStartVolume.FDefaultPadding := True;
    FAnalysisStartVolume.ShowDecimalPoint := True;

    FMaximumSeepageRate.FData := 0.0;
    FMaximumSeepageRate.FInitalised := False;
    FMaximumSeepageRate.FLength := 10;
    FMaximumSeepageRate.FDecimal := 2;
    FMaximumSeepageRate.FDefaultPadding := True;
    FMaximumSeepageRate.ShowDecimalPoint := True;

    FSeepageExponent.FData := 0.0;
    FSeepageExponent.FInitalised := False;
    FSeepageExponent.FLength := 10;
    FSeepageExponent.FDecimal := 2;
    FSeepageExponent.FDefaultPadding := True;
    FSeepageExponent.ShowDecimalPoint := True;

    FPCDSurfaceArea.FData := 0.0;
    FPCDSurfaceArea.FInitalised := False;
    FPCDSurfaceArea.FLength := 10;
    FPCDSurfaceArea.FDecimal := 2;
    FPCDSurfaceArea.FDefaultPadding := True;
    FPCDSurfaceArea.ShowDecimalPoint := True;

    FPCDStorageCapacity.FData := 0.0;
    FPCDStorageCapacity.FInitalised := False;
    FPCDStorageCapacity.FLength := 10;
    FPCDStorageCapacity.FDecimal := 2;
    FPCDStorageCapacity.FDefaultPadding := True;
    FPCDStorageCapacity.ShowDecimalPoint := True;

    FPCDAnalysisStartVolume.FData := 0.0;
    FPCDAnalysisStartVolume.FInitalised := False;
    FPCDAnalysisStartVolume.FLength := 10;
    FPCDAnalysisStartVolume.FDecimal := 2;
    FPCDAnalysisStartVolume.FDefaultPadding := True;
    FPCDAnalysisStartVolume.ShowDecimalPoint := True;

    for LIndex := Low(FWorkingAreaRechargeFactor) to High(FWorkingAreaRechargeFactor) do
    begin
      FWorkingAreaRechargeFactor[LIndex].FData := 0.0;
      FWorkingAreaRechargeFactor[LIndex].FInitalised := False;
      FWorkingAreaRechargeFactor[LIndex].FLength := 10;
      FWorkingAreaRechargeFactor[LIndex].FDecimal := 2;
      FWorkingAreaRechargeFactor[LIndex].FDefaultPadding := True;
      FWorkingAreaRechargeFactor[LIndex].ShowDecimalPoint := True;
    end;
    for LIndex := Low(FDisturbedRechargeFactor) to High(FDisturbedRechargeFactor) do
    begin
      FDisturbedRechargeFactor[LIndex].FData := 0.0;
      FDisturbedRechargeFactor[LIndex].FInitalised := False;
      FDisturbedRechargeFactor[LIndex].FLength := 10;
      FDisturbedRechargeFactor[LIndex].FDecimal := 2;
      FDisturbedRechargeFactor[LIndex].FDefaultPadding := True;
      FDisturbedRechargeFactor[LIndex].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMineUndegroundFileObject }

procedure TMineUndegroundFileObject.CreateMemberObjects;
const OPNAME = 'TMineUndegroundFileObject.CreateMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FUnderGroundSectionName         := TString.Create;
    FChannelNumberToUGDam           := TInteger.Create;
    FUpstreamCatchmentArea          := TDouble.Create;
    FBoardPillarCatchmentArea       := TDouble.Create;
    FHighExtractionCatchmentArea    := TDouble.Create;
    FHighExtractionAreaRunoffFactor := TDouble.Create;
    for LIndex := Low(FUpstreamRunoffPortion) to High(FUpstreamRunoffPortion) do
    begin
      FUpstreamRunoffPortion[LIndex] := TDouble.Create;
    end;
    for LIndex := Low(FBoardAndPilarRechargeFactor) to High(FBoardAndPilarRechargeFactor) do
    begin
      FBoardAndPilarRechargeFactor[LIndex] := TDouble.Create;
    end;
    for LIndex := Low(FHighExtractionRechargeFactor) to High(FHighExtractionRechargeFactor) do
    begin
      FHighExtractionRechargeFactor[LIndex] := TDouble.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndegroundFileObject.DestroyMemberObjects;
const OPNAME = 'TMineUndegroundFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FUnderGroundSectionName.Free;
    FChannelNumberToUGDam.Free;
    FUpstreamCatchmentArea.Free;
    FBoardPillarCatchmentArea.Free;
    FHighExtractionCatchmentArea.Free;
    FHighExtractionAreaRunoffFactor.Free;
    for LIndex := Low(FUpstreamRunoffPortion) to High(FUpstreamRunoffPortion) do
    begin
      FUpstreamRunoffPortion[LIndex].Free;
    end;
    for LIndex := Low(FBoardAndPilarRechargeFactor) to High(FBoardAndPilarRechargeFactor) do
    begin
      FBoardAndPilarRechargeFactor[LIndex].Free;
    end;
    for LIndex := Low(FHighExtractionRechargeFactor) to High(FHighExtractionRechargeFactor) do
    begin
      FHighExtractionRechargeFactor[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndegroundFileObject.Reset;
const OPNAME = 'TMineUndegroundFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndegroundFileObject.Initialise: boolean;
const OPNAME = 'TMineUndegroundFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FUnderGroundSectionName.FData := '';
    FUnderGroundSectionName.FInitalised := False;
    FUnderGroundSectionName.FLength := 255;
    FUnderGroundSectionName.FDecimal := 0;
    FUnderGroundSectionName.FDefaultPadding := True;

    FChannelNumberToUGDam.FData := 0;
    FChannelNumberToUGDam.FInitalised := False;
    FChannelNumberToUGDam.FLength := 4;
    FChannelNumberToUGDam.FDecimal := 0;
    FChannelNumberToUGDam.FDefaultPadding := True;

    FUpstreamCatchmentArea.FData := 0.0;
    FUpstreamCatchmentArea.FInitalised := False;
    FUpstreamCatchmentArea.FLength := 10;
    FUpstreamCatchmentArea.FDecimal := 2;
    FUpstreamCatchmentArea.FDefaultPadding := True;
    FUpstreamCatchmentArea.ShowDecimalPoint := True;

    FBoardPillarCatchmentArea.FData := 0.0;
    FBoardPillarCatchmentArea.FInitalised := False;
    FBoardPillarCatchmentArea.FLength := 10;
    FBoardPillarCatchmentArea.FDecimal := 2;
    FBoardPillarCatchmentArea.FDefaultPadding := True;
    FBoardPillarCatchmentArea.ShowDecimalPoint := True;

    FHighExtractionCatchmentArea.FData := 0.0;
    FHighExtractionCatchmentArea.FInitalised := False;
    FHighExtractionCatchmentArea.FLength := 10;
    FHighExtractionCatchmentArea.FDecimal := 2;
    FHighExtractionCatchmentArea.FDefaultPadding := True;
    FHighExtractionCatchmentArea.ShowDecimalPoint := True;

    FHighExtractionAreaRunoffFactor.FData := 0.0;
    FHighExtractionAreaRunoffFactor.FInitalised := False;
    FHighExtractionAreaRunoffFactor.FLength := 10;
    FHighExtractionAreaRunoffFactor.FDecimal := 3;
    FHighExtractionAreaRunoffFactor.FDefaultPadding := True;
    FHighExtractionAreaRunoffFactor.ShowDecimalPoint := True;

    for LIndex := Low(FUpstreamRunoffPortion) to High(FUpstreamRunoffPortion) do
    begin
      FUpstreamRunoffPortion[LIndex].FData := 0.0;
      FUpstreamRunoffPortion[LIndex].FInitalised := False;
      FUpstreamRunoffPortion[LIndex].FLength := 10;
      FUpstreamRunoffPortion[LIndex].FDecimal := 2;
      FUpstreamRunoffPortion[LIndex].FDefaultPadding := True;
      FUpstreamRunoffPortion[LIndex].ShowDecimalPoint := True;
    end;
    for LIndex := Low(FBoardAndPilarRechargeFactor) to High(FBoardAndPilarRechargeFactor) do
    begin
      FBoardAndPilarRechargeFactor[LIndex].FData := 0.0;
      FBoardAndPilarRechargeFactor[LIndex].FInitalised := False;
      FBoardAndPilarRechargeFactor[LIndex].FLength := 10;
      FBoardAndPilarRechargeFactor[LIndex].FDecimal := 2;
      FBoardAndPilarRechargeFactor[LIndex].FDefaultPadding := True;
      FBoardAndPilarRechargeFactor[LIndex].ShowDecimalPoint := True;
    end;
    for LIndex := Low(FHighExtractionRechargeFactor) to High(FHighExtractionRechargeFactor) do
    begin
      FHighExtractionRechargeFactor[LIndex].FData := 0.0;
      FHighExtractionRechargeFactor[LIndex].FInitalised := False;
      FHighExtractionRechargeFactor[LIndex].FLength := 10;
      FHighExtractionRechargeFactor[LIndex].FDecimal := 2;
      FHighExtractionRechargeFactor[LIndex].FDefaultPadding := True;
      FHighExtractionRechargeFactor[LIndex].ShowDecimalPoint := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMineSlurryDumpFileObject }

procedure TMineSlurryDumpFileObject.CreateMemberObjects;
const OPNAME = 'TMineSlurryDumpFileObject.CreateMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FDumpName               := TString.Create;
    FDumpSurfaceArea        := TDouble.Create;
    FRunoffFactorToPCD      := TDouble.Create;
    FSeepageSplitFactor     := TDouble.Create;
    FPCDStorageCapacity     := TDouble.Create;
    FPCDSurfaceArea         := TDouble.Create;
    FPCDAnalysisStartVolume := TDouble.Create;
    for LIndex := Low(FRechargeFactor) to High(FRechargeFactor) do
    begin
      FRechargeFactor[LIndex] := TDouble.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpFileObject.DestroyMemberObjects;
const OPNAME = 'TMineSlurryDumpFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited;
  try
    FDumpName.Free;
    FDumpSurfaceArea.Free;
    FRunoffFactorToPCD.Free;
    FSeepageSplitFactor.Free;
    FPCDStorageCapacity.Free;
    FPCDSurfaceArea.Free;
    FPCDAnalysisStartVolume.Free;
    for LIndex := Low(FRechargeFactor) to High(FRechargeFactor) do
    begin
      FRechargeFactor[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpFileObject.Reset;
const OPNAME = 'TMineSlurryDumpFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpFileObject.Initialise: boolean;
const OPNAME = 'TMineSlurryDumpFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FDumpName.FData := '';
    FDumpName.FInitalised := False;
    FDumpName.FLength := 255;
    FDumpName.FDecimal := 0;
    FDumpName.FDefaultPadding := True;

    FDumpSurfaceArea.FData := 0.0;
    FDumpSurfaceArea.FInitalised := False;
    FDumpSurfaceArea.FLength := 10;
    FDumpSurfaceArea.FDecimal := 2;
    FDumpSurfaceArea.FDefaultPadding := True;
    FDumpSurfaceArea.ShowDecimalPoint := True;

    FRunoffFactorToPCD.FData := 0.0;
    FRunoffFactorToPCD.FInitalised := False;
    FRunoffFactorToPCD.FLength := 10;
    FRunoffFactorToPCD.FDecimal := 3;
    FRunoffFactorToPCD.FDefaultPadding := True;
    FRunoffFactorToPCD.ShowDecimalPoint := True;

    FSeepageSplitFactor.FData := 0.0;
    FSeepageSplitFactor.FInitalised := False;
    FSeepageSplitFactor.FLength := 10;
    FSeepageSplitFactor.FDecimal := 3;
    FSeepageSplitFactor.FDefaultPadding := True;
    FSeepageSplitFactor.ShowDecimalPoint := True;

    FPCDStorageCapacity.FData := 0.0;
    FPCDStorageCapacity.FInitalised := False;
    FPCDStorageCapacity.FLength := 10;
    FPCDStorageCapacity.FDecimal := 2;
    FPCDStorageCapacity.FDefaultPadding := True;
    FPCDStorageCapacity.ShowDecimalPoint := True;

    FPCDSurfaceArea.FData := 0.0;
    FPCDSurfaceArea.FInitalised := False;
    FPCDSurfaceArea.FLength := 10;
    FPCDSurfaceArea.FDecimal := 2;
    FPCDSurfaceArea.FDefaultPadding := True;
    FPCDSurfaceArea.ShowDecimalPoint := True;

    FPCDAnalysisStartVolume.FData := 0.0;
    FPCDAnalysisStartVolume.FInitalised := False;
    FPCDAnalysisStartVolume.FLength := 10;
    FPCDAnalysisStartVolume.FDecimal := 2;
    FPCDAnalysisStartVolume.FDefaultPadding := True;
    FPCDAnalysisStartVolume.ShowDecimalPoint := True;

    for LIndex := Low(FRechargeFactor) to High(FRechargeFactor) do
    begin
      FRechargeFactor[LIndex].FData := 0.0;
      FRechargeFactor[LIndex].FInitalised := False;
      FRechargeFactor[LIndex].FLength := 10;
      FRechargeFactor[LIndex].FDecimal := 2;
      FRechargeFactor[LIndex].FDefaultPadding := True;
      FRechargeFactor[LIndex].ShowDecimalPoint := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TMineFileObject}

procedure TMineFileObject.CreateMemberObjects;
const OPNAME = 'TMineFileObject.CreateMemberObjects';
var
  LIndex : integer;
Begin
  try
    FOpenCastObjectContainer    := TObjectList.Create(True);
    FUnderGroundObjectContainer := TObjectList.Create(True);
    FSlurryDumpObjectContainer  := TObjectList.Create(True);
    FNodeNumber                 := TInteger.Create;
    FMineName                   := TString.Create;
    FRiverChannelNumber         := TInteger.Create;
    FPCDChannelNumber           := TInteger.Create;
    FHydrologyNodeNumber        := TInteger.Create;
    FBeneficiationPlantArea     := TDouble.Create;
    FBeneficiationRunOffFactor  := TDouble.Create;
    for LIndex := Low(FPanEvaporation) to High(FPanEvaporation) do
    begin
      FPanEvaporation[LIndex] := TDouble.Create;
    end;
    for LIndex := Low(FPanEvaporation) to High(FPanEvaporation) do
    begin
      FLakeEvaporation[LIndex] := TDouble.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineFileObject.DestroyMemberObjects;
const OPNAME = 'TMineFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  try
    FOpenCastObjectContainer.Free;
    FUnderGroundObjectContainer.Free;
    FSlurryDumpObjectContainer.Free;
    FNodeNumber.Free;
    FMineName.Free;
    FRiverChannelNumber.Free;
    FPCDChannelNumber.Free;
    FHydrologyNodeNumber.Free;
    FBeneficiationPlantArea.Free;
    FBeneficiationRunOffFactor.Free;
    for LIndex := Low(FPanEvaporation) to High(FPanEvaporation) do
    begin
      FPanEvaporation[LIndex].Free;
    end;
    for LIndex := Low(FPanEvaporation) to High(FPanEvaporation) do
    begin
      FLakeEvaporation[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineFileObject.Reset;
const OPNAME = 'TMineFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.Initialise: boolean;
const OPNAME = 'TMineFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FOpenCastObjectContainer.Clear;
    FUnderGroundObjectContainer.Clear;
    FSlurryDumpObjectContainer.Clear;

    FNodeNumber.FData := 0;
    FNodeNumber.FInitalised := False;
    FNodeNumber.FLength := 4;
    FNodeNumber.FDecimal := 0;
    FNodeNumber.FDefaultPadding := True;

    FMineName.FData := '';
    FMineName.FInitalised := False;
    FMineName.FLength := 255;
    FMineName.FDecimal := 0;
    FMineName.FDefaultPadding := True;

    FRiverChannelNumber.FData := 0;
    FRiverChannelNumber.FInitalised := False;
    FRiverChannelNumber.FLength := 4;
    FRiverChannelNumber.FDecimal := 0;
    FRiverChannelNumber.FDefaultPadding := True;

    FPCDChannelNumber.FData := 0;
    FPCDChannelNumber.FInitalised := False;
    FPCDChannelNumber.FLength := 4;
    FPCDChannelNumber.FDecimal := 0;
    FPCDChannelNumber.FDefaultPadding := True;

    FHydrologyNodeNumber.FData := 0;
    FHydrologyNodeNumber.FInitalised := False;
    FHydrologyNodeNumber.FLength := 4;
    FHydrologyNodeNumber.FDecimal := 0;
    FHydrologyNodeNumber.FDefaultPadding := True;

    FBeneficiationPlantArea.FData := 0.0;
    FBeneficiationPlantArea.FInitalised := False;
    FBeneficiationPlantArea.FLength := 10;
    FBeneficiationPlantArea.FDecimal := 2;
    FBeneficiationPlantArea.FDefaultPadding := True;
    FBeneficiationPlantArea.ShowDecimalPoint := True;

    FBeneficiationRunOffFactor.FData := 0.0;
    FBeneficiationRunOffFactor.FInitalised := False;
    FBeneficiationRunOffFactor.FLength := 10;
    FBeneficiationRunOffFactor.FDecimal := 3;
    FBeneficiationRunOffFactor.FDefaultPadding := True;
    FBeneficiationRunOffFactor.ShowDecimalPoint := True;
    for LIndex := Low(FPanEvaporation) to High(FPanEvaporation) do
    begin
      FPanEvaporation[LIndex].FData := 0.0;
      FPanEvaporation[LIndex].FInitalised := False;
      FPanEvaporation[LIndex].FLength := 10;
      FPanEvaporation[LIndex].FDecimal := 2;
      FPanEvaporation[LIndex].FDefaultPadding := True;
      FPanEvaporation[LIndex].ShowDecimalPoint := True;
    end;
    for LIndex := Low(FPanEvaporation) to High(FPanEvaporation) do
    begin
      FLakeEvaporation[LIndex].FData := 0.0;
      FLakeEvaporation[LIndex].FInitalised := False;
      FLakeEvaporation[LIndex].FLength := 10;
      FLakeEvaporation[LIndex].FDecimal := 2;
      FLakeEvaporation[LIndex].FDefaultPadding := True;
      FLakeEvaporation[LIndex].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.AddOpenCast: TMineOpenCastFileObject;
const OPNAME = 'TMineFileObject.AddOpenCast';
begin
  Result := nil;
  try
    Result := TMineOpenCastFileObject.Create;
    FOpenCastObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.AddUndeground: TMineUndegroundFileObject;
const OPNAME = 'TMineFileObject.AddUndeground';
begin
  Result := nil;
  try
    Result := TMineUndegroundFileObject.Create;
    FUnderGroundObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.AddSlurryDump: TMineSlurryDumpFileObject;
const OPNAME = 'TMineFileObject.AddSlurryDump';
begin
  Result := nil;
  try
    Result := TMineSlurryDumpFileObject.Create;
    FSlurryDumpObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.GetOpenCastByIndex( AIndex: integer): TMineOpenCastFileObject;
const OPNAME = 'TMineFileObject.GetOpenCastByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FOpenCastObjectContainer.Count) then
      Result := TMineOpenCastFileObject(FOpenCastObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.GetUndegroundByIndex(AIndex: integer): TMineUndegroundFileObject;
const OPNAME = 'TMineFileObject.GetUndegroundByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FUnderGroundObjectContainer.Count) then
      Result := TMineUndegroundFileObject(FUnderGroundObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.GetSlurryDumpByIndex(AIndex: integer): TMineSlurryDumpFileObject;
const OPNAME = 'TMineFileObject.GetSlurryDumpByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FSlurryDumpObjectContainer.Count) then
      Result := TMineSlurryDumpFileObject(FSlurryDumpObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.OpenCastCount: integer;
const OPNAME = 'TMineFileObject.OpenCastCount';
begin
  Result := 0;
  try
    Result := FOpenCastObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.UndegroundCount: integer;
const OPNAME = 'TMineFileObject.UndegroundCount';
begin
  Result := 0;
  try
    Result := FUnderGroundObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineFileObject.SlurryDumpCount: integer;
const OPNAME = 'TMineFileObject.SlurryDumpCount';
begin
  Result := 0;
  try
    Result := FSlurryDumpObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMineListFileObject }


procedure TMineListFileObject.CreateMemberObjects;
const OPNAME = 'TMineListFileObject.CreateMemberObjects';
begin
  try
    inherited;
    FMineFileObjectContainer := TObjectList.Create(True);
    FComment                 := TStringlist.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineListFileObject.DestroyMemberObjects;
const OPNAME = 'TMineListFileObject.DestroyMemberObjects';
begin
  try
    inherited;
    FMineFileObjectContainer.Clear;
    FMineFileObjectContainer.Free;
    FComment.Clear;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineListFileObject.AddMineFileObject: TMineFileObject;
const OPNAME = 'TMineListFileObject.AddMineFileObject';
begin
  Result := nil;
  try
    Result := TMineFileObject.Create;
    FMineFileObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineListFileObject.GeTMineFileObjectByIndex(AIndex: integer): TMineFileObject;
const OPNAME = 'TMineListFileObject.GeTMineFileObjectByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FMineFileObjectContainer.Count) then
      Result := TMineFileObject(FMineFileObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineListFileObject.Initialise: boolean;
const OPNAME = 'TMineListFileObject.Initialise';
begin
  Result := False;
  try
    FMineFileObjectContainer.Clear;
    FComment.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineListFileObject.Reset;
const OPNAME = 'TMineListFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineListFileObject.MineFileObjectCount: integer;
const OPNAME = 'TMineListFileObject.MineFileObjectCount';
begin
  Result := 0;
  try
    Result := FMineFileObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMineSubCatchmentFileObject }

procedure TMineSubCatchmentFileObject.CreateMemberObjects;
const OPNAME = 'TMineSubCatchmentFileObject.CreateMemberObjects';
var
  LIndex : integer;
Begin
  try
    FCatchmentRefNumber          := TInteger.Create;
    FProportionAntecedentFlow    := TDouble.Create;
    FGroundwaterFlowVolume       := TDouble.Create;
    FAntecedentRunoffDecayFactor := TDouble.Create;
    FCatchmentRefUsed            := TString.Create;
    for LIndex := Low(FMineSubCatchmentFlowVolume) to High(FMineSubCatchmentFlowVolume) do
    begin
      FMineSubCatchmentFlowVolume[LIndex] := TDouble.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFileObject.DestroyMemberObjects;
const OPNAME = 'TMineSubCatchmentFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  try
    FCatchmentRefNumber.Free;
    FProportionAntecedentFlow.Free;
    FGroundwaterFlowVolume.Free;
    FAntecedentRunoffDecayFactor.Free;
    FCatchmentRefUsed.Free;
    for LIndex := Low(FMineSubCatchmentFlowVolume) to High(FMineSubCatchmentFlowVolume) do
    begin
      FMineSubCatchmentFlowVolume[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFileObject.Initialise: boolean;
const OPNAME = 'TMineSubCatchmentFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FCatchmentRefNumber.FData := 0;
    FCatchmentRefNumber.FInitalised := False;
    FCatchmentRefNumber.FLength := 4;
    FCatchmentRefNumber.FDecimal := 0;
    FCatchmentRefNumber.FDefaultPadding := True;

    FProportionAntecedentFlow.FData := 0.0;
    FProportionAntecedentFlow.FInitalised := False;
    FProportionAntecedentFlow.FLength := 10;
    FProportionAntecedentFlow.FDecimal := 3;
    FProportionAntecedentFlow.FDefaultPadding := True;
    FProportionAntecedentFlow.ShowDecimalPoint := True;

    FGroundwaterFlowVolume.FData := 0.0;
    FGroundwaterFlowVolume.FInitalised := False;
    FGroundwaterFlowVolume.FLength := 10;
    FGroundwaterFlowVolume.FDecimal := 3;
    FGroundwaterFlowVolume.FDefaultPadding := True;
    FGroundwaterFlowVolume.ShowDecimalPoint := True;

    FAntecedentRunoffDecayFactor.FData := 0.0;
    FAntecedentRunoffDecayFactor.FInitalised := False;
    FAntecedentRunoffDecayFactor.FLength := 10;
    FAntecedentRunoffDecayFactor.FDecimal := 2;
    FAntecedentRunoffDecayFactor.FDefaultPadding := True;
    FAntecedentRunoffDecayFactor.ShowDecimalPoint := True;

    FCatchmentRefUsed.FData := '';
    FCatchmentRefUsed.FInitalised := False;
    FCatchmentRefUsed.FLength := 3;
    FCatchmentRefUsed.FDecimal := 0;
    FCatchmentRefUsed.FDefaultPadding := True;

    for LIndex := Low(FMineSubCatchmentFlowVolume) to High(FMineSubCatchmentFlowVolume) do
    begin
      FMineSubCatchmentFlowVolume[LIndex].FData := 0.0;
      FMineSubCatchmentFlowVolume[LIndex].FInitalised := False;
      FMineSubCatchmentFlowVolume[LIndex].FLength := 10;
      FMineSubCatchmentFlowVolume[LIndex].FDecimal := 2;
      FMineSubCatchmentFlowVolume[LIndex].FDefaultPadding := True;
      FMineSubCatchmentFlowVolume[LIndex].ShowDecimalPoint := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFileObject.Reset;
const OPNAME = 'TMineSubCatchmentFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMineSubCatchmentListFileObject }

procedure TMineSubCatchmentListFileObject.CreateMemberObjects;
const OPNAME = 'TMineSubCatchmentListFileObject.CreateMemberObjects';
begin
  try
    inherited;
    FMineSubCatchmentFileContainer := TObjectList.Create(True);
    FComment                       := TStringlist.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentListFileObject.DestroyMemberObjects;
const OPNAME = 'TMineSubCatchmentListFileObject.DestroyMemberObjects';
begin
  try
    inherited;
    FMineSubCatchmentFileContainer.Clear;
    FMineSubCatchmentFileContainer.Free;
    FComment.Clear;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentListFileObject.AddMineSubCatchmentFileObject: TMineSubCatchmentFileObject;
const OPNAME = 'TMineSubCatchmentListFileObject.AddMineSubCatchmentFileObject';
begin
  Result := nil;
  try
    Result := TMineSubCatchmentFileObject.Create;
    FMineSubCatchmentFileContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end; 

function TMineSubCatchmentListFileObject.GetMineSubCatchmentListByIndex(AIndex: integer): TMineSubCatchmentFileObject;
const OPNAME = 'TMineSubCatchmentListFileObject.GetMineSubCatchmentListByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FMineSubCatchmentFileContainer.Count) then
      Result := TMineSubCatchmentFileObject(FMineSubCatchmentFileContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentListFileObject.Initialise: boolean;
const OPNAME = 'TMineSubCatchmentListFileObject.Initialise';
begin
  Result := False;
  try
    FMineSubCatchmentFileContainer.Clear;
    FComment.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentListFileObject.MineSubCatchmentObjectCount: integer;
const OPNAME = 'TMineSubCatchmentListFileObject.MineSubCatchmentObjectCount';
begin
  Result := 0;
  try
    Result := FMineSubCatchmentFileContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentListFileObject.Reset;
const OPNAME = 'TMineSubCatchmentListFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
