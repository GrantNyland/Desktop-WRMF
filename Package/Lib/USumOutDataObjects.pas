//
//
//  UNIT      : Contains SumOutDataObjects Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutDataObjects;

interface

uses
  Classes, sysutils,contnrs,windows,

  //  DWAF VCL
  UAbstractDataObject,
  UAbstractObject,
  UBasicObjects,
  VoaimsCom_TLB,
  UConstants,
  DB;


const
  HEADINGEXITLABEL = '    -----  -----       ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------';
  VALUESEXITLABEL  = '                       ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------';
  YIELDFAILUREHEADINGEXITLABEL = '  YEAR';
  SEQUENCESWITHFAILURESEXITLABEL = 'YEAR / NO OF FAILURE SEQ.';
  AVARAGELABEL     = '   AVERAGE      ';
  VIOLATIONSLABEL  = 'NUMBER OF STORAGE VIOLATIONS = ';
  TOTALSYSTEMLABEL = 'TOTAL SYSTEM   ';
  untMonthEndReservoirVolume            = '(mcm)';
  untMonthEndReservoirElevation         = '(m)';
  untNetBasinRunoffIntoResArea          = '(m3/s)';
  untRainfallOnReservoirSurface         = '(m3/s)';
  untGrossEvaporationLossFromReservoir  = '(m3/s)';
  untMonthlyAveragePowerFlow            = '(m3/s)';
  untMonthlyAverageSpillFlow            = '(m3/s)';
  untMonthlyAverageStackedCapacity      = '(mw)';
  untMonthlyAverageStackedEnergy        = '(mw continuous)';
  untMonthlyAverageIrrigationDeficits   = '(m3/s)';
  untMonthlyAverageChannelFlow          = '(m3/s)';
  untMonthlyPumpingEnergy               = '(gwh)';

  lblMonthEndReservoirVolume            = 'MONTH-END RESERVOIR VOLUME (MCM)';
  lblMonthEndReservoirElevation         = 'MONTH-END RESERVOIR ELEVATION (M)';
  lblNetBasinRunoffIntoResArea          = 'NET BASIN RUNOFF INTO RES AREA(M3/S)';
  lblRainfallOnReservoirSurface         = 'RAINFALL ON RESERVOIR SURFACE(M3/S)';
  lblGrossEvaporationLossFromReservoir  = 'GROSS EVAPORATION LOSS FROM RESERVOIR(M3/S)';
  lblMonthlyAveragePowerFlow            = 'MONTHLY AVERAGE POWER FLOW (M3/S)';
  lblMonthlyAverageSpillFlow            = 'MONTHLY AVERAGE SPILL FLOW (M3/S)';
  lblMonthlyAverageStackedCapacity      = 'MONTHLY AVERAGE STACKED CAPACITY (MW)';
  lblMonthlyAverageStackedEnergy        = 'MONTHLY AVERAGE STACKED ENERGY (MW CONTINUOUS)';
  lblMonthlyAverageIrrigationDeficits   = 'MONTHLY AVERAGE IRRIGATION DEFICITS (M3/S)';
  lblMonthlyAverageChannelFlow          = 'MONTHLY AVERAGE CHANNEL FLOW (M3/S)';
  lblMonthlyPumpingEnergy               = 'MONTHLY PUMPING ENERGY (GWH)';

  lblPlaningMonthEndReservoirVolume            = 'MONTH-END RESERVOIR VOLUME ( MCM )';
  lblPlaningMonthEndReservoirElevation         = 'MONTH-END RESERVOIR ELEVATION ( M )';
  lblPlaningNetBasinRunoffIntoResArea          = 'NET BASIN RUNOFF INTO RES AREA( M3/S )';
  lblPlaningRainfallOnReservoirSurface         = 'RAINFALL ON RESERVOIR SURFACE AREA( M3/S )';
  lblPlaningGrossEvaporationLossFromReservoir  = 'GROSS EVAPORATION LOSS FROM RESERVOIR ( M3/S )';
  lblPlaningMonthlyAveragePowerFlow            = 'MONTHLY AVERAGE POWER FLOW ( M3/S )';
  lblPlaningMonthlyAverageSpillFlow            = 'MONTHLY AVERAGE SPILL FLOW ( M3/S )';
  lblPlaningMonthlyAverageStackedCapacity      = 'MONTHLY AVERAGE STACKED CAPACITY ( MW )';
  lblPlaningMonthlyAverageStackedEnergy        = 'MONTHLY AVERAGE STACKED ENERGY( MW CONTINUOUS )';
  lblPlaningMonthlyAverageIrrigationDeficits   = 'MONTHLY AVERAGE IRRIGATION DEFICITS ( M3/S )';
  lblPlaningMonthlyAverageChannelFlow          = 'MONTHLY AVERAGE CHANNEL FLOW ( M3/S )';
  lblPlaningMonthlyPumpingEnergy               = 'MONTHLY PUMPING ENERGY ( GWH )';


  lblYieldFailurePerYearPerSequence     = 'YIELD FAILURE PER YEAR PER SEQUENCE';

  lblOutputSummary                      = 'UPSTREAM   LOCAL  EVAP0';

  lblAnualFirmYieldDemands              = 'ANNUAL FIRM YIELD DEMANDS (MCM)';
  lblAnualFirmEnergyDemands             = 'ANNUAL FIRM ENERGY DEMANDS (MWc)';
  lblAnualFirmSelectedYieldDemands      = 'ANNUAL FIRM SELECTED YIELD DEMANDS (MCM)';

  lblAnualNonFirmYieldDemands           = 'ANNUAL NON-FIRM YIELD DEMANDS (MCM)';
  lblAnualSecondaryYieldDemands         = 'ANNUAL SECONDARY YIELD DEMANDS (MCM)';
  lblAnualTotalSystemPumpingEnergy      = 'AVERAGE OF TOTAL SYSTEM PUMPING ENERGY';
  lblAnualFullSystemSupplyVolume        = 'FULL SYSTEM SUPPLY VOLUME';
  lblAnualAverageInflow                 = 'ANNUAL AVERAGE INFLOW HYDROLOGY BY DRAINAGE BASIN ( MCM )';
  lblSequencesWithFailures              = 'NUMBER OF SEQUENCES WITH FAILURES';

  lblNumberOfFailureSequence            = 'NUMBER OF FAILURE SEQUENCES';
  lblFirmYieldRecurrenceInterval        = 'FIRM YIELD RECURRENCE INTERVALS';

  lblCriticalPeriodsNumber              = 'NUMBER OF CRITICAL PERIODS';
  lblCriticalPeriodsLength              = 'AVERAGE LENGTH OF CRITICAL PERIOD';
  lblCriticalPeriodsDeficit             = 'AVERAGE CRITICAL PERIOD DEFICIT';
  lblCriticalPeriodsAvarage             = 'AVERAGE DRAWDOWN PERIOD';
  lblDeficitPropotion                   = 'DEFICITS  (PROPORTION)';

  TITLE1    = 'TOTAL INTEGRATED POWER SYSTEM';
  TITLE2    = 'WATER YIELD SUMMARY';
  TITLE3    = 'TOTAL SYSTEM SUMMARY';

  LowIndex  = 0;
  HighIndex = 14;
  TotalAVGLine = 99999;

type

  TOpenFileAction = (ofaRead,ofaWrite);
  TRunType = (rtUnKnown,rtHistoric,rtStochastic);
  TModelType = (mtUnKnown,mtYield,mtPlanning);

  TSumOutFile = class(TAbstractAppDataObject)
  protected
    FFileEOF: boolean;
    FCurrentPosition: integer;
    FFileName: string;
    FFileStream: TFileStream;
    FLines: TStringList;
    FReadBlock: PAnsiChar;
    FBeforeExraChars: PAnsiChar;
    FAfterExraChars: PAnsiChar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ClearReadBlock;
    procedure ReadBlock;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function PeekNextLine: string;
    function GetCurrentLine: string;
    function AddLine(Aline: string): boolean;
    function AppendToFile: boolean;
    function OpenTheFile(AFileName: string; AOpenAction: TOpenFileAction): boolean;
    property EOF: boolean read FFileEOF;
    property FileStream:TFileStream read FFileStream;
  end;

  TDataBlock = class(TAbstractAppDataObject)
  protected
    FDataContainer : TStringList;
    FBlockNumber   : integer;
    FBlockType     : TOutputDataType;
    FRunType       : TRunType;
    FModelType     : TModelType;
    function GetOutputDataType    : TOutputDataType;
    function GetLoadCaseNumber    : integer;
    function GetSequenceNumber    : integer;
    function GetAnnualWaterDemand : double;
    function GetAnnualPowerDemand : double;
    function GetElementID         : integer;
    function GetElementName       : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    function ReadBlock(ACurrentFile: TSumOutFile; var ABlockSkipped : boolean): boolean;
    property BlockNumber : integer     read FBlockNumber write FBlockNumber;
    property BlockType:TOutputDataType read FBlockType;
    property LoadCaseNumber:integer    read GetLoadCaseNumber;
    property SequenceNumber:integer    read GetSequenceNumber;
    property ElementID:integer         read GetElementID;
    property AnnualWaterDemand:double  read GetAnnualWaterDemand;
    property AnnualPowerDemand:double  read GetAnnualPowerDemand;
    property ElementName:string        read GetElementName;
    property RunType    : TRunType     read FRunType write FRunType;
    property ModelType  : TModelType   read FModelType write FModelType;
    property Lines      : TStringList  read FDataContainer;
  end;

  TReserviorIDsAgent = class(TAbstractAppObject)
  protected
    FReserviorIDs,
    FReserviorNames: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean;override;
    function GetReserviorId(AReserviorName: string): integer;
  end;

  TValuesLine = class(TAbstractAppDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FBlockType :TOutputDataType;
    FExitLabel1: string;
    FExitLabel2: string;
    FValCountLength: integer;
    FValYearLength: integer;
    FValFirstLength: integer;
    FValOtherLength: integer;
    FValAvgLength: integer;
    FDecimals: integer;
    FBlockNumber: integer;
    FLoadCaseNumber: integer;
    FSequenceNumber: integer;
    FShowDecimals: boolean;
    FValues : array[LowIndex..HighIndex] of TDouble;
    FReservoirName: TString;
    procedure Reset;override;
    function Initialise: boolean;override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure Assign(ASource : TValuesLine); virtual;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;virtual;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;virtual;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;virtual;
    function ReadBlockIDs(ADataset:TDataset): boolean;
    property ValCountLength:integer read FValCountLength write FValCountLength;
    property ValYearLength:integer read FValYearLength write FValYearLength;
    property ValFirstLength:integer read FValFirstLength write FValFirstLength;
    property ValOtherLength:integer read FValOtherLength write FValOtherLength;
    property ValAvgLength:integer read FValAvgLength write FValAvgLength;
    property Decimals:integer read FDecimals write FDecimals;
    property ShowDecimals:boolean read FShowDecimals write FShowDecimals;
    property LoadCaseNumber:integer read FLoadCaseNumber write FLoadCaseNumber;
    property SequenceNumber:integer read FSequenceNumber write FSequenceNumber;
    property BlockNumber:integer read FBlockNumber write FBlockNumber;
    property ExitLabel1:string read FExitLabel1 write FExitLabel1;
    property ExitLabel2:string read FExitLabel2 write FExitLabel2;
  end;

  TSumOutGenericBlockHeading = class(TValuesLine)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FHeadingLines: TStringList;
    FTitleID: TInteger;
    FHeading: TString;
    FTitle: TString;
    FElementID: integer;
    FAnnualWaterDemand : double;
    FAnnualPowerDemand : double;

    procedure Reset;override;
    procedure SaveToStream(Stream: TStream); override;
    function Initialise: boolean;override;
    function AddLine(ALine: string): boolean;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
    property ElementID : integer read FElementID write FElementID;
    property AnnualWaterDemand : double read FAnnualWaterDemand write FAnnualWaterDemand;
    property AnnualPowerDemand : double read FAnnualPowerDemand write FAnnualPowerDemand;
  end;

  TSumOutGenericBlockValues = class(TValuesLine)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FValuesLines: TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutGenericBlockAvarage = class(TValuesLine)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FViolationsCount: TInteger;
    procedure SaveToStream(Stream: TStream); override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    function Initialise: boolean;override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutMonthlyBlockHeading = class(TSumOutGenericBlockHeading)
  public
    function Initialise: boolean;override;
  end;

  TSumOutMonthlyBlockValues = class(TSumOutGenericBlockValues)
  public
    function Initialise: boolean;override;
  end;

  TSumOutMonthlyBlockAvarage= class(TSumOutGenericBlockAvarage)
  public
    function Initialise: boolean;override;
  end;

  TSumOutYieldFailureBlockHeading = class(TSumOutGenericBlockHeading)
  public
    function Initialise: boolean;override;
  end;

  TSumOutYieldFailureBlockValues = class(TSumOutGenericBlockValues)
  public
    function Initialise: boolean;override;
  end;

  TSumOutOutputSummaryBlockHeading = class(TSumOutGenericBlockHeading)
  public
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
  end;

  TSumOutOutputSummaryBlockValues = class(TSumOutGenericBlockValues)
  public
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutOutputSummaryBlockTotal = class(TSumOutGenericBlockAvarage)
  public
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutAnualSummaryBlockHeading = class(TSumOutGenericBlockHeading)
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
  end;


  TAnualSummaryTenValues = array[LowIndex..HighIndex] of TDouble;

  TAnualSummaryValuesLine = class(TObject)
  protected
  public
    FFirstInteger: TInteger;
    FAnualSummaryValuesLine: TAnualSummaryTenValues;
    constructor Create;
    destructor Destroy; override;
    function Initialise: boolean;
    property FirstInteger:TInteger read FFirstInteger;
    property AnualSummaryValuesLine:TAnualSummaryTenValues read FAnualSummaryValuesLine;
  end;

  TAnualSummaryValues = class(TObjectList)
  protected
    function GetAnualSummaryValuesLine(AIndex: Integer): TAnualSummaryValuesLine;
  public
    function Initialise: boolean;
    property AnualSummaryValuesLine[AIndex: Integer]: TAnualSummaryValuesLine read GetAnualSummaryValuesLine;
  end;

  TSumOutAnualSummaryBlockValues = class(TSumOutGenericBlockAvarage)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetLine1Label: string;
  public
    FYearPeriod: TInteger;
    FCaption: TString;
    FSelectedYieldLabel: TString;
    FSystemSupplyVolume : TDouble;
    FAnualValues : TAnualSummaryValues;
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutAnualAverageInflowBlockHeading = class(TSumOutGenericBlockHeading)
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
  end;

  TSumOutAnualAverageInflowBlockValues = class(TSumOutGenericBlockValues)
  public
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutSequencesWithFailuresBlockHeading = class(TSumOutGenericBlockHeading)
    function Initialise: boolean;override;
  end;

  TSumOutSequencesWithFailuresBlockValues = class(TSumOutGenericBlockValues)
  public
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutSequencesWithFailuresBlockTotal = class(TSumOutGenericBlockAvarage)
  public
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutCriticalPeriodsBlockHeading = class(TSumOutGenericBlockHeading)
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
  end;

  TSumOutCriticalPeriodsBlockValues = class(TSumOutGenericBlockAvarage)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FCaption: TString;
    FFirstInteger: TInteger;
    FPeriodsValues : array[LowIndex..HighIndex] of TDouble;
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
    procedure SaveToStream(Stream: TStream); override;
    function SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;override;
    function ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;override;
  end;

  TSumOutRecurrenceIntervalBlockHeading = class(TSumOutGenericBlockHeading)
    function ReadFromBlockData(ABLockData: TDataBlock): boolean;override;
  end;

  TSumOutRecurrenceIntervalBlockValues = class(TSumOutGenericBlockValues)
   protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FCaption : TString;
    FTargetDraft : array[LowIndex..HighIndex] of TDouble;
    FPeriodLength : array[LowIndex..HighIndex] of TInteger;
    FNumberOfFailureSeqValues : array [LowIndex..HighIndex] of TInteger;
    FAnualValues : TAnualSummaryValues;
    function Initialise: boolean;override;
    function ReadFromBlockData(ABLockData : TDataBlock): boolean;override;
    procedure SaveToStream(Stream : TStream); override;
    function SaveToDatabase(ADatabaseName, AModel, AStudy, ASubArea, AScenario : string) : boolean;override;
    function ReadFromDatabase(ADatabaseName : string; ADataset : TDataset) : boolean;override;
  end;

  TSumOutAnualFirmEnergyDemandBlockValues = class(TSumOutGenericBlockValues)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FCaption : TString;
    FActualDemand : array[LowIndex..HighIndex] of TDouble;
    FAnualValues : TAnualSummaryValues;
    function Initialise: boolean;override;
    //function ReadFromBlockData(ABLockData : TDataBlock): boolean;override;
    function ReadFromBlockValues(ANetworkElementID : integer; ABLockData : TDataBlock): boolean;


  end;

implementation

uses Math, System.AnsiStrings,
     UUtilities,
     UDatabaseUtilities,
     UDataSetType,

     UErrorHandlingOperations;

const
      BlockSize = 32768;
      LineLength = 136;
      BufferSize = BlockSize + LineLength;


{ TSumOutGenericBlockHeading }

procedure TSumOutGenericBlockHeading.CreateMemberObjects;
const OPNAME = 'TSumOutGenericBlockHeading.CreateMemberObjects';
begin
  inherited;
  try
    FHeadingLines := TStringList.Create;
    FTitleID := TInteger.Create;
    FHeading := TString.Create;
    FTitle   := TString.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockHeading.DestroyMemberObjects;
const OPNAME = 'TSumOutGenericBlockHeading.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FHeadingLines);
    FreeAndNil(FTitleID);
    FreeAndNil(FHeading);
    FreeAndNil(FTitle);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockHeading.Initialise: boolean;
const OPNAME = 'TSumOutGenericBlockHeading.Initialise';
begin
  Result := False;
  try
    Result := inherited Initialise;;
    FHeadingLines.Clear;

    FTitleID.FData := 0;
    FTitleID.FInitalised := False;
    FTitleID.FLength := 4;
    FTitleID.FDecimal := 0;
    FTitleID.FDefaultPadding := True;

    FHeading.FData := '';
    FHeading.FInitalised := False;
    FHeading.FLength := 50;
    FHeading.FDefaultPadding := True;

    FTitle.FData := '';
    FTitle.FInitalised := False;
    FTitle.FLength := 36;
    FTitle.FDecimal := 0;
    FTitle.FDefaultPadding := True;

    FAnnualWaterDemand := NullFloat;
    FAnnualPowerDemand := NullFloat;
    FElementID         := NullInteger;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockHeading.Reset;
const OPNAME = 'TSumOutGenericBlockHeading.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockHeading.AddLine(ALine: string): boolean;
const OPNAME = 'TSumOutGenericBlockHeading.AddLine';
begin
  Result := False;
  try
    FHeadingLines.Add(ALine);
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockHeading.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutGenericBlockHeading.SaveToStream';
begin
  try
    FHeadingLines.SaveToStream(Stream);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockHeading.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutGenericBlockHeading.ReadFromBlockData';
var
  LCurrentLine: string;
  LHeading,
  LTitle: string;
begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      FHeadingLines.Add(LCurrentLine);
      ABLockData.Lines.Delete(0);
      if (Pos(ExitLabel1,LCurrentLine) > 0) or (Pos(ExitLabel2,LCurrentLine) > 0) then
        Break;
    end;

    if (FHeadingLines.Count > 7) then
    begin
      if(Trim(FHeadingLines[7]) = 'YEAR') then
        LCurrentLine := FHeadingLines[6]
      else
        LCurrentLine := FHeadingLines[7];
      LHeading     := Copy(LCurrentLine,3,50);
      LTitle       := Copy(LCurrentLine,62,36);
      if(Trim(LHeading) <> '') then
      begin
        FHeading.FData  := LHeading;
        FHeading.FInitalised := True;
      end;
      if(Trim(LTitle) <> '') then
      begin
        FTitle.FData  := LTitle;
        FTitle.FInitalised := True;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockHeading.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutGenericBlockHeading.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier, LineData'+
        ' FROM suBlockHeader'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier');

      LDataSet.DataSet.Open;
      FHeadingLines.Clear;
      while not LDataSet.DataSet.Eof do
      begin
        FHeadingLines.Add(Trim(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockHeading.SaveToDatabase(ADatabaseName,AModel,AStudy,ASubArea,AScenario: string): boolean;
const OPNAME = 'TSumOutGenericBlockHeading.SaveToDatabase';
var
  LCount       : integer;
  LDataSet     : TAbstractModelDataset;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(
        'INSERT INTO suBlockDescription'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, BlockType,'+
        '  BlockHeading, BlockTitle, ElementID, AnnualWaterDemand, AnnualPowerDemand)'+
        ' VALUES'+
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :BlockType,'+
        '  :BlockHeading, :BlockTitle, :ElementID, :AnnualWaterDemand, :AnnualPowerDemand)');

      LDataSet.ClearQueryParams;
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                          'BlockNumber','LoadCaseNumber','SequenceNumber'],
                         [AModel, AStudy, ASubArea, AScenario,
                          IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber)]);

     if(FBlockType = btNone) then
       LDataSet.SetParamValue('BlockType','NULL',ftInteger)
     else
       LDataSet.SetParamValue('BlockType',IntToStr(Ord(FBlockType)),ftInteger);

     if(FElementID = NullInteger) then
       LDataSet.SetParamValue('ElementID','NULL',ftInteger)
     else
       LDataSet.SetParamValue('ElementID',IntToStr(FElementID),ftInteger);

     if(FAnnualPowerDemand = NullFloat) then
       LDataSet.SetParamValue('AnnualWaterDemand','NULL',ftFloat)
     else
       LDataSet.SetParamValue('AnnualWaterDemand',FloatToStr(FAnnualWaterDemand),ftFloat);

     if(FAnnualPowerDemand = NullFloat) then
       LDataSet.SetParamValue('AnnualPowerDemand','NULL',ftFloat)
     else
       LDataSet.SetParamValue('AnnualPowerDemand',FloatToStr(FAnnualPowerDemand),ftFloat);

     if not FHeading.FInitalised then
       LDataSet.SetParamValue('BlockHeading','NULL',ftString)
     else
       LDataSet.SetParamValue('BlockHeading',FHeading.FData,ftString);

     if not FTitle.FInitalised then
       LDataSet.SetParamValue('BlockTitle','NULL',ftString)
     else
       LDataSet.SetParamValue('BlockTitle',FTitle.FData,ftString);

      LDataSet.ExecSQL;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(
        'INSERT INTO suBlockHeader'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier, LineData)'+
        ' VALUES'+
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier, :LineData)');
      for LCount := 0 to FHeadingLines.Count -1 do
      begin
        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier','LineData'],
                           [AModel, AStudy, ASubArea, AScenario,
                            IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber),
                            IntToStr(LCount+1), FHeadingLines[LCount]]);
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutGenericBlockValues }

procedure TSumOutGenericBlockValues.CreateMemberObjects;
const OPNAME = 'TSumOutGenericBlockValues.CreateMemberObjects';
begin
  inherited;
  try
    FValuesLines :=  TObjectList.Create(True);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockValues.DestroyMemberObjects;
const OPNAME = 'TSumOutGenericBlockValues.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FValuesLines);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutGenericBlockValues.Initialise';
begin
  Result := False;
  try
    FValuesLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockValues.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutGenericBlockValues.ReadFromBlockData';
var
  LCurrentLine: string;
  LLineValues: TValuesLine;
  function StringToValues( ALine: String; ALineValues: TValuesLine): boolean;
  const OPNAME = 'StringToValues';
  var
    LIndex,
    LErrCode: integer;
    LSubStr: string;
    LValue: Double;
  begin
    for LIndex := LowIndex to HighIndex do
    begin
      if(Length(ALine) > 0) then
      begin
        if (LIndex = LowIndex) then
           LSubStr := Copy(ALine,1,ValCountLength)
        else if (LIndex = (LowIndex+1)) then
           LSubStr := Copy(ALine,1,ValYearLength)
        else if (LIndex = (LowIndex+2)) then
           LSubStr := Copy(ALine,1,ValFirstLength)
        else if (LIndex = HighIndex) then
           LSubStr := Copy(ALine,1,ValAvgLength)
        else
           LSubStr := Copy(ALine,1,ValOtherLength);

        Delete(ALine,1,Length(LSubStr));
        Val(LSubStr,LValue,LErrCode);
        if(LErrCode = 0) then
        begin
          ALineValues.FValues[LIndex].FData := LValue;
          ALineValues.FValues[LIndex].FInitalised := True;
        end
        else
        ALineValues.FValues[LIndex].FLength := 0;
      end
      else
      begin
        ALineValues.FValues[LIndex].FLength := 0;
      end;
    end;
    Result := True;
  end;
begin

  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;

    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      if (Pos(ExitLabel1,LCurrentLine) > 0) or (Pos(ExitLabel2,LCurrentLine) > 0) then
        Break;

      LLineValues := TValuesLine.Create(FAppModules);
      LLineValues.ValCountLength := Self.ValCountLength;
      LLineValues.ValYearLength  := Self.ValYearLength;
      LLineValues.ValFirstLength := Self.ValFirstLength;
      LLineValues.ValOtherLength := Self.ValOtherLength;
      LLineValues.ValAvgLength   := Self.ValAvgLength;
      LLineValues.Decimals       := Self.Decimals;
      LLineValues.BlockNumber    := Self.BlockNumber;
      LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
      LLineValues.SequenceNumber := Self.SequenceNumber;
      LLineValues.ExitLabel1     := Self.ExitLabel1;
      LLineValues.ExitLabel2     := Self.ExitLabel2;
      LLineValues.ShowDecimals   := Self.ShowDecimals;

      if LLineValues.Initialise and StringToValues(LCurrentLine,LLineValues) then
        FValuesLines.Add(LLineValues)
      else
       FreeAndNil(LLineValues);

      ABLockData.Lines.Delete(0);
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockValues.SaveToDatabase(
         ADatabaseName, AModel,AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutGenericBlockValues.SaveToDatabase';
var
  LCount,
  LIndex      : integer;
  LDataSet    : TAbstractModelDataset;
  LLineValues : TValuesLine;
  LFieldName  : string;
  LSQLText    : string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        'INSERT INTO suBlockGenericValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', GenericValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', :GenericValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);
      for LCount := 0 to FValuesLines.Count -1 do
      begin

        LLineValues := TValuesLine(FValuesLines.Items[LCount]);
        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                           [AModel, AStudy, ASubArea, AScenario,
                            IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(LCount+1)]);
        for LIndex := LowIndex to HighIndex do
        begin
          if LLineValues.FValues[LIndex].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['GenericValue',LIndex+1]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LLineValues.FValues[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockValues.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutGenericBlockValues.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
  LIndex: integer;
  LColumnName,
  LSQLText: string;
  LLineValues: TValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', GenericValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockGenericValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      FValuesLines.Clear;
      while not LDataSet.DataSet.EOF do
      begin
        LLineValues := TValuesLine.Create(FAppModules);
        LLineValues.ValCountLength := Self.ValCountLength;
        LLineValues.ValYearLength  := Self.ValYearLength;
        LLineValues.ValFirstLength := Self.ValFirstLength;
        LLineValues.ValOtherLength := Self.ValOtherLength;
        LLineValues.ValAvgLength   := Self.ValAvgLength;
        LLineValues.Decimals       := Self.Decimals;
        LLineValues.BlockNumber    := Self.BlockNumber;
        LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
        LLineValues.SequenceNumber := Self.SequenceNumber;
        LLineValues.ExitLabel1     := Self.ExitLabel1;
        LLineValues.ExitLabel2     := Self.ExitLabel2;
        LLineValues.ShowDecimals   := Self.ShowDecimals;

        if LLineValues.Initialise  then
        begin
          for LIndex := LowIndex to HighIndex do
          begin
            LColumnName :=  Format('%s%2.2d',['GenericValue',LIndex+1]);
            if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
            begin
              LLineValues.FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
              LLineValues.FValues[LIndex].FInitalised := True;
            end
            else
              LLineValues.FValues[LIndex].FInitalised := False;
          end;
          FValuesLines.Add(LLineValues);
        end
        else
         FreeAndNil(LLineValues);
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockValues.Reset;
const OPNAME = 'TSumOutGenericBlockValues.Reset';
begin
  inherited;
  try
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockValues.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutGenericBlockValues.SaveToStream';
var
  LLine: string;
  LIndex,
  LCount: integer;
  LValuesLine:TValuesLine;
begin
  try
    for LCount := 0 to FValuesLines.Count - 1 do
    begin
      LLine := '';
      LValuesLine := TValuesLine(FValuesLines[LCount]);
      for LIndex := LowIndex to HighIndex do
      begin
        if LValuesLine.FValues[LIndex].FInitalised then
        begin
          if(LValuesLine.FValues[LIndex].FData >= 1000) and (LValuesLine.FValues[LIndex].FDecimal = 3) then
            LValuesLine.FValues[LIndex].FDecimal := 2;
          LLine := LLine + PadDouble(LValuesLine.FValues[LIndex]);
        end;
      end;
      LLine := LLine + #13#10;
      Stream.Write(Pchar(LLine)^, Length(LLine));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutGenericBlockAvarage }

procedure TSumOutGenericBlockAvarage.CreateMemberObjects;
const OPNAME = 'TSumOutGenericBlockAvarage.CreateMemberObjects';
begin
  inherited;
  try
    FViolationsCount := TInteger.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockAvarage.DestroyMemberObjects;
const OPNAME = 'TSumOutGenericBlockAvarage.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FViolationsCount);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockAvarage.Initialise: boolean;
const OPNAME = 'TSumOutGenericBlockAvarage.Initialise';
begin
  Result := inherited Initialise;
  try
    FViolationsCount.FData := 0;
    FViolationsCount.FInitalised := False;
    FViolationsCount.FLength := 4;
    FViolationsCount.FDecimal := 0;
    FViolationsCount.FDefaultPadding := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockAvarage.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutGenericBlockAvarage.ReadFromBlockData';
var
  LIndex : integer;
  LCurrentLine: string;
  LErrCode: integer;
  LSubStr: string;
  LValue: Integer;

  function StringToValues( ALine: String): boolean;
  const OPNAME = 'StringToValues';
  var
    LIndex,
    LErrCode: integer;
    LSubStr: string;
    LValue: Double;
  begin
    for LIndex := 2 to HighIndex do
    begin
      if(Length(ALine) > 0) then
      begin
        if (LIndex = LowIndex) then
           LSubStr := Copy(ALine,1,ValCountLength)
        else if (LIndex = (LowIndex+1)) then
           LSubStr := Copy(ALine,1,ValYearLength)
        else if (LIndex = (LowIndex+2)) then
           LSubStr := Copy(ALine,1,ValFirstLength)
        else if (LIndex = HighIndex) then
           LSubStr := Copy(ALine,1,ValAvgLength)
        else
           LSubStr := Copy(ALine,1,ValOtherLength);

        Delete(ALine,1,Length(LSubStr));
        Val(LSubStr,LValue,LErrCode);
        if(LErrCode = 0) then
        begin
          FValues[LIndex].FData := LValue;
          FValues[LIndex].FInitalised := True;
        end
        else
          FValues[LIndex].FLength := 0;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LIndex := 0;
    while(ABLockData.Lines.Count > 0) do
    begin
      LIndex := LIndex + 1;

      LCurrentLine := ABLockData.Lines[0];
      ABLockData.Lines.Delete(0);

      if (LIndex = 2) then
      begin
        Delete(LCurrentLine,1,(ValCountLength +  ValYearLength));
        Result :=  StringToValues(LCurrentLine);
      end
      else if (LIndex = 4) then
      begin
        LSubStr  := Copy(LCurrentLine,32,4);
        Val(LSubStr,LValue,LErrCode);
        if(LErrCode = 0) then
        begin
          FViolationsCount.FData := LValue;
          FViolationsCount.FInitalised := True;
        end
        else
          Result := False;;
      end ;
    end;
    Result := Result and (LIndex >= 2) and (ABLockData.Lines.Count = 0);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockAvarage.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutGenericBlockAvarage.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
  LIndex: integer;
  LColumnName,
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber,Violations';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', AverageValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockAvarageValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.EOF do
      begin
        for LIndex := LowIndex to HighIndex do
        begin
          LColumnName :=  Format('%s%2.2d',['AverageValue',LIndex+1]);
          if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
          begin
            FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
            FValues[LIndex].FInitalised := True;
          end;
        end;
        if not LDataSet.DataSet.FieldByName('Violations').IsNull then
        begin
          FViolationsCount.FData := LDataSet.DataSet.FieldByName('Violations').AsInteger;
          FViolationsCount.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutGenericBlockAvarage.SaveToDatabase(ADatabaseName, AModel,
  AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutGenericBlockAvarage.SaveToDatabase';
var
  LIndex: integer;
  LDataSet : TAbstractModelDataset;
  LFieldName: string;
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        'INSERT INTO suBlockAvarageValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber';
      for LIndex := LowIndex to HighIndex do
        LSQLText := LSQLText + Format('%s%2.2d',[', AverageValue',LIndex+1]);
      LSQLText := LSQLText + ',Violations) VALUES';
      LSQLText := LSQLText +
      ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber';
      for LIndex := LowIndex to HighIndex do
        LSQLText := LSQLText + Format('%s%2.2d',[', :AverageValue',LIndex+1]);
      LSQLText := LSQLText + ',:Violations)';

      LDataSet.SetSQL(LSQLText);

      LDataSet.ClearQueryParams;
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                          'BlockNumber','LoadCaseNumber','SequenceNumber'],
                         [AModel, AStudy, ASubArea, AScenario, 
                          IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber)]);
      for LIndex := LowIndex to HighIndex do
      begin
        if FValues[LIndex].FInitalised then
        begin
          LFieldName := Format('%s%2.2d',['AverageValue',LIndex+1]);
          LDataSet.SetParams([LFieldName], [FloatToStr(FValues[LIndex].FData)]);
        end;
      end;
      if FViolationsCount.FInitalised then
        LDataSet.SetParams(['Violations'], [IntToStr(FViolationsCount.FData)]);

      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutGenericBlockAvarage.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutGenericBlockAvarage.SaveToStream';
var
  LLine: string;
  LIndex: integer;
begin
  try
    LLine := VALUESEXITLABEL + #13#10;
    Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

    LLine := AVARAGELABEL;
    for LIndex := 2 to High(FValues) do
    begin
      if FValues[LIndex].FInitalised then
      begin
        if(FValues[LIndex].FData >= 1000) and (FValues[LIndex].FDecimal = 3) then
            FValues[LIndex].FDecimal := 2;
        LLine := LLine + PadDouble(FValues[LIndex]);
      end;
    end;

    LLine := LLine + #13#10;
    Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

    if FViolationsCount.FInitalised then
    begin
      LLine := ' ' + #13#10;
      Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

      LLine := VIOLATIONSLABEL + PadInt(FViolationsCount)+ #13#10;
      Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

      LLine := ' ' + #13#10;
      Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TSumOutFile.CreateMemberObjects;
const OPNAME = 'TSumOutFile.CreateMemberObjects';
begin
  try
    FFileName        := '';
    FFileEOF         := False;
    FFileStream      := nil;
    FCurrentPosition := -1;

    FLines           := TStringList.Create;

    // Allocate the memory buffers.
    GetMem(FReadBlock, BufferSize+1);
    GetMem(FBeforeExraChars, BufferSize+1);
    GetMem(FAfterExraChars, LineLength+1);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutFile.DestroyMemberObjects;
const OPNAME = 'TSumOutFile.DestroyMemberObjects';
begin
  try
    FreeAndNil(FLines);
    FreeAndNil(FFileStream);

    // Allocate the memory buffers.
    FreeMem(FReadBlock);
    FreeMem(FBeforeExraChars);
    FreeMem(FAfterExraChars);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFile.Initialise: boolean;
const OPNAME = 'TSumOutFile.Initialise';
Begin
  Result := False;
  try
    FLines.Clear;
    ZeroMemory(@FReadBlock[0],BufferSize);
    ZeroMemory(@FBeforeExraChars[0],BufferSize);
    ZeroMemory(@FAfterExraChars[0],LineLength );
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutFile.Reset;
const OPNAME = 'TSumOutFile.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutFile.ClearReadBlock;
const OPNAME = 'TSumOutFile.ClearReadBlock';
Begin
  try
    ZeroMemory(@FReadBlock[0],BufferSize);
    ZeroMemory(@FBeforeExraChars[0],BufferSize);
    System.AnsiStrings.StrMove(FBeforeExraChars,FAfterExraChars,System.AnsiStrings.StrLen(FAfterExraChars)+1);
    ZeroMemory(@FAfterExraChars[0],LineLength );
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutFile.ReadBlock;
const OPNAME = 'TSumOutFile.ReadBlock';
var
  LStartOfExtraChars: PAnsiChar;
  LActualRead: cardinal;
  LTotalRead: integer;
  LNewBlockData: TStringList;
  LIndex: integer;
  LFileEOF: boolean;
Begin
  try
    if FFileEOF then Exit;

    ClearReadBlock;
    LActualRead := FFileStream.Read(FReadBlock^,BlockSize);
    LTotalRead  := LActualRead;
    for LIndex := 0 to LTotalRead-1 do
    begin
      if(FReadBlock[LIndex] = Char(0)) then
        FReadBlock[LIndex] := Char(32);
    end;

    LFileEOF := (LTotalRead <= 0);

    if not LFileEOF then
    begin
      //Remove Extra chars that are not a full line.
      LStartOfExtraChars := @FReadBlock[LTotalRead - 1];
      LStartOfExtraChars := LineStart(FReadBlock,LStartOfExtraChars);
      System.AnsiStrings.StrMove(FAfterExraChars,LStartOfExtraChars,System.AnsiStrings.StrLen(LStartOfExtraChars));
      LStartOfExtraChars[0] := #0;
    end;

    //Add Extra chars that are not a full line that was read previously.
    if(System.AnsiStrings.StrLen(FBeforeExraChars) > 0) then
    begin
      System.AnsiStrings.StrCat(FBeforeExraChars, FReadBlock);
      FReadBlock[0] := #0;
      System.AnsiStrings.StrCopy(FReadBlock,FBeforeExraChars);
    end;

    //Add new lines to string list(in case some were left eg. on peek line call).
    LNewBlockData := TStringList.Create;
    try
      LNewBlockData.Text := string(FReadBlock);                  //SetText(PChar(FReadBlock));
      for LIndex := 0 to LNewBlockData.Count - 1 do
        FLines.Add(LNewBlockData.Strings[LIndex]);
      LNewBlockData.Clear;

      if LFileEOF and (System.AnsiStrings.strLen(FAfterExraChars) > 0) then
      begin
        LNewBlockData.Text := string(FReadBlock);             //LNewBlockData.SetText(PChar(FAfterExraChars));
        for LIndex := 0 to LNewBlockData.Count - 1 do
         FLines.Add(LNewBlockData.Strings[LIndex]);
        LNewBlockData.Clear;
      end;

      FFileEOF := LFileEOF and (Self.FLines.Count = 0);
    finally
      FreeAndNil(LNewBlockData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFile.GetCurrentLine: string;
const OPNAME = 'TSumOutFile.GetCurrentLine';
Begin
  Result := '';
  try
    if (FLines.Count = 0) then
      ReadBlock;

    if FLines.Count > 0 then
    begin
      Result := FLines[0];
      FLines.Delete(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFile.PeekNextLine: string;
const OPNAME = 'TSumOutFile.PeekNextLine';
Begin
  Result := '';
  try
    if (FLines.Count < 2) then
      ReadBlock;

    if FLines.Count > 1 then
      Result := FLines[1];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFile.AddLine(Aline: string): boolean;
const OPNAME = 'TSumOutFile.AddLine';
Begin
  Result := False;
  try
    FLines.Add(Aline);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFile.AppendToFile: boolean;
const OPNAME = 'TSumOutFile.AppendToFile';
var
  LBlockText: PChar;
Begin
  Result := False;
  try
    LBlockText := FLines.GetText;
    try
      FFileStream.Write(LBlockText,StrLen(LBlockText));
    finally
      FreeMem(LBlockText);
    end;

    Result:= True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFile.OpenTheFile(AFileName: string; AOpenAction: TOpenFileAction): boolean;
const OPNAME = 'TSumOutFile.OpenTheFile';
Begin
  Result := False;
  try
    try
      case AOpenAction of
        ofaRead  : FFileStream := TFileStream.Create(AFileName,fmOpenRead, fmShareDenyNone	);
        ofaWrite : FFileStream := TFileStream.Create(AFileName,fmCreate,fmShareExclusive);
      end;
      Result := True;
    except
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDataBlock }

procedure TDataBlock.CreateMemberObjects;
const OPNAME    = 'TDataBlock.CreateMemberObjects';
begin
  inherited;
  try
    FDataContainer := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataBlock.DestroyMemberObjects;
const OPNAME    = 'TDataBlock.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDataContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.ReadBlock(ACurrentFile: TSumOutFile; var ABlockSkipped : boolean): boolean;
const OPNAME = 'TDataBlock.ReadBlock';
      EXITLABEL1 = 'ACRES INTERNATIONAL LIMITED';
      EXITLABEL2 = 'MULTI-PURPOSE/MULTI-RESERVOIR SIMULATION PROGRAM';

      SKIPBLOCKLABEL1 = 'UPSTREAM   LOCAL  EVAP0-   POWER  SPILL   OTHER      WATER INSTALLED AVERAGE    ENERGY   STORAGE    ADJUSTED';
      SKIPBLOCKLABEL2 = 'SUMMARY OF OPERATING COSTS';
      SKIPBLOCKLABEL3 = 'YEAR      DAM    PUMPING     PURIF    RECLAM COST OF   COST OF       TOTAL';
      SKIPBLOCKLABEL4 = 'ECONOMIC ANALYSIS';
      SKIPBLOCKLABEL5 = 'YEAR      INVESTMENT COSTS   ANNUAL CAPITAL COST  OPERATING   TOTAL      P.W.     C.P.W.   AVERAGE   AVERAGE    C.P.W.   C.P.W';
      SKIPBLOCKLABEL6 = 'SUMMARY OF WATER SUPPLY OPERATIONS';
      SKIPBLOCKLABEL7 = 'WATER SUPPLY REQUIREMENTS';
var
  LCurrentLine: string;
  LNextLine: string;
  LUpperCaseLine: string;
begin
  Result := False;
  try
    if not Assigned(ACurrentFile) then
      raise Exception.Create('File stream object parameter is not yet assigned.');

    ABlockSkipped := False;
    FDataContainer.Clear;

    while not ACurrentFile.EOF do
    begin
      LNextLine := ACurrentFile.PeekNextLine;
      LUpperCaseLine := UpperCase(LNextLine);
      LCurrentLine := ACurrentFile.GetCurrentLine;
      if (Pos('Hist',LCurrentLine)>0) or (Pos('Hist',LNextLine)>0) then
        FRunType := rtHistoric;
      if (Pos('Stoc',LCurrentLine)>0) or (Pos('Stoc',LNextLine)>0) then
        FRunType := rtStochastic;
      FDataContainer.Add(LCurrentLine);
      if (Pos(EXITLABEL1,LUpperCaseLine) > 0) or (Pos(EXITLABEL2,LUpperCaseLine) > 0)then
        Break;

      //Should the whole block be skipped?
      if (Pos(SKIPBLOCKLABEL1,LUpperCaseLine) > 0) or (Pos(SKIPBLOCKLABEL2,LUpperCaseLine) > 0) or
         (Pos(SKIPBLOCKLABEL3,LUpperCaseLine) > 0) or (Pos(SKIPBLOCKLABEL4,LUpperCaseLine) > 0) or
         (Pos(SKIPBLOCKLABEL5,LUpperCaseLine) > 0) or (Pos(SKIPBLOCKLABEL6,LUpperCaseLine) > 0) or
         (Pos(SKIPBLOCKLABEL7,LUpperCaseLine) > 0) then
      begin
        while not ACurrentFile.EOF do
        begin
          LUpperCaseLine := UpperCase(ACurrentFile.PeekNextLine);
          if (Pos(EXITLABEL1,LUpperCaseLine) > 0) or (Pos(EXITLABEL2,LUpperCaseLine) > 0)then
              Break;
          ACurrentFile.GetCurrentLine;
        end;
        ABlockSkipped := True;
        FDataContainer.Clear;
        Result := True;
        Exit;
      end;
    end;

    if(FAppModules.Model.ModelName = CPlanning) then
      FModelType := mtPlanning
    else
      FModelType := mtYield;
    FBlockType := GetOutputDataType;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.GetOutputDataType: TOutputDataType;
const OPNAME    = 'TDataBlock.GetOutputDataType';
var
  LLinesText: string;
begin
  Result := btNone;
  try
    if (FDataContainer.Count = 0) then
      raise Exception.Create('The read block is empty.');

    LLinesText := FDataContainer.Text;

    if((Pos(lblMonthEndReservoirVolume,LLinesText) > 0) or
       (Pos(lblPlaningMonthEndReservoirVolume,LLinesText) > 0)) then
      Result := btMonthEndReservoirVolume

    else if((Pos(lblMonthEndReservoirElevation,LLinesText) > 0) or
            (Pos(lblPlaningMonthEndReservoirElevation,LLinesText) > 0)) then
      Result := btMonthEndReservoirElevation

    else if((Pos(lblNetBasinRunoffIntoResArea,LLinesText) > 0)  or
            (Pos(lblPlaningNetBasinRunoffIntoResArea,LLinesText) > 0)) then
      Result := btNetBasinRunoffIntoResArea

    else if((Pos(lblRainfallOnReservoirSurface,LLinesText) > 0) or
            (Pos(lblPlaningRainfallOnReservoirSurface,LLinesText) > 0)) then
      Result := btRainfallOnReservoirSurface

    else if((Pos(lblGrossEvaporationLossFromReservoir,LLinesText) > 0) or
            (Pos(lblPlaningGrossEvaporationLossFromReservoir,LLinesText) > 0)) then
      Result := btGrossEvaporationLossFromReservoir

    else if((Pos(lblMonthlyAveragePowerFlow,LLinesText) > 0) or
            (Pos(lblPlaningMonthlyAveragePowerFlow,LLinesText) > 0)) then
      Result := btMonthlyAveragePowerFlow

    else if((Pos(lblMonthlyAverageSpillFlow,LLinesText) > 0)  or
            (Pos(lblPlaningMonthlyAverageSpillFlow,LLinesText) > 0)) then
      Result := btMonthlyAverageSpillFlow

    else if((Pos(lblMonthlyAverageStackedCapacity,LLinesText) > 0) or
            (Pos(lblPlaningMonthlyAverageStackedCapacity,LLinesText) > 0)) then
      Result := btMonthlyAverageStackedCapacity

    else if((Pos(lblMonthlyAverageStackedEnergy,LLinesText) > 0) or
            (Pos(lblPlaningMonthlyAverageStackedEnergy,LLinesText) > 0)) then
      Result := btMonthlyAverageStackedEnergy

    else if((Pos(lblMonthlyAverageIrrigationDeficits,LLinesText) > 0)  or
            (Pos(lblPlaningMonthlyAverageIrrigationDeficits,LLinesText) > 0)) then
      Result := btMonthlyAverageIrrigationDeficits

    else if((Pos(lblMonthlyAverageChannelFlow,LLinesText) > 0) or
            (Pos(lblPlaningMonthlyAverageChannelFlow,LLinesText) > 0)) then
      Result := btMonthlyAverageChannelFlow

    else if((Pos(lblMonthlyPumpingEnergy,LLinesText) > 0) or
            (Pos(lblPlaningMonthlyPumpingEnergy,LLinesText) > 0)) then
      Result := btMonthlyPumpingEnergy

    else if(Pos(lblYieldFailurePerYearPerSequence,LLinesText) > 0) then
      Result := btYieldFailurePerYearPerSequence

    else if(Pos(lblOutputSummary,LLinesText) > 0) then
      Result := btOutputSummary

    else if(Pos(lblAnualFirmYieldDemands,LLinesText) > 0) then
      Result := btAnualFirmYieldDemands

    else if(Pos(lblAnualFirmEnergyDemands,LLinesText) > 0) then
      Result := btAnualFirmEnergyDemands

    else if(Pos(lblAnualFirmSelectedYieldDemands,LLinesText) > 0) then
      Result := btAnualFirmSelectedYieldDemands

    else if(Pos(lblAnualNonFirmYieldDemands,LLinesText) > 0) then
      Result := btAnualNonFirmYieldDemands

    else if(Pos(lblAnualSecondaryYieldDemands,LLinesText) > 0) then
      Result := btAnualSecondaryYieldDemands

    else if(Pos(lblAnualTotalSystemPumpingEnergy,LLinesText) > 0) then
      Result := btAnualTotalSystemPumpingEnergy

    else if(Pos(lblAnualFullSystemSupplyVolume,LLinesText) > 0) then
      Result := btAnualFullSystemSupplyVolume

    else if(Pos(lblAnualAverageInflow,LLinesText) > 0) then
      Result := btAnualAverageInflow

    else if(Pos(lblSequencesWithFailures,LLinesText) > 0) then
      Result := btSequencesWithFailures

    else if ( Pos ( lblNumberOfFailureSequence, LLinesText ) > 0 ) then
      Result := btNumberOfFailureSequence

    else if ( Pos ( lblFirmYieldRecurrenceInterval,LLinesText ) > 0 ) then
      Result := btFirmYieldRecurrenceInterval

    else if(Pos(lblCriticalPeriodsNumber,LLinesText) > 0) then
      Result := btCriticalPeriodsNumber

    else if(Pos(lblCriticalPeriodsLength,LLinesText) > 0) then
      Result := btCriticalPeriodsLength

    else if(Pos(lblCriticalPeriodsDeficit,LLinesText) > 0) then
      Result := btCriticalPeriodsDeficit

    else if(Pos(lblCriticalPeriodsAvarage,LLinesText) > 0) then
      Result := btCriticalPeriodsAvarage

   else if(Pos(lblDeficitPropotion,LLinesText) > 0) then
      Result := btDeficitPropotion;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.GetAnnualPowerDemand: double;
const OPNAME    = 'TDataBlock.GetAnnualPowerDemand';
      C_SearchStr = 'Annual Power Demand =';
var
  LValue,
  LLinesText: string;
  LIndex: integer;
begin
  Result := NullFloat;
  try
    if (FDataContainer.Count = 0) then
      raise Exception.Create('The read block is empty.');

    if(FModelType = mtPlanning) then
      Result := 0.0
    else
    begin
      LLinesText := FDataContainer.Text;
      LIndex := Pos(C_SearchStr,LLinesText);
      if(LIndex > 0) then
      begin
        LValue := Copy(LLinesText,LIndex+Length(C_SearchStr),11);
        LValue := Trim(LValue);
        Result := StrToFloatDef(LValue,NullFloat);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.GetAnnualWaterDemand: double;
const OPNAME    = 'TDataBlock.GetAnnualWaterDemand';
      C_SearchStr = 'Annual Water Demand =';
var
  LValue,
  LLinesText: string;
  LIndex: integer;
begin
  Result := NullFloat;
  try
    if (FDataContainer.Count = 0) then
      raise Exception.Create('The read block is empty.');

    if(FModelType = mtPlanning) then
      Result := 0.0
    else
    begin
      LLinesText := FDataContainer.Text;

      LIndex := Pos(C_SearchStr,LLinesText);
      if(LIndex > 0) then
      begin
        LValue := Copy(LLinesText,LIndex+Length(C_SearchStr),11);
        LValue := Trim(LValue);
        Result := StrToFloatDef(LValue,NullFloat);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.GetLoadCaseNumber: integer;
const OPNAME    = 'TDataBlock.GetLoadCaseNumber';
      C_SearchStr = 'Load Case =';
var
  LValue,
  LLinesText: string;
  LIndex: integer;
begin
  Result := 1;
  try
    if (FDataContainer.Count = 0) then
      raise Exception.Create('The read block is empty.');
    if(FModelType = mtPlanning) then
      Result := 1
    else
    begin
      LLinesText := FDataContainer.Text;
      LIndex := Pos(C_SearchStr,LLinesText);
      if(LIndex > 0) then
      begin
        LValue := Copy(LLinesText,LIndex+Length(C_SearchStr),4);
        LValue := Trim(LValue);
        Result := StrToIntDef(LValue,NullInteger);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.GetSequenceNumber: integer;
const OPNAME    = 'TDataBlock.GetSequenceNumber';
      C_SearchStr = 'Stochastic Sequence =';
var
  LValue,
  LLinesText: string;
  LIndex: integer;
begin
  Result := 1;
  try
    if (FDataContainer.Count = 0) then
      raise Exception.Create('The read block is empty.');

    if(FModelType = mtPlanning) then
      Result := 0
    else
    begin
      LLinesText := FDataContainer.Text;
      LIndex := Pos(C_SearchStr,LLinesText);
      if(LIndex > 0) then
      begin
        LValue := Copy(LLinesText,LIndex+Length(C_SearchStr),5);
        LValue := Trim(LValue);
        Result := StrToIntDef(LValue,1);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.GetElementID: integer;
const OPNAME    = 'TDataBlock.GetElementID';
      C_SearchStr = 'Stochastic Sequence =';
var
  LValue,
  LLinesText: string;
begin
  Result := NullInteger;
  try
    if (FDataContainer.Count = 0) then
      raise Exception.Create('The read block is empty.');
    if(FModelType = mtPlanning) then
      Result := 0
    else
    begin
      if(FDataContainer.Count >= 8) then
      begin
        LLinesText := FDataContainer.Strings[7];
        LValue := Trim(Copy(LLinesText,94,4));
        Result := StrToIntDef(LValue,Result);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.GetElementName: string;
const OPNAME    = 'TDataBlock.GetElementName';
var
  LLinesText: string;
begin
  Result := '';
  try
    if (FDataContainer.Count = 0) then
      raise Exception.Create('The read block is empty.');

    if(FDataContainer.Count >= 8) then
    begin
      if(FModelType = mtPlanning) then
        LLinesText := FDataContainer.Strings[6]
      else
        LLinesText := FDataContainer.Strings[7];

      if(Pos(lblMonthlyAveragePowerFlow,LLinesText) = 2) then
        Result := Trim(Copy(LLinesText,36,80))
      else if(Pos(lblMonthlyAverageSpillFlow,LLinesText) = 2) then
        Result := Trim(Copy(LLinesText,36,80))
      else if(Pos(lblMonthlyAverageStackedCapacity,LLinesText) = 2) then
        Result := Trim(Copy(LLinesText,40,80))
      else if(Pos(lblMonthlyAverageStackedEnergy,LLinesText) = 2) then
        Result := Trim(Copy(LLinesText,50,80))
      else if(Pos(lblMonthlyAverageIrrigationDeficits,LLinesText) = 2) then
        Result := Trim(Copy(LLinesText,46,80))
      else if(Pos(lblMonthlyPumpingEnergy,LLinesText) = 2) then
        Result := Trim(Copy(LLinesText,34,80))
      else
        Result := Trim(Copy(LLinesText,62,36));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataBlock.Reset;
const OPNAME    = 'TDataBlock.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataBlock.Initialise: boolean;
const OPNAME    = 'TDataBlock.Initialise';
begin
  Result := inherited Initialise;
  try
    FDataContainer.Clear;
    FBlockNumber   := NullInteger;
    FBlockType     := btNone;
    FRunType       := rtUnKnown;
    FModelType     := mtUnKnown;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TValuesLine }

procedure TValuesLine.CreateMemberObjects;
const OPNAME = 'TValuesLine.CreateMemberObjects';
var
 LIndex: integer;
 LCurrentValue: TDouble;
begin
  inherited;
  try
    FValCountLength := 0;
    FValYearLength  := 0;
    FValFirstLength := 0;
    FValOtherLength := 0;
    FValAvgLength   := 0;
    FDecimals       := 0;
    FBlockNumber    := NullInteger;
    LoadCaseNumber  := NullInteger;
    SequenceNumber  := NullInteger;
    FExitLabel1     := '';
    FExitLabel2     := '';
    FShowDecimals   := True;

    FReservoirName := TString.Create;
    for LIndex := LowIndex to HighIndex do
    begin
       LCurrentValue := TDouble.Create;
       FValues[LIndex] := LCurrentValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TValuesLine.DestroyMemberObjects;
const OPNAME = 'TValuesLine.DestroyMemberObjects';
var
 LIndex: integer;
begin
  inherited;
  try
    FreeAndNil(FReservoirName);
    for LIndex := LowIndex to HighIndex do
    begin
      FValues[LIndex].Free;
      FValues[LIndex] := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TValuesLine.Initialise: boolean;
const OPNAME = 'TValuesLine.Initialise';
var
 LIndex: integer;
begin
  Result := False;
  try
    FReservoirName.FData := '';
    FReservoirName.FLength := 0;
    FReservoirName.FDefaultPadding := True;
    FReservoirName.FInitalised := False;

    for LIndex := LowIndex to HighIndex do
    begin
      FValues[LIndex].FData := 0.0;
      FValues[LIndex].FInitalised := False;
      FValues[LIndex].FDefaultPadding := True;

      if (LIndex = LowIndex) then
      begin
        FValues[LIndex].FLength := ValCountLength;
        FValues[LIndex].FDecimal := 0;
        FValues[LIndex].ShowDecimalPoint := False;
      end
      else if (LIndex = (LowIndex+1)) then
      begin
        FValues[LIndex].FLength := ValYearLength;
        FValues[LIndex].FDecimal := 0;
        FValues[LIndex].ShowDecimalPoint := False;
      end
      else if (LIndex = (LowIndex+2)) then
      begin
        FValues[LIndex].FLength := ValFirstLength;
        FValues[LIndex].FDecimal := FDecimals;
        FValues[LIndex].ShowDecimalPoint := True;
      end
      else if (LIndex = HighIndex) then
      begin
        FValues[LIndex].FLength := ValAvgLength;
        FValues[LIndex].FDecimal := FDecimals;
        FValues[LIndex].ShowDecimalPoint := ShowDecimals;
      end
      else
      begin
        FValues[LIndex].FLength := ValOtherLength;
        FValues[LIndex].FDecimal := FDecimals;
        FValues[LIndex].ShowDecimalPoint := ShowDecimals;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TValuesLine.Assign(ASource: TValuesLine);
const OPNAME = 'TValuesLine.CreateMemberObjects';
var
 LIndex: integer;
begin
  try
    if not Assigned(ASource) then
      raise Exception.Create('Source parameter is not assigned.');

    Self.FValCountLength := ASource.ValCountLength;
    Self.FValYearLength  := ASource.ValYearLength;
    Self.FValFirstLength := ASource.ValFirstLength;
    Self.FValOtherLength := ASource.ValOtherLength;
    Self.FValAvgLength   := ASource.ValAvgLength;
    Self.FDecimals       := ASource.Decimals;
    Self.FBlockNumber    := ASource.BlockNumber;
    Self.LoadCaseNumber  := ASource.LoadCaseNumber;
    Self.SequenceNumber  := ASource.SequenceNumber;
    Self.FExitLabel1     := ASource.ExitLabel1;
    Self.FExitLabel2     := ASource.ExitLabel2;
    Self.FShowDecimals   := ASource.ShowDecimals;

    Self.FReservoirName.FData           := ASource.FReservoirName.FData;
    Self.FReservoirName.FInitalised     := ASource.FReservoirName.FInitalised;
    Self.FReservoirName.FLength         := ASource.FReservoirName.FLength;
    Self.FReservoirName.FDecimal        := ASource.FReservoirName.FDecimal;
    Self.FReservoirName.FDefaultPadding := ASource.FReservoirName.FDefaultPadding;

    for LIndex := LowIndex to HighIndex do
    begin
       Self.FValues[LIndex].FData            := ASource.FValues[LIndex].FData;
       Self.FValues[LIndex].FInitalised      := ASource.FValues[LIndex].FInitalised;
       Self.FValues[LIndex].FLength          := ASource.FValues[LIndex].FLength;
       Self.FValues[LIndex].FDecimal         := ASource.FValues[LIndex].FDecimal;
       Self.FValues[LIndex].FDefaultPadding  := ASource.FValues[LIndex].FDefaultPadding;
       Self.FValues[LIndex].ShowDecimalPoint := ASource.FValues[LIndex].ShowDecimalPoint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TValuesLine.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TValuesLine.ReadFromBlockData';
begin
  Result := False;
  try
    FBlockType := ABLockData.BlockType;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TValuesLine.Reset;
const OPNAME = 'TValuesLine.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TValuesLine.SaveToStream(Stream: TStream);
const OPNAME = 'TValuesLine.SaveToStream';
begin
   //Do nothing at this level;
end;

function TValuesLine.ReadBlockIDs(ADataset: TDataset): boolean;
const OPNAME = 'TValuesLine.ReadBlockIDs';
var
  LInt: integer;
Begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');

    if not (ADataset.Bof and ADataset.Eof) then
    begin
      if not ADataset.FieldByName('BlockType').IsNull then
      begin
        LInt := ADataset.FieldByName('BlockType').AsInteger;
        FBlockType := TOutputDataType(LInt);
      end;
      if not ADataset.FieldByName('BlockNumber').IsNull then
      begin
        FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
      end;
      if not ADataset.FieldByName('LoadCaseNumber').IsNull then
      begin
        FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
      end;
      if not ADataset.FieldByName('SequenceNumber').IsNull then
      begin
        FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TValuesLine.ReadFromDatabase(ADatabaseName: string; ADataset: TDataset): boolean;
const OPNAME = 'TValuesLine.ReadFromDatabase';
Begin
  Result := False;
  try
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TValuesLine.SaveToDatabase(ADatabaseName, AModel, AStudy,ASubArea, AScenario: string): boolean;
const OPNAME = 'TValuesLine.SaveToDatabase';
Begin
  Result := False;
  try
     Result := True;
    //Do nothing at this level;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutMonthlyBlockHeading }

function TSumOutMonthlyBlockHeading.Initialise: boolean;
const OPNAME = 'TSumOutMonthlyBlockHeading.Initialise';
Begin
  Result := False;
  try
    ExitLabel1  := HEADINGEXITLABEL;
    ExitLabel2  := HEADINGEXITLABEL;
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutMonthlyBlockValues }

function TSumOutMonthlyBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutMonthlyBlockValues.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 09;
    ValYearLength  := 07;
    ValFirstLength := 13;
    ValOtherLength := 08;
    ValAvgLength   := 10;
    Decimals       := 3;
    ShowDecimals   := True;
    ExitLabel1     := VALUESEXITLABEL;
    ExitLabel2     := VALUESEXITLABEL;
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutMonthlyBlockAvarage }

function TSumOutMonthlyBlockAvarage.Initialise: boolean;
const OPNAME = 'TSumOutMonthlyBlockAvarage.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 09;
    ValYearLength  := 07;
    ValFirstLength := 13;
    ValOtherLength := 08;
    ValAvgLength   := 10;
    Decimals       := 3;
    ShowDecimals    := True;
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutYieldFailureBlockHeading }

function TSumOutYieldFailureBlockHeading.Initialise: boolean;
const OPNAME = 'TSumOutYieldFailureBlockHeading.Initialise';
Begin
  Result := False;
  try
    ExitLabel1  := YIELDFAILUREHEADINGEXITLABEL;
    ExitLabel2  := HEADINGEXITLABEL;
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutYieldFailureBlockValues }

function TSumOutYieldFailureBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutYieldFailureBlockValues.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 05;
    ValYearLength  := 06;
    ValFirstLength := 03;
    ValOtherLength := 03;
    ValAvgLength   := 03;
    Decimals       := 0;
    ShowDecimals   := False;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutOutputSummaryBlockHeading }

function TSumOutOutputSummaryBlockHeading.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockHeading.ReadFromBlockData';
var
  LCurrentLine: string;
  LEmptyLinesCount: integer;
begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LEmptyLinesCount := 0;
    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      FHeadingLines.Add(LCurrentLine);
      ABLockData.Lines.Delete(0);
      if(Trim(LCurrentLine) = '') then
        LEmptyLinesCount := LEmptyLinesCount + 1;
      if (LEmptyLinesCount = 3) then
        Break;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutOutputSummaryBlockValues }

function TSumOutOutputSummaryBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutOutputSummaryBlockValues.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 15;
    ValYearLength  := 8;
    ValFirstLength := 8;
    ValOtherLength := 10;
    ValAvgLength   := 11;
    Decimals       := 3;
    ShowDecimals   := True;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockValues.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockValues.ReadFromBlockData';
var
  LCurrentLine: string;
  LLineValues: TValuesLine;
  function StringToValues( ALine: String; ALineValues: TValuesLine): boolean;
  const OPNAME = 'StringToValues';
  var
    LIndex,
    LErrCode: integer;
    LSubStr: string;
    LValue: Double;
  begin
    for LIndex := LowIndex to HighIndex do
    begin
      If (LIndex = 1) then
         Continue;
      if(Length(ALine) > 0) then
      begin
        LSubStr := '';
        if (LIndex = LowIndex) then
           LSubStr := Copy(ALine,1,15)
        else if (LIndex < (2+7)) then
           LSubStr := Copy(ALine,1,8)
        else if (LIndex < (2+8)) then
           LSubStr := Copy(ALine,1,10)
        else if (LIndex < (2+9)) then
           LSubStr := Copy(ALine,1,8)
        else if (LIndex < (2+11)) then
           LSubStr := Copy(ALine,1,10)
        else if (LIndex < (2+12)) then
           LSubStr := Copy(ALine,1,11);

        if(LSubStr <> '') then
        begin
          Delete(ALine,1,Length(LSubStr));

          if(LIndex = LowIndex) then
          begin
            ALineValues.FReservoirName.FData := LSubStr;
            ALineValues.FReservoirName.FInitalised := true;
            ALineValues.FReservoirName.FLength := 15;
          end
          else
          begin
            Val(LSubStr,LValue,LErrCode);
            if(LErrCode = 0) then
            begin
              ALineValues.FValues[LIndex-2].FData := LValue;
              ALineValues.FValues[LIndex-2].FInitalised := True;
              ALineValues.FValues[LIndex-2].FLength := Length(LSubStr);
              ALineValues.FValues[LIndex-2].FDecimal := 3;
              ALineValues.FValues[LIndex-2].ShowDecimalPoint := True;

            end
            else
             ALineValues.FValues[LIndex-2].FLength := 0;
          end;
        end
        else
        begin
          ALineValues.FValues[LIndex-2].FLength := 0;
        end;
      end
      else
      begin
        ALineValues.FValues[LIndex-2].FLength := 0;
      end;
    end;
    Result := True;
  end;
begin

  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      if (LCurrentLine = '') then
        Break;

      LLineValues := TValuesLine.Create(FAppModules);
      LLineValues.ValCountLength := Self.ValCountLength;
      LLineValues.ValYearLength  := Self.ValYearLength;
      LLineValues.ValFirstLength := Self.ValFirstLength;
      LLineValues.ValOtherLength := Self.ValOtherLength;
      LLineValues.ValAvgLength   := Self.ValAvgLength;
      LLineValues.Decimals       := Self.Decimals;
      LLineValues.BlockNumber    := Self.BlockNumber;
      LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
      LLineValues.SequenceNumber := Self.SequenceNumber;
      LLineValues.ExitLabel1     := Self.ExitLabel1;
      LLineValues.ExitLabel2     := Self.ExitLabel2;
      LLineValues.ShowDecimals   := Self.ShowDecimals;

      if LLineValues.Initialise and StringToValues(LCurrentLine,LLineValues) then
        FValuesLines.Add(LLineValues)
      else
       FreeAndNil(LLineValues);

      ABLockData.Lines.Delete(0);
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockValues.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockValues.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
  LLength,
  LIndex: integer;
  LColumnName,
  LSQLText: string;
  LLineValues: TValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier,ReserviorName';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', OutputSummaryValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockOutputSummaryValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' AND Identifier <> '+ IntToStr(TotalAVGLine)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      FValuesLines.Clear;
      while not LDataSet.DataSet.EOF do
      begin
        LLineValues := TValuesLine.Create(FAppModules);
        LLineValues.ValCountLength := Self.ValCountLength;
        LLineValues.ValYearLength  := Self.ValYearLength;
        LLineValues.ValFirstLength := Self.ValFirstLength;
        LLineValues.ValOtherLength := Self.ValOtherLength;
        LLineValues.ValAvgLength   := Self.ValAvgLength;
        LLineValues.Decimals       := Self.Decimals;
        LLineValues.BlockNumber    := Self.BlockNumber;
        LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
        LLineValues.SequenceNumber := Self.SequenceNumber;
        LLineValues.ExitLabel1     := Self.ExitLabel1;
        LLineValues.ExitLabel2     := Self.ExitLabel2;
        LLineValues.ShowDecimals   := Self.ShowDecimals;

        if LLineValues.Initialise  then
        begin
          if not LDataSet.DataSet.FieldByName('ReserviorName').IsNull then
          begin
            LLineValues.FReservoirName.FData := Trim(LDataSet.DataSet.FieldByName('ReserviorName').AsString);
            LLineValues.FReservoirName.FInitalised := True;
            LLineValues.FReservoirName.FLength := 15
          end;

          for LIndex := 0 to HighIndex do
          begin
            LColumnName :=  Format('%s%2.2d',['OutputSummaryValue',LIndex+1]);
            if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
            begin
              LLength := 0;
              if (LIndex < 7) then
                 LLength := 8
              else if (LIndex < 8) then
                 LLength := 10
              else if (LIndex < 9) then
                 LLength := 8
              else if (LIndex < 11) then
                 LLength := 10
              else if (LIndex < 12) then
                 LLength := 11;
              LLineValues.FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
              LLineValues.FValues[LIndex].FInitalised := True;
              LLineValues.FValues[LIndex].FLength := LLength;
              LLineValues.FValues[LIndex].FDecimal := 3;
            end
            else
              LLineValues.FValues[LIndex].FInitalised := False;
          end;
          FValuesLines.Add(LLineValues);
        end
        else
         FreeAndNil(LLineValues);
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockValues.SaveToDatabase(ADatabaseName,AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockValues.SaveToDatabase';
var
  LCount,
  LIndex: integer;
  LDataSet : TAbstractModelDataset;
  LLineValues : TValuesLine;
  LFieldName: string;
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        'INSERT INTO suBlockOutputSummaryValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier, ReserviorName';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', OutputSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier, :ReserviorName';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', :OutputSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);
      for LCount := 0 to FValuesLines.Count -1 do
      begin

        LLineValues := TValuesLine(FValuesLines.Items[LCount]);
        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                           [AModel, AStudy, ASubArea, AScenario,
                            IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(LCount+1)]);
        if LLineValues.FReservoirName.FInitalised then
          LDataSet.SetParams(['ReserviorName'], [LLineValues.FReservoirName.FData]);

        for LIndex := LowIndex to HighIndex do
        begin
          if LLineValues.FValues[LIndex].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['OutputSummaryValue',LIndex+1]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LLineValues.FValues[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutOutputSummaryBlockValues.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutOutputSummaryBlockValues.SaveToStream';
var
  LLine: string;
  LIndex,
  LCount: integer;
  LValuesLine:TValuesLine;
begin
  try
    for LCount := 0 to FValuesLines.Count - 1 do
    begin
      LValuesLine := TValuesLine(FValuesLines[LCount]);
      LLine := PadString(LValuesLine.FReservoirName);
      for LIndex := 0 to HighIndex do
        if(LValuesLine.FValues[LIndex].FInitalised) then
          LLine := LLine + PadDouble(LValuesLine.FValues[LIndex]);

      LLine := LLine + #13#10;
      Stream.Write(Pchar(LLine)^, Length(LLine));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutOutputSummaryBlockTotal }

function TSumOutOutputSummaryBlockTotal.Initialise: boolean;
const OPNAME = 'TSumOutOutputSummaryBlockTotal.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 15;
    ValYearLength  := 8;
    ValFirstLength := 8;
    ValOtherLength := 10;
    ValAvgLength   := 11;
    Decimals       := 3;
    ShowDecimals   := True;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockTotal.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockTotal.ReadFromBlockData';
var
  LIndex : integer;
  LCurrentLine: string;

  function StringToValues( ALine: String): boolean;
  const OPNAME = 'StringToValues';
  var
    LIndex,
    LErrCode: integer;
    LSubStr: string;
    LValue: Double;
  begin
    for LIndex := 2 to HighIndex do
    begin
      if(Length(ALine) > 0) then
      begin
        LSubStr := '';
        if (LIndex = LowIndex) then
           LSubStr := Copy(ALine,1,15)
        else if (LIndex < (2+7)) then
           LSubStr := Copy(ALine,1,8)
        else if (LIndex < (2+8)) then
           LSubStr := Copy(ALine,1,10)
        else if (LIndex < (2+9)) then
           LSubStr := Copy(ALine,1,8)
        else if (LIndex < (2+11)) then
           LSubStr := Copy(ALine,1,10)
        else if (LIndex < (2+12)) then
           LSubStr := Copy(ALine,1,11);

        if(LSubStr <> '') then
        begin
          Delete(ALine,1,Length(LSubStr));
          if(LIndex = LowIndex) then
          begin
            FReservoirName.FData := LSubStr;
            FReservoirName.FInitalised := true;
            FReservoirName.FLength := 15;
          end
          else
          begin
            Val(LSubStr,LValue,LErrCode);
            if(LErrCode = 0) then
            begin
              FValues[LIndex].FData := LValue;
              FValues[LIndex].FInitalised := True;
              FValues[LIndex].FLength := Length(LSubStr);
            end
            else
             FValues[LIndex].FLength := 0;
          end;
        end;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LIndex := 0;
    while(ABLockData.Lines.Count > 0) do
    begin
      LIndex := LIndex + 1;

      LCurrentLine := ABLockData.Lines[0];
      ABLockData.Lines.Delete(0);

      if (LIndex = 2) then
      begin
        Delete(LCurrentLine,1,ValCountLength);
        Result :=  StringToValues(LCurrentLine);
      end;
    end;
    Result := Result and (LIndex >= 2) and (ABLockData.Lines.Count = 0);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockTotal.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockTotal.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
  LLength,
  LIndex: integer;
  LColumnName,
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier,ReserviorName';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', OutputSummaryValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockOutputSummaryValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' AND Identifier = '+ IntToStr(TotalAVGLine)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.EOF do
      begin

        if not LDataSet.DataSet.FieldByName('ReserviorName').IsNull then
        begin
          FReservoirName.FData := Trim(LDataSet.DataSet.FieldByName('ReserviorName').AsString);
          FReservoirName.FInitalised := True;
          FReservoirName.FLength := 15
        end;

        for LIndex := LowIndex to HighIndex do
        begin
          LColumnName :=  Format('%s%2.2d',['OutputSummaryValue',LIndex+1]);
          if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
          begin
              LLength := 0;
              if (LIndex < (2+7)) then
                 LLength := 8
              else if (LIndex < (2+8)) then
                 LLength := 10
              else if (LIndex < (2+9)) then
                 LLength := 8
              else if (LIndex < (2+11)) then
                 LLength := 10
              else if (LIndex < (2+12)) then
                 LLength := 11;
              FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
              FValues[LIndex].FInitalised := True;
              FValues[LIndex].FLength := LLength;
              FValues[LIndex].FDecimal := 3;
          end;
        end;
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockTotal.SaveToDatabase(ADatabaseName, AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockTotal.SaveToDatabase';
var
  LIndex: integer;
  LDataSet : TAbstractModelDataset;
  LFieldName: string;
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        'INSERT INTO suBlockOutputSummaryValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier, ReserviorName';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', OutputSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier, :ReserviorName';
        for LIndex := LowIndex to HighIndex do
          LSQLText := LSQLText + Format('%s%2.2d',[', :OutputSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);

      LDataSet.ClearQueryParams;
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                          'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                         [AModel, AStudy, ASubArea, AScenario, 
                          IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(TotalAVGLine)]);
      if FReservoirName.FInitalised then
        LDataSet.SetParams(['ReserviorName'], [FReservoirName.FData]);

      for LIndex := LowIndex to HighIndex do
      begin
        if FValues[LIndex].FInitalised then
        begin
          LFieldName := Format('%s%2.2d',['OutputSummaryValue',LIndex+1]);
          LDataSet.SetParams([LFieldName], [FloatToStr(FValues[LIndex].FData)]);
        end;
      end;
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutOutputSummaryBlockTotal.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutOutputSummaryBlockTotal.SaveToStream';
var
  LLine: string;
  LIndex: integer;
begin
  try
    LLine :=  #13#10;
    Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

    LLine := TOTALSYSTEMLABEL;
    for LIndex := 2 to High(FValues) do
      if(FValues[LIndex].FInitalised) then
        LLine := LLine + PadDouble(FValues[LIndex]);

    LLine := LLine + #13#10;
    Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutAnualSummaryBlockHeading }

function TSumOutAnualSummaryBlockHeading.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockHeading.ReadFromBlockData';
var
  LCurrentLine: string;
  LEmptyLinesCount: integer;
begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LEmptyLinesCount := 0;
    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      FHeadingLines.Add(LCurrentLine);
      ABLockData.Lines.Delete(0);
      if(Trim(LCurrentLine) = '') then
        LEmptyLinesCount := LEmptyLinesCount + 1;
      if (LEmptyLinesCount >= 3) and (Trim(ABLockData.Lines[0]) <> '')then
        Break;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutAnualSummaryBlockValues }

procedure TSumOutAnualSummaryBlockValues.CreateMemberObjects;
const OPNAME = 'TSumOutAnualSummaryBlockValues.CreateMemberObjects';
begin
  inherited;
  try
    FYearPeriod         := TInteger.Create;
    FCaption            := TString.Create;
    FSelectedYieldLabel := TString.Create;
    FSystemSupplyVolume := TDouble.Create;
    FAnualValues        := TAnualSummaryValues.Create(True);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutAnualSummaryBlockValues.DestroyMemberObjects;
const OPNAME = 'TSumOutAnualSummaryBlockValues.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FYearPeriod);
    FreeAndNil(FCaption);
    FreeAndNil(FSelectedYieldLabel);
    FreeAndNil(FSystemSupplyVolume);
    FreeAndNil(FAnualValues);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutAnualSummaryBlockValues.Initialise';
var
  LIndex: integer;
Begin
  Result := False;
  try
    ValCountLength := 10;
    ValYearLength  := 10;
    ValFirstLength := 10;
    ValOtherLength := 10;
    ValAvgLength   := 10;
    Decimals       := 4;
    ShowDecimals   := True;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;

    FYearPeriod.FData := 0;
    FYearPeriod.FLength := 4;
    FYearPeriod.FDefaultPadding := True;
    FYearPeriod.FInitalised := False;

    FCaption.FData := '';
    FCaption.FLength := 0;
    FCaption.FDefaultPadding := True;
    FCaption.FInitalised := False;

    FSelectedYieldLabel.FData := '';
    FSelectedYieldLabel.FLength := 0;
    FSelectedYieldLabel.FDefaultPadding := True;
    FSelectedYieldLabel.FInitalised := False;


    FSystemSupplyVolume.FData := 0.0;
    FSystemSupplyVolume.FLength := 10;
    FSystemSupplyVolume.FDecimal := 4;
    FSystemSupplyVolume.FInitalised := False;
    FSystemSupplyVolume.ShowDecimalPoint := True;

    for LIndex := LowIndex to HighIndex do
    begin
      FValues[LIndex].FData := 0.0;
      FValues[LIndex].FLength := 10;
      FValues[LIndex].FDecimal := 4;
      FValues[LIndex].FInitalised := False;
      FValues[LIndex].ShowDecimalPoint := True;
    end;
    Result := FAnualValues.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockValues.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockValues.ReadFromBlockData';
var
  LCurrentLine: string;
  //LLineValues: TValuesLine;
  LIndex,
  LCount,
  LIntValue,
  LErrCode: integer;
  LSubStr: string;
  LValue: Double;
  LAnualSummaryValuesLine:TAnualSummaryValuesLine;
begin

  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LIndex := 0;
    while(ABLockData.Lines.Count > 0) do
    begin
      LIndex := LIndex + 1;
      LCurrentLine := ABLockData.Lines[0];
      ABLockData.Lines.Delete(0);
      if (LCurrentLine = '') then
        Continue;

      if (LIndex = 1) then
      begin
        if (ABLockData.BlockType = btAnualFirmSelectedYieldDemands) then
        begin
          LSubStr := Copy(LCurrentLine,55,Length(LCurrentLine));
          FSelectedYieldLabel.FData := LSubStr;
          FSelectedYieldLabel.FInitalised := True;
          FSelectedYieldLabel.FLength := Length(LSubStr);
        end
        else if (ABLockData.BlockType = btAnualFullSystemSupplyVolume) then
        begin
          LSubStr := Copy(LCurrentLine,37,10);
          LSubStr := Trim(LSubStr);
          if(LSubStr <> '') then
          begin
            Val(LSubStr,LValue,LErrCode);
            if(LErrCode = 0) then
            begin
              FSystemSupplyVolume.FData := LValue;
              FSystemSupplyVolume.FInitalised := True;
            end;
          end;
        end;
        Result := True;
        Continue;
      end;

      if (LIndex = 2) then
      begin
        Delete(LCurrentLine,1,14);
        for LCount := 0 to 9 do
        begin
          LSubStr := Copy(LCurrentLine,1,10);
          if(LSubStr <> '') then
          begin
            Val(LSubStr,LValue,LErrCode);
            if(LErrCode = 0) then
            begin
              FValues[LCount].FData := LValue;
              FValues[LCount].FInitalised := True;
            end;
            Delete(LCurrentLine,1,Length(LSubStr));
          end;
        end;
        Result := True;
        Continue;
      end;

      if (LIndex = 5) then
      begin
        if (Pos('WATER SUPPLY',LCurrentLine) = 0) and
           ((ABLockData.BlockType = btAnualFirmYieldDemands) or
            (Self.FBlockType = btDeficitPropotion) or           
           (ABLockData.BlockType = btAnualFirmEnergyDemands) or
           (ABLockData.BlockType = btAnualFirmSelectedYieldDemands)) then
        begin
          LSubStr := Copy(LCurrentLine,1,32);
          FCaption.FData := LSubStr;
          FCaption.FInitalised := True;
          FCaption.FLength := Length(LSubStr);

          LSubStr := Copy(LCurrentLine,33,4);
          Val(LSubStr,LIntValue,LErrCode);
          if(LErrCode = 0) then
          begin
            FYearPeriod.FData := LIntValue;
            FYearPeriod.FInitalised := True;
          end;
        end
        else
        begin
          FCaption.FData := LCurrentLine;
          FCaption.FInitalised := True;
          FCaption.FLength := Length(LCurrentLine);
        end;
        Result := True;
        Continue;
      end;

      if (LIndex >= 6) then
      begin
        while(LCurrentLine <> '') do
        begin
          LAnualSummaryValuesLine := TAnualSummaryValuesLine.Create;
          FAnualValues.Add(LAnualSummaryValuesLine);

          LSubStr := Copy(LCurrentLine,1,9);
          Val(LSubStr,LIntValue,LErrCode);
          if(LErrCode = 0) then
          begin
            LAnualSummaryValuesLine.FirstInteger.FData := LIntValue;
            LAnualSummaryValuesLine.FirstInteger.FInitalised := True;
          end;
          Delete(LCurrentLine,1,Length(LSubStr)+5);

          for LCount := 0 to 9 do
          begin
            LSubStr := Copy(LCurrentLine,1,10);
            if(LSubStr <> '') then
            begin
              Val(LSubStr,LValue,LErrCode);
              if(LErrCode = 0) then
              begin
                LAnualSummaryValuesLine.AnualSummaryValuesLine[LCount].FData := LValue;
                LAnualSummaryValuesLine.AnualSummaryValuesLine[LCount].FInitalised := True;
              end;
              Delete(LCurrentLine,1,Length(LSubStr));
            end;
          end;

          if(ABLockData.Lines.Count = 0) then
            LCurrentLine := ''
          else
          begin
            LCurrentLine := ABLockData.Lines[0];
            ABLockData.Lines.Delete(0);
          end
        end;
        Result := True;
        Continue;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockValues.GetLine1Label: string;
const OPNAME = 'TSumOutAnualSummaryBlockValues.GetLine1Label';
begin
  Result := '';
  try
    Case Self.FBlockType of
      btAnualFirmYieldDemands:         Result := lblAnualFirmYieldDemands;
      btAnualFirmEnergyDemands:        Result := lblAnualFirmEnergyDemands;
      btAnualFirmSelectedYieldDemands: Result := lblAnualFirmSelectedYieldDemands;
      btAnualNonFirmYieldDemands:      Result := lblAnualNonFirmYieldDemands;
      btAnualSecondaryYieldDemands:    Result := lblAnualSecondaryYieldDemands;
      btAnualTotalSystemPumpingEnergy: Result := lblAnualTotalSystemPumpingEnergy;
      btAnualFullSystemSupplyVolume:   Result := lblAnualFullSystemSupplyVolume;
      btDeficitPropotion:              Result := lblDeficitPropotion;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutAnualSummaryBlockValues.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutAnualSummaryBlockValues.SaveToStream';
var
  LLine: string;
  LIndex,
  LCount,
  LCount2: integer;
  LAnualSummaryValuesLine:TAnualSummaryValuesLine;
begin
  try
    for LIndex := 1 to 6 do
    begin
      if (LIndex = 3) or (LIndex = 4) then
      begin
        LLine :=  #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      LLine := '';
      if (LIndex = 1) then
      begin
        LLine := GetLine1Label;
        if (self.FBlockType = btAnualFirmSelectedYieldDemands) then
        begin
          LLine := LLine + PadString(FSelectedYieldLabel);
        end
        else if (self.FBlockType = btAnualFirmSelectedYieldDemands) then
        begin
          LLine := LLine + PadDouble(FSystemSupplyVolume);
        end;
        LLine := LLine + #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      if (LIndex = 2) then
      begin
        LLine := '              ';
        for LCount := 0 to 9 do
        begin
          if FValues[LCount].FInitalised  then
          begin
            LLine := LLine + PadDouble(FValues[LCount]);
          end;
        end;
        LLine := LLine + #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      if (LIndex = 5) then
      begin
        LLine := FCaption.FData;
        if (Pos('WATER SUPPLY',LLine) = 0) and
           ((Self.FBlockType = btAnualFirmYieldDemands) or
            (Self.FBlockType = btDeficitPropotion) or
           (Self.FBlockType = btAnualFirmEnergyDemands) or
           (Self.FBlockType = btAnualFirmSelectedYieldDemands)) then
        begin
          LLine := LLine + PadInt(FYearPeriod) + '  YEAR PERIOD';
        end;
        LLine := LLine + #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      if (LIndex >= 6) then
      begin

        for LCount := 0 to FAnualValues.Count -1 do
        begin
          LLine := '';
          LAnualSummaryValuesLine := FAnualValues.AnualSummaryValuesLine[LCount];
          LLine := PadInt(LAnualSummaryValuesLine.FirstInteger);

          for LCount2 := 0 to 9 do
          begin
            if(LAnualSummaryValuesLine.AnualSummaryValuesLine[LCount2].FInitalised) then
            begin
              LLine := LLine + PadDouble(LAnualSummaryValuesLine.AnualSummaryValuesLine[LCount2]);
            end;
          end;
          LLine := LLine + #13#10;
          Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockValues.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockValues.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
  LIndex: integer;
  LColumnName,
  LSQLText: string;
  LAnualSummaryValuesLine:TAnualSummaryValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier'+
        ' , FirstInteger,YearPeriod, SystemSupplyVolume, SelectedYieldLabel,AnualSummaryCaption ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', AnualSummaryValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockAnualSummaryValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;

      if not LDataSet.DataSet.FieldByName('YearPeriod').IsNull then
      begin
        FYearPeriod.FData := LDataSet.DataSet.FieldByName('YearPeriod').AsInteger;
        FYearPeriod.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('SystemSupplyVolume').IsNull then
      begin
        FSystemSupplyVolume.FData := LDataSet.DataSet.FieldByName('SystemSupplyVolume').AsFloat;
        FSystemSupplyVolume.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('SelectedYieldLabel').IsNull then
      begin
        FSelectedYieldLabel.FData := Trim(LDataSet.DataSet.FieldByName('SelectedYieldLabel').AsString);
        FSelectedYieldLabel.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('AnualSummaryCaption').IsNull then
      begin
        FCaption.FData := Trim(LDataSet.DataSet.FieldByName('AnualSummaryCaption').AsString);
        FCaption.FInitalised := True;
      end;

      for LIndex := 0 to 9 do
      begin
        LColumnName := Format('%s%2.2d',['AnualSummaryValue',LIndex+1]);
        if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
        begin
          FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
          FValues[LIndex].FInitalised := True;
        end;
      end;

      LDataSet.DataSet.Next;
      while not LDataSet.DataSet.EOF do
      begin
        LAnualSummaryValuesLine := TAnualSummaryValuesLine.Create;
        FAnualValues.Add(LAnualSummaryValuesLine);

        if not LDataSet.DataSet.FieldByName('FirstInteger').IsNull then
        begin
          LAnualSummaryValuesLine.FirstInteger.FData := LDataSet.DataSet.FieldByName('FirstInteger').AsInteger;
          LAnualSummaryValuesLine.FirstInteger.FInitalised := True;
        end;

        for LIndex := 0 to 9 do
        begin
          LColumnName := Format('%s%2.2d',['AnualSummaryValue',LIndex+1]);
          if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
          begin
            LAnualSummaryValuesLine.FAnualSummaryValuesLine[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
            LAnualSummaryValuesLine.FAnualSummaryValuesLine[LIndex].FInitalised := True;
          end
        end;

        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockValues.SaveToDatabase(ADatabaseName, AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockValues.SaveToDatabase';
var
  LCount : integer;
  LIndex: integer;
  LDataSet : TAbstractModelDataset;
  LFieldName: string;
  LSQLText: string;
  LAnualSummaryValuesLine:TAnualSummaryValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        'INSERT INTO suBlockAnualSummaryValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier'+
        ' , FirstInteger,YearPeriod, SystemSupplyVolume, SelectedYieldLabel,AnualSummaryCaption ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', AnualSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier'+
        ' ,:FirstInteger, :YearPeriod, :SystemSupplyVolume, :SelectedYieldLabel,:AnualSummaryCaption ';
            for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', :AnualSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);
      LDataSet.ClearQueryParams;
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                          'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                         [AModel, AStudy, ASubArea, AScenario,
                          IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(1)]);

      if FYearPeriod.FInitalised then
        LDataSet.SetParams(['YearPeriod'], [IntToStr(FYearPeriod.FData)]);

      if FSystemSupplyVolume.FInitalised then
        LDataSet.SetParams(['SystemSupplyVolume'], [FloatToStr(FSystemSupplyVolume.FData)]);

      if FSelectedYieldLabel.FInitalised then
        LDataSet.SetParams(['SelectedYieldLabel'], [FSelectedYieldLabel.FData]);

      if FCaption.FInitalised then
        LDataSet.SetParams(['AnualSummaryCaption'], [FCaption.FData]);

      for LIndex := 0 to 9 do
      begin
        if FValues[LIndex].FInitalised then
        begin
          LFieldName := Format('%s%2.2d',['AnualSummaryValue',LIndex+1]);
          LDataSet.SetParams([LFieldName], [FloatToStr(FValues[LIndex].FData)]);
        end;
      end;
      LDataSet.ExecSQL;


      for LCount := 0  to FAnualValues.Count - 1 do
      begin
        LAnualSummaryValuesLine := FAnualValues.AnualSummaryValuesLine[LCount];
        LDataSet.SetSQL(LSQLText);

        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                           [AModel, AStudy, ASubArea, AScenario,
                            IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(LCount+2)]);


        if LAnualSummaryValuesLine.FirstInteger.FInitalised then
          LDataSet.SetParams(['FirstInteger'], [IntToStr(LAnualSummaryValuesLine.FirstInteger.FData)]);

        for LIndex := 0 to 9 do
        begin

          if LAnualSummaryValuesLine.AnualSummaryValuesLine[LIndex].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['AnualSummaryValue',LIndex+1]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LAnualSummaryValuesLine.AnualSummaryValuesLine[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutAnualAverageInflowBlockHeading }

function TSumOutAnualAverageInflowBlockHeading.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockHeading.ReadFromBlockData';
var
  LCurrentLine: string;
begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      if (Pos(lblAnualAverageInflow,LCurrentLine) > 0) then
        Break;
      FHeadingLines.Add(LCurrentLine);
      ABLockData.Lines.Delete(0);
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutAnualAverageInflowBlockValues }

function TSumOutAnualAverageInflowBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockValues.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 10;
    ValYearLength  := 10;
    ValFirstLength := 10;
    ValOtherLength := 10;
    ValAvgLength   := 10;
    Decimals       := 2;
    ShowDecimals   := True;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualAverageInflowBlockValues.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockValues.ReadFromBlockData';
var
  LCurrentLine: string;
  LLineValues: TValuesLine;
  LIndex,
  LCount,
  LIntValue,
  LErrCode: integer;
  LSubStr: string;
  LValue: Double;
begin

  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    while(ABLockData.Lines.Count > 0) do
    begin
      for LIndex := 1 to 9 do
      begin
        if(ABLockData.Lines.Count = 0) then
        Break;
        LCurrentLine := ABLockData.Lines[0];
        ABLockData.Lines.Delete(0);
        if (LIndex = 1) or (LIndex = 2) or (LIndex = 4) or
           (LIndex = 5) or (LIndex = 7) or (LIndex = 9) then
           Continue;

        if (LIndex = 3) then
        begin
          LLineValues := TValuesLine.Create(FAppModules);
          LLineValues.ValCountLength := Self.ValCountLength;
          LLineValues.ValYearLength  := Self.ValYearLength;
          LLineValues.ValFirstLength := Self.ValFirstLength;
          LLineValues.ValOtherLength := Self.ValOtherLength;
          LLineValues.ValAvgLength   := Self.ValAvgLength;
          LLineValues.Decimals       := 0;
          LLineValues.BlockNumber    := Self.BlockNumber;
          LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
          LLineValues.SequenceNumber := Self.SequenceNumber;
          LLineValues.ExitLabel1     := Self.ExitLabel1;
          LLineValues.ExitLabel2     := Self.ExitLabel2;
          LLineValues.ShowDecimals   := False;
          LLineValues.Initialise;
          LLineValues.FValues[2].ShowDecimalPoint := False;
          FValuesLines.Add(LLineValues);

          Delete(LCurrentLine,1,14);
          for LCount := 0 to 9 do
          begin
            LSubStr := Copy(LCurrentLine,1,10);
            if(LSubStr <> '') then
            begin
              Val(LSubStr,LIntValue,LErrCode);
              if(LErrCode = 0) then
              begin
                LLineValues.FValues[LCount].FData := LIntValue;
                LLineValues.FValues[LCount].FInitalised := True;
              end;
              Delete(LCurrentLine,1,Length(LSubStr));
            end;
          end;

        end
        else
        begin
          LLineValues := TValuesLine.Create(FAppModules);
          LLineValues.ValCountLength := Self.ValCountLength;
          LLineValues.ValYearLength  := Self.ValYearLength;
          LLineValues.ValFirstLength := Self.ValFirstLength;
          LLineValues.ValOtherLength := Self.ValOtherLength;
          LLineValues.ValAvgLength   := Self.ValAvgLength;
          LLineValues.Decimals       := 2;
          LLineValues.BlockNumber    := Self.BlockNumber;
          LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
          LLineValues.SequenceNumber := Self.SequenceNumber;
          LLineValues.ExitLabel1     := Self.ExitLabel1;
          LLineValues.ExitLabel2     := Self.ExitLabel2;
          LLineValues.ShowDecimals   := True;
          LLineValues.Initialise;
          LLineValues.FValues[0].ShowDecimalPoint := True;
          LLineValues.FValues[0].FDecimal := 2;
          LLineValues.FValues[1].ShowDecimalPoint := True;
          LLineValues.FValues[1].FDecimal := 2;
          FValuesLines.Add(LLineValues);

          LLineValues.FReservoirName.FData :=   Copy(LCurrentLine,1,14);
          LLineValues.FReservoirName.FLength := Length(LLineValues.FReservoirName.FData);
          LLineValues.FReservoirName.FInitalised := True;
          Delete(LCurrentLine,1,14);

          for LCount := 0 to 9 do
          begin
            LSubStr := Copy(LCurrentLine,1,10);
            if(LSubStr <> '') then
            begin
              Val(LSubStr,LValue,LErrCode);
              if(LErrCode = 0) then
              begin
                LLineValues.FValues[LCount].FData := LValue;
                LLineValues.FValues[LCount].FInitalised := True;
              end;
              Delete(LCurrentLine,1,Length(LSubStr));
            end;
          end;
        end;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualAverageInflowBlockValues.ReadFromDatabase(ADatabaseName: string; ADataSet: TDataset): boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockValues.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
  LIndex: integer;
  LColumnName,
  LSQLText: string;
  LLineValues: TValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier,InflowType';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', AverageInflowValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockAnualAverageInflowValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      FValuesLines.Clear;
      while not LDataSet.DataSet.EOF do
      begin
        LLineValues := TValuesLine.Create(FAppModules);
        LLineValues.ValCountLength := Self.ValCountLength;
        LLineValues.ValYearLength  := Self.ValYearLength;
        LLineValues.ValFirstLength := Self.ValFirstLength;
        LLineValues.ValOtherLength := Self.ValOtherLength;
        LLineValues.ValAvgLength   := Self.ValAvgLength;
        LLineValues.Decimals       := Self.Decimals;
        LLineValues.BlockNumber    := Self.BlockNumber;
        LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
        LLineValues.SequenceNumber := Self.SequenceNumber;
        LLineValues.ExitLabel1     := Self.ExitLabel1;
        LLineValues.ExitLabel2     := Self.ExitLabel2;
        LLineValues.ShowDecimals   := Self.ShowDecimals;

        if LLineValues.Initialise  then
        begin
          if not LDataSet.DataSet.FieldByName('InflowType').IsNull then
          begin
            LLineValues.FReservoirName.FData := Trim(LDataSet.DataSet.FieldByName('InflowType').AsString);
            LLineValues.FReservoirName.FInitalised := True;
          end;

          for LIndex := 0 to 9 do
          begin
            LColumnName :=  Format('%s%2.2d',['AverageInflowValue',LIndex+1]);
            if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
            begin
              LLineValues.FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
              LLineValues.FValues[LIndex].FInitalised := True;
            end
            else
              LLineValues.FValues[LIndex].FInitalised := False;
          end;
          FValuesLines.Add(LLineValues);
        end
        else
         FreeAndNil(LLineValues);
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualAverageInflowBlockValues.SaveToDatabase(ADatabaseName,AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockValues.SaveToDatabase';
var
  LCount : integer;
  LIndex: integer;
  LDataSet : TAbstractModelDataset;
  LFieldName: string;
  LSQLText: string;
  LLineValues : TValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        'INSERT INTO suBlockAnualAverageInflowValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier, InflowType ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', AverageInflowValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier,:InflowType';
            for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', :AverageInflowValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);

      for LCount := 0 to FValuesLines.Count -1  do
      begin
        LLineValues := TValuesLine(FValuesLines.Items[LCount]);

        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                           [AModel, AStudy, ASubArea, AScenario, 
                            IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(LCount+1)]);
        if LLineValues.FReservoirName.FInitalised then
          LDataSet.SetParams(['InflowType'], [LLineValues.FReservoirName.FData]);

        for LIndex := 0 to 9 do
        begin
          if LLineValues.FValues[LIndex].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['AverageInflowValue',LIndex+1]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LLineValues.FValues[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutAnualAverageInflowBlockValues.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutAnualAverageInflowBlockValues.SaveToStream';
var
  LLine: string;
  LLineValues: TValuesLine;
  LCount,
  LLinesCount,
  LIndex: integer;
begin

  try
    LLinesCount := 0;
    while (LLinesCount < FValuesLines.Count) do
    begin

      for LIndex := 1 to 9 do
      begin
        if(LLinesCount = FValuesLines.Count) then
        Break;
        if (LIndex = 1) then
        begin
          LLine := ' '+ lblAnualAverageInflow + #13#10;
          Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
          Continue;
        end;

        if (LIndex = 2) or (LIndex = 4) or
           (LIndex = 5) or (LIndex = 7) or (LIndex = 9) then
        begin
          LLine :=  #13#10;
          Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
          Continue;
        end;

        if (LIndex = 3) then
        begin
          LLineValues := TValuesLine(FValuesLines[LLinesCount]);
          LLine := '              ';
          for LCount := 0 to 9 do
          begin
            if LLineValues.FValues[LCount].FInitalised then
            begin
              LLineValues.FValues[LCount].ShowDecimalPoint := False;
              LLineValues.FValues[LCount].FDecimal := 0;
              LLine := LLine + PadDouble(LLineValues.FValues[LCount]);
            end;
          end;
          LLine := LLine + #13#10;
          Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
          LLinesCount := LLinesCount + 1;
          Continue;
        end
        else
        begin
          LLineValues := TValuesLine(FValuesLines[LLinesCount]);

          LLineValues.FReservoirName.FLength := 14;
          LLineValues.FReservoirName.FData := Trim(LLineValues.FReservoirName.FData);
          while (Length(LLineValues.FReservoirName.FData) < 9) do
            LLineValues.FReservoirName.FData := ' ' + LLineValues.FReservoirName.FData;
          LLine := PadString(LLineValues.FReservoirName);

          for LCount := 0 to 9 do
          begin
            if LLineValues.FValues[LCount].FInitalised then
            begin
              LLineValues.FValues[LCount].ShowDecimalPoint := True;
              LLineValues.FValues[LCount].FDecimal := 2;
              LLine := LLine + PadDouble(LLineValues.FValues[LCount]);
            end;
          end;
          LLine := LLine + #13#10;
          Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
          LLinesCount := LLinesCount + 1;
          Continue;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutSequencesWithFailuresBlockHeading }

function TSumOutSequencesWithFailuresBlockHeading.Initialise: boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockHeading.Initialise';
Begin
  Result := False;
  try
    ExitLabel1  := SEQUENCESWITHFAILURESEXITLABEL;
    ExitLabel2  := SEQUENCESWITHFAILURESEXITLABEL;
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

{ TSumOutSequencesWithFailuresBlockValues }

function TSumOutSequencesWithFailuresBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockValues.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 9;
    ValYearLength  := 10;
    ValFirstLength := 10;
    ValOtherLength := 10;
    ValAvgLength   := 10;
    Decimals       := 0;
    ShowDecimals   := False;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockValues.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockValues.ReadFromBlockData';
var
  LCurrentLine: string;
  LLineValues: TValuesLine;
  LIndex,
  LCount,
  LErrCode: integer;
  LSubStr: string;
  LValue: Double;
begin

  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LIndex := 0;
    while(ABLockData.Lines.Count > 0) do
    begin

      LCurrentLine := ABLockData.Lines[0];
      if(Pos('--------',LCurrentLine) > 0) then
        Break;
      ABLockData.Lines.Delete(0);

      LIndex := LIndex + 1;
      if(LIndex = 1) then
        Continue;

      LLineValues := TValuesLine.Create(FAppModules);
      LLineValues.ValCountLength := Self.ValCountLength;
      LLineValues.ValYearLength  := Self.ValYearLength;
      LLineValues.ValFirstLength := Self.ValFirstLength;
      LLineValues.ValOtherLength := Self.ValOtherLength;
      LLineValues.ValAvgLength   := Self.ValAvgLength;
      LLineValues.Decimals       := 0;
      LLineValues.BlockNumber    := Self.BlockNumber;
      LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
      LLineValues.SequenceNumber := Self.SequenceNumber;
      LLineValues.ExitLabel1     := Self.ExitLabel1;
      LLineValues.ExitLabel2     := Self.ExitLabel2;
      LLineValues.ShowDecimals   := False;
      LLineValues.Initialise;
      LLineValues.FValues[0].FLength := 9;
      FValuesLines.Add(LLineValues);

      for LCount := 0 to 9 do
      begin
        if(LCount = 0) then
          LSubStr := Copy(LCurrentLine,1,09)
        else
          LSubStr := Copy(LCurrentLine,1,10);
        if(LSubStr <> '') then
        begin
          Val(LSubStr,LValue,LErrCode);
          if(LErrCode = 0) then
          begin
            LLineValues.FValues[LCount].FData := LValue;
            LLineValues.FValues[LCount].FInitalised := True;
          end;
          if(LCount = 0) then
            Delete(LCurrentLine,1,14)
          else
          Delete(LCurrentLine,1,Length(LSubStr));
        end;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockValues.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockValues.ReadFromDatabase';
var
  LDataSet : TAbstractModelDataset;
  LModel,LStudy,LSubArea,LScenario: string;
  LIndex: integer;
  LColumnName,
  LSQLText: string;
  LLineValues: TValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', SequenceFailuresValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockSequencesWithFailuresValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' AND Identifier <> '+ IntToStr(TotalAVGLine)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      FValuesLines.Clear;
      while not LDataSet.DataSet.EOF do
      begin
        LLineValues := TValuesLine.Create(FAppModules);
        LLineValues.ValCountLength := Self.ValCountLength;
        LLineValues.ValYearLength  := Self.ValYearLength;
        LLineValues.ValFirstLength := Self.ValFirstLength;
        LLineValues.ValOtherLength := Self.ValOtherLength;
        LLineValues.ValAvgLength   := Self.ValAvgLength;
        LLineValues.Decimals       := Self.Decimals;
        LLineValues.BlockNumber    := Self.BlockNumber;
        LLineValues.LoadCaseNumber := Self.LoadCaseNumber;
        LLineValues.SequenceNumber := Self.SequenceNumber;
        LLineValues.ExitLabel1     := Self.ExitLabel1;
        LLineValues.ExitLabel2     := Self.ExitLabel2;
        LLineValues.ShowDecimals   := Self.ShowDecimals;

        if LLineValues.Initialise  then
        begin
          for LIndex := 0 to 9 do
          begin
            LColumnName :=  Format('%s%2.2d',['SequenceFailuresValue',LIndex+1]);
            if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
            begin
              LLineValues.FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
              LLineValues.FValues[LIndex].FInitalised := True;
            end
            else
              LLineValues.FValues[LIndex].FInitalised := False;
          end;
          FValuesLines.Add(LLineValues);
        end
        else
         FreeAndNil(LLineValues);
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockValues.SaveToDatabase(ADatabaseName, AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockValues.SaveToDatabase';
var
  LCount : integer;
  LIndex: integer;
  LDataSet : TAbstractModelDataset;
  LFieldName: string;
  LSQLText: string;
  LLineValues : TValuesLine;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        'INSERT INTO suBlockSequencesWithFailuresValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', SequenceFailuresValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier ';
            for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', :SequenceFailuresValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);

      for LCount := 0 to FValuesLines.Count -1  do
      begin
        LLineValues := TValuesLine(FValuesLines.Items[LCount]);

        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                           [AModel, AStudy, ASubArea, AScenario,
                            IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(LCount+1)]);

        for LIndex := 0 to 9 do
        begin
          if LLineValues.FValues[LIndex].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['SequenceFailuresValue',LIndex+1]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LLineValues.FValues[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutSequencesWithFailuresBlockValues.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutSequencesWithFailuresBlockValues.SaveToStream';
var
  LLine: string;
  LLineValues: TValuesLine;
  LIndex,
  LCount: integer;
begin
  try
    for LIndex := -1 to FValuesLines.Count-1 do
    begin

      if(LIndex < 0) then
      begin
        LLine :=  #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      LLineValues := TValuesLine(FValuesLines[LIndex]);
      LLine := '';
      for LCount := 0 to 9 do
      begin
        if LLineValues.FValues[LCount].FInitalised then
        begin
          if(LCount = 0) then
            LLine := LLine + PadDouble(LLineValues.FValues[LCount]) + '     '
          else
            LLine := LLine + PadDouble(LLineValues.FValues[LCount]);
        end;
      end;

      LLine := LLine + #13#10;
      Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutSequencesWithFailuresBlockTotal }

function TSumOutSequencesWithFailuresBlockTotal.Initialise: boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockTotal.Initialise';
Begin
  Result := False;
  try
    ValCountLength := 9;
    ValYearLength  := 10;
    ValFirstLength := 10;
    ValOtherLength := 10;
    ValAvgLength   := 10;
    Decimals       := 0;
    ShowDecimals   := False;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockTotal.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockTotal.ReadFromBlockData';
var
  LCurrentLine: string;
  LIndex,
  LCount,
  LErrCode: integer;
  LSubStr: string;
  LValue: Double;
begin

  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LIndex := 0;
    while(ABLockData.Lines.Count > 0) do
    begin

      LCurrentLine := ABLockData.Lines[0];
      ABLockData.Lines.Delete(0);

      LIndex := LIndex + 1;
      if(LIndex = 1) then
        Continue;

      Delete(LCurrentLine,1,14);
      for LCount := 0 to 9 do
      begin
        LSubStr := Copy(LCurrentLine,1,10);
        if(LSubStr <> '') then
        begin
          Val(LSubStr,LValue,LErrCode);
          if(LErrCode = 0) then
          begin
            FValues[LCount].FData := LValue;
            FValues[LCount].FInitalised := True;
          end;
          Delete(LCurrentLine,1,Length(LSubStr));
        end;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockTotal.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockTotal.ReadFromDatabase';
var
  LDataSet: TAbstractModelDataSet;
  LModel,LStudy,LSubArea,LScenario: string;
  LIndex: integer;
  LColumnName,
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber,Identifier';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', SequenceFailuresValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockSequencesWithFailuresValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' AND Identifier = '+ IntToStr(TotalAVGLine)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber,Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.EOF do
      begin
        for LIndex := 0 to 9 do
        begin
          LColumnName :=  Format('%s%2.2d',['SequenceFailuresValue',LIndex+1]);
          if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
          begin
            FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
            FValues[LIndex].FInitalised := True;
          end;
        end;
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockTotal.SaveToDatabase(ADatabaseName, AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockTotal.SaveToDatabase';
var
  LIndex: integer;
  LDataSet: TAbstractModelDataSet;
  LFieldName: string;
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);;
    try
      LSQLText :=
        'INSERT INTO suBlockSequencesWithFailuresValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', SequenceFailuresValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier ';
            for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', :SequenceFailuresValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);

      LDataSet.ClearQueryParams;
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                          'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                         [AModel, AStudy, ASubArea, AScenario, 
                          IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(TotalAVGLine)]);

      for LIndex := 0 to 9 do
      begin
        if FValues[LIndex].FInitalised then
        begin
          LFieldName := Format('%s%2.2d',['SequenceFailuresValue',LIndex+1]);
          LDataSet.SetParams([LFieldName], [FloatToStr(FValues[LIndex].FData)]);
        end;
      end;
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutSequencesWithFailuresBlockTotal.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutSequencesWithFailuresBlockTotal.SaveToStream';
var
  LLine: string;
  LCount: integer;
begin
  try
    LLine :='                --------  --------  --------  --------  --------  --------  --------  --------  '+
            '--------  --------' +  #13#10;
    Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

    LLine := '      TOT      ';
    for LCount := 0 to 9 do
    begin
      if FValues[LCount].FInitalised then
      begin
          LLine := LLine + PadDouble(FValues[LCount]);
      end;
    end;

    LLine := LLine + #13#10;
    Stream.WriteBuffer(PChar(LLine)^, Length(LLine));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutCriticalPeriodsBlockHeading }

function TSumOutCriticalPeriodsBlockHeading.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockHeading.ReadFromBlockData';
var
  LCurrentLine: string;
begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      FHeadingLines.Add(LCurrentLine);
      ABLockData.Lines.Delete(0);
      if (Pos(lblCriticalPeriodsNumber,LCurrentLine) > 0) or
         (Pos(lblCriticalPeriodsLength,LCurrentLine) > 0) or
         (Pos(lblCriticalPeriodsDeficit,LCurrentLine) > 0) or
         (Pos(lblCriticalPeriodsAvarage,LCurrentLine) > 0) then
        Break;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutCriticalPeriodsBlockValues }

procedure TSumOutCriticalPeriodsBlockValues.CreateMemberObjects;
const OPNAME = 'TSumOutCriticalPeriodsBlockValues.CreateMemberObjects';
var
  LIndex: integer;
  LCurrentValue: TDouble;
begin
  inherited;
  try
    FCaption    := TString.Create;
    FFirstInteger :=  TInteger.Create;
    for LIndex := LowIndex to HighIndex do
    begin
       LCurrentValue := TDouble.Create;
       FPeriodsValues[LIndex] := LCurrentValue;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutCriticalPeriodsBlockValues.DestroyMemberObjects;
const OPNAME = 'TSumOutCriticalPeriodsBlockValues.DestroyMemberObjects';
var
 LIndex: integer;
begin
  inherited;
  try
    FreeAndNil(FCaption);
    FreeAndNil(FFirstInteger);
    for LIndex := LowIndex to HighIndex do
    begin
      FPeriodsValues[LIndex].Free;
      FPeriodsValues[LIndex] := nil;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockValues.Initialise';
var
  LIndex: integer;
Begin
  Result := False;
  try
    ValCountLength := 10;
    ValYearLength  := 10;
    ValFirstLength := 10;
    ValOtherLength := 10;
    ValAvgLength   := 10;
    Decimals       := 4;
    ShowDecimals   := True;
    ExitLabel1     := '';
    ExitLabel2     := '';
    Result := inherited Initialise;

    FFirstInteger.FData := 0;
    FFirstInteger.FLength := 9;
    FFirstInteger.FDefaultPadding := True;
    FFirstInteger.FInitalised := False;

    FCaption.FData := '';
    FCaption.FLength := 0;
    FCaption.FDefaultPadding := True;
    FCaption.FInitalised := False;

    for LIndex := LowIndex to HighIndex do
    begin
      FPeriodsValues[LIndex].FData := 0.0;
      FPeriodsValues[LIndex].FLength := 10;
      FPeriodsValues[LIndex].FDecimal := 2;
      FPeriodsValues[LIndex].FInitalised := False;
      FPeriodsValues[LIndex].FDefaultPadding := True;

      FValues[LIndex].FData := 0.0;
      FValues[LIndex].FLength := 10;
      FValues[LIndex].FDecimal := 4;
      FValues[LIndex].FInitalised := False;
      FValues[LIndex].ShowDecimalPoint := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockValues.ReadFromBlockData(ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockValues.ReadFromBlockData';
var
  LCurrentLine: string;
  //LLineValues: TValuesLine;
  LIndex,
  LCount,
  LIntValue,
  LErrCode: integer;
  LSubStr: string;
  LValue: Double;
begin

  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    LIndex := 0;
    while(ABLockData.Lines.Count > 0) do
    begin
      LIndex := LIndex + 1;
      LCurrentLine := ABLockData.Lines[0];
      ABLockData.Lines.Delete(0);
      if (LCurrentLine = '') then
        Continue;

      if (LIndex = 1) then
      begin
        Delete(LCurrentLine,1,15);
        for LCount := 0 to 9 do
        begin
          LSubStr := Copy(LCurrentLine,1,10);
          if(LSubStr <> '') then
          begin
            Val(LSubStr,LValue,LErrCode);
            if(LErrCode = 0) then
            begin
              FValues[LCount].FData := LValue;
              FValues[LCount].FInitalised := True;
            end;
            Delete(LCurrentLine,1,Length(LSubStr));
          end;
        end;
        Result := True;
        Continue;
      end;

      if (LIndex = 4) then
      begin
        FCaption.FData := LCurrentLine;
        FCaption.FInitalised := True;
        FCaption.FLength := Length(LCurrentLine);
        Result := True;
        Continue;
      end;

      if (LIndex = 5) then
      begin
        LSubStr := Copy(LCurrentLine,1,9);
        Val(LSubStr,LIntValue,LErrCode);
        if(LErrCode = 0) then
        begin
          FFirstInteger.FData := LIntValue;
          FFirstInteger.FInitalised := True;
        end;
        Delete(LCurrentLine,1,Length(LSubStr)+5);

        for LCount := 0 to 9 do
        begin
          LSubStr := Copy(LCurrentLine,1,10);
          if(LSubStr <> '') then
          begin
            Val(LSubStr,LValue,LErrCode);
            if(LErrCode = 0) then
            begin
              FPeriodsValues[LCount].FData := LValue;
              FPeriodsValues[LCount].FInitalised := True;
            end;
            Delete(LCurrentLine,1,Length(LSubStr));
          end;
        end;
        Result := True;
        Continue;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockValues.ReadFromDatabase(ADatabaseName: string; ADataset:TDataset): boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockValues.ReadFromDatabase';
var
  LDataSet: TAbstractModelDataSet;
  LModel,LStudy,LSubArea,LScenario: string;
  LCount,
  LIndex: integer;
  LColumnName,
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not assigned.');
    if not ADataset.Active then
      raise Exception.Create('Dataset parameter is not active.');
    if (ADataset.RecordCount = 0) then
      raise Exception.Create('Dataset parameter is empty.');
    if ADataset.Eof then
      raise Exception.Create('Dataset parameter past OEF.');

    if not ReadBlockIDs(ADataset) then
      Exit;

    LModel       := '';
    LStudy       := '';
    LSubArea     := '';
    LScenario    := '';

    LModel := Trim(ADataset.FieldByName('Model').AsString);
    LStudy := Trim(ADataset.FieldByName('StudyAreaName').AsString);
    LSubArea := Trim(ADataset.FieldByName('SubArea').AsString);
    LScenario := Trim(ADataset.FieldByName('Scenario').AsString);

    FBlockType   := TOutputDataType(ADataset.FieldByName('BlockType').AsInteger);
    FBlockNumber := ADataset.FieldByName('BlockNumber').AsInteger;
    FLoadCaseNumber := ADataset.FieldByName('LoadCaseNumber').AsInteger;
    FSequenceNumber := ADataset.FieldByName('SequenceNumber').AsInteger;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);;
    try
      LSQLText :=
        ' SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier'+
        ' , FirstInteger,CriticalPeriodValCaption ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', CriticalPeriodsValue',LIndex+1]);
        LSQLText := LSQLText +
        ' FROM suBlockCriticalPeriodsValues'+
        ' WHERE Model = '+ QuotedStr(LModel)+
        ' AND StudyAreaName = '+ QuotedStr(LStudy)+
        ' AND SubArea = '+ QuotedStr(LSubArea)+
        ' AND Scenario = '+ QuotedStr(LScenario)+
        ' AND BlockNumber = '+ IntToStr(FBlockNumber)+
        ' AND LoadCaseNumber = '+ IntToStr(FLoadCaseNumber)+
        ' AND SequenceNumber = '+ IntToStr(FSequenceNumber)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier';

      LDataSet.SetSQL(LSQLText);
      LDataSet.DataSet.Open;
      for LCount := 1 to 2 do
      begin

        if not LDataSet.DataSet.FieldByName('FirstInteger').IsNull then
        begin
          FFirstInteger.FData := LDataSet.DataSet.FieldByName('FirstInteger').AsInteger;
          FFirstInteger.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('CriticalPeriodValCaption').IsNull then
        begin
          FCaption.FData := Trim(LDataSet.DataSet.FieldByName('CriticalPeriodValCaption').AsString);
          FCaption.FInitalised := True;
        end;

        for LIndex := 0 to 9 do
        begin
          LColumnName := Format('%s%2.2d',['CriticalPeriodsValue',LIndex+1]);
          if not LDataSet.DataSet.FieldByName(LColumnName).IsNull then
          begin
            if (LCount = 1) then
            begin
                FValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
                FValues[LIndex].FInitalised := True;
            end
            else
            begin
              FPeriodsValues[LIndex].FData := LDataSet.DataSet.FieldByName(LColumnName).AsFloat;
              FPeriodsValues[LIndex].FInitalised := True;
            end
          end;
        end;

        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockValues.SaveToDatabase(ADatabaseName, AModel, AStudy, ASubArea, AScenario: string): boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockValues.SaveToDatabase';
var
  LCount : integer;
  LIndex: integer;
  LDataSet: TAbstractModelDataSet;
  LFieldName: string;
  LSQLText: string;
begin
  Result := False;
  try
    if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);;
    try
      LSQLText :=
        'INSERT INTO suBlockCriticalPeriodsValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier'+
        ' , FirstInteger,CriticalPeriodValCaption ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', CriticalPeriodsValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier'+
        ' ,:FirstInteger, :CriticalPeriodValCaption ';
            for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', :CriticalPeriodsValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);
      for LCount := 1 to 2 do
      begin
        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                           [AModel, AStudy, ASubArea, AScenario,
                            IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(LCount)]);

        if FFirstInteger.FInitalised then
          LDataSet.SetParams(['FirstInteger'], [IntToStr(FFirstInteger.FData)]);

        if FCaption.FInitalised then
          LDataSet.SetParams(['CriticalPeriodValCaption'], [FCaption.FData]);

        for LIndex := 0 to 9 do
        begin
          if (LCount = 1) then
          begin
            if FValues[LIndex].FInitalised then
            begin
              LFieldName := Format('%s%2.2d',['CriticalPeriodsValue',LIndex+1]);
              LDataSet.SetParams([LFieldName], [FloatToStr(FValues[LIndex].FData)]);
            end;
          end
          else
          begin
            if FPeriodsValues[LIndex].FInitalised then
            begin
              LFieldName := Format('%s%2.2d',['CriticalPeriodsValue',LIndex+1]);
              LDataSet.SetParams([LFieldName], [FloatToStr(FPeriodsValues[LIndex].FData)]);
            end;
          end;
        end;
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutCriticalPeriodsBlockValues.SaveToStream(Stream: TStream);
const OPNAME = 'TSumOutCriticalPeriodsBlockValues.SaveToStream';
var
  LLine: string;
  LIndex,
  LCount: integer;
begin
  try
    for LIndex := 1 to 5 do
    begin
      if (LIndex = 2) or (LIndex = 3) then
      begin
        LLine :=  #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      if (LIndex = 1) then
      begin
        LLine := '               ';
        for LCount := 0 to 9 do
        begin
          if FValues[LCount].FInitalised  then
          begin
            LLine := LLine + PadDouble(FValues[LCount]);
          end;
        end;
        LLine := LLine + #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      if (LIndex = 4) then
      begin
        LLine := FCaption.FData;
        LLine := LLine + #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;

      if (LIndex = 5) then
      begin
        LLine := PadInt(FFirstInteger);

        for LCount := 0 to 9 do
        begin
          if(FPeriodsValues[LCount].FInitalised) then
          begin
            LLine := LLine + PadDouble(FPeriodsValues[LCount]);
          end;
        end;
        LLine := LLine + #13#10;
        Stream.WriteBuffer(PChar(LLine)^, Length(LLine));
        Continue;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReserviorIDsAgent }

procedure TReserviorIDsAgent.CreateMemberObjects;
const OPNAME = 'TReserviorIDsAgent.CreateMemberObjects';
Begin
  inherited;
  try
    FReserviorIDs   := TStringList.Create;
    FReserviorNames := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReserviorIDsAgent.DestroyMemberObjects;
const OPNAME = 'TReserviorIDsAgent.DestroyMemberObjects';
Begin
  inherited;
  try
    FreeAndNil(FReserviorIDs);
    FreeAndNil(FReserviorNames);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReserviorIDsAgent.GetReserviorId(AReserviorName: string): integer;
const OPNAME = 'TReserviorIDsAgent.GetReserviorId';
var
  LIndex,
  LCount,
  LCurrentMatchingCharCount,
  LPrevMatchingCharCount,
  LPosWithIndex: integer;
  LFirstWord1,
  LFirstWord2,
  LString1,
  LString2: string;
Begin
  Result := -1;
  try
    LString1 := Uppercase(Trim(AReserviorName));
    if(LString1 = '') then
    Exit;

    LPrevMatchingCharCount    := 0;
    LPosWithIndex := -1;
    for Lindex := 0 to FReserviorNames.Count -1 do
    begin
      LString2 := Uppercase(Trim(FReserviorNames[Lindex]));
      if(LString2 = '') then
      Continue;

      if(pos(LString2,LString1) > 0 ) then
      begin
        LPosWithIndex := Lindex;
        Break;
      end
      else
      begin
        LCurrentMatchingCharCount := 0;
        for LCount := 1 to (min(Length(LString1),Length(LString2))) do
        begin
          if(LString1[LCount] <> LString2[LCount]) then
            break;
           LCurrentMatchingCharCount := LCurrentMatchingCharCount + 1;
        end;
        if(LCurrentMatchingCharCount  > 0) then
        begin
          if(LCurrentMatchingCharCount  > LPrevMatchingCharCount) then
          begin
            LPrevMatchingCharCount := LCurrentMatchingCharCount;
            LPosWithIndex := Lindex;
          end;
        end;
      end;
    end;

    if (LPosWithIndex >= 0) then
    begin
      //heck if the first word match
      if (LPrevMatchingCharCount <> 0) then
      begin
        LString2 := Uppercase(Trim(FReserviorNames[LPosWithIndex]));
        if(Pos(' ',LString1) = 0) then
          LString1 := LString1 + ' ';
        if(Pos(' ',LString2) = 0) then
          LString2 := LString2 + ' ';
        LIndex := Pos(' ',LString1);
        LFirstWord1 := Copy(LString1,1,LIndex);
        LIndex := Pos(' ',LString2);
        LFirstWord2 := Copy(LString2,1,LIndex);
        if(LFirstWord1 <> LFirstWord2) then
          LPosWithIndex := -1;
      end;
      if (LPosWithIndex >= 0) then
        Result := StrToInt(FReserviorIDs[LPosWithIndex]);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReserviorIDsAgent.Initialise: boolean;
const OPNAME = 'TReserviorIDsAgent.Initialise';
Begin
  Result := inherited Initialise;
  try
    FReserviorIDs.Clear;
    FReserviorNames.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAnualSummaryValuesLine }

constructor TAnualSummaryValuesLine.Create;
const OPNAME = 'TAnualSummaryValuesLine.Create';
var
  LCount: integer;
  LDouble:TDouble;
begin
  inherited Create;
  try
    FFirstInteger       := TInteger.Create;
    for LCount := LowIndex to HighIndex do
    begin
     LDouble := TDouble.Create;
     FAnualSummaryValuesLine[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAnualSummaryValuesLine.Destroy;
const OPNAME = 'TAnualSummaryValuesLine.Destroy';
var
  LCount: integer;
begin
  try
    FreeAndNil(FFirstInteger);
    for LCount := LowIndex to HighIndex do
    begin
     FAnualSummaryValuesLine[LCount].Free;
     FAnualSummaryValuesLine[LCount] := nil;
    end;

    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnualSummaryValuesLine.Initialise: boolean;
const OPNAME = 'TAnualSummaryValuesLine.Initialise';
var
  LCount: integer;
  LDouble:TDouble;
begin
  Result := False;
  try
    FFirstInteger.FData := 0;
    FFirstInteger.FLength := 9;
    FFirstInteger.FDefaultPadding := True;
    FFirstInteger.FInitalised := False;

    for LCount := LowIndex to HighIndex do
    begin
      LDouble := FAnualSummaryValuesLine[LCount];
      if Assigned(LDouble) then
      begin
        LDouble.FData := 0.0;
        LDouble.FLength := 10;
        LDouble.FDecimal := 2;
        LDouble.FInitalised := False;
        LDouble.FDefaultPadding := True;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAnualSummaryValues }

function TAnualSummaryValues.GetAnualSummaryValuesLine(AIndex: Integer): TAnualSummaryValuesLine;
const OPNAME = 'TAnualSummaryValues.GetAnualSummaryValuesLine';
begin
  Result := Nil;
  try
    if (AIndex >=0) and (AIndex < Self.Count) then
      Result := TAnualSummaryValuesLine(Self.Items[AIndex]);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnualSummaryValues.Initialise: boolean;
const OPNAME = 'TAnualSummaryValues.Initialise';
var
  LCount: integer;
  LAnualSummaryValuesLine: TAnualSummaryValuesLine;
begin
  Result := False;
  try
    for LCount := 0 to Self.Count - 1 do
    begin
      LAnualSummaryValuesLine := AnualSummaryValuesLine[LCount];
      if Assigned(LAnualSummaryValuesLine) then
         LAnualSummaryValuesLine.Initialise;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSumOutRecurrenceIntervalBlockValues }

procedure TSumOutRecurrenceIntervalBlockValues.CreateMemberObjects;
const OPNAME = 'TSumOutRecurrenceIntervalBlockValues.CreateMemberObjects';
var
  lIndex : integer;
  lCurrentValue : TInteger;
  lPeriodLength : TInteger;
  lTargetDraft :  TDouble;
begin
  inherited;
  try
    FCaption            := TString.Create;
    FAnualValues        := TAnualSummaryValues.Create ( True );
    for lIndex := LowIndex to HighIndex do
    begin
       lCurrentValue := TInteger.Create;
       lPeriodLength :=  TInteger.Create;
       lTargetDraft :=  TDouble.Create;
       FNumberOfFailureSeqValues [ lIndex ] := lCurrentValue;
       FPeriodLength [ lIndex ] := lPeriodLength;
       FTargetDraft [ lIndex ] := lTargetDraft;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSumOutRecurrenceIntervalBlockValues.DestroyMemberObjects;
const OPNAME = 'TSumOutRecurrenceIntervalBlockValues.DestroyMemberObjects';
var
 lIndex: integer;
begin
  inherited;
  try
    FreeAndNil(FCaption);
    FreeAndNil ( FAnualValues );
    for lIndex := LowIndex to HighIndex do
    begin
      FPeriodLength [ lIndex ].Free;
      FPeriodLength [ lIndex ] := nil;
      FNumberOfFailureSeqValues [ lIndex ].Free;
      FNumberOfFailureSeqValues [ lIndex ] := nil;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockValues.Initialise : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockValues.Initialise';
var
  LIndex: integer;
Begin
  Result := False;
  try
    ValCountLength := 10;
    ValYearLength  := 10;
    ValFirstLength := 10;
    ValOtherLength := 10;
    ValAvgLength   := 10;
    Decimals       := 4;
    ShowDecimals   := True;
    ExitLabel1     := '';
    ExitLabel2     := '';

    FCaption.FData := '';
    FCaption.FLength := 0;
    FCaption.FDefaultPadding := True;
    FCaption.FInitalised := False;

    Result := inherited Initialise;

    for LIndex := LowIndex to HighIndex do
    begin

      FPeriodLength[LIndex].FData := 0;
      FPeriodLength[LIndex].FLength := 10;
      FPeriodLength[LIndex].FInitalised := False;

      FTargetDraft[LIndex].FData := 0.0;
      FTargetDraft[LIndex].FLength := 10;
      FTargetDraft[LIndex].FDecimal := 4;
      FTargetDraft[LIndex].FInitalised := False;
      FTargetDraft[LIndex].ShowDecimalPoint := True;

      FValues[LIndex].FData := 0.0;
      FValues[LIndex].FLength := 10;
      FValues[LIndex].FDecimal := 4;
      FValues[LIndex].FInitalised := False;
      FValues[LIndex].ShowDecimalPoint := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockValues.ReadFromBlockData ( ABLockData : TDataBlock ) : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockValues.ReadFromBlockData';
var
  lCurrentLine: string;
  lIndex,
  lCount,
  lIntValue,
  lErrCode: integer;
  lSubStr: string;
  lValue: Double;
  lAnualSummaryValuesLine : TAnualSummaryValuesLine;
begin
  Result := False;
  try
    FBlockType :=  ABLockData.BlockType;
    lIndex := 0;
    FCaption.FData := ABLockData.Lines.Text;
    FCaption.FInitalised := True;
    FCaption.FLength := Length(FCaption.FData);
//    ABLockData.Lines.Delete(0);
    while ABLockData.Lines.Count > 0 do
    begin
      lCurrentLine := ABLockData.Lines[0];
      ABLockData.Lines.Delete(0);
      if (lCurrentLine = '') {or ( lIndex = 1 )} then
        Continue;

      lIndex := lIndex + 1;

      if (lIndex = 2) then
      begin
        Delete(lCurrentLine,1,13);
        for lCount := 0 to 9 do
        begin
          LSubStr := Copy(lCurrentLine,1,10);
          if (LSubStr <> '') then
          begin
            Val(lSubStr,lIntValue,lErrCode);
            if (lErrCode = 0) then
            begin
              FPeriodLength[lCount].FData := lIntValue;
              FPeriodLength[lCount].FInitalised := True;
            end;
            Delete(lCurrentLine, 1, Length(lSubStr));
          end;
        end;
        Result := True;
        Continue;
      end;
        if (lIndex > 2)  then
        begin
          LIntValue := 0;
          LSubStr := Copy(lCurrentLine,1,14);
          if (LSubStr <> '') then
          begin
            Val(lSubStr,lValue,lErrCode);
            if (lErrCode = 0 ) then
            begin
              FTargetDraft[lIndex].FData := lValue;
              FTargetDraft[lIndex].FInitalised := True;
            end;
            Delete(lCurrentLine, 1, Length(lSubStr));
          end;
          while (lCurrentLine <> '') do
          begin
            LIntValue := LIntValue + 1;
            lAnualSummaryValuesLine := TAnualSummaryValuesLine.Create;
            FAnualValues.Add(LAnualSummaryValuesLine);

            lAnualSummaryValuesLine.FirstInteger.FData := LIntValue;
            lAnualSummaryValuesLine.FirstInteger.FInitalised := True;
          for lCount := 0 to 9 do
          begin
            LSubStr := Copy(lCurrentLine,1,10);
            if (LSubStr <> '') then
            begin
              Val(lSubStr,lValue,lErrCode);
              if (lErrCode = 0) then
              begin
                lAnualSummaryValuesLine.AnualSummaryValuesLine[LCount].FData := lValue;
                lAnualSummaryValuesLine.AnualSummaryValuesLine[LCount].FInitalised := True;
              end;
              Delete(lCurrentLine, 1, Length(lSubStr));
            end;
          end;
          Result := True;
          Continue;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutRecurrenceIntervalBlockValues.ReadFromDatabase ( ADatabaseName : string;
                                                                 ADataset : TDataset ) : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockValues.ReadFromDatabase';
begin
  Result := False;
  try
    try

    finally
    
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockValues.SaveToDatabase ( ADatabaseName, AModel,
                                                               AStudy, ASubArea, AScenario : string ) : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockValues.SaveToDatabase';
{var
  LCount : integer;
  LIndex : integer;
  LDataSet : TAbstractModelDataset;
  LFieldName : string;
  LSQLText : string;
  LAnualSummaryValuesLine : TAnualSummaryValuesLine;
  }
begin
    Result := False;
  try
   { if (Trim(ADatabaseName)='') then
      raise Exception.Create('Database name parameter is empty.');
    if (Trim(AModel)='') then
      raise Exception.Create('Model parameter is empty.');
    if (Trim(AStudy)='') then
      raise Exception.Create('Study parameter is empty.');
    if (Trim(ASubArea)='') then
      raise Exception.Create('SubArea parameter is empty.');
    if (Trim(AScenario)='') then
      raise Exception.Create('Scenario parameter is empty.');

    FAppModules.Database.CreateDataset ( integer ( dtExecSQL ), LDataSet );
    try
      LSQLText :=
        'INSERT INTO suBlockAnualSummaryValues'+
        ' (Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber, Identifier'+
        ' , FirstInteger,YearPeriod, AnualSummaryCaption ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', AnualSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ') VALUES';
        LSQLText := LSQLText +
        ' (:Model, :StudyAreaName, :SubArea, :Scenario, :BlockNumber, :LoadCaseNumber,:SequenceNumber, :Identifier'+
        ' ,:FirstInteger, :YearPeriod, :AnualSummaryCaption ';
        for LIndex := 0 to 9 do
          LSQLText := LSQLText + Format('%s%2.2d',[', :AnualSummaryValue',LIndex+1]);
        LSQLText := LSQLText + ')';

      LDataSet.SetSQL(LSQLText);
      LDataSet.ClearQueryParams;
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                          'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                          [AModel, AStudy, ASubArea, AScenario,
                          IntToStr(FBlockNumber), IntToStr(FLoadCaseNumber),IntToStr(FSequenceNumber), IntToStr(1)]);

      if FCaption.FInitalised then
        LDataSet.SetParams ( [ 'AnualSummaryCaption' ], [ FCaption.FData ] );

      for LIndex := 3 to 11 do
      begin
        if FTargetDraft [ LIndex ].FInitalised then
        begin
          LFieldName := Format ( '%s%2.2d', [ 'AnualSummaryValue', LIndex - 2 ] );
          LDataSet.SetParams ( [ LFieldName ], [ FloatToStr ( FTargetDraft [ LIndex ].FData ) ] );
        end;
      end;
      LDataSet.ExecSQL;

      for LIndex := 0 to 9 do
      begin

        if FPeriodLength [ LIndex ].FInitalised then
        begin
          LFieldName := Format('%s%2.2d',[ 'AnualSummaryValue', LIndex + 1 ] );
          LDataSet.SetParams ( [ LFieldName ], [ IntToStr ( FPeriodLength [ LIndex ].FData ) ] );
        end;
//        LDataSet.SetSQL ( LSQLText );

//        LDataSet.ClearQueryParams;
//        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
//                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
//                           [AModel, AStudy, ASubArea, AScenario,
//                            IntToStr ( FBlockNumber ), IntToStr ( FRunNumber ), IntToStr ( LCount + 2 ) ] );

      end;
      LDataSet.ExecSQL;
      for LCount := 0  to FAnualValues.Count - 1 do
      begin
        LAnualSummaryValuesLine := FAnualValues.AnualSummaryValuesLine[LCount];
        LDataSet.SetSQL(LSQLText);

        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'BlockNumber','LoadCaseNumber','SequenceNumber','Identifier'],
                           [AModel, AStudy, ASubArea, AScenario,
                            IntToStr ( FBlockNumber ), IntToStr ( FRunNumber ), IntToStr ( LCount + 2 ) ] );


        if LAnualSummaryValuesLine.FirstInteger.FInitalised then
          LDataSet.SetParams( [ 'FirstInteger' ], [ IntToStr ( LAnualSummaryValuesLine.FirstInteger.FData ) ] );

        for LIndex := 0 to 9 do
        begin
          if LAnualSummaryValuesLine.AnualSummaryValuesLine [ LIndex ].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',[ 'AnualSummaryValue', LIndex + 1 ] );
            LDataSet.SetParams ( [ LFieldName ], [FloatToStr ( LAnualSummaryValuesLine.AnualSummaryValuesLine [ LIndex ].FData ) ] );
          end;
        end;
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
    }
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSumOutRecurrenceIntervalBlockValues.SaveToStream ( Stream : TStream );
const OPNAME = 'TSumOutRecurrenceIntervalBlockValues.SaveToStream';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TSumOutRecurrenceIntervalBlockHeading }

function TSumOutRecurrenceIntervalBlockHeading.ReadFromBlockData ( ABLockData : TDataBlock ) : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockHeading.ReadFromBlockData';
var
  LCurrentLine: string;
begin
  Result := False;
  try
    //Reset;
    FBlockType := ABLockData.BlockType;
    while(ABLockData.Lines.Count > 0) do
    begin
      LCurrentLine := ABLockData.Lines[0];
      FHeadingLines.Add(LCurrentLine);
      ABLockData.Lines.Delete(0);

      if (Pos(lblNumberOfFailureSequence,LCurrentLine) > 0) or
         (Pos(lblFirmYieldRecurrenceInterval,LCurrentLine) > 0) then
        Break;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TSumOutAnualFirmEnergyDemandBlockValues }

procedure TSumOutAnualFirmEnergyDemandBlockValues.CreateMemberObjects;
const OPNAME = 'TSumOutAnualFirmEnergyDemandBlockValues.CreateMemberObjects';
var
  LIndex : integer;
  LActualDemand :  TDouble;
begin
  inherited;
  try
    FCaption            := TString.Create;
    FAnualValues        := TAnualSummaryValues.Create(True);
    for LIndex := LowIndex to HighIndex do
    begin
       LActualDemand :=  TDouble.Create;
       FActualDemand[LIndex] := LActualDemand;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSumOutAnualFirmEnergyDemandBlockValues.DestroyMemberObjects;
const OPNAME = 'TSumOutAnualFirmEnergyDemandBlockValues.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited;
  try
    FreeAndNil(FCaption);
    FreeAndNil(FAnualValues);
    for lIndex := LowIndex to HighIndex do
    begin
      FActualDemand[LIndex].Free;
      FActualDemand[LIndex] := nil;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutAnualFirmEnergyDemandBlockValues.Initialise: boolean;
const OPNAME = 'TSumOutAnualFirmEnergyDemandBlockValues.Initialise';
var
  LIndex: integer;
Begin
  Result := False;
  try
    ValCountLength := 10;
    ValYearLength  := 10;
    ValFirstLength := 10;
    ValOtherLength := 10;
    ValAvgLength   := 10;
    Decimals       := 4;
    ShowDecimals   := True;
    ExitLabel1     := '';
    ExitLabel2     := '';

    FCaption.FData := '';
    FCaption.FLength := 0;
    FCaption.FDefaultPadding := True;
    FCaption.FInitalised := False;
    Result := inherited Initialise;

    for LIndex := LowIndex to HighIndex do
    begin

      FActualDemand[LIndex].FData := 0.0;
      FActualDemand[LIndex].FLength := 10;
      FActualDemand[LIndex].FDecimal := 4;
      FActualDemand[LIndex].FInitalised := False;
      FActualDemand[LIndex].ShowDecimalPoint := True;

      FValues[LIndex].FData := 0.0;
      FValues[LIndex].FLength := 10;
      FValues[LIndex].FDecimal := 4;
      FValues[LIndex].FInitalised := False;
      FValues[LIndex].ShowDecimalPoint := True;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutAnualFirmEnergyDemandBlockValues.ReadFromBlockValues(ANetworkElementID : integer;ABLockData: TDataBlock): boolean;
const OPNAME = 'TSumOutAnualFirmEnergyDemandBlockValues.ReadFromBlockValues';
var
  LCurrentLine: string;
  LIndex,
  LCount,
  LIntValue,
  LErrCode: integer;
  LSubStr: string;
  LValue: Double;
  LAnualSummaryValuesLine : TAnualSummaryValuesLine;
  LNetworkElementID : integer;
  LChannel : IGeneralFlowChannel;
begin
  Result := False;
  try
    FBlockType :=  ABLockData.BlockType;
    FCaption.FData := ABLockData.Lines.Text;
    FCaption.FInitalised := True;
    FCaption.FLength := Length(FCaption.FData);
    for LIndex := 0 to 6 do
      ABLockData.Lines.Delete(0);
    LIndex := 0;
    while ABLockData.Lines.Count > 0 do
    begin
      LCurrentLine := ABLockData.Lines[0];
      ABLockData.Lines.Delete(0);

      if (LCurrentLine = '') or (Pos('DEFICITS  (PROPORTION)',LCurrentLine)>0)then
        Continue;

      if (Pos('WATER SUPPLY (MCM)',LCurrentLine)>0) then
        Exit;

      LIndex := LIndex + 1;

      if LIndex = 1 then
      begin
        LCurrentLine := Copy(LCurrentLine,85,Length(LCurrentLine));
        if Trim(LCurrentLine)<> '' then
        begin
          LNetworkElementID := StrToInt(Trim(LCurrentLine));
          if ANetworkElementID <> LNetworkElementID then
            Exit
          else
            Continue;
        end
        else
        begin
          LChannel := (FAppModules.Model.ModelData as IYieldModelData).
                 NetworkElementData.ChannelList.ChannelByChannelNumber[ANetworkElementID];
          if (LChannel.MasterControlFeature<>nil) then
            Continue
          else
            Exit;
        end;

      end;

      if (LIndex = 2) then
      begin
        Delete(LCurrentLine,1,16);
        for LCount := 0 to 9 do
        begin
          LSubStr := Copy(LCurrentLine,1,10);
          if (Trim(LSubStr) <> '') then
          begin
            Val(Trim(LSubStr),LValue,LErrCode );
            if (LErrCode = 0) then
            begin
              FActualDemand[LCount].FData := LValue;
              FActualDemand[LCount].FInitalised := True;
            end;
            Delete(LCurrentLine, 1,Length(LSubStr));
          end;
        end;
        Result := True;
        Continue;
      end;

      if(LIndex > 2)  then
      begin
        LSubStr := Copy(LCurrentLine,1,10);
        if (Trim(LSubStr) <> '') then
        begin
          Val(Trim(LSubStr),LIntValue,LErrCode);
          if (LErrCode = 0) then
          begin
            LAnualSummaryValuesLine := TAnualSummaryValuesLine.Create;
            FAnualValues.Add(LAnualSummaryValuesLine);
            LAnualSummaryValuesLine.FirstInteger.FData := LIntValue;
            LAnualSummaryValuesLine.FirstInteger.FInitalised := True;
            Delete(LCurrentLine, 1, Length(LSubStr)+6);
            while (lCurrentLine <> '' ) do
            begin
              for LCount := 0 to 9 do
              begin
                LSubStr := Copy(LCurrentLine,1,10);
                if (Trim(LSubStr) <> '' ) then
                begin
                  Val(Trim(LSubStr),LValue,lErrCode );
                  if (LErrCode = 0) then
                  begin
                    LAnualSummaryValuesLine.AnualSummaryValuesLine[LCount].FData := LValue;
                    LAnualSummaryValuesLine.AnualSummaryValuesLine[LCount].FInitalised := True;
                  end;
                  Delete(LCurrentLine, 1,Length(LSubStr));
                end;
              end;
            end;
            Result := True;
            Continue;
          end;
        end;
      end;
     end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
