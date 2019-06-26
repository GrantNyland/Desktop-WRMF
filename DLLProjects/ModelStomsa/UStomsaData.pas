unit UStomsaData;

interface

uses SysUtils, Classes, VCL.Dialogs, Windows, VCL.Controls,Contnrs,
     UFlowCorrelation, UFileNames,  UAbstractModelData, UAbstractFileNamesObject, UViewModelDataObject;

const
  TheMonths : Array[1..13] of string = ('OCTOBER', 'NOVEMBER', 'DECEMBER', 'JANUARY',
                                        'FEBRUARY', 'MARCH', 'APRIL', 'MAY', 'JUNE',
                                        'JULY', 'AUGUST', 'SEPTEMBER', 'ANNUAL');

type
  array1F          = Array[1..1] of Double;
  arrayCoefs       = Array[1..4] of Double;
  array5F          = Array[1..5] of double;
  array6F          = Array[1..6] of double;
  array7F          = Array[1..7] of Double;
  array9F          = Array[1..9] of double;
  array10F         = Array[1..10] of double;
  array12F         = Array[1..12] of double;
  array13F         = Array[1..13] of double;
  array20F         = Array[1..20] of double;
  array30F         = Array[1..30] of double;
  array101F        = Array[1..101] of double;
  array200F        = Array[1..200] of double;
  array500F        = Array[1..500] of double;
  array1000F       = Array[1..1000] of Double;
  arrayT2_9        = Array[1..2,1..9] of double;
  arrayT2_30       = Array[1..2,1..30] of Double;
  array7_5F        = Array[1..7,1..5] of double;
  array7_13F       = Array[1..7,1..13] of double;
  array7_10F       = Array[1..7,1..10] of double;
  array12_7F       = Array[1..12,1..7] of double;
  array12_100F     = Array[1..12,1..100] of double;
  array13_100F     = Array[1..13,1..100] of double;
  array13_400F     = Array[1..13,1..400] of double;
  FittedArray      = Array[0..3,0..1000] Of Double;
  array101_20F     = Array[1..101,1..20] of double;
  array200_2F      = Array[1..200,1..2] of Double;
  array200_200F    = Array[1..200,1..200] of double;
  array500_500F    = Array[1..500,1..500] of double;
  array500_1000F   = Array[1..500,1..1000] of double;
  arrayT9_2_30     = Array[1..9,1..2,1..30] of Double;
  array_flows      = Array[1..500,1..100,1..12] of single;
  array_flows_400  = Array[1..500,1..400,1..12] of single;
  array101_100_12F = array[1..101,1..100,1..12] of double;
  array101_400_12F = array[1..101,1..400,1..12] of double;
  array101_100_13F = Array[1..101,1..100,1..13] of double;
  array101_400_13F = Array[1..101,1..400,1..13] of double;
  array_Hist       = Array[1..200,1..100,1..12] of double;
  array_Hist_400   = Array[1..200,1..400,1..12] of double;

  arrayT1I       = Array[1..1] of Integer;
  arrayT2I       = Array[1..2] of integer;
  array5I        = Array[1..5] of integer;
  arrayT9I       = Array[1..9] of integer;
  array10I       = Array[1..10] of integer;
  array13I       = Array[1..13] of integer;
  array20I       = Array[1..20] of integer;
  array200I      = Array[1..200] of integer;
  array500I      = Array[1..500] of integer;
  array1000I     = Array[1..1000] of integer;
  array7_5I      = Array[1..7,1..5] of integer;

  //This is the structured file used to store data for the STOMSA program
  //string types are kept to 72 bytes as this corresponds with the maximum
  //numeric storage 9 x double @ 8 bytes each = 72 bytes. This optimises the
  //storage, which is necessary for flow generation which will potentially
  //produce 121,200 doubles per gauge ( = 969,600 bytes)
  StomsaFileType = record
    RecordTag : String[15];
    Case RecordNumber : Word of
      //General Info
      1   : (Version : Word);
      2   : (FileName : String[72]);
      3   : (Directory : String[72]);
      //Inc File Data
      10  : (StartYear, EndYear, RecordLength : Integer);
      11  : (AnnualRawIncData, Month1, Month2, Month3, Month4, Month5, Month6,
                               Month7, Month8, Month9, Month10, Month11, Month12 : Double);
      //need to make provision to store the data for all the months in the file,
      //as this data is used in various places

      //Marginal Fitting Data
      100 : (DefaultCurve, UserCurve, ZeroCount, IType, IFault : Integer);
      101 : (KeyGauge : Boolean);
      102 : (SNVR : Double);
      103 : (Gamma1, Gamma2, Gamma3, Gamma4 : Double);
      104 : (Delta1, Delta2, Delta3, Delta4 : Double);
      105 : (XLamda1, XLamda2, XLamda3, XLamda4 : Double);
      106 : (XX1, XX2, XX3, XX4 : Double);
      107 : (Criterion1, Criterion2, Criterion3, Criterion4 : Double);
      108 : (NaturalFlows : Double);
      109 : (Fitted1, Fitted2, Fitted3, Fitted4 : Double);
      110 : (Standardised : Double);
      111 : (Normalised1, Normalised2, Normalised3, Normalised4 : Double);

      //Time Series Data
      //MDStats Items
      200 : (M1, M2 : Integer);
      201 : (Mean, StdDev, Skew, Excess, ChiSquare : Double);
      202 : (NormalMean, NormalStdDev, NormalSkew, NormalExcess, NormalChiSquare : Double);
      203 : (Skew95, Excess95, Exceedance1, Exceedance2 : Double);
      //Sercor Items
      210 : (NFRD, NLAG : Integer);
      211 : (NPORT, NEXCD : Double);
      212 : (Crit1, Crit2, Crit3, Crit4, Crit5, Crit6, Crit7, Crit8, Crit9 : Double);
      213 : (AIC1, AIC2, AIC3, AIC4, AIC5, AIC6, AIC7, AIC8, AIC9 : Double);
      214 : (DIST1, DIST2, DIST3, DIST4, DIST5, DIST6, DIST7, DIST8, DIST9 : Double);
      215 : (NCorr1, NCorr2 : Double);
      216 : (PCorr11, PCorr12, PCorr13, PCorr14, PCorr15, PCorr16, PCorr17, PCorr18, PCorr19 : Double);
      217 : (PCorr21, PCorr22, PCorr23, PCorr24, PCorr25, PCorr26, PCorr27, PCorr28, PCorr29 : Double);
      218 : (Phi11, Phi12, Phi13, Phi14, Phi15, Phi16, Phi17, Phi18, Phi19 : Double);
      219 : (Phi21, Phi22, Phi23, Phi24, Phi25, Phi26, Phi27, Phi28, Phi29 : Double);
      220 : (Theta11, Theta12, Theta13, Theta14, Theta15, Theta16, Theta17, Theta18, Theta19 : Double);
      221 : (Theta21, Theta22, Theta23, Theta24, Theta25, Theta26, Theta27, Theta28, Theta29 : Double);
      222 : (Hist11, Hist12, Hist13, Hist14, Hist15, Hist16, Hist17, Hist18, Hist19 : Double);
      223 : (Hist21, Hist22, Hist23, Hist24, Hist25, Hist26, Hist27, Hist28, Hist29 : Double);
      224 : (NConf : Double);
      225 : (PFRD1, PFRD2, PFRD3, PFRD4, PFRD5, PFRD6, PFRD7, PFRD8, PFRD9 : Integer);
      226 : (PLAG1, PLAG2, PLAG3, PLAG4, PLAG5, PLAG6, PLAG7, PLAG8, PLAG9 : Integer);
      227 : (PCONF1, PCONF2, PCONF3, PCONF4, PCONF5, PCONF6, PCONF7, PCONF8, PCONF9 : Double);
      228 : (PEXCD1, PEXCD2, PEXCD3, PEXCD4, PEXCD5, PEXCD6, PEXCD7, PEXCD8, PEXCD9 : Double);
      229 : (PPORT1, PPORT2, PPORT3, PPORT4, PPORT5, PPORT6, PPORT7, PPORT8, PPORT9 : Double);
      230 : (CRP1, CRP2, CRP3, CRP4, CRP5, CRP6 : Double);
      231 : (QI1, QI2, QI3, QI4, QI5, QI6, QI7, QI8, QI9, QI10, QI11, QI12 : Double);
      232 : (CRNX1, CRNX2, CRNX3, CRNX4, CRNX5, CRNX6, CRNX7, CRNX8, CRNX9, CRNX10, CRNX11, CRNX12 : Integer);
      233 : (CRJX1, CRJX2, CRJX3, CRJX4, CRJX5, CRJX6, CRJX7, CRJX8, CRJX9, CRJX10, CRJX11, CRJX12 : Integer);
      234 : (CRSX1, CRSX2, CRSX3, CRSX4, CRSX5, CRSX6, CRSX7, CRSX8, CRSX9, CRSX10, CRSX11, CRSX12 : Double);
      235 : (Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8, Rank9 : Integer);
      236 : (DefaultTimeSeries, UserTimeSeries : Integer);

      //Flow Generation Data
      300 : (SequenceCount, GeneratedYearCount : Integer);
      301 : (SequenceNumber, GeneratedYearNumber : Integer;
             FlowData1, FlowData2, FlowData3, FlowData4, FlowData5, FlowData6,
             FlowData7, FlowData8, FlowData9, FlowData10, FlowData11, FlowData12 : single);

      //Check Sum Entry - this should be equivalent to the total number of
      //lines in the record for this file, this checks to see if all the data
      //was located, i.e this will have to be an integer
      999 : (CheckSum : Boolean);//will extend in future
  end;//StomsaFileType

  //From CROSS dll
  TCrossResults = record
    Eigen_G0, Eigen_H1, Eigen_H0 : array500F;
    Root_G0, Root_H0, Root_H1, Coef_Z, Coef_A, BB, Differences : array500_500F;
  end;

  TCrossInput = record
    Lengths,StartYr : array500I;
    Phi1, Phi2, Theta1, Theta2 : array500F;
    CrossRawData : array500_1000F;
    Dispersion : array500_500F;
  end;

  TCrossData = class(TObject)
  public
    FCrossInput   : TCrossInput;
    FCrossResults : TCrossResults;
  end;

  TCrossLocalData = class(TObject)
  public
    NY : Array1000I;
    XY1, XY2, RV1 : array1000F;
    W : array1000F;
    G, G0, G1,
    U, V,
    H, H0, H1,
    S1 : Array500_500F;
  end;

  TDataChangedEvent = procedure(Sender: TObject; FileName, FileDirectory : string) of Object;
  TFileLoadError = procedure(Sender: TObject; ErrorList : TStringList) of Object;

  TIncData = class(TObject)
  protected
    //Key Gauge Data
    FKeyGauge             : Boolean;
    FMarginalFitted       : boolean;
    FTimeSeriesFitted     : boolean;
    FStatisticsCalculated : boolean;

    //Flow correlation data generated by MTHRNK
    FOnKeyGaugeChange         : TDataChangedEvent;
    FOnMarginalFitted         : TDataChangedEvent;
    FOnMarginalUnfitted       : TDataChangedEvent;
    FOnTimeSeriesFitted       : TDataChangedEvent;
    FOnTimeSeriesUnFitted     : TDataChangedEvent;
    FOnFlowsAndStatsGenerated : TNotifyEvent;

    procedure Set_KeyGauge(AKeyGauge : boolean);
    procedure Set_MarginalFitted(AMarginalFitted : boolean);
    procedure Set_TimeSeriesFitted(ATimeSeriesFitted : boolean);
    procedure Set_StatisticsCalculated(AStatisticsCalculated : boolean);
  public
    //General Data
    FileName, Directory : String;

    //Inc File Data
    StartYear, EndYear, RecordLength : Integer;
    AnnualRawIncData  : array1000F;                         //Annual values
    MonthlyRawIncData : Array[1..1000,1..12] of double; //Monthly values

    //Marginal Fitting Data
    ZeroCount, DefaultCurve, UserCurve, Itype, IFault : integer;
    PZero : Double;
    NaturalFlows, Standardised : array1000F;
    Gamma,Delta,Xlamda,Xx,Criterion : arrayCoefs;
    Average, StdDeviation : Array1F;
    SNVR : array1F;
    Fitted, NormalisedFlows : FittedArray;


    Area : double;
    MAR  : double;

    //Time Series data
    //Items from MDSTATS
    M1, M2: Integer;
    Mean,StdDev,Skew,Excess,
    NormalisedMean,NormalisedStdDev,
    NormalisedSkew,NormalisedExcess,
    Skew95,Excess95,ChiSquare,ExceedanceXX1,
    NormalisedChiSquare,ExceedanceXX2 : array1F;
    //Items from SERCOR
    NFRD,NLAG : arrayT1I;
    NPORT,NEXCD:array1F;
    CRIT,
    AIC,DIST :array9F;

    Rank : Array[1..9] of Integer;//still need to save this data !!!!!!

    //need to add these items to the data file
    CRP  : array6F;   //Exceedance
    QI   : array12F;  //Flow
    CRNX,             //Crossings
    CRJX : array13I; //Run Length
    CRSX : array13F;  //Run Sum
    //end of items to add to data file

    NCORR:arrayT2_30;
    PCORR:arrayT9_2_30;
    NCONF:array1F;    //Confidence interval
    PFRD,            //Degrees of freedom for models
    PLAG : arrayT9I; //Length of correlogram for models
    PCONF,           //Confidence interval for nine models
    PEXCD,           //Exceedance probability for models
    PPORT : array9F; //Portmanteau statistic for models
    Phi, Theta :arrayT2_9;
    Historical : arrayT2_9;
    DefaultTimeSeriesModel, UserTimeSeriesModel : Integer;

    //Flow generation results from FLOWGEN
    GenerateFlows : Boolean;
    GeneratedFlows : array_flows_400;
    SequenceCount, GeneratedYearCount : Integer;
    FlowsGenerated : boolean;

    //Flow statistics results from FLOWSTAT
    //Historical data
    HSKD:array1F;//Skew
    HJAKD:array1F;//At K1
    //Simulation statistics
    TSAMD, TSTMD, TSKMD : Array1F;//Average of annual streamflow
    TSASD, TSTSD, TSKSD : Array1F;//Std. dev. of annual flows
    TSCVD, TSTVD, TSKVD : Array1F;//CV of annual flows
    {Graph 6}
    GJAD : array1F;//Annual historical means
    PGD  : array7F;//Annual means
    X1D  : array12F;//Monthly historical means
    PMAD : array12_7F;//Monthly means
    {Graph 7}
    HSAD : array1F;//Std. dev.
    PSD  : array7F;//Annual std. dev.
    X2D  : array12F;//Monthly historical std. dev.
    PMSD : array12_7F;//Monthly std. dev.
    {Graph 8}
    LWD   : arrayT1I;//Number of years simulated
    LPD   : array10I;//X value (months)
    AMIND : array10F;//Historical N-month run sums
    WMIND : array7_10F;//Simulated N-month run sums
    {Graph 9}
    RELD : array5F;                   //Release % MAR
    QRHD : array5F; QRSD : array7_5F; //Release per month
    RSHD : array5F; RSSD : array7_5F; //Maximum Deficit
    RILHD:array5F; RILSD:array7_5F;   //Duration of max. deficit
    RJLHD:array5F; RJLSD:array7_5F;   //Longest depletion
    {Graph 10}
    APAD   : array101_20F; //Stochastic yield
    GPAD   : Array101_20F; //Stochastic capacity
    MNGPAD : Array20F;     //Stochastic mean
    MDXJD  : Array20F;     //Stochastic median
    HGPAD  : Array20F;     //Historical

    //Flow correlation data generated by MTHRNK
    FlowCorrelationData : TFlowCorrelation;//this should not be changed, need a current pointer also

    //Event handlers
    property OnKeyGaugeChange     : TDataChangedEvent read FOnKeyGaugeChange     write FOnKeyGaugeChange;
    property OnMarginalFitted     : TDataChangedEvent read FOnMarginalFitted     write FOnMarginalFitted;
    property OnMarginalUnfitted   : TDataChangedEvent read FOnMarginalUnfitted   write FOnMarginalUnfitted;
    property OnTimeSeriesFitted   : TDataChangedEvent read FOnTimeSeriesFitted   write FOnTimeSeriesFitted;
    property OnTimeSeriesUnFitted : TDataChangedEvent read FOnTimeSeriesUnFitted write FOnTimeSeriesUnFitted;
    property OnFlowsAndStatsGenerated: TNotifyEvent   read FOnFlowsAndStatsGenerated write FOnFlowsAndStatsGenerated;

    property KeyGauge             : Boolean read FKeyGauge             write Set_KeyGauge;
    property MarginalFitted       : boolean read FMarginalFitted       write Set_MarginalFitted;
    property TimeSeriesFitted     : boolean read FTimeSeriesFitted     write Set_TimeSeriesFitted;
    property StatisticsCalculated : boolean read FStatisticsCalculated write Set_StatisticsCalculated;
  end;

  TStomsaData = class(TAbstractModelData)
  protected
    //FIncFileCount: Integer;
    //FKeyGaugeCount: Integer;
    //FMarginalFittedCount: Integer;
    //FStatisticsCalculatedCount: Integer;
    //FTimeSeriesFittedCount: Integer;

    FIncDataList        : TObjectList;
    FCurrentIncPosition : integer;
    FErrorList          : TStringList;
    FRunReference       : string;
    FParamFileName      : string;
    FProjectFileName    : string;
    FBoxPlotDataSelection: TStringList;

    //Data change flags
    FIncFilesHaveChanged,
    FKeyGaugesHaveChanged,
    FMarginalDataHasChanged,
    FTimeSeriesDataHasChanged,
    FParamDataHasChanged,
    FStochasticDataHasChanged : Boolean;

    FCrossResults : TCrossResults;
    FSaveExtraData,
    FAutomatic,
    FPARAMFileCreated,
    FPARAMFileOpened : Boolean;

    //EVENTS
    FOnIncAdd,
    FOnIncRemove,
    FOnKeyGaugeChange,
    FOnMarginalFitted,
    FOnMarginalUnfitted,
    FOnTimeSeriesFitted,
    FOnTimeSeriesUnFitted : TDataChangedEvent;
    FOnFileLoadError : TFileLoadError;
    FOnPARAMFileCreated,
    FOnPARAMFileDestroyed,
    FOnFlowsAndStatsGenerated : TNotifyEvent;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure SetPARAMFileCreated(Status : Boolean);
    function LoadProjectData(AFileName : string) : boolean;
    function GetCastFileNamesObject: TModelFileNames;
    function Get_CurrentIncData   : TIncData;
    function Get_IncFileCount: integer;
    function Get_KeyGaugeCount: integer;
    function Get_MarginalFittedCount: integer;
    function Get_TimeSeriesFittedCount: integer;
    function Get_StatisticsCalculatedCount: integer;
    function Get_IncFileByIndex(AIndex : Integer) : TIncData;
  public
    function GetViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList; var AHandled:boolean): boolean; override;
    function CheckHydrologyFilesPaths: boolean;

    //General Methods
    function  Initialise: boolean; override;
    procedure SaveProject(FileName : string);
    function OpenProject(FileName : string) : boolean;
    function MergeProject(FileName : string) : boolean;
    function GetANSFile : TStringList;
    function GeneratePARAMFile : TStringList;
    function WriteANSFile(FileName : String) : Boolean;
    function SavePARAMFile(FileName : string) : boolean;
    function OpenPARAMFile(FileName : string) : boolean;
    function WriteStochasticFiles(FileName : string) : boolean;
    //procedure UpdateCurrentRecord(AKeyGauge, AMarginalFitted, ATimeSeriesFitted, AStatisticsCalculated: boolean);
    //INC File Methods
    function AddNode(ADirectory, AFileName : string): boolean;
    function RemoveNode(ADirectory, AFileName : string): boolean;
    function FindIncFileByName(ADirectory, AFileName : string):TIncData;
    function FindIncFileByIndex(AIndex : Integer):TIncData;
    procedure RefreshInc;
    //Navigation Methods
    function First: boolean;
    function Next: boolean;
    function Last: boolean;
    function GotoIndex(APosition : integer): boolean;

    property RunReference: String read FRunReference write FRunReference;
    property IncFileCount: Integer read Get_IncFileCount;
    property KeyGaugeCount: Integer read Get_KeyGaugeCount;
    property MarginalFittedCount: Integer read Get_MarginalFittedCount;
    property TimeSeriesFittedCount: Integer read Get_TimeSeriesFittedCount;
    property StatisticsCalculatedCount: Integer read Get_StatisticsCalculatedCount;
    property IncFileByIndex[AIndex : Integer] : TIncData read Get_IncFileByIndex;
    property CastFileNamesObject: TModelFileNames read GetCastFileNamesObject;

    //Data change flags
    property IncFilesHaveChanged: Boolean read FIncFilesHaveChanged write FIncFilesHaveChanged;
    property KeyGaugesHaveChanged: Boolean read FKeyGaugesHaveChanged write FKeyGaugesHaveChanged;
    property MarginalDataHasChanged: Boolean read FMarginalDataHasChanged write FMarginalDataHasChanged;
    property TimeSeriesDataHasChanged: Boolean read FTimeSeriesDataHasChanged write FTimeSeriesDataHasChanged;
    property ParamDataHasChanged: Boolean read FParamDataHasChanged write FParamDataHasChanged;
    property StochasticDataHasChanged: Boolean read FStochasticDataHasChanged write FStochasticDataHasChanged;
    property ErrorList : TStringList read FErrorList;

    //Properties
    property CurrentRecord: TIncData read Get_CurrentIncData;
    property CrossResults: TCrossResults Read FCrossResults write FCrossResults;
    property SaveExtraData: Boolean Read FSaveExtraData write FSaveExtraData;
    property Automatic: Boolean Read FAutomatic write FAutomatic;
    property PARAMFileCreated: boolean read FPARAMFileCreated write SetPARAMFileCreated;
    property PARAMFileOpened: boolean read FPARAMFileOpened write FPARAMFileOpened;
    property ParamFileName : string read FParamFileName;
    property ProjectFileName : string read FProjectFileName;
    property BoxPlotDataSelection : TStringList read FBoxPlotDataSelection;

    //Events
    property OnIncAdd: TDataChangedEvent Read FOnIncAdd write FOnIncAdd;
    property OnIncRemove: TDataChangedEvent read FOnIncRemove write FOnIncRemove;
    property OnKeyGaugeChange: TDataChangedEvent read FOnKeyGaugeChange write FOnKeyGaugeChange;
    property OnMarginalFitted: TDataChangedEvent read FOnMarginalFitted write FOnMarginalFitted;
    property OnMarginalUnfitted: TDataChangedEvent read FOnMarginalUnfitted write FOnMarginalUnfitted;
    property OnTimeSeriesFitted: TDataChangedEvent read FOnTimeSeriesFitted write FOnTimeSeriesFitted;
    property OnTimeSeriesUnFitted: TDataChangedEvent read FOnTimeSeriesUnFitted write FOnTimeSeriesUnFitted;
    property OnFlowsAndStatsGenerated: TNotifyEvent read FOnFlowsAndStatsGenerated write FOnFlowsAndStatsGenerated;
    property OnFileLoadError: TFileLoadError Read FOnFileLoadError write FOnFileLoadError;
    property OnPARAMFileCreated: TNotifyEvent Read FOnPARAMFileCreated write FOnPARAMFileCreated;
    property OnPARAMFileDestroyed: TNotifyEvent Read FOnPARAMFileDestroyed write FOnPARAMFileDestroyed;
  end;

  function FloatToStrAny(InFloat : Double; Length : Integer) : String;

implementation

uses
  System.UITypes,
  UUtilities,
  UMessagesForm,
  UStomsaGlobalData,
  UErrorHandlingOperations;

function FloatToStrAny(InFloat : Double; Length : Integer) : String;
const OPNAME = 'UStomsaData.FloatToStrAny';
begin
  Result := '';
  try
    Result := FloatToStrF(InFloat,ffFixed,15,Length);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// ————————— Public Methods —————————

procedure TStomsaData.CreateMemberObjects;
const OPNAME = 'TStomsaData.CreateMemberObjects';
begin
  inherited;
  try
    inherited CreateMemberObjects;
    FIncDataList        := TObjectList.Create(True);
    FCurrentIncPosition := -1;
    FBoxPlotDataSelection            := TStringList.Create;
    FBoxPlotDataSelection.Sorted     := True;
    FBoxPlotDataSelection.Duplicates := dupIgnore;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStomsaData.DestroyMemberObjects;
const OPNAME = 'TStomsaData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FIncDataList);
    FreeAndNil(FBoxPlotDataSelection);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Initialise: boolean;
const OPNAME = 'TStomsaData.Initialise';
//Initialise all major variables
begin
  Result := inherited Initialise;
  try
    FIncDataList.Clear;
    FCurrentIncPosition := -1;
    FRunReference := 'Run 1';
    FParamFileName  := '';
    FProjectFileName := '';

    //Counters
    //FIncFileCount := 0;
    //FKeyGaugeCount := 0;
    //FMarginalFittedCount := 0;
    //FTimeSeriesFittedCount := 0;
    //FStatisticsCalculatedCount := 0;

    FErrorList := TStringList.Create;
    FErrorList.Clear;
    SaveExtraData := True;
    PARAMFileCreated := false;
    PARAMFileOpened := false;

    //Data change flags
    FIncFilesHaveChanged := true;
    FKeyGaugesHaveChanged := true;
    FMarginalDataHasChanged := true;
    FTimeSeriesDataHasChanged := true;
    FStochasticDataHasChanged := true;

    Automatic := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//Initialise

function TStomsaData.LoadProjectData(AFileName : string) : boolean;
{Require some error handling on the file read to allow files to be read
 if they are 'slightly' incorrect}
{Set ErrorFlag, if this is so append a text message to a list which can be displayed
 to the user on demand}
{Also provide the capability that if there is an error in the file then user can load the
 file up in a viewer and modify the affected portion ? - Will use a margin with numbers
 tied back to the error list for cross reference purposes
 - this will not be feasible due to the large file size which will occur}
const OPNAME = 'TStomsaData.LoadProjectData';
var
  BlankRecord : StomsaFileType;
  tempIncNode : TIncData;

  fyle        : TFileStream;
  TheRecord   : StomsaFileType;
  NumRead     : integer;

  RecordLengthCount,
  NaturalFlowsLoop, FittedFlowsLoop,
  StandardisedFlowsLoop, NormalisedFlowsLoop,
  NCorrLoop, PCorrLoop1, PCorrLoop2 : Integer;

  procedure CreateTempNode;
  {This node holds all data until a checksum is received
   if this is not located then no data is loaded, need to give the user
   an error message in this regard}
  const OPNAME = 'UStomsaData.CreateTempNode';
  begin
    try
      //Create a new INC record
      tempIncNode := TIncData.Create;
      FIncDataList.Add(tempIncNode);
      tempIncNode.FileName := '';
      tempIncNode.Directory := '';
      tempIncNode.StartYear := 0;
      tempIncNode.EndYear := 0;
      tempIncNode.RecordLength := 0;
      tempIncNode.ZeroCount := 0;
      tempIncNode.DefaultCurve := 0;
      tempIncNode.UserCurve := 0;
      tempIncNode.Itype := 0;
      tempIncNode.FlowCorrelationData := TFlowCorrelation.Create;
      tempIncNode.FKeyGauge := False;
      tempIncNode.FMarginalFitted := False;
      tempIncNode.FTimeSeriesFitted := false;
      tempIncNode.SequenceCount := 0;
      tempIncNode.GeneratedYearCount := 0;
      tempIncNode.GenerateFlows := false;
      tempIncNode.FlowsGenerated := false;
      tempIncNode.FStatisticsCalculated := false;

      tempIncNode.OnKeyGaugeChange         := FOnKeyGaugeChange;
      tempIncNode.OnMarginalFitted         := FOnMarginalFitted;
      tempIncNode.OnMarginalUnfitted       := FOnMarginalUnfitted;
      tempIncNode.OnTimeSeriesFitted       := FOnTimeSeriesFitted;
      tempIncNode.OnTimeSeriesUnFitted     := FOnTimeSeriesUnFitted;
      tempIncNode.OnFlowsAndStatsGenerated := FOnFlowsAndStatsGenerated;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

  procedure HandleData;
  const OPNAME = 'UStomsaData.HandleData';

    procedure InitialiseData;
    const OPNAME = 'UStomsaData.InitialiseData';
    begin
      try
        fmMessages.DisplayMessage('Initialising Data');

        TheRecord.FileName := '';
        TheRecord.Directory := '';
        CreateTempNode;

        RecordLengthCount := 0;
        NaturalFlowsLoop := 0;
        FittedFlowsLoop := 0;
        StandardisedFlowsLoop := 0;
        NormalisedFlowsLoop := 0;

        NCorrLoop := 0;
        PCorrLoop1 := 0;
        PCorrLoop2 := 0;
      except on E: Exception do HandleError(E, OPNAME); end;
    end;

    procedure LoadIncData(FileRecordTag : Integer);
    const OPNAME = 'UStomsaData.LoadIncData';
    begin
      try
        case FileRecordTag of
          10 : begin
                 tempIncNode.StartYear := TheRecord.StartYear;
                 tempIncNode.EndYear := TheRecord.EndYear;
                 tempIncNode.RecordLength := TheRecord.RecordLength;
               end;
          11 : begin
                 inc(RecordLengthCount);
                 if (RecordLengthCount <= 1000) and (RecordLengthCount <= tempIncNode.RecordLength) then
                 begin
                   tempIncNode.AnnualRawIncData[RecordLengthCount] := TheRecord.AnnualRawIncData;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,1] := TheRecord.Month1;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,2] := TheRecord.Month2;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,3] := TheRecord.Month3;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,4] := TheRecord.Month4;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,5] := TheRecord.Month5;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,6] := TheRecord.Month6;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,7] := TheRecord.Month7;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,8] := TheRecord.Month8;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,9] := TheRecord.Month9;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,10] := TheRecord.Month10;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,11] := TheRecord.Month11;
                   tempIncNode.MonthlyRawIncData[RecordLengthCount,12] := TheRecord.Month12;
                 end
                 else
                   FErrorList.Add('Too Much Raw Data');
               end;
        end;//case FileRecordTag
      except on E: Exception do HandleError(E, OPNAME); end;
    end;//PROCEDURE LoadIncData

    procedure LoadMarginalData(FileRecordTag : Integer);
    const OPNAME = 'UStomsaData.LoadMarginalData';
    begin
      try
        //have a new case statement to handle all marginal stuff
        case FileRecordTag of
          100 : begin
                  fmMessages.DisplayMessage('Loading Marginal Data for '+tempIncNode.FileName);

                  tempIncNode.DefaultCurve := TheRecord.DefaultCurve;
                  tempIncNode.UserCurve := TheRecord.UserCurve;
                  tempIncNode.ZeroCount := TheRecord.ZeroCount;
                  tempIncNode.Itype := TheRecord.IType;
                  tempIncNode.IFault := TheRecord.IFault;
                end;
          101 : begin
                  tempIncNode.FKeyGauge := TheRecord.KeyGauge;
                end;
          102 : begin
                  tempIncNode.SNVR[1] := TheRecord.SNVR;
                end;
          103 : begin
                  tempIncNode.Gamma[1] := TheRecord.Gamma1;
                  tempIncNode.Gamma[2] := TheRecord.Gamma2;
                  tempIncNode.Gamma[3] := TheRecord.Gamma3;
                  tempIncNode.Gamma[4] := TheRecord.Gamma4;
                end;
          104 : begin
                  tempIncNode.Delta[1] := TheRecord.Delta1;
                  tempIncNode.Delta[2] := TheRecord.Delta2;
                  tempIncNode.Delta[3] := TheRecord.Delta3;
                  tempIncNode.Delta[4] := TheRecord.Delta4;
                end;
          105 : begin
                  tempIncNode.Xlamda[1] := TheRecord.XLamda1;
                  tempIncNode.Xlamda[2] := TheRecord.XLamda2;
                  tempIncNode.Xlamda[3] := TheRecord.XLamda3;
                  tempIncNode.Xlamda[4] := TheRecord.XLamda4;
                end;
          106 : begin
                  tempIncNode.Xx[1] := TheRecord.XX1;
                  tempIncNode.Xx[2] := TheRecord.XX2;
                  tempIncNode.Xx[3] := TheRecord.XX3;
                  tempIncNode.Xx[4] := TheRecord.XX4;
                end;
          107 : begin
                  tempIncNode.Criterion[1] := TheRecord.Criterion1;
                  tempIncNode.Criterion[2] := TheRecord.Criterion2;
                  tempIncNode.Criterion[3] := TheRecord.Criterion3;
                  tempIncNode.Criterion[4] := TheRecord.Criterion4;
                end;
          108 : begin
                  inc(NaturalFlowsLoop);
                  if (NaturalFlowsLoop > tempIncNode.RecordLength) or (NaturalFlowsLoop > 1000) then
                  begin
                    FErrorList.Add('Too much natural flow data');
                  end
                  else
                    tempIncNode.NaturalFlows[NaturalFlowsLoop] := TheRecord.NaturalFlows;
                end;
          109 : begin
                  inc(FittedFlowsLoop);
                  if (FittedFlowsLoop > (tempIncNode.RecordLength - tempIncNode.ZeroCount)) or (FittedFlowsLoop > 1000) then
                  begin
                    FErrorList.Add('Too much fitted flows data');
                  end
                  else
                  begin
                    tempIncNode.Fitted[0,FittedFlowsLoop-1] := TheRecord.Fitted1;
                    tempIncNode.Fitted[1,FittedFlowsLoop-1] := TheRecord.Fitted2;
                    tempIncNode.Fitted[2,FittedFlowsLoop-1] := TheRecord.Fitted3;
                    tempIncNode.Fitted[3,FittedFlowsLoop-1] := TheRecord.Fitted4;
                  end;
                end;
          110 : begin
                  inc(StandardisedFlowsLoop);
                  if (StandardisedFlowsLoop > (tempIncNode.RecordLength-tempIncNode.ZeroCount)) or (StandardisedFlowsLoop > 1000) then
                  begin
                    FErrorList.Add('Too much standardised flows data');
                  end
                  else
                    tempIncNode.Standardised[StandardisedFlowsLoop] := TheRecord.Standardised;
                end;
          111 : begin
                  inc(NormalisedFlowsLoop);
                  if (NormalisedFlowsLoop > (tempIncNode.RecordLength - tempIncNode.ZeroCount)) or (NormalisedFlowsLoop > 1000) then
                  begin
                    FErrorList.Add('Too much normalised flows data');
                  end
                  else
                  begin
                    tempIncNode.NormalisedFlows[0,NormalisedFlowsLoop-1] := TheRecord.Normalised1;
                    tempIncNode.NormalisedFlows[1,NormalisedFlowsLoop-1] := TheRecord.Normalised2;
                    tempIncNode.NormalisedFlows[2,NormalisedFlowsLoop-1] := TheRecord.Normalised3;
                    tempIncNode.NormalisedFlows[3,NormalisedFlowsLoop-1] := TheRecord.Normalised4;
                  end;
                end;
        end;//case
        tempIncNode.FMarginalFitted := true;
      except on E: Exception do HandleError(E, OPNAME); end;
    end;//procedure LoadMarginalData

    procedure LoadTimeSeriesData(FileRecordTag : Integer);
    const OPNAME = 'UStomsaData.LoadTimeSeriesData';
    begin
      try
        case FileRecordTag of
          200 : begin
                  fmMessages.DisplayMessage('Loading Time Series Data for '+tempIncNode.FileName);

                  tempIncNode.M1 := TheRecord.M1;
                  tempIncNode.M2 := TheRecord.M2;
                end;
          201 : begin
                  tempIncNode.Mean[1] := TheRecord.Mean;
                  tempIncNode.StdDev[1] := TheRecord.StdDev;
                  tempIncNode.Skew[1] := TheRecord.Skew;
                  tempIncNode.Excess[1] := TheRecord.Excess;
                  tempIncNode.ChiSquare[1] := TheRecord.ChiSquare;
                end;
          202 : begin
                  tempIncNode.NormalisedMean[1] := TheRecord.NormalMean;
                  tempIncNode.NormalisedStdDev[1] := TheRecord.NormalStdDev;
                  tempIncNode.NormalisedSkew[1] := TheRecord.NormalSkew;
                  tempIncNode.NormalisedExcess[1] := TheRecord.NormalExcess;
                  tempIncNode.NormalisedChiSquare[1] := TheRecord.NormalChiSquare;
                end;
          203 : begin
                  tempIncNode.Skew95[1] := TheRecord.Skew95;
                  tempIncNode.Excess95[1] := TheRecord.Excess95;
                  tempIncNode.ExceedanceXX1[1] := TheRecord.Exceedance1;
                  tempIncNode.ExceedanceXX2[1] := TheRecord.Exceedance2;
                end;
          210 : begin
                  tempIncNode.NFRD[1] := TheRecord.NFRD;
                  tempIncNode.NLAG[1] := TheRecord.NLAG;
                end;
          211 : begin
                  tempIncNode.NPORT[1] := TheRecord.NPORT;
                  tempIncNode.NEXCD[1] := TheRecord.NEXCD;
                end;
          212 : begin
                  tempIncNode.Crit[1] := TheRecord.Crit1;
                  tempIncNode.Crit[2] := TheRecord.Crit2;
                  tempIncNode.Crit[3] := TheRecord.Crit3;
                  tempIncNode.Crit[4] := TheRecord.Crit4;
                  tempIncNode.Crit[5] := TheRecord.Crit5;
                  tempIncNode.Crit[6] := TheRecord.Crit6;
                  tempIncNode.Crit[7] := TheRecord.Crit7;
                  tempIncNode.Crit[8] := TheRecord.Crit8;
                  tempIncNode.Crit[9] := TheRecord.Crit9;
                end;
          213 : begin
                  tempIncNode.AIC[1] := TheRecord.AIC1;
                  tempIncNode.AIC[2] := TheRecord.AIC2;
                  tempIncNode.AIC[3] := TheRecord.AIC3;
                  tempIncNode.AIC[4] := TheRecord.AIC4;
                  tempIncNode.AIC[5] := TheRecord.AIC5;
                  tempIncNode.AIC[6] := TheRecord.AIC6;
                  tempIncNode.AIC[7] := TheRecord.AIC7;
                  tempIncNode.AIC[8] := TheRecord.AIC8;
                  tempIncNode.AIC[9] := TheRecord.AIC9;
                end;
          214 : begin
                  tempIncNode.Dist[1] := TheRecord.Dist1;
                  tempIncNode.Dist[2] := TheRecord.Dist2;
                  tempIncNode.Dist[3] := TheRecord.Dist3;
                  tempIncNode.Dist[4] := TheRecord.Dist4;
                  tempIncNode.Dist[5] := TheRecord.Dist5;
                  tempIncNode.Dist[6] := TheRecord.Dist6;
                  tempIncNode.Dist[7] := TheRecord.Dist7;
                  tempIncNode.Dist[8] := TheRecord.Dist8;
                  tempIncNode.Dist[9] := TheRecord.Dist9;
                end;
          215 : begin
                  inc(NCorrLoop);
                  if NCorrLoop <= tempIncNode.NLAG[1] then
                  begin
                    tempIncNode.NCORR[1,NCorrLoop] := TheRecord.NCorr1;
                    tempIncNode.NCORR[2,NCorrLoop] := TheRecord.NCorr2;
                  end
                  else
                    FErrorList.Add('Too much NCorr Data');
                end;
          216 : begin
                  inc(PCorrLoop1);
                  if PCorrLoop1 <= tempIncNode.NLAG[1] then
                  begin
                    tempIncNode.PCORR[1,1,PCorrLoop1] := TheRecord.PCorr11;
                    tempIncNode.PCORR[2,1,PCorrLoop1] := TheRecord.PCorr12;
                    tempIncNode.PCORR[3,1,PCorrLoop1] := TheRecord.PCorr13;
                    tempIncNode.PCORR[4,1,PCorrLoop1] := TheRecord.PCorr14;
                    tempIncNode.PCORR[5,1,PCorrLoop1] := TheRecord.PCorr15;
                    tempIncNode.PCORR[6,1,PCorrLoop1] := TheRecord.PCorr16;
                    tempIncNode.PCORR[7,1,PCorrLoop1] := TheRecord.PCorr17;
                    tempIncNode.PCORR[8,1,PCorrLoop1] := TheRecord.PCorr18;
                    tempIncNode.PCORR[9,1,PCorrLoop1] := TheRecord.PCorr19;
                  end
                  else
                    FErrorList.Add('Too much PCorr1 Data');
                end;
          217 : begin
                  inc(PCorrLoop2);
                  if PCorrLoop2 <= tempIncNode.NLAG[1] then
                  begin
                    tempIncNode.PCORR[1,2,PCorrLoop2] := TheRecord.PCorr21;
                    tempIncNode.PCORR[2,2,PCorrLoop2] := TheRecord.PCorr22;
                    tempIncNode.PCORR[3,2,PCorrLoop2] := TheRecord.PCorr23;
                    tempIncNode.PCORR[4,2,PCorrLoop2] := TheRecord.PCorr24;
                    tempIncNode.PCORR[5,2,PCorrLoop2] := TheRecord.PCorr25;
                    tempIncNode.PCORR[6,2,PCorrLoop2] := TheRecord.PCorr26;
                    tempIncNode.PCORR[7,2,PCorrLoop2] := TheRecord.PCorr27;
                    tempIncNode.PCORR[8,2,PCorrLoop2] := TheRecord.PCorr28;
                    tempIncNode.PCORR[9,2,PCorrLoop2] := TheRecord.PCorr29;
                  end
                  else
                    FErrorList.Add('Too much PCorr2 Data');
                end;
          218: begin
                  tempIncNode.Phi[1,1] := TheRecord.Phi11;
                  tempIncNode.Phi[1,2] := TheRecord.Phi12;
                  tempIncNode.Phi[1,3] := TheRecord.Phi13;
                  tempIncNode.Phi[1,4] := TheRecord.Phi14;
                  tempIncNode.Phi[1,5] := TheRecord.Phi15;
                  tempIncNode.Phi[1,6] := TheRecord.Phi16;
                  tempIncNode.Phi[1,7] := TheRecord.Phi17;
                  tempIncNode.Phi[1,8] := TheRecord.Phi18;
                  tempIncNode.Phi[1,9] := TheRecord.Phi19;
                end;
          219: begin
                  tempIncNode.Phi[2,1] := TheRecord.Phi21;
                  tempIncNode.Phi[2,2] := TheRecord.Phi22;
                  tempIncNode.Phi[2,3] := TheRecord.Phi23;
                  tempIncNode.Phi[2,4] := TheRecord.Phi24;
                  tempIncNode.Phi[2,5] := TheRecord.Phi25;
                  tempIncNode.Phi[2,6] := TheRecord.Phi26;
                  tempIncNode.Phi[2,7] := TheRecord.Phi27;
                  tempIncNode.Phi[2,8] := TheRecord.Phi28;
                  tempIncNode.Phi[2,9] := TheRecord.Phi29;
                end;
          220: begin
                  tempIncNode.Theta[1,1] := TheRecord.Theta11;
                  tempIncNode.Theta[1,2] := TheRecord.Theta12;
                  tempIncNode.Theta[1,3] := TheRecord.Theta13;
                  tempIncNode.Theta[1,4] := TheRecord.Theta14;
                  tempIncNode.Theta[1,5] := TheRecord.Theta15;
                  tempIncNode.Theta[1,6] := TheRecord.Theta16;
                  tempIncNode.Theta[1,7] := TheRecord.Theta17;
                  tempIncNode.Theta[1,8] := TheRecord.Theta18;
                  tempIncNode.Theta[1,9] := TheRecord.Theta19;
                end;
          221: begin
                  tempIncNode.Theta[2,1] := TheRecord.Theta21;
                  tempIncNode.Theta[2,2] := TheRecord.Theta22;
                  tempIncNode.Theta[2,3] := TheRecord.Theta23;
                  tempIncNode.Theta[2,4] := TheRecord.Theta24;
                  tempIncNode.Theta[2,5] := TheRecord.Theta25;
                  tempIncNode.Theta[2,6] := TheRecord.Theta26;
                  tempIncNode.Theta[2,7] := TheRecord.Theta27;
                  tempIncNode.Theta[2,8] := TheRecord.Theta28;
                  tempIncNode.Theta[2,9] := TheRecord.Theta29;
                end;
          222: begin
                  tempIncNode.Historical[1,1] := TheRecord.Hist11;
                  tempIncNode.Historical[1,2] := TheRecord.Hist12;
                  tempIncNode.Historical[1,3] := TheRecord.Hist13;
                  tempIncNode.Historical[1,4] := TheRecord.Hist14;
                  tempIncNode.Historical[1,5] := TheRecord.Hist15;
                  tempIncNode.Historical[1,6] := TheRecord.Hist16;
                  tempIncNode.Historical[1,7] := TheRecord.Hist17;
                  tempIncNode.Historical[1,8] := TheRecord.Hist18;
                  tempIncNode.Historical[1,9] := TheRecord.Hist19;
                end;
          223: begin
                  tempIncNode.Historical[2,1] := TheRecord.Hist21;
                  tempIncNode.Historical[2,2] := TheRecord.Hist22;
                  tempIncNode.Historical[2,3] := TheRecord.Hist23;
                  tempIncNode.Historical[2,4] := TheRecord.Hist24;
                  tempIncNode.Historical[2,5] := TheRecord.Hist25;
                  tempIncNode.Historical[2,6] := TheRecord.Hist26;
                  tempIncNode.Historical[2,7] := TheRecord.Hist27;
                  tempIncNode.Historical[2,8] := TheRecord.Hist28;
                  tempIncNode.Historical[2,9] := TheRecord.Hist29;
                end;
          224 : begin
                  tempIncNode.NCONF[1] := TheRecord.NConf;
                end;
          225 : begin
                  tempIncNode.PFRD[1] := TheRecord.PFRD1;
                  tempIncNode.PFRD[2] := TheRecord.PFRD2;
                  tempIncNode.PFRD[3] := TheRecord.PFRD3;
                  tempIncNode.PFRD[4] := TheRecord.PFRD4;
                  tempIncNode.PFRD[5] := TheRecord.PFRD5;
                  tempIncNode.PFRD[6] := TheRecord.PFRD6;
                  tempIncNode.PFRD[7] := TheRecord.PFRD7;
                  tempIncNode.PFRD[8] := TheRecord.PFRD8;
                  tempIncNode.PFRD[9] := TheRecord.PFRD9;
                end;
          226 : begin
                  tempIncNode.PLAG[1] := TheRecord.PLAG1;
                  tempIncNode.PLAG[2] := TheRecord.PLAG2;
                  tempIncNode.PLAG[3] := TheRecord.PLAG3;
                  tempIncNode.PLAG[4] := TheRecord.PLAG4;
                  tempIncNode.PLAG[5] := TheRecord.PLAG5;
                  tempIncNode.PLAG[6] := TheRecord.PLAG6;
                  tempIncNode.PLAG[7] := TheRecord.PLAG7;
                  tempIncNode.PLAG[8] := TheRecord.PLAG8;
                  tempIncNode.PLAG[9] := TheRecord.PLAG9;
                end;
          227 : begin
                  tempIncNode.PCONF[1] := TheRecord.PCONF1;
                  tempIncNode.PCONF[2] := TheRecord.PCONF2;
                  tempIncNode.PCONF[3] := TheRecord.PCONF3;
                  tempIncNode.PCONF[4] := TheRecord.PCONF4;
                  tempIncNode.PCONF[5] := TheRecord.PCONF5;
                  tempIncNode.PCONF[6] := TheRecord.PCONF6;
                  tempIncNode.PCONF[7] := TheRecord.PCONF7;
                  tempIncNode.PCONF[8] := TheRecord.PCONF8;
                  tempIncNode.PCONF[9] := TheRecord.PCONF9;
                end;
          228 : begin
                  tempIncNode.PEXCD[1] := TheRecord.PEXCD1;
                  tempIncNode.PEXCD[2] := TheRecord.PEXCD2;
                  tempIncNode.PEXCD[3] := TheRecord.PEXCD3;
                  tempIncNode.PEXCD[4] := TheRecord.PEXCD4;
                  tempIncNode.PEXCD[5] := TheRecord.PEXCD5;
                  tempIncNode.PEXCD[6] := TheRecord.PEXCD6;
                  tempIncNode.PEXCD[7] := TheRecord.PEXCD7;
                  tempIncNode.PEXCD[8] := TheRecord.PEXCD8;
                  tempIncNode.PEXCD[9] := TheRecord.PEXCD9;
                end;
          229 : begin
                  tempIncNode.PPORT[1] := TheRecord.PPORT1;
                  tempIncNode.PPORT[2] := TheRecord.PPORT2;
                  tempIncNode.PPORT[3] := TheRecord.PPORT3;
                  tempIncNode.PPORT[4] := TheRecord.PPORT4;
                  tempIncNode.PPORT[5] := TheRecord.PPORT5;
                  tempIncNode.PPORT[6] := TheRecord.PPORT6;
                  tempIncNode.PPORT[7] := TheRecord.PPORT7;
                  tempIncNode.PPORT[8] := TheRecord.PPORT8;
                  tempIncNode.PPORT[9] := TheRecord.PPORT9;
                end;
          230 : begin
                  tempIncNode.CRP[1] := TheRecord.CRP1;
                  tempIncNode.CRP[2] := TheRecord.CRP2;
                  tempIncNode.CRP[3] := TheRecord.CRP3;
                  tempIncNode.CRP[4] := TheRecord.CRP4;
                  tempIncNode.CRP[5] := TheRecord.CRP5;
                  tempIncNode.CRP[6] := TheRecord.CRP6;
                end;
          231 : begin
                  tempIncNode.QI[1] := TheRecord.QI1;
                  tempIncNode.QI[2] := TheRecord.QI2;
                  tempIncNode.QI[3] := TheRecord.QI3;
                  tempIncNode.QI[4] := TheRecord.QI4;
                  tempIncNode.QI[5] := TheRecord.QI5;
                  tempIncNode.QI[6] := TheRecord.QI6;
                  tempIncNode.QI[7] := TheRecord.QI7;
                  tempIncNode.QI[8] := TheRecord.QI8;
                  tempIncNode.QI[9] := TheRecord.QI9;
                  tempIncNode.QI[10] := TheRecord.QI10;
                  tempIncNode.QI[11] := TheRecord.QI11;
                  tempIncNode.QI[12] := TheRecord.QI12;
                end;
          232 : begin
                  tempIncNode.CRNX[1] := TheRecord.CRNX1;
                  tempIncNode.CRNX[2] := TheRecord.CRNX2;
                  tempIncNode.CRNX[3] := TheRecord.CRNX3;
                  tempIncNode.CRNX[4] := TheRecord.CRNX4;
                  tempIncNode.CRNX[5] := TheRecord.CRNX5;
                  tempIncNode.CRNX[6] := TheRecord.CRNX6;
                  tempIncNode.CRNX[7] := TheRecord.CRNX7;
                  tempIncNode.CRNX[8] := TheRecord.CRNX8;
                  tempIncNode.CRNX[9] := TheRecord.CRNX9;
                  tempIncNode.CRNX[10] := TheRecord.CRNX10;
                  tempIncNode.CRNX[11] := TheRecord.CRNX11;
                  tempIncNode.CRNX[12] := TheRecord.CRNX12;
                end;
          233 : begin
                  tempIncNode.CRJX[1] := TheRecord.CRJX1;
                  tempIncNode.CRJX[2] := TheRecord.CRJX2;
                  tempIncNode.CRJX[3] := TheRecord.CRJX3;
                  tempIncNode.CRJX[4] := TheRecord.CRJX4;
                  tempIncNode.CRJX[5] := TheRecord.CRJX5;
                  tempIncNode.CRJX[6] := TheRecord.CRJX6;
                  tempIncNode.CRJX[7] := TheRecord.CRJX7;
                  tempIncNode.CRJX[8] := TheRecord.CRJX8;
                  tempIncNode.CRJX[9] := TheRecord.CRJX9;
                  tempIncNode.CRJX[10] := TheRecord.CRJX10;
                  tempIncNode.CRJX[11] := TheRecord.CRJX11;
                  tempIncNode.CRJX[12] := TheRecord.CRJX12;
                end;
          234 : begin
                  tempIncNode.CRSX[1] := TheRecord.CRSX1;
                  tempIncNode.CRSX[2] := TheRecord.CRSX2;
                  tempIncNode.CRSX[3] := TheRecord.CRSX3;
                  tempIncNode.CRSX[4] := TheRecord.CRSX4;
                  tempIncNode.CRSX[5] := TheRecord.CRSX5;
                  tempIncNode.CRSX[6] := TheRecord.CRSX6;
                  tempIncNode.CRSX[7] := TheRecord.CRSX7;
                  tempIncNode.CRSX[8] := TheRecord.CRSX8;
                  tempIncNode.CRSX[9] := TheRecord.CRSX9;
                  tempIncNode.CRSX[10] := TheRecord.CRSX10;
                  tempIncNode.CRSX[11] := TheRecord.CRSX11;
                  tempIncNode.CRSX[12] := TheRecord.CRSX12;
                end;
          235: begin
                 tempIncNode.Rank[1] := TheRecord.Rank1;
                 tempIncNode.Rank[2] := TheRecord.Rank2;
                 tempIncNode.Rank[3] := TheRecord.Rank3;
                 tempIncNode.Rank[4] := TheRecord.Rank4;
                 tempIncNode.Rank[5] := TheRecord.Rank5;
                 tempIncNode.Rank[6] := TheRecord.Rank6;
                 tempIncNode.Rank[7] := TheRecord.Rank7;
                 tempIncNode.Rank[8] := TheRecord.Rank8;
                 tempIncNode.Rank[9] := TheRecord.Rank9;
               end;
          236: begin
                 tempIncNode.DefaultTimeSeriesModel := TheRecord.DefaultTimeSeries;
                 tempIncNode.UserTimeSeriesModel := TheRecord.UserTimeSeries;
                end;
        end;//Case
        tempIncNode.FTimeSeriesFitted := true;
      except on E: Exception do HandleError(E, OPNAME); end;
    end;//PROCEDURE LoadTimeSeriesData

    procedure LoadFlowData(FileRecordTag : Integer);
    const OPNAME = 'UStomsaData.LoadFlowData';
    begin
      try
        case FileRecordTag of
          300 : begin
                  fmMessages.DisplayMessage('Loading Flow Data for '+tempIncNode.FileName);

                  tempIncNode.SequenceCount := TheRecord.SequenceCount;
                  tempIncNode.GeneratedYearCount := TheRecord.GeneratedYearCount;
                end;//300
          301 : begin
                  if TheRecord.SequenceNumber > tempIncNode.SequenceCount then
                    FErrorList.Add('Sequence Number Undefined')
                  else if TheRecord.GeneratedYearNumber > tempIncNode.GeneratedYearCount then
                    FErrorList.Add('Year Number Undefined')
                  else with TheRecord do begin
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,1] := FlowData1;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,2] := FlowData2;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,3] := FlowData3;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,4] := FlowData4;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,5] := FlowData5;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,6] := FlowData6;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,7] := FlowData7;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,8] := FlowData8;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,9] := FlowData9;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,10] := FlowData10;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,11] := FlowData11;
                    tempIncNode.GeneratedFlows[SequenceNumber,GeneratedYearNumber,12] := FlowData12;
                  end;
                end;//301
        end;//case FileRecordTag
        PARAMFileCreated := true;
        tempIncNode.FlowsGenerated := true;
        SaveExtraData := true;
      except on E: Exception do HandleError(E, OPNAME); end;
    end;//PROCEDURE LoadFlowData

  begin
    try
      with tempIncNode do
      begin
        case TheRecord.RecordNumber of
          1        : InitialiseData;
          2        : FileName := Filename + string(TheRecord.FileName);
          3        : Directory := Directory + string(TheRecord.Directory);
          10..99   : LoadIncData(TheRecord.RecordNumber);
          100..199 : LoadMarginalData(TheRecord.RecordNumber);
          200..299 : LoadTimeSeriesData(TheRecord.RecordNumber);
          300..399 : LoadFlowData(TheRecord.RecordNumber);
          999 : begin

                  FCurrentIncPosition := FIncDataList.IndexOf(tempIncNode);

                  FIncFilesHaveChanged := true;
                  //Inc(FIncFileCount);
                  if Assigned(FOnIncAdd) then FOnIncAdd(Self,FileName,Directory);

                  if CurrentRecord.FKeyGauge then
                  begin
                    //Inc(FKeyGaugeCount);
                    if Assigned(FOnKeyGaugeChange) then
                      FOnKeyGaugeChange(Self,CurrentRecord.FileName,CurrentRecord.Directory);
                  end;
                  if CurrentRecord.FMarginalFitted = true then
                  begin
                    //Inc(FMarginalFittedCount);
                    if Assigned(FOnMarginalFitted) then
                      FOnMarginalFitted(Self,CurrentRecord.FileName,CurrentRecord.Directory);
                  end;
                  if CurrentRecord.FTimeSeriesFitted = true then
                  begin
                    //Inc(FTimeSeriesFittedCount);
                    if Assigned(FOnTimeSeriesFitted) then
                      FOnTimeSeriesFitted(Self,CurrentRecord.FileName,CurrentRecord.Directory);
                  end;
                  //Don't check for the stats or correlated flags as we never save that data
                  FCurrentIncPosition := FIncDataList.IndexOf(tempIncNode);
                end;//999
        end;//case TheRecord
      end;//with CurrentRecord
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

begin
  Result := true;
  try
    //ensure that the object is free of existing data
    try
      BlankRecord.RecordTag := '';
      BlankRecord.RecordNumber := 0;

      fyle := TFileStream.Create(AFileName,(fmOpenRead or fmShareDenyWrite));
      TheRecord := BlankRecord;
      NumRead := fyle.Read(TheRecord,SizeOf(TheRecord));
      while (NumRead <> 0) and (NumRead >= SizeOf(TheRecord)) do
      begin
        HandleData;
        TheRecord := BlankRecord;
        NumRead := fyle.Read(TheRecord,SizeOf(TheRecord));
      end;
      fyle.Destroy;

      fmMessages.Hide;

      if FErrorList.Count > 0 then
        if Assigned(OnFileLoadError) then
          OnFileLoadError(Self,FErrorList);

    except
      on E: EInOutError do
      begin
        result := false;
        MessageDlg('Unable to open the file '+AFileName,mtError,[mbOk],0);
        Raise;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE LoadProjectData

procedure TStomsaData.SaveProject(FileName : string);
const OPNAME = 'TStomsaData.SaveProject';
var
  fyle : file of StomsaFileType;
  TheRecord, BlankRecord : StomsaFileType;

  procedure SaveGeneralData;
  const OPNAME = 'UStomsaData.SaveGeneralData';
  //Make provision for strings longer than 72 bytes
  var
    StringLength, LoopCount : Integer;
  begin
    try
      with CurrentRecord do
      begin
        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'VERSION';
        TheRecord.RecordNumber := 1;
        TheRecord.Version := 1;
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'FILENAME';
        TheRecord.RecordNumber := 2;
        TheRecord.FileName := AnsiString(FileName);
        write(fyle,TheRecord);

        StringLength := length(FileName) - 72;
        LoopCount := 1;
        while StringLength > 0 do
        begin
          TheRecord := BlankRecord;
          TheRecord.RecordTag := 'FILENAME';
          TheRecord.RecordNumber := 2;
          TheRecord.FileName := AnsiString(Copy(FileName,(LoopCount*72)+1,72));
          write(fyle,TheRecord);
          inc(LoopCount);
          StringLength := length(FileName) - (72 * LoopCount);
        end;//While StringLength

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'DIRECTORY';
        TheRecord.RecordNumber := 3;
        TheRecord.Directory := AnsiString(Directory);
        write(fyle,TheRecord);

        StringLength := length(Directory) - 72;
        LoopCount := 1;
        while StringLength > 0 do
        begin
          TheRecord := BlankRecord;
          TheRecord.RecordTag := 'DIRECTORY';
          TheRecord.RecordNumber := 3;
          TheRecord.Directory := AnsiString(Copy(Directory,(LoopCount*72)+1,72));
          write(fyle,TheRecord);
          inc(LoopCount);
          StringLength := length(Directory) - (72 * LoopCount);
        end;//While StringLength

        //Inc File Data
        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'STARTYEAR';
        TheRecord.RecordNumber := 10;
        TheRecord.StartYear := StartYear;
        TheRecord.EndYear := EndYear;
        TheRecord.RecordLength := RecordLength;
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'RAWINCDATA';
        TheRecord.RecordNumber := 11;
        for LoopCount := 1 to RecordLength do
        begin
          TheRecord.AnnualRawIncData := AnnualRawIncData[LoopCount];
          TheRecord.Month1           := MonthlyRawIncData[LoopCount,1];
          TheRecord.Month2           := MonthlyRawIncData[LoopCount,2];
          TheRecord.Month3           := MonthlyRawIncData[LoopCount,3];
          TheRecord.Month4           := MonthlyRawIncData[LoopCount,4];
          TheRecord.Month5           := MonthlyRawIncData[LoopCount,5];
          TheRecord.Month6           := MonthlyRawIncData[LoopCount,6];
          TheRecord.Month7           := MonthlyRawIncData[LoopCount,7];
          TheRecord.Month8           := MonthlyRawIncData[LoopCount,8];
          TheRecord.Month9           := MonthlyRawIncData[LoopCount,9];
          TheRecord.Month10          := MonthlyRawIncData[LoopCount,10];
          TheRecord.Month11          := MonthlyRawIncData[LoopCount,11];
          TheRecord.Month12          := MonthlyRawIncData[LoopCount,12];
          write(fyle,TheRecord);
        end;

      end;//with CurrentRecord
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//PROCEDURE SaveGeneralData

  procedure SaveMarginalData;
  const OPNAME = 'UStomsaData.SaveMarginalData';
  var ItemLoop : Integer;
  begin
    try
      with CurrentRecord do
      begin
        fmMessages.DisplayMessage('Saving Marginal Data for '+CurrentRecord.FileName);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'MARGINAL';
        TheRecord.RecordNumber := 100;
        TheRecord.DefaultCurve := DefaultCurve;
        TheRecord.UserCurve := UserCurve;
        TheRecord.ZeroCount := ZeroCount;
        TheRecord.IType := Itype;
        TheRecord.IFault := IFault;
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'KEYGAUGE';
        TheRecord.RecordNumber := 101;
        TheRecord.KeyGauge := FKeyGauge;
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'SNVR';
        TheRecord.RecordNumber := 102;
        TheRecord.SNVR := SNVR[1];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'GAMMA';
        TheRecord.RecordNumber := 103;
        TheRecord.Gamma1 := Gamma[1]; TheRecord.Gamma2 := Gamma[2];
        TheRecord.Gamma3 := Gamma[3]; TheRecord.Gamma4 := Gamma[4];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'DELTA';
        TheRecord.RecordNumber := 104;
        TheRecord.Delta1 := Delta[1]; TheRecord.Delta2 := Delta[2];
        TheRecord.Delta3 := Delta[3]; TheRecord.Delta4 := Delta[4];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'XLAMDA';
        TheRecord.RecordNumber := 105;
        TheRecord.XLamda1 := XLamda[1]; TheRecord.XLamda2 := XLamda[2];
        TheRecord.XLamda3 := XLamda[3]; TheRecord.XLamda4 := XLamda[4];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'XX';
        TheRecord.RecordNumber := 106;
        TheRecord.XX1 := XX[1]; TheRecord.XX2 := XX[2];
        TheRecord.XX3 := XX[3]; TheRecord.XX4 := XX[4];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Criterion';
        TheRecord.RecordNumber := 107;
        TheRecord.Criterion1 := Criterion[1]; TheRecord.Criterion2 := Criterion[2];
        TheRecord.Criterion3 := Criterion[3]; TheRecord.Criterion4 := Criterion[4];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'NaturalFlows';
        TheRecord.RecordNumber := 108;
        for ItemLoop := 1 to RecordLength do
        begin
          TheRecord.NaturalFlows := NaturalFlows[ItemLoop];
          write(fyle,TheRecord);
        end;

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Fitted';
        TheRecord.RecordNumber := 109;
        for ItemLoop := 0 to (RecordLength - ZeroCount - 1) do
        begin
          TheRecord.Fitted1 := Fitted[0,ItemLoop]; TheRecord.Fitted2 := Fitted[1,ItemLoop];
          TheRecord.Fitted3 := Fitted[2,ItemLoop]; TheRecord.Fitted4 := Fitted[3,ItemLoop];
          write(fyle,TheRecord);
        end;

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Standardised';
        TheRecord.RecordNumber := 110;
        for ItemLoop := 1 to (RecordLength - ZeroCount) do
        begin
          TheRecord.Standardised := Standardised[ItemLoop];
          write(fyle,TheRecord);
        end;

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Normalised';
        TheRecord.RecordNumber := 111;
        for ItemLoop := 0 to (RecordLength - ZeroCount - 1) do
        begin
          TheRecord.Normalised1 := NormalisedFlows[0,ItemLoop];
          TheRecord.Normalised2 := NormalisedFlows[1,ItemLoop];
          TheRecord.Normalised3 := NormalisedFlows[2,ItemLoop];
          TheRecord.Normalised4 := NormalisedFlows[3,ItemLoop];
          write(fyle,TheRecord);
        end;
      end;//with CurrentRecord
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//PROCEDURE SaveMarginalData

  procedure SaveTimeSeriesData;
  const OPNAME = 'UStomsaData.SaveTimeSeriesData';
  var ItemLoop : Integer;
  begin
    try
      with CurrentRecord do
      begin
        fmMessages.DisplayMessage('Saving Time Series Data for '+CurrentRecord.FileName);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'MValues';
        TheRecord.RecordNumber := 200;
        TheRecord.M1 := M1; TheRecord.M2 := M2;
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Stats1';
        TheRecord.RecordNumber := 201;
        TheRecord.Mean := Mean[1]; TheRecord.StdDev := StdDev[1];
        TheRecord.Skew := Skew[1]; TheRecord.Excess := Excess[1];
        TheRecord.ChiSquare := ChiSquare[1];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Stats2';
        TheRecord.RecordNumber := 202;
        TheRecord.NormalMean := NormalisedMean[1]; TheRecord.NormalStdDev := NormalisedStdDev[1];
        TheRecord.NormalSkew := NormalisedSkew[1]; TheRecord.NormalExcess := NormalisedExcess[1];
        TheRecord.NormalChiSquare := NormalisedChiSquare[1];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Stats3';
        TheRecord.RecordNumber := 203;
        TheRecord.Skew95 := Skew95[1]; TheRecord.Excess95 := Excess95[1];
        TheRecord.Exceedance1 := ExceedanceXX1[1]; TheRecord.Exceedance2 := ExceedanceXX2[1];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'NFRD';
        TheRecord.RecordNumber := 210;
        TheRecord.NFRD := NFRD[1]; TheRecord.NLAG := NLAG[1];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'NPORT';
        TheRecord.RecordNumber := 211;
        TheRecord.NPORT := NPORT[1]; TheRecord.NEXCD := NEXCD[1];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'CRIT';
        TheRecord.RecordNumber := 212;
        TheRecord.Crit1 := Crit[1]; TheRecord.Crit2 := Crit[2];
        TheRecord.Crit3 := Crit[3]; TheRecord.Crit4 := Crit[4];
        TheRecord.Crit5 := Crit[5]; TheRecord.Crit6 := Crit[6];
        TheRecord.Crit7 := Crit[7]; TheRecord.Crit8 := Crit[8];
        TheRecord.Crit9 := Crit[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'AIC';
        TheRecord.RecordNumber := 213;
        TheRecord.AIC1 := AIC[1]; TheRecord.AIC2 := AIC[2];
        TheRecord.AIC3 := AIC[3]; TheRecord.AIC4 := AIC[4];
        TheRecord.AIC5 := AIC[5]; TheRecord.AIC6 := AIC[6];
        TheRecord.AIC7 := AIC[7]; TheRecord.AIC8 := AIC[8];
        TheRecord.AIC9 := AIC[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'DIST';
        TheRecord.RecordNumber := 214;
        TheRecord.DIST1 := DIST[1]; TheRecord.DIST2 := DIST[2];
        TheRecord.DIST3 := DIST[3]; TheRecord.DIST4 := DIST[4];
        TheRecord.DIST5 := DIST[5]; TheRecord.DIST6 := DIST[6];
        TheRecord.DIST7 := DIST[7]; TheRecord.DIST8 := DIST[8];
        TheRecord.DIST9 := DIST[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'NCORR';
        TheRecord.RecordNumber := 215;
        for ItemLoop := 1 to NLAG[1] do
        begin
          TheRecord.NCorr1 := NCorr[1,ItemLoop]; TheRecord.NCorr2 := NCorr[2,ItemLoop];
          write(fyle,TheRecord);
        end;

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PCORR1';
        TheRecord.RecordNumber := 216;
        for ItemLoop := 1 to NLAG[1] do
        begin
          TheRecord.PCorr11 := PCorr[1,1,ItemLoop]; TheRecord.PCorr12 := PCorr[2,1,ItemLoop];
          TheRecord.PCorr13 := PCorr[3,1,ItemLoop]; TheRecord.PCorr14 := PCorr[4,1,ItemLoop];
          TheRecord.PCorr15 := PCorr[5,1,ItemLoop]; TheRecord.PCorr16 := PCorr[6,1,ItemLoop];
          TheRecord.PCorr17 := PCorr[7,1,ItemLoop]; TheRecord.PCorr18 := PCorr[8,1,ItemLoop];
          TheRecord.PCorr19 := PCorr[9,1,ItemLoop];
          write(fyle,TheRecord);
        end;

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PCORR2';
        TheRecord.RecordNumber := 217;
        for ItemLoop := 1 to NLAG[1] do
        begin
          TheRecord.PCorr21 := PCorr[1,2,ItemLoop]; TheRecord.PCorr22 := PCorr[2,2,ItemLoop];
          TheRecord.PCorr23 := PCorr[3,2,ItemLoop]; TheRecord.PCorr24 := PCorr[4,2,ItemLoop];
          TheRecord.PCorr25 := PCorr[5,2,ItemLoop]; TheRecord.PCorr26 := PCorr[6,2,ItemLoop];
          TheRecord.PCorr27 := PCorr[7,2,ItemLoop]; TheRecord.PCorr28 := PCorr[8,2,ItemLoop];
          TheRecord.PCorr29 := PCorr[9,2,ItemLoop];
          write(fyle,TheRecord);
        end;

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PHI1';
        TheRecord.RecordNumber := 218;
        TheRecord.Phi11 := Phi[1,1]; TheRecord.Phi12 := Phi[1,2];
        TheRecord.Phi13 := Phi[1,3]; TheRecord.Phi14 := Phi[1,4];
        TheRecord.Phi15 := Phi[1,5]; TheRecord.Phi16 := Phi[1,6];
        TheRecord.Phi17 := Phi[1,7]; TheRecord.Phi18 := Phi[1,8];
        TheRecord.Phi19 := Phi[1,9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PHI2';
        TheRecord.RecordNumber := 219;
        TheRecord.Phi21 := Phi[2,1]; TheRecord.Phi22 := Phi[2,2];
        TheRecord.Phi23 := Phi[2,3]; TheRecord.Phi24 := Phi[2,4];
        TheRecord.Phi25 := Phi[2,5]; TheRecord.Phi26 := Phi[2,6];
        TheRecord.Phi27 := Phi[2,7]; TheRecord.Phi28 := Phi[2,8];
        TheRecord.Phi29 := Phi[2,9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'THETA1';
        TheRecord.RecordNumber := 220;
        TheRecord.Theta11 := Theta[1,1]; TheRecord.Theta12 := Theta[1,2];
        TheRecord.Theta13 := Theta[1,3]; TheRecord.Theta14 := Theta[1,4];
        TheRecord.Theta15 := Theta[1,5]; TheRecord.Theta16 := Theta[1,6];
        TheRecord.Theta17 := Theta[1,7]; TheRecord.Theta18 := Theta[1,8];
        TheRecord.Theta19 := Theta[1,9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'THETA2';
        TheRecord.RecordNumber := 221;
        TheRecord.Theta21 := Theta[2,1]; TheRecord.Theta22 := Theta[2,2];
        TheRecord.Theta23 := Theta[2,3]; TheRecord.Theta24 := Theta[2,4];
        TheRecord.Theta25 := Theta[2,5]; TheRecord.Theta26 := Theta[2,6];
        TheRecord.Theta27 := Theta[2,7]; TheRecord.Theta28 := Theta[2,8];
        TheRecord.Theta29 := Theta[2,9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Historical1';
        TheRecord.RecordNumber := 222;
        TheRecord.Hist11 := Historical[1,1]; TheRecord.Hist12 := Historical[1,2];
        TheRecord.Hist13 := Historical[1,3]; TheRecord.Hist14 := Historical[1,4];
        TheRecord.Hist15 := Historical[1,5]; TheRecord.Hist16 := Historical[1,6];
        TheRecord.Hist17 := Historical[1,7]; TheRecord.Hist18 := Historical[1,8];
        TheRecord.Hist19 := Historical[1,9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Historical2';
        TheRecord.RecordNumber := 223;
        TheRecord.Hist21 := Historical[2,1]; TheRecord.Hist22 := Historical[2,2];
        TheRecord.Hist23 := Historical[2,3]; TheRecord.Hist24 := Historical[2,4];
        TheRecord.Hist25 := Historical[2,5]; TheRecord.Hist26 := Historical[2,6];
        TheRecord.Hist27 := Historical[2,7]; TheRecord.Hist28 := Historical[2,8];
        TheRecord.Hist29 := Historical[2,9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'NConf';
        TheRecord.RecordNumber := 224;
        TheRecord.NConf := NConf[1];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PFRD';
        TheRecord.RecordNumber := 225;
        TheRecord.PFRD1 := PFRD[1]; TheRecord.PFRD2 := PFRD[2];
        TheRecord.PFRD3 := PFRD[3]; TheRecord.PFRD4 := PFRD[4];
        TheRecord.PFRD5 := PFRD[5]; TheRecord.PFRD6 := PFRD[6];
        TheRecord.PFRD7 := PFRD[7]; TheRecord.PFRD8 := PFRD[8];
        TheRecord.PFRD9 := PFRD[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PLAG';
        TheRecord.RecordNumber := 226;
        TheRecord.PLAG1 := PLAG[1]; TheRecord.PLAG2 := PLAG[2];
        TheRecord.PLAG3 := PLAG[3]; TheRecord.PLAG4 := PLAG[4];
        TheRecord.PLAG5 := PLAG[5]; TheRecord.PLAG6 := PLAG[6];
        TheRecord.PLAG7 := PLAG[7]; TheRecord.PLAG8 := PLAG[8];
        TheRecord.PLAG9 := PLAG[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PCONF';
        TheRecord.RecordNumber := 227;
        TheRecord.PCONF1 := PCONF[1]; TheRecord.PCONF2 := PCONF[2];
        TheRecord.PCONF3 := PCONF[3]; TheRecord.PCONF4 := PCONF[4];
        TheRecord.PCONF5 := PCONF[5]; TheRecord.PCONF6 := PCONF[6];
        TheRecord.PCONF7 := PCONF[7]; TheRecord.PCONF8 := PCONF[8];
        TheRecord.PCONF9 := PCONF[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PEXCD';
        TheRecord.RecordNumber := 228;
        TheRecord.PEXCD1 := PEXCD[1]; TheRecord.PEXCD2 := PEXCD[2];
        TheRecord.PEXCD3 := PEXCD[3]; TheRecord.PEXCD4 := PEXCD[4];
        TheRecord.PEXCD5 := PEXCD[5]; TheRecord.PEXCD6 := PEXCD[6];
        TheRecord.PEXCD7 := PEXCD[7]; TheRecord.PEXCD8 := PEXCD[8];
        TheRecord.PEXCD9 := PEXCD[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'PPORT';
        TheRecord.RecordNumber := 229;
        TheRecord.PPORT1 := PPORT[1]; TheRecord.PPORT2 := PPORT[2];
        TheRecord.PPORT3 := PPORT[3]; TheRecord.PPORT4 := PPORT[4];
        TheRecord.PPORT5 := PPORT[5]; TheRecord.PPORT6 := PPORT[6];
        TheRecord.PPORT7 := PPORT[7]; TheRecord.PPORT8 := PPORT[8];
        TheRecord.PPORT9 := PPORT[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'CRP';
        TheRecord.RecordNumber := 230;
        TheRecord.CRP1 := CRP[1]; TheRecord.CRP2 := CRP[2];
        TheRecord.CRP3 := CRP[3]; TheRecord.CRP4 := CRP[4];
        TheRecord.CRP5 := CRP[5]; TheRecord.CRP6 := CRP[6];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'QI';
        TheRecord.RecordNumber := 231;
        TheRecord.QI1 := QI[1];   TheRecord.QI2 := QI[2];
        TheRecord.QI3 := QI[3];   TheRecord.QI4 := QI[4];
        TheRecord.QI5 := QI[5];   TheRecord.QI6 := QI[6];
        TheRecord.QI7 := QI[7];   TheRecord.QI8 := QI[8];
        TheRecord.QI9 := QI[9];   TheRecord.QI10 := QI[10];
        TheRecord.QI11 := QI[11]; TheRecord.QI12 := QI[12];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'CRNX';
        TheRecord.RecordNumber := 232;
        TheRecord.CRNX1 := CRNX[1];   TheRecord.CRNX2 := CRNX[2];
        TheRecord.CRNX3 := CRNX[3];   TheRecord.CRNX4 := CRNX[4];
        TheRecord.CRNX5 := CRNX[5];   TheRecord.CRNX6 := CRNX[6];
        TheRecord.CRNX7 := CRNX[7];   TheRecord.CRNX8 := CRNX[8];
        TheRecord.CRNX9 := CRNX[9];   TheRecord.CRNX10 := CRNX[10];
        TheRecord.CRNX11 := CRNX[11]; TheRecord.CRNX12 := CRNX[12];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'CRJX';
        TheRecord.RecordNumber := 233;
        TheRecord.CRJX1 := CRJX[1];   TheRecord.CRJX2 := CRJX[2];
        TheRecord.CRJX3 := CRJX[3];   TheRecord.CRJX4 := CRJX[4];
        TheRecord.CRJX5 := CRJX[5];   TheRecord.CRJX6 := CRJX[6];
        TheRecord.CRJX7 := CRJX[7];   TheRecord.CRJX8 := CRJX[8];
        TheRecord.CRJX9 := CRJX[9];   TheRecord.CRJX10 := CRJX[10];
        TheRecord.CRJX11 := CRJX[11]; TheRecord.CRJX12 := CRJX[12];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'CRSX';
        TheRecord.RecordNumber := 234;
        TheRecord.CRSX1 := CRSX[1];   TheRecord.CRSX2 := CRSX[2];
        TheRecord.CRSX3 := CRSX[3];   TheRecord.CRSX4 := CRSX[4];
        TheRecord.CRSX5 := CRSX[5];   TheRecord.CRSX6 := CRSX[6];
        TheRecord.CRSX7 := CRSX[7];   TheRecord.CRSX8 := CRSX[8];
        TheRecord.CRSX9 := CRSX[9];   TheRecord.CRSX10 := CRSX[10];
        TheRecord.CRSX11 := CRSX[11]; TheRecord.CRSX12 := CRSX[12];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'RANK';
        TheRecord.RecordNumber := 235;
        TheRecord.Rank1 := Rank[1];   TheRecord.Rank2 := Rank[2];
        TheRecord.Rank3 := Rank[3];   TheRecord.Rank4 := Rank[4];
        TheRecord.Rank5 := Rank[5];   TheRecord.Rank6 := Rank[6];
        TheRecord.Rank7 := Rank[7];   TheRecord.Rank8 := Rank[8];
        TheRecord.Rank9 := Rank[9];
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'TimeSeries';
        TheRecord.RecordNumber := 236;
        TheRecord.DefaultTimeSeries := DefaultTimeSeriesModel;
        TheRecord.UserTimeSeries := UserTimeSeriesModel;
        write(fyle,TheRecord);
      end;//with CurrentRecord
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//PROCEDURE SaveTimeSeriesData

  procedure SaveFlowData;
  const OPNAME = 'UStomsaData.SaveFlowData';
  var SequenceLoop, YearLoop : Integer;
  begin
    try
      with CurrentRecord do
      begin
        fmMessages.DisplayMessage('Saving Flow Data for '+CurrentRecord.FileName);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'FlowParams';
        TheRecord.RecordNumber := 300;
        TheRecord.SequenceCount := SequenceCount;
        TheRecord.GeneratedYearCount := GeneratedYearCount;
        write(fyle,TheRecord);

        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'Flows';
        TheRecord.RecordNumber := 301;
        for SequenceLoop := 1 to SequenceCount do
          for YearLoop := 1 to GeneratedYearCount do
          begin
            TheRecord.SequenceNumber := SequenceLoop;
            TheRecord.GeneratedYearNumber := YearLoop;
            TheRecord.FlowData1  := GeneratedFlows[SequenceLoop,YearLoop,1];
            TheRecord.FlowData2  := GeneratedFlows[SequenceLoop,YearLoop,2];
            TheRecord.FlowData3  := GeneratedFlows[SequenceLoop,YearLoop,3];
            TheRecord.FlowData4  := GeneratedFlows[SequenceLoop,YearLoop,4];
            TheRecord.FlowData5  := GeneratedFlows[SequenceLoop,YearLoop,5];
            TheRecord.FlowData6  := GeneratedFlows[SequenceLoop,YearLoop,6];
            TheRecord.FlowData7  := GeneratedFlows[SequenceLoop,YearLoop,7];
            TheRecord.FlowData8  := GeneratedFlows[SequenceLoop,YearLoop,8];
            TheRecord.FlowData9  := GeneratedFlows[SequenceLoop,YearLoop,9];
            TheRecord.FlowData10 := GeneratedFlows[SequenceLoop,YearLoop,10];
            TheRecord.FlowData11 := GeneratedFlows[SequenceLoop,YearLoop,11];
            TheRecord.FlowData12 := GeneratedFlows[SequenceLoop,YearLoop,12];
            write(fyle,TheRecord);
          end;//for YearLoop
      end;//with CurrentRecord
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//PROCEDURE SaveFlowData

  procedure SaveCheckSum;
  const OPNAME = 'UStomsaData.SaveCheckSum';
  begin
    try
      with CurrentRecord do
      begin
        TheRecord := BlankRecord;
        TheRecord.RecordTag := 'CheckSum';
        TheRecord.RecordNumber := 999;
        TheRecord.CheckSum := true;
        write(fyle,TheRecord);
      end;//with CurrentRecord
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

begin
  try
    BlankRecord.RecordTag := '';
    BlankRecord.RecordNumber := 0;
    BlankRecord.FileName := '';

    AssignFile(fyle,FileName);
    Rewrite(fyle);
    First;
    repeat
      SaveGeneralData;
      if CurrentRecord.FMarginalFitted then
        SaveMarginalData;
      if CurrentRecord.FTimeSeriesFitted then
        SaveTimeSeriesData;
      if CurrentRecord.FlowsGenerated and (SaveExtraData = True) then
        SaveFlowData;
      SaveCheckSum;
    until Next = False;
    fmMessages.Hide;
    CloseFile(fyle);
    FProjectFileName := FileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.FindIncFileByName(ADirectory, AFileName : string): TIncData;
const OPNAME = 'TStomsaData.FindIncFileByName';
var
  LIndex : integer;
  LData  : TIncData;
begin
  Result := nil;
  try
    ADirectory := IncludeTrailingPathDelimiter(ADirectory);
    ADirectory := UpperCase(ADirectory);
    AFileName := UpperCase(AFileName);

    for LIndex := 0 to FIncDataList.Count-1 do
    begin
      LData := TIncData(FIncDataList.Items[LIndex]);
      if(UpperCase(LData.Directory) = ADirectory) and (UpperCase(LData.FileName) = AFileName) then
      begin
        Result := LData;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.FindIncFileByIndex(AIndex : Integer) : TIncData;
const OPNAME = 'TStomsaData.FindIncFileByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FIncDataList.Count) then
    begin
      Result := TIncData(FIncDataList.Items[AIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.AddNode(ADirectory, AFileName : string): boolean;
const OPNAME = 'TStomsaData.AddNode';
var
  tmpIncData : TIncData;
  fyle       : TextFile;
  LMeanCounter,
  Counter,
  loop,loop2 : Integer;
  FileString : String;
  LIndex     : integer;
begin
  Result := False;
  try
    ADirectory  := IncludeTrailingPathDelimiter(ADirectory);
    tmpIncData := FindIncFileByName(ADirectory,AFileName);

    if tmpIncData = nil then
    begin
      //Insert new INC file at end beginning of the list
      tmpIncData := TIncData.Create;
      FIncDataList.Add(tmpIncData);
      tmpIncData.FileName             := AFileName;
      tmpIncData.Directory            := ADirectory;
      tmpIncData.StartYear            := 0;
      tmpIncData.EndYear              := 0;
      tmpIncData.RecordLength         := 0;
      tmpIncData.FKeyGauge             := False;
      tmpIncData.FMarginalFitted       := False;
      tmpIncData.FTimeSeriesFitted     := false;
      tmpIncData.SequenceCount        := 0;
      tmpIncData.GeneratedYearCount   := 0;
      tmpIncData.GenerateFlows        := false;
      tmpIncData.FlowsGenerated       := false;
      tmpIncData.FStatisticsCalculated := false;
      tmpIncData.Area                 := 1.0;
      tmpIncData.MAR                  := 0;
      tmpIncData.FlowCorrelationData  := TFlowCorrelation.Create;;

      tmpIncData.OnKeyGaugeChange         := FOnKeyGaugeChange;
      tmpIncData.OnMarginalFitted         := FOnMarginalFitted;
      tmpIncData.OnMarginalUnfitted       := FOnMarginalUnfitted;
      tmpIncData.OnTimeSeriesFitted       := FOnTimeSeriesFitted;
      tmpIncData.OnTimeSeriesUnFitted     := FOnTimeSeriesUnFitted;
      tmpIncData.OnFlowsAndStatsGenerated := FOnFlowsAndStatsGenerated;

      //Load all data from the file
      if FileExists(ADirectory + AFileName) then
      begin
        AssignFile(fyle,ADirectory + AFileName);
        Reset(fyle);
        //Note a Max of 1000 entries due to restrictions of array size
        for loop := 1 to 1000 do
          tmpIncData.AnnualRawIncData[loop] := 0.0;
        Counter := 1;
        LMeanCounter := 0;
        while NOT(EOF(fyle)) do
        begin
          ReadLn(fyle,FileString);
          if tmpIncData.StartYear = 0 then
            tmpIncData.StartYear := StrToInt(Copy(FileString,1,8));
          tmpIncData.MonthlyRawIncData[Counter,1] := StrToFloat(copy(FileString,10,7));
          tmpIncData.MonthlyRawIncData[Counter,2] := StrToFloat(copy(FileString,18,7));
          tmpIncData.MonthlyRawIncData[Counter,3] := StrToFloat(copy(FileString,26,7));
          tmpIncData.MonthlyRawIncData[Counter,4] := StrToFloat(copy(FileString,34,7));
          tmpIncData.MonthlyRawIncData[Counter,5] := StrToFloat(copy(FileString,42,7));
          tmpIncData.MonthlyRawIncData[Counter,6] := StrToFloat(copy(FileString,50,7));
          tmpIncData.MonthlyRawIncData[Counter,7] := StrToFloat(copy(FileString,58,7));
          tmpIncData.MonthlyRawIncData[Counter,8] := StrToFloat(copy(FileString,66,7));
          tmpIncData.MonthlyRawIncData[Counter,9] := StrToFloat(copy(FileString,74,7));
          tmpIncData.MonthlyRawIncData[Counter,10] := StrToFloat(copy(FileString,82,7));
          tmpIncData.MonthlyRawIncData[Counter,11] := StrToFloat(copy(FileString,90,7));
          tmpIncData.MonthlyRawIncData[Counter,12] := StrToFloat(copy(FileString,98,7));

          //The file could potentially contain incorrect annual totals, rather recalculate
          for loop2 := 1 to 12 do
            tmpIncData.AnnualRawIncData[Counter]:= tmpIncData.AnnualRawIncData[Counter]+ tmpIncData.MonthlyRawIncData[Counter,loop2];

          //!!!!!!!!!!!!!!!!!!
          //Round the data to 2 decimals, this is a temporary measure to ensure that the results are compatible
          //with the old CROSSYR calculations
          tmpIncData.AnnualRawIncData[Counter]:= Round(tmpIncData.AnnualRawIncData[Counter]*100)/100;
          //!!!!!!!!!!!!!!!!!!

          for loop2 := 1 to 12 do
           tmpIncData.MAR := tmpIncData.MAR + tmpIncData.MonthlyRawIncData[Counter,loop2];
          LMeanCounter := LMeanCounter + 1;

          Inc(Counter);
        end;
        tmpIncData.MAR := tmpIncData.MAR / LMeanCounter;
        tmpIncData.EndYear := StrToInt(copy(FileString,0,8));
        tmpIncData.RecordLength := Counter - 1;
        CloseFile(fyle);
      end;

      //Inc(FIncFileCount);
      PARAMFileCreated := false;
      Result := True;
      if Assigned(FOnIncAdd) then FOnIncAdd(Self,AFileName,ADirectory);
      LIndex := FFileNamesObject.HydrologyFileNames.Count+1;
      CastFileNamesObject.PopulateHydrologyPaths(ExtractFilePath(ADirectory));
      CastFileNamesObject.AddHydrologyFileName(LIndex,AFileName,False,0.0,FileLastWriteDate(AFileName));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.RemoveNode(ADirectory, AFileName : string): boolean;
const OPNAME = 'TStomsaData.RemoveNode';
var
  tmpIncData : TIncData;
begin
  Result := False;
  try
    tmpIncData := FindIncFileByName(ADirectory,AFileName);
    if(tmpIncData <> nil) then
    begin
      //Decrease the counters where required
      //Dec(FIncFileCount);
      //if tmpIncData.FKeyGauge then
      //  Dec(FKeyGaugeCount);
      //if tmpIncData.FMarginalFitted then
      //  Dec(FMarginalFittedCount);
      //if tmpIncData.FTimeSeriesFitted then
      //  Dec(FTimeSeriesFittedCount);
      //if tmpIncData.FStatisticsCalculated then
      //  Dec(FStatisticsCalculatedCount);
      PARAMFileCreated := false;

      if Assigned(FOnIncRemove) then FOnIncRemove(Self,AFileName,ADirectory);

      FIncDataList.Delete(FIncDataList.IndexOf(tmpIncData));
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStomsaData.RefreshInc;
const OPNAME = 'TStomsaData.RefreshInc';
begin
  try
    FIncDataList.Clear;
    FCurrentIncPosition := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function WriteFixedLengthFloat(TheValue : Extended; Precision,Decimals,TheLength : integer) : string;
const OPNAME = 'UStomsaData.WriteFixedLengthFloat';
const Space = ' ';
var
  TempString : string;
begin
  Result := '';
  try
    TempString := FloatToStrF(TheValue,ffFixed,Precision,Decimals);
    while (Length(TempString) < TheLength) do
      TempString := Space + TempString;
    Result := TempString;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function WriteFixedLengthInteger(TheValue, TheLength : Integer) : string;
const OPNAME = 'UStomsaData.WriteFixedLengthInteger';
      Space = ' ';
var
  TempString : string;
begin
  Result := '';
  try
    TempString := IntToStr(TheValue);
    while (Length(TempString) < TheLength) do
      TempString := Space + TempString;
    Result := TempString;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.GetANSFile : TStringList;
const OPNAME = 'TStomsaData.GetANSFile';
var ANSData : TStringList;
begin
  Result := nil;
  try
    ANSData := TStringList.Create;
    ANSData.Clear;
    if IncFileCount > 0 then
    begin
      ANSData.Add('THE GAUGES USED IN THIS ANALYSIS ARE:');
      ANSData.Add('');
      if First then
      begin
        ANSData.Add(CurrentRecord.FileName);
        while Next do
          ANSData.Add(CurrentRecord.FileName);
      end;
    end;
    Result := ANSData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//FUNCTION GetANSFile

function TStomsaData.GeneratePARAMFile : TStringList;
const OPNAME = 'TStomsaData.GeneratePARAMFile';
var PARAMData : TStringList;
    KeyGaugePosition,
    LastPosition : Integer;
    x,r : double;

  function WriteGauge : boolean;
  const OPNAME = 'UStomsaData.WriteGauge';
  begin
    Result := true;
    try
      PARAMData.Add(CurrentRecord.Directory + Copy(CurrentRecord.FileName,1,Pos('.',CurrentRecord.FileName)-1));
      PARAMData.Add(WriteFixedLengthInteger(CurrentRecord.RecordLength,12) +
                    //WriteFixedLengthInteger((CurrentRecord.StartYear - 1900),12) + DSR
                    WriteFixedLengthInteger((CurrentRecord.StartYear),12) +
                    WriteFixedLengthFloat(CurrentRecord.Historical[1,CurrentRecord.UserTimeSeriesModel],15,4,12) +
                    WriteFixedLengthFloat(CurrentRecord.Historical[1,1],15,4,12) +
                    WriteFixedLengthFloat(CurrentRecord.Historical[2,CurrentRecord.UserTimeSeriesModel],15,4,12) +
                    WriteFixedLengthFloat(CurrentRecord.Historical[2,1],15,4,12));
      PARAMData.Add(' ' + IntToStr(CurrentRecord.Itype) + ' ' +
              WriteFixedLengthFloat(CurrentRecord.Gamma[CurrentRecord.UserCurve],15,7,20) +
              WriteFixedLengthFloat(CurrentRecord.Delta[CurrentRecord.UserCurve],15,7,20) +
              WriteFixedLengthFloat(CurrentRecord.XLamda[CurrentRecord.UserCurve],15,7,20) +
              WriteFixedLengthFloat(CurrentRecord.Xx[CurrentRecord.UserCurve],15,7,20));
      PARAMData.Add(WriteFixedLengthFloat(CurrentRecord.Average[1],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.StdDeviation[1],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.Phi[1,CurrentRecord.UserTimeSeriesModel],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.Phi[2,CurrentRecord.UserTimeSeriesModel],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.Theta[1,CurrentRecord.UserTimeSeriesModel],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.Theta[2,CurrentRecord.UserTimeSeriesModel],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.PZero,15,5,12) +
                    WriteFixedLengthInteger(CurrentRecord.ZeroCount,4));

  //!!!!!!!!!!!
  //need to calculate this in the PARAM form and store it in the data file
  //!!!!!!!!!!!

  // The following shoud be ANC where ANC = XN*XN/(XN-2.), - and XN number of non-zero years.
                     x := CurrentRecord.RecordLength -CurrentRecord.ZeroCount;
                     r := x*x/(x-2.0);
      PARAMData.Add(WriteFixedLengthFloat(CurrentRecord.Mean[1],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.StdDev[1],15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.Crit[CurrentRecord.UserTimeSeriesModel],15,5,12) +
  //                  WriteFixedLengthFloat(CurrentRecord.AIC[CurrentRecord.UserTimeSeriesModel],15,5,12));
                    WriteFixedLengthFloat(r,15,5,12) +
                    WriteFixedLengthFloat(CurrentRecord.Area,15,3,7)) ; // Write the ANC
      PARAMData.Add('');
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//FUNCTION WriteGauge

  procedure WriteG0;
  const OPNAME = 'UStomsaData.WriteG0';
  var
    loop1, loop2, LastPosition : integer;
  begin
    try
      PARAMData.Add(' B - THE SQUARE-ROOT OF THE LAG-ZERO DISPERSION MATRIX G0');
      for loop1 := 1 to IncFileCount do
      begin
        LastPosition := PARAMData.Add('');
        for loop2 := 1 to IncFileCount do
        begin
          PARAMData.Strings[LastPosition] := PARAMData.Strings[LastPosition] + WriteFixedLengthFloat(FCrossResults.Root_G0[loop2,loop1],15,8,12);
        end;
      end;
      PARAMData.Add('');
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

  procedure WriteH0;
  const OPNAME = 'UStomsaData.WriteH0';
  var
    loop1, loop2, LastPosition : integer;
  begin
    try
      PARAMData.Add(' B0 - THE SQUARE-ROOT OF THE LAG-ZERO STARTING MATRIX H0');
      for loop1 := 1 to IncFileCount do
      begin
        LastPosition := PARAMData.Add('');
        for loop2 := 1 to IncFileCount do
        begin
          PARAMData.Strings[LastPosition] := PARAMData.Strings[LastPosition] + WriteFixedLengthFloat(FCrossResults.Root_H0[loop2,loop1],15,8,12);
        end;
      end;
      PARAMData.Add('');
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

  procedure WriteH1;
  const OPNAME = 'UStomsaData.WriteH1';
  var
    loop1, loop2, LastPosition : integer;
  begin
    try
      PARAMData.Add(' B1 - THE SQUARE-ROOT OF THE LAG-ONE STARTING MATRIX H1');
      for loop1 := 1 to IncFileCount do
      begin
        LastPosition := PARAMData.Add('');
        for loop2 := 1 to IncFileCount do
        begin
          PARAMData.Strings[LastPosition] := PARAMData.Strings[LastPosition] + WriteFixedLengthFloat(FCrossResults.Root_H1[loop2,loop1],15,8,12);
        end;
      end;
      PARAMData.Add('');
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

  procedure WriteZ;
  const OPNAME = 'UStomsaData.WriteZ';
  var
    loop1, loop2, LastPosition : integer;
  begin
    try
      PARAMData.Add(' A - THE COEFFICIENT MATRIX OF Z(-1) IN CALCULATING Z(0)');
      for loop1 := 1 to IncFileCount do
      begin
        LastPosition := PARAMData.Add('');
        for loop2 := 1 to IncFileCount do
        begin
          PARAMData.Strings[LastPosition] := PARAMData.Strings[LastPosition] + WriteFixedLengthFloat(FCrossResults.Coef_Z[loop2,loop1],15,8,12);
        end;
      end;
      PARAMData.Add('');
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

  procedure WriteA;
  const OPNAME = 'UStomsaData.WriteA';
  var
    loop1, loop2, LastPosition : integer;
  begin
    try
      PARAMData.Add(' C - THE COEFFICIENT MATRIX OF A(-1) IN CALCULATING Z(0)');
      for loop1 := 1 to IncFileCount do
      begin
        LastPosition := PARAMData.Add('');
        for loop2 := 1 to IncFileCount do
        begin
          PARAMData.Strings[LastPosition] := PARAMData.Strings[LastPosition] + WriteFixedLengthFloat(FCrossResults.Coef_A[loop2,loop1],15,8,12);
        end;
      end;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

begin
  Result := nil;
  try
    PARAMData := TStringList.Create;
    PARAMData.Clear;
    if IncFileCount > 0 then
    begin
      //HEADER

      PARAMData.Add(WriteFixedLengthInteger(IncFileCount,12)+'  PARAMETER FILE');
      PARAMData.Add('');

      //This list identifies the NUMBER of KEY gauges and their position in the list
      KeyGaugePosition := 1;
      LastPosition := PARAMData.Add(IntToStr(KeyGaugeCount));
      if First then
      begin
        if  CurrentRecord.FKeyGauge then
          PARAMData.Strings[LastPosition] := PARAMData.Strings[LastPosition] + ' ' + IntToStr(KeyGaugePosition);
        Inc(KeyGaugePosition);
        While Next do
        begin
          if  CurrentRecord.FKeyGauge then
            PARAMData.Strings[LastPosition] := PARAMData.Strings[LastPosition] + ' ' + IntToStr(KeyGaugePosition);
          Inc(KeyGaugePosition);
        end;
      end;
      PARAMData.Add('');

      //Headers from the YER file
      if First then
      begin
        WriteGauge;
        while Next do
          WriteGauge;
      end;

      //Fitting MATRICES
      WriteG0;
      WriteH0;
      WriteH1;
      WriteZ;
      WriteA;
    end;
    Result := PARAMData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//FUNCTION GetPARAMFile

function TStomsaData.WriteStochasticFiles(FileName : string) : boolean;
const OPNAME = 'TStomsaData.WriteStochasticFiles';
{Write out all the sequences seperately to data files}
var
  fyle : TextFile;
  SequenceLoop, YearLoop, MonthLoop : Integer;
  AnnTot : double;
begin
  Result := False;
  try
    with CurrentRecord do
    begin
      for SequenceLoop := 1 to 500 do
      begin
        if CurrentRecord.GeneratedYearCount > 0 then
        begin
          AssignFile(fyle,ChangeFileExt(FileName,'.' + Format('%3.3d',[SequenceLoop])));
          ReWrite(fyle);
          for YearLoop := 1 to CurrentRecord.GeneratedYearCount do
          begin
            write(fyle,format('%8S',[IntToStr(StartYear + YearLoop - 1)]));
            AnnTot := 0.0;
            for MonthLoop := 1 to 12 do
            begin
              //Calculate the annual total
              AnnTot:= AnnTot + GeneratedFlows[SequenceLoop,YearLoop,MonthLoop];
              Write(fyle,format('%8S',[FloatToStrAny(GeneratedFlows[SequenceLoop,YearLoop,MonthLoop],2)]));
            end;//for MonthLoop
            // Write the annual total
              Write(fyle,format('%10S',[FloatToStrAny(AnnTot,2)]));
            WriteLn(fyle);
          end;
          CloseFile(fyle);
        end;//if CurrentRecord
      end;//for SequenceLoop
    end;//with CurrentRecord

    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.WriteANSFile(FileName : String) : Boolean;
const OPNAME = 'TStomsaData.WriteANSFile';
var
  fyle : TextFile;
  Loop,ip : integer;
  xv      : double;
begin
  Result := False;
  try
    AssignFile(fyle,Filename);
    ReWrite(fyle);

    //Assemble the report
    with CurrentRecord do
    begin
      WriteLn(fyle,Filename + ' Commencing in Year ' + IntToStr(StartYear) +
                          ' and of length ' + IntToStr(RecordLength));
      WriteLn(fyle);

      WriteLn(fyle,'THE TABLE OF PARAMETERS AND CRITERIA FITTED BY NORMALIZ');
      WriteLn(fyle,'        LN3             LN2             SB4             SB3');

      WriteLn(fyle,'GA    ' +
                          Format('%15S',[FloatToStrF(Gamma[1],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Gamma[2],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Gamma[3],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Gamma[4],ffExponent,8,2)]));
      WriteLn(fyle,'DE    ' +
                          Format('%15S',[FloatToStrF(Delta[1],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Delta[2],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Delta[3],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Delta[4],ffExponent,8,2)]));
      WriteLn(fyle,'XL    ' +
                          Format('%15S',[FloatToStrF(XLamda[1],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(XLamda[2],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(XLamda[3],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(XLamda[4],ffExponent,8,2)]));
      WriteLn(fyle,'XX    ' +
                          Format('%15S',[FloatToStrF(Xx[1],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Xx[2],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Xx[3],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Xx[4],ffExponent,8,2)]));
      WriteLn(fyle,'CR    ' +
                          Format('%15S',[FloatToStrF(Criterion[1],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Criterion[2],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Criterion[3],ffExponent,8,2)]) + ' ' +
                          Format('%15S',[FloatToStrF(Criterion[4],ffExponent,8,2)]));

      WriteLn(fyle);
      WriteLn(fyle,'THE MEAN AND STANDARD DEVIATION OF THE RAW POSITIVE FLOWS');
      WriteLn(fyle,FloatToStrAny(Average[1],2) + ' ' + FloatToStrAny(StdDeviation[1],2));
      WriteLn(fyle);
      WriteLn(fyle,'ITYPE' + #9 + 'GAMMA' + #9 + 'DELTA' + #9 +
                            'XLAM' + #9 + 'XI' + #9 + 'IFAULT');
      WriteLn(fyle,IntToStr(itype) + #9 +
                            FloatToStrF(Gamma[UserCurve],ffFixed,12,7) + #9 +
                            FloatToStrF(Delta[UserCurve],ffFixed,12,7) + #9 +
                            FloatToStrF(XLamda[UserCurve],ffFixed,12,7) + #9 +
                            FloatToStrF(Xx[UserCurve],ffFixed,12,7) + #9 +
                            IntToStr(IFault));
      WriteLn(fyle);
      WriteLn(fyle,'AND THE MEAN AND STANDARD DEVIATION OF THE SEQUENCE BEFORE STANDARDIZATION ARE:');
      WriteLn(fyle,'' + ' ' + '');
      WriteLn(fyle);
      WriteLn(fyle,'THE RANKED FLOWS FOR PLOTTING -');
      WriteLn(fyle,'ABCISSA    NATURAL   NORMALISED STANDARD');

      for Loop :=1 to RecordLength do
      begin
        // xv is the plotted X-axis value based on all the years
        xv := Loop/(RecordLength+1);
        if Loop > ZeroCount then
        begin
          ip := Loop - ZeroCount;
          WriteLn(fyle,Format('%10S',[FloatToStrAny(xv,3)]) +
                       Format('%10S',[FloatToStrAny(NaturalFlows[ip],3)]) +
                       Format('%10S',[FloatToStrAny(NormalisedFlows[UserCurve-1,ip-1],3)]) +
                       Format('%10S',[FloatToStrAny(Standardised[ip],3)]));
        end;
      end;
      WriteLn(fyle);
      WriteLn(fyle,'EXCEEDANCE PROBABILITY');
      WriteLn(fyle,'MEAN, STANDARD DEVIATION, SKEWNESS AND EXCESS OF');
      WriteLn(fyle,'NON-ZERO FLOWS   :' + Format('%10S',[FloatToStrAny(Mean[1],3)]) + Format('%10S',[FloatToStrAny(StdDev[1],3)]) + Format('%10S',[FloatToStrAny(Skew[1],3)]) + Format('%10S',[FloatToStrAny(Excess[1],3)]));
      WriteLn(fyle,'NORMALIZED FLOWS :' + Format('%10S',[FloatToStrAny(NormalisedMean[1],3)]) + Format('%10S',[FloatToStrAny(NormalisedStdDev[1],3)]) + Format('%10S',[FloatToStrAny(NormalisedSkew[1],3)]) + Format('%10S',[FloatToStrAny(NormalisedExcess[1],3)]));
      WriteLn(fyle);
      WriteLn(fyle,'THE 95% C.L. FOR SKEW AND EXCESS ARE :' + #9 + FloatToStrAny(Skew95[1],3) + #9 + FloatToStrAny(Excess95[1],3));
      WriteLn(fyle);
      WriteLn(fyle,'THE CHI-SQUARE STATISTIC FOR THE NORMAL FIT TO THE FLOWS :');
      WriteLn(fyle,FloatToStrAny(ChiSquare[1],3)+ ' WITH ' + IntToStr(M1) + ' DEGREES OF FREEDOM');
      WriteLn(fyle);
      WriteLn(fyle,'THE EXCEEDANCE PROBABILITY OF THIS VALUE IS : ' + FloatToStrAny(ExceedanceXX1[1],4));
      WriteLn(fyle);
      WriteLn(fyle,'AND TO THE NORMALIZED FLOWS IS:');
      WriteLn(fyle,FloatToStrAny(NormalisedChiSquare[1],3)+ ' WITH ' + IntToStr(M2) + ' DEGREES OF FREEDOM');
      WriteLn(fyle);
      WriteLn(fyle,'THE EXCEEDANCE PROBABILITY OF THIS VALUE IS : ' + FloatToStrAny(ExceedanceXX2[1],4));
      WriteLn(fyle);
      WriteLn(fyle,'THE CORRELATION AND PARTIAL CORRELATION FUNCTION FOR THE');
      WriteLn(fyle,'NORMALIZED FLOWS, WITH CONFIDENCE INTERVAL ±' + FloatToStrAny(NCONF[1],3));
      WriteLn(fyle);
      for loop := 1 to NLAG[1] do
      begin
        WriteLn(fyle,Format('%10S',[IntToStr(Loop)]) +
                     Format('%10S',[FloatToStrAny(NCorr[1,Loop],3)]) +
                     Format('%10S',[FloatToStrAny(NCorr[2,Loop],3)]));
      end;
      WriteLn(fyle);
      WriteLn(fyle,'THE SEQUENCE IS OF LENGTH '+IntToStr(RecordLength)+
                          ' WITH A CORRELOGRAM OF LENGTH '+ IntToStr(NLAG[1]));
      WriteLn(fyle,'IT HAS ' + IntToStr(NFRD[1]) + ' DEGREES OF FREEDOM AND A PORTMATEAU STATISTIC OF ' + FloatToStrAny(NPORT[1],2));
      WriteLn(fyle,'THE EXCEEDANCE PROBABILITY OF THIS CHI-SQUARE STATISTIC IS ' + FloatToStrAny(NEXCD[1],4));
      WriteLn(fyle);
      WriteLn(fyle,'HERE FOLLOWS A SUMMARY OF THE OPTIMIZATION OUTPUT :');
      WriteLn(fyle,'...');
      WriteLn(fyle,'THE UPDATED AIC TABLE FOR ' + FileName + ', THE ROWS ARE THE ORDER p OF THE');
      WriteLn(fyle,'AR PART WITH p = 0, 1, 2. THE COLUMNS ARE THE ORDER q OF THE MA PART');
      WriteLn(fyle,'OF THE ARMA EQUATION WITH q = 0, 1, 2.');
      WriteLn(fyle);
      for loop := 1 to 3 do
      begin
        WriteLn(fyle,#9 + FloatToStrAny(CRIT[3*loop-2],6) + #9 + FloatToStrAny(CRIT[3*loop-1],6) + #9 + FloatToStrAny(CRIT[3*loop],6));
        WriteLn(fyle);
      end;
      WriteLn(fyle);
      WriteLn(fyle,'THE MINIMUM AIC INDICATES THE "BEST" RESULT');
      WriteLn(fyle,'p AND q OF THE MINIMUM AIC :');
      WriteLn(fyle,'' + FloatToStrAny(CRIT[UserTimeSeriesModel],2));
      WriteLn(fyle);
      WriteLn(fyle,'phi ' + FloatToStrAny(Phi[1,UserTimeSeriesModel],4) + ' ' + FloatToStrAny(Phi[2,UserTimeSeriesModel],4));
      WriteLn(fyle,'theta ' + FloatToStrAny(Theta[1,UserTimeSeriesModel],4) + ' ' + FloatToStrAny(Theta[2,UserTimeSeriesModel],4));
      WriteLn(fyle);
      WriteLn(fyle,'ARMA22 STATISTICS FOR THE NORMALIZED FLOWS ARE :');
      WriteLn(fyle,'phi AND theta :');
      WriteLn(fyle,'AR- AND MA-LAG, p AND q :');
      WriteLn(fyle);
      WriteLn(fyle,'THE CORRELATION AND PARTIAL CORRELATION FUNCTION FOR THE RESIDUALS');
      WriteLn(fyle,'WITH CONFIDENCE INTERVAL ±' + FloatToStrAny(PCONF[UserTimeSeriesModel],3));
      WriteLn(fyle);
      for loop := 1 to NLAG[1] do
      begin
        WriteLn(fyle,Format('%10S',[IntToStr(Loop)]) +
                     Format('%10S',[FloatToStrAny(PCorr[UserTimeSeriesModel,1,Loop],3)]) +
                     Format('%10S',[FloatToStrAny(PCorr[UserTimeSeriesModel,2,Loop],3)]));
      end;
      WriteLn(fyle);
      WriteLn(fyle,'THE SEQUENCE IS OF LENGTH '+IntToStr(RecordLength)+
                   ' WITH A CORRELOGRAM OF LENGTH ' + IntToStr(PLAG[UserTimeSeriesModel]));
      WriteLn(fyle,'IT HAS ' + IntToStr(PFRD[UserTimeSeriesModel]) +
                   ' DEGREES OF FREEDOM AND A PORTMATEAU STATISTIC OF ' + FloatToStrAny(PPORT[UserTimeSeriesModel],2));
      WriteLn(fyle,'THE EXCEEDANCE PROBABILITY OF THIS CHI-SQUARE STATISTIC IS ' + FloatToStrAny(PEXCD[UserTimeSeriesModel],4));
      WriteLn(fyle);
      WriteLn(fyle,'LEVEL CROSSING PROPERTIES');
      WriteLn(fyle);
      WriteLn(fyle,'DOWN CROSSINGS   TOTAL DEFICIT RUN LENGTHS AND SUM');
      WriteLn(fyle,'P(EXCEEDANCE) % ' + Format('%6S',[FloatToStrAny(CRP[1],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[2],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[3],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[4],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[5],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[6],1)]) );
      WriteLn(fyle,'FLOW            ' + Format('%6S',[FloatToStrAny(QI[1],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[2],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[3],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[4],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[5],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[6],1)]) );
      WriteLn(fyle,'CROSSINGS       ' + Format('%6S',[FloatToStrAny(CRNX[1],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[2],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[3],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[4],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[5],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[6],0)]) );
      WriteLn(fyle,'TOTAL RUN LENGTH' + Format('%6S',[FloatToStrAny(CRJX[1],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[2],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[3],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[4],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[5],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[6],0)]) );
      WriteLn(fyle,'TOTAL RUN SUM   ' + Format('%6S',[FloatToStrAny(CRSX[1],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[2],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[3],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[4],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[5],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[6],0)]) );
      WriteLn(fyle);

      WriteLn(fyle,'UP CROSSINGS   TOTAL SURPLUS RUN LENGTHS AND SUM');
      WriteLn(fyle,'P(EXCEEDANCE) % ' + Format('%6S',[FloatToStrAny(CRP[1],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[2],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[3],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[4],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[5],1)]) +
                                        Format('%6S',[FloatToStrAny(CRP[6],1)]) );
      WriteLn(fyle,'FLOW            ' + Format('%6S',[FloatToStrAny(QI[7],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[8],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[9],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[10],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[11],1)]) +
                                        Format('%6S',[FloatToStrAny(QI[12],1)]) );
      WriteLn(fyle,'CROSSINGS       ' + Format('%6S',[FloatToStrAny(CRNX[7],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[8],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[9],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[10],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[11],0)]) +
                                        Format('%6S',[FloatToStrAny(CRNX[12],0)]) );
      WriteLn(fyle,'TOTAL RUN LENGTH' + Format('%6S',[FloatToStrAny(CRJX[7],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[8],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[9],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[10],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[11],0)]) +
                                        Format('%6S',[FloatToStrAny(CRJX[12],0)]) );
      WriteLn(fyle,'TOTAL RUN SUM   ' + Format('%6S',[FloatToStrAny(CRSX[7],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[8],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[9],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[10],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[11],0)]) +
                                        Format('%6S',[FloatToStrAny(CRSX[12],0)]) );
      WriteLn(fyle);

      //statistical itema
      if FStatisticsCalculated then
      begin
        WriteLn(fyle,'HISTORICAL DATA');
        WriteLn(fyle,'AV AND R                    = ' + FloatToStrAny(GJAD[1],2));
        WriteLn(fyle,'STDDEV                      = ' + FloatToStrAny(HSAD[1],2));
        WriteLn(fyle,'SKEWN                       = ' + FloatToStrAny(HSKD[1],2));
        WriteLn(fyle,'AUT K1                      = ' + FloatToStrAny(HJAKD[1],2));
        WriteLn(fyle,'LENGTH OF HISTORICAL RECORD = ' + FloatToStrAny(RecordLength,0));
        WriteLn(fyle);
        WriteLn(fyle,'SUMMARY OF STATISTICS FOR SIMULATION        AVG    STD DEV    SKEWN');
        WriteLn(fyle,'AVERAGE OF ANNUAL STREAM FLOW = ' + Format('%10S',[FloatToStrAny(TSAMD[1],2)]) + Format('%10S',[FloatToStrAny(TSTMD[1],2)]) + Format('%10S',[FloatToStrAny(TSKMD[1],2)]));
        WriteLn(fyle,'STD.DEV. OF ANNUAL FLOWS      = ' + Format('%10S',[FloatToStrAny(TSASD[1],2)]) + Format('%10S',[FloatToStrAny(TSTSD[1],2)]) + Format('%10S',[FloatToStrAny(TSKSD[1],2)]));
        WriteLn(fyle,'CV OF ANNUAL FLOWS            = ' + Format('%10S',[FloatToStrAny(TSCVD[1],2)]) + Format('%10S',[FloatToStrAny(TSTVD[1],2)]) + Format('%10S',[FloatToStrAny(TSKVD[1],2)]));
        WriteLn(fyle);
        WriteLn(fyle,'FIVE FIGURE SUMMARY (BOX PLOT) OF THE SIMULATED SAMPLE DISTRIBUTION OF STATISTICS OF INTEREST');
        WriteLn(fyle);
        WriteLn(fyle,'SAMPLE HISTORICAL VALUES AND SIMULATED DISTRIBUTION OF ANNUAL MEAN (A) AND STANDARD DEVIATION (B)');
        WriteLn(fyle);
        WriteLn(fyle,'                           HISTORICAL   MAXIMUM        95%         75%         50%         25%         5%       MINIMUM');
        WriteLn(fyle,'                             VALUE     SIMULATED                                                               SIMULATED');
        WriteLn(fyle);
        WriteLn(fyle,'(A)                   ' + Format('%12S',[FloatToStrAny(GJAD[1],1)]) +
                               Format('%12S',[FloatToStrAny(PGD[1],1)]) + Format('%12S',[FloatToStrAny(PGD[2],1)]) + Format('%12S',[FloatToStrAny(PGD[3],1)]) +
                               Format('%12S',[FloatToStrAny(PGD[4],1)]) + Format('%12S',[FloatToStrAny(PGD[5],1)]) + Format('%12S',[FloatToStrAny(PGD[6],1)]) +
                               Format('%12S',[FloatToStrAny(PGD[7],1)]));
        WriteLn(fyle,'(B)                   ' + Format('%12S',[FloatToStrAny(HSAD[1],1)]) +
                               Format('%12S',[FloatToStrAny(PSD[1],1)]) + Format('%12S',[FloatToStrAny(PSD[2],1)]) + Format('%12S',[FloatToStrAny(PSD[3],1)]) +
                               Format('%12S',[FloatToStrAny(PSD[4],1)]) + Format('%12S',[FloatToStrAny(PSD[5],1)]) + Format('%12S',[FloatToStrAny(PSD[6],1)]) +
                               Format('%12S',[FloatToStrAny(PSD[7],1)]));
        WriteLn(fyle);

        WriteLn(fyle,'SAMPLE HISTORICAL VALUES AND SIMULATED DISTRIBUTION OF MONTHLY MEANS (A) AND STANDARD DEVIATIONS (B)');
        WriteLn(fyle);
        WriteLn(fyle,'       MONTH               HISTORICAL   MAXIMUM        95%         75%         50%         25%         5%       MINIMUM');
        WriteLn(fyle,'                             VALUE     SIMULATED                                                               SIMULATED');
        WriteLn(fyle);
        for Loop := 1 to 12 do
        begin
          WriteLn(fyle,'(A)     ' + Format('%2S',[IntToStr(Loop)]) + '            ' + Format('%12S',[FloatToStrAny(X1D[Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(PMAD[Loop,1],1)]) + Format('%12S',[FloatToStrAny(PMAD[Loop,2],1)]) + Format('%12S',[FloatToStrAny(PMAD[Loop,3],1)]) +
                                 Format('%12S',[FloatToStrAny(PMAD[Loop,4],1)]) + Format('%12S',[FloatToStrAny(PMAD[Loop,5],1)]) + Format('%12S',[FloatToStrAny(PMAD[Loop,6],1)]) +
                                 Format('%12S',[FloatToStrAny(PMAD[Loop,7],1)]));
          WriteLn(fyle,'(B)     ' + Format('%2S',[IntToStr(Loop)]) + '            ' + Format('%12S',[FloatToStrAny(X2D[Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(PMSD[Loop,1],1)]) + Format('%12S',[FloatToStrAny(PMSD[Loop,2],1)]) + Format('%12S',[FloatToStrAny(PMSD[Loop,3],1)]) +
                                 Format('%12S',[FloatToStrAny(PMSD[Loop,4],1)]) + Format('%12S',[FloatToStrAny(PMSD[Loop,5],1)]) + Format('%12S',[FloatToStrAny(PMSD[Loop,6],1)]) +
                                 Format('%12S',[FloatToStrAny(PMSD[Loop,7],1)]));
          WriteLn(fyle);
        end;//for Loop

        WriteLn(fyle,'SAMPLE HISTORICAL VALUES AND SIMULATED DISTRIBUTION OF MINIMUM N-MONTH RUN SUMS IN ' + IntToStr(LWD[1]) + ' YEARS');
        WriteLn(fyle);
        WriteLn(fyle,'                           HISTORICAL   MAXIMUM        95%         75%         50%         25%         5%       MINIMUM');
        WriteLn(fyle,'                             VALUE     SIMULATED                                                               SIMULATED');
        WriteLn(fyle);
        for Loop := 1 to 10 do
        begin
          WriteLn(fyle,Format('%3S',[IntToStr(LPD[Loop])]) + ' MONTH             ' + Format('%12S',[FloatToStrAny(AMIND[Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(WMIND[Loop,1],0)]) + Format('%12S',[FloatToStrAny(WMIND[Loop,2],0)]) + Format('%12S',[FloatToStrAny(WMIND[Loop,3],0)]) +
                                 Format('%12S',[FloatToStrAny(WMIND[Loop,4],0)]) + Format('%12S',[FloatToStrAny(WMIND[Loop,5],0)]) + Format('%12S',[FloatToStrAny(WMIND[Loop,6],0)]) +
                                 Format('%12S',[FloatToStrAny(WMIND[Loop,7],0)]));
        end;//for Loop
        WriteLn(fyle);

        for Loop := 5 downto 1 do
        begin
          WriteLn(fyle,'SAMPLE HISTORICAL VALUES AND SIMULATED DISTRIBUTION OF DEFICITS AND THEIR RUNS FOR RELEASE = ' + FloatToStrAny(RELD[Loop],0) + '% MAR');
          WriteLn(fyle);
          WriteLn(fyle,'                           HISTORICAL   MAXIMUM        95%         75%         50%         25%         5%       MINIMUM');
          WriteLn(fyle,'                             VALUE     SIMULATED                                                               SIMULATED');
          WriteLn(fyle);
          WriteLn(fyle,'RELEASE PER MONTH     ' + Format('%12S',[FloatToStrAny(QRHD[Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(QRSD[1,Loop],1)]) + Format('%12S',[FloatToStrAny(QRSD[2,Loop],1)]) + Format('%12S',[FloatToStrAny(QRSD[3,Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(QRSD[4,Loop],1)]) + Format('%12S',[FloatToStrAny(QRSD[5,Loop],1)]) + Format('%12S',[FloatToStrAny(QRSD[6,Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(QRSD[7,Loop],1)]));
          WriteLn(fyle,'MAXIMUM DEFICIT       ' + Format('%12S',[FloatToStrAny(RSHD[Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(RSSD[1,Loop],1)]) + Format('%12S',[FloatToStrAny(RSSD[2,Loop],1)]) + Format('%12S',[FloatToStrAny(RSSD[3,Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(RSSD[4,Loop],1)]) + Format('%12S',[FloatToStrAny(RSSD[5,Loop],1)]) + Format('%12S',[FloatToStrAny(RSSD[6,Loop],1)]) +
                                 Format('%12S',[FloatToStrAny(RSSD[7,Loop],1)]));
          WriteLn(fyle,'DURATION OF MAX. DEF. ' + Format('%12S',[FloatToStrAny(RILHD[Loop],0)]) +
                                 Format('%12S',[FloatToStrAny(RILSD[1,Loop],0)]) + Format('%12S',[FloatToStrAny(RILSD[2,Loop],0)]) + Format('%12S',[FloatToStrAny(RILSD[3,Loop],0)]) +
                                 Format('%12S',[FloatToStrAny(RILSD[4,Loop],0)]) + Format('%12S',[FloatToStrAny(RILSD[5,Loop],0)]) + Format('%12S',[FloatToStrAny(RILSD[6,Loop],0)]) +
                                 Format('%12S',[FloatToStrAny(RILSD[7,Loop],0)]));
          WriteLn(fyle,'LONGEST DEPLETION     ' + Format('%12S',[FloatToStrAny(RJLHD[Loop],0)]) +
                                 Format('%12S',[FloatToStrAny(RJLSD[1,Loop],0)]) + Format('%12S',[FloatToStrAny(RJLSD[2,Loop],0)]) + Format('%12S',[FloatToStrAny(RJLSD[3,Loop],0)]) +
                                 Format('%12S',[FloatToStrAny(RJLSD[4,Loop],0)]) + Format('%12S',[FloatToStrAny(RJLSD[5,Loop],0)]) + Format('%12S',[FloatToStrAny(RJLSD[6,Loop],0)]) +
                                 Format('%12S',[FloatToStrAny(RJLSD[7,Loop],0)]));
          WriteLn(fyle);
        end;//for Loop

        //Need to write out the stuff from graph 10 (the generated flows)

        //Need to write out the stuff from the gauge comparisons

        if FlowCorrelationData.First then
        begin
          repeat
            WriteLn(fyle);
            WriteLn(fyle,'MONTHLY COMPARISON OF GAUGES');
            WriteLn(fyle,FileName + ' TO ' + FlowCorrelationData.CurrentData.FileName);
            WriteLn(fyle,'USING THE SPEARMAN RANK COEFFICIENT');
            WriteLn(fyle,'SAMPLE HISTORICAL VALUES AND SIMULATED DISTRIBUTIONS');
            WriteLn(fyle);
            WriteLn(fyle,'               HISTORICAL   MEAN      MAXIMUM        95%         75%         50%         25%         5%       MINIMUM');
            WriteLn(fyle,'                 VALUE      VALUE    SIMULATED                                                               SIMULATED');
            WriteLn(fyle);

            for Loop := 1 to 13 do
            begin
              WriteLn(fyle,Format('%10S',[TheMonths[Loop]]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.RSHO[Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.RMEANO[Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.PO[1,Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.PO[2,Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.PO[3,Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.PO[4,Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.PO[5,Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.PO[6,Loop],5)]) +
                           Format('%12S',[FloatToStrAny(FlowCorrelationData.CurrentData.PO[7,Loop],5)]));
            end;

          until NOT(FlowCorrelationData.Next);
        end;



      end;//if StatisticsCalculated
    end;//with CurrentRecord

    CloseFile(fyle);

    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.SavePARAMFile(FileName : string) : boolean;
const OPNAME = 'TStomsaData.SavePARAMFile';
var
  fyle : TextFile;
  PARAMData : TStringList;
  LIndex : integer;
begin
  Result := False;
  try
    try
      AssignFile(fyle,FileName);
      Rewrite(fyle);
      PARAMData := TStringList.Create;
      PARAMData.Clear;
      PARAMData := GeneratePARAMFile;
      for LIndex := 0 to PARAMData.Count - 1 do
        Writeln(fyle,PARAMData.Strings[LIndex]);
      Close(fyle);

      FParamFileName := FileName;
      CastFileNamesObject.PopulateHydrologyPaths(ExtractFilePath(FParamFileName));
      CastFileNamesObject.UpdateParamFileName(00,FParamFileName,False,0.0,FileLastWriteDate(FParamFileName));
      Result := True;
    except on E: EInOutError do
    begin
      MessageDlg('Unable to save PARAM file '+FileName,mtError,[mbOk],0);
      raise;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//FUNCTION SavePARAMFile

function TStomsaData.OpenPARAMFile(FileName : string) : boolean;
const OPNAME = 'TStomsaData.OpenPARAMFile';
var
  fyle : TextFile;
begin
  Result := true;
  try
    PARAMFileOpened := false;
    try
    AssignFile(fyle,FileName);
    Reset(fyle);
    //Load up all the PARAM file data necessary to perform the checks
    CloseFile(fyle);
    PARAMFileOpened := true;
    except
      on E: EInOutError do
      begin
        result := false;
        MessageDlg('Unable to open PARAM file '+FileName,mtError,[mbOk],0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.OpenProject(FileName : string) : boolean;
const OPNAME = 'TStomsaData.OpenProject';
begin
  Result := False;
  try
    RefreshInc;
    Initialise;
    Result := LoadProjectData(FileName);
    if Result then
      FProjectFileName := FileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE OpenProject

function TStomsaData.MergeProject(FileName : string) : boolean;
const OPNAME = 'TStomsaData.MergeProject';
begin
  Result := False;
  try
    Result := LoadProjectData(FileName);
    if Result then
      FProjectFileName := FileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE MergeProject

function TStomsaData.Get_CurrentIncData: TIncData;
const OPNAME = 'TStomsaData.Get_CurrentIncData';
begin
  Result := nil;
  try
    Result := FindIncFileByIndex(FCurrentIncPosition);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.First: boolean;
const OPNAME = 'TStomsaData.First';
//Move to top of list, return Nil pointer if empty list
begin
  Result := false;
  try
    FCurrentIncPosition := -1;
    if (FIncDataList.Count > 0) then
    begin
      FCurrentIncPosition := 0;
      Result              := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Next: boolean;
const OPNAME = 'TStomsaData.Next';
//Move to next position in list, if at END then return a Nil pointer
begin
  Result := false;
  try
    if (FIncDataList.Count > 0) and ((FCurrentIncPosition + 1) < FIncDataList.Count) then
    begin
      FCurrentIncPosition := FCurrentIncPosition + 1;
      Result     := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Last: boolean;
const OPNAME = 'TStomsaData.Last';
//Move to last item in list, return Nil if empty list
begin
  Result := false;
  try
    FCurrentIncPosition := -1;
    if (FIncDataList.Count > 0) then
    begin
      FCurrentIncPosition := FIncDataList.Count-1;
      Result              := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.GotoIndex(APosition : integer): boolean;
const OPNAME = 'TStomsaData.GotoIndex';
//Move to specified position in list with 1 = first item, return Nil pointer if position undefined
begin
  Result := false;
  try
    APosition := APosition -1;
    if(APosition >= 0) and (APosition < FIncDataList.Count) then
    begin
      FCurrentIncPosition := APosition;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// ————————— Property Write Methods —————————

procedure TStomsaData.SetPARAMFileCreated(Status : Boolean);
const OPNAME = 'TStomsaData.SetPARAMFileCreated';
begin
  try
    if FPARAMFileCreated <> Status then
    begin
      FPARAMFileCreated := Status;
      if FPARAMFileCreated then
        if assigned(FOnPARAMFileCreated) then FOnPARAMFileCreated(self)
      else
        if assigned(FOnPARAMFileDestroyed) then FOnPARAMFileDestroyed(self);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.GetViewDataItems(AViewId: string;AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TStomsaData.GetViewDataItems';
begin
  Result := False;
  try
   AHandled := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.CheckHydrologyFilesPaths: boolean;
const OPNAME = 'TStomsaData.CheckHydrologyFilesPaths';
      Mesg   = 'INC files needs to be copied to PARAM file directory before being imported. Continue?';
var
  LIndex : integer;
  LFileName,
  LFilePath,
  LParamFilePath: string;
  LFileList : TstringList;
  LMustCopy : boolean;
begin
  Result := False;
  try
    if not FileExists(FParamFileName) then Exit;

    LParamFilePath :=  ExtractFilePath(FParamFileName);
    CastFileNamesObject.PopulateHydrologyPaths(LParamFilePath);
    CastFileNamesObject.UpdateParamFileName(00,FParamFileName,False,0.0,FileLastWriteDate(FParamFileName));

    LFileList := TstringList.Create;
    try
      if Self.First then
      begin
        LFileName := Self.CurrentRecord.Directory + Self.CurrentRecord.Filename;
        LFileList.Add(LFileName);
      end;

      while Self.Next do
      begin
        LFileName := Self.CurrentRecord.Directory + Self.CurrentRecord.Filename;
        LFileList.Add(LFileName);
      end;

      LMustCopy      := False;
      for LIndex := 0 to LFileList.Count -1 do
      begin
        LFilePath := Uppercase(ExtractFilePath(LFileList[LIndex]));
        if(LFilePath <> Uppercase(LParamFilePath)) then
        begin
          LMustCopy := True;
          Break;
        end;
      end;

      if LMustCopy then
      begin
        if (MessageDlg(Mesg,mtConfirmation,mbOKCancel,0) = mrCancel) then Exit;

        for LIndex := 0 to LFileList.Count -1 do
        begin
          LFilePath := ExtractFilePath(LFileList[LIndex]);
          if(Uppercase(LFilePath) <> Uppercase(LParamFilePath)) then
          begin
            LFileName := LParamFilePath + ExtractFileName(LFileList[LIndex]);
            if not FileExists(LFileName) then
            begin
              CopyFile(PChar(LFileList[LIndex]),PChar(LFileName),True);
            end;
            LFileList[LIndex] := LFileName;
          end;
        end;
      end;

      CastFileNamesObject.CastHydrologyFileNames.Clear;
      for LIndex := 0 to LFileList.Count -1 do
      begin
        LFileName := LFileList[LIndex];
        CastFileNamesObject.AddHydrologyFileName(LIndex,LFileName,False,0.0,FileLastWriteDate(LFileName));
      end;
    finally
      LFileList.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{procedure TStomsaData.UpdateCurrentRecord(AKeyGauge, AMarginalFitted, ATimeSeriesFitted, AStatisticsCalculated: boolean);
const OPNAME = 'TStomsaData.UpdateCurrentRecord';
begin
  try
    //check if value of the fitted flag has changed, if so then raise the relevant event
    if AKeyGauge <> CurrentRecord.FKeyGauge then
    begin
      if AKeyGauge then
        Inc(FKeyGaugeCount)
      else
        Dec(FKeyGaugeCount);
      CurrentRecord.FKeyGauge := AKeyGauge;
      if Assigned(FOnKeyGaugeChange) then
        FOnKeyGaugeChange(Self,CurrentRecord.FileName,CurrentRecord.Directory);
    end;

    if AMarginalFitted <> CurrentRecord.FMarginalFitted then
    begin
      if AMarginalFitted then
      begin
        Inc(FMarginalFittedCount);
        CurrentRecord.FMarginalFitted := AMarginalFitted;
        if Assigned(FOnMarginalFitted) then
          FOnMarginalFitted(Self,CurrentRecord.FileName,CurrentRecord.Directory);
      end
      else
      begin
        Dec(FMarginalFittedCount);
        CurrentRecord.FMarginalFitted := AMarginalFitted;
        if Assigned(FOnMarginalUnfitted) then
          FOnMarginalUnfitted(Self,CurrentRecord.FileName,CurrentRecord.Directory);
      end;
    end;

    if ATimeSeriesFitted <> CurrentRecord.FTimeSeriesFitted then
    begin
      if ATimeSeriesFitted then
      begin
        Inc(FTimeSeriesFittedCount);
        CurrentRecord.FTimeSeriesFitted := ATimeSeriesFitted;
        if Assigned(FOnTimeSeriesFitted) then
          FOnTimeSeriesFitted(Self,CurrentRecord.FileName,CurrentRecord.Directory);
      end
      else
      begin
        Dec(FTimeSeriesFittedCount);
        CurrentRecord.FTimeSeriesFitted := ATimeSeriesFitted;
        if Assigned(FOnTimeSeriesUnfitted) then
          FOnTimeSeriesUnfitted(Self,CurrentRecord.FileName,CurrentRecord.Directory);
      end;
    end;

    if (CurrentRecord.FMarginalFitted = false) and (CurrentRecord.FTimeSeriesFitted = true) then
    begin
      CurrentRecord.FTimeSeriesFitted := False;
      Dec(FTimeSeriesFittedCount);
      if Assigned(FOnTimeSeriesUnfitted) then
        FOnTimeSeriesUnfitted(Self,CurrentRecord.FileName,CurrentRecord.Directory);
    end;

    if AStatisticsCalculated <> CurrentRecord.FStatisticsCalculated then
    begin
      if AStatisticsCalculated then
      begin
        inc(FStatisticsCalculatedCount);
        CurrentRecord.FStatisticsCalculated := AStatisticsCalculated;
        if Assigned(FOnFlowsAndStatsGenerated) then
          FOnFlowsAndStatsGenerated(Self)
      end
      else
      begin
        dec(FStatisticsCalculatedCount);
        CurrentRecord.FStatisticsCalculated := AStatisticsCalculated;
        if Assigned(FOnFlowsAndStatsGenerated) then
          FOnFlowsAndStatsGenerated(Self)
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

{ TIncData }

procedure TIncData.Set_KeyGauge(AKeyGauge: boolean);
const OPNAME = 'TIncData.Set_KeyGauge';
begin
  try
    if AKeyGauge <> FKeyGauge then
    begin
      FKeyGauge := AKeyGauge;
      if Assigned(FOnKeyGaugeChange) then
        FOnKeyGaugeChange(Self,FileName,Directory);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIncData.Set_MarginalFitted(AMarginalFitted: boolean);
const OPNAME = 'TIncData.Set_MarginalFitted';
begin
  try
    if AMarginalFitted <> FMarginalFitted then
    begin
      FMarginalFitted := AMarginalFitted;
      if FMarginalFitted then
      begin
        if Assigned(FOnMarginalFitted) then
          FOnMarginalFitted(Self,FileName,Directory);
      end
      else
      begin
        if Assigned(FOnMarginalUnfitted) then
          FOnMarginalUnfitted(Self,FileName,Directory);
      end;
    end;

    if (not FMarginalFitted ) and FTimeSeriesFitted then
    begin
      FTimeSeriesFitted := False;
      if Assigned(FOnTimeSeriesUnfitted) then
        FOnTimeSeriesUnfitted(Self,FileName,Directory);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIncData.Set_TimeSeriesFitted(ATimeSeriesFitted: boolean);
const OPNAME = 'TIncData.Set_TimeSeriesFitted';
begin
  try
    if ATimeSeriesFitted <> FTimeSeriesFitted then
    begin
      FTimeSeriesFitted := ATimeSeriesFitted;
      if FTimeSeriesFitted then
      begin
        if Assigned(FOnTimeSeriesFitted) then
          FOnTimeSeriesFitted(Self,FileName,Directory);
      end
      else
      begin
        if Assigned(FOnTimeSeriesUnfitted) then
          FOnTimeSeriesUnfitted(Self,FileName,Directory);
      end;
    end;

    if (not FMarginalFitted ) and FTimeSeriesFitted then
    begin
      FTimeSeriesFitted := False;
      if Assigned(FOnTimeSeriesUnfitted) then
        FOnTimeSeriesUnfitted(Self,FileName,Directory);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIncData.Set_StatisticsCalculated(AStatisticsCalculated: boolean);
const OPNAME = 'TIncData.Set_StatisticsCalculated';
begin
  try
    if AStatisticsCalculated <> FStatisticsCalculated then
    begin
      FStatisticsCalculated := AStatisticsCalculated;
      if FStatisticsCalculated then
      begin
        if Assigned(FOnFlowsAndStatsGenerated) then
          FOnFlowsAndStatsGenerated(Self)
      end
      else
      begin
        if Assigned(FOnFlowsAndStatsGenerated) then
          FOnFlowsAndStatsGenerated(Self)
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Get_IncFileCount: integer;
const OPNAME = 'TStomsaData.Get_IncFileCount';
begin
  Result := 0;
  try
    Result :=  FIncDataList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Get_KeyGaugeCount: integer;
const OPNAME = 'TStomsaData.Get_KeyGaugeCount';
var
  LIndex : integer;
  LTempIncFile : TIncData;
begin
  Result := 0;
  try
    for LIndex := 0 to FIncDataList.Count-1 do
    begin
      LTempIncFile := IncFileByIndex[LIndex];
      if LTempIncFile.KeyGauge then
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Get_MarginalFittedCount: integer;
const OPNAME = 'TStomsaData.Get_MarginalFittedCount';
var
  LIndex : integer;
  LTempIncFile : TIncData;
begin
  Result := 0;
  try
    for LIndex := 0 to FIncDataList.Count-1 do
    begin
      LTempIncFile := IncFileByIndex[LIndex];
      if LTempIncFile.MarginalFitted then
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Get_StatisticsCalculatedCount: integer;
const OPNAME = 'TStomsaData.Get_StatisticsCalculatedCount';
var
  LIndex : integer;
  LTempIncFile : TIncData;
begin
  Result := 0;
  try
    for LIndex := 0 to FIncDataList.Count-1 do
    begin
      LTempIncFile := IncFileByIndex[LIndex];
      if LTempIncFile.StatisticsCalculated then
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Get_TimeSeriesFittedCount: integer;
const OPNAME = 'TStomsaData.Get_TimeSeriesFittedCount';
var
  LIndex : integer;
  LTempIncFile : TIncData;
begin
  Result := 0;
  try
    for LIndex := 0 to FIncDataList.Count-1 do
    begin
      LTempIncFile := IncFileByIndex[LIndex];
      if LTempIncFile.TimeSeriesFitted then
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.Get_IncFileByIndex(AIndex : Integer): TIncData;
const OPNAME = 'TStomsaData.Get_IncFileByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FIncDataList.Count) then
       Result :=  TIncData(FIncDataList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaData.GetCastFileNamesObject: TModelFileNames;
const OPNAME = 'TStomsaData.GetCastFileNamesObject';
begin
  Result := nil;
  try
    Result := TModelFileNames(FFileNamesObject);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
