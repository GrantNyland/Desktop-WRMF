//
//
//  UNIT      : Contains TAbstractObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAbstractObject;

interface

uses
  Classes,
  VCL.Forms,
  Data.DB,
  VCL.Menus,
  VCL.ComCtrls,
  Contnrs,
  VCL.ExtCtrls,
  VoaimsCom_TLB,
  UDWADBComponents,
  USystemModelLinkClasses,
  UStringListOfStringLists;
const
  CSystem            = 'System';
  CYield             = 'Yield';
  CPlanning          = 'Planning';
  CHydrology         = 'Hydrology';
  CRainfall          = 'Rainfall';
  CPreProcessor      = 'BSPPS';//Broad Scale PreProcessor';
  CDailyDiversion    = 'DailyDiversion';
  CIFRPreProcessor   = 'IFRPreProcessor';
  CDamSedimentation  = 'DamSedimentation';
  CYRC               = 'YRC Model';
  CProjectGauges     = 'Project Gauges';
  CDamLevels         = 'Dam Levels';
  CStomsa            = 'Stomsa';
  CRWH               = 'RWH';
  CDDTS              = 'DDTS';
  CWQT               = 'WQT';
  //DWAF_ALL_MODELS    = 'BSPPS,PLANN,RAINF,YIELD,WSAM,WQ200' ;    Stomsa,BSPPS, ,DDTS,DamSedimentation
  DWAF_ALL_MODELS    = 'Yield,Planning,Hydrology,Rainfall,DailyDiversion,IFRPreProcessor,Stomsa,RWH';

  CTStringsSeparator = '(XZXZ!~ZXZX';

  ReservoirsSet                  = [ntReservoir,ntWetlandNode,ntMinePolutionControlDam,ntMineUndergroundDam,ntGroundWater];
  ReservoirsAndNodeWithInflowSet = [ntReservoir,ntNodeWithInflow,ntWetlandNode,ntMinePolutionControlDam,ntMineUndergroundDam,ntGroundWater];
  NodesSet                       = [ntNodeWithInflow,ntNodeWithoutInflow,ntIrrigationNode,ntDemandCentreNode,ntMineNode,ntAbstractionNode,ntCollectionNode,ntBaseFlowNode];
  NodesWithoutInflowSet          = [ntNodeWithoutInflow,ntIrrigationNode,ntDemandCentreNode,ntMineNode,ntAbstractionNode,ntCollectionNode,ntBaseFlowNode];
  NodeWithInflowAndReservoirSet  = [ntReservoir,ntNodeWithInflow];
var
  HImagesInstance: LongWord; // Contains the instance handle of the images resource DLL.

type
  TSortOrder   = (soAscending,soDescending);
  TModelDataChangeLevel   = (mdclAll,mdclStructure,mdclValue);
  TModelDataChangeType    = (mdctAll,mdctChannel,mdctNode,mdctReservior,mdctPenalty);
  TModelDataChangeAction  = (mdcaAdd,mdcaUpdate,mdcaDelete);
  TModelActionLevel = (malNone,malStudy,malModel,malSubArea,malScenarion);
  TViewMode = (vmNone,vmEditable,vmSelect,vmEditableSelect);
  TFieldGroup = (fgNone, fgArray, fgMinMax, fgMinMaxArray, fgValidation,
                 fgValidationArray, fgMinMaxString,fgMinMaxValidation,fgMinMaxValidationArray);
  //TSummaryOutputDataSourceName = (sodsNone,sodsSumFile,sodsDatabase,sodsBlobFile,sodsBlobDatabase,sodsPltFile);
  TSummaryOutputDataSourceName = (sodsNone,sodsSumFile,sodsPltFile);
  TProgressType = (ptError, ptWarning, ptNone);
  TDailyFileType = (dftRunoff,dftOtherInflow,dftIncreamentalRunoff,dftEWR,dftRainfall,dftEvaporation);

  TOnFileSave = function: boolean of object;

  //Channel Array types start
  TArrayOfString = array of string;
  TChannelPenaltyValuesArray    = array of double;
  TMasterMonthlyDoublesArray    = array of double;
  TMinMonthlyDoublesArray       = array of double;
  TLossMonthlyDoublesArray      = array of double;
  TMinMaxMonthlyDoublesArray    = array of array of double;
  TDivMonthlyDoublesArray       = array of double;
  TReservoirPenaltyValuesArray  = array of double;
  TReservoirElevationsArray     = array of double;
  TReferenceFlowsArray          = array of double;
  TDivertedFlowProportionsArray = array of array of double;
  TIFRArray                     = array of array of double;
  TExceedencePercentagesArray   = array of double;
  TConstraintArray              = array of double;
  TConstraintType11Array        = array of array of double;
  TIrrMonthlyDoublesArray       = array of double;
  TIrrMonthlyIntegerArray       = array of Integer;
  TPowMonthlyDoublesArray       = array of double;
  TEfficiencyFactorsArray       = array of double;
  TNetHeadFactorsArray          = array of double;
  TElevationsArray              = array of double;
  TDischargesArray              = array of double;
  TWaterDemandPortion           = array of double;
  //Channel Array types end
  //RunConfigurationData Array types start
  TMonthNamesArray            = array of String;
  TMonthDaysArray             = array of double;
  TTargetYieldArray           = array of double;
  TMaximumYieldArray          = array of double;
  TTargetPowerArray           = array of double;
  TSequencesToBeAnalysedArray = array of integer;
  TDecisionMonthArray         = array of integer;
  TDecisionTypeArray          = array of string;
  THydroPowerIndicatorArray   = array of string;
  TOneDimensionDoubleArray    = array of double;
  TTwoDimensionDoubleArray    = array of array of double;
  TThreeDimensionDoubleArray  = array of array of array of double;
  TOneDimensionIntegerArray   = array of Integer;
  TTwoDimensionIntegerArray   = array of array of Integer;
  TThreeDimensionIntegerArray = array of array of array of Integer;

  //_________________YRC types ________________________________________
  TChartSeriesType  = (cstLabelSeries, cstLineSeries, cstLineSpecialTicsSeries);
  TChartMode        = (cmPlane,cmView,cmManipulating);
  TChartEditMode    = (tdmNone,tdmDeterministic,tdmRegression);
  TShowTargetDrafts = (stdAll, stdAdjacent,stdSelected);
  TPenWidth         = (pwThin,pwThick);
  TCurveGroupType   = (ctPureRegression,ctRegression,ctDeterministic,ctOriginal);

  //_________________Output Review types _______________________________
  TIntegerArray = array of integer;
  TMonthyIntegerArray = array [0..12] of integer;
  TMonthlyDoubleArray = array[1..12] of double;
  TFifteenDoubleArray = array [1..15] of double;
  TEightDoubleArray  = array [1..8] of double;

  TStartMonthArray = array [1..10] of integer;
  TStorageVolumesArray = array [1..10] of double;
  TAllocationFactorsArray = array [1..10] of double;

  TYRCPoint = record
    YValue: double;
    XValue: double;
    XTValue: double;//X value transformed to the plotting base;
  end;
  TAssuranceIntervalArray  = array of integer;
  TYRCFlatPointArray = array of double;
  TYRCConstantsArray = array [0..3] of double;
  TYRCFourPointsArray = array [0..3] of TYRCPoint;
  TYRCRecordPointArray = array of TYRCPoint;
  //______________________________________________________________________________________

  //RunConfigurationData Array types end

  // Used in IGeneralFlowChannel
   TArcCountSet = Set of 1..5;

  TDBProgressUpdate = procedure (AProgress: String; ACurrentPos,AMaxPos: integer) of object;
  TFieldUpdateFunction = function (AFieldName: String; ANewValue: String; AOldValue: String;
                                   AContextData: TStrings): Boolean of object;
  TFieldEditFunction = function (AFieldName, ADefaultValue: string;
                                 AContextData: TStrings): boolean of object;
  TMenuSetAction = (msShow, msHide, msDisable, msEnable, msChecked, msUnChecked, msDelete);
  TSetOfMenuAction = set of TMenuSetAction;
  TModelManagerType = (mmModelUnchanged, mmModelSystem, mmModelYield,mmModelPlanning, mmModelRainfallData,
                       mmModelHydrology, mmModelYRC, mmBroadScalePreprocessor,mmDailyDiversionPreprocessor,
                       mmIFRPreprocessor,mmStomsa,mmRWH,mmDDTS,mmDamSedimentation);
  TParamType = (prAll,prString,prInt,prFloat,prChar);
  TFileType = (ftInput,ftOutput);
  TStudyAreaType = (prjYield, prjPlanning);
  TModelType = (mdlYield, mdlPlanning);
  TUserType = (utStandard = 1, utExpert = 2);
  TFilesReadArray = array [0..15] of boolean;
  TFileNamesArray = array [0..15] of String;
  TXValuesArray = array [0..15] of double;
  TYValuesArray = array [0..15] of double;
  TOutputFileType = (oftNone,oftYield,oftSum,oftData,oftDebug,oftPlot,oftHydroPower,oftDemand,oftPmp);
  TProgressUpdateFuntion = procedure (AProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean=False) of object;
  TExecFunction = function (AProgressUpdateFuntion: TProgressUpdateFuntion):boolean of object;
  TExecFunctionArray = array [1..10] of TExecFunction;
  TCustomFieldEditArray = record
                            FFieldName      : string;
                            FUpdateFunction : TFieldUpdateFunction;
                            FEditFunction   : TFieldEditFunction;
                          end;
  {$Z4}
    TLoadModelMessage = packed record
      Msg: Cardinal;
      ModelManagerType: TModelManagerType;
      Unused: Longint;
      Result: Longint;
    end;
  {$Z1}

  TAbstractObject = class(TInterfacedObject)
  protected
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AfterCreateMemberObjects; virtual;
    procedure BeforeDestroyMemberObjects; virtual;
    procedure DummyShowProgress(AProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean=False); virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TAbstractModelDataset = class; // Forward declaration

  TReportTypes = (rtNotSupported, rtAcrobat, rtWord);
  TStudyDocumentDetail = class(TObject)
  protected
    FCategoryKey: string;
    FIdentifierKey: string;
    FFilename: string;
    FMenuCaption: string;
    FBookMark: string;
    FPageNumber: integer;
  public
    procedure Reset;
    procedure AssignFrom(AObject: TObject);
    function ReportType: TReportTypes;
    property CategoryKey: string    read FCategoryKey    write FCategoryKey;
    property IdentifierKey: string  read FIdentifierKey  write FIdentifierKey;
    property Filename: string       read FFilename       write FFilename;
    property MenuCaption: string    read FMenuCaption    write FMenuCaption;
    property BookMark: string       read FBookMark       write FBookMark;
    property PageNumber: integer    read FPageNumber     write FPageNumber;
  end;

  TAbstractStudyDocumentList = class(TStringList)
  protected
    function GetDocumentDetail(AContext: string): TStudyDocumentDetail; virtual; abstract;
  public
    function IsDocumentSet(AContext: string): boolean; virtual; abstract;
    property DocumentDetail[AContext: string]: TStudyDocumentDetail read GetDocumentDetail;
  end;

  TAbstractConfiguration = class(TAbstractObject)
  public
    function FileName: string; virtual;abstract;
    function ReadInteger(const ASection, AIdent: string; const ADefault: integer): integer; virtual;abstract;
    function ReadString(const ASection, AIdent, ADefault: string): string; virtual;abstract;
    function ReadSectionValues(const Section: string; Strings: TStrings):boolean; virtual;abstract;
    function ReadSection(const Section: string; Strings: TStrings):boolean; virtual;abstract;
    procedure WriteInteger(const ASection, AIdent: string; const AValue: integer); virtual;abstract;
    procedure WriteString(const ASection, AIdent, AValue: string); virtual;abstract;
    procedure DeleteKey(const ASection, AIdent: String); virtual; abstract;
    procedure LoadFormView(AForm: TObject); virtual; abstract;
    procedure SaveFormView(AForm: TObject); virtual; abstract;
    procedure ResetToDefaults; virtual; abstract;
  end;

  TAbstractHydrologyFileType = class(TAbstractObject)
  public
    function GetFileType(AHydrologyFileExt: string): integer; virtual;abstract;
  end;

  TAbstractGlobalData = class(TAbstractObject)
  protected
  public
    function ApplicationName: string; virtual;abstract;
    function SetApplicationName(AApplicationName: string): boolean; virtual;abstract;
    function ApplicationVersion: string; virtual;abstract;
    function SetApplicationVersion(AApplicationVersion: string): boolean; virtual;abstract;
    function DatabaseName: TDWAConnection; virtual;abstract;
    function SetDatabaseName(ADatabaseName: TDWAConnection): boolean; virtual;abstract;
    function StopOnFirstErr: boolean; virtual;abstract;
    function SetStopOnFirstErr(AStopOnFirstErr: boolean): boolean; virtual;abstract;
    function IncludeHydrologyFiles: boolean; virtual;abstract;
    function SetIncludeHydrologyFiles(AInclude: boolean): boolean; virtual;abstract;
    function IncludeDemandFiles: boolean; virtual;abstract;
    function SetIncludeDemandFiles(AInclude: boolean): boolean; virtual;abstract;
    function COMServerMode: boolean; virtual;abstract;
    function SetCOMServerMode(AMode: boolean): boolean; virtual;abstract;
    function AutoSelectStudy: boolean; virtual;abstract;
    function SetAutoSelectStudy(AAutoSelectStudy: boolean): boolean; virtual;abstract;
    function AutoSelectUser: boolean; virtual;abstract;
    function SetAutoSelectUser(AAutoSelectUser: boolean): boolean; virtual;abstract;
    procedure SetLastError(AErrorCode: integer); virtual;abstract;
    function GetLastError: integer; virtual;abstract;
    procedure SetLastErrorMessage(AErrorMessage: string); virtual;abstract;
    function GetLastErrorMessage: string; virtual;abstract;
    function SQLReservedWordsInTablesCommaText: string; virtual;abstract;
  end;

  // Put new abstract class here.
  TAbstractTableProperty = class(TAbstractObject)
  protected
    FTableName: string;
    FPrimaryKeys: TStrings;
  public
    property TableName: string read FTableName;
    property PrimaryKeys: TStrings read FPrimaryKeys;
  end;
  TAbstractFieldProperty = class;
  TAbstractFieldUpdateSQLStep = class(TAbstractObject)
  protected
    FFieldProperty: TAbstractFieldProperty;
    FTableProperty: TAbstractTableProperty;
    FStepNo: integer;
    FFieldInTable: string;
    FUpdateSQL: string;
    FGetValueSQL: string;
  public
    property FieldProperty: TAbstractFieldProperty read FFieldProperty;
    property TableProperty: TAbstractTableProperty read FTableProperty;
    property StepNo: integer read FStepNo;
    property FieldInTable: string read FFieldInTable;
    property UpdateSQL: string read FUpdateSQL;
    property GetValueSQL: string read FGetValueSQL;
  end;

  TStudyDataHasChangedAction = (saNone,saAdd, saDelete, saUpdate);
  TStudyDataHasChangedSeverity = (scsNone, scsMajor, scsMinor);
  TStudyDataHasChangedData = class(TAbstractObject)
  protected
    FFieldProperty : TAbstractFieldProperty;
    FAction        : TStudyDataHasChangedAction;
    FSeverity      : TStudyDataHasChangedSeverity;
    FOldValue      : string;
    FNewValue      : string;
    FChangedBy     : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write FFieldProperty;
    property Action: TStudyDataHasChangedAction read FAction write FAction;
    property Severity: TStudyDataHasChangedSeverity read FSeverity write FSeverity;
    property OldValue: string read FOldValue write FOldValue;
    property NewValue: string read FNewValue write FNewValue;
    property ChangedBy: string read FChangedBy write FChangedBy;
  end;

  TAbstractMainFormManager = class;
  TAbstractViewDataList = class;
  TAbstractAccessControlManager = class;
  TAbstractStudyDocumentManager = class;
  TAbstractLanguage = class;
  TAbstractFieldsProperties = class;
  TAbstractDatabaseLayer = class;
  TAbstractModelManager = class;
  TAbstractPrintManager = class;
  TAbstractChangeManager = class;
  TAbstractMetaDataManager = class;
  TAbstractWeatherEventsManager = class;
  TAbstractScenarioLockManager = class;
  //TAbstractLicenceManager = class;
  TAbstractUser = class;
  TAbstractStudyArea = class;
  TAbstractStudyAreaManager = class;
  TAbstractDBTablePropertyManager = class;
  TAbstractTableFieldsDefList = class;

  TAppModules = class(TAbstractObject)
  public
    // Abstract methods that every application must provide an implementation for.
    function IniFile : TAbstractConfiguration; virtual; abstract;
    function ViewIni : TAbstractConfiguration; virtual; abstract;
    function Database : TAbstractDatabaseLayer; virtual; abstract;

    // Abstract function that return objects.
    function Language : TAbstractLanguage; virtual;
    function HydrologyFileType: TAbstractHydrologyFileType; virtual;
    function User: TAbstractUser; virtual;
    function AccessControlManager: TAbstractAccessControlManager; virtual;
    function Model: TAbstractModelManager; virtual;
    function MainForm: TAbstractMainFormManager; virtual;
    function FieldProperties : TAbstractFieldsProperties; virtual;
    function StudyDocumentManager: TAbstractStudyDocumentManager; virtual;
    function StudyArea : TAbstractStudyArea; virtual;
    function GlobalData : TAbstractGlobalData; virtual;
    function PrintManager : TAbstractPrintManager; virtual;
    function Changes : TAbstractChangeManager; virtual;
    function MetaData : TAbstractMetaDataManager; virtual;
    function WeatherEvents : TAbstractWeatherEventsManager; virtual;
    function ScenarioLockManager:TAbstractScenarioLockManager; virtual;
    //function LicenceManager:TAbstractLicenceManager; virtual;
    function StudyAreaManager:TAbstractStudyAreaManager; virtual;
    function DBTablePropertyManager:TAbstractDBTablePropertyManager; virtual;
    function DBTableFieldsDefList:TAbstractTableFieldsDefList; virtual;

    // Virtual functions that certain applications implement.
    procedure DoLogOn; virtual;
    procedure DoLogOff; virtual;
    procedure DoAbout; virtual;
    procedure DoUserAdministration; virtual;
    procedure SetStudyArea(ANewStudyArea: TAbstractStudyArea); virtual;
    procedure SelectStudyArea; virtual;
    procedure SelectStudyDetails(AData: TObject); virtual;
    function CanApplicationClose : boolean; virtual;
    procedure ApplicationIsClosing; virtual;
    function ViewData : TAbstractViewDataList; virtual;
    function AddMenuItem(AMainMenuKeys: array of string; ASortWeight: integer; AEvent: integer = 0; AData: TObject = nil): TObject; virtual;
    function SetMenuItem(AMainMenuKeys: array of string; AnAction: TMenuSetAction; AStatusReason: string = ''): boolean; virtual;
    function SetMenuItemCaption(AMainMenuKeys: array of string; ACaption : string): boolean; virtual;
    function GetMenuItemProperties(AMenuKeys: array of string) : TSetOfMenuAction; virtual;
    function LoadModel(AModel: TModelManagerType): boolean; virtual;
    function GetExportFilename(const ADefaultExt, AFilter: string; var AFileName: string): boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; virtual; abstract;
    function GetModelViewItems(AModelName: string;AItems: TStringListOfStringLists): boolean; virtual;

  end;

  ILanguageEventsInterface = interface
    function LanguageHasChanged: boolean;
  end;

  TChangeContext = (sdccEdit,sdccDelete,sdccAdd,sdccImport,sdccExport,sdccClearData,sdccSaveData,sdccSelect);
  IAppEventsInterface = interface(ILanguageEventsInterface)
    function StudyHasChanged: boolean;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
  end;

  TAbstractAppObjectList = class;
  TAbstractAppObject = class(TAbstractObject, IAppEventsInterface)
  protected
    FAppModules: TAppModules;
    FOwnedAppObjects: TAbstractAppObjectList;
    FDLLHandle: longword;
    procedure DestroyMemberObjects; override;
    function GetDefaultKey: string;
  public
    constructor Create(AAppModules: TAppModules); reintroduce;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function ResetState: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function StudyHasChanged: boolean; virtual;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue, ANewValue: string): boolean; virtual;
    property AppModules: TAppModules read FAppModules;
    property DLLHandle: longword read FDLLHandle;
  end;

  TAbstractAppObjectList = class(TList)
  protected
    function GetAppObject(AIndex: integer): TAbstractAppObject;
  public
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    property AppObject[AIndex: integer]: TAbstractAppObject read GetAppObject;
  end;

  TStudyFields = class(TObject)
  public
    FModel: String;
    FSubModel: string;
    FStudyAreaName: String;
    FSubArea: String;
    FScenario: String;

    FStudyDate: TDateTime;
    FConsultant: string;
    FClient: string;
    FStudyNumber: string;
    FStudyLabel: string;
    FStudyAreaDescr: string;
    FStudyShapeFileName : string;

    FSubAreaLabel: String;
    FSubAreaDescr: String;
    FSubAreaShapeFileName : string;
    FTopLeftCoord         : double;
    FTopRightCoord        : double;
    FBottomLeftCoord      : double;
    FBottomRightCoord     : double;

    FScenarioLabel        : String;
    FScenarioDescr        : String;
    FDataFilesPrefix      : String;
    FDataFilesPath        : String;
    FFilesLoaded          : boolean;
    FCalenderStartMonth   : integer;
    FEditable             : boolean;
    FVersion              : string;

    FInputDir             : WideString;
    FOutputDir            : WideString;
    FDebugRequired        : WideString;
    FDebugStartPeriod     : Integer;
    FDebudEndPeriod       : Integer;
    FSummaryRequired      : WideString;
    FSimulationStartYear  : Integer;
    FSimulationEndYear    : Integer;

    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure AfterConstruction;override;
  end;

  TAbstractStudyAreaManager = class(TAbstractAppObject)
  public
    function SelectStudy(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea): boolean; virtual; abstract;
    function AutoSelectStudy(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea): boolean; virtual; abstract;
    function SelectStudyDetails(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea; AStudyLabels: TStudyLabels): boolean; virtual; abstract;
    function ExportStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea): boolean;virtual; abstract;
    function ImportStudyData (AAppModules : TAppModules): boolean; virtual; abstract;
    function CopyStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea; AStudyFields: TStudyFields): boolean;virtual; abstract;
    function DeleteStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea): boolean;virtual; abstract;
    function BuildDatabase ( AAppModules : TAppModules ) : boolean;virtual; abstract;
    function ExportSystemData ( AAppModules : TAppModules ) : boolean;virtual; abstract;
    procedure SetSelectStudyAreaEnabled(AEnabled: boolean);virtual; abstract;
    procedure ExportChangeLists(AChangeListNumbersCommaText: string);virtual; abstract;
    procedure ImportChangeList;virtual; abstract;
  end;


  TAbstractStudyArea = class(TAbstractAppObject)
  protected
    FStudyAreaCode        : string;
    FStudyLabel           : string;
    FStudyAreaDescription : string;
    FStudyDate            : TDateTime;
    FConsultant           : string;
    FClient               : string;
    FStudyNumber          : string;
    FStudyShapeFileName   : string;

    FModelCode            : string;
    FModelLabel           : string;
    FModelSubCode         : string;
    FModelVersion         : string;

    FSubArea              : string;
    FSubAreaLabel         : string;
    FSubAreaDescription   : string;
    FSubAreaShapeFileName : string;
    FTopLeftCoord         : double;
    FTopRightCoord        : double;
    FBottomLeftCoord      : double;
    FBottomRightCoord     : double;

    FScenario             : string;
    FScenarioLabel        : string;
    FScenarioDescription  : string;

    FDataFilesPrefix      : string;
    FDataFilesPath        : string;
    FFilesLoaded          : boolean;
    FCalendarStartMonth   : integer;
    FScenarioLocked       : boolean;

    FEditable             : boolean;
    FSelected             : boolean;
    FSubAreaIndex         : integer;
    FScenarioIndex        : integer;
    FStudyImportDate      : TDateTime;
    FLastUpdateDate       : TDateTime;
    FDataImported         : boolean;

    FSubAreaScenario        : TStringListOfStringLists;
    FSubAreaDescriptions    : TStringListOfStringLists;
    FSubAreaLabels          : TStringListOfStringLists;
    FScenarioDescriptions   : TStringListOfStringLists;
    FScenarioLabels         : TStringListOfStringLists;
    FSubAreaDocumentDetail  : TAbstractStudyDocumentList;
    FScenarioDocumentDetail : TAbstractStudyDocumentList;

  public
    procedure AssignFrom(AStudyArea: TAbstractStudyArea); virtual; abstract;
    function GetDefaultStudyNameList(
      AAfterSep : string = '';
      ATableName : string = '';
      ALevel : integer = 4): string; virtual; abstract;
    function GetDefaultStudyValueList(
      AAfterSep : string = '';
      ALevel : integer = 4): string; virtual; abstract;
    function GetDefaultStudyNameValueList(
      AAfterSep : string = '';
      ATableName : string = '';
      ALevel : integer = 4): string; virtual; abstract;
    function GetSQLStudyCriteria(
      AAfterSep : string = '';
      ATableName : string = '';
      ALevel : integer = 4) : string; virtual; abstract;
    procedure SetDefaultParams(var ADataset : TAbstractModelDataset; ALevel : integer = 4); virtual; abstract;
    procedure SetStudyImportDate(AValue: TDateTime); virtual; abstract;
    procedure SetLastUpdateDate(AValue: TDateTime); virtual; abstract;
    function GetStudyImportDate: TDateTime; virtual; abstract;
    function GetLastUpdateDate: TDateTime; virtual; abstract;

    property StudyAreaCode : string        read FStudyAreaCode;
    property StudyLabel : string           read FStudyLabel;
    property StudyAreaDescription : string read FStudyAreaDescription write FStudyAreaDescription;
    property StudyDate: TDateTime          read FStudyDate            write FStudyDate;
    property Consultant: string            read FConsultant           write FConsultant;
    property Client: string                read FClient               write FClient;
    property StudyNumber: string           read FStudyNumber          write FStudyNumber;
    property StudyShapeFileName : string read FStudyShapeFileName write FStudyShapeFileName;

    property ModelCode : string            read FModelCode;
    property ModelLabel : string           read FModelLabel;
    property ModelSubCode : string         read FModelSubCode;
    property ModelVersion : string         read FModelVersion;

    property SubAreaCode: string           read FSubArea;
    property SubAreaLabel: string          read FSubAreaLabel;
    property SubAreaDescription: string    read FSubAreaDescription   write FSubAreaDescription;
    property SubAreaShapeFileName : string read FSubAreaShapeFileName write FSubAreaShapeFileName;
    property TopLeftCoord         : double read FTopLeftCoord     write FTopLeftCoord;
    property TopRightCoord        : double read FTopRightCoord    write FTopRightCoord;
    property BottomLeftCoord      : double read FBottomLeftCoord  write FBottomLeftCoord;
    property BottomRightCoord     : double read FBottomRightCoord write FBottomRightCoord;

    property ScenarioCode: string          read FScenario;
    property ScenarioLabel: string         read FScenarioLabel;
    property ScenarioDescription: string   read FScenarioDescription  write FScenarioDescription;

    property DataFilesPrefix: string       read FDataFilesPrefix      write FDataFilesPrefix;
    property DataFilesPath: string         read FDataFilesPath        write FDataFilesPath;
    property FilesLoaded: boolean          read FFilesLoaded          write FFilesLoaded;
    property CalendarStartMonth: integer   read FCalendarStartMonth   write FCalendarStartMonth;
    property ScenarioLocked: boolean       read FScenarioLocked       write FScenarioLocked;

    property Selected : boolean            read FSelected;
    property Editable: boolean             read FEditable;
    property SubAreaIndex: integer         read FSubAreaIndex         write FSubAreaIndex;
    property ScenarioIndex: integer        read FScenarioIndex        write FScenarioIndex;
    property StudyImportDate : TDateTime   read GetStudyImportDate    write SetStudyImportDate;
    property LastUpdateDate : TDateTime    read GetLastUpdateDate     write SetLastUpdateDate;
    property DataImported: boolean         read FDataImported         write FDataImported;

    property SubAreaScenario: TStringListOfStringLists          read FSubAreaScenario;
    property SubAreaDescriptions: TStringListOfStringLists      read FSubAreaDescriptions;
    property ScenarioDescriptions: TStringListOfStringLists     read FScenarioDescriptions;
    property SubAreaLabels: TStringListOfStringLists            read FSubAreaLabels;
    property ScenarioLabels: TStringListOfStringLists           read FScenarioLabels;
    property SubAreaDocumentDetail: TAbstractStudyDocumentList  read FSubAreaDocumentDetail;
    property ScenarioDocumentDetail: TAbstractStudyDocumentList read FScenarioDocumentDetail;

  end;

  TAbstractUser = class(TAbstractAppObject)
  public
    procedure SetUserRights(ARights: integer); virtual;abstract;
    procedure SetUserType(AType: TUserType); virtual;abstract;
    procedure SetLoggedOn(ALoggedOn : boolean); virtual;abstract;
    procedure ReloadData; virtual;abstract;
    function UserId : String; virtual;abstract;
    function Password : String; virtual;abstract;
    function Initials : String; virtual;abstract;
    function FirstName : String; virtual;abstract;
    function SecondName : String; virtual;abstract;
    function LastName : String; virtual;abstract;
    function PreferedLanguage : String; virtual;abstract;
    function UserRights : integer; virtual;abstract;
    function LoggedOn : boolean; virtual;abstract;
    function UserType : TUserType; virtual;abstract;
    function AutoLogOn: Boolean;virtual;abstract;
    function AutoSelectStudy: Boolean;virtual;abstract;
    function UserModelCount: integer; virtual;abstract;
    function UserModelByIndex(AIndex: integer): string; virtual;abstract;
  end;

  TAbstractLanguage = class(TAbstractAppObject)
  protected
    function GetLanguageID: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
  public
    function GetString(AStringConstant: string): string; virtual; abstract;
    property LanguageID: string read GetLanguageID;
    property Description: string read GetDescription;
  end;

  TAbstractFieldProperty = class(TAbstractAppObject)
  protected
    FFieldName           : string;
    FFieldDescription    : string;
    FFieldLangDescr      : string;
    FFieldType           : Integer;
    FFieldDataType       : Integer;
    FFieldWidth          : Integer;
    FNumberOfDecimals    : Integer;
    FSmartFormat         : string;
    FFormatStringGrid    : string;
    FFormatStringGraph   : string;
    FModeIsEditable      : string;
    FFieldUnits          : string;
    FFieldSource         : string;
    FFieldMinimumValue   : string;
    FFieldMaximumValue   : string;
    FFileFieldName       : string;
    FFieldArrayLength    : string;
    FFieldAcceptedValues : String;
    FFieldGroup          : TFieldGroup;
    FModelVersionNumber  : string;
    FFileName            : string;
    FDBTableName         : string;
    FDBFieldName         : string;
    FDataClassName       : string;
    FInChangeList        : boolean;
    function GetFieldUpdateSQL(AStepIndex: integer): TAbstractFieldUpdateSQLStep; virtual; abstract;
    function GetArrayLow: integer; virtual; abstract;
    function GetArrayHigh: integer; virtual; abstract;
    function GetArrayLowDimTwo: integer; virtual; abstract;
    function GetArrayHighDimTwo: integer; virtual; abstract;
    function GetFieldMinimumValue: string; virtual; abstract;
    function GetFieldMaximumValue: string; virtual; abstract;
    function GetSmartFormat: boolean;
  public
    function ArrayLength(Aindex: integer=0): integer; virtual; abstract;
    function FieldUpdateSQLCount: integer; virtual; abstract;
    property FieldName            : string       read FFieldName;
    property FieldLangDescr       : string       read FFieldLangDescr;
    property FieldType            : Integer      read FFieldType;
    property FieldDataType        : Integer      read FFieldDataType;
    property FieldWidth           : Integer      read FFieldWidth;
    property NumberOfDecimals     : Integer      read FNumberOfDecimals;
    property FormatStringGrid     : string       read FFormatStringGrid;
    property FormatStringGraph    : string       read FFormatStringGraph;
    property ModeIsEditable       : string       read FModeIsEditable;
    property FieldUnits           : string       read FFieldUnits;
    property FieldDescription     : string       read FFieldDescription;
    property FieldSource          : string       read FFieldSource;
    property FieldMinimumValue    : string       read GetFieldMinimumValue;
    property FieldMaximumValue    : string       read GetFieldMaximumValue;
    property FileFieldName        : String       read FFileFieldName;
    property FieldArrayLength     : String       read FFieldArrayLength;
    property FieldAcceptedValues  : String       read FFieldAcceptedValues;
    property FieldGroup           : TFieldGroup  read FFieldGroup;
    property ArrayLow             : integer      read GetArrayLow;
    property ArrayHigh            : integer      read GetArrayHigh;
    property ArrayLowDimTwo       : integer      read GetArrayLowDimTwo;
    property ArrayHighDimTwo      : integer      read GetArrayHighDimTwo;
    Property ModelVersionNumber   : string       read FModelVersionNumber;
    property FileName             : string       read FFileName;
    property DBTableName          : string       read FDBTableName;
    property DBFieldName          : string       read FDBFieldName;
    property DataClassName        : string       read FDataClassName;
    property InChangeList         : boolean      read FInChangeList;
    property SmartFormat          : boolean      read GetSmartFormat;

    property FieldUpdateSQL[AStepIndex: integer]: TAbstractFieldUpdateSQLStep read GetFieldUpdateSQL;
  end;

  TAbstractFieldsProperties = class(TAbstractAppObject)
  protected
    FLastFieldToChange: TStudyDataHasChangedData;
  public
    function FieldProperty(AFieldName: string):TAbstractFieldProperty; virtual; abstract;
    function UpdateFieldValue(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean; virtual; abstract;
    function FieldPropertyFromFileReference(
      AFileName,ALineNo: string; ACharacterPosition: integer): TAbstractFieldProperty; virtual; abstract;
    function GetFieldPropertiesList(AFieldList: TObjectList;AFieldGroup : TFieldGroup = fgNone): boolean; virtual; abstract;
    function GetArrayFieldPropertiesList(AFieldList: TObjectList): boolean; virtual; abstract;
    function ValidateFieldProperty(AFieldName,AFieldValue: string; var AErrorMessage: string;
             ADimOneIndex: integer = -1; ADimTwoIndex: integer = -1 ): boolean;virtual; abstract;
    function ModeIsEditable(AFieldName: string): boolean; virtual; abstract;
    function FieldAvailableInModel(AFieldName: string): boolean; virtual; abstract;
    function LoadFieldFileReferencesData(AModelName: string): boolean; virtual; abstract;
    // HACK. Must be passed down in StudyDataHasChanged method. Grant/Val
    property LastFieldToChange: TStudyDataHasChangedData read FLastFieldToChange;
  end;

  TAbstractChangeManager = class(TAbstractAppObject)
  protected
    function GetShowAllChangeLists : boolean; virtual; abstract;
    procedure SetShowAllChangeLists (AShowAll : boolean); virtual; abstract;
  public
    function ChangeGroups : TList; virtual; abstract;
    function ChangeGroupWithID (AGroupID : integer) : IChangeGroup; virtual; abstract;
    function ChangeGroupWithIndex (AIndex : integer) : IChangeGroup; virtual; abstract;
    function ChangeLists : TStringList{TList}; virtual; abstract;
    function Get_MasterGroup : IChangeGroup; virtual; abstract;
    procedure GetChangeListIDsInOrder (AGroupID : integer;
                                       ALists   : TStringList); virtual; abstract;
    function IsChangeListActive (AListID : integer) : WordBool; virtual; abstract;
    function ChangeListWithID (AListID : integer) : IChangeList; virtual; abstract;
    function ChangeListWithIndex (AIndex : integer) : IChangeList; virtual; abstract;
    function DoCreateNewChangeGroup : IChangeGroup; virtual; abstract;
    function DoDeleteChangeGroup (AChangeGroupID : integer) : WordBool; virtual; abstract;
    function MayCreateNewChangeGroupElement (AOldParentID    : integer;
                                             ANewParentID    : integer;
                                             AElementID      : integer;
                                             AIsElementGroup : WordBool;
                                             var AErrorMsg   : string) : WordBool; virtual; abstract;
    function DoCreateNewChangeGroupElement (AParentGroupID  : integer;
                                            AElementID      : integer;
                                            AIsElementGroup : WordBool) : IChangeGroupElement; virtual; abstract;
    function DoDeleteChangeGroupElement (AParentGroupID  : integer;
                                         AElementID      : integer;
                                         AIsElementGroup : WordBool) : WordBool; virtual; abstract;
    function DoCreateNewChangeList : IChangeList; virtual; abstract;
    function DoDeleteChangeList (AChangeListID : integer) : WordBool; virtual; abstract;
    function DoCopyChangeList (AChangeListID : integer) : IChangeList; virtual; abstract;
    function DoMoveUpChangeElement (AParentGroupID  : integer;
                                    AElementID      : integer;
                                    AIsElementGroup : Boolean) : WordBool; virtual; abstract;
    function DoMoveDownChangeElement (AParentGroupID  : integer;
                                      AElementID      : integer;
                                      AIsElementGroup : Boolean) : WordBool; virtual; abstract;
    function DoActivateChangeElement (AParentGroupID  : integer;
                                      AElementID      : integer;
                                      AIsElementGroup : Boolean) : WordBool; virtual; abstract;
    function DoDeactivateChangeElement (AParentGroupID  : integer;
                                        AElementID      : integer;
                                        AIsElementGroup : Boolean) : WordBool; virtual; abstract;
    function DoApplyChangeList (AChangeListID : integer) : WordBool; virtual; abstract;
    function DoImportChangeList (AChangeListID : integer) : WordBool; virtual; abstract;
    function DoExportChangeList (AChangeListID : integer) : WordBool; virtual; abstract;
    function DoStationFilter(AChangeListID : integer) : WordBool; virtual; abstract;
    procedure SetCreateNewChangeGroup (AEnabled : boolean);  virtual; abstract;
    procedure SetDeleteChangeGroup (AEnabled : boolean); virtual; abstract;
    procedure SetCreateNewChangeList (AEnabled : boolean); virtual; abstract;
    procedure SetDeleteChangeList (AEnabled : boolean); virtual; abstract;
    procedure SetCopyChangeList (AEnabled : boolean); virtual; abstract;
    procedure SetMoveUpChangeElement (AEnabled : boolean); virtual; abstract;
    procedure SetMoveDownChangeElement (AEnabled : boolean); virtual; abstract;
    procedure SetActivateChangeElement (AEnabled : boolean); virtual; abstract;
    procedure SetDeactivateChangeElement (AEnabled : boolean); virtual; abstract;
    procedure SetApplyChangeList (AEnabled : boolean); virtual; abstract;
    procedure SetImportChangeList (AEnabled : boolean); virtual; abstract;
    procedure SetExportChangeList (AEnabled : boolean); virtual; abstract;
    procedure SetStationFilter (AEnabled : boolean); virtual; abstract;
    procedure SetParameterChanges (AEnabled : boolean); virtual; abstract;
    procedure ShowParameterChanges (AParamField     : string;
                                    AKeyValues      : string;
                                    AFieldIndex     : string); virtual; abstract;
    function GetParameterValue (AParamField : string;
                                AKeyValues  : string;
                                ABaseValue  : string;
                                AFieldIndex : string) : string; virtual; abstract;
    function GetKeyValue (AKeyName   : string;
                          AKeyValues : string) : string; virtual; abstract;
    procedure TabHasChanged; virtual; abstract;
    procedure AddTabsheetToPageControl; virtual; abstract;
    procedure RemoveTabsheetFromPageControl; virtual; abstract;
    procedure GetIndexes (AFieldIndex  : string;
                          var lDim1Idx : integer;
                          var lDim2Idx : integer); virtual; abstract;
    function HasParamChange (AParamField : WideString;
                             AKeyValues  : WideString;
                             AFieldIndex : WideString) : boolean;  virtual; abstract;
    function ProcessMetaDataEvent : boolean; virtual; abstract;
    function EntityDescription (AFieldPropName : string;
                                AKeyValues     : string;
                                AFieldIndex    : string) : string; virtual; abstract;
    property ShowAllChangeLists : boolean read GetShowAllChangeLists write SetShowAllChangeLists;
    property MasterGroup  : IChangeGroup read Get_MasterGroup;
  end;

  TAbstractMetaDataManager = class(TAbstractAppObject)
  protected
  public
    function DataList : TList; virtual; abstract;
    procedure SetMetaDataMenuItem (AEnabled : boolean); virtual; abstract;
    function FindMetaData (AParamField : WideString;
                           AKeyValues  : WideString;
                           AFieldIndex : WideString) : IMetaData;  virtual; abstract;
    procedure ShowMetaData (AParamField     : string;
                            AKeyValues      : string;
                            AFieldIndex     : string); virtual; abstract;
    function CreateNewMetaData (AParamField : WideString;
                                AKeyValues  : WideString;
                                AFieldIndex : WideString) : IMetaData; virtual; abstract;
    function DeleteMetaData (AParamField : WideString;
                             AKeyValues  : WideString;
                             AFieldIndex : WideString) : WordBool; virtual; abstract;
  end;

  TAbstractWeatherEventsManager = class(TAbstractAppObject)
  public
    procedure TabHasChanged; virtual; abstract;
    procedure AddTabsheetToPageControl; virtual; abstract;
    procedure RemoveTabsheetFromPageControl; virtual; abstract;
    function ProcessMetaDataEvent : boolean; virtual; abstract;
    function Data : IWeatherEvents; virtual; abstract;
    procedure ShowWeatherEvents (AStartDateTime : TDateTime;
                                 AEndDateTime   : TDateTime;
                                 const AArea    : WideString); virtual; abstract;
  end;

  {TAbstractLicenceManager = class(TAbstractAppObject)
  public
    procedure LicenceModels; virtual;abstract;
    function ModelIsLicenced(AModelName: string): boolean;virtual;abstract;
    function LicencedModelsCommatext: string;virtual;abstract;
    function AllModelsCommatext: string;virtual;abstract;
    function LicenceHolderName: string; virtual;abstract;
  end;}

  TAbstractScenarioLockManager = class(TAbstractAppObject)
  public
    function RequestLock: boolean;virtual;abstract;
    function ReleaseLock: boolean;virtual;abstract;
    function RequestLockStatus(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode : string) : boolean;virtual;abstract;
    function RequestScenarioUnlock(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode : string) : boolean;virtual;abstract;
  end;

  TAbstractAccessControlManager = class(TAbstractAppObject)
  public
    procedure DoLogOn(var AUser: TAbstractUser); virtual; abstract;
    procedure DoLogOff(var AUser: TAbstractUser); virtual; abstract;
    procedure DoUserAdministration(AUser: TAbstractUser);virtual; abstract;
    procedure DoAutoLogOn(var AUser: TAbstractUser); virtual; abstract;
    procedure HideMenus; virtual; abstract;
  end;

  TAbstractPrintManager = class(TAbstractAppObject)
  public
    procedure SetPrintEnabled(AEnabled: boolean);virtual; abstract;
    procedure DoPrintSettings;virtual; abstract;
    procedure DoPrintPreview(AReportData: TObject);virtual; abstract;
    procedure DoPrint(AReportData: TObject);virtual; abstract;

    procedure SetMenuPrintState(AAction: TMenuSetAction);virtual; abstract;
    procedure SetMenuPrintPreviewState(AAction: TMenuSetAction);virtual; abstract;
    procedure SetMenuPrintSettingsState(AAction: TMenuSetAction);virtual; abstract;
  end;

  TAbstractStudyDocumentManager = class(TAbstractAppObject)
  public
    function LaunchReport(AData: TObject): boolean; virtual; abstract;
    function ViewFile(AFileNAme: string): boolean; virtual; abstract;
    function AddReport: boolean; virtual; abstract;
    function EditReport: boolean; virtual; abstract;
    function DeleteReport: boolean; virtual; abstract;
  end;

  TAbstractModelDataset = class(TAbstractAppObject)
  public
    function DataSet: TDWADataSet; virtual; abstract;
    //function Table: TDWATable; virtual; abstract;
    function DataSetType: integer; virtual; abstract;
    procedure ClearSQL; virtual; abstract;
    procedure ResetDefaultSQL; virtual; abstract;
    procedure SetSQL(ASQL: string); virtual; abstract;
    procedure AppendSQL(ASQL: string); virtual; abstract;
    procedure SetParams(AParamNames, AParamValues: array of string); virtual; abstract;
    procedure SetParamValue(AParamName: string; AValue: WideString; ADataType: TFieldType; ABlobField: boolean=False; ABlobLength: integer=0); virtual; abstract;
    procedure ReplaceSQLParam(AParamName, AParamValue: String); virtual; abstract;
    procedure ReplaceSQLParamArray(AParamName, AParamValue: array of String); virtual; abstract;
    procedure ExecSQL(AIgnoreErrors : boolean = false); virtual; abstract;
    //procedure ConvertDatasetToTable; virtual; abstract;
    procedure SetReadOnly(AReadOnly: boolean); virtual; abstract;
    function IsReadOnly: boolean; virtual; abstract;
    //procedure SetUniDirectional(AUniDirectional: boolean); virtual; abstract;
    //function IsUniDirectional: boolean; virtual; abstract;
    function AreAllParamsBound(AReportError: boolean = False): boolean; virtual; abstract;
    function ClearDataset : boolean; virtual; abstract;
    function ClearQueryParams(AParamType :TParamType=prAll): boolean; virtual; abstract;
    function GetParamByName( AParamName: string): TDWAParameter; virtual; abstract;
  end;

  TDatabaseType = (dbtUnknown, dbtAccessODBC, dbtInformix, dbtParadox);
  TAbstractDatabaseLayer = class(TAbstractAppObject)
  protected
    procedure SetConnected(AConnected: boolean); virtual; abstract;
    function GetConnected: boolean; virtual; abstract;
  public
    function ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean; virtual; abstract;
    function ExecMultipleSQL(AMultipleSQL: array of string; AShowErrors: boolean = True): boolean; virtual; abstract;
    function DropTable(ATableName: string): boolean; virtual; abstract;
    function CreateTable(ATableName: string; AFieldNames: TArrayOfString): boolean; virtual; abstract;
    function LoadTable(ALoadDataSQL: TArrayOfString): boolean; virtual; abstract;
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; virtual; abstract;
    function DatabaseName: string; virtual; abstract;
    function Connection: TDWAConnection; virtual; abstract;
    function Commit: boolean;virtual; abstract;
    function Rollback: boolean;virtual; abstract;
    function StartTransaction: boolean;virtual; abstract;
    function InTransaction: boolean;virtual; abstract;
    function GetTableNames ( AContainer : TStrings ) : boolean; virtual; abstract;
    function UpdateMemoField(ATableName,AFieldName,AWhereClause,AMemoText: string): boolean; virtual; abstract;
    function UpdateBlobField(ATableName,AFieldName,AWhereClause: string; ABlobStream : TStream): boolean; virtual; abstract;
    property Connected: boolean read GetConnected write SetConnected;
  end;

  TAbstractViewDataList = class(TAbstractAppObject)
  protected
    function GetCurrentTreeNodeData: TObject; virtual; abstract;
    procedure SetCurrentTreeNodeData(ATreeNodeData: TObject); virtual; abstract;
  public
    procedure PopulateTreeNodes(ATreeNodes: TObject; AViewTypeID: string); virtual; abstract;
    property CurrentTreeNodeData: TObject read GetCurrentTreeNodeData write SetCurrentTreeNodeData;
  end;

  TAbstractModelManager = class(TAbstractAppObject)
  protected
    function GetFieldUpdateFunction(AFieldName: String) : TFieldUpdateFunction; virtual; abstract;
    function GetFieldEditFunction(AFieldName: String) : TFieldEditFunction; virtual; abstract;
  public
    function ModelName: string; virtual; abstract;
    function ViewData: TAbstractViewDataList; virtual; abstract;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; virtual; abstract;
    function ModelDataLoaded: boolean; virtual; abstract;
    function ModelData: TInterfacedObject;  virtual; abstract;
    function PopulateDataList(AModelDataChangeType : TModelDataChangeType; AList : TList) : boolean; virtual; abstract;
    function UpdateFieldValue(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;
    function EditFieldValue(AFieldName, AOldValue : string; AContextData: TStrings): boolean; overload; virtual; abstract;
    function EditFieldValue(AFieldName, AOldValue, ADefaultValue: string; AContextData: TStrings): boolean; overload; virtual; abstract;
    function DoesFieldHaveACustomEditor(AFieldName: string): boolean; virtual; abstract;
    procedure DoModelDataHasChanged(AChangeLevel:TModelDataChangeLevel;AChangeType:TModelDataChangeType;
              AChangeAction:TModelDataChangeAction);  virtual; abstract;
    function GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string; virtual;
    function CanApplicationClose : boolean; virtual;
    procedure ApplicationIsClosing; virtual;
    function IsGridLoaded : boolean; virtual;
    function IsGraphLoaded : boolean; virtual;
    // VGN - ARIVIA - 20030408/10 see check in commments
    function ViewInputDialog(AParent : TObject; ACommaTextContextData : String; AOwner : TObject = nil): boolean; virtual;
    function ViewInputPopupDialog(AParent : TObject; ACommaTextContextData : String;  AOwner : TObject = nil): boolean; virtual;
    function ViewOutputDialog(AParent : TObject; ACommaTextContextData : String; AOwner : TObject = nil): boolean; virtual;
    function ViewOutputPopupDialog(AParent : TObject; ACommaTextContextData : String;  AOwner : TObject = nil): boolean; virtual;
    function ViewOutputComparisonDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;virtual;

    function DeleteModelDataViewer(AParent : TObject; AOwner : TObject) : boolean; virtual;

    function ValidateBusinessRule(ABusinessRule: integer; AObjectsArray: array of TObject;
             AErrorMessages: TStrings;AOptionalValues: string=''): boolean; virtual; abstract;
    function CanCopyToCLipboard: boolean; virtual;
    function CanExport: boolean; virtual;
    function CanPrint: boolean; virtual;
    function EntityDescription (AFieldPropName : string;
                                AKeyValues     : string;
                                AFieldIndex    : string) : string; virtual;
    function SetBaseValue (AFieldPropName : string;
                           AKeyValues     : string;
                           AFieldIndex    : string;
                           ANewValue      : string) : boolean; virtual;
    function GetBaseValue (AFieldPropName : string;
                           AKeyValues     : string;
                           AFieldIndex    : string) : string; virtual;
    procedure DoCopyToCLipboard; virtual;
    procedure DoExport(AFileName: string = ''); virtual;
    procedure DoPrint; virtual;
    function GetModelViewItems(AItems: TStringListOfStringLists): boolean; virtual;
    function GetModelDataSetKey : string; virtual;
    function GetChangeListWhereClause : string; virtual;
    //function DoStationFilter(AChangeListID : integer) : WordBool; virtual; abstract;
  end;

  IMenuItemManager = interface(IAppEventsInterface)
    ['{77068F0A-BE8A-489E-8C6A-DA975E18FB8A}']
    procedure AddMenuItems;
    procedure DeleteMenuItems;
    procedure Show;
    procedure Hide;
    procedure Enable(AStatusReason: string = '');
    procedure Disable(AStatusReason: string = '');
  end;

  IMainFormMenuItemManager = interface(IMenuItemManager)
    ['{2DDFDBBB-0189-4B16-91B4-95DFBDEC0417}']
    function IsToolBarVisible: boolean;
    procedure SetViewToolBarChecked(AChecked: boolean);
    procedure SetViewStudyPanelChecked(AChecked: boolean);
    procedure SetClipboardEnabled(AEnabled: boolean);
    procedure SetExportEnabled(AEnabled: boolean);
  end;

  TAbstractMainFormManager = class(TAbstractAppObject)
  protected
    function GetMainFormClassReference: TComponentClass; virtual; abstract;
    function GetMainForm: TForm; virtual; abstract;
    procedure SetMainForm(AForm: TForm); virtual; abstract;
    function GetMenu: TMainMenu; virtual; abstract;
    function GetPageControl: TPageControl; virtual; abstract;
    function GetActivePage: TTabSheet; virtual; abstract;
    procedure SetActivePage(ATabSheet: TTabSheet); virtual; abstract;
    function GetToolBar: TCustomPanel; virtual; abstract;
    function GetChildToolBar: TCustomPanel; virtual; abstract;
    procedure SetChildToolBar(AChildToolBar: TCustomPanel); virtual; abstract;
    function GetMenuItemManager: IMainFormMenuItemManager; virtual; abstract;
    function GetStudyPanelVisible: boolean; virtual; abstract;
    procedure SetStudyPanelVisible(AVisible: boolean); virtual; abstract;
    function GetToolBarVisible: boolean; virtual; abstract;
    procedure SetToolBarVisible(AVisible: boolean); virtual; abstract;
  public
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; virtual; abstract;
    procedure RefreshState; virtual; abstract;
    procedure ApplyPreviousTabIndex; virtual; abstract;
    procedure SaveCurrentTabIndex; virtual; abstract;
    procedure AddSystemChildToolBar(ASystemChildToolBar:TCustomPanel);virtual; abstract;
    procedure RemoveSystemChildToolBar(ASystemChildToolBar: TCustomPanel);virtual; abstract;
    function GetBtnWhatIsThis() : TObject;virtual; abstract;
    property MainFormClassReference: TComponentClass read GetMainFormClassReference;
    property MainForm: TForm read GetMainForm write SetMainForm;
    property Menu: TMainMenu read GetMenu;
    property PageControl: TPageControl read GetPageControl;
    property ActivePage: TTabSheet read GetActivePage write SetActivePage;
    property ToolBar: TCustomPanel read GetToolBar;
    property ChildToolBar: TCustomPanel read GetChildToolBar write SetChildToolBar;
    property MenuItemManager: IMainFormMenuItemManager read GetMenuItemManager;
    property StudyPanelVisible: boolean read GetStudyPanelVisible write SetStudyPanelVisible;
    property ToolBarVisible: boolean read GetToolBarVisible write SetToolBarVisible;
  end;

  TAbstractDbTableProperty = class(TAbstractObject)
  protected
    function GetTableName: string; virtual; abstract;
    function GetTableIndex: integer; virtual; abstract;
    function GetModelNames: TStringList; virtual; abstract;
    function GetFieldNames: TStringList; virtual; abstract;
    function GetIndexFieldNames: TStringList; virtual; abstract;
    function GetTableGroup: integer; virtual; abstract;
    function GetTableFilter: string; virtual; abstract;
    procedure SetTableFilter(AFilter: string); virtual; abstract;
    function GetDelimitedFieldNamesCommatext: String; virtual; abstract;
  public
    function FieldsParams: string;virtual; abstract;
    property TableName       : string      read GetTableName;
    property TableIndex      : integer     read GetTableIndex;
    property ModelNames      : TStringList read GetModelNames;
    property IndexFieldNames : TStringList read GetIndexFieldNames;
    property FieldNames      : TStringList read GetFieldNames;
    property TableGroup      : integer     read GetTableGroup;
    property TableFilter     : string      read GetTableFilter write SetTableFilter;
    property DelimitedFieldNamesCommatext: string read GetDelimitedFieldNamesCommatext;
  end;

  TAbstractSQLAgent = class(TAbstractAppObject)
  protected
    function LoadSQLFromResource(AResourceName: String; var ASQL: String; OPNAME: String): Boolean; virtual;
    function CleanSQL(ASQL, OPNAME: String): String; virtual;
    procedure ReplaceSQLParameter(AParamName, AParamValue: String; var ASQL: String; OPNAME: String); virtual;
    procedure ReplaceSQLParameterList(ANameValues: TStringList; var ASQL: String; OPNAME: String); overload; virtual;
    procedure ReplaceSQLParameterList(ANames, AValues: array of const; var ASQL: String; OPNAME: String); overload; virtual;
    procedure ReplaceScenarioParameters(var ASQL: String; OPNAME: String); virtual;
  end;

  TAbstractDBTablePropertyManager = class(TAbstractAppObject)
  protected
    function GetTablePropertyByName(ATableName: string): TAbstractDbTableProperty; virtual; abstract;
    function GetTablePropertyByIndex(AIndex: integer): TAbstractDbTableProperty; virtual; abstract;
    function GetTablePropertyCount : integer;virtual; abstract;
  public
    function GetTablesPerModel(AModel: string; AContainer: TObjectList): boolean; virtual; abstract;
    function GetTablesPerGroup ( AGroup : integer; AContainer : TObjectList ) : boolean; virtual; abstract;
    property TablePropertyByName[ATableName: string]: TAbstractDbTableProperty read GetTablePropertyByName;
    property TablePropertyByIndex[AIndex: integer]: TAbstractDbTableProperty read GetTablePropertyByIndex;
    property TablesCount : integer read GetTablePropertyCount;
  end;

  TAbstractTableFieldDef = class (TAbstractObject)
  protected
    function GetTableName   : string; virtual; abstract;
    function GetFieldName   : string; virtual; abstract;
    function GetFieldType   : string; virtual; abstract;
    function GetFieldLength : integer; virtual; abstract;
  public
    property TableName       : string  read GetTableName;
    property FieldName       : string  read GetFieldName;
    property FieldType       : string  read GetFieldType;
    property FieldLength     : integer read GetFieldLength;
  end;

  TAbstractTableFieldsDef = class (TAbstractObject)
  protected
    function GetTableName   : string; virtual; abstract;
    function GetFieldByIndex(AIndex: integer):TAbstractTableFieldDef;virtual; abstract;
    function GetFieldByName(AName: string):TAbstractTableFieldDef;virtual; abstract;
    function GetCreateTableFields: string;virtual; abstract;
    function GetFieldsCount: integer;virtual; abstract;
  public
    property TableName       : string  read GetTableName;
    property CreateTableFields    : string  read GetCreateTableFields;
    property FieldsCount     : integer read GetFieldsCount;
    property FieldByIndex[AIndex: integer]: TAbstractTableFieldDef  read GetFieldByIndex;
    property FieldByName[AName: string]: TAbstractTableFieldDef  read GetFieldByName;
  end;

  TAbstractTableFieldsDefList = class(TAbstractAppObject)
  protected
    function GetTableFieldsDefByName ( ATableName : string ) : TAbstractTableFieldsDef; virtual; abstract;
    function GetTableFieldsDefByIndex ( AIndex: integer ) : TAbstractTableFieldsDef; virtual; abstract;
    function GetTableFieldsDefCount  : integer; virtual; abstract;
  public
    property TableFieldsDefByIndex [ AIndex: integer ] : TAbstractTableFieldsDef read GetTableFieldsDefByIndex;
    property TableFieldsDefByName [ ATableName : string ] : TAbstractTableFieldsDef read GetTableFieldsDefByName;
    property TableFieldsDefCount : integer read  GetTableFieldsDefCount;
  end;

function CreateDLLObject(ADLLFileName: string; var AObject: TAbstractAppObject; AAppModules: TAppModules;
                         AReportError: boolean; ALocation: string): boolean;
procedure DestroyDLLObject(var AObject: TAbstractAppObject; ALocation: string);

function GetNumberOfDecimals(AValueType: TOutputDataType): integer;
function GetNumberFormat(AValueType: TOutputDataType;ALength: integer=0): string;
function GetFloatFormat(AValueType: TOutputDataType;ALength: integer=0): string;
function FormatOutputValue(AValueType: TOutputDataType; AValue:double;ALength: integer=0): string;

implementation

{ TAbstractObject }
uses
  SysUtils,
  Windows,
  Dialogs,

{$IFDEF MERGE_DLLS}

  UAccessControlManager,
  UChangeManager,
  UDailyDiversionModelManager,
  UFieldPropertyManager,
  UFileEditManager,
  UGridEditorManager,
  UGridOutputEditorManager,
  UHydrologyModel,
  UHydroNVManager,
  ULanguageManager,
  UMetaDataManager,
  UModelCapabilityManager,
  UDDTSModelManager,
  UModelHydrologyManager,
  UPlanningModelManager,
  UModelRainfallDataManager,
  URWHModelManager,
  UStomsaModelManager,
  UYieldModelManager,
  UModelYRCManager,
  UAccessADODatabaseLayer,
  UOutputComparisonManager,
  UOutputReviewManager,
  USedimentationModelManager,
  UIFRModelManager,
  UPrintManager,
  UStudyDocumentManager,
  UStudyAreaManager,
  UTimeSeriesComparitorManager,
  UViewDataGraphManager,
  UViewOutputGraphManager,
  UNetworkVisualiserManager,
  UWeatherEventsManager,
  UYieldReliabilityCurveManager,

{$ENDIF}

  UDLLOperations,
  UErrorHandlingOperations;


procedure TAbstractObject.AfterConstruction;
const OPNAME = 'TAbstractObject.AfterConstruction';
begin
  inherited;
  try
    FRefCount := 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractObject.AfterCreateMemberObjects;
const OPNAME = 'TAbstractObject.AfterCreateMemberObjects';
begin
  try
   // Do nothing here.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractObject.BeforeDestroyMemberObjects;
const OPNAME = 'TAbstractObject.BeforeDestroyMemberObjects';
begin
  try
   // Do nothing here.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractObject.BeforeDestruction;
const OPNAME = 'TAbstractObject.BeforeDestruction';
begin
  try
    try
      FRefCount := 0;
    except on E: Exception do HandleError(E, OPNAME) end;
  finally
    inherited;
  end;
end;

constructor TAbstractObject.Create;
const OPNAME = 'TAbstractObject.Create';
begin
  inherited Create;
  try
    CreateMemberObjects ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractObject.CreateMemberObjects;
const OPNAME = 'TAbstractObject.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractObject.Destroy;
const OPNAME = 'TAbstractObject.Destroy';
begin
  try
    try
      DestroyMemberObjects;
    finally
      inherited Destroy;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractObject.DestroyMemberObjects;
const OPNAME = 'TAbstractObject.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractObject.DummyShowProgress(AProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean=False);
const OPNAME = 'TAbstractObject.DummyShowProgress';
begin
  AStop := False;
//Do nothing. this is a place holder for silent validation.
end;

function TAbstractObject.Initialise: boolean;
const OPNAME = 'TAbstractObject.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractObject.SaveState: boolean;
const OPNAME = 'TAbstractObject.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractObject._AddRef: Integer;
const OPNAME = 'TAbstractObject._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result := FRefCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractObject._Release: Integer;
const OPNAME = 'TAbstractObject._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result := FRefCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAppModules }

function TAppModules.GlobalData: TAbstractGlobalData;
const OPNAME = 'TAppModules.GlobalData';
begin
  Result := nil;
end;

function TAppModules.AddMenuItem(AMainMenuKeys: array of string;
  ASortWeight: integer; AEvent: integer; AData: TObject): TObject;
const OPNAME = 'TAppModules.AddMenuItem';
begin
  Result := nil;
end;

function TAppModules.FieldProperties: TAbstractFieldsProperties;
const OPNAME = 'TAppModules.FieldProperties';
begin
  Result := nil;
end;
{
function TAppModules.IsDocumentManagerLoaded: boolean;
const OPNAME = 'TAppModules.IsDocumentManagerLoaded';
begin
  Result := False;
end;
}
function TAppModules.StudyDocumentManager: TAbstractStudyDocumentManager;
const OPNAME  = 'TAppModules.StudyDocumentManager';
begin
  Result := nil;
end;

function TAppModules.LoadModel(AModel: TModelManagerType): boolean;
const OPNAME = 'TAppModules.LoadModel';
begin
  Result := True;
end;

function TAppModules.SetMenuItem(AMainMenuKeys: array of string; AnAction: TMenuSetAction; AStatusReason: string = ''): boolean;
const OPNAME = 'TAppModules.SetMenuItem';
begin
  Result := True;
end;

function TAppModules.StudyArea: TAbstractStudyArea;
const OPNAME = 'TAppModules.StudyArea';
begin
  Result := nil;
end;

procedure TAppModules.SetStudyArea(ANewStudyArea: TAbstractStudyArea);
const OPNAME = 'TAppModules.SetStudyArea';
begin
end;

procedure TAppModules.SelectStudyArea;
const OPNAME = 'TAppModules.SelectStudyArea';
begin
end;

procedure TAppModules.SelectStudyDetails(AData: TObject);
const OPNAME = 'TAppModules.SelectStudyDetails';
begin
end;

function TAppModules.ViewData: TAbstractViewDataList;
const OPNAME = 'TAppModules.ViewData';
begin
  Result := nil;
end;

function TAppModules.Language: TAbstractLanguage;
const OPNAME = 'TAppModules.Language';
begin
  Result := nil;
end;

function TAppModules.LanguageHasChanged: boolean;
const OPNAME = 'TAppModules.LanguageHasChanged';
begin
  Result := True;
end;

function TAppModules.MainForm: TAbstractMainFormManager;
const OPNAME = 'TAppModules.MainForm';
begin
  Result := nil;
end;

function TAppModules.Model: TAbstractModelManager;
const OPNAME = 'TAppModules.Model';
begin
  Result := nil;
end;

function TAppModules.User: TAbstractUser;
const OPNAME = 'TAppModules.User';
begin
  Result := nil;
end;

function TAppModules.AccessControlManager: TAbstractAccessControlManager;
const OPNAME = 'TAppModules.AccessControlManager';
begin
  Result := nil;
end;

procedure TAppModules.DoLogOn;
const OPNAME = 'TAppModules.DoLogOn';
begin
end;

procedure TAppModules.DoLogOff;
const OPNAME = 'TAppModules.DoLogOff';
begin
end;

procedure TAppModules.DoUserAdministration;
const OPNAME = 'TAppModules.DoUserAdministration';
begin
end;


function TAppModules.GetExportFilename(const ADefaultExt, AFilter: string; var AFileName: string): boolean;
const OPNAME = 'TAppModules.GetExportFilename';
begin
  AFileName := '';
  Result := False;
end;

function TAppModules.DBTableFieldsDefList: TAbstractTableFieldsDefList;
const OPNAME = 'TAppModules.DBTableFieldsDefList';
begin
  Result := nil;
end;

function TAppModules.DBTablePropertyManager: TAbstractDBTablePropertyManager;
const OPNAME = 'TAppModules.DBTablePropertyManager';
begin
  Result := nil;
end;

procedure TAppModules.DoAbout;
const OPNAME = 'TAppModules.DoAbout';
begin
end;

function TAppModules.SetMenuItemCaption(AMainMenuKeys: array of string;
  ACaption: string): boolean;
const OPNAME = 'TAppModules.SetMenuItemCaption';
begin
  Result := true;
end;

function TAppModules.HydrologyFileType: TAbstractHydrologyFileType;
const OPNAME = 'TAppModules.HydrologyFileType';
begin
  Result := nil;
end;

{function TAppModules.SetMenuItemHelpContext(AMainMenuKeys: array of string;
  AHelpContextID: THelpContext): boolean;
const OPNAME = 'TAppModules.SetMenuItemHelpContext';
begin
  Result := true;
end;}

function TAppModules.GetMenuItemProperties(
  AMenuKeys: array of string): TSetOfMenuAction;
const OPNAME = 'TAppModules.GetMenuItemProperties';
begin
  Result := [];
  try
    Result := [];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModules.PrintManager: TAbstractPrintManager;
const OPNAME = 'TAppModules.PrintManager';
begin
  Result := nil;
end;

function TAppModules.CanApplicationClose: boolean;
const OPNAME = 'TAppModules.CanApplicationClose';
begin
  Result := True;
  try
    if Assigned(Self.Model()) then
      Result := Model().CanApplicationClose;
    if Result then
      ApplicationIsClosing;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAppModules.ApplicationIsClosing;
const OPNAME = 'TAppModules.ApplicationIsClosing';
begin
  try
    if Assigned(Self.Model()) then
      Self.Model().ApplicationIsClosing;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModules.Changes: TAbstractChangeManager;
const OPNAME = 'TAppModules.Changes';
begin
  Result := nil;
end;

function TAppModules.MetaData: TAbstractMetaDataManager;
const OPNAME = 'TAppModules.MetaData';
begin
  Result := nil;
end;

function TAppModules.WeatherEvents : TAbstractWeatherEventsManager;
const OPNAME = 'TAppModules.WeatherEvents';
begin
  Result := nil;
end;

function TAppModules.ScenarioLockManager: TAbstractScenarioLockManager;
const OPNAME = 'TAppModules.ScenarioLockManager';
begin
  Result := nil;
end;

{function TAppModules.LicenceManager: TAbstractLicenceManager;
const OPNAME = 'TAppModules.LicenceManager';
begin
  Result := nil;
end;}

function TAppModules.GetModelViewItems(AModelName: string;AItems: TStringListOfStringLists): boolean;
const OPNAME = 'TAppModules.GetModelViewItems';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppModules.StudyAreaManager: TAbstractStudyAreaManager;
const OPNAME = 'TAppModules.StudyAreaManager';
begin
  Result := nil;
end;

{ TAbstractAppObject }

constructor TAbstractAppObject.Create(AAppModules: TAppModules);
const OPNAME = 'TAbstractObject.CreateMemberObjects';
begin
  try

    // Assign and create these before calling the ancestor so that
    // they are available to decendants.
    FAppModules := AAppModules;
    FOwnedAppObjects := TAbstractAppObjectList.Create;
    FDLLHandle := 0;
    inherited Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractAppObject.DestroyMemberObjects;
const OPNAME = 'TAbstractAppObject.DestroyMemberObjects';
var LIndex, LDLLHandle: integer;
begin
  try
    inherited DestroyMemberObjects;

    // Destroy the list last.
    if Assigned(FOwnedAppObjects) then
    begin
      for LIndex := FOwnedAppObjects.Count - 1 downto 0 do
      begin
        LDLLHandle := TAbstractAppObject(FOwnedAppObjects.AppObject[LIndex]).FDLLHandle;
        FOwnedAppObjects.AppObject[LIndex].Free;
        if (LDLLHandle <> 0) then
          FreeLibrary(LDLLHandle);
      end;
      FreeAndNil(FOwnedAppObjects);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObject.Initialise: boolean;
const OPNAME = 'TAbstractAppObject.Initialise';
begin
  Result := True;
  try
    if Assigned(FOwnedAppObjects) then
      Result := FOwnedAppObjects.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObject.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractAppObject.LanguageHasChanged';
begin
  Result := True;
  try
    //FOwnedAppObjects.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObject.SaveState: boolean;
const OPNAME = 'TAbstractAppObject.SaveState';
begin
  Result := True;
  try
    FOwnedAppObjects.SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObject.StudyHasChanged: boolean;
const OPNAME = 'TAbstractAppObject.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObject.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TAbstractAppObject.StudyDataHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObject.ResetState: boolean;
const OPNAME = 'TAbstractAppObject.ResetState';
begin
  Result := True;
  try
    FOwnedAppObjects.ResetState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObject.GetDefaultKey : string;
const OPNAME = 'TAbstractAppObject.GetDefaultKey';
begin
  Result := '';
  try
    Result := 'Model='           + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ',StudyAreaName='  + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ',SubArea='        + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ',Scenario='       + QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractAppObjectList }

function TAbstractAppObjectList.GetAppObject(AIndex: integer): TAbstractAppObject;
const OPNAME = 'TAbstractAppObjectList.GetAppObject';
begin
  Result := nil;
  try
    Result := TAbstractAppObject(Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObjectList.Initialise: boolean;
const OPNAME = 'TAbstractAppObjectList.Initialise';
var LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to Count - 1 do
      if (not AppObject[LIndex].Initialise) then
        Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObjectList.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractAppObjectList.LanguageHasChanged';
var LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to Count - 1 do
      AppObject[LIndex].LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObjectList.ResetState: boolean;
const OPNAME = 'TAbstractAppObjectList.ResetState';
var LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to Count - 1 do
      AppObject[LIndex].ResetState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractAppObjectList.SaveState: boolean;
const OPNAME = 'TAbstractAppObjectList.SaveState';
var LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to Count - 1 do
      AppObject[LIndex].SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStudyDocumentDetail }

procedure TStudyDocumentDetail.Reset;
const OPNAME = 'TStudyDocumentDetail.Reset';
begin
  try
    FCategoryKey := '';
    FIdentifierKey := '';
    FFilename := '';
    FBookMark := '';
    FPageNumber := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDocumentDetail.AssignFrom(AObject: TObject);
const OPNAME = 'TStudyDocumentDetail.AssignFrom';
begin
  try
    Reset;
    if Assigned(AObject) then
    begin
      FCategoryKey   := TStudyDocumentDetail(AObject).CategoryKey;
      FIdentifierKey := TStudyDocumentDetail(AObject).IdentifierKey;
      FFilename      := TStudyDocumentDetail(AObject).Filename;
      FBookMark      := TStudyDocumentDetail(AObject).BookMark;
      FPageNumber    := TStudyDocumentDetail(AObject).PageNumber;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentDetail.ReportType: TReportTypes;
const OPNAME = 'TStudyDocumentDetail.ReportType';
var LFileExtension: string;
begin
  Result := rtNotSupported;
  try
    LFileExtension := UpperCase(ExtractFileExt(FFilename));
    if (LFileExtension = '.PDF') then
    begin
      Result := rtAcrobat;
    end else begin
      if (LFileExtension = '.DOC') then
      begin
        Result := rtWord;
      end else begin
        Result := rtNotSupported;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractSQLAgent }

//
// Loads the SQL contained in a resource block into the string list.
//
function TAbstractSQLAgent.LoadSQLFromResource(AResourceName: String; var ASQL: String; OPNAME: String): boolean;
var
  LSQL: TStringList;
  LStream: TResourceStream;
begin
  Result := False;
  try
    ASQL := '';
    LStream := TResourceStream.Create(HInstance, AResourceName, pointer(23));
    try
      LSQL := TStringList.Create;
      try
        LSQL.LoadFromStream(LStream);
        ASQL := CleanSQL(LSQL.Text, OPNAME);
        Result := True;
      finally
        LSQL.Free;
      end;
    finally
      LStream.Free;
    end;

  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Opens a database query.
//
function TAbstractSQLAgent.CleanSQL(ASQL, OPNAME: String): String;
var
  LSQL: string;
  I, LPos: integer;
  LLines: TStringList;
begin
  Result := '';
  try

    // Discard documentation.
    LLines := TStringList.Create;
    try
      LLines.Text := ASQL;
      for I := 0 to LLines.Count - 1 do
      begin
        LPos := Pos('--', LLines[I]);
        if (LPos > 0) then
          LLines[I] := Copy(LLines[I], 1, LPos - 1);
      end;
      ASQL := LLines.Text;
    finally
      LLines.Free;
    end;

    // Change into one long line.
    ASQL := StringReplace(ASQL, #13, ' ', [rfReplaceAll]);
    ASQL := StringReplace(ASQL, #10, ' ', [rfReplaceAll]);

    // Collapse the spaces.
    repeat
      LSQL := ASQL;
      ASQL := StringReplace(LSQL, '  ', ' ', [rfReplaceAll]);
    until (LSQL = ASQL);

    // Done.
    Result := Trim(ASQL);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Replaces all the parameters in the list.
//
procedure TAbstractSQLAgent.ReplaceSQLParameterList(ANameValues: TStringList; var ASQL: String; OPNAME: String);
var
  I: Integer;
  LOriginalSQL: String;
begin
  try
    LOriginalSQL := ASQL;
    for I := 0 to ANameValues.Count - 1 do
      ReplaceSQLParameter(ANameValues.Names[I], ANameValues.ValueFromIndex[I], ASQL, OPNAME);

  // Handle exceptions.
  except on E: Exception do
    LogSilentError('Error attempting to replace SQL parameters in [%s]. ' + E.Message, [LOriginalSQL], OPNAME);
  end;
end;


//
// Replaces all the parameters in the list.
//
procedure TAbstractSQLAgent.ReplaceSQLParameterList(ANames, AValues: array of const; var ASQL: String; OPNAME: String);
var
  I: Integer;
  LOriginalSQL: String;
begin
  try
    LOriginalSQL := ASQL;
    for I := 0 to Length(ANames) - 1 do
      ReplaceSQLParameter(string(ANames[I].VPChar), string(AValues[I].VPChar), ASQL, OPNAME);

  // Handle exceptions.
  except on E: Exception do
    LogSilentError('Error attempting to replace SQL parameters in [%s]. ' + E.Message, [LOriginalSQL], OPNAME);
  end;
end;


//
// Replaces a single parameter from the list.
//
procedure TAbstractSQLAgent.ReplaceSQLParameter(AParamName, AParamValue: String; var ASQL: String; OPNAME: String);
var
  LPos: Integer;
  LOriginalSQL, LNewSQL, LChar: String;
begin
  try
    LNewSQL := ASQL;
    LOriginalSQL := ASQL;
    LPos := Pos(AParamName, ASQL);
    if (LPos > 0) then
    begin

      // @p means replace the parameter as is.
      LNewSQL := StringReplace(LNewSQL, '@p' + AParamName, AParamValue, [rfReplaceAll, rfIgnoreCase]);

      // @a means alpha-numeric which needs quotes.
      LNewSQL := StringReplace(LNewSQL, '@a' + AParamName, QuotedStr(AParamValue), [rfReplaceAll, rfIgnoreCase]);

      // @n means numeric which must not contain quotes.
      LNewSQL := StringReplace(LNewSQL, '@n' + AParamName, AParamValue, [rfReplaceAll, rfIgnoreCase]);

      // @b means boolean which needs keywords.
      LChar := UpperCase(Copy(AParamValue, 1, 1));
      if ((AParamValue = '1') or (LChar = 'Y') or (LChar = 'T')) then
        LNewSQL := StringReplace(LNewSQL, '@b' + AParamName, 'TRUE', [rfReplaceAll, rfIgnoreCase])
      else
        LNewSQL := StringReplace(LNewSQL, '@b' + AParamName, 'FALSE', [rfReplaceAll, rfIgnoreCase]);

      // Done.
      ASQL := LNewSQL;
    end;

  // Handle exceptions.
  except on E: Exception do
    LogSilentError('Error attempting to replace SQL parameter [%s] with value [%s] in SQL [%s]. ' + E.Message,
                    [AParamName, AParamValue, LOriginalSQL], OPNAME);
  end;
end;

procedure TAbstractSQLAgent.ReplaceScenarioParameters(var ASQL: String; OPNAME: String);
begin
  try
    ReplaceSQLParameterList(
      ['Model',
       'StudyAreaName',
       'SubArea',
       'Scenario'],
      [FAppModules.StudyArea.ModelCode,
       FAppModules.StudyArea.StudyAreaCode,
       FAppModules.StudyArea.SubAreaCode,
       FAppModules.StudyArea.ScenarioCode],
       ASQL,
      OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ CreateDLLObject }

function CreateDLLObject(ADLLFileName: string; var AObject: TAbstractAppObject; AAppModules: TAppModules;
                         AReportError: boolean; ALocation: string): boolean;
type TConstructDLLObjectFunction = function (var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean; stdcall;
const OPNAME = 'UAbstractObject.CreateDLLObject';
{$IFNDEF MERGE_DLLS}
var
  LDLLHandle: longword;
  LConstructDLLObject: TConstructDLLObjectFunction;
{$ENDIF}
begin
  Result := False;
  AObject := nil;
  try

{$IFDEF MERGE_DLLS}
   if      (ExtractFileName(ADLLFileName) = 'AccessControl.dll')              then begin AObject := TAccessControlManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'Changes.dll')                    then begin AObject := TChangeManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'DailyDiversionPreProcessor.dll') then begin AObject := TDailyDiversionModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'FileEditor.dll')                 then begin AObject := TFileEditManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'FieldProperties.dll')            then begin AObject := TFieldPropertyManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'GridEditor.dll')                 then begin AObject := TGridEditorManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'GridOutputEditor.dll')           then begin AObject := TGridOutputEditorManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'HydrologyModel.dll')             then begin AObject := THydrologyModel.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'HydroNetworkVisualiser.dll')     then begin AObject := THydroNVManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'LanguageManager.dll')            then begin AObject := TLanguageManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'MetaData.dll')                   then begin AObject := TMetaDataManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelCapability.dll')            then begin AObject := TModelCapabilityManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelDDTS.dll')                  then begin AObject := TDDTSModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelHydrology.dll')             then begin AObject := TModelHydrologyManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelPlanning.dll')              then begin AObject := TPlanningModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelRainfallData.dll')          then begin AObject := TModelRainfallDataManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelRWH.dll')                   then begin AObject := TRWHModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelStomsa.dll')                then begin AObject := TStomsaModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelYield.dll')                 then begin AObject := TYieldModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ModelYRC.dll')                   then begin AObject := TModelYRCManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'MSAccessDatabaseLayer.dll')      then begin AObject := TAccessADODatabaseLayer.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'OutputComparison.dll')           then begin AObject := TOutputComparisonManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'OutputReview.dll')               then begin AObject := TOutputReviewManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'SedimentationPreProcessor.dll')  then begin AObject := TSedimentationModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'IFRPreProcessor.dll')            then begin AObject := TIFRModelManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'PrintManager.dll')               then begin AObject := TPrintManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'StudyDocuments.dll')             then begin AObject := TStudyDocumentManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'StudySelection.dll')             then begin AObject := TStudyAreaManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'TimeSeriesComparitor.dll')       then begin AObject := TTimeSeriesComparitorManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ViewDataGraph.dll')              then begin AObject := TViewDataGraphManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'ViewOutputGraph.dll')            then begin AObject := TViewOutputGraphManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'VisioNetworkVisualiser.dll')     then begin AObject := TNetworkVisualiserManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'WeatherEvents.dll')              then begin AObject := TWeatherEventsManager.Create(AAppModules) end
   else if (ExtractFileName(ADLLFileName) = 'YieldReliabilityCurve.dll')      then begin AObject := TYieldReliabilityCurveManager.Create(AAppModules) end

   else ShowMessage('Unknown DLL [' + ExtractFileName(ADLLFileName) + '].');
     ;
   Result := True;
{$ELSE}

    if LoadDLL(ADLLFileName, LDLLHandle, AReportError, ALocation) then
    begin
      if GetDLLFunction(LDLLHandle, ADLLFileName, 'ConstructDLLObject', @LConstructDLLObject, ALocation) then
      begin
        if LConstructDLLObject(TObject(AObject), AAppModules, HImagesInstance) then
        begin
          AObject.FDLLHandle := LDLLHandle;
          Result := True;
        end;
      end;
    end;

{$ENDIF}

  except on E: Exception do HandleErrorFunction(E, ALocation, Result) end;
end;

procedure DestroyDLLObject(var AObject: TAbstractAppObject; ALocation: string);
const OPNAME = 'UAbstractObject.DestroyDLLObject';
{$IFNDEF MERGE_DLLS}
var LDLLHandle: integer;
{$ENDIF}
begin
  try
{$IFDEF MERGE_DLLS}
    FreeAndNil(AObject);
{$ELSE}
    if Assigned(AObject) then
    begin
      LDLLHandle := AObject.DLLHandle;
      FreeAndNil(AObject);
      FreeLibrary(LDLLHandle);
    end;
{$ENDIF}
  except on E: Exception do HandleError(E, ALocation) end;
end;

function GetNumberOfDecimals(AValueType: TOutputDataType): integer;
const OPNAME = 'UAbstractObject.GetNumberOfDecimals';
begin
  Result := 0;
  try
    case AValueType of
      btMonthEndReservoirVolume:                Result := 2;
      btMonthEndReservoirElevation:             Result := 2;
      btNetBasinRunoffIntoResArea:              Result := 3;
      btRainfallOnReservoirSurface:             Result := 3;
      btGrossEvaporationLossFromReservoir:      Result := 3;
      btMonthlyAveragePowerFlow:                Result := 2;
      btMonthlyAverageSpillFlow:                Result := 2;
      btMonthlyAverageStackedCapacity:          Result := 2;
      btMonthlyAverageStackedEnergy:            Result := 2;
      btMonthlyAverageIrrigationDeficits:       Result := 3;
      btMonthlyAverageChannelFlow:              Result := 3;
      btMonthlyPumpingEnergy:                   Result := 1;

      //not sure
      btYieldFailurePerYearPerSequence:         Result := 0;
      btOutputSummary:                          Result := 0;
      btAnualFirmYieldDemands:                  Result := 0;
      btAnualFirmEnergyDemands:                 Result := 3;
      btAnualFirmSelectedYieldDemands:          Result := 3;
      btAnualNonFirmYieldDemands:               Result := 3;
      btAnualSecondaryYieldDemands:             Result := 3;
      btAnualTotalSystemPumpingEnergy:          Result := 3;
      btAnualFullSystemSupplyVolume:            Result := 3;
      btAnualAverageInflow:                     Result := 3;
      btFirmYieldRecurrenceInterval:            Result := 3;
      btNumberOfFailureSequence:                Result := 3;
      btCriticalPeriodsNumber:                  Result := 3;
      btCriticalPeriodsLength:                  Result := 3;
      btCriticalPeriodsDeficit:                 Result := 3;
      btCriticalPeriodsAvarage:                 Result := 3;
      btDeficitPropotion:                       Result := 3;
      btReservoirChannel:                       Result := 3;
      btReservoirStorage:                       Result := 3;
      btChannelFlowDeficit:                     Result := 3;
      btChannelSuppAndDeficitPerc:              Result := 3;
      btChannelSuppAndCompliencePerc:           Result := 3;
      btSequencesWithFailures:                  Result := 3;
      btIFRRequirementAndSupply:                Result := 3;
      btIFRFlow:                                Result := 3;
      btIFRRequirement:                         Result := 3;
      btIFRRequirementAndFlow:                  Result := 3;
      btRequirementAndSupplyDifference:         Result := 3;
      btDefinedAndSimulatedData:                Result := 3;
      btDefinedIFR:                             Result := 3;
      btDefinedReferenceflowVsIFRRelationship:  Result := 3;
      btIFRStats:                               Result := 3;
      btIFRHistogram:                           Result := 3;
      btIFRData:                                Result := 3;
      btDefinedAndRequired:                     Result := 3;
    else
      Result := 3;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetNumberFormat(AValueType: TOutputDataType;ALength: integer=0): string;
const OPNAME = 'UAbstractObject.GetNumberFormat';
begin
  Result := '%';
  try
    if(ALength <= 0) then
      ALength := 8;
    Result := Result + IntToStr(ALength) + '.';
    Result := Result + IntToStr(GetNumberOfDecimals(AValueType)) + 'f';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function GetFloatFormat(AValueType: TOutputDataType;ALength: integer=0): string;
const OPNAME = 'UAbstractObject.GetFloatFormat';
var
  LIndex  : integer;
  LDecimal : integer;
begin
  Result := '';
  try
    LDecimal := GetNumberOfDecimals(AValueType);
    if(LDecimal < 1) then
    begin
      if(ALength <= 0) then
        Result := '##0'
      else
      begin
        for LIndex := 1 to ALength do
          Result := Result + '0';
      end;
    end
    else
    begin
      if(ALength <= 0) then
      begin
        Result := '##0.';
        for LIndex := 1 to LDecimal do
          Result := Result + '0';
      end
      else
      begin
        Result := '0.';
        for LIndex := 1 to LDecimal do
         Result := Result + '0';
        while (Length(Result) <= ALength) do
          Result := '0' + Result;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function FormatOutputValue(AValueType: TOutputDataType; AValue:double;ALength: integer=0): string;
const OPNAME = 'UAbstractObject.FormatOutputValue';
var
  LFormat : string;
begin
  Result := '';
  try
    LFormat := GetFloatFormat(AValueType,ALength);
    Result  := FormatFloat(LFormat,AValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{ TAbstractModelManager }

function TAbstractModelManager.UpdateFieldValue(AFieldName, ANewValue,
  AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TAbstractModelManager.UpdateFieldValue';
var LFieldUpdateFunction: TFieldUpdateFunction;
begin
  Result := False;
  try
    LFieldUpdateFunction := GetFieldUpdateFunction(AFieldName);
    if Assigned(LFieldUpdateFunction) then
    begin
      Result := LFieldUpdateFunction(AFieldName, ANewValue, AOldValue, AContextData)
    end else begin
      if Assigned(FAppModules) and (Assigned(FAppModules.FieldProperties())) then
        Result := FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData);
    end;
    StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractModelManager.ApplicationIsClosing;
const OPNAME = 'TAbstractModelManager.ApplicationIsClosing';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.CanApplicationClose: boolean;
const OPNAME = 'TAbstractModelManager.CanApplicationClose';
begin
  Result := true;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string;
const OPNAME = 'TAbstractModelManager.GetFileLineType';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.IsGraphLoaded: boolean;
const OPNAME = 'TAbstractModelManager.IsGraphLoaded';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.IsGridLoaded: boolean;
const OPNAME = 'TAbstractModelManager.IsGridLoaded';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TAbstractModelManager.CanCopyToCLipboard';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.CanExport: boolean;
const OPNAME = 'TAbstractModelManager.CanExport';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.CanPrint: boolean;
const OPNAME = 'TAbstractModelManager.CanPrint';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.EntityDescription (AFieldPropName : string;
                                                  AKeyValues     : string;
                                                  AFieldIndex    : string) : string;
const OPNAME = 'TAbstractModelManager.EntityDescription';
begin
  Result := 'Entity Unknown';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.SetBaseValue (AFieldPropName : string;
                                             AKeyValues     : string;
                                             AFieldIndex    : string;
                                             ANewValue      : string) : boolean;
const OPNAME = 'TAbstractModelManager.SetBaseValue';
begin
  Result := FALSE;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.GetBaseValue (AFieldPropName : string;
                                             AKeyValues     : string;
                                             AFieldIndex    : string) : string;
const OPNAME = 'TAbstractModelManager.GetBaseValue';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractModelManager.DoCopyToCLipboard;
const OPNAME = 'TAbstractModelManager.DoCopyToCLipboard';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractModelManager.DoExport(AFileName: string = '');
const OPNAME = 'TAbstractModelManager.DoExport';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractModelManager.DoPrint;
const OPNAME = 'TAbstractModelManager.DoPrint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.GetModelViewItems(AItems: TStringListOfStringLists): boolean;
const OPNAME = 'TAbstractModelManager.GetModelViewItems';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.GetModelDataSetKey : string;
const OPNAME = 'TAbstractModelManager.GetModelDataSetKey';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.GetChangeListWhereClause: string;
const OPNAME = 'TAbstractModelManager.GetChangeListWhereClause';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.DeleteModelDataViewer(AParent, AOwner: TObject): boolean;
const OPNAME = 'TAbstractModelManager.DeleteModelDataViewer';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.ViewInputDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TAbstractModelManager.ViewInputDialog';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.ViewInputPopupDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TAbstractModelManager.ViewInputPopupDialog';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.ViewOutputDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TAbstractModelManager.ViewOutputDialog';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.ViewOutputPopupDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TAbstractModelManager.ViewOutputPopupDialog';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelManager.ViewOutputComparisonDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TAbstractModelManager.ViewOutputComparisonDialog';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TStudyDataHasChangedData }

procedure TStudyDataHasChangedData.CreateMemberObjects;
const OPNAME = 'TStudyDataHasChangedData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDataHasChangedData.DestroyMemberObjects;
const OPNAME = 'TStudyDataHasChangedData.DestroyMemberObjects';
begin
  try
    Reset;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDataHasChangedData.Reset;
const OPNAME = 'TStudyDataHasChangedData.Reset';
begin
  try
    FFieldProperty := nil;
    FAction        := saNone;
    FSeverity      := scsNone;
    FOldValue      := '';
    FNewValue      := '';
    FChangedBy     := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractFieldProperty }

function TAbstractFieldProperty.GetSmartFormat: boolean;
const OPNAME = 'TAbstractFieldProperty.GetSmartFormat';
begin
  Result := False;
  try
    Result := (UpperCase(FSmartFormat) = 'Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStudyFields }


procedure TStudyFields.AfterConstruction;
const OPNAME = 'TStudyFields.AfterConstruction';
begin
  inherited;
  Reset;
end;

constructor TStudyFields.Create;
const OPNAME = 'TStudyFields.Create';
begin

end;

destructor TStudyFields.Destroy;
const OPNAME = 'TStudyFields.Destroy';
begin

  inherited;
end;

procedure TStudyFields.Reset;
const OPNAME = 'TStudyFields.Reset';
begin
  FModel := '';
  FSubModel := '';
  FSubArea := '';
  FScenario := '';
  FStudyAreaName := '';

  FStudyDate := 0.0;
  FConsultant := '';
  FClient := '';
  FStudyNumber := '';
  FStudyLabel := '';
  FStudyAreaDescr := '';

  FSubAreaLabel := '';
  FSubAreaDescr := '';
  FTopLeftCoord         := 0.0;
  FTopRightCoord        := 0.0;
  FBottomLeftCoord      := 0.0;
  FBottomRightCoord     := 0.0;

  FScenarioLabel   := '';
  FScenarioDescr   := '';
  FDataFilesPrefix := '';
  FDataFilesPath   := '';
  FFilesLoaded     := False;
  FCalenderStartMonth := 10;
  FVersion            := '';

  FStudyShapeFileName := '';
  FSubAreaShapeFileName := '';

  FEditable := True;


end;

end.
