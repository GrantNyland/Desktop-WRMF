unit UAbstractHydrologyModelData;

interface
  uses

    Classes,
    Contnrs,

    UAbstractObject,
    UAbstractStudyConfigurationData,
//    UAbstractYieldModelDataObject,
    UAbstractModelCalendar,
    UAbstractModelData;
type

  TAbstractModelFileName = class(TAbstractAppObject)
  protected
    function GetFileGroup : integer; virtual; abstract;
    function GetFileNumber : integer; virtual; abstract;
    function GetCurrentFullName : string; virtual; abstract;
    function GetCurrentPath : string; virtual; abstract;
    function GetChanged : Boolean; virtual; abstract;
    function GetSavedInDB : Boolean; virtual; abstract;
    function GetUpdate : Boolean; virtual; abstract;
    function GetSelected : Boolean; virtual; abstract;
    function GetHintsSet : Boolean; virtual; abstract;
    function GetCurrentName : string; virtual; abstract;
    function GetFileReadOnly : Boolean; virtual; abstract;
    procedure SetChanged(AChanged : Boolean); virtual; abstract;
    procedure SetCurrentName(ACurrentName : string); virtual; abstract;
    procedure SetSavedInDB(ASavedInDB : Boolean); virtual; abstract;
    procedure SetUpdate(AUpdate : Boolean); virtual; abstract;
    procedure SetSelected(ASelected : Boolean); virtual; abstract;
    procedure SetHintsSet(AHintsSet : Boolean); virtual; abstract;
    procedure SetFileGroup(AFileGroup : integer); virtual; abstract;
    procedure SetFileReadOnly(AReadOnly : Boolean); virtual; abstract;
  public
    function CheckForEqualityTrue(AFileNameObject:TAbstractModelFileName): boolean; virtual; abstract;
    function FileFound: boolean; virtual; abstract;
    function FileDateHasChanged(AFileName: string): boolean; virtual; abstract;
    function StandardFileName: string;  virtual; abstract;
    property FileGroup: integer   read GetFileGroup;
    property FileNumber: integer  read GetFileNumber;
    property FileName: string     read GetCurrentFullName;
    property FilePath: string     read GetCurrentPath;
    property ShortName: string    read GetCurrentName write SetCurrentName;
    property SavedInDB: boolean        read GetSavedInDB       write SetSavedInDB;
    property Changed: boolean          read GetChanged         write SetChanged;
    property Update: boolean           read GetUpdate          write SetUpdate;
    property Selected: boolean         read GetSelected        write SetSelected;
    property HintsSet: boolean         read GetHintsSet        write SetHintsSet;
    property FileReadOnly: boolean     read GetFileReadOnly    write SetFileReadOnly;
  end;

  TAbstractFileNamesList = class(TAbstractAppObject)
  protected
    function GetFileObject(AIndex: integer): TAbstractModelFileName; virtual; abstract;
    function GetCaptionStr: string; virtual; abstract;
    procedure SetCaptionStr(ACaptionStr: string); virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    function Count: integer; virtual; abstract;
    procedure Add(AObject: TObject); virtual; abstract;
    function FindFile(AFileName: string): TAbstractModelFileName; virtual; abstract;
    function FilesCount: integer;  virtual; abstract;
    function SelectedCount: integer; virtual; abstract;
    function FindFilesFromPrefix(AFilePrefix: string; AFilesList: TStringList): boolean; virtual; abstract;
    property FileNameObject[AIndex: integer]: TAbstractModelFileName read GetFileObject;
    property CaptionStr: string read GetCaptionStr write SetCaptionStr;
  end;

  TAbstractParamReference = class ( TAbstractAppObject )
  protected
   function GetCatchReference:integer; virtual; abstract;
   function GetFileReference : string; virtual; abstract;
  public
    property CatchReference: integer read GetCatchReference;
    property FileReference: string read GetFileReference;
  end;

  TAbstractParamSetup = class( TAbstractAppObject )
  protected
    function GetAllReferenceData:TObjectList; virtual; abstract;
    function GetReferenceDataByCatchNumber(ACatchReferenceNumber:integer):TAbstractParamReference; virtual; abstract;
    function GetReferenceDataByIndex(AIndex:integer):TAbstractParamReference; virtual; abstract;
    function GetReferenceCount: integer;virtual; abstract;
  public
    property ReferenceCount: integer read GetReferenceCount;
    property ReferenceDataByIndex[AIndex:integer]:TAbstractParamReference read GetReferenceDataByIndex;
    property ReferenceDataByCatchNumber[ARefNumber:integer]:TAbstractParamReference read GetReferenceDataByCatchNumber;
    property AllReferenceData: TObjectList read GetAllReferenceData;
  end;

  TAbstractModelFileNameList = class(TAbstractAppObject)
  protected
    function GetOutputFileNames    : TAbstractFileNamesList; virtual; abstract;
    function GetConfigFileNames    : TAbstractFileNamesList; virtual; abstract;
    function GetDirectoryFileNames : TAbstractFileNamesList; virtual; abstract;
    function GetParamFileNames     : TAbstractFileNamesList; virtual; abstract;
    function GetDemandFileNames    : TAbstractFileNamesList; virtual; abstract;
    function GetHydrologyFileNames : TAbstractFileNamesList; virtual; abstract;
    function GetParamFileName      : string; virtual; abstract;
    procedure SetParamFileName(AFileName : string); virtual; abstract;
    function GetDirectoryFileName      : string; virtual; abstract;
    procedure SetDirectoryFileName(AFileName : string); virtual; abstract;
  public
    function FilesCount: integer; virtual; abstract;
    function FilesSavedInDatabaseCount: integer; virtual; abstract;
    function SelectedCount: integer; virtual; abstract;
    function GetSumOoutFile:TAbstractModelFileName; virtual; abstract;
    property ParamFileName      : string read GetParamFileName     write SetParamFileName;
    property DirectoryFileName  : string read GetDirectoryFileName write SetDirectoryFileName;
    property OutputFileNames    : TAbstractFileNamesList read GetOutputFileNames;
    property ConfigFileNames    : TAbstractFileNamesList read GetConfigFileNames;
    property ParamFileNames     : TAbstractFileNamesList read GetParamFileNames;
    property DirectoryFileNames : TAbstractFileNamesList read GetDirectoryFileNames;
    property DemandFileNames    : TAbstractFileNamesList read GetDemandFileNames;
    property HydrologyFileNames : TAbstractFileNamesList read GetHydrologyFileNames;
  end;

  TAbstractFileLineTypesObject = class(TObject)
  protected
    function GetFileNameObject: TAbstractModelFileName; virtual; abstract;
  public
    function GetFileLineType(ALineNumber: Integer): string; virtual; abstract;
    function LinesCount: integer; virtual; abstract;
    property FileNameObject: TAbstractModelFileName  read GetFileNameObject;
  end;

  TAbstractFilesLineTypes = class(TObject)
  protected
    function GetFileLineTypesObject(AFileObject: TAbstractModelFileName): TAbstractFileLineTypesObject; virtual; abstract;
  public
    function FilesCount: integer; virtual; abstract;
    function GetFileLineType(AFileObject: TAbstractModelFileName; ALineNumber: Integer): string;virtual; abstract;
    property FileLineTypesObject[AFileObject: TAbstractModelFileName]: TAbstractFileLineTypesObject
      read GetFileLineTypesObject;
  end;

  TAbstractHygrologyModelDataObject = class(TAbstractModelData)
  protected
//    function GetParamSetup: TAbstractParamSetup; virtual; abstract;
    function GetFileNamesObject: TAbstractModelFileNameList; virtual; abstract;
//    function GetConfigurationsData: TAbstractConfigurationsData; virtual; abstract;
  public
    function GetStudyConfigurationData: TAbstractStudyConfigurationData;  virtual; abstract;
//    function GetNetworkElementData: TAbstractNetworkElementData;  virtual; abstract;
    function GetModelCalendar: AbstractModelCalendar;  virtual; abstract;
    function GetHydrologyFilesForCatchment(ACatchmentRef: integer;AFilesNamesContainer: TStringList): boolean; virtual; abstract;
    function GetHydrologyFileDataSet(AHydrologyFileName: string;ADataSet:TAbstractModelDataset): boolean; virtual; abstract;
    function GetDatasetFileDataSet(ADemandFileName: string;ADataSet:TAbstractModelDataset): boolean; virtual; abstract;

    property StudyConfigurationData: TAbstractStudyConfigurationData read GetStudyConfigurationData;
//    property NetworkElementData: TAbstractNetworkElementData read GetNetworkElementData;
//    property NetworkFeaturesData: TAbstractNetworkFeaturesData read GetNetworkFeaturesData;
    property ModelCalendar: AbstractModelCalendar read GetModelCalendar;
//    property ParamSetup: TAbstractParamSetup read GetParamSetup;
    property FileNamesObject: TAbstractModelFileNameList read GetFileNamesObject;
  //  property ConfigurationsData: TAbstractConfigurationsData read GetConfigurationsData;
//    property YRCGraphDataObject : TAbstractYRCGraphDataObject read GetYRCGraphDataObject;
//    property YieldModelCapability:TAbstractYieldModelCapability read GetYieldModelCapability;
  end;

  TAbstractHydrologyModelDataAgent = class(TAbstractAppObject)
  protected
    function GetCastModelData: TAbstractHygrologyModelDataObject;
    property ModelData: TAbstractHygrologyModelDataObject read GetCastModelData;
  end;

implementation
uses
  UErrorHandlingOperations,
  SysUtils;

{ TAbstractYieldModelDataAgent }

function TAbstractHydrologyModelDataAgent.GetCastModelData: TAbstractHygrologyModelDataObject;
const OPNAME = 'TAbstractHydrologyModelDataAgent.GetCastModelData';
begin
  Result := nil;
  try
    Result := TAbstractHygrologyModelDataObject ( FAppModules.Model.ModelData );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
