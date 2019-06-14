//
//
//  UNIT      : Contains TAbstractYRCModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2003/09/18
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UAbstractYRCModelDataObject;

interface

uses
  Classes, SysUtils, Contnrs,
  UAbstractObject,
  UAbstractModelData,
  UAbstractYRCData,
  UAbstractFileNamesObject;

type

  {
  //_________________This must be moved to a unit ________________________________________
  TChartSeriesType = (cstLabelSeries, cstLineSeries, cstLineSpecialTicsSeries);
  TChartMode = (cmPlane,cmView,cmManipulating);
  TPenWidth = (pwThin,pwThick);
  TCurveGroupType = (ctPureRegression,ctRegression,ctDeterministic,ctOriginal);

  TIntegerArray = array of integer;
  TMonthlyDoubleArray = array[1..12] of double;
  TFifteenDoubleArray = array [1..15] of double;
  TEightDoubleArray  = array [1..8] of double;
  //______________________________________________________________________________________

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
  end; }

  //
  // The yield model data object container.
  //
  TAbstractYRCModelDataObject = class(TAbstractModelData)
  protected
    function GetYRCGraphDataObject: TAbstractYRCGraphDataObject; virtual; abstract;
  public
    property YRCGraphDataObject : TAbstractYRCGraphDataObject read GetYRCGraphDataObject;
  end;

implementation
uses
  UErrorHandlingOperations;

{ TAbstractYieldModelDataAgent }

end.
