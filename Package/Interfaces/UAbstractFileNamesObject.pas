//
//
//  UNIT      : Contains TAbstractYieldModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2003/09/18
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UAbstractFileNamesObject;

interface

uses
  Classes, SysUtils, Contnrs,
  UAbstractObject;

type

  TAbstractModelFileName = class(TAbstractAppObject)
  protected
    function GetFileGroup : integer; virtual; abstract;
    function GetFileNumber : integer; virtual; abstract;
    function GetFullFileName : string; virtual; abstract;
    function GetShortFileName : string; virtual; abstract;
    function GetFilePath : string; virtual; abstract;
    function GetChanged : Boolean; virtual; abstract;
    function GetSavedInDB : Boolean; virtual; abstract;
    function GetUpdate : Boolean; virtual; abstract;
    function GetSelected : Boolean; virtual; abstract;
    function GetHintsSet : Boolean; virtual; abstract;
    function GetFileReadOnly : Boolean; virtual; abstract;
    function GetFileDate : TDateTime; virtual; abstract;
    function GetImportDate : TDateTime; virtual; abstract;
    function GetImportable : Boolean; virtual; abstract;
    function GetExportable : Boolean; virtual; abstract;
    function GetValidatable : Boolean; virtual; abstract;
    function GetPopulated : Boolean; virtual; abstract;
    procedure SetChanged(AChanged : Boolean); virtual; abstract;
    procedure SetFullFileName(ACurrentName : string); virtual; abstract;
    procedure SetSavedInDB(ASavedInDB : Boolean); virtual; abstract;
    procedure SetUpdate(AUpdate : Boolean); virtual; abstract;
    procedure SetSelected(ASelected : Boolean); virtual; abstract;
    procedure SetHintsSet(AHintsSet : Boolean); virtual; abstract;
    procedure SetFileGroup(AFileGroup : integer); virtual; abstract;
    procedure SetFileReadOnly(AReadOnly : Boolean); virtual; abstract;
    procedure SetFileDate(ADate : TDateTime); virtual; abstract;
    procedure SetImportDate(ADate : TDateTime); virtual; abstract;
    procedure SetImportable(AImportable : Boolean); virtual; abstract;
    procedure SetExportable(AExportable : Boolean); virtual; abstract;
    procedure SetValidatable(AValidatable : Boolean); virtual; abstract;
  public
    function CheckForEqualityTrue(AFileNameObject:TAbstractModelFileName): boolean; virtual; abstract;
    function FileFound: boolean; virtual; abstract;
    function FileDateHasChanged(AFileName: string): boolean; virtual; abstract;
    function StandardFileName: string;  virtual; abstract;
    property FileGroup: integer        read GetFileGroup;
    property FileNumber: integer       read GetFileNumber;
    property FilePath: string          read GetFilePath;
    property ShortName: string         read GetShortFileName;
    property FileName: string          read GetFullFileName    write SetFullFileName;
    property SavedInDB: boolean        read GetSavedInDB       write SetSavedInDB;
    property Changed: boolean          read GetChanged         write SetChanged;
    property Update: boolean           read GetUpdate          write SetUpdate;
    property Selected: boolean         read GetSelected        write SetSelected;
    property HintsSet: boolean         read GetHintsSet        write SetHintsSet;
    property FileReadOnly: boolean     read GetFileReadOnly    write SetFileReadOnly;
    property FileDate: TDateTime       read GetFileDate        write SetFileDate;
    property ImportDate: TDateTime     read GetImportDate      write SetImportDate;
    property Importable: boolean       read GetImportable      write SetImportable;
    property Exportable: boolean       read GetExportable      write SetExportable;
    property Validatable: boolean      read GetValidatable     write SetValidatable;
    property Populated: boolean        read GetPopulated;
  end;

  TAbstractFileNamesList = class(TAbstractAppObject)
  protected
    function GetFileObject(AIndex: integer): TAbstractModelFileName; virtual; abstract;
    function GetCaptionStr: string; virtual; abstract;
    procedure SetCaptionStr(ACaptionStr: string); virtual; abstract;
  public
    function HighestFileNumber : integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Count: integer; virtual; abstract;
    procedure Add(AObject: TObject); virtual; abstract;
    function FindFile(AFileName: string): TAbstractModelFileName; virtual; abstract;
    function FilesCount: integer;  virtual; abstract;
    function SelectedCount: integer; virtual; abstract;
    function FindFilesFromPrefix(AFilePrefix,AFileExt: string; AFilesList: TStringList): boolean; virtual; abstract;
    function DeleteAllFiles : boolean;  virtual; abstract;
    property FileNameObject[AIndex: integer]: TAbstractModelFileName read GetFileObject;
    property CaptionStr: string read GetCaptionStr write SetCaptionStr;
  end;

  TAbstractModelFileNameList = class(TAbstractAppObject)
  protected
    //Yield files
    function GetConfigFileNames           : TAbstractFileNamesList; virtual; abstract;
    function GetDirectoryFileNames        : TAbstractFileNamesList; virtual; abstract;
    function GetParamFileNames            : TAbstractFileNamesList; virtual; abstract;
    function GetAltParamFileNames         : TAbstractFileNamesList; virtual; abstract;
    function GetDemandFileNames           : TAbstractFileNamesList; virtual; abstract;
    function GetHydrologyFileNames        : TAbstractFileNamesList; virtual; abstract;
    function GetOutputFileNames           : TAbstractFileNamesList; virtual; abstract;
    function GetDamLevelsFileNames        : TAbstractFileNamesList; virtual; abstract;



    //PLanning files
    function GetAllocationDefinitionFileNames    : TAbstractFileNamesList; virtual; abstract;
    function GetReservoirImplementationFileNames : TAbstractFileNamesList; virtual; abstract;
    function GetDisbenefitDefinitionFileNames    : TAbstractFileNamesList; virtual; abstract;
    function GetGrowthFactorsFileNames           : TAbstractFileNamesList; virtual; abstract;
    function GetMonthlyWaterRequirementFileNames : TAbstractFileNamesList; virtual; abstract;
    function GetHydropowerAllocationFileNames    : TAbstractFileNamesList; virtual; abstract;
    function GetPumpingChannelControlFileNames   : TAbstractFileNamesList; virtual; abstract;
    function GetGeneralChannelControlFileNames   : TAbstractFileNamesList; virtual; abstract;
    function GetReclamationPlantControlFileNames : TAbstractFileNamesList; virtual; abstract;
    function GetReturnFlowChannelFileNames       : TAbstractFileNamesList; virtual; abstract;
    function GetChannelSwitchControlFileNames    : TAbstractFileNamesList; virtual; abstract;
    function GetTariffCalculationFileNames       : TAbstractFileNamesList; virtual; abstract;
    function GetAllocationChannelFileNames       : TAbstractFileNamesList; virtual; abstract;
    function GetReleaseStructureFileNames        : TAbstractFileNamesList; virtual; abstract;
    function GetCurtailFileNames                 : TAbstractFileNamesList; virtual; abstract;
    function GetSaltsWashoffFileNames            : TAbstractFileNamesList; virtual; abstract;

    function GetMineFileNames                    : TAbstractFileNamesList; virtual; abstract;
    function GetMineRainfallFileNames            : TAbstractFileNamesList; virtual; abstract;

    // Daily Diversion
    function GetDailyDataFlowFileNames           : TAbstractFileNamesList; virtual; abstract;
    function GetDailyInstreamFlowFileNames       : TAbstractFileNamesList; virtual; abstract;

    function GetParamFileName     : string; virtual; abstract;
    function GetAltParamFileName     : string; virtual; abstract;
    function GetDirectoryFileName : string; virtual; abstract;
    procedure SetParamFileName(AFileName : string); virtual; abstract;
    procedure SetAltParamFileName(AFileName : string); virtual; abstract;
    procedure SetDirectoryFileName(AFileName : string); virtual; abstract;


    function GetFileNamePrefix: string;virtual; abstract;
    procedure SetFileNamePrefix(APrefix: string);virtual; abstract;
    function GetInputFilesPath: string;virtual; abstract;
    procedure SetInputFilesPath(APath: string);virtual; abstract;
    function GetOutputFilesPath: string;virtual; abstract;
    procedure SetOutputFilesPath(APath: string);virtual; abstract;
    function GetHydrologyFilesPath: string;virtual; abstract;
    procedure SetHydrologyFilesPath(APath: string);virtual; abstract;
    function GetDemandFilesPath: string;virtual; abstract;
    procedure SetDemandFilesPath(APath: string);virtual; abstract;
    function GetDamLevelsPath: string;virtual; abstract;
    procedure SetDamLevelsPath(APath: string);virtual; abstract;
    procedure SetSaltsWashoffPath(APath: string);virtual; abstract;

  public
    function FilesCount: integer; virtual; abstract;
    function FilesSavedInDatabaseCount: integer; virtual; abstract;
    function SelectedCount: integer; virtual; abstract;
    function GetSumOutFile:TAbstractModelFileName; virtual; abstract;
    function GetPlotOutFile:TAbstractModelFileName; virtual; abstract;
    function GetPumpOutFile:TAbstractModelFileName; virtual; abstract;
    function GetDemandOutFile:TAbstractModelFileName; virtual; abstract;
    function GetRunoffFileNamesCommaText:string; virtual; abstract;
    function GetSoilMoistureFileNamesCommaText:string; virtual; abstract;
    function SelectAllFiles(ASelected: boolean): boolean; virtual; abstract;

    // Paths
    property FileNamePrefix : string read GetFileNamePrefix write SetFileNamePrefix;
    property InputFilesPath : string read GetInputFilesPath write SetInputFilesPath;
    property OutputFilesPath : string read GetOutputFilesPath write SetOutputFilesPath;
    property HydrologyFilesPath : string read GetHydrologyFilesPath write SetHydrologyFilesPath;
    property DemandFilesPath : string read GetDemandFilesPath write SetDemandFilesPath;
    property DamLevelsPath : string read GetDamLevelsPath write SetDamLevelsPath;



    property ParamFileName              : string read GetParamFileName     write SetParamFileName;
    property AltParamFileName           : string read GetAltParamFileName  write SetAltParamFileName;
    property DirectoryFileName          : string read GetDirectoryFileName write SetDirectoryFileName;

    //Yield files
    property ConfigFileNames            : TAbstractFileNamesList read GetConfigFileNames;
    property ParamFileNames             : TAbstractFileNamesList read GetParamFileNames;
    property AltParamFileNames          : TAbstractFileNamesList read GetAltParamFileNames;
    property DirectoryFileNames         : TAbstractFileNamesList read GetDirectoryFileNames;
    property DemandFileNames            : TAbstractFileNamesList read GetDemandFileNames;
    property HydrologyFileNames         : TAbstractFileNamesList read GetHydrologyFileNames;
    property OutputFileNames            : TAbstractFileNamesList read GetOutputFileNames;
    property DamLevelsFileNames         : TAbstractFileNamesList read GetDamLevelsFileNames;

    // Daily Diversion
    property DailyDataFlowFileNames         : TAbstractFileNamesList read GetDailyDataFlowFileNames;
    property DailyInstreamFlowFileNames     : TAbstractFileNamesList read GetDailyInstreamFlowFileNames;


    //PLanning files
    property AllocationDefinitionFileNames    : TAbstractFileNamesList read GetAllocationDefinitionFileNames    ;
    property ReservoirImplementationFileNames : TAbstractFileNamesList read GetReservoirImplementationFileNames ;
    property DisbenefitDefinitionFileNames    : TAbstractFileNamesList read GetDisbenefitDefinitionFileNames    ;
    property GrowthFactorsFileNames           : TAbstractFileNamesList read GetGrowthFactorsFileNames           ;
    property MonthlyWaterRequirementFileNames : TAbstractFileNamesList read GetMonthlyWaterRequirementFileNames ;
    property HydropowerAllocationFileNames    : TAbstractFileNamesList read GetHydropowerAllocationFileNames    ;
    property PumpingChannelControlFileNames   : TAbstractFileNamesList read GetPumpingChannelControlFileNames   ;
    property GeneralChannelControlFileNames   : TAbstractFileNamesList read GetGeneralChannelControlFileNames   ;
    property ReclamationPlantControlFileNames : TAbstractFileNamesList read GetReclamationPlantControlFileNames ;
    property ReturnFlowChannelFileNames       : TAbstractFileNamesList read GetReturnFlowChannelFileNames       ;
    property ChannelSwitchControlFileNames    : TAbstractFileNamesList read GetChannelSwitchControlFileNames    ;
    property TariffCalculationFileNames       : TAbstractFileNamesList read GetTariffCalculationFileNames       ;
    property AllocationChannelFileNames       : TAbstractFileNamesList read GetAllocationChannelFileNames       ;
    property ReleaseStructureFileNames        : TAbstractFileNamesList read GetReleaseStructureFileNames        ;
    property CurtailFileNames                 : TAbstractFileNamesList read GetCurtailFileNames;
    property SaltsWashoffFileNames            : TAbstractFileNamesList read GetSaltsWashoffFileNames;

    property MineFileNames                    : TAbstractFileNamesList read GetMineFileNames;
    property MineRainfallFileNames            : TAbstractFileNamesList read GetMineRainfallFileNames;

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

implementation

end.
