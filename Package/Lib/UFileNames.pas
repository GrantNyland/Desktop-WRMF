//
//
//  UNIT      : Contains TFileName Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/12/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UFileNames;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UAbstractObject,
  UAbstractDataObject,//must be deleted
  UBasicObjects,//Must be deleted
  UGenericModelLinkClasses,
  UAbstractFileNamesObject;

type
//  TFileNameObject = class(TAbstractModelLinkClass)
  TFileNameObject = class(TAbstractModelFileName)
  protected
    FCurrentPath    : string;
    FFileName       : string;
    FSavedInDB      : Boolean;
    FChanged        : Boolean;
    FUpdate         : boolean;
    FSelected       : boolean;
    FHintsSet       : boolean;
    FFileGroup      : integer;
    FFileNumber     : integer;
    FFileReadOnly   : boolean;
    FFileDate       : TDateTime;
    FImportDate     : TDateTime;
    FImportable     : boolean;
    FExportable     : boolean;
    FValidatable    : boolean;
    FPopulated      : boolean;
    procedure CreateMemberObjects; override;
    function GetChanged : Boolean; override;
    function GetFileGroup : integer; override;
    function GetFileNumber : integer; override;
    function GetShortFileName : string; override;
    function GetFullFileName : string; override;
    procedure SetFullFileName(ACurrentName : string); override;
    function GetFilePath : string; override;
    function GetSavedInDB : Boolean; override;
    function GetUpdate : Boolean; override;
    function GetSelected : Boolean; override;
    function GetHintsSet : Boolean; override;
    function GetFileReadOnly : Boolean; override;
    function GetFileDate : TDateTime; override;
    function GetImportDate : TDateTime; override;
    function GetImportable : Boolean; override;
    function GetExportable : Boolean; override;
    function GetValidatable : Boolean; override;
    function GetPopulated : Boolean; override;
    procedure SetChanged(AChanged : Boolean); override;
    procedure SetSavedInDB(ASavedInDB : Boolean); override;
    procedure SetUpdate(AUpdate : Boolean); override;
    procedure SetSelected(ASelected : Boolean); override;
    procedure SetHintsSet(AHintsSet : Boolean); override;
    procedure SetFileGroup(AFileGroup : integer); override;
    procedure SetFileReadOnly(AReadOnly : Boolean); override;
    procedure SetFileDate(ADate : TDateTime); override;
    procedure SetImportDate(ADate : TDateTime); override;
    procedure SetImportable(AImportable : Boolean); override;
    procedure SetExportable(AExportable : Boolean); override;
    procedure SetValidatable(AValidatable : Boolean); override;
  public
    procedure LoadContextData(AContextData: TStringList);
    procedure Reset;
    procedure SetFileName(AFilePath,AFileName: string; AFromDB: boolean;AFileGroup,AFileNumber: integer;
              AImportDate : TDateTime;AFileDate : TDateTime);
    function AssignFrom(AFileNameObject: TFileNameObject): boolean;
    function FileFound: boolean; override;
    function FileDateHasChanged(AFileName: String): boolean; override;
    function StandardFileName: string;  override;
    function CheckForEqualityTrue(AFileNameObject:TAbstractModelFileName): boolean; override;
    function FileHasBeenEdited: boolean;
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

  TFileNamesList = class(TAbstractFileNamesList)
  protected
    FCaptionStr: string;
    FFileList  : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetCaptionStr: string; override;
    procedure SetCaptionStr(ACaptionStr: string); override;
    function GetFileObject(AIndex: integer): TAbstractModelFileName; override;
    function GetCastFileObject(AIndex: integer): TFileNameObject;
    function AddFileObject(AFileNameObject: TFileNameObject): boolean;
    function DeleteFileName(AFileNumber: integer): boolean;
    function AddFileName(AFilePath,AFileName: string; AFromDB: boolean;AFileGroup,AFileNumber: integer;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;
  public
    //procedure Reset;
    procedure LoadContextData(AContextData: TStringList);
    function HighestFileNumber : integer; override;
    procedure Clear; override;
    function Count: integer; override;
    procedure Add(AObject: TObject); override;
    function AssignFrom(AFileNames: TFileNamesList): boolean;
    function FilesCount: integer;  override;
    function SelectedCount: integer; override;
    function FilesSavedInDatabaseCount: integer;
    function SetAllSelected(ASelected: boolean): boolean;
    function SetFileHintsSet(ASetStatus: boolean): boolean;
    function FindFile(AFileName: String):TAbstractModelFileName; override;
    function FindFilesFromPrefix(AFilePrefix,AFileExt: string; AFilesList: TStringList): boolean; override;
    function UpdateFilesPath(APath:string):boolean;
    function DeleteAllFiles : boolean;  override;
    property CaptionStr: string read FCaptionStr write FCaptionStr;
    property FileNameObject[AIndex: integer]:TAbstractModelFileName read GetFileObject;default;
    property CastFileObject[AIndex: integer]:TFileNameObject read GetCastFileObject;

  end;
  {
  TFileGroups = class(TList)
    function GetFileGroup(AIndex: integer):TFileNamesList;
  public
    property FileGroup[AIndex: integer]:TFileNamesList read GetFileGroup;default;
  end;}
  TModelFileNames = class(TAbstractModelFileNameList)
  protected
    FFileNamePrefix            : string;
    FInputFilesPath            : string;
    FOutputFilesPath           : string;
    FHydrologyFilesPath        : string;
    FDemandFilesPath           : string;
    FDamLevelsPath             : string;
    FSaltsWashoffPath          : string;
    FConfigFileNames           : TFileNamesList;
    FParamFileNames            : TFileNamesList;
    FAltParamFileNames         : TFileNamesList;
    FDirectoryFileNames        : TFileNamesList;
    FDemandFileNames           : TFileNamesList;
    FHydrologyFileNames        : TFileNamesList;
    FOutputFileNames           : TFileNamesList;
    FDamLevelsFileNames        : TFileNamesList;
    // Daily Diversion pre-processor
    FDailyDataFlowFileNames            : TFileNamesList;
    FDailyInstreamFlowFileNames        : TFileNamesList;

    FAllocationDefinitionFileNames    : TFileNamesList;
    FReservoirImplementationFileNames : TFileNamesList;
    FDisbenefitDefinitionFileNames    : TFileNamesList;
    FGrowthFactorsFileNames           : TFileNamesList;
    FMonthlyWaterRequirementFileNames : TFileNamesList;
    FHydropowerAllocationFileNames    : TFileNamesList;
    FPumpingChannelControlFileNames   : TFileNamesList;
    FGeneralChannelControlFileNames   : TFileNamesList;
    FReclamationPlantControlFileNames : TFileNamesList;
    FReturnFlowChannelFileNames       : TFileNamesList;
    FChannelSwitchControlFileNames    : TFileNamesList;
    FTariffCalculationFileNames       : TFileNamesList;
    FAllocationChannelFileNames       : TFileNamesList;
    FReleaseStructureFileNames        : TFileNamesList;
    FCurtailFileNames                 : TFileNamesList;
    FMineFileNames                    : TFileNamesList;
    FMineRainfallFileNames            : TFileNamesList;
    FSaltsWashoffFileNames            : TFileNamesList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    //Yield files
    function GetOutputFileNames            : TAbstractFileNamesList; override;
    function GetConfigFileNames            : TAbstractFileNamesList; override;
    function GetDirectoryFileNames         : TAbstractFileNamesList; override;
    function GetParamFileNames             : TAbstractFileNamesList; override;
    function GetAltParamFileNames          : TAbstractFileNamesList; override;
    function GetDemandFileNames            : TAbstractFileNamesList; override;
    function GetHydrologyFileNames         : TAbstractFileNamesList; override;
    function GetDamLevelsFileNames         : TAbstractFileNamesList; override;
    //PLanning files
    function GetAllocationDefinitionFileNames    : TAbstractFileNamesList; override;
    function GetReservoirImplementationFileNames : TAbstractFileNamesList; override;
    function GetDisbenefitDefinitionFileNames    : TAbstractFileNamesList; override;
    function GetGrowthFactorsFileNames           : TAbstractFileNamesList; override;
    function GetMonthlyWaterRequirementFileNames : TAbstractFileNamesList; override;
    function GetHydropowerAllocationFileNames    : TAbstractFileNamesList; override;
    function GetPumpingChannelControlFileNames   : TAbstractFileNamesList; override;
    function GetGeneralChannelControlFileNames   : TAbstractFileNamesList; override;
    function GetReclamationPlantControlFileNames : TAbstractFileNamesList; override;
    function GetReturnFlowChannelFileNames       : TAbstractFileNamesList; override;
    function GetChannelSwitchControlFileNames    : TAbstractFileNamesList; override;
    function GetTariffCalculationFileNames       : TAbstractFileNamesList; override;
    function GetAllocationChannelFileNames       : TAbstractFileNamesList; override;
    function GetReleaseStructureFileNames        : TAbstractFileNamesList; override;
    function GetCurtailFileNames                 : TAbstractFileNamesList; override;
    function GetSaltsWashoffFileNames            : TAbstractFileNamesList; override;
    function GetMineFileNames                    : TAbstractFileNamesList; override;
    function GetMineRainfallFileNames            : TAbstractFileNamesList; override;

    // Daily Diversion pre-processor file
    function GetDailyDataFlowFileNames        : TAbstractFileNamesList; override;
    function GetDailyInstreamFlowFileNames    : TAbstractFileNamesList; override;

    function GetParamFileName      : string; override;
    function GetAltParamFileName    : string; override;
    procedure SetParamFileName(AFileName : string); override;
    procedure SetAltParamFileName(AFileName : string); override;
    function GetDirectoryFileName      : string; override;
    procedure SetDirectoryFileName(AFileName : string); override;

    function GetFileNamePrefix: string; override;
    procedure SetFileNamePrefix(APrefix: string); override;
    function GetInputFilesPath: string; override;
    procedure SetInputFilesPath(APath: string); override;
    function GetOutputFilesPath: string;override;
    procedure SetOutputFilesPath(APath: string);override;
    function GetHydrologyFilesPath: string;override;
    procedure SetHydrologyFilesPath(APath: string);override;
    function GetDemandFilesPath: string;override;
    procedure SetDemandFilesPath(APath: string);override;
    function GetDamLevelsPath: string; override;
    procedure SetDamLevelsPath(APath: string);override;
    procedure SetSaltsWashoffPath(APath: string);override;

  public
    procedure Reset;
    function Initialise: boolean;override;

    function FilesCount: integer; override;
    function FilesSavedInDatabaseCount: integer;override;
    function SelectedCount: integer;override;
    function GetSumOutFile:TAbstractModelFileName; override;
    function GetPlotOutFile:TAbstractModelFileName; override;
    function GetPumpOutFile:TAbstractModelFileName; override;
    function GetDemandOutFile:TAbstractModelFileName; override;
    function GetRunoffFileNamesCommaText:string; override;
    function GetSoilMoistureFileNamesCommaText:string; override;
    function SelectAllFiles(ASelected: boolean): boolean; override;

    function FileNameExists(AFileName: string; AFileNamesList : TFileNamesList): boolean;
    function FileNameExistsInDB(AFileName: string; AFileNamesList : TFileNamesList): boolean;

    function PopulateInputFilesPaths(AInputFilesPath:string):boolean;
    function PopulateOutputPaths(AOutputFilesPath:string):boolean;
    function PopulateHydrologyPaths(AHydrologyFilesPath:string):boolean;
    function PopulateDemandFilesPaths(ADemandFilesPath:string):boolean;

    function UpdateConfigFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateParamFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateAltParamFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateDirectoryFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function AddDemandFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;
    function AddHydrologyFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;
    function AddDamLevelsFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;

    // Daily Diversion pre-processor files
    function AddDailyDataFlowFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;
    function AddDailyInstreamFlowFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;

    function AddAllocationDefinitionFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;
    function DeleteAllocationDefinitionFileName(AFileIndex: integer): boolean;
    function UpdateReservoirImplementationFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateDisbenefitDefinitionFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateGrowthFactorsFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateMonthlyWaterRequirementFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateHydropowerAllocationFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdatePumpingChannelControlFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateGeneralChannelControlFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateReclamationPlantControlFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateReturnFlowChannelFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function AddChannelSwitchControlFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;
    function DeleteChannelSwitchControlFileName(AFileIndex: integer): boolean;
    function UpdateTariffCalculationFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateAllocationChannelFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;
    function UpdateReleaseStructureFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;

    function UpdateCurtailFileName(AFileIndex: integer; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;

    function AddOutputFileName(AFileName: string; ASavedToDB:Boolean;AFileGroup,AFileNumber: integer;
             AImportDate : TDateTime; AFileDate : TDateTime): boolean;
    function UpdateOutputFileName(AFileType:TOutputFileType; AFileName: string; ASavedToDB:Boolean;
             AImportDate : TDateTime;AFileDate : TDateTime): boolean;

    function AddSaltsWashoffFileNames(AFileIndex: integer; AFileName: string;
             ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;

    function DeleteSaltsWashoffFileName(AFileIndex: integer): boolean;

    function AddMineRainfallFileNames(AFileIndex: integer; AFileName: string;
             ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;

    function DeleteMineRainfallFileName(AFileIndex: integer): boolean;

    function UpdateMineFileName(AFileIndex: integer; AFileName: string;
             ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;

    function AddMineFileNames(AFileIndex: integer; AFileName: string;
             ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
    function DeleteMineFileName(AFileIndex: integer): boolean;


    //Yield files
    property CastConfigFileNames            : TFileNamesList read FConfigFileNames;
    property CastParamFileNames             : TFileNamesList read FParamFileNames;
    property CastAltParamFileNames          : TFileNamesList read FAltParamFileNames;
    property CastDirectoryFileNames         : TFileNamesList read FDirectoryFileNames;
    property CastDemandFileNames            : TFileNamesList read FDemandFileNames;
    property CastHydrologyFileNames         : TFileNamesList read FHydrologyFileNames;
    property CastOutputFileNames            : TFileNamesList read FOutputFileNames;
    property CastDamLevelsFileNames         : TFileNamesList read FDamLevelsFileNames;
   // Daily Diversion pre-processor files
    property CastDailyDataFlowFileNames     : TFileNamesList read FDailyDataFlowFileNames;
    property CastDailyInstreamFlowFileNames : TFileNamesList read FDailyInstreamFlowFileNames;
    //PLanning files
    property CastAllocationDefinitionFileNames    : TFileNamesList read FAllocationDefinitionFileNames   ;
    property CastReservoirImplementationFileNames : TFileNamesList read FReservoirImplementationFileNames;
    property CastDisbenefitDefinitionFileNames    : TFileNamesList read FDisbenefitDefinitionFileNames   ;
    property CastGrowthFactorsFileNames           : TFileNamesList read FGrowthFactorsFileNames          ;
    property CastMonthlyWaterRequirementFileNames : TFileNamesList read FMonthlyWaterRequirementFileNames;
    property CastHydropowerAllocationFileNames    : TFileNamesList read FHydropowerAllocationFileNames   ;
    property CastPumpingChannelControlFileNames   : TFileNamesList read FPumpingChannelControlFileNames  ;
    property CastGeneralChannelControlFileNames   : TFileNamesList read FGeneralChannelControlFileNames  ;
    property CastReclamationPlantControlFileNames : TFileNamesList read FReclamationPlantControlFileNames;
    property CastReturnFlowChannelFileNames       : TFileNamesList read FReturnFlowChannelFileNames      ;
    property CastChannelSwitchControlFileNames    : TFileNamesList read FChannelSwitchControlFileNames   ;
    property CastTariffCalculationFileNames       : TFileNamesList read FTariffCalculationFileNames      ;
    property CastAllocationChannelFileNames       : TFileNamesList read FAllocationChannelFileNames      ;
    property CastReleaseStructureFileNames        : TFileNamesList read FReleaseStructureFileNames       ;
    property CastCurtailFileNames                 : TFileNamesList read FCurtailFileNames       ;

    property CastMineFileNames                    : TFileNamesList read FMineFileNames;
    property CastMineRainfallFileNames            : TFileNamesList read FMineRainfallFileNames;

    property CastSaltsWashoffFileNames            : TFileNamesList read FSaltsWashoffFileNames   ;

  end;

implementation

uses
     Types,
     VCL.Dialogs,
     DateUtils,
     UUtilities,
     UAbstractFileAgent,
     UFileNameConstants,
     UMainMenuEventType,
     UAbstractModelData,
     UPlanningModelDataObject,
     UErrorHandlingOperations, Math;

{ TModelFileNames }

{procedure TModelFileNames.CreateMemberObjects;
const OPNAME = 'TModelFileNames.CreateMemberObjects';
begin
  try
    FModelFileNames:= TFileNames.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.DestroyMemberObjects;
const OPNAME = 'TModelFileNames.DestroyMemberObjects';
begin
  try
    FreeAndNil(FModelFileNames);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.Initialise: boolean;
const OPNAME = 'TModelFileNames.Initialise';
begin
  Result := False;
  try
   Reset;
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.Reset;
const OPNAME = 'TModelFileNames.Reset';
begin
  try
   FModelFileNames.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateModelFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean): boolean;
const OPNAME = 'TModelFileNames.UpdateModelFileName';
var
  LFileName: TFileName;
begin
  Result := False;
  try
    if(AFileIndex < FModelFileNames.Count) then
    begin
      LFileName :=  FModelFileNames.FileName[AFileIndex];
      if Assigned(LFileName) then
      begin
        LFileName.SetFileName(Trim(AFileName),ASavedToDB);
        Result := True;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.ModelFileNameExists(AFileName: string): boolean;
const OPNAME = 'TModelFileNames.ModelFileNameExists';
var
  LCount: integer;
  LFileName: TFileName;
begin
  Result := False;
  try
    for LCount := 0 to FModelFileNames.Count -1 do
    begin
      LFileName := FModelFileNames.FileName[LCount];
      if Assigned(LFileName) and (AFileName =  LFileName.FileName) then
      begin
        Result := True;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.ModelFileNameExistsInDB(AFileName: string): boolean;
const OPNAME = 'TModelFileNames.ModelFileNameExistsInDB';
var
  LCount: integer;
  LFileName: TFileName;
begin
  Result := False;
  try
    for LCount := 0 to FModelFileNames.Count -1 do
    begin
      LFileName := FModelFileNames.FileName[LCount];
      if Assigned(LFileName) and (AFileName =  LFileName.FileName) and (LFileName.SavedInDB) then
      begin
        Result := True;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.AddModelFileName(AFileName: string; ASavedToDB: Boolean): boolean;
const OPNAME = 'TModelFileNames.AddModelFileName';
var
  LFileName: TFileName;
begin
  Result := False;
  try
    if (not ModelFileNameExists(AFileName)) then
    begin
      LFileName := TFileName.Create;
      LFileName.SetFileName(Trim(AFileName),ASavedToDB);
      FModelFileNames.Add(LFileName);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{ TFileName }

function TFileNameObject.AssignFrom(AFileNameObject: TFileNameObject): boolean;
const OPNAME = 'TFileNameObject.AssignFrom';
begin
  Result := False;
  try
    FFileName        := AFileNameObject.FileName;
    FCurrentPath     := AFileNameObject.FilePath;
    FSavedInDB       := AFileNameObject.SavedInDB;
    FChanged         := AFileNameObject.Changed;
    FUpdate          := AFileNameObject.Update;
    FSelected        := AFileNameObject.Selected;
    FFileReadOnly    := AFileNameObject.FileReadOnly;
    FHintsSet        := AFileNameObject.HintsSet;
    FFileGroup       := AFileNameObject.FileGroup;
    FFileNumber      := AFileNameObject.FileNumber;
    FFileDate        := AFileNameObject.FileDate;
    FImportDate      := AFileNameObject.ImportDate;
    FImportable      := AFileNameObject.Importable;
    FExportable      := AFileNameObject.Exportable;
    FValidatable     := AFileNameObject.Validatable;
    FPopulated       := AFileNameObject.Populated;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.FileDateHasChanged(AFileName: String): boolean;
const OPNAME = 'TFileNameObject.FileDateHasChanged';
begin
  Result := False;
  try
    if (Trim(AFileName) <> '') and FileExists(AFileName) then
      Result := (CompareDateTime(FileDate,FileLastWriteDate(AFileName)) <>  EqualsValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.StandardFileName: string;
const OPNAME = 'TFileNameObject.StandardFileName';
begin
  Result := '';
  try
    case FFileGroup of
      fgConfiguration:
        case  FFileNumber of
          01: Result := sfnFile01;
          02: Result := sfnFile02;
          03: Result := sfnFile03;
          04: Result := sfnFile04;
          05: Result := sfnFile05;
          06: Result := sfnFile06;
          07: Result := sfnFile07;
          08: Result := sfnFile08;
          09: Result := sfnFile09;
          10: Result := sfnFile10;
          11: Result := sfnFile11;
          12: Result := sfnFile12;
          13: Result := sfnFile13;
          14: Result := sfnFile14;
          15: Result := sfnFile15;
          16: Result := sfnFile16;
          17: Result := sfnFile17;
          18: Result := sfnFile18;
          19: Result := sfnFile19;
          20: Result := sfnFile20;
          21: Result := sfnFile21;
          22: Result := sfnFile22;

        end;//case
      fgParameter    :  Result := sfnParameter;
      fgAltParameter :  Result := sfnAltParameter;
      fgDirectories  :  Result := sfnDirectory;
      fgDemand       :  Result := sfnDemand;
      fgHydrology    :  Result := sfnHydrology;

      fgAllocationDefinition    :  Result := sfnFM;
      fgGrowthFactors           :  Result := sfnGth;
      fgDisbenefitDefinition    :  Result := sfnDBF;
      fgMonthlyWaterRequirement :  Result := sfnHST;
      fgReservoirImplementation :  Result := sfnDAM;
      fgHydropowerAllocation    :  Result := sfnHYD;
      fgPumpingChannelControl   :  Result := sfnPMP;
      fgGeneralChannelControl   :  Result := sfnPUR;
      fgReclamationPlantControl :  Result := sfnREC;
      fgReturnFlowChannel       :  Result := sfnRET;
      fgChannelSwitchControl    :  Result := sfnSW;
      fgTariffCalculation       :  Result := sfnTAR;
      fgAllocationChannel       :  Result := sfnALO;
      fgReleaseStructure        :  Result := sfnREL;
      fgCurtail                 :  Result := sfnCUR;
      fgMIMM                    :  Result := sfMIMM;

      fgOutput       :
        case GetOutputFileType(FFileName) of
          oftYield      : Result :=  sfnOutYield;
          oftSum        : Result :=  sfnOutSummary;
          oftData       : Result :=  sfnOutData;
          oftDebug      : Result :=  sfnOutDebug;
          oftPlot       : Result :=  sfnOutPlot;
          oftHydroPower : Result :=  sfnOutHydrology;
        end;//case
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.FileFound: boolean;
const OPNAME = 'TFileNameObject.FileFound';
begin
  Result := False;
  try
    if (Trim(FileName) <> '') then
      Result :=  FileExists(FileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.Reset;
const OPNAME = 'TFileNameObject.Reset';
begin
  try
    FCurrentPath     := '';
    FFileName        := '';
    FSavedInDB       := False;
    FChanged         := False;
    Update           := False;
    FSelected        := False;
    FFileReadOnly    := False;
    FHintsSet        := False;
    FFileGroup       := 0;
    FFileNumber      := 0;
    FFileDate        := 0.0;
    FImportDate      := 0.0;
    FImportable      := True;
    FExportable      := True;
    FValidatable     := True;
    FPopulated       := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetFileName(AFilePath,AFileName: string; AFromDB: boolean; AFileGroup,AFileNumber: integer;
          AImportDate : TDateTime;AFileDate : TDateTime);
var
  LOutputFile : boolean;
const OPNAME = 'TFileNameObject.SetFileName';
begin
  try
    if(AFileName = '') then
    begin
      FCurrentPath     := '';
      FFileName         := '';
      FSavedInDB       := false;
      FChanged         := False;
      FUpdate          := False;
      FSelected        := False;
      //FFileReadOnly    := False;
      FHintsSet        := False;
      FFileGroup       := 0;
      FFileNumber      := 0;
      FFileDate        := 0.0;
      FImportDate      := 0.0;
      FPopulated       := False;
    end
    else
    begin
      FFileName        := ExtractFileName(AFileName);
      FCurrentPath     := AFilePath;
      LOutputFile      := (UpperCase(ExtractFileExt(FFileName)) = '.OUT');
      FSavedInDB       := AFromDB;
      FChanged         := not AFromDB;
      FUpdate          := False;
      FFileReadOnly    := not (FileGroup in [1,3]);
      FImportable      := not LOutputFile;
      FExportable      := not LOutputFile;
      FValidatable     := not LOutputFile;
      FSelected        := False;
      FHintsSet        := False;
      FFileGroup       := AFileGroup;
      FFileNumber      := AFileNumber;
      FImportDate      := AImportDate;
      FFileDate        := AFileDate;
      FPopulated       := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFileNamesList }

function TFileNamesList.AddFileName(AFilePath,AFileName: string; AFromDB: boolean;AFileGroup,AFileNumber: integer;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TFileNamesList.AddFileName';
var
  LFileNameObject:TFileNameObject;
begin
  Result := False;
  try
    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.SetFileName(AFilePath,AFileName,AFromDB,AFileGroup,AFileNumber,AImportDate,AFileDate);
    FFileList.Add(LFileNameObject);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.AddFileObject(AFileNameObject: TFileNameObject): boolean;
const OPNAME = 'TFileNamesList.AddFileObject';
var
  LFileNameObject:TFileNameObject;
begin
  Result := False;
  try
    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.AssignFrom(AFileNameObject);
    FFileList.Add(LFileNameObject);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.AssignFrom(AFileNames: TFileNamesList): boolean;
const OPNAME = 'TFileNamesList.AssignFrom';
var
  LCount: integer;
  LFileNameObject:TFileNameObject;
begin
  Result := False;
  try
    if Assigned(AFileNames) then
    begin
      FFileList.Clear;
      for LCount := 0 to AFileNames.FFileList.Count-1 do
      begin
        LFileNameObject := TFileNameObject.Create(FAppModules);
        LFileNameObject.AssignFrom(TFileNameObject(AFileNames.FileNameObject[LCount]));
        FFileList.Add(LFileNameObject);
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.FilesCount: integer;
const OPNAME = 'TFileNamesList.FilesCount';
var
  LCount: integer;
begin
  Result := 0;
  try
    for LCount := 0 to FFileList.Count-1 do
      if (FileNameObject[LCount].FileName <> '') then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.FilesSavedInDatabaseCount: integer;
const OPNAME = 'TFileNamesList.FilesSavedInDatabaseCount';
var
  LCount: integer;
begin
  Result := 0;
  try
    for LCount := 0 to FFileList.Count-1 do
      if TFileNameObject(FileNameObject[LCount]).SavedInDB then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.FindFile(AFileName: String): TAbstractModelFileName;
const OPNAME = 'TFileNamesList.FindFile';
var
  LIndex: integer;
  LFileName: string;
begin
  Result := nil;
  try
    LFileName := Trim(ExtractFileName(AFileName));
    if (LFileName <> '') then
    begin
      for LIndex := 0 to FFileList.Count - 1 do
      begin
        if(UpperCase(TFileNameObject(FFileList.Items[LIndex]).ShortName) = UpperCase(LFileName)) then
        begin
          Result := TFileNameObject(FFileList.Items[LIndex]);
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.FindFilesFromPrefix(AFilePrefix,AFileExt: string; AFilesList: TStringList): boolean;
const OPNAME = 'TFileNamesList.FindFilesFromPrefix';
var
  LCount: integer;
  LFileExt: string;
begin
  Result := False;
  try
    if (Trim(AFilePrefix) <> '') and Assigned(AFilesList) then
    begin
      AFilesList.Clear;
      for LCount := 0 to FFileList.Count-1 do
      begin
        if (Pos(UpperCase(Trim(AFilePrefix)),UpperCase(FileNameObject[LCount].FileName)) > 0) then
        begin
         if(AFileExt <> '') then
         begin
           LFileExt := ExtractFileExt(FileNameObject[LCount].FileName);
           if(UpperCase(LFileExt) = UpperCase(AFileExt)) then
             AFilesList.Add(FileNameObject[LCount].FileName);
         end
         else
          AFilesList.Add(FileNameObject[LCount].FileName);
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.GetCaptionStr: string;
const OPNAME = 'TFileNamesList.GetCaptionStr';
begin
  Result := '';
  try
    Result := FCaptionStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNamesList.SetCaptionStr(ACaptionStr: string);
const OPNAME = 'TFileNamesList.SetCaptionStr';
begin
  try
    FCaptionStr:= ACaptionStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileNamesList.GetFileObject(AIndex: integer): TAbstractModelFileName;
const OPNAME = 'TFileNamesList.GetFileObject';
begin
  Result := nil;
  try
    if (AIndex < FFileList.Count) and Assigned(FFileList.Items[AIndex]) then
      Result := TFileNameObject(FFileList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TFileNamesList.Reset;
const OPNAME = 'TFileNamesList.Reset';
begin
  try
    Self.OwnsObjects := True;
    Self.Clear;
    //FCaptionStr := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TFileNamesList.DeleteAllFiles: boolean;
const OPNAME = 'TFileNamesList.DeleteAllFiles';
var
  LIndex: integer;
  LFilename: string;
begin
  Result := False;
  try
    for LIndex  := 0 to FFileList.Count -1 do
    begin
      LFilename :=  TFileNameObject(FFileList.Items[LIndex]).FileName;
      if FileExists(LFilename) then
      begin
{$WARN SYMBOL_PLATFORM OFF}
        	FileSetAttr(LFilename,0);
{$WARN SYMBOL_PLATFORM ON}
          SysUtils.DeleteFile(LFilename);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.CreateMemberObjects;
const OPNAME = 'TModelFileNames.CreateMemberObjects';
begin
  try
    FFileNamePrefix            := '';
    FInputFilesPath            := '';
    FOutputFilesPath           := '';
    FHydrologyFilesPath        := '';
    FDemandFilesPath           := '';
    FDamLevelsPath             := '';
    FSaltsWashoffPath          := '';
    FConfigFileNames           := TFileNamesList.Create(FAppModules);
    FParamFileNames            := TFileNamesList.Create(FAppModules);
    FAltParamFileNames         := TFileNamesList.Create(FAppModules);
    FDirectoryFileNames        := TFileNamesList.Create(FAppModules);
    FDemandFileNames           := TFileNamesList.Create(FAppModules);
    FHydrologyFileNames        := TFileNamesList.Create(FAppModules);
    FOutputFileNames           := TFileNamesList.Create(FAppModules);
    FDamLevelsFileNames        := TFileNamesList.Create(FAppModules);

    FDailyDataFlowFileNames    := TFileNamesList.Create(FAppModules);
    FDailyInstreamFlowFileNames := TFileNamesList.Create(FAppModules);

    FAllocationDefinitionFileNames    := TFileNamesList.Create(FAppModules);
    FReservoirImplementationFileNames := TFileNamesList.Create(FAppModules);
    FDisbenefitDefinitionFileNames    := TFileNamesList.Create(FAppModules);
    FGrowthFactorsFileNames           := TFileNamesList.Create(FAppModules);
    FMonthlyWaterRequirementFileNames := TFileNamesList.Create(FAppModules);
    FHydropowerAllocationFileNames    := TFileNamesList.Create(FAppModules);
    FPumpingChannelControlFileNames   := TFileNamesList.Create(FAppModules);
    FGeneralChannelControlFileNames   := TFileNamesList.Create(FAppModules);
    FReclamationPlantControlFileNames := TFileNamesList.Create(FAppModules);
    FReturnFlowChannelFileNames       := TFileNamesList.Create(FAppModules);
    FChannelSwitchControlFileNames    := TFileNamesList.Create(FAppModules);
    FTariffCalculationFileNames       := TFileNamesList.Create(FAppModules);
    FAllocationChannelFileNames       := TFileNamesList.Create(FAppModules);
    FReleaseStructureFileNames        := TFileNamesList.Create(FAppModules);
    FCurtailFileNames                 := TFileNamesList.Create(FAppModules);

    FMineFileNames                    := TFileNamesList.Create(FAppModules);
    FMineRainfallFileNames            := TFileNamesList.Create(FAppModules);
    FSaltsWashoffFileNames            := TFileNamesList.Create(FAppModules);
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.DestroyMemberObjects;
const OPNAME = 'TModelFileNames.DestroyMemberObjects';
begin
  try
    FreeAndNil(FConfigFileNames);
    FreeAndNil(FParamFileNames);
    FreeAndNil(FAltParamFileNames);
    FreeAndNil(FDirectoryFileNames);
    FreeAndNil(FDemandFileNames);
    FreeAndNil(FHydrologyFileNames);
    FreeAndNil(FOutputFileNames);
    FreeAndNil(FDamLevelsFileNames);
    FreeAndNil(FDailyDataFlowFileNames);
    FreeAndNil(FDailyInstreamFlowFileNames);        

    FreeAndNil(FAllocationDefinitionFileNames);
    FreeAndNil(FReservoirImplementationFileNames);
    FreeAndNil(FDisbenefitDefinitionFileNames);
    FreeAndNil(FGrowthFactorsFileNames);
    FreeAndNil(FMonthlyWaterRequirementFileNames);
    FreeAndNil(FHydropowerAllocationFileNames);
    FreeAndNil(FPumpingChannelControlFileNames);
    FreeAndNil(FGeneralChannelControlFileNames);
    FreeAndNil(FReclamationPlantControlFileNames);
    FreeAndNil(FReturnFlowChannelFileNames);
    FreeAndNil(FChannelSwitchControlFileNames);
    FreeAndNil(FTariffCalculationFileNames);
    FreeAndNil(FAllocationChannelFileNames);
    FreeAndNil(FReleaseStructureFileNames);
    FreeAndNil(FCurtailFileNames);

    FreeAndNil(FMineFileNames);
    FreeAndNil(FMineRainfallFileNames);
    FreeAndNil(FSaltsWashoffFileNames);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.Initialise: boolean;
const OPNAME = 'TModelFileNames.Initialise';
begin
  Result := False;
  try
    Reset;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.Reset;
const OPNAME = 'TModelFileNames.Reset';
var
  LFileNameObject: TFileNameObject;
  LCount: integer;
begin
  try
    FFileNamePrefix            := '';
    FInputFilesPath            := '';
    FOutputFilesPath           := '';
    FHydrologyFilesPath        := '';
    FDemandFilesPath           := '';
    FDamLevelsPath             := '';
    FSaltsWashoffPath          := '';
    FConfigFileNames.Clear;
    FParamFileNames.Clear;
    FAltParamFileNames.Clear;
    FDirectoryFileNames.Clear;
    FDemandFileNames.Clear;
    FHydrologyFileNames.Clear;
    FOutputFileNames.Clear;
    FDamLevelsFileNames.Clear;
    FDailyDataFlowFileNames.Clear;
    FDailyInstreamFlowFileNames.Clear;

    for LCount := 1 to 22 do
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.FFileName := format('%s%2.2d%s',['F',LCount,'.dat']);
      FConfigFileNames.Add(LFileNameObject);
    end;

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := 'Param.dat';
    FParamFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := 'ParamFor.dat';
    FAltParamFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := 'WRYM.dat';
    FDirectoryFileNames.Add(LFileNameObject);

    //'hyd.out';
    //'plt.out'
    //'dat.out'
    //'dbg.out'
    //'sum.out'
    //'yld.out'

    // for Planning
    //'Res.out'
    //'Sys.out'
    //'Pmp.out'
    if (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      for LCount := 1 to 9 do
      begin
        LFileNameObject := TFileNameObject.Create(FAppModules);
        case LCount of
          1: LFileNameObject.FFileName := 'Hyd.out';
          2: LFileNameObject.FFileName := 'Plt.out';
          3: LFileNameObject.FFileName := 'Dat.out';
          4: LFileNameObject.FFileName := 'Dbg.out';
          5: LFileNameObject.FFileName := 'Sum.out';
          6: LFileNameObject.FFileName := 'Yld.out';

          7: LFileNameObject.FFileName := 'Res.out';
          8: LFileNameObject.FFileName := 'Sys.out';
          9: LFileNameObject.FFileName := 'Pmp.out';

        end;
        FOutputFileNames.Add(LFileNameObject);
      end;
    end
    else
    begin
      for LCount := 1 to 7 do
      begin
        LFileNameObject := TFileNameObject.Create(FAppModules);
        case LCount of
          1: LFileNameObject.FFileName := 'Hyd.out';
          2: LFileNameObject.FFileName := 'Plt.out';
          3: LFileNameObject.FFileName := 'Dat.out';
          4: LFileNameObject.FFileName := 'Dbg.out';
          5: LFileNameObject.FFileName := 'Sum.out';
          6: LFileNameObject.FFileName := 'Yld.out';
          7: LFileNameObject.FFileName := 'IrrBlockDEM.OUT';
        end;
        FOutputFileNames.Add(LFileNameObject);
      end;
    end;
    FAllocationDefinitionFileNames.Clear;
    FReservoirImplementationFileNames.Clear;
    FDisbenefitDefinitionFileNames.Clear;
    FGrowthFactorsFileNames.Clear;
    FMonthlyWaterRequirementFileNames.Clear;
    FHydropowerAllocationFileNames.Clear;
    FPumpingChannelControlFileNames.Clear;
    FGeneralChannelControlFileNames.Clear;
    FReclamationPlantControlFileNames.Clear;
    FReturnFlowChannelFileNames.Clear;
    FChannelSwitchControlFileNames.Clear;
    FTariffCalculationFileNames.Clear;
    FAllocationChannelFileNames.Clear;
    FReleaseStructureFileNames.Clear;
    FCurtailFileNames.Clear;
    FMineFileNames.Clear;
    FMineRainfallFileNames.Clear;
    FSaltsWashoffFileNames.Clear;

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*DAM.dat';
    FReservoirImplementationFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*DBF.dat';
    FDisbenefitDefinitionFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*GTH.dat';
    FGrowthFactorsFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*HST.dat';
    FMonthlyWaterRequirementFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*HYD.dat';
    FHydropowerAllocationFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*PMP.dat';
    FPumpingChannelControlFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*PUR.dat';
    FGeneralChannelControlFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*REC.dat';
    FReclamationPlantControlFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*RET.dat';
    FReturnFlowChannelFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*TAR.dat';
    FTariffCalculationFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*ALO.dat';
    FAllocationChannelFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*REL.dat';
    FReleaseStructureFileNames.Add(LFileNameObject);

    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.FFileName := '*CUR.dat';
    FCurtailFileNames.Add(LFileNameObject);

    


    FDirectoryFileNames.CaptionStr := 'ViewData.FileDirectory';
    FParamFileNames.CaptionStr     := 'ViewData.FileParam';
    FAltParamFileNames.CaptionStr  := 'ViewData.AltFileParam';
    FConfigFileNames.CaptionStr    := 'ViewData.FileConfiguration';
    FDemandFileNames.CaptionStr    := 'ViewData.FileDemand';
    FHydrologyFileNames.CaptionStr := 'ViewData.FileHydrological';
    FOutputFileNames.CaptionStr    := 'ViewData.FileOutput';
    FDamLevelsFileNames.CaptionStr := 'ViewData.FileDamWaterLevels';

    FAllocationDefinitionFileNames.CaptionStr    := 'ViewData.FileAllocationDefinition';
    FReservoirImplementationFileNames.CaptionStr := 'ViewData.FileReservoirImplementation';
    FDisbenefitDefinitionFileNames.CaptionStr    := 'ViewData.FileDisbenefitDefinition';
    FGrowthFactorsFileNames.CaptionStr           := 'ViewData.FileGrowthFactors';
    FMonthlyWaterRequirementFileNames.CaptionStr := 'ViewData.FileMonthlyWaterRequirement';
    FHydropowerAllocationFileNames.CaptionStr    := 'ViewData.FileHydropowerAllocation';
    FPumpingChannelControlFileNames.CaptionStr   := 'ViewData.FilePumpingChannelControl';
    FGeneralChannelControlFileNames.CaptionStr   := 'ViewData.FileGeneralChannelControl';
    FReclamationPlantControlFileNames.CaptionStr := 'ViewData.FileReclamationPlantControl';
    FReturnFlowChannelFileNames.CaptionStr       := 'ViewData.FileReturnFlowChannel';
    FChannelSwitchControlFileNames.CaptionStr    := 'ViewData.FileChannelSwitchControl';
    FTariffCalculationFileNames.CaptionStr       := 'ViewData.FileTariffCalculation';
    FAllocationChannelFileNames.CaptionStr       := 'ViewData.FileAllocationChannel';
    FReleaseStructureFileNames.CaptionStr        := 'ViewData.FileReleaseStructure';

    FCurtailFileNames.CaptionStr                 := 'ViewData.FileCurtail';

    FMineFileNames.CaptionStr                    := 'ViewData.FileMine';
    FMineRainfallFileNames.CaptionStr            := 'ViewData.FileMineRanfall';
    FSaltsWashoffFileNames.CaptionStr            := 'ViewData.FileSaltsWashoff';


  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateConfigFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateConfigFileName';
var
  LFileNameObject: TAbstractModelFileName;
begin
  Result := False;
  try
    if(AFileIndex < FConfigFileNames.Count) then
    begin
      LFileNameObject := FConfigFileNames.FileNameObject[AFileIndex];
      TFileNameObject(LFileNameObject).SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgConfiguration,AFileIndex + 1,
                                                   AImportDate,AFileDate);
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateDirectoryFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateDirectoryFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FDirectoryFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FDirectoryFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgDirectories,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateParamFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateParamFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FParamFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FParamFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FHydrologyFilesPath,Trim(AFileName),ASavedToDB,fgParameter,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateAltParamFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateParamFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FAltParamFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FAltParamFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FHydrologyFilesPath,Trim(AFileName),ASavedToDB,fgAltParameter,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateOutputFileName(AFileType:TOutputFileType; AFileName: string; ASavedToDB: Boolean; AImportDate,
  AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateOutputFileName';
var
  LFileNameObject: TAbstractModelFileName;
  LFileIndex: integer;
begin
  Result := False;
  try
    LFileNameObject := nil;
    LFileIndex := 0;

    if (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      {case AFileType of
        oftHydroPower :
          begin
            LFileIndex := 0;
            LFileNameObject := FOutputFileNames.FileNameObject[0];
          end;
        oftPlot       :
          begin
            LFileIndex := 1;
            LFileNameObject := FOutputFileNames.FileNameObject[1];
          end;
        oftData       :
          begin
            LFileIndex := 2;
            LFileNameObject := FOutputFileNames.FileNameObject[2];
          end;
        oftDebug      :
          begin
            LFileIndex := 3;
            LFileNameObject := FOutputFileNames.FileNameObject[3];
          end;
        oftSum        :
          begin
            LFileIndex := 4;
            LFileNameObject := FOutputFileNames.FileNameObject[4];
          end;
        oftYield      :
          begin
            LFileIndex := 5;
            LFileNameObject := FOutputFileNames.FileNameObject[5];
          end;
        oftRes     :
          begin
            LFileIndex := 6;
            LFileNameObject := FOutputFileNames.FileNameObject[6];
          end;
        oftSys     :
          begin
            LFileIndex := 7;
            LFileNameObject := FOutputFileNames.FileNameObject[7];
          end;
        oftPmp     :
          begin
            LFileIndex := 8;
            LFileNameObject := FOutputFileNames.FileNameObject[8];
          end;
      end;//case}
    end
    else
    begin
      case AFileType of
        oftHydroPower :
          begin
            LFileIndex := 0;
            LFileNameObject := FOutputFileNames.FileNameObject[0];
          end;
        oftPlot       :
          begin
            LFileIndex := 1;
            LFileNameObject := FOutputFileNames.FileNameObject[1];
          end;
        oftData       :
          begin
            LFileIndex := 2;
            LFileNameObject := FOutputFileNames.FileNameObject[2];
          end;
        oftDebug      :
          begin
            LFileIndex := 3;
            LFileNameObject := FOutputFileNames.FileNameObject[3];
          end;
        oftSum        :
          begin
            LFileIndex := 4;
            LFileNameObject := FOutputFileNames.FileNameObject[4];
          end;
        oftYield      :
          begin
            LFileIndex := 5;
            LFileNameObject := FOutputFileNames.FileNameObject[5];
          end;
        oftDemand     :
          begin
            LFileIndex := 6;
            LFileNameObject := FOutputFileNames.FileNameObject[6]
          end;
      end;//case
    end;

    if Assigned(LFileNameObject) then
    begin
      TFileNameObject(LFileNameObject).SetFileName(FOutputFilesPath,(AFileName),ASavedToDB,fgOutput,LFileIndex + 1,AImportDate,
                                                   AFileDate);
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.AddDemandFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddDemandFileName';
var
  LFileName : string;
  LPath     : string;
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileName := ExtractFileName(AFileName);
    LPath     := ExtractFilePath(AFileName);
    if(LPath = '') then
      LPath := FHydrologyFilesPath;
    if (not FileNameExists(LFileName,FDemandFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(LPath,LFileName,ASavedToDB,fgDemand,AFileIndex,AImportDate,AFileDate);
      FDemandFileNames.Add(LFileNameObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.AddHydrologyFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddHydrologyFileName';
var
  LFileName : string;
  LPath     : string;
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileName := ExtractFileName(AFileName);
    LPath     := ExtractFilePath(AFileName);
    if(LPath = '') then
      LPath := FHydrologyFilesPath;
    if (not FileNameExists(LFileName,FHydrologyFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(LPath,LFileName,ASavedToDB,fgHydrology,AFileIndex,AImportDate,AFileDate);
      FHydrologyFileNames.Add(LFileNameObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.AddOutputFileName(AFileName: string; ASavedToDB:Boolean;AFileGroup,AFileNumber: integer;
         AImportDate : TDateTime;AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddHydrologyFileName';
var
  LFileName : string;
  LPath     : string;
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileName := ExtractFileName(AFileName);
    LPath     := ExtractFilePath(AFileName);
    if(LPath = '') then
      LPath := FOutputFilesPath;
    if (not FileNameExists(LFileName,FOutputFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(LPath,Trim(LFileName),ASavedToDB,AFileGroup,AFileNumber,AImportDate,AFileDate);
      FOutputFileNames.Add(LFileNameObject);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.FileNameExists(AFileName: string; AFileNamesList: TFileNamesList): boolean;
const OPNAME = 'TModelFileNames.FileNameExists';
var
  LCount: integer;
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    for LCount := 0 to AFileNamesList.Count -1 do
    begin
      LFileNameObject := TFileNameObject(AFileNamesList[LCount]);
      if(UpperCase(AFileName) =  UpperCase(LFileNameObject.FileName)) then
      begin
        Result := True;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.FileNameExistsInDB(AFileName: string; AFileNamesList: TFileNamesList): boolean;
const OPNAME = 'TModelFileNames.FileNameExistsInDB';
var
  LCount: integer;
  LFileNameObject: TAbstractModelFileName;
begin
  Result := False;
  try
    for LCount := 0 to AFileNamesList.Count -1 do
    begin
      LFileNameObject := AFileNamesList.FileNameObject[LCount];
      if(AFileName =  LFileNameObject.FileName) then
      begin
        Result := TFileNameObject(LFileNameObject).SavedInDB;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.FilesCount: integer;
const OPNAME = 'TModelFileNames.FilesCount';
begin
  Result := 0;
  try
    Result := FConfigFileNames.FilesCount +
              FParamFileNames.FilesCount +
              FAltParamFileNames.FilesCount +
              FDirectoryFileNames.FilesCount +
              FDemandFileNames.FilesCount +
              FHydrologyFileNames.FilesCount +
              FOutputFileNames.FilesCount +
              FDamLevelsFileNames.FilesCount +
              FDailyDataFlowFileNames.FilesCount +
              FDailyInstreamFlowFileNames.FilesCount +
              FAllocationDefinitionFileNames.FilesCount +
              FReservoirImplementationFileNames.FilesCount +
              FPumpingChannelControlFileNames.FilesCount +
              FGeneralChannelControlFileNames.FilesCount +
              FReclamationPlantControlFileNames.FilesCount +
              FChannelSwitchControlFileNames.FilesCount+
              FGrowthFactorsFileNames.FilesCount+
              FCurtailFileNames.FilesCount+
              FMineFileNames.FilesCount+
              FMineRainfallFileNames.FilesCount+
              FSaltsWashoffFileNames.FilesCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TModelFileNames.FilesSavedInDatabaseCount: integer;
const OPNAME = 'TModelFileNames.FilesSavedInDatabaseCount';
begin
  Result := 0;
  try
    Result :=
      FConfigFileNames.FilesSavedInDatabaseCount +
      FParamFileNames.FilesSavedInDatabaseCount +
      FAltParamFileNames.FilesSavedInDatabaseCount +
      FDirectoryFileNames.FilesSavedInDatabaseCount +
      FDemandFileNames.FilesSavedInDatabaseCount +
      FHydrologyFileNames.FilesSavedInDatabaseCount +
      FOutputFileNames.FilesSavedInDatabaseCount +
      FDamLevelsFileNames.FilesSavedInDatabaseCount+
      FDailyDataFlowFileNames.FilesSavedInDatabaseCount+
      FDailyInstreamFlowFileNames.FilesSavedInDatabaseCount +
      FAllocationDefinitionFileNames.FilesSavedInDatabaseCount+
      FReservoirImplementationFileNames.FilesSavedInDatabaseCount+
      FPumpingChannelControlFileNames.FilesSavedInDatabaseCount+
      FGeneralChannelControlFileNames.FilesSavedInDatabaseCount+
      FReclamationPlantControlFileNames.FilesSavedInDatabaseCount+
      FChannelSwitchControlFileNames.FilesSavedInDatabaseCount+
      FGrowthFactorsFileNames.FilesSavedInDatabaseCount+
      FCurtailFileNames.FilesSavedInDatabaseCount+
      FMineFileNames.FilesSavedInDatabaseCount+
      FMineRainfallFileNames.FilesSavedInDatabaseCount+
      FSaltsWashoffFileNames.FilesSavedInDatabaseCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFileGroups }

{function TFileGroups.GetFileGroup(AIndex: integer): TFileNamesList;
const OPNAME = 'TFileNamesList.GetFileObject';
begin
  Result := nil;
  try
    if (AIndex < Self.Count) and Assigned(Items[AIndex]) then
      Result := TFileNamesList(Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TModelFileNames.SelectedCount: integer;
const OPNAME = 'TModelFileNames.SelectedCount';
begin
  Result := 0;
  try
    Result :=
      FConfigFileNames.SelectedCount +
      FParamFileNames.SelectedCount +
      FAltParamFileNames.SelectedCount +
      FDirectoryFileNames.SelectedCount +
      FDemandFileNames.SelectedCount +
      FHydrologyFileNames.SelectedCount +
      FOutputFileNames.SelectedCount +
      FDamLevelsFileNames.SelectedCount+
      FDailyDataFlowFileNames.SelectedCount+
      FDailyInstreamFlowFileNames.SelectedCount+
      FAllocationDefinitionFileNames.SelectedCount+
      FReservoirImplementationFileNames.SelectedCount+
      FPumpingChannelControlFileNames.SelectedCount+
      FGeneralChannelControlFileNames.SelectedCount+
      FReclamationPlantControlFileNames.SelectedCount+
      FChannelSwitchControlFileNames.SelectedCount+
      FGrowthFactorsFileNames.SelectedCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.SelectedCount: integer;
const OPNAME = 'TFileNamesList.SelectedCount';
var
  LCount: integer;
begin
  Result := 0;
  try
    for LCount := 0 to Self.Count-1 do
      if TFileNameObject(FileNameObject[LCount]).Selected then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.SetAllSelected(ASelected: boolean): boolean;
const OPNAME = 'TFileNamesList.SetAllSelected';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := 0 to Self.Count-1 do
      TFileNameObject(FileNameObject[LCount]).Selected := ASelected;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.SetFileHintsSet(ASetStatus: boolean): boolean;
const OPNAME = 'TFileNamesList.SetFileHintsSet';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := 0 to Self.Count-1 do
      TFileNameObject(FileNameObject[LCount]).HintsSet := ASetStatus;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetSumOutFile: TAbstractModelFileName;
const OPNAME = 'TModelFileNames.GetSumOutFile';
var
  LCount: integer;
begin
  Result := Nil;
  try
    for LCount := 0 to FOutputFileNames.Count-1 do
      if (GetOutputFileType(FOutputFileNames.FileNameObject[LCount].FileName) = oftSum) then
         Result := FOutputFileNames.FileNameObject[LCount];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetPlotOutFile: TAbstractModelFileName;
const OPNAME = 'TModelFileNames.GetPlotOutFile';
var
  LCount: integer;
begin
  Result := Nil;
  try
    for LCount := 0 to FOutputFileNames.Count-1 do
      if (GetOutputFileType(FOutputFileNames.FileNameObject[LCount].FileName) = oftPlot) then
         Result := FOutputFileNames.FileNameObject[LCount];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetPumpOutFile: TAbstractModelFileName;
const OPNAME = 'TModelFileNames.GetPumpOutFile';
var
  LCount: integer;
begin
  Result := Nil;
  try
    for LCount := 0 to FOutputFileNames.Count-1 do
      if (GetOutputFileType(FOutputFileNames.FileNameObject[LCount].FileName) = oftPmp) then
         Result := FOutputFileNames.FileNameObject[LCount];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetDemandOutFile: TAbstractModelFileName;
const OPNAME = 'TModelFileNames.GetDemandOutFile';
var
  LCount: integer;
begin
  Result := Nil;
  try
    for LCount := 0 to FOutputFileNames.Count-1 do
      if (GetOutputFileType(FOutputFileNames.FileNameObject[LCount].FileName) = oftDemand) then
         Result := FOutputFileNames.FileNameObject[LCount];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.CreateMemberObjects;
const OPNAME = 'TFileNameObject.CreateMemberObjects';
begin
  inherited;
  try
     Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.CheckForEqualityTrue(AFileNameObject:TAbstractModelFileName): boolean;
const OPNAME = 'TFileNameObject.CheckForEqualityTrue';
begin
  Result := False;
  try
    if Assigned(AFileNameObject) then
    begin
      Result := (AFileNameObject.FileName = Self.FileName) and
                (AFileNameObject.FileGroup = Self.FFileGroup) and
                (AFileNameObject.FileNumber = Self.FFileNumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.FileHasBeenEdited: boolean;
const OPNAME = 'TFileNameObject.FileHasBeenEdited';
var
  LCurrentFileDate: TDateTime;
begin
  Result := False;
  try
    if FileFound then
    begin
      LCurrentFileDate := FileLastWriteDate(FileName);
      Result := CompareDateTime(FileDate,LCurrentFileDate) = LessThanValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetFilePath: string;
const OPNAME = 'TFileNameObject.GetFilePath';
begin
  Result := '';
  try
    if(Trim(FCurrentPath) <> '') then
      Result := IncludeTrailingPathDelimiter(FCurrentPath);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetFileGroup: integer;
const OPNAME = 'TFileNameObject.GetFileGroup';
begin
  Result := -1;
  try
    Result := FFileGroup;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetFileNumber: integer;
const OPNAME = 'TFileNameObject.GetFileNumber';
begin
  Result := -1;
  try
    Result := FFileNumber;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetShortFileName: string;
const OPNAME = 'TFileNameObject.GetShortFileName';
begin
  Result := '';
  try
    Result := ExtractFileName(FFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetFullFileName: string;
const OPNAME = 'TFileNameObject.GetFullFileName';
begin
  Result := '';
  try
    if (FFileName <> '') then
      if (FCurrentPath <> '') then
        Result := IncludeTrailingPathDelimiter(FCurrentPath)+ ExtractFileName(FFileName)
      else
        Result := FFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetHintsSet: Boolean;
const OPNAME = 'TFileNameObject.GetHintsSet';
begin
  Result := False;
  try
    Result := FHintsSet;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetChanged: Boolean;
const OPNAME = 'TFileNameObject.GetChanged';
begin
  Result := False;
  try
    Result := FChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetSavedInDB: Boolean;
const OPNAME = 'TFileNameObject.GetSavedInDB';
begin
  Result := False;
  try
    Result := FSavedInDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetSelected: Boolean;
const OPNAME = 'TFileNameObject.GetSelected';
begin
  Result := False;
  try
    Result := FSelected;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetUpdate: Boolean;
const OPNAME = 'TFileNameObject.GetUpdate';
begin
  Result := False;
  try
    Result := FUpdate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetFullFileName(ACurrentName: string);
const OPNAME = 'TFileNameObject.SetFullFileName';
begin
  try
   FFileName := ExtractFileName(ACurrentName);
   if FSavedInDB then
   begin
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetFileGroup(AFileGroup: integer);
const OPNAME = 'TFileNameObject.SetFileGroup';
begin
  try
   FFileGroup := AFileGroup;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetHintsSet(AHintsSet: Boolean);
const OPNAME = 'TFileNameObject.SetHintsSet';
begin
  try
   FHintsSet := AHintsSet;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetSavedInDB(ASavedInDB: Boolean);
const OPNAME = 'TFileNameObject.SetSavedInDB';
begin
  try
   FSavedInDB := ASavedInDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetSelected(ASelected: Boolean);
const OPNAME = 'TFileNameObject.SetSelected';
begin
  try
   FSelected := ASelected;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetUpdate(AUpdate: Boolean);
const OPNAME = 'TFileNameObject.SetUpdate';
begin
  try
   FUpdate := AUpdate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetChanged(AChanged: Boolean);
const OPNAME = 'TFileNameObject.SetChanged';
begin
  try
   FChanged := AChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetConfigFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetConfigFileNames';
begin
  Result := nil;
  try
    Result := FConfigFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetDemandFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetDemandFileNames';
begin
  Result := nil;
  try
    Result := FDemandFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetDirectoryFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetDirectoryFileNames';
begin
  Result := nil;
  try
    Result := FDirectoryFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetHydrologyFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetHydrologyFileNames';
begin
  Result := nil;
  try
    Result := FHydrologyFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetOutputFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetOutputFileNames';
begin
  Result := nil;
  try
    Result := FOutputFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetParamFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetParamFileNames';
begin
  Result := nil;
  try
    Result := FParamFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetAltParamFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetAltParamFileNames';
begin
  Result := nil;
  try
    Result := FAltParamFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetFileReadOnly: Boolean;
const OPNAME = 'TFileNameObject.GetFileReadOnly';
begin
  Result := False;
  try
    Result := FFileReadOnly;
    if (not Result) and FileExists(FileName) then
      Result := FileIsReadOnly(FileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetFileReadOnly(AReadOnly: Boolean);
const OPNAME = 'TFileNameObject.SetFileReadOnly';
begin
  try
    FFileReadOnly := AReadOnly;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetDirectoryFileName: string;
const OPNAME = 'TModelFileNames.GetDirectoryFileName';
begin
  Result := '';
  try
    Result := FDirectoryFileNames.FileNameObject[0].FileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetDirectoryFileName(AFileName: string);
const OPNAME = 'TModelFileNames.SetDirectoryFileName';
var
  LFileNameObject: TFileNameObject;
  LContextData: TStringList;
begin
  try
    LFileNameObject := TFileNameObject(FDirectoryFileNames.FileNameObject[0]);
    if Assigned(LFileNameObject) then
    begin
      LContextData := TStringList.Create;
      try
        LFileNameObject.LoadContextData(LContextData);
        //if FAppModules.FieldProperties.UpdateFieldValue(
        //     'DirectoryFileName', AFileName, LFileNameObject.FileName, LContextData) then
        //begin

          LFileNameObject.SetFileName(FInputFilesPath,AFileName,LFileNameObject.SavedInDB,
                                      fgDirectories,1,
                                      LFileNameObject.ImportDate,
                                      FileLastWriteDate(AFileName));
        ///end;
      finally
        LContextData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetParamFileName: string;
const OPNAME = 'TModelFileNames.GetParamFileName';
begin
  Result := '';
  try
    if (FParamFileNames.FilesCount > 0) then
      Result := FParamFileNames.FileNameObject[0].FileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetAltParamFileName: string;
const OPNAME = 'TModelFileNames.GetAltParamFileName';
begin
  Result := '';
  try
    if (FAltParamFileNames.FilesCount > 0) then
      Result := FAltParamFileNames.FileNameObject[0].FileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetParamFileName(AFileName: string);
const OPNAME = 'TModelFileNames.SetParamFileName';
var
  LFileNameObject: TFileNameObject;
  //LContextData: TStringList;
  LOldFileName: string;
begin
  try
    LFileNameObject := TFileNameObject(FParamFileNames.FileNameObject[0]);
    if Assigned(LFileNameObject) and
       (UpperCase(LFileNameObject.FileName) <> UpperCase(AFileName)) then
    begin
      ShowMessage(FAppModules.Language.GetString('ParamFile.ImportFileTakesTime'));
      if LFileNameObject.FSavedInDB then
        FAppModules.Model.ProcessEvent(CmeClearParamFile,LFileNameObject);

      LOldFileName := LFileNameObject.FileName;
      LFileNameObject.SetFileName(FHydrologyFilesPath,AFileName,LFileNameObject.SavedInDB,
                                fgParameter,1,
                                LFileNameObject.ImportDate,
                                FileLastWriteDate(AFileName));
      if not LFileNameObject.FSavedInDB then
        FAppModules.Model.ProcessEvent(CmeImportParamFile,LFileNameObject);

      FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParamFile',LOldFileName,AFileName);

     { LContextData := TStringList.Create;
      try
        if LFileNameObject.FSavedInDB then
        begin
          LFileNameObject.LoadContextData(LContextData);
          if not FAppModules.FieldProperties.UpdateFieldValue(
          'ParamFile', AFileName, LFileNameObject.FileName, LContextData) then
          Exit;
        end;
        LFileNameObject.SetFileName(AFileName,LFileNameObject.SavedInDB,
                                  fgParameter,1,
                                  LFileNameObject.ImportDate,
                                  FileLastWriteDate(AFileName));
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParamFile',LOldFileName,AFileName);
      finally
        LContextData.Free;
      end; }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetAltParamFileName(AFileName: string);
const OPNAME = 'TModelFileNames.SetAltParamFileName';
var
  LFileNameObject: TFileNameObject;
  LOldFileName: string;
begin
  try
    LFileNameObject := TFileNameObject(FAltParamFileNames.FileNameObject[0]);
    if Assigned(LFileNameObject) and
       (UpperCase(LFileNameObject.FileName) <> UpperCase(AFileName)) then
    begin
      LOldFileName := LFileNameObject.FileName;
      LFileNameObject.SetFileName(FHydrologyFilesPath,AFileName,LFileNameObject.SavedInDB,
                                fgAltParameter,1,
                                LFileNameObject.ImportDate,
                                FileLastWriteDate(AFileName));

      FAppModules.Model.StudyDataHasChanged(sdccEdit,'AltFile',LOldFileName,AFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TModelFileNames.SetDamLevelsPath(APath: string);
const OPNAME = 'TModelFileNames.SetDamLevelsPath';
begin
  try
    APath := Trim(APath);
    if (APath <> '')  then  APath := IncludeTrailingPathDelimiter(APath);
    if(APath <> FDamLevelsPath) then
    begin
      FDamLevelsPath := APath;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetSaltsWashoffPath(APath: string);
const OPNAME = 'TModelFileNames.SetDamLevelsPath';
begin
  try
    APath := Trim(APath);
    if (APath <> '')  then  APath := IncludeTrailingPathDelimiter(APath);
    if(APath <> FSaltsWashoffPath) then
    begin
      FSaltsWashoffPath := APath;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TModelFileNames.GetDamLevelsPath: string;
const OPNAME = 'TModelFileNames.SetDamLevelsPath';
begin
  Result := '';
  try
    Result := FDamLevelsPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetDemandFilesPath: string;
const OPNAME = 'TModelFileNames.GetDemandFilesPath';
begin
  Result := '';
  try
    Result := FDemandFilesPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetDemandFilesPath(APath: string);
const OPNAME = 'TModelFileNames.SetDemandFilesPath';
var
  LContextData: TStringList;
  LOldValue    : string;
begin
  try
    APath := Trim(APath);
    if (APath <> '')  then  APath := IncludeTrailingPathDelimiter(APath);
    if(APath <> FDemandFilesPath) then
    begin
      LContextData := TStringList.Create;
      try
        FOutputFileNames.LoadContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SpecifiedDemandPath', APath, FDemandFilesPath, LContextData) then
        begin
          LOldValue := FDemandFilesPath;
          FDemandFilesPath := APath;
          FDemandFileNames.UpdateFilesPath(FDemandFilesPath);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SpecifiedDemandPath',LOldValue,APath);
        end;
      finally
        LContextData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetHydrologyFilesPath: string;
const OPNAME = 'TModelFileNames.GetHydrologyFilesPath';
begin
  Result := '';
  try
    Result := FHydrologyFilesPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetHydrologyFilesPath(APath: string);
const OPNAME = 'TModelFileNames.SetHydrologyFilesPath';
var
  LContextData: TStringList;
  LOldValue    : string;
begin
  try
    APath := Trim(APath);
    if (APath <> '')  then  APath := IncludeTrailingPathDelimiter(APath);
    if(APath <> FHydrologyFilesPath) then
    begin
      LContextData := TStringList.Create;
      try
        FHydrologyFileNames.LoadContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'HydrologyPath', APath, FHydrologyFilesPath, LContextData) then
        begin
          LOldValue := FHydrologyFilesPath;
          FHydrologyFilesPath := APath;
          FParamFileNames.UpdateFilesPath(FHydrologyFilesPath);
          FAltParamFileNames.UpdateFilesPath(FHydrologyFilesPath);
          FHydrologyFileNames.UpdateFilesPath(FHydrologyFilesPath);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'HydrologyPath',LOldValue,APath);
        end;
      finally
        LContextData.Free;
      end;
      if(Trim(FDemandFilesPath) = '') then
        FDemandFilesPath := FHydrologyFilesPath;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetFileNamePrefix: string;
const OPNAME = 'TModelFileNames.GetInputFilesPath';
begin
  Result := '';
  try
    Result := FFileNamePrefix;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetFileNamePrefix(APrefix: string);
const OPNAME = 'TModelFileNames.GetInputFilesPath';
begin
  try
    FFileNamePrefix := APrefix;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetInputFilesPath: string;
const OPNAME = 'TModelFileNames.GetInputFilesPath';
begin
  Result := '';
  try
    Result := FInputFilesPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetInputFilesPath(APath: string);
const OPNAME = 'TModelFileNames.SetInputFilesPath';
var
  LContextData: TStringList;
  LOldValue    : string;
begin
  try
    APath := Trim(APath);
    if (APath <> '')  then  APath := IncludeTrailingPathDelimiter(APath);
    if(APath <> FInputFilesPath) then
    begin
      LContextData := TStringList.Create;
      try
        FConfigFileNames.LoadContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'InputPath', APath, FInputFilesPath, LContextData) then
        begin
          LOldValue       := FInputFilesPath;
          FInputFilesPath := APath;
          DamLevelsPath   := IncludeTrailingPathDelimiter(APath+CDamLevels);
          FSaltsWashoffPath := IncludeTrailingPathDelimiter(APath+CWQT);
          FConfigFileNames.UpdateFilesPath(FInputFilesPath);
          FDirectoryFileNames.UpdateFilesPath(FInputFilesPath);
          FDamLevelsFileNames.UpdateFilesPath(FInputFilesPath);
          // Daily Diversion pre-processor
          FDailyDataFlowFileNames.UpdateFilesPath(FInputFilesPath);
          FDailyInstreamFlowFileNames.UpdateFilesPath(FInputFilesPath);

          FAllocationDefinitionFileNames.UpdateFilesPath(FInputFilesPath);
          FReservoirImplementationFileNames.UpdateFilesPath(FInputFilesPath);
          FDisbenefitDefinitionFileNames.UpdateFilesPath(FInputFilesPath);
          FGrowthFactorsFileNames.UpdateFilesPath(FInputFilesPath);
          FMonthlyWaterRequirementFileNames.UpdateFilesPath(FInputFilesPath);
          FHydropowerAllocationFileNames.UpdateFilesPath(FInputFilesPath);
          FPumpingChannelControlFileNames.UpdateFilesPath(FInputFilesPath);
          FGeneralChannelControlFileNames.UpdateFilesPath(FInputFilesPath);
          FReclamationPlantControlFileNames.UpdateFilesPath(FInputFilesPath);
          FReturnFlowChannelFileNames.UpdateFilesPath(FInputFilesPath);
          FChannelSwitchControlFileNames.UpdateFilesPath(FInputFilesPath);
          FTariffCalculationFileNames.UpdateFilesPath(FInputFilesPath);
          FAllocationChannelFileNames.UpdateFilesPath(FInputFilesPath);
          FReleaseStructureFileNames.UpdateFilesPath(FInputFilesPath);
          FCurtailFileNames.UpdateFilesPath(FInputFilesPath);
         // FSaltsWashoffFileNames.UpdateFilesPath(FInputFilesPath+'WQT\');
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InputPath',LOldValue,APath);
        end;
      finally
        LContextData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetOutputFilesPath: string;
const OPNAME = 'TModelFileNames.GetOutputFilesPath';
begin
  Result := '';
  try
    Result := FOutputFilesPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelFileNames.SetOutputFilesPath(APath: string);
const OPNAME = 'TModelFileNames.SetOutputFilesPath';
var
  LContextData: TStringList;
  LOldValue    : string;
begin
  try
    APath := Trim(APath);
    if (APath <> '')  then  APath := IncludeTrailingPathDelimiter(APath);
    if(APath <> FOutputFilesPath) then
    begin
      LContextData := TStringList.Create;
      try
        FOutputFileNames.LoadContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'OutputPath', APath, FOutputFilesPath, LContextData) then
        begin
          LOldValue := FOutputFilesPath;
          FOutputFilesPath := APath;
          FOutputFileNames.UpdateFilesPath(FOutputFilesPath);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'OutputPath',LOldValue,APath);
        end;
      finally
        LContextData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNamesList.CreateMemberObjects;
const OPNAME = 'TFileNamesList.CreateMemberObjects';
begin
  try
    inherited;
    FFileList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNamesList.DestroyMemberObjects;
const OPNAME = 'TFileNamesList.DestroyMemberObjects';
begin
  try
    inherited;
    FFileList.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNamesList.Clear;
const OPNAME = 'TFileNamesList.Clear';
begin
  try
    FFileList.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNamesList.Add(AObject: TObject);
const OPNAME = 'TFileNamesList.Add';
begin
  try
    FFileList.Add(AObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.Count: integer;
const OPNAME = 'TFileNamesList.Count';
begin
  Result := 0;
  try
    Result := FFileList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFileNameObject.LoadContextData(AContextData: TStringList);
const OPNAME = 'TFileNameObject.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + IntToStr(FFileNumber));
    AContextData.Add('FileGroup='     + IntToStr(FFileGroup));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetFileDate: TDateTime;
const OPNAME = 'TFileNameObject.GetFileDate';
begin
  Result := 0.0;
  try
    //if FileFound then
    //  Result := FileLastWriteDate(FileName)
    //else
    Result := FFileDate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetImportDate: TDateTime;
const OPNAME = 'TFileNameObject.GetImportDate';
begin
  Result := 0.0;
  try
    Result := FImportDate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetFileDate(ADate: TDateTime);
const OPNAME = 'TFileNameObject.SetFileDate';
begin
  try
    FFileDate := ADate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetImportDate(ADate: TDateTime);
const OPNAME = 'TFileNameObject.SetImportDate';
begin
  try
    FImportDate := ADate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetDamLevelsFileNames : TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetDamLevelsFileNames';
begin
  Result := nil;
  try
    Result := FDamLevelsFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetAllocationDefinitionFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetAllocationDefinitionFileNames';
begin
  Result := nil;
  try
    Result := FAllocationDefinitionFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetSaltsWashoffFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetSaltsWashoffFileNames';
begin
  Result := nil;
  try
    Result := FSaltsWashoffFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TModelFileNames.GetChannelSwitchControlFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetChannelSwitchControlFileNames';
begin
  Result := nil;
  try
    Result := FChannelSwitchControlFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetGeneralChannelControlFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetGeneralChannelControlFileNames';
begin
  Result := nil;
  try
    Result := FGeneralChannelControlFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetPumpingChannelControlFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetPumpingChannelControlFileNames';
begin
  Result := nil;
  try
    Result := FPumpingChannelControlFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetReclamationPlantControlFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetReclamationPlantControlFileNames';
begin
  Result := nil;
  try
    Result := FReclamationPlantControlFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetReservoirImplementationFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetReservoirImplementationFileNames';
begin
  Result := nil;
  try
    Result := FReservoirImplementationFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetAllocationChannelFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetAllocationChannelFileNames';
begin
  Result := nil;
  try
    Result := FAllocationChannelFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetDisbenefitDefinitionFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetDisbenefitDefinitionFileNames';
begin
  Result := nil;
  try
    Result := FDisbenefitDefinitionFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetHydropowerAllocationFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetHydropowerAllocationFileNames';
begin
  Result := nil;
  try
    Result := FHydropowerAllocationFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetMineFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetSaltsWashoffFileNames';
begin
  Result := nil;
  try
    Result := FMineFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetMineRainfallFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetSaltsWashoffFileNames';
begin
  Result := nil;
  try
    Result := FMineRainfallFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetMonthlyWaterRequirementFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetMonthlyWaterRequirementFileNames';
begin
  Result := nil;
  try
    Result := FMonthlyWaterRequirementFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetReleaseStructureFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetReleaseStructureFileNames';
begin
  Result := nil;
  try
    Result := FReleaseStructureFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TModelFileNames.GetCurtailFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetCurtailFileNames';
begin
  Result := nil;
  try
    Result := FCurtailFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetDailyDataFlowFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetDailyDataFlowFileNames';
begin
  Result := nil;
  try
    Result := FDailyDataFlowFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetDailyInstreamFlowFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetDailyInstreamFlowFileNames';
begin
  Result := nil;
  try
    Result := FDailyInstreamFlowFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TModelFileNames.GetReturnFlowChannelFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetReturnFlowChannelFileNames';
begin
  Result := nil;
  try
    Result := FReturnFlowChannelFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.GetTariffCalculationFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetTariffCalculationFileNames';
begin
  Result := nil;
  try
    Result := FTariffCalculationFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.AddDamLevelsFileName(AFileIndex: integer;AFileName: string;
  ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddDamLevelsFileName';
var
  LFileName : string;
  LPath     : string;
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileName := ExtractFileName(AFileName);
    LPath     := ExtractFilePath(AFileName);
    if(LPath = '') then
      LPath := FDamLevelsPath;
    if (not FileNameExists(AFileName,FDamLevelsFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(LPath,LFileName,ASavedToDB,fgDamWaterLevels,AFileIndex,AImportDate,AFileDate);
      FDamLevelsFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := True;
      LFileNameObject.Importable   := False;
      LFileNameObject.Exportable   := False;
      LFileNameObject.Validatable  := False;

    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.AddDailyDataFlowFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
         AImportDate : TDateTime; AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddDailyDataFlowFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if (not FileNameExists(AFileName,FDailyDataFlowFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(ExtractFilePath(AFileName),Trim(AFileName),ASavedToDB,fgDailyDataFlow,AFileIndex,AImportDate,AFileDate);
      FDailyDataFlowFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := False;
      LFileNameObject.Importable   := True;
      LFileNameObject.Exportable   := True;
      LFileNameObject.Validatable  := True;

    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.AddDailyInstreamFlowFileName(AFileIndex: integer;AFileName: string; ASavedToDB:Boolean;
         AImportDate : TDateTime; AFileDate : TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddDailyInstreamFlowFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if (not FileNameExists(AFileName,FDailyInstreamFlowFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(ExtractFilePath(AFileName),Trim(AFileName),ASavedToDB,fgDailyInstreamFlow,AFileIndex,AImportDate,AFileDate);
      FDailyInstreamFlowFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := False;
      LFileNameObject.Importable   := True;
      LFileNameObject.Exportable   := True;
      LFileNameObject.Validatable  := True;

    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileNamesList.DeleteFileName(AFileNumber: integer): boolean;
const OPNAME = 'TFileNamesList.DeleteFileName';
var
  LIndex: integer;
  LModelFileName:TAbstractModelFileName;
begin
  Result := False;
  try
    for LIndex := 0 to FFileList.Count-1 do
    begin
      LModelFileName := FileNameObject[LIndex];
      if(LModelFileName.FileNumber = AFileNumber) then
      begin
        FFileList.Delete(LIndex);
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetExportable: Boolean;
const OPNAME = 'TFileNameObject.GetExportable';
begin
  Result := False;
  try
    Result := FExportable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetImportable: Boolean;
const OPNAME = 'TFileNameObject.GetImportable';
begin
  Result := False;
  try
    Result := FImportable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetValidatable: Boolean;
const OPNAME = 'TFileNameObject.GetValidatable';
begin
  Result := False;
  try
    Result := FValidatable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNameObject.GetPopulated: Boolean;
const OPNAME = 'TFileNameObject.GetPopulated';
begin
  Result := False;
  try
    Result := FPopulated;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetExportable(AExportable: Boolean);
const OPNAME = 'TFileNameObject.SetExportable';
begin
  try
   FExportable := AExportable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetImportable(AImportable: Boolean);
const OPNAME = 'TFileNameObject.SetImportable';
begin
  try
   FImportable := AImportable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNameObject.SetValidatable(AValidatable: Boolean);
const OPNAME = 'TFileNameObject.SetValidatable';
begin
  try
   FValidatable := AValidatable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.HighestFileNumber: integer;
const OPNAME = 'TFileNamesList.HighestFileNumber';
var
  LIndex: integer;
  LModelFileName:TAbstractModelFileName;
begin
  Result := 0;
  try
    for LIndex := 0 to FFileList.Count-1 do
    begin
      LModelFileName := FileNameObject[LIndex];
      if(LModelFileName.FileNumber > Result) then
        Result := LModelFileName.FileNumber;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.AddAllocationDefinitionFileName(AFileIndex: integer;AFileName: string; ASavedToDB: Boolean; AImportDate,
         AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddAllocationDefinitionFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if (not FileNameExists(AFileName,FAllocationDefinitionFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgAllocationDefinition,AFileIndex,AImportDate,AFileDate);
      FAllocationDefinitionFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := False;
      LFileNameObject.Importable   := True;
      LFileNameObject.Exportable   := True;
      LFileNameObject.Validatable  := True;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.DeleteAllocationDefinitionFileName(AFileIndex: integer): boolean;
const OPNAME = 'TModelFileNames.DeleteAllocationDefinitionFileName';
begin
  Result := False;
  try
    Result := FAllocationDefinitionFileNames.DeleteFileName(AFileIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.AddSaltsWashoffFileNames(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddChannelSwitchControlFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if (not FileNameExists(AFileName,FSaltsWashoffFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(FSaltsWashoffPath,Trim(AFileName),ASavedToDB,fgSaltsWashoff,AFileIndex,AImportDate,AFileDate);
      FSaltsWashoffFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := False;
      LFileNameObject.Importable   := True;
      LFileNameObject.Exportable   := True;
      LFileNameObject.Validatable  := True;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.AddMineRainfallFileNames(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddChannelSwitchControlFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if (not FileNameExists(AFileName,FMineRainfallFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(FSaltsWashoffPath,Trim(AFileName),ASavedToDB,fgRAN,AFileIndex,AImportDate,AFileDate);
      FMineRainfallFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := False;
      LFileNameObject.Importable   := True;
      LFileNameObject.Exportable   := True;
      LFileNameObject.Validatable  := True;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.AddMineFileNames(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddChannelSwitchControlFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if (not FileNameExists(AFileName,FMineFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(FSaltsWashoffPath,Trim(AFileName),ASavedToDB,fgMIMM,AFileIndex,AImportDate,AFileDate);
      FMineFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := False;
      LFileNameObject.Importable   := True;
      LFileNameObject.Exportable   := True;
      LFileNameObject.Validatable  := True;
      TPlanningModelDataObject(FAppModules.Model.ModelData).CastFilesLineTypes.AddFile(LFileNameObject);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TModelFileNames.DeleteSaltsWashoffFileName(AFileIndex: integer): boolean;
const OPNAME = 'TModelFileNames.DeleteSaltsWashoffFileName';
begin
  Result := False;
  try
    Result := FSaltsWashoffFileNames.DeleteFileName(AFileIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.DeleteMineRainfallFileName(AFileIndex: integer): boolean;
const OPNAME = 'TModelFileNames.DeleteMineRainfallFileName';
begin
  Result := False;
  try
    Result := FMineRainfallFileNames.DeleteFileName(AFileIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.DeleteMineFileName(AFileIndex: integer): boolean;
const OPNAME = 'TModelFileNames.DeleteMineFileName';
begin
  Result := False;
  try
    Result := FMineFileNames.DeleteFileName(AFileIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.AddChannelSwitchControlFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.AddChannelSwitchControlFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if (not FileNameExists(AFileName,FChannelSwitchControlFileNames)) then
    begin
      LFileNameObject := TFileNameObject.Create(FAppModules);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgChannelSwitchControl,AFileIndex,AImportDate,AFileDate);
      FChannelSwitchControlFileNames.Add(LFileNameObject);
      LFileNameObject.FileReadOnly := False;
      LFileNameObject.Importable   := True;
      LFileNameObject.Exportable   := True;
      LFileNameObject.Validatable  := True;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.DeleteChannelSwitchControlFileName(AFileIndex: integer): boolean;
const OPNAME = 'TModelFileNames.DeleteChannelSwitchControlFileName';
begin
  Result := False;
  try
    Result := FChannelSwitchControlFileNames.DeleteFileName(AFileIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.UpdateGeneralChannelControlFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateGeneralChannelControlFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FGeneralChannelControlFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FGeneralChannelControlFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgGeneralChannelControl,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdatePumpingChannelControlFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate,AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdatePumpingChannelControlFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FPumpingChannelControlFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FPumpingChannelControlFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgPumpingChannelControl,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateReclamationPlantControlFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateReclamationPlantControlFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FReclamationPlantControlFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FReclamationPlantControlFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgReclamationPlantControl,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateReservoirImplementationFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate,AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateReservoirImplementationFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FReservoirImplementationFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FReservoirImplementationFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgReservoirImplementation,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetGrowthFactorsFileNames: TAbstractFileNamesList;
const OPNAME = 'TModelFileNames.GetGrowthFactorsFileNames';
begin
  Result := nil;
  try
    Result := FGrowthFactorsFileNames;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelFileNames.UpdateGrowthFactorsFileName(AFileIndex: integer; AFileName: string; ASavedToDB: Boolean;
         AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateGrowthFactorsFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FGrowthFactorsFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FGrowthFactorsFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgGrowthFactors,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateAllocationChannelFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate,AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateAllocationChannelFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FAllocationChannelFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FAllocationChannelFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgAllocationChannel,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateDisbenefitDefinitionFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateDisbenefitDefinitionFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FDisbenefitDefinitionFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FDisbenefitDefinitionFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgDisbenefitDefinition,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateHydropowerAllocationFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateHydropowerAllocationFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FHydropowerAllocationFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FHydropowerAllocationFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgHydropowerAllocation,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateMonthlyWaterRequirementFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateMonthlyWaterRequirementFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FMonthlyWaterRequirementFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FMonthlyWaterRequirementFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgMonthlyWaterRequirement,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateReleaseStructureFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateReleaseStructureFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FReleaseStructureFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FReleaseStructureFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgReleaseStructure,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TModelFileNames.UpdateCurtailFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateCurtailFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try

    if(AFileIndex < FCurtailFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FCurtailFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgCurtail,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateMineFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateMineFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FMineFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FMineFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FSaltsWashoffPath,Trim(AFileName),ASavedToDB,fgMIMM,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateReturnFlowChannelFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateReturnFlowChannelFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FReturnFlowChannelFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FReturnFlowChannelFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgReturnFlowChannel,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.UpdateTariffCalculationFileName(AFileIndex: integer; AFileName: string;
         ASavedToDB: Boolean; AImportDate, AFileDate: TDateTime): boolean;
const OPNAME = 'TModelFileNames.UpdateTariffCalculationFileName';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(AFileIndex < FTariffCalculationFileNames.Count) then
    begin
      LFileNameObject := TFileNameObject(FTariffCalculationFileNames[AFileIndex]);
      LFileNameObject.SetFileName(FInputFilesPath,Trim(AFileName),ASavedToDB,fgTariffCalculation,AFileIndex + 1,AImportDate,AFileDate);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.UpdateFilesPath(APath: string): boolean;
const OPNAME = 'TFileNamesList.UpdateFilesPath';
var
  LIndex: integer;
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    if(APath <> '') then
      APath := IncludeTrailingPathDelimiter(APath);
    for LIndex := 0 to FFileList.Count-1 do
    begin
      LFileNameObject := CastFileObject[LIndex];
      LFileNameObject.FCurrentPath := APath;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNamesList.GetCastFileObject(AIndex: integer): TFileNameObject;
const OPNAME = 'TFileNamesList.GetCastFileObject';
begin
  Result := nil;
  try
    if (AIndex < FFileList.Count) and Assigned(FFileList.Items[AIndex]) then
      Result := TFileNameObject(FFileList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.PopulateDemandFilesPaths(ADemandFilesPath: string): boolean;
const OPNAME = 'TModelFileNames.PopulateDemandFilesPaths';
begin
  Result := False;
  try
    FDemandFilesPath           := ADemandFilesPath;
    FDemandFileNames.UpdateFilesPath(FDemandFilesPath);
    Result                     := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.PopulateHydrologyPaths(AHydrologyFilesPath: string): boolean;
const OPNAME = 'TModelFileNames.PopulateHydrologyPaths';
begin
  Result := False;
  try
    FHydrologyFilesPath        := AHydrologyFilesPath;
    FParamFileNames.UpdateFilesPath(FHydrologyFilesPath);
    FAltParamFileNames.UpdateFilesPath(FHydrologyFilesPath);
    FHydrologyFileNames.UpdateFilesPath(FHydrologyFilesPath);
    FOutputFileNames.UpdateFilesPath(FOutputFilesPath);
    FDamLevelsFileNames.UpdateFilesPath(FHydrologyFilesPath);
    if(Trim(FDemandFilesPath) = '') then
      PopulateDemandFilesPaths(FHydrologyFilesPath);
    Result                     := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.PopulateInputFilesPaths(AInputFilesPath: string): boolean;
const OPNAME = 'TModelFileNames.PopulateInputFilesPaths';
begin
  Result := False;
  try
    FInputFilesPath            := IncludeTrailingPathDelimiter(AInputFilesPath);
    FDamLevelsPath             := IncludeTrailingPathDelimiter(FInputFilesPath+ CDamLevels);
    FSaltsWashoffPath          := IncludeTrailingPathDelimiter(FInputFilesPath+ CWQT);
    FConfigFileNames.UpdateFilesPath(FInputFilesPath);
    FDirectoryFileNames.UpdateFilesPath(FInputFilesPath);
    FDailyDataFlowFileNames.UpdateFilesPath(FInputFilesPath);
    FDailyInstreamFlowFileNames.UpdateFilesPath(FInputFilesPath);
    FAllocationDefinitionFileNames.UpdateFilesPath(FInputFilesPath);
    FReservoirImplementationFileNames.UpdateFilesPath(FInputFilesPath);
    FDisbenefitDefinitionFileNames.UpdateFilesPath(FInputFilesPath);
    FGrowthFactorsFileNames.UpdateFilesPath(FInputFilesPath);
    FMonthlyWaterRequirementFileNames.UpdateFilesPath(FInputFilesPath);
    FHydropowerAllocationFileNames.UpdateFilesPath(FInputFilesPath);
    FPumpingChannelControlFileNames.UpdateFilesPath(FInputFilesPath);
    FGeneralChannelControlFileNames.UpdateFilesPath(FInputFilesPath);
    FReclamationPlantControlFileNames.UpdateFilesPath(FInputFilesPath);
    FReturnFlowChannelFileNames.UpdateFilesPath(FInputFilesPath);
    FChannelSwitchControlFileNames.UpdateFilesPath(FInputFilesPath);
    FTariffCalculationFileNames.UpdateFilesPath(FInputFilesPath);
    FAllocationChannelFileNames.UpdateFilesPath(FInputFilesPath);
    FReleaseStructureFileNames.UpdateFilesPath(FInputFilesPath);
    FCurtailFileNames.UpdateFilesPath(FInputFilesPath);
   // FSaltsWashoffFileNames.UpdateFilesPath(FSaltsWashoffPath);
    Result                     := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.PopulateOutputPaths(AOutputFilesPath: string): boolean;
const OPNAME = 'TModelFileNames.PopulateOutputPaths';
begin
  Result := False;
  try
    FOutputFilesPath           := AOutputFilesPath;
    Result                     := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNamesList.LoadContextData(AContextData: TStringList);
const OPNAME = 'TFileNamesList.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetRunoffFileNamesCommaText: string;
const OPNAME = 'TModelFileNames.GetRunoffFileNamesCommaText';
var
  LIndex     : integer;
  LExtension : string;
  LContainer : TStringList;
  LFileNameObject  : TAbstractModelFileName;
begin
  Result := '';
  try
    LContainer := TStringList.Create;
    try
      for LIndex := 0 to FHydrologyFileNames.Count-1 do
      begin
        LFileNameObject := FHydrologyFileNames.FileNameObject[LIndex];
        if(LFileNameObject <> nil) then
        begin
          LExtension := UpperCase(ExtractFileExt(LFileNameObject.FileName));
          if(LExtension = '.R') then
            LContainer.Add(ExtractFileName(LFileNameObject.FileName));
        end;
      end;
      Result := LContainer.CommaText;
    finally
      LContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.GetSoilMoistureFileNamesCommaText: string;
const OPNAME = 'TModelFileNames.GetSoilMoistureFileNamesCommaText';
var
  LIndex     : integer;
  LExtension : string;
  LContainer : TStringList;
  LFileNameObject  : TAbstractModelFileName;
begin
  Result := '';
  try
    LContainer := TStringList.Create;
    try
      for LIndex := 0 to FHydrologyFileNames.Count-1 do
      begin
        LFileNameObject := FHydrologyFileNames.FileNameObject[LIndex];
        if(LFileNameObject <> nil) then
        begin
          LExtension := UpperCase(ExtractFileExt(LFileNameObject.FileName));
          if(LExtension = '.S') then
            LContainer.Add(ExtractFileName(LFileNameObject.FileName));
        end;
      end;
      Result := LContainer.CommaText;
    finally
      LContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelFileNames.SelectAllFiles(ASelected: boolean): boolean;
const OPNAME = 'TModelFileNames.SelectAllFiles';
begin
  Result := False;
  try
    FConfigFileNames.SetAllSelected(ASelected);
    FParamFileNames.SetAllSelected(ASelected);
    FAltParamFileNames.SetAllSelected(ASelected);
    FDirectoryFileNames.SetAllSelected(ASelected);
    FDemandFileNames.SetAllSelected(ASelected);
    FHydrologyFileNames.SetAllSelected(ASelected);
    FOutputFileNames.SetAllSelected(ASelected);
    FDamLevelsFileNames.SetAllSelected(ASelected);
    FDailyDataFlowFileNames.SetAllSelected(ASelected);
    FDailyInstreamFlowFileNames.SetAllSelected(ASelected);
    FAllocationDefinitionFileNames.SetAllSelected(ASelected);
    FReservoirImplementationFileNames.SetAllSelected(ASelected);
    FPumpingChannelControlFileNames.SetAllSelected(ASelected);
    FGeneralChannelControlFileNames.SetAllSelected(ASelected);
    FReclamationPlantControlFileNames.SetAllSelected(ASelected);
    FChannelSwitchControlFileNames.SetAllSelected(ASelected);
    FGrowthFactorsFileNames.SetAllSelected(ASelected);
    FCurtailFileNames.SetAllSelected(ASelected);
    FSaltsWashoffFileNames.SetAllSelected(ASelected);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

