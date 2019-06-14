//
//
//  UNIT      : Contains TFilesActionYieldManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/03/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFilesActionYieldManager;

interface

uses
  classes,
  Contnrs,
  VCL.Controls,
  VCL.Dialogs,
  VCL.StdCtrls,
  VoaimsCom_TLB,
  UDataFileObjects,
  UAbstractObject,
  UFileNames,
  UFilesActionAbstractManager,

  UFile01Agent,
  UFile02Agent,
  UFile03Agent,
  UFile04Agent,
  UFile05Agent,
  UFile06Agent,
  UFile07Agent,
  UFile08Agent,
  UFile09Agent,
  UFile10Agent,
  UFile11Agent,
  UFile12Agent,
  UFile13Agent,
  UFile14Agent,
  UFile15Agent,
  UFile16Agent,
  UFile17Agent,
  UFile18Agent,
  UFile19Agent,
  UFile20Agent,
  UFile21Agent,
  UFile22Agent,
  UFilePathsAgent,
  UFileParamAgent,
  UFileAltParamAgent,
  UDemandFileAgent,
  UHydrologyFileAgent,
  UUnKnownFileAgent,
  URunYieldModelAgent,
  USumOutFileManager,
  UPlotFileManager,
  //UReservoirHydrologyFilesFileAgent,

  UFile01DatabaseAgent,
  UFile02DatabaseAgent,
  UFile03DatabaseAgent,
  UFile04DatabaseAgent,
  UFile05DatabaseAgent,
  UFile06DatabaseAgent,
  UFile07DatabaseAgent,
  UFile08DatabaseAgent,
  UFile09DatabaseAgent,
  UFile10DatabaseAgent,
  UFile11DatabaseAgent,
  UFile12DatabaseAgent,
  UFile13DatabaseAgent,
  UFile14DatabaseAgent,
  UFile15DatabaseAgent,
  UFile16DatabaseAgent,
  UFile17DatabaseAgent,
  UFile18DatabaseAgent,
  UFile19DatabaseAgent,
  UFile20DatabaseAgent,
  UFile21DatabaseAgent,
  UFile22DatabaseAgent,
  UFilePathsDatabaseAgent,
  UFileParamDatabaseAgent,
  UFileAltParamDatabaseAgent,
  UUknownDatabaseAgent,
  UDemandDatabaseAgent,
  UHydrologyDatabaseAgent,
  UScenarioDatabaseAgent,
  UMonthlyDamLevelsFileAgent,
  UUknownOutputFilesDatabaseAgent,

  UMonthlyDamLevelsDatabaseAgent,

  //UReservoirHydrologyFilesDatabaseAgent,
  //UYieldNetworkVisualiserLinkDatabaseAgent,
  UYieldModelDataObject,
  UAbstractFileNamesObject;

  //UFileNamesAgent,
  //USumOutFileManager,
  //URunYieldModelAgent;

type
  TStorageVsYieldData = class(TAbstractObject)
  protected
    FReservoirNumber : integer;
    FStartingStorage : TStringList;
    FMinTargetDraft  : TStringList;
    FMaxTargetDraft  : TStringList;
    FYield           : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetStartingStorageCommaText(ACommatextValue: string);
    function GetStartingStorageCommaText: string;
    procedure SetMinTargetDraftCommaText(ACommatextValue: string);
    function GetMinTargetDraftCommaText: string;
    procedure SetMaxTargetDraftCommaText(ACommatextValue: string);
    function GetMaxTargetDraftCommaText: string;
    procedure SetYieldCommaText(ACommatextValue: string);
    function GetYieldCommaText: string;
  public
    function Initialise: boolean; override;
    property ReservoirNumber          : integer read FReservoirNumber            write FReservoirNumber;
    property StartingStorageCommaText : string  read GetStartingStorageCommaText write SetStartingStorageCommaText;
    property MinTargetDraftCommaText  : string  read GetMinTargetDraftCommaText  write SetMinTargetDraftCommaText;
    property MaxTargetDraftCommaText  : string  read GetMaxTargetDraftCommaText  write SetMaxTargetDraftCommaText;
    property YieldCommaText           : string  read GetYieldCommaText           write SetYieldCommaText;
  end;


  TFilesActionYieldManager = class(TFilesActionAbstractManager,IIterationEventHandler)
  protected
    FFile01Agent: TFile01Agent;
    FFile02Agent: TFile02Agent;
    FFile03Agent: TFile03Agent;
    FFile04Agent: TFile04Agent;
    FFile05Agent: TFile05Agent;
    FFile06Agent: TFile06Agent;
    FFile07Agent: TFile07Agent;
    FFile08Agent: TFile08Agent;
    FFile09Agent: TFile09Agent;
    FFile10Agent: TFile10Agent;
    FFile11Agent: TFile11Agent;
    FFile12Agent: TFile12Agent;
    FFile13Agent: TFile13Agent;
    FFile14Agent: TFile14Agent;
    FFile15Agent: TFile15Agent;
    FFile16Agent: TFile16Agent;
    FFile17Agent: TFile17Agent;
    FFile18Agent: TFile18Agent;
    FFile19Agent: TFile19Agent;
    FFile20Agent: TFile20Agent;
    FFile21Agent: TFile21Agent;
    FFile22Agent: TFile22Agent;
    FFilePathsAgent: TFilePathsAgent;
    FFileParamAgent: TFileParamAgent;
    FFileAltParamAgent: TFileAltParamAgent;
    FUnKnownFileAgent: TUnKnownFileAgent;
    FDemandFileAgent: TDemandFileAgent;
    FHydrologyFileAgent: THydrologyFileAgent;
    FRunModelAgent:TRunYieldModelAgent;
    FSumOutFileManager: TSumOutFileManager;
    FMonthlyDamLevelsFileAgent : TMonthlyDamLevelsFileAgent;
    //FReservoirHydrologyFilesFileAgent:TReservoirHydrologyFilesFileAgent;
    FPlotFileManager: TPlotFileManager;

    FFile01DatabaseAgent: TFile01DatabaseAgent;
    FFile02DatabaseAgent: TFile02DatabaseAgent;
    FFile03DatabaseAgent: TFile03DatabaseAgent;
    FFile04DatabaseAgent: TFile04DatabaseAgent;
    FFile05DatabaseAgent: TFile05DatabaseAgent;
    FFile06DatabaseAgent: TFile06DatabaseAgent;
    FFile07DatabaseAgent: TFile07DatabaseAgent;
    FFile08DatabaseAgent: TFile08DatabaseAgent;
    FFile09DatabaseAgent: TFile09DatabaseAgent;
    FFile10DatabaseAgent: TFile10DatabaseAgent;
    FFile11DatabaseAgent: TFile11DatabaseAgent;
    FFile12DatabaseAgent: TFile12DatabaseAgent;
    FFile13DatabaseAgent: TFile13DatabaseAgent;
    FFile14DatabaseAgent: TFile14DatabaseAgent;
    FFile15DatabaseAgent: TFile15DatabaseAgent;
    FFile16DatabaseAgent: TFile16DatabaseAgent;
    FFile17DatabaseAgent: TFile17DatabaseAgent;
    FFile18DatabaseAgent: TFile18DatabaseAgent;
    FFile19DatabaseAgent: TFile19DatabaseAgent;
    FFile20DatabaseAgent: TFile20DatabaseAgent;
    FFile21DatabaseAgent: TFile21DatabaseAgent;
    FFile22DatabaseAgent: TFile22DatabaseAgent;
    FFilePathsDatabaseAgent:TFilePathsDatabaseAgent;
    FFileParamDatabaseAgent:TFileParamDatabaseAgent;
    FFileAltParamDatabaseAgent:TFileAltParamDatabaseAgent;
    FScenarioDatabaseAgent: TScenarioDatabaseAgent;
    FDemandDatabaseAgent: TDemandDatabaseAgent;
    FHydrologyDatabaseAgent: THydrologyDatabaseAgent;
    FOutputFilesUknownDatabaseAgent: TUknownOutputFilesDatabaseAgent;
    FUknownDatabaseAgent: TUknownDatabaseAgent;

    FMonthlyDamLevelsDatabaseAgent : TMonthlyDamLevelsDatabaseAgent;
    FStorageVsYieldData            : TStorageVsYieldData;
    //FNetworkVisualiserDatabaseAgent:TYieldNetworkVisualiserLinkDatabaseAgent;
    //FReservoirHydrologyFilesDatabaseAgent: TReservoirHydrologyFilesDatabaseAgent;
    //FSumOutFileManager: TSumOutFileManager;
    FOldProgressMsg : string;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateRunYieldModelAgent; virtual;
    procedure AddExtraButtons; override;
    procedure OnViewRunOptionsDialog(Sender: TObject);
    function IncludeHydrologyFile(AFileName: TAbstractModelFileName): boolean;virtual;
    function IncludeDemandFile(AFileName: TAbstractModelFileName): boolean;virtual;
    function CheckModelFilesAreComplete(AProgressUpdateFuntion:TProgressUpdateFuntion): boolean; override;
    function ValidateFileName(AFileName: TAbstractModelFileName;AProgressUpdateFuntion:TProgressUpdateFuntion): boolean;override;
    function ValidateHydrologyFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
    function AdvanceProgressBar(ASteps: integer): boolean;
    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecValidateFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecRunModel(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecRunStorageVsYield(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; virtual;
    function GetFileNamesObjectCast: TModelFileNames;override;
    function CheckFileImportDate(AFilename : TAbstractModelFileName) : boolean; override;
    function OnIterationEvent(const AIterationName: WideString): WordBool; safecall;

  public
    function GetYieldChannelYield(var AValue: double): boolean; virtual;
    function DoRunModel: boolean; override;
    function ImportDemandFile(const AFileNAme: WideString): boolean;
    function ImportHydrologyFile(const AFileNAme: WideString): boolean;
    function ImportParamFile(const AFileNAme: WideString): boolean;
    function ImportPathsFile(const AFileNAme: WideString): boolean;
    function ImportAllHydrologyFiles: boolean;
    function ClearParamFile(const AFileNAme: WideString): boolean;
    function ImportDamWaterLevelsFile(const AFileNAme: WideString): boolean;
    function ReadFirmYieldFromDebugFile: Double; safecall;
    function DoRunStorageVsYield(AReservoirNumber: integer; const AStartingStorageCommaText: WideString;
             var AMinTargetDraftCommaText, AMaxTargetDraftCommaText, AYieldCommaText: WideString): WordBool;

  end;

implementation

uses
  System.UITypes,
  SysUtils,
  UUtilities,
  UConstants,
  URunOptionsDialog,
  UYieldContextValidationType,
//  UYRCGraphDataObject,
  UErrorHandlingOperations, Math;


{ TFilesActionYieldManager }

procedure TFilesActionYieldManager.CreateMemberObjects;
const OPNAME = 'TFilesActionYieldManager.CreateMemberObjects';
begin
  inherited  CreateMemberObjects;

  try
    FFile01Agent         := TFile01Agent.Create(FAppModules);
    FFile02Agent         := TFile02Agent.Create(FAppModules);
    FFile03Agent         := TFile03Agent.Create(FAppModules);
    FFile04Agent         := TFile04Agent.Create(FAppModules);
    FFile05Agent         := TFile05Agent.Create(FAppModules);
    FFile06Agent         := TFile06Agent.Create(FAppModules);
    FFile07Agent         := TFile07Agent.Create(FAppModules);
    FFile08Agent         := TFile08Agent.Create(FAppModules);
    FFile09Agent         := TFile09Agent.Create(FAppModules);
    FFile10Agent         := TFile10Agent.Create(FAppModules);
    FFile11Agent         := TFile11Agent.Create(FAppModules);
    FFile12Agent         := TFile12Agent.Create(FAppModules);
    FFile13Agent         := TFile13Agent.Create(FAppModules);
    FFile14Agent         := TFile14Agent.Create(FAppModules);
    FFile15Agent         := TFile15Agent.Create(FAppModules);
    FFile16Agent         := TFile16Agent.Create(FAppModules);
    FFile17Agent         := TFile17Agent.Create(FAppModules);
    FFile18Agent         := TFile18Agent.Create(FAppModules);
    FFile19Agent         := TFile19Agent.Create(FAppModules);
    FFile20Agent         := TFile20Agent.Create(FAppModules);
    FFile21Agent         := TFile21Agent.Create(FAppModules);
    FFile22Agent         := TFile22Agent.Create(FAppModules);
    FFilePathsAgent      := TFilePathsAgent.Create(FAppModules);
    FFileParamAgent      := TFileParamAgent.Create(FAppModules);
    FFileAltParamAgent   := TFileAltParamAgent.Create(FAppModules);
    FUnKnownFileAgent    := TUnKnownFileAgent.Create(FAppModules);
    FDemandFileAgent     := TDemandFileAgent.Create(FAppModules);
    FHydrologyFileAgent  := THydrologyFileAgent.Create(FAppModules);
    FSumOutFileManager   := TSumOutFileManager.Create(FAppModules);
    FMonthlyDamLevelsFileAgent := TMonthlyDamLevelsFileAgent.Create(FAppModules);
    //FReservoirHydrologyFilesFileAgent := TReservoirHydrologyFilesFileAgent.Create(FAppModules);
    FPlotFileManager     := TPlotFileManager.Create(FAppModules);

    FFile01DatabaseAgent    := TFile01DatabaseAgent.Create(FAppModules);
    FFile02DatabaseAgent    := TFile02DatabaseAgent.Create(FAppModules);
    FFile03DatabaseAgent    := TFile03DatabaseAgent.Create(FAppModules);
    FFile04DatabaseAgent    := TFile04DatabaseAgent.Create(FAppModules);
    FFile05DatabaseAgent    := TFile05DatabaseAgent.Create(FAppModules);
    FFile06DatabaseAgent    := TFile06DatabaseAgent.Create(FAppModules);
    FFile07DatabaseAgent    := TFile07DatabaseAgent.Create(FAppModules);
    FFile08DatabaseAgent    := TFile08DatabaseAgent.Create(FAppModules);
    FFile09DatabaseAgent    := TFile09DatabaseAgent.Create(FAppModules);
    FFile10DatabaseAgent    := TFile10DatabaseAgent.Create(FAppModules);
    FFile11DatabaseAgent    := TFile11DatabaseAgent.Create(FAppModules);
    FFile12DatabaseAgent    := TFile12DatabaseAgent.Create(FAppModules);
    FFile13DatabaseAgent    := TFile13DatabaseAgent.Create(FAppModules);
    FFile14DatabaseAgent    := TFile14DatabaseAgent.Create(FAppModules);
    FFile15DatabaseAgent    := TFile15DatabaseAgent.Create(FAppModules);
    FFile16DatabaseAgent    := TFile16DatabaseAgent.Create(FAppModules);
    FFile17DatabaseAgent    := TFile17DatabaseAgent.Create(FAppModules);
    FFile18DatabaseAgent    := TFile18DatabaseAgent.Create(FAppModules);
    FFile19DatabaseAgent    := TFile19DatabaseAgent.Create(FAppModules);
    FFile20DatabaseAgent    := TFile20DatabaseAgent.Create(FAppModules);
    FFile21DatabaseAgent    := TFile21DatabaseAgent.Create(FAppModules);
    FFile22DatabaseAgent    := TFile22DatabaseAgent.Create(FAppModules);
    FFilePathsDatabaseAgent := TFilePathsDatabaseAgent.Create(FAppModules);
    FFileParamDatabaseAgent := TFileParamDatabaseAgent.Create(FAppModules);
    FFileAltParamDatabaseAgent := TFileAltParamDatabaseAgent.Create(FAppModules);
    FScenarioDatabaseAgent  := TScenarioDatabaseAgent.Create(FAppModules);
    FUknownDatabaseAgent    := TUknownDatabaseAgent.Create(FAppModules);
    FDemandDatabaseAgent    := TDemandDatabaseAgent.Create(FAppModules);
    FHydrologyDatabaseAgent := THydrologyDatabaseAgent.Create(FAppModules);
    //FNetworkVisualiserDatabaseAgent := TYieldNetworkVisualiserLinkDatabaseAgent.Create(FAppModules);
    FOutputFilesUknownDatabaseAgent := TUknownOutputFilesDatabaseAgent.Create(FAppModules);
    //FReservoirHydrologyFilesDatabaseAgent := TReservoirHydrologyFilesDatabaseAgent.Create(FAppModules);
    FMonthlyDamLevelsDatabaseAgent := TMonthlyDamLevelsDatabaseAgent.Create(FAppModules);
    FStorageVsYieldData            := TStorageVsYieldData.Create;
    CreateRunYieldModelAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionYieldManager.DestroyMemberObjects;
const OPNAME = 'TFilesActionYieldManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFile01Agent);
    FreeAndNil(FFile02Agent);
    FreeAndNil(FFile03Agent);
    FreeAndNil(FFile04Agent);
    FreeAndNil(FFile05Agent);
    FreeAndNil(FFile06Agent);
    FreeAndNil(FFile07Agent);
    FreeAndNil(FFile08Agent);
    FreeAndNil(FFile09Agent);
    FreeAndNil(FFile10Agent);
    FreeAndNil(FFile11Agent);
    FreeAndNil(FFile12Agent);
    FreeAndNil(FFile13Agent);
    FreeAndNil(FFile14Agent);
    FreeAndNil(FFile15Agent);
    FreeAndNil(FFile16Agent);
    FreeAndNil(FFile17Agent);
    FreeAndNil(FFile18Agent);
    FreeAndNil(FFile19Agent);
    FreeAndNil(FFile20Agent);
    FreeAndNil(FFile21Agent);
    FreeAndNil(FFile22Agent);
    FreeAndNil(FFilePathsAgent);
    FreeAndNil(FFileParamAgent);
    FreeAndNil(FFileAltParamAgent);
    FreeAndNil(FUnKnownFileAgent);
    FreeAndNil(FDemandFileAgent);
    FreeAndNil(FHydrologyFileAgent);
    FreeAndNil(FRunModelAgent);
    FreeAndNil(FSumOutFileManager);
    FreeandNil(FMonthlyDamLevelsFileAgent);
    //FreeAndNil(FReservoirHydrologyFilesFileAgent);
    FreeAndNil(FPlotFileManager);

    FreeAndNil(FFile01DatabaseAgent);
    FreeAndNil(FFile02DatabaseAgent);
    FreeAndNil(FFile03DatabaseAgent);
    FreeAndNil(FFile04DatabaseAgent);
    FreeAndNil(FFile05DatabaseAgent);
    FreeAndNil(FFile06DatabaseAgent);
    FreeAndNil(FFile07DatabaseAgent);
    FreeAndNil(FFile08DatabaseAgent);
    FreeAndNil(FFile09DatabaseAgent);
    FreeAndNil(FFile10DatabaseAgent);
    FreeAndNil(FFile11DatabaseAgent);
    FreeAndNil(FFile12DatabaseAgent);
    FreeAndNil(FFile13DatabaseAgent);
    FreeAndNil(FFile14DatabaseAgent);
    FreeAndNil(FFile15DatabaseAgent);
    FreeAndNil(FFile16DatabaseAgent);
    FreeAndNil(FFile17DatabaseAgent);
    FreeAndNil(FFile18DatabaseAgent);
    FreeAndNil(FFile19DatabaseAgent);
    FreeAndNil(FFile20DatabaseAgent);
    FreeAndNil(FFile21DatabaseAgent);
    FreeAndNil(FFile22DatabaseAgent);
    FreeAndNil(FFilePathsDatabaseAgent);
    FreeAndNil(FFileParamDatabaseAgent);
    FreeAndNil(FFileAltParamDatabaseAgent);
    FreeAndNil(FScenarioDatabaseAgent);
    FreeAndNil(FUknownDatabaseAgent);
    FreeAndNil(FDemandDatabaseAgent);
    FreeAndNil(FHydrologyDatabaseAgent);
    //FreeAndNil(FNetworkVisualiserDatabaseAgent);
    FreeAndNil(FOutputFilesUknownDatabaseAgent);
    //FreeAndNil(FReservoirHydrologyFilesDatabaseAgent);
    FreeAndNil(FMonthlyDamLevelsDatabaseAgent);
    FreeAndNil(FStorageVsYieldData);

    inherited  DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionYieldManager.CreateRunYieldModelAgent;
const OPNAME = 'TFilesActionYieldManager.CreateRunYieldModelAgent';
begin
  try
    FRunModelAgent  := TRunYieldModelAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionYieldManager.ExecValidateFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecValidateFiles';
begin
  Result := False;
  try
    Result := ExecLoadDataFromFiles(AProgressUpdateFuntion);
    if(FFileAction in [fatValidateAll,fatImportAll]) then
    begin
      if not UpdateProgress(Result) then Exit;
      Result := Result and FDataFileObjects.ValidateFileData(FAppModules,AProgressUpdateFuntion);
      Result := Result and ValidateHydrologyFiles(AProgressUpdateFuntion);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ValidateHydrologyFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ValidateHydrologyFiles';
function ValidHydrologyFile(AFileName: string): boolean;
const OPNAME = 'UFilesActionYieldManager.ValidHydrologyFile';
var
  LFileExt : string;
begin
  LFileExt := UpperCase(ExtractFileExt(AFileName));
  Result := (LFileExt = '.INC') or
            (LFileExt = '.RAN') or
            (LFileExt = '.IRR') or
            (LFileExt = '.AFF') or
            (LFileExt = '.S') or
            (LFileExt = '.DEM');

end;
var
  LStarYear : integer;
  LEndYear  : integer;
  LRowCount : integer;

  LCurrentStarYear : integer;
  LCurrentEndYear  : integer;
  LCurrentRowCount : integer;

  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LMessage,
  LFirstFileName : string;
  LStop: boolean;
begin
  Result := True;
  try
    LStarYear := 0;
    LEndYear  := 0;
    LRowCount := 0;
    LFirstFileName := '';

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not ValidHydrologyFile(LCurrentFileNames.FileNameObject[LCount].FileName) then
         Continue;
      if not IncludeDemandFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LCurrentStarYear := 0;
        LCurrentEndYear  := 0;
        LCurrentRowCount := 0;
        LResult := FDemandFileAgent.GetFileStatistics(LCurrentFileNames.FileNameObject[LCount],
                   LCurrentStarYear,LCurrentEndYear,LCurrentRowCount);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;

        if not LResult then Continue;
        if(LFirstFileName = '') then
        begin
          LStarYear := LCurrentStarYear;
          LEndYear  := LCurrentEndYear;
          LRowCount := LCurrentRowCount;
          LFirstFileName := LCurrentFileNames.FileNameObject[LCount].ShortName;
        end
        else
        begin
          if(LStarYear <> LCurrentStarYear) then
          begin
            LMessage := FAppModules.Language.GetString('TDemandFileAgent.strFileStartYearError');
            LMessage := Format(LMessage,[LCurrentFileNames.FileNameObject[LCount].ShortName,LFirstFileName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LEndYear <> LCurrentEndYear) then
          begin
            LMessage := FAppModules.Language.GetString('TDemandFileAgent.strFileEndYearError');
            LMessage := Format(LMessage,[LCurrentFileNames.FileNameObject[LCount].ShortName,LFirstFileName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LRowCount <> LCurrentRowCount) then
          begin
            LMessage := FAppModules.Language.GetString('TDemandFileAgent.strFileLengthError');
            LMessage := Format(LMessage,[LCurrentFileNames.FileNameObject[LCount].ShortName,LFirstFileName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;
      end;
    end;


    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not ValidHydrologyFile(LCurrentFileNames.FileNameObject[LCount].FileName) then
         Continue;
      if not IncludeHydrologyFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LCurrentStarYear := 0;
        LCurrentEndYear  := 0;
        LCurrentRowCount := 0;
        LResult := FHydrologyFileAgent.GetFileStatistics(LCurrentFileNames.FileNameObject[LCount],
                   LCurrentStarYear,LCurrentEndYear,LCurrentRowCount);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;

        if not LResult then Continue;
        if(LFirstFileName = '') then
        begin
          LStarYear := LCurrentStarYear;
          LEndYear  := LCurrentEndYear;
          LRowCount := LCurrentRowCount;
          LFirstFileName := LCurrentFileNames.FileNameObject[LCount].ShortName;
        end
        else
        begin
          if(LStarYear <> LCurrentStarYear) then
          begin
            LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strFileStartYearError');
            LMessage := Format(LMessage,[LCurrentFileNames.FileNameObject[LCount].ShortName,LFirstFileName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LEndYear <> LCurrentEndYear) then
          begin
            LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strFileEndYearError');
            LMessage := Format(LMessage,[LCurrentFileNames.FileNameObject[LCount].ShortName,LFirstFileName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LRowCount <> LCurrentRowCount) then
          begin
            LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strFileLengthError');
            LMessage := Format(LMessage,[LCurrentFileNames.FileNameObject[LCount].ShortName,LFirstFileName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ExecLoadDataFromFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecLoadDataFromFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LConfigCount,
  LCount: integer;
  LResult: boolean;
  LColumns : TStringList;
  LValidationErrors : TStringList;
  LErrors : WideString;
  LErrorMsg : string;
  LStop : boolean;
begin
  Result := False;
  try
    Result := True;

    //Directory file(Wrym.dat) (Start with the paths in case they are wrong)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Configuration files(F01..F13)
    LConfigCount := 0;
    LCurrentFileNames := FileNamesObject.ConfigFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFile01Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
    begin
      LResult := FFile02Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[01],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[01]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion) then
    begin
      LResult := FFile03Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[02],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[02]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion) then
    begin
      LResult := FFile04Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[03],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[03]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion) then
    begin
      LResult := FFile05Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[04],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[04]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion) then
    begin
      LResult := FFile06Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[05],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[05]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[06],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[06],AProgressUpdateFuntion) then
    begin
      LResult := FFile07Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[06],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[06]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[07],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[07],AProgressUpdateFuntion) then
    begin
      LResult := FFile08Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[07],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[07]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[09],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[08],AProgressUpdateFuntion) then
    begin
      LResult := FFile09Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[08],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[08]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[09],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[09],AProgressUpdateFuntion) then
    begin
      LResult := FFile10Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[09],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[09]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[10],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[10],AProgressUpdateFuntion) then
    begin
      LResult := FFile11Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[10],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[10]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[11],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[11],AProgressUpdateFuntion) then
    begin
      LResult := FFile12Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[11],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[11]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[12],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[12],AProgressUpdateFuntion) then
    begin
      LResult := FFile13Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[12],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[12]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[13],AProgressUpdateFuntion);
    if FileExists(LCurrentFileNames.FileNameObject[13].FileName) and
       ValidateFileName(LCurrentFileNames.FileNameObject[13],AProgressUpdateFuntion) then
    begin
      LResult := FFile14Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[13],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[13]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[14],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[14],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile15Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[14],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[14]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;  


    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[15],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[15],AProgressUpdateFuntion) then
    begin
      LResult := FFile16Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[15],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[15]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[16],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[16],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile17Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[16],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[16]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[17],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[17],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile18Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[17],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[17]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[18],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[18],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile19Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[18],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[18]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[19],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[19],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile20Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[19],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[19]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[20],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[20],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile21Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[20],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[20]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;


    CheckFileModifiedDate(LCurrentFileNames.FileNameObject[21],AProgressUpdateFuntion);
    if ValidateFileName(LCurrentFileNames.FileNameObject[21],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile22Agent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[21],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[21]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    FDataFileObjects.AddNodesWithoutInflow;
    FDataFileObjects.UpdateNetworkElementsType;

    //Parameter file(Param.dat)
    LCurrentFileNames :=   FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
      begin
        LResult := FFileParamAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                   FDataFileObjects,AProgressUpdateFuntion);
        if LResult then
        begin
          LColumns := TStringList.Create;
          LValidationErrors := TStringList.Create;
          try
            TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.Validate(LErrors,'');
            while (LErrors <> '') do
            begin
              LErrors := CutErrorsAndColumns(LErrors,LValidationErrors,LColumns);
              for LCount := 1 to LValidationErrors.Count -1 do
              begin
                LErrorMsg := LValidationErrors[LCount];
                if(ErrorSeverity(LErrorMsg)= 1) then
                  AProgressUpdateFuntion(LErrorMsg,ptWarning,LStop)
                else
                  AProgressUpdateFuntion(LErrorMsg,ptError,LStop);
              end;
            end;
          finally
            FreeAndNil(LColumns);
            FreeAndNil(LValidationErrors);
          end;
        end;

        TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //AltParameter file(ParamFOR.dat)
    LCurrentFileNames :=   FileNamesObject.AltParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
      begin
        LResult := FFileAltParamAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                   FDataFileObjects,AProgressUpdateFuntion);

        TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    FDataFileObjects.FDemandFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeDemandFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FDemandFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;


    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeHydrologyFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    FDataFileObjects.UpdateSpecifiedInflowChanelsFileNames;
    FDataFileObjects.CreateNaturalInflowChannels;

    //Network Visualiser data
    if (LConfigCount >= 12) then;
    {LCurrentFileNames := FileNamesObject.ConfigFileNames;
    if (LConfigCount >= 12) and ((FFileAction in [fatValidateAll,fatImportAll]) or
       (LCurrentFileNames.SelectedCount >= 12)) then
    begin
      LResult := FNetworkVisualiserDatabaseAgent.ReadYieldNetworkVisualiserLinkData(FDataFileObjects,
                 AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;}

    //Reservoir Hydrology Files
    {LCurrentFileNames := FileNamesObject.ParamFileNames;
    if (FFileAction in [fatValidateAll,fatImportAll]) or
       ((FFileAction in [fatValidateSingle,fatImportSingle]) and (TFileNameObject(LCurrentFileNames.FileNameObject[00]).Selected)) then
    begin
      LResult := FReservoirHydrologyFilesFileAgent.ReadModelDataFromFile(FileNamesObject,FDataFileObjects,
                 AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ExecSaveDataToFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecSaveDataToFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LFileAction : TFileActionType;
begin
  Result := False;
  try
    Result := True;

    LFileAction := FFileAction;
    if (FFileAction = fatRunModel) then
      FFileAction := fatExportAll;
    try

      //Directory file(Wrym.dat)
      LCurrentFileNames :=  FileNamesObject.DirectoryFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFilePathsAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                   FDataFileObjects, AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      //Configuration files(F01..F13)
      LCurrentFileNames := FileNamesObject.ConfigFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFile01Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
      begin
        LResult := FFile02Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[01],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[01]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion) then
      begin
        LResult := FFile03Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[02],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[02]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion) then
      begin
        LResult := FFile04Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[03],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[03]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion) then
      begin
        LResult := FFile05Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[04],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[04]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion) then
      begin
        LResult := FFile06Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[05],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[05]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[06],AProgressUpdateFuntion) then
      begin
        LResult := FFile07Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[06],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[06]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[07],AProgressUpdateFuntion) then
      begin
        LResult := FFile08Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[07],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[07]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[08],AProgressUpdateFuntion) then
      begin
        LResult := FFile09Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[08],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[08]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[09],AProgressUpdateFuntion) then
      begin
        LResult := FFile10Agent.WriteModelDataToFile(TFileNameObject(LCurrentFileNames.FileNameObject[09]),FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[09]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[10],AProgressUpdateFuntion) then
      begin
        LResult := FFile11Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[10],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[10]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[11],AProgressUpdateFuntion) then
      begin
        LResult := FFile12Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[11],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[11]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[12],AProgressUpdateFuntion) then
      begin
        LResult := FFile13Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[12],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[12]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      //if LCurrentFileNames.FileNameObject[13].SavedInDB and
      if   ValidateFileName(LCurrentFileNames.FileNameObject[13],AProgressUpdateFuntion) then
      begin
        LResult := FFile14Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[13],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[13]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[14],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile15Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[14],FDataFileObjects,
                     AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[14]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[15],AProgressUpdateFuntion) then
      begin
        LResult := FFile16Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[15],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[15]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[16],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile17Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[16],FDataFileObjects,
                     AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[16]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;  
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[17],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile18Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[17],FDataFileObjects,
                     AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[17]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[18],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile19Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[18],FDataFileObjects,
                     AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[18]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[19],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile20Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[19],FDataFileObjects,
                     AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[19]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[20],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile21Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[20],FDataFileObjects,
                     AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[20]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[21],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile22Agent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[21],FDataFileObjects,
                     AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[21]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Parameter file(Param.dat)
      LCurrentFileNames := FileNamesObject.ParamFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
        begin
          LResult := FFileParamAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                     FDataFileObjects,AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //AltParameter file(ParamFOR.dat)
      LCurrentFileNames := FileNamesObject.AltParamFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
        begin
          LResult := FFileAltParamAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                     FDataFileObjects,AProgressUpdateFuntion);
          TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Demand files(*.abs,*.dem, ...)
      LCurrentFileNames :=FileNamesObject.DemandFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeDemandFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FDemandFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Hydrology files(*.irr,*.inc, ...)
      LCurrentFileNames := FileNamesObject.HydrologyFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeHydrologyFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FHydrologyFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;
    finally
     FFileAction := LFileAction;
    end;
    if Result then
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionYieldManager.ExecLoadDataFromDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecLoadDataFromDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LConfigCount,
  LCount: integer;
  LResult: boolean;
  LFileAction : TFileActionType;
begin
  Result := False;
  try
    Result := True;

    LFileAction := FFileAction;
    if (FFileAction = fatRunModel) then
      FFileAction := fatExportAll;
    try

      //Directory file(Wrym.dat)
      LCurrentFileNames := FileNamesObject.DirectoryFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFilePathsDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        FileNamesObject.PopulateHydrologyPaths(FDataFileObjects.FPathsObject.HydrologyPath.FData);
        if not UpdateProgress(LResult) then Exit;
      end;

      //Configuration files(F01..F13)
      LConfigCount := 0;
      LCurrentFileNames := FileNamesObject.ConfigFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFile01DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
      begin
        LResult := FFile02DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[01]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion) then
      begin
        LResult := FFile03DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[02]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion) then
      begin
        LResult := FFile04DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[03]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion) then
      begin
        LResult := FFile05DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[04]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion) then
      begin
        LResult := FFile06DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[05]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[06],AProgressUpdateFuntion) then
      begin
        LResult := FFile07DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[06]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[07],AProgressUpdateFuntion) then
      begin
        LResult := FFile08DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[07]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[08],AProgressUpdateFuntion) then
      begin
        LResult := FFile09DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[08]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[09],AProgressUpdateFuntion) then
      begin
        LResult := FFile10DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[09]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[10],AProgressUpdateFuntion) then
      begin
        LResult := FFile11DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[10]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[11],AProgressUpdateFuntion) then
      begin
        LResult := FFile12DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[11]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[12],AProgressUpdateFuntion) then
      begin
        LResult := FFile13DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[12]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      //if LCurrentFileNames.FileNameObject[13].SavedInDB and
      if ValidateFileName(LCurrentFileNames.FileNameObject[13],AProgressUpdateFuntion) then
      begin
        LResult := FFile14DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[13]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

     if ValidateFileName(LCurrentFileNames.FileNameObject[14],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile15DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[14]),FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
          if LResult then
             LConfigCount := LConfigCount + 1;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[15],AProgressUpdateFuntion) then
      begin
        LResult := FFile16DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[15]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[16],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile17DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[16]),FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
          if LResult then
             LConfigCount := LConfigCount + 1;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[17],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile18DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[17]),FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
          if LResult then
             LConfigCount := LConfigCount + 1;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[18],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile19DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[18]),FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
          if LResult then
             LConfigCount := LConfigCount + 1;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[19],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile20DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[19]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
          if LResult then
             LConfigCount := LConfigCount + 1;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[20],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile21DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[20]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
          if LResult then
             LConfigCount := LConfigCount + 1;
        end;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[21],AProgressUpdateFuntion) then
      begin
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LResult := FFile22DatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[21]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
          if LResult then
             LConfigCount := LConfigCount + 1;
        end;
      end;


      FDataFileObjects.AddNodesWithoutInflow;

      //Parameter file(Param.dat)
      LCurrentFileNames := FileNamesObject.ParamFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
        begin
          LResult := FFileParamDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //AltParameter file(ParamFOR.dat)
      LCurrentFileNames := FileNamesObject.AltParamFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
        begin
          LResult := FFileAltParamDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Demand files(*.abs,*.dem, ...)
      LCurrentFileNames := FileNamesObject.DemandFileNames;
      FDataFileObjects.FDemandFilesObject.Reset;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeDemandFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FDemandDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Hydrology files(*.irr,*.inc, ...)
      LCurrentFileNames := FileNamesObject.HydrologyFileNames;
      FDataFileObjects.FHydrologyFilesObject.Reset;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeHydrologyFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FHydrologyDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Network Visualiser data
      if (LConfigCount >= 12) then;
      {LCurrentFileNames := FileNamesObject.ConfigFileNames;
      if (LConfigCount >= 12) and ((FFileAction = fatExportAll) or (LCurrentFileNames.SelectedCount >= 12)) then
      begin
        LResult := FNetworkVisualiserDatabaseAgent.ReadModelDataFromDatabase(nil,FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;}

      //Reservoir Hydrology Files
      {LCurrentFileNames := FileNamesObject.ParamFileNames;
      if (FFileAction in [fatValidateAll,fatImportAll]) or
         ((FFileAction in [fatValidateSingle,fatImportSingle]) and (TFileNameObject(LCurrentFileNames.FileNameObject[00]).Selected)) then
      begin
        LResult := FReservoirHydrologyFilesDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[0]),
                   FDataFileObjects, AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
      }
    finally
      FFileAction := LFileAction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ExecSaveDataToDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecSaveDataToDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LConfigCount,
  LCount: integer;
  LResult: boolean;
  LImportDate: TDateTime;
begin
  Result := False;
  try
    Result := True;
    // Set study import date to now
    LImportDate := Now();

    //Directory file(Wrym.dat)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).ImportDate := LImportDate;
      LResult := FFilePathsDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Configuration files(F01..F13)
    LConfigCount := 0;
    LCurrentFileNames := FileNamesObject.ConfigFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).ImportDate := LImportDate;
      LResult := FFile01DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[01]).ImportDate := LImportDate;
      LResult := FFile02DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[01]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[02]).ImportDate := LImportDate;
      LResult := FFile03DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[02]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[03]).ImportDate := LImportDate;
      LResult := FFile04DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[03]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[04]).ImportDate := LImportDate;
      LResult := FFile05DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[04]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[05]).ImportDate := LImportDate;
      LResult := FFile06DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[05]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[06],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[06]).ImportDate := LImportDate;
      LResult := FFile07DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[06]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[07],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[07]).ImportDate := LImportDate;
      LResult := FFile08DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[07]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[08],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[08]).ImportDate := LImportDate;
      LResult := FFile09DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[08]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[09],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[09]).ImportDate := LImportDate;
      LResult := FFile10DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[09]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[10],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[10]).ImportDate := LImportDate;
      LResult := FFile11DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[10]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[11],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[11]).ImportDate := LImportDate;
      LResult := FFile12DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[11]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[12],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[12]).ImportDate := LImportDate;
      LResult := FFile13DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[12]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if FileExists(LCurrentFileNames.FileNameObject[13].FileName) and
       ValidateFileName(LCurrentFileNames.FileNameObject[13],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[13]).ImportDate := LImportDate;
      LResult := FFile14DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[13]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[14],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[14]).ImportDate := LImportDate;
        LResult := FFile15DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[14]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[15],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[15]).ImportDate := LImportDate;
      LResult := FFile16DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[15]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      if LResult then
         LConfigCount := LConfigCount + 1;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[16],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[16]).ImportDate := LImportDate;
        LResult := FFile17DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[16]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;     
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[17],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[17]).ImportDate := LImportDate;
        LResult := FFile18DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[17]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[18],AProgressUpdateFuntion) then
    begin
      //if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[18]).ImportDate := LImportDate;
        LResult := FFile19DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[18]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[19],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[19]).ImportDate := LImportDate;
        LResult := FFile20DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[19]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[20],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[20]).ImportDate := LImportDate;
        LResult := FFile21DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[20]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[21],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[21]).ImportDate := LImportDate;
        LResult := FFile22DatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[21]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
        if LResult then
           LConfigCount := LConfigCount + 1;
      end;
    end;

    //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[00]).ImportDate := LImportDate;
        LResult := FFileParamDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //AltParameter file(ParamFOR.dat)
    LCurrentFileNames := FileNamesObject.AltParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[00]).ImportDate := LImportDate;
        LResult := FFileAltParamDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeDemandFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
        LResult := FDemandDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeHydrologyFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;

      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
        LResult := FHydrologyDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Network Visualiser data
    if (LConfigCount >= 12) then;
    {LCurrentFileNames := FileNamesObject.ConfigFileNames;
    if (LConfigCount >= 12) and ((FFileAction = fatImportAll) or (LCurrentFileNames.SelectedCount >= 12)) then
    begin
      LResult := FNetworkVisualiserDatabaseAgent.WriteModelDataToDatabase(nil,FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;}

    //Reservoir Hydrology Files
    {LCurrentFileNames := FileNamesObject.ParamFileNames;
    if (FFileAction in [fatValidateAll,fatImportAll]) or
       ((FFileAction in [fatValidateSingle,fatImportSingle]) and
       (TFileNameObject(LCurrentFileNames.FileNameObject[0]).Selected)) then
    begin
      LResult := FReservoirHydrologyFilesDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(TFileNameObject(LCurrentFileNames.FileNameObject[0])),
                 FDataFileObjects, AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
     }

    if Result then
    begin
      Result := FScenarioDatabaseAgent.SetFilesLoaded(True);
      FAppModules.StudyArea.StudyImportDate := LImportDate;
      FAppModules.StudyArea.LastUpdateDate  := LImportDate;
    end;

    if Result then
       FProgressDialog.KeepStartBtnDisabled := True;

    if Result then
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ExecClearModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecClearModelData';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Directory file(Wrym.dat)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Configuration files(F01..F13)
    LCurrentFileNames := FileNamesObject.ConfigFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFile01DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
    begin
      LResult := FFile02DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[01]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion) then
    begin
      LResult := FFile03DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[02]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion) then
    begin
      LResult := FFile04DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[03]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion) then
    begin
      LResult := FFile05DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[04]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion) then
    begin
      LResult := FFile06DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[05]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[06],AProgressUpdateFuntion) then
    begin
      LResult := FFile07DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[06]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[07],AProgressUpdateFuntion) then
    begin
      LResult := FFile08DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[07]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[08],AProgressUpdateFuntion) then
    begin
      LResult := FFile09DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[08]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[09],AProgressUpdateFuntion) then
    begin
      LResult := FFile10DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[09]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[10],AProgressUpdateFuntion) then
    begin
      LResult := FFile11DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[10]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[11],AProgressUpdateFuntion) then
    begin
      LResult := FFile12DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[11]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    if ValidateFileName(LCurrentFileNames.FileNameObject[12],AProgressUpdateFuntion) then
    begin
      LResult := FFile13DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[12]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //if FileExists(LCurrentFileNames.FileNameObject[13].FileName) and
    if ValidateFileName(LCurrentFileNames.FileNameObject[13],AProgressUpdateFuntion) then
    begin
      LResult := FFile14DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[13]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[14],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile15DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[14]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[15],AProgressUpdateFuntion) then
    begin
      LResult := FFile16DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[15]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[16],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile17DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[16]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;  
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[17],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile18DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[17]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[18],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile19DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[18]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[19],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile20DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[19]),
                   AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[20],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile21DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[20]),
                   AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[21],AProgressUpdateFuntion) then
    begin
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LResult := FFile22DatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[21]),
                   AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;


    //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
      begin
        LResult := FFileParamDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;


    //AltParameter file(ParamFor.dat)
    LCurrentFileNames := FileNamesObject.AltParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      if IncludeHydrologyFile(LCurrentFileNames.FileNameObject[0]) then
      begin
        LResult := FFileAltParamDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    FDataFileObjects.FDemandFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeDemandFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FDemandDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeHydrologyFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Network Visualiser data
    {LCurrentFileNames := FileNamesObject.ConfigFileNames;
    if (FFileAction = fatClearModelData) or (LCurrentFileNames.SelectedCount >= 12) then
    begin
      LResult := FNetworkVisualiserDatabaseAgent.ClearModelDataInDatabase(nil,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;}

    //Reservoir Hydrology Files
    {LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ((FFileAction = fatClearModelData) or (TFileNameObject(LCurrentFileNames.FileNameObject[0]).Selected)) then
    begin
      LResult := FReservoirHydrologyFilesDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[0]),
                 AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
    }

    if Result then
    begin
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;
      Result := FScenarioDatabaseAgent.SetFilesLoaded(False);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.CheckModelFilesAreComplete(AProgressUpdateFuntion:TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.CheckModelFilesAreComplete';
var
  LIndex    : integer;
  LMessage  : string;
  LStop     : boolean;
begin
  Result := False;
  try
    FExtraSteps := 0;
    if (FFileAction = fatExportSingle ) then
    begin
      if (FileNamesObject.ConfigFileNames.SelectedCount > 0) then
      begin
        FileNamesObject.CastConfigFileNames.SetAllSelected(True);
        FileNamesObject.CastParamFileNames.SetAllSelected(True);
        FileNamesObject.CastAltParamFileNames.SetAllSelected(True);
        FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      end;
      if (FileNamesObject.CastParamFileNames.SelectedCount > 0) then
      begin
        FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      end;
      Result := True;
    end;

    if(FFileAction = fatValidateSingle) then
    begin
      if FileNamesObject.ConfigFileNames.FileNameObject[11].Selected then
        FileNamesObject.ConfigFileNames.FileNameObject[02].Selected := True;
      if FileNamesObject.ConfigFileNames.FileNameObject[16].Selected then
        FileNamesObject.ConfigFileNames.FileNameObject[02].Selected := True;
    end;

    if(FFileAction in [fatImportSingle,fatExportSingle]) then
    begin
      if (FileNamesObject.ConfigFileNames.SelectedCount > 0) then
      begin
        FileNamesObject.CastConfigFileNames.SetAllSelected(True);
        FileNamesObject.CastParamFileNames.SetAllSelected(True);
        FileNamesObject.CastAltParamFileNames.SetAllSelected(True);
        FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      end;
      if (FileNamesObject.CastParamFileNames.SelectedCount > 0) then
      begin
        FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      end;
    end;

    if FFileAction in [fatValidateAll,fatImportAll,fatImportSingle,fatRunModel] then
    begin
      if(FAppModules.StudyArea.ModelVersion <> '7' ) and (FFileAction <> fatRunModel) then
      begin
        CheckFileModifiedDate(FileNamesObject.DirectoryFileNames.FileNameObject[0],AProgressUpdateFuntion);
        if not FileNamesObject.DirectoryFileNames.FileNameObject[0].FileFound then
        begin
          LMessage := FAppModules.Language.GetString('TFilesActionYieldManager.strYieldModelFileNoExist');
          LMessage := Format(LMessage,[FileNamesObject.DirectoryFileNames.FileNameObject[0].ShortName]);
          AProgressUpdateFuntion(LMessage,ptError,LStop);
          Exit;
        end;

        CheckFileModifiedDate(FileNamesObject.ParamFileNames.FileNameObject[0],AProgressUpdateFuntion);
        if not FileNamesObject.ParamFileNames.FileNameObject[0].FileFound then
        begin
          LMessage := FAppModules.Language.GetString('TFilesActionYieldManager.strYieldModelFileNoExist');
          LMessage := Format(LMessage,[FileNamesObject.ParamFileNames.FileNameObject[0].ShortName]);
          AProgressUpdateFuntion(LMessage,ptError,LStop);
          Exit;
        end;

        for LIndex := 0 to 12 do
        begin
          CheckFileModifiedDate(FileNamesObject.ConfigFileNames.FileNameObject[LIndex],AProgressUpdateFuntion);
          if (not FileNamesObject.ConfigFileNames.FileNameObject[LIndex].FileFound) then
          begin
            LMessage := FAppModules.Language.GetString('TFilesActionYieldManager.strYieldModelFileNoExist');
            LMessage := Format(LMessage,[FileNamesObject.ConfigFileNames.FileNameObject[LIndex].ShortName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            //Exit;
          end;
        end;
      end;

      if (FFileAction = fatRunModel) then
      begin
        if(FAppModules.StudyArea.ModelVersion <> '7' ) then
        begin
          for LIndex := 0 to FileNamesObject.DemandFileNames.FilesCount -1 do
          begin
            CheckFileModifiedDate(FileNamesObject.DemandFileNames.FileNameObject[LIndex],AProgressUpdateFuntion);
            if not FileNamesObject.DemandFileNames.FileNameObject[LIndex].FileFound then
            begin
              LMessage := FAppModules.Language.GetString('TFilesActionYieldManager.strYieldModelFileNoExist');
              LMessage := Format(LMessage,[FileNamesObject.DemandFileNames.FileNameObject[LIndex].ShortName]);
              AProgressUpdateFuntion(LMessage,ptError,LStop);
              Exit;
            end;
          end;

          for LIndex := 0 to FileNamesObject.HydrologyFileNames.FilesCount -1 do
          begin
            CheckFileModifiedDate(FileNamesObject.HydrologyFileNames.FileNameObject[LIndex],AProgressUpdateFuntion);
            if not FileNamesObject.HydrologyFileNames.FileNameObject[LIndex].FileFound then
            begin
              LMessage := FAppModules.Language.GetString('TFilesActionYieldManager.strYieldModelFileNoExist');
              LMessage := Format(LMessage,[FileNamesObject.HydrologyFileNames.FileNameObject[LIndex].ShortName]);
              AProgressUpdateFuntion(LMessage,ptError,LStop);
              Exit;
            end;
          end;
        end;
      end;
      Result := True;
    end
    else
      Result := True;

    if Result then
    begin
      // This is used to make the track bar show progress when readind/saving network visualiser data
      if (FFileAction = fatClearModelData) then
        FExtraSteps := 0;
      if (FFileAction = fatExportAll) then
        FExtraSteps := 0;
      if (FFileAction = fatValidateAll) then
        FExtraSteps := 0;
      if (FFileAction = fatImportAll) then
        FExtraSteps := 1;
      if (FFileAction = fatImportSingle) then
      begin
        if (TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.ConfigFileNames.SelectedCount >= 12) then
          FExtraSteps := FExtraSteps + 1;
      end;
      if (FFileAction = fatValidateSingle) then
      begin
        if (TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.ConfigFileNames.SelectedCount >= 12) then
          FExtraSteps := FExtraSteps + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ExecRunModel(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecRunModel';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LYieldModel       : IYieldModel;
  LIterationTracker : IYieldModelIterationTracker;
begin
  Result := False;
  try
    Result := True;
    LYieldModel := (FAppModules.Model as IYieldModel);
    LYieldModel.WRYMRunOptions.FirmYield := NullFloat;
    LYieldModel.WRYMRunOptions.SumOutBlobAddress := 0;
    LYieldModel.WRYMRunOptions.BlobSize := 0;
    FOldProgressMsg := '';

    LIterationTracker := (FAppModules.Model as IYieldModel).YieldModelIterationTracker;
    LIterationTracker.IterationEventHandler := Self;
    try
      //if(FAppModules.StudyArea.ModelVersion <> '7' ) then
      //begin
        if not CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then Exit;
        LCurrentFileNames := FileNamesObject.DirectoryFileNames;
        Result := ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion);
        Result := Result and  FFilePathsAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                       FDataFileObjects,AProgressUpdateFuntion);
        if not UpdateProgress(Result) then Exit;

        Result := Result and FRunModelAgent.CheckInputFiles(FDataFileObjects,AProgressUpdateFuntion);
        if not UpdateProgress(Result) then Exit;
      //end;

      if Result then
      begin
        //if(FAppModules.User.UserType <> utExpert) then
          FileNamesObject.OutputFileNames.DeleteAllFiles;

        FProgressDialog.FStopButton.Enabled := False;
        Result := Result and FRunModelAgent.RunModel(FDataFileObjects,AProgressUpdateFuntion,FProgressDialog.Handle);
        FProgressDialog.FStopButton.Enabled := True;
        if not UpdateProgress(Result) then Exit;

        Result := Result and FRunModelAgent.CheckOutputFiles(FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and ExecReadYRCDataFromFile(AProgressUpdateFuntion);
      end;
    finally
      LIterationTracker.IterationEventHandler := nil;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.GetFileNamesObjectCast: TModelFileNames;
const OPNAME = 'TFilesActionYieldManager.GetFileNamesObjectCast';
begin
  Result := nil;
  try
    Result := TModelFileNames(ModelData.FileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.CheckFileImportDate(AFilename : TAbstractModelFileName) : boolean;
const OPNAME = 'TFilesActionYieldManager.CheckFileImportDate';
begin
  Result := False;
  try
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.AdvanceProgressBar(ASteps: integer): boolean;
const OPNAME = 'TFilesActionYieldManager.AdvanceProgressBar';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := 1 to ASteps do
      UpdateProgress(True);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.IncludeDemandFile(AFileName: TAbstractModelFileName): boolean;
const OPNAME = 'TFilesActionYieldManager.IncludeDemandFile';
begin
  Result := False;
  try
    if Assigned(AFileName) and (Trim(AFileName.FileName) <> '')then
    begin
      case FFileAction of
        fatValidateAll,fatClearModelData, fatImportAll, fatExportAll,fatRunModel:
          begin
            Result := FAppModules.GlobalData.IncludeDemandFiles;
            if not Result then
               UpdateProgress(True);
          end;

        fatValidateSingle, fatExportSingle, fatImportSingle:
          begin
            Result := AFileName.Selected;
          end;

        fatImportSumOut, fatSaveOutputFiles:
          begin
            Result := AFileName.Selected;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.IncludeHydrologyFile(AFileName: TAbstractModelFileName): boolean;
const OPNAME = 'TFilesActionYieldManager.IncludeHydrologyFile';
begin
  Result := False;
  try
    if Assigned(AFileName) and (Trim(AFileName.FileName) <> '')then
    begin
      case FFileAction of
        fatValidateAll,fatClearModelData, fatImportAll, fatExportAll,fatRunModel:
          begin
            Result := FAppModules.GlobalData.IncludeHydrologyFiles;
            if not Result then
               UpdateProgress(True);
          end;

        fatValidateSingle, fatExportSingle, fatImportSingle:
          begin
            Result := AFileName.Selected;
          end;

        fatImportSumOut, fatSaveOutputFiles:
          begin
            Result := AFileName.Selected;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.GetYieldChannelYield(var AValue: double): boolean;
const OPNAME = 'TFilesActionYieldManager.GetYieldChannelYield';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LFileNameObject: TFileNameObject;
  LCount: integer;
begin
  Result := False;
  AValue := 0.00;
  try

    //Get file name
    LFileNameObject := nil;
    LCurrentFileNames := FileNamesObject.OutputFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if (GetOutputFileType(LCurrentFileNames.FileNameObject[LCount].FileName) = oftSum) then
      begin
        LFileNameObject:= TFileNameObject(LCurrentFileNames.FileNameObject[LCount]);
        Break;
      end;
    end;

    // Read the sum.out file
    if assigned(LFileNameObject) then
      Result := FSumOutFileManager.GetYieldChannelYield(AValue,LFileNameObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ExecValidateModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecValidateModelData';

  function ErrorSeverity(var AErrorMsg: string): integer;
  const OPNAME = 'ErrorSeverity';
  begin
    Result := 0;
    try
      if(Pos('WARNING:',AErrorMsg) = 1) then
      begin
        AErrorMsg := Copy(AErrorMsg,9,Length(AErrorMsg));
        Result    := 1;
      end
      else
      if(Pos('ERROR:',AErrorMsg) = 1) then
      begin
        AErrorMsg := Copy(AErrorMsg,7,Length(AErrorMsg));
        Result    := 2;
      end;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

var
  LColumns,
  LValidationErrors: TStringList;
  LErrors: WideString;
  LCount: integer;
  LStop: boolean;
  LErrorMsg: string;
begin
  Result := False;
  try
    if not Assigned(AProgressUpdateFuntion) then
      AProgressUpdateFuntion := DummyShowProgress;
    LColumns := TStringList.Create;
    LValidationErrors := TStringList.Create;
    try
      AProgressUpdateFuntion('Started validating model data',ptNone,LStop);
      Result := TYieldModelDataObject(FAppModules.Model.ModelData).Validate(LErrors,'');
      while (LErrors <> '') do
      begin
        LErrors := CutErrorsAndColumns(LErrors,LValidationErrors,LColumns);
        for LCount := 0 to LValidationErrors.Count -1 do
        begin
          LErrorMsg := LValidationErrors[LCount];
          if(ErrorSeverity(LErrorMsg)= 1) then
            AProgressUpdateFuntion(LErrorMsg,ptWarning,LStop)
          else
            AProgressUpdateFuntion(LErrorMsg,ptError,LStop);
        end;
      end;
      AProgressUpdateFuntion('Completed validating model data',ptNone,LStop);
    finally
      LColumns.Free;
      LValidationErrors.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.DoRunModel: boolean;
const OPNAME = 'TFilesActionYieldManager.DoRunModel';
var
  LExportFirst : boolean;
  LResult      : word;
  {LImportDate  : TDateTime;
  LUpdateDate  : TDateTime;
  LExportFiles,
  LIndex       : integer;
  LFileObject:TAbstractModelFileName;}
begin
  Result := False;
  try
    {LExportFirst := False;

    if(FAppModules.StudyArea.ModelVersion = '7' ) then
    else
    if(FAppModules.User.UserType = utExpert) then
    begin
      LImportDate  :=  FAppModules.StudyArea.GetStudyImportDate;
      LUpdateDate  :=  FAppModules.StudyArea.GetLastUpdateDate;
      if (LUpdateDate > LImportDate) then
      begin
        LExportFiles := False;
        for LIndex := 0 to 12 do
        begin
          LFileObject := FileNamesObject.CastConfigFileNames.FileNameObject[LIndex];
          if(LFileObject = nil) then
            LExportFiles := True
          else
          begin
            if not LFileObject.FileFound then
              LExportFiles := True
            else
            begin
              if(LFileObject.FileDate < lUpdateDate) then
                LExportFiles := True;
            end;
          end;
          if LExportFiles then Break;
        end;

        if LExportFiles  then
          if(MessageDlg('Files in the harddrive are different from files in the database, do you want to export them first?',mtConfirmation,[mbYes,mbNo],0)=mrYes) then
             LExportFirst := True;
      end;}
    //end;
    LResult := MessageDlg('Files in the harddrive will be used for the analysis. Would you like to update these files with data in the database?',mtConfirmation,[mbYes,mbNo,mbCancel],0);
    if(LResult = mrCancel) then Exit;

    LExportFirst := (LResult = mrYes);
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FGlobalDataIndex.Reset;
      FExtraSteps     := 0;
      FFileAction     := fatRunModel;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Clear;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      SetTotalFiles(3 + FExtraSteps);
      //if(FAppModules.User.UserType = utExpert) then
      //begin
        FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
        if LExportFirst then
        begin
          FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
          FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
          FProgressDialog.clbOptions.Items.Add('Include Hydrology Files');
          FProgressDialog.clbOptions.Items.Add('Include Demand Files');
          FProgressDialog.clbOptions.Checked[1] :=  FAppModules.GlobalData.IncludeHydrologyFiles;
          FProgressDialog.clbOptions.Checked[2] :=  FAppModules.GlobalData.IncludeDemandFiles;
          FGlobalDataIndex.IncludeHydrologyFilesIndex        := 1;
          FGlobalDataIndex.IncludeDemandFilesIndex           := 2;
        end;
        FProgressDialog.AddExecutionFunctions(ExecRunModel);
      //end
      {else
      begin
        FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
        FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
        FProgressDialog.AddExecutionFunctions(ExecRunModel);
        FProgressDialog.clbOptions.Items.Add('Include Hydrology Files');
        FProgressDialog.clbOptions.Items.Add('Include Demand Files');
        FProgressDialog.clbOptions.Checked[1] :=  FAppModules.GlobalData.IncludeHydrologyFiles;
        FProgressDialog.clbOptions.Checked[2] :=  FAppModules.GlobalData.IncludeDemandFiles;
        FGlobalDataIndex.IncludeHydrologyFilesIndex        := 1;
        FGlobalDataIndex.IncludeDemandFilesIndex           := 2;
      end;}
      AddExtraButtons;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strRunModel');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      //Result := FProgressDialog.Succsessful;
      Result := True;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ImportDemandFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TFilesActionYieldManager.ImportDemandFile';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileNameObject :=
    TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
     (LFileNameObject.FileFound) and
     (not LFileNameObject.SavedInDB)  then
    begin
      CreateDataFileObject;
      try
        FProgressDialog.Initialise;
        Result := FDemandFileAgent.ReadModelDataFromFile(LFileNameObject,FDataFileObjects,FProgressDialog.ShowProgress);
        Result := Result and FDemandDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDataFileObjects,nil);
        if (FProgressDialog.ErrorCount > 0)  and Assigned(FAppModules.MainForm()) then
        begin
            AddExtraButtons;
            FProgressDialog.FStartButton.Visible := False;
            FProgressDialog.FStopButton.Visible := False;
            FProgressDialog.ActionProgressBar.Visible := False;
            FProgressDialog.ShowModal;

            FProgressDialog.FStartButton.Visible := True;
            FProgressDialog.FStopButton.Visible := True;
            FProgressDialog.ActionProgressBar.Visible := True;
        end;

      finally
        DestroyDataFileObject;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ImportHydrologyFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TFilesActionYieldManager.ImportHydrologyFile';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileNameObject :=
    TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
     (LFileNameObject.FileFound) and
     (not LFileNameObject.SavedInDB)  then
    begin
      CreateDataFileObject;
      try
        Result := FHydrologyFileAgent.ReadModelDataFromFile(LFileNameObject,FDataFileObjects,nil);
        Result := Result and FHydrologyDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDataFileObjects,nil);
      finally
        DestroyDataFileObject;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ImportParamFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TFilesActionYieldManager.ImportParamFile';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileNameObject :=  TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.ParamFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
     (LFileNameObject.FileFound) and
     (not LFileNameObject.SavedInDB)  then
    begin
      CreateDataFileObject;
      try
        Result := FFileParamAgent.ReadModelDataFromFile(LFileNameObject,FDataFileObjects,nil);
        Result := Result and FFileParamDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDataFileObjects,nil);
        if Result then
        begin
          LFileNameObject :=  TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AltParamFileNames.FileNameObject[0]);
          if(LFileNameObject <> nil) and
           (LFileNameObject.FileFound) and
           (not LFileNameObject.SavedInDB)  then
          begin
              Result := FFileAltParamAgent.ReadModelDataFromFile(LFileNameObject,FDataFileObjects,nil);
              Result := Result and FFileAltParamDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDataFileObjects,nil);
          end;
        end;
      finally
        DestroyDataFileObject;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ClearParamFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TFilesActionYieldManager.ClearParamFile';
var
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileNameObject := TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.ParamFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and LFileNameObject.SavedInDB  then
    begin
      Result := FFileParamDatabaseAgent.ClearModelDataInDatabase(LFileNameObject,nil,True);
      if Result then
      begin
        LFileNameObject :=  TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AltParamFileNames.FileNameObject[0]);
        if(LFileNameObject <> nil) and LFileNameObject.SavedInDB  then
          Result := FFileAltParamDatabaseAgent.ClearModelDataInDatabase(LFileNameObject,nil,True);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ImportPathsFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TFilesActionYieldManager.ImportPathsFile';
var
  LF01FileNameObject: TFileNameObject;
  LFileNameObject: TFileNameObject;
begin
  Result := False;
  try
    LFileNameObject :=  TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DirectoryFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
     (LFileNameObject.FileFound) and
     (not LFileNameObject.SavedInDB)  then
    begin
      CreateDataFileObject;
      try
        Result := FFilePathsAgent.ReadModelDataFromFile(LFileNameObject,FDataFileObjects,nil);

        LF01FileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastConfigFileNames.CastFileObject[0];
        if(LF01FileNameObject <> nil) then
          FFile01Agent.ReadModelDataFromFile(LF01FileNameObject,FDataFileObjects,nil);

        Result := Result and FFilePathsDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDataFileObjects,nil);
      finally
        DestroyDataFileObject;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ImportDamWaterLevelsFile(const AFileName: WideString): boolean;
const OPNAME = 'TFilesActionYieldManager.ImportDamWaterLevelsFile';
var
  LFileNameObject: TFileNameObject;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DamLevelsFileNames.Count + 1;
    TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddDamLevelsFileName(LNewFileIndex,AFileNAme,False,Now,Now);
    LFileNameObject :=
    TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DamLevelsFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
      (LFileNameObject.FileFound) and
      (not LFileNameObject.SavedInDB)  then
    begin
      CreateDataFileObject;
      try
        FProgressDialog.Initialise;
        Result := FMonthlyDamLevelsFileAgent.ReadModelDataFromFile(LFileNameObject,FDataFileObjects,FProgressDialog.ShowProgress);
        Result := Result and FMonthlyDamLevelsDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDataFileObjects,nil);

        if (FProgressDialog.ErrorCount > 0)  and Assigned(FAppModules.MainForm()) then
        begin
          AddExtraButtons;
          FProgressDialog.FStartButton.Visible := False;
          FProgressDialog.FStopButton.Visible := False;
          FProgressDialog.ActionProgressBar.Visible := False;
          FProgressDialog.ShowModal;

          FProgressDialog.FStartButton.Visible := True;
          FProgressDialog.FStopButton.Visible := True;
          FProgressDialog.ActionProgressBar.Visible := True;
        end;
      finally
        DestroyDataFileObject;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionYieldManager.AddExtraButtons;
const OPNAME = 'TFilesActionYieldManager.AddExtraButtons';
//var
//  LButton: TButton;
begin
  inherited;
  try
   {if (FFileAction = fatRunModel) then
   begin
     LButton := FProgressDialog.AddButton(AppModules.Language.GetString('TFilesActionAbstractManager.strRunOptions'));
     LButton.OnClick := OnViewRunOptionsDialog;
   end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionYieldManager.OnViewRunOptionsDialog(Sender: TObject);
const OPNAME = 'TFilesActionYieldManager.OnViewRunOptionsDialog';
var
  LForm : TRunOptionsDialog;
  LOptions:   IWRYMRunOptions;
begin
  try
    LForm := TRunOptionsDialog.Create(nil,FAppModules);
    try
      LForm.ChklBoxRunOptions.Items.Clear;
      LForm.ChklBoxRunOptions.Items.Add('Run Silent');
      LForm.ChklBoxRunOptions.Items.Add('Auto Run');
      LForm.ChklBoxRunOptions.Items.Add('Close On Completion');
      LForm.ChklBoxRunOptions.Items.Add('Run Debug Version');
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LForm.ChklBoxRunOptions.Items.Add('Create SUM.OUT file on the harddrive');
        LForm.ChklBoxRunOptions.Items.Add('Save output as a binary file on the harddrive');
        LForm.ChklBoxRunOptions.Items.Add('Save binary output to the database');
      end;
      LOptions := (FAppModules.Model as IYieldModel).WRYMRunOptions;
      LForm.ChklBoxRunOptions.Checked[0] :=  LOptions.RunSilent;
      LForm.ChklBoxRunOptions.Checked[1] :=  LOptions.AutoRun;
      LForm.ChklBoxRunOptions.Checked[2] :=  LOptions.CloseOnComplete;
      LForm.ChklBoxRunOptions.Checked[3] :=  LOptions.RunDebugVersion;
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LForm.ChklBoxRunOptions.Checked[4] :=  LOptions.CreateSumOutFile;
        LForm.ChklBoxRunOptions.Checked[5] :=  LOptions.SaveOutputAsBinaryFile;
        LForm.ChklBoxRunOptions.Checked[6] :=  LOptions.SaveOutputToDB;
      end;
      LForm.ShowModal;
      if(LForm.ModalResult = mrOk) then
      begin
        LOptions.RunSilent       := LForm.ChklBoxRunOptions.Checked[0];
        LOptions.AutoRun         := LForm.ChklBoxRunOptions.Checked[1];
        LOptions.CloseOnComplete := LForm.ChklBoxRunOptions.Checked[2];
        LOptions.RunDebugVersion := LForm.ChklBoxRunOptions.Checked[3];
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LOptions.CreateSumOutFile       := LForm.ChklBoxRunOptions.Checked[4];
          LOptions.SaveOutputAsBinaryFile := LForm.ChklBoxRunOptions.Checked[5];
          LOptions.SaveOutputToDB         := LForm.ChklBoxRunOptions.Checked[6];
        end;
        LOptions.SaveToINI;
      end;
    finally
      LForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ValidateFileName(AFileName: TAbstractModelFileName; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ValidateFileName';
var
  LYieldModelData : TYieldModelDataObject;
begin
  Result := inherited ValidateFileName(AFileName, AProgressUpdateFuntion);
  try
    if (not Result)  and (FFileAction in [fatExportAll, fatExportSingle, fatClearModelData]) then
    begin
      if (TFileNameObject(AFileName).FileGroup  = 1) then
      begin
        LYieldModelData := TYieldModelDataObject(FAppModules.Model.ModelData);
        Result := (TFileNameObject(AFileName).FileNumber = 1);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 2)  and (LYieldModelData.NetworkElementData.ReservoirList.ReservoirAndNodesCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 3)  and (LYieldModelData.NetworkElementData.ChannelList.ChannelCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 4)  and (LYieldModelData.NetworkElementData.ChannelList.ChannelCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 5)  and (LYieldModelData.NetworkElementData.ReservoirList.ReservoirAndNodesCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 6)  and (LYieldModelData.NetworkElementData.ReservoirList.ReservoirAndNodesCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 7)  and (LYieldModelData.NetworkFeaturesData.PowerPlantList.PowerPlantCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 8)  and (LYieldModelData.NetworkFeaturesData.PowerPlantList.PowerPlantCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 9)  and (LYieldModelData.NetworkFeaturesData.IrrigationAreaList.IrrigationAreaCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 10)  and (LYieldModelData.NetworkFeaturesData.DiversionFeatureList.DiversionFeatureCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 11)  and (LYieldModelData.NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 12)  and (LYieldModelData.NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 13)  and (LYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 14)  and ((LYieldModelData.NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureCount > 0) or (LYieldModelData.NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureCount > 0));
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 15)  and (LYieldModelData.NetworkFeaturesData.CurtailmentAndDrought.CurtailmentPeriodCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 16)  and (LYieldModelData.NetworkFeaturesData.WaterDemandFeatureList.WaterDemandFeatureCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 17)  and (LYieldModelData.NetworkFeaturesData.IrrigationBlockList.IrrigationBlockCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 18)  and (LYieldModelData.NetworkFeaturesData.WetlandList.WetLandCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 19)  and (LYieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 20)  and (LYieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 21)  and (LYieldModelData.NetworkFeaturesData.MineList.MineCount > 0);
        if Result then Exit;
        Result := (TFileNameObject(AFileName).FileNumber = 22)  and (LYieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterCount > 0);
        if Result then Exit;
      end;
    end;

    if (TFileNameObject(AFileName).FileGroup  = 1) and (TFileNameObject(AFileName).FileNumber = 22) then
    begin
      Result := Result and TYieldModelDataObject(FAppModules.Model.ModelData).ImplementedNetworkFeatures.GroundWaterFeatureImplemented;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.DoRunStorageVsYield(AReservoirNumber: integer; const AStartingStorageCommaText: WideString;
         var AMinTargetDraftCommaText, AMaxTargetDraftCommaText,AYieldCommaText: WideString): WordBool;
const OPNAME = 'TFilesActionYieldManager.DoRunStorageVsYield';
var
  LYieldModel : IYieldModel;
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FStorageVsYieldData.Initialise;
      FStorageVsYieldData.ReservoirNumber          := AReservoirNumber;
      FStorageVsYieldData.StartingStorageCommaText := AStartingStorageCommaText;
      FStorageVsYieldData.MinTargetDraftCommaText  := AMinTargetDraftCommaText;
      FStorageVsYieldData.MaxTargetDraftCommaText  := AMaxTargetDraftCommaText;
      FStorageVsYieldData.YieldCommaText           := AYieldCommaText;

      LYieldModel := (FAppModules.Model as IYieldModel);
      LYieldModel.WRYMRunOptions.AutoRun          := True;
      LYieldModel.WRYMRunOptions.CloseOnComplete  := True;
      LYieldModel.WRYMRunOptions.RunDebugVersion  := False;
      LYieldModel.WRYMRunOptions.RunSilent        := True;
      LYieldModel.WRYMRunOptions.CreateSumOutFile := False;
      LYieldModel.WRYMRunOptions.SaveOutputAsBinaryFile := False;
      LYieldModel.WRYMRunOptions.SaveOutputToDB         := False;

      FDataFileObjects.Initialise;
      FGlobalDataIndex.Reset;
      FExtraSteps     := 0;
      FFileAction     := fatRunModel;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Clear;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      SetTotalFiles(FStorageVsYieldData.FStartingStorage.Count + FExtraSteps);

      FProgressDialog.AddExecutionFunctions(ExecRunStorageVsYield);
      AddExtraButtons;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strRunModel');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      if Result then
      begin
        AMinTargetDraftCommaText  := FStorageVsYieldData.MinTargetDraftCommaText;
        AMaxTargetDraftCommaText  := FStorageVsYieldData.MaxTargetDraftCommaText;
        AYieldCommaText           := FStorageVsYieldData.YieldCommaText;
      end;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ExecRunStorageVsYield(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionYieldManager.ExecRunStorageVsYield';
var
  LIndex          : integer;
  LYieldModelData : TYieldModelDataObject;
  LReservoir      : IReservoirData;
  LMessage        : string;
  LStop           : boolean;
  LStartingStorage: double;
  LMinTD          : double;
  LMaxTD          : double;
  LFirmYield      : double;
  LIterationTracker : IYieldModelIterationTracker;
  LYieldModel       : IYieldModel;
begin
  Result := False;
  try
    LStop := False;
    LIterationTracker := (FAppModules.Model as IYieldModel).YieldModelIterationTracker;
    LIterationTracker.IterationEventHandler := Self;

    FAppModules.User.SetUserType(utExpert);
    LYieldModel := (FAppModules.Model as IYieldModel);

    LYieldModelData := TYieldModelDataObject(FAppModules.Model.ModelData);
    LYieldModelData.RunConfigurationData.CalculateHistoricFirmYield := 1;

    LReservoir := LYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FStorageVsYieldData.ReservoirNumber];
    if (LReservoir = nil) then
    begin
     LMessage := 'Resevoir number ('+IntToStr(FStorageVsYieldData.ReservoirNumber)+') does not exists. Run stopped.';
     AProgressUpdateFuntion(LMessage,ptError,LStop);
     Exit;
    end;

    LMessage := 'Run started. Total number of runs is '+ IntToStr(FStorageVsYieldData.FStartingStorage.Count);
    AProgressUpdateFuntion(LMessage,ptWarning,LStop);

    {FProgressDialog.AddExecutionFunctions();
    FProgressDialog.AddExecutionFunctions();
    FProgressDialog.AddExecutionFunctions();
    FProgressDialog.AddExecutionFunctions();}

    if (FAppModules.StudyArea.ModelVersion <> '7') then
    begin
      FAppModules.GlobalData.SetIncludeHydrologyFiles(True);
      FAppModules.GlobalData.SetIncludeDemandFiles(True);
    end;

    for LIndex := 0 to FStorageVsYieldData.FStartingStorage.Count-1 do
    begin
      LMessage := 'Run number ('+IntToStr(LIndex+1)+') out of '+(IntToStr(FStorageVsYieldData.FStartingStorage.Count))+' started.';
      AProgressUpdateFuntion(LMessage,ptWarning,LStop,True);

      LStartingStorage := StrToFloat(FStorageVsYieldData.FStartingStorage[LIndex]);
      LReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation := LStartingStorage;
      LReservoir.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1] := LStartingStorage;

      LMinTD := StrToFloat(FStorageVsYieldData.FMinTargetDraft[LIndex]);
      LMaxTD := StrToFloat(FStorageVsYieldData.FMaxTargetDraft[LIndex]);

      LYieldModelData.RunConfigurationData.TargetYieldByIndex[1]  := LMaxTD;
      LYieldModelData.RunConfigurationData.MaximumYieldByIndex[1] := LMaxTD;
      LYieldModelData.RunConfigurationData.TargetYieldByIndex[2]  := LMinTD;
      LYieldModelData.RunConfigurationData.MaximumYieldByIndex[2] := LMinTD;

      if (FAppModules.StudyArea.ModelVersion <> '7') then
      begin

        if (not ExecValidateModelData(AProgressUpdateFuntion)) and FAppModules.GlobalData.StopOnFirstErr  then Break;
        if LStop then Break;
        if (not ExecLoadDataFromDatabase(AProgressUpdateFuntion)) and FAppModules.GlobalData.StopOnFirstErr then Break;
        if LStop then Break;
        if (not ExecSaveDataToFiles(AProgressUpdateFuntion)) and FAppModules.GlobalData.StopOnFirstErr then Break;
      end;

      if LStop then Break;
      if not ExecRunModel(AProgressUpdateFuntion) then Break;
      if LStop then Break;

      if (FAppModules.StudyArea.ModelVersion = '7') then
        LFirmYield := LYieldModel.WRYMRunOptions.FirmYield
      else
        LFirmYield := ReadFirmYieldFromDebugFile;

      If (LFirmYield < 0.0) then
      begin
        LMessage := 'Firm yield could not be determined within the range. Run stopped ';
        AProgressUpdateFuntion(LMessage,ptError,LStop);
        Break;
      end
      else
      begin
        FStorageVsYieldData.FYield.Add(FloatToStr(LFirmYield));
        if(LFirmYield < LMinTD) then
        begin
         FStorageVsYieldData.FMinTargetDraft.Add(FloatToStr(LFirmYield));
         FStorageVsYieldData.FMaxTargetDraft.Add(FloatToStr(LMinTD));
        end
        else
        if(LFirmYield < LMaxTD) then
        begin
         FStorageVsYieldData.FMinTargetDraft.Add(FloatToStr(LFirmYield));
         FStorageVsYieldData.FMaxTargetDraft.Add(FloatToStr(LMaxTD));
        end
        else
        begin
         FStorageVsYieldData.FMinTargetDraft.Add(FloatToStr(LMaxTD));
         FStorageVsYieldData.FMaxTargetDraft.Add(FloatToStr(LFirmYield));
        end
      end;
      Result := (LIndex = (FStorageVsYieldData.FStartingStorage.Count-1));
      if (FAppModules.StudyArea.ModelVersion <> '7') then
      begin
        FAppModules.GlobalData.SetIncludeHydrologyFiles(False);
        FAppModules.GlobalData.SetIncludeDemandFiles(False);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.OnIterationEvent(const AIterationName: WideString): WordBool;
const OPNAME = 'TFilesActionYieldManager.OnIterationEvent';
var
  LMessage : string;
  LStop    : boolean;
  LIterationTracker : IYieldModelIterationTracker;
begin
  Result := False;
  try
    if not (FAppModules.Model as IYieldModel).WRYMRunOptions.RunSilent then
    begin
      Result := True;
    end
    else
    begin
      LStop := False;
      LIterationTracker := (FAppModules.Model as IYieldModel).YieldModelIterationTracker;
      if(AIterationName = 'ATimePeriodEnd') then
      begin
        LMessage := AIterationName + ' : ' +
        ' Curent Sequence = '     + IntToStr(LIterationTracker.CurrentSequence)      + ' Total Sequences = '   + IntToStr(LIterationTracker.SequenceCount)+
        ' Curent TargetDraft  = ' + IntToStr(LIterationTracker.CurrentTargetDraft)   + ' Total TargetDrafts = '+ IntToStr(LIterationTracker.TargetDraftCount)+
        ' Curent Year  = '        + IntToStr(LIterationTracker.CurrentYearGregorian) + ' Total Years= '        + IntToStr(LIterationTracker.YearCount)+
        ' Curent Month  = '       + IntToStr(LIterationTracker.CurrentMonth)         + ' Total Months = '      + IntToStr(LIterationTracker.MonthCount)+
        ' Curent Interval  = '    + IntToStr(LIterationTracker.CurrentInterval)      + ' Total Intervals = '   + IntToStr(LIterationTracker.IntervalCount);
        FProgressDialog.ShowProgressInPlace(FOldProgressMsg,LMessage,ptNone,LStop);
        FOldProgressMsg := LMessage;
      end;
      LIterationTracker.Abort := LStop;
      Result := not LStop;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ReadFirmYieldFromDebugFile: Double;
const OPNAME = 'TFilesActionYieldManager.ReadFirmYieldFromDebugFile';
var
  LIndex         : integer;
  LFirmYieldLine : string;
  LDebugFileName,
  LFileName      : TAbstractModelFileName;
  LFileContents  : TStringlist;
begin
  Result := NullFloat;
  try
    LDebugFileName := nil;
    for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastOutputFileNames.Count-1 do
    begin
      LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastOutputFileNames.FileNameObject[LIndex];
      if(Pos('DBG.OUT',UpperCase(LFileName.FileName)) > 0) then
      begin
        LDebugFileName := LFileName;
        Break;
      end;
    end;
    if(LDebugFileName <> nil) and FileExists(LDebugFileName.FileName) then
    begin
      LFileContents := TStringlist.Create;
      try
        LFileContents.LoadFromFile(LDebugFileName.FileName);
        LFirmYieldLine := '';
        for LIndex := 0 to LFileContents.Count-1 do
        begin
          if(Pos('PREVIOUS YIELD WITHOUT FAILURES',LFileContents[LIndex]) > 0) then
            LFirmYieldLine := LFileContents[LIndex];
        end;

        if(LFirmYieldLine <> '') then
        begin
          LIndex := Pos('=',LFirmYieldLine);
          if(LIndex <> 0) then
          begin
            LFirmYieldLine := Copy(LFirmYieldLine,LIndex+1,Length(LFirmYieldLine));
            LFirmYieldLine := Trim(LFirmYieldLine);
            Result := StrToFloatDef(LFirmYieldLine,NullFloat);
          end;
        end;
      finally
        LFileContents.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionYieldManager.ImportAllHydrologyFiles: boolean;
const OPNAME = 'TFilesActionYieldManager.ImportAllHydrologyFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LImportDate: TDateTime;
begin
  Result := True;
  try
    CreateDataFileObject;
    try
      // Set study import date to now
      LImportDate := Now();

      //Demand files(*.abs,*.dem, ...)
      LCurrentFileNames := FileNamesObject.DemandFileNames;
      FDataFileObjects.FDemandFilesObject.Reset;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        LResult := FDemandFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,nil);
        if not LResult then Result := False;
      end;


      //Hydrology files(*.irr,*.inc, ...)
      LCurrentFileNames := FileNamesObject.HydrologyFileNames;
      FDataFileObjects.FHydrologyFilesObject.Reset;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        LResult := FHydrologyFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,nil);
        if not LResult then Result := False;
      end;

      //Demand files(*.abs,*.dem, ...)
      LCurrentFileNames := FileNamesObject.DemandFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
          TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
          LResult := FDemandDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                     FDataFileObjects,nil);
        if not LResult then Result := False;
      end;

      //Hydrology files(*.irr,*.inc, ...)
      LCurrentFileNames := FileNamesObject.HydrologyFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
        LResult := FHydrologyDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,nil);
        if not LResult then Result := False;
      end;
      Result := True;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStorageVsYieldData }

procedure TStorageVsYieldData.CreateMemberObjects;
const OPNAME = 'TStorageVsYieldData.CreateMemberObjects';
begin
  inherited;
  try
    FReservoirNumber := NullInteger;
    FStartingStorage := TStringList.Create;
    FMinTargetDraft  := TStringList.Create;
    FMaxTargetDraft  := TStringList.Create;
    FYield           := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStorageVsYieldData.DestroyMemberObjects;
const OPNAME = 'TStorageVsYieldData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FStartingStorage);
    FreeAndNil(FMinTargetDraft);
    FreeAndNil(FMaxTargetDraft);
    FreeAndNil(FYield);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStorageVsYieldData.Initialise: boolean; 
const OPNAME = 'TStorageVsYieldData.Initialise';
begin
  Result := inherited Initialise;
  try
    FReservoirNumber := NullInteger;
    FStartingStorage.Clear;
    FMinTargetDraft.Clear;
    FMaxTargetDraft.Clear;
    FYield.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStorageVsYieldData.GetMaxTargetDraftCommaText: string;
const OPNAME = 'TStorageVsYieldData.GetMaxTargetDraftCommaText';
begin
  Result := '';
  try
    Result := FMaxTargetDraft.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStorageVsYieldData.GetMinTargetDraftCommaText: string;
const OPNAME = 'TStorageVsYieldData.GetMinTargetDraftCommaText';
begin
  Result := '';
  try
    Result := FMinTargetDraft.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStorageVsYieldData.GetStartingStorageCommaText: string;
const OPNAME = 'TStorageVsYieldData.GetStartingStorageCommaText';
begin
  Result := '';
  try
    Result := FStartingStorage.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStorageVsYieldData.GetYieldCommaText: string;
const OPNAME = 'TStorageVsYieldData.GetYieldCommaText';
begin
  Result := '';
  try
    Result := FYield.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStorageVsYieldData.SetMaxTargetDraftCommaText(ACommatextValue: string);
const OPNAME = 'TStorageVsYieldData.SetMaxTargetDraftCommaText';
begin
  try
    FMaxTargetDraft.CommaText  := ACommatextValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStorageVsYieldData.SetMinTargetDraftCommaText(ACommatextValue: string);
const OPNAME = 'TStorageVsYieldData.SetMinTargetDraftCommaText';
begin
  try
    FMinTargetDraft.CommaText  := ACommatextValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStorageVsYieldData.SetStartingStorageCommaText(ACommatextValue: string);
const OPNAME = 'TStorageVsYieldData.SetStartingStorageCommaText';
begin
  try
    FStartingStorage.CommaText  := ACommatextValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStorageVsYieldData.SetYieldCommaText(ACommatextValue: string);
const OPNAME = 'TStorageVsYieldData.SetYieldCommaText';
begin
  try
    FYield.CommaText  := ACommatextValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

