//
//
//  UNIT      : Contains TModelYRCManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/12/2001
//  COPYRIGHT : Copyright © 2001 DWAF
//
//

unit UModelYRCManager;

interface

uses
  // Delphi
  Classes,

  // DWAF
  USystemModelManager,
  UMenuItemManager,
  UFileNames,
  UViewDataItem,
  UGridFieldData,
  UAbstractObject,
  UTabSheetManager,
  UGridActionObject,
  UDataViewerManager,
  UYieldModelDataObject,
  UFileselectionManager,
  UGenericModelLinkClasses,
  UFilesActionAbstractManager;

type
  TModelYRCManager = class(TSystemModelManager)
  protected
    FModelData: TInterfacedObject;
    FModelMenuItemManager: TMenuItemManager;
    FFileSelectionManager: TFileselectionManager;
    FModelFilesActionManager: TFilesActionAbstractManager;

    FFileEditManager: TAbstractFileEditManager;
    FYieldReliabilityCurveManager: TTabSheetManager;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function YieldModelData: TYieldModelDataObject;

    procedure CreateModelData; virtual;
    procedure CreateFileActionManager; virtual;
    procedure CreateFileSelectionManager; virtual;
    procedure CreateModelDatasetConstructor; virtual;
    procedure DeleteModelDatasetConstructor; virtual;

    procedure CreateFileEditManager; virtual;
    procedure CreateYRCTabSheetManager; virtual;

    procedure CreateModelTabSheetManagers; virtual;
    procedure CreateModelMenuItemManager; virtual;

    procedure SetSystemMenuState;

    function SelectModelFileNamesAndDisplay: boolean;

    function LoadModelData: boolean;

    function ProcessCustomModelEvent(AData: TModelMenuData): boolean; override;
    function DoResultGridAction(AAction: TGridAction): boolean;
    function DoShowResultGraphs: boolean; virtual;
    function ShowFileEditor: boolean; virtual;
    function DoRefreshFileHints: boolean; virtual;
    function RefreshModelData: boolean;

  public
    function ModelName: string; override;
    function ResetState: boolean; override;
    function Initialise: boolean; override;
    function ModelData: TInterfacedObject; override;
    function ModelDataLoaded: boolean; override;
    function PopulateDataList(AModelDataChangeType : TModelDataChangeType; AList : TList) : boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure DoModelDataHasChanged(AChangeLevel:TModelDataChangeLevel;AChangeType:TModelDataChangeType;
              AChangeAction:TModelDataChangeAction); override;
    //function GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string; override;
    function CanApplicationClose: Boolean; override;
    procedure ApplicationIsClosing; override;
    function IsGraphLoaded: Boolean; override;
    function IsGridLoaded: Boolean; override;

    //function DoStationFilter(AChangeListID : integer) : WordBool;override;
    function DoValidateSingleFile: boolean; virtual;
    function DoValidateAllFiles: boolean; virtual;
    function DoExportAllFiles: boolean; virtual;
    function DoExportSingleFile: boolean; virtual;
    function DoImportAllFiles: boolean; virtual;
    function DoImportSingleFile: boolean; virtual;
    function DoClearModelData: boolean; virtual;
    function DoReadYRCDataFromDB(AEvent: boolean = False): boolean; virtual;
    function DoReadYRCDataFromFile: boolean; virtual;
    function DoSaveYRCDataToDB: boolean; virtual;
    function DoDeleteYRCDataFromDB: boolean;
    function DoRunModel: boolean; virtual;
    function DoExportAllFilesAndRunModel(ASilent:boolean): boolean; virtual;
    //function GetYRCChannelYield(var AValue: double): boolean; virtual;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;


  end;

implementation

uses
  // Delphi
  windows,
  SysUtils,
  VCL.Dialogs,
  VCL.Controls,
  // DWAF
  UDataSetType,
  UDataFileObjects,
  //UFunctionTimer,
  UTreeViewTabSheet,
  USQLDatabaseLayer,
  UYRCModelDataLoadAgent,
  UMainMenuEventType,
  UStringFieldOperations,
  UFilesActionYieldManager,
  UYRCModelMenuItemManager,
  UYieldFileSelectionManager,
  UYRCModelDatasetConstructor,
  UErrorHandlingOperations;

procedure TModelYRCManager.CreateMemberObjects;
const OPNAME = 'TModelYRCManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FLoadViewData := False;


    FFileSelectionManager    := nil;
    FModelFilesActionManager := nil;

    // Create the correct model data.
    CreateModelData;

    // Create these managers first(They may be used by other constructors).
    CreateModelDatasetConstructor;

    // Create member objects.
    //FModelCalendar:= TModelCalendar.Create(FAppModules);
    //FOwnedAppObjects.Add(FModelCalendar);

    // Create the DLL based managers.
    CreateModelTabSheetManagers;

    // Create these managers last(They check if othe managers are created).
    CreateModelMenuItemManager;

    // Create the application based managers.
    CreateFileActionManager;
    CreateFileSelectionManager;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.DestroyMemberObjects;
const OPNAME = 'TModelYRCManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FModelData);
    FreeAndNil(FModelMenuItemManager);
    DeleteModelDatasetConstructor;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.Initialise: boolean;
const OPNAME = 'TModelYRCManager.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := YieldModelData.Initialise;
    if Assigned(FFileEditManager) then
      FFileEditManager.SetOnFileSave(DoValidateSingleFile);
    if Assigned(FModelMenuItemManager) then
     FModelMenuItemManager.Initialise
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.ModelData: TInterfacedObject;
const OPNAME = 'TModelYRCManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.YieldModelData: TYieldModelDataObject;
const OPNAME = 'TModelYRCManager.YieldModelData';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.CreateModelData;
const OPNAME = 'TModelYRCManager.CreateModelData';
begin
  inherited;
  try
    FModelData := TYieldModelDataObject.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelYRCManager.CreateFileActionManager;
const OPNAME = 'TModelYRCManager.CreateFileActionManager';
begin
  try
    FModelFilesActionManager := TFilesActionYieldManager.Create(FAppModules);
    if Assigned(FModelFilesActionManager) then
    begin
      FOwnedAppObjects.Add(FModelFilesActionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelYRCManager.CreateFileSelectionManager;
const OPNAME = 'TModelYRCManager.CreateFileSelectionManager';
begin
  try
    FFileSelectionManager := TYieldFileSelectionManager.Create(FAppModules);
    if Assigned(FFileSelectionManager) then
    begin
      FOwnedAppObjects.Add(FFileSelectionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelYRCManager.CreateFileEditManager;
const OPNAME = 'TModelYRCManager.CreateFileEditManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\FileEditor.dll') then Exit;
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\FileEditor.dll',
      TAbstractAppObject(FFileEditManager), FAppModules, False, OPNAME);
    if Assigned(FFileEditManager) then
    begin
      FOwnedAppObjects.Add(FFileEditManager);
      FFileEditManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.CreateYRCTabSheetManager;
const OPNAME = 'TModelYRCManager.CreateYRCTabSheetManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\YieldReliabilityCurve.dll') then Exit;
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\YieldReliabilityCurve.dll',
      TAbstractAppObject(FYieldReliabilityCurveManager), FAppModules, False, OPNAME);
    if Assigned(FYieldReliabilityCurveManager) then
    begin
      FOwnedAppObjects.Add(FYieldReliabilityCurveManager);
      FYieldReliabilityCurveManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.CreateModelDatasetConstructor;
const OPNAME = 'TModelYRCManager.CreateModelDatasetConstructor';
begin
  try
    // Add the data set constructor.
    TSQLDatabaseLayer(FAppModules.Database).AddDataSetConstructor(TYRCModelDatasetConstructor.Create(FAppModules));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.DeleteModelDatasetConstructor;
const OPNAME = 'TModelYRCManager.DeleteModelDatasetConstructor';
begin
  try
    TSQLDatabaseLayer(FAppModules.Database).DeleteDataSetConstructorsOfType(TYRCModelDatasetConstructor);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.LanguageHasChanged: boolean;
const OPNAME = 'TModelYRCManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Result and Assigned(FYieldReliabilityCurveManager) then
        Result := FYieldReliabilityCurveManager.LanguageHasChanged;
    if Result and Assigned(FModelFilesActionManager) then
        Result := FModelFilesActionManager.LanguageHasChanged;
    if Result and Assigned(FFileSelectionManager) then
        Result := FFileSelectionManager.LanguageHasChanged;
    if Result and Assigned(FFileEditManager) then
        Result := FFileEditManager.LanguageHasChanged;
    if Result and Assigned(FModelMenuItemManager) then
        Result := FModelMenuItemManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.ModelDataLoaded: boolean;
const OPNAME = 'TModelYRCManager.ModelDataLoaded';
begin
  Result := False;
  try
    Result := True;
   if Assigned(FModelData) and
      Assigned(YieldModelData.FileNamesObject) and
      Assigned(YieldModelData.CastYRCGraphDataObject) and//axp
//axp
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames) and
      Assigned(YieldModelData.FileNamesObject.ParamFileNames) and
      Assigned(YieldModelData.FileNamesObject.DirectoryFileNames) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[00]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[01]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[02]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[03]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[04]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[05]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[06]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[07]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[08]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[09]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[10]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[11]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[12]) and
      Assigned(YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[13]) and
      Assigned(YieldModelData.FileNamesObject.ParamFileNames.FileNameObject[00]) and//axp
      Assigned(YieldModelData.FileNamesObject.DirectoryFileNames.FileNameObject[00]) then

   begin
     Result :=
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[00].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[01].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[02].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[03].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[04].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[05].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[06].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[07].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[08].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[09].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[10].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[11].SavedInDB or
              YieldModelData.FileNamesObject.ConfigFileNames.FileNameObject[12].SavedInDB or
              YieldModelData.FileNamesObject.ParamFileNames.FileNameObject[00].SavedInDB or
              YieldModelData.FileNamesObject.DirectoryFileNames.FileNameObject[00].SavedInDB;
   end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TModelYRCManager.ProcessEvent';
var
  LCanChange: boolean;
begin
  Result := True;
  try
    case AEventType of
      CmeValidateFile              : DoValidateSingleFile;
      CmeImportFile                : DoImportSingleFile;
      CmeExportFile                : DoExportSingleFile;
      CmeValidateFiles             : DoValidateAllFiles;
      CmeImportFiles               : DoImportAllFiles;
      CmeExportFiles               : DoExportAllFiles;
      CmeClearModelData            : DoClearModelData;
      CmeRunModel                  : DoRunModel;
      CmeYRCLoadFromDB             : DoReadYRCDataFromDB(True);
      CmeYRCLoadFromFile           : DoReadYRCDataFromFile;
      CmeYRCSaveChart              : DoSaveYRCDataToDB;
      CmeYRCDeleteChart            : DoDeleteYRCDataFromDB;
      CmeViewResultGraphs          : DoShowResultGraphs;
      CmeRefreshFileHints          : DoRefreshFileHints;
      CmeResultPageControlChanged  : OnTabHasChanged(nil);
      CmeResultPageControlChanging :
      begin
        LCanChange := True;
        OnTabChangeRequest(nil,LCanChange);
      end;
    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoShowResultGraphs: boolean;
const OPNAME = 'TModelYRCManager.DoShowResultGraphs';
var LCanChange: boolean;
begin
  Result := False;
  try
    OnTabChangeRequest(nil, LCanChange);
    if Assigned(FYieldReliabilityCurveManager) and LCanChange then
    begin
      FAppModules.MainForm.ActivePage := FYieldReliabilityCurveManager.TabSheet;
      OnTabHasChanged(nil);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.ShowFileEditor: boolean;
const OPNAME = 'TModelYRCManager.ShowFileEditor';
var LCanChange: boolean;
begin
  Result := False;
  try
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
    begin
      OnTabChangeRequest(nil, LCanChange);
      if LCanChange then
      begin
        FAppModules.MainForm.ActivePage := FFileEditManager.TabSheet;
        OnTabHasChanged(nil);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.StudyHasChanged: boolean;
const OPNAME = 'TModelYRCManager.StudyHasChanged';
begin
  //StartFunctionTimer;
  //ProcessFunctionCall(OPNAME);
  Result := False;
  try

    Result :=  LoadModelData;

    Result :=  Result  and inherited StudyHasChanged;

    if Result  and Assigned(FModelMenuItemManager) then
      Result := FModelMenuItemManager.StudyHasChanged;

    if Result and Assigned(FModelFilesActionManager) then
      Result := FModelFilesActionManager.StudyHasChanged;

    if Result and Assigned(FYieldReliabilityCurveManager) then
      Result := Result and FYieldReliabilityCurveManager.StudyHasChanged;

    if Result and Assigned(FFileEditManager) then
      Result := FFileEditManager.StudyHasChanged;


    if Assigned(FAppModules.MainForm()) then
    begin
      FAppModules.MainForm.ApplyPreviousTabIndex;
      OnTabHasChanged(nil);
      // Set System menu state.
      SetSystemMenuState;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
  //ProcessFunctionReturn(OPNAME);
  //FunctionTimerCallTraceTable;
  //FunctionTimerFunctionCountTable;
end;

function TModelYRCManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TModelYRCManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FFileSelectionManager) then
      Result := Result and FFileSelectionManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if Assigned(FYieldReliabilityCurveManager) then
      Result := Result and FYieldReliabilityCurveManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

      if Result and Assigned(FFileEditManager) then
        Result := FFileEditManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.ProcessCustomModelEvent(AData: TModelMenuData): boolean;
const OPNAME = 'TModelYRCManager.ProcessCustomModelEvent';
begin
  Result := False;
  try
    if (not Result) and  Assigned(FYieldReliabilityCurveManager) then
      Result :=  FYieldReliabilityCurveManager.DoCustomTabSheetEvent(AData);

    if (not Result) and Assigned(FFileEditManager) then
      Result := FFileEditManager.DoCustomTabSheetEvent(AData);

    if not Result then
      inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoResultGridAction( AAction: TGridAction): boolean;
const OPNAME = 'TModelYRCManager.DoResultGridAction';
var
  LAction: TGridActionObject;
begin
  Result := False;
  try
    LAction := TGridActionObject.Create(AAction);
    try
      if Assigned(FYieldReliabilityCurveManager) then FYieldReliabilityCurveManager.DoGridAction(LAction);
    finally
      FreeAndNil(LAction);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TModelYRCManager.EditFieldValue(AFieldName, AOldValue: string;
  AContextData: TStrings): boolean;
const OPNAME = 'TModelYRCManager.EditFieldValue';
begin
  Result := False;
  try
    EditFieldValue(AFieldName, AOldValue, '', AContextData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.EditFieldValue(AFieldName, AOldValue,
  ADefaultValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TModelYRCManager.EditFieldValue';
var
  LFieldEditFunction : TFieldEditFunction;
begin
  Result := False;
  try
    LFieldEditFunction := GetFieldEditFunction(AFieldName);
    if Assigned(LFieldEditFunction) then
      LFieldEditFunction(AFieldName, ADefaultValue, AContextData)
    else
      Result := inherited EditFieldValue(AFieldName, AOldValue, ADefaultValue, AContextData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoesFieldHaveACustomEditor(
  AFieldName: String): Boolean;
const OPNAME = 'TModelYRCManager.DoesFieldHaveACustomEditor';
begin
  Result := false;
  try
    Result := Assigned(GetFieldEditFunction(AFieldName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TModelYRCManager.DoImportSumOut: boolean;
const OPNAME = 'TModelYRCManager.DoImportSumOut';
begin
  Result := inherited DoImportSumOut;
  try
    if Result and Assigned(FResultsTabSheetManager) then
      Result := FResultsTabSheetManager.StudyDataHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

procedure TModelYRCManager.CreateModelMenuItemManager;
const OPNAME = 'TModelYRCManager.CreateModelMenuItemManager';
begin
  try

    if not Assigned(FAppModules.MainForm()) then Exit;
    FModelMenuItemManager :=
      TYRCModelMenuItemManager.Create(FAppModules,
        False,
        False,
        False,
        Assigned(FFileEditManager),
        Assigned(FYieldReliabilityCurveManager));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.CreateModelTabSheetManagers;
const OPNAME = 'TModelYRCManager.CreateModelTabSheetManagers';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateFileEditManager;
    CreateYRCTabSheetManager;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelYRCManager.DoValidateSingleFile: boolean;
const OPNAME = 'TModelYRCManager.DoValidateSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateSingleFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoValidateAllFiles: boolean;
const OPNAME = 'TModelYRCManager.DoValidateAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateAllFiles;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoExportAllFiles: boolean;
const OPNAME = 'TModelYRCManager.DoExportAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoExportAllFiles;
    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'FileNames','','');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoImportAllFiles: boolean;
const OPNAME = 'TModelYRCManager.DoImportAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoImportAllFiles;
    Result := Result and RefreshModelData

    {DoModelDataHasChanged(mdclAll,mdctAll,mdcaAdd);

    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoExportSingleFile: boolean;
const OPNAME = 'TModelYRCManager.DoExportSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoExportSingleFile;
    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccImport,'SingleFile','','');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoImportSingleFile: boolean;
const OPNAME = 'TModelYRCManager.DoImportSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoImportSingleFile;
    Result := Result and RefreshModelData
   { // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoClearModelData: boolean;
const OPNAME = 'TModelYRCManager.DoClearModelData';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoClearModelData;
    Result := Result and RefreshModelData

    {SelectModelFileNamesAndDisplay;
    DoModelDataHasChanged(mdclAll,mdctAll,mdcaDelete);
    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
    begin
      FFileEditManager.TabSheet.LanguageHasChanged;
      FFileEditManager.TabSheet.StudyDataHasChanged;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoExportAllFilesAndRunModel(ASilent:boolean): boolean;
const OPNAME = 'TModelYRCManager.DoExportAllFilesAndRunModel';
begin
  Result := False;
  try
    if Asilent then
    begin
      FAppModules.GlobalData.SetStopOnFirstErr(False);
    end;

    Result := FModelFilesActionManager.DoExportAllFilesAndRunModel(ASilent);
    if Result and
       Assigned(FFileSelectionManager) and
       Assigned(FFileEditManager) and
       Assigned(FFileEditManager.TabSheet) then
    begin
      TFileNamesList(YieldModelData.FileNamesObject.OutputFileNames).SetFileHintsSet(True);
        Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                 TModelFileNames(YieldModelData.FileNamesObject));
        FFileEditManager.TabSheet.LanguageHasChanged;
    end;

    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
    begin
      SetSystemMenuState;
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'FileNames','','');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoRunModel: boolean;
const OPNAME = 'TModelYRCManager.DoRunModel';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoRunModel;
    TFileNamesList(YieldModelData.FileNamesObject.OutputFileNames).SetFileHintsSet(True);
    if Result and
       Assigned(FFileSelectionManager) and
       Assigned(FFileEditManager) and
       Assigned(FFileEditManager.TabSheet) then
    begin
      //LockWindowUpdate(FAppModules.MainForm.MainForm.Handle);
      //try
        Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                 TModelFileNames(YieldModelData.FileNamesObject));
        FFileEditManager.TabSheet.LanguageHasChanged;
      //finally
      //  LockWindowUpdate(0);
      //  FAppModules.MainForm.MainForm.Refresh;
     // end;
    end;

    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
    begin
      SetSystemMenuState;
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'FileNames','','');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.SetSystemMenuState;
const OPNAME = 'TModelYRCManager.SetSystemMenuState';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if (YieldModelData.FileNamesObject.FilesCount = 0) then
    begin
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuValidateFiles(msDisable);
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuImportFiles(msDisable);
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msDisable);
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msDisable);
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuRunModel(msDisable);
    end
    else
    begin
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuValidateFiles(msEnable);
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuImportFiles(msEnable);
      TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuRunModel(msEnable);
      if (YieldModelData.FileNamesObject.FilesSavedInDatabaseCount = 0) then
      begin
        TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msDisable);
        TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msDisable);
      end
      else
      begin
        TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msEnable);
        TYRCModelMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msEnable);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.DoModelDataHasChanged(AChangeLevel: TModelDataChangeLevel; AChangeType: TModelDataChangeType;
  AChangeAction: TModelDataChangeAction);
const OPNAME = 'TModelYRCManager.DoModelDataHasChanged';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.SelectModelFileNamesAndDisplay: boolean;
const OPNAME = 'TModelYRCManager.SelectModelFileNamesAndDisplay';
var
  LDataFileObjects: TDataFileObjects;
begin
  Result := False;
  try
    if not Assigned(FFileSelectionManager) then
    begin
      Result := True;
    end
    else
    begin
      LDataFileObjects := TDataFileObjects.Create;
      try
        Result := FFileSelectionManager.PopulateFileNames(LDataFileObjects,TModelFileNames(YieldModelData.FileNamesObject));
        // Populate the File Edit tabsheet treeview.
        if Result and
         Assigned(FFileSelectionManager) and
         Assigned(FFileEditManager) and
         Assigned(FFileEditManager.TabSheet) then
         Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                                                         TModelFileNames(YieldModelData.FileNamesObject));
      finally
        FreeAndNil(LDataFileObjects);
      end;
    end

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.LoadModelData: boolean;
const OPNAME = 'TModelYRCManager.LoadModelData';
begin
  Result := False;
  try
    Result := SelectModelFileNamesAndDisplay;
    Result := Result and DoReadYRCDataFromDB(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoReadYRCDataFromDB(AEvent: boolean = False): boolean;
const OPNAME = 'TModelYRCManager.DoReadYRCDataFromDB';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try

      Result := LDataLoadAgent.ReadYRCDataFromDB(YieldModelData.CastYRCGraphDataObject);
      if Result and AEvent and Assigned(FYieldReliabilityCurveManager) then
        FYieldReliabilityCurveManager.DoCustomTabSheetEvent(TModelMenuData.Create(meYRCChartDataLoaded));
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoReadYRCDataFromFile: boolean;
const OPNAME = 'TModelYRCManager.DoReadYRCDataFromFile';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try
      Result := LDataLoadAgent.ReadYRCDataFromFile(YieldModelData.CastYRCGraphDataObject,YieldModelData.CastFileNamesObject);
      if Result and Assigned(FYieldReliabilityCurveManager) then
        FYieldReliabilityCurveManager.DoCustomTabSheetEvent(TModelMenuData.Create(meYRCChartDataLoaded));
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoSaveYRCDataToDB: boolean;
const OPNAME = 'TModelYRCManager.DoSaveYRCDataToDB';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try
      Result := LDataLoadAgent.SaveYRCDataToDB(YieldModelData.CastYRCGraphDataObject);
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.DoDeleteYRCDataFromDB: boolean;
const OPNAME = 'TModelYRCManager.DoDeleteYRCDataFromDB';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try
      Result := LDataLoadAgent.DeleteYRCDataFromDB(YieldModelData.CastYRCGraphDataObject);
      if Result then
        StudyDataHasChanged(sdccDelete,'YRCData','','');
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{
function TModelYRCManager.GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string;
const OPNAME = 'TModelYRCManager.GetFileLineType';
begin
  Result := '';
  try
    if Assigned(AFileObject) and Assigned(ModelData()) and Assigned(YieldModelData.FilesLineTypes) then
       Result := YieldModelData.FilesLineTypes.GetFileLineType(TFileNameObject(AFileObject),ALineNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
function TModelYRCManager.DoRefreshFileHints: boolean;
const OPNAME = 'TModelYRCManager.DoRefreshFileHints';
begin
  Result := False;
  try
   Result := FModelFilesActionManager.DoRefreshFileHints;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCManager.ApplicationIsClosing;
const OPNAME = 'TModelYRCManager.ApplicationIsClosing';
begin
  inherited ApplicationIsClosing;
  try
    // We need a list of Managers here - VGN 20030214

    if Assigned(FYieldReliabilityCurveManager) and Assigned(FYieldReliabilityCurveManager.TabSheet) then
      FYieldReliabilityCurveManager.TabSheet.ApplicationIsClosing;

    (*
    if Assigned(FViewDataManager) and Assigned(FViewDataManager) then
      FViewDataManager.ApplicationIsClosing;
    *)

    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.ApplicationIsClosing;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.CanApplicationClose: Boolean;
const OPNAME = 'TModelYRCManager.CanApplicationClose';
begin
  Result := inherited CanApplicationClose;
  try
    // We need a list of Managers here - VGN 20030214
    if Assigned(FYieldReliabilityCurveManager) and Assigned(FYieldReliabilityCurveManager.TabSheet) then
      Result := FYieldReliabilityCurveManager.TabSheet.CanApplicationClose;

    (*
    if Assigned(FViewDataManager) and Assigned(FViewDataManager) then
      Result := FViewDataManager.CanApplicationClose;
    *)

    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      Result := FFileEditManager.TabSheet.CanApplicationClose;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.IsGraphLoaded: Boolean;
const OPNAME = 'TModelYRCManager.IsGraphLoaded';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.IsGridLoaded: Boolean;
const OPNAME = 'TModelYRCManager.IsGridLoaded';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.PopulateDataList(AModelDataChangeType: TModelDataChangeType; AList: TList): boolean;
const OPNAME = 'TModelYRCManager.PopulateDataList';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.RefreshModelData: boolean;
const OPNAME = 'TModelYRCManager.RefreshModelData';
begin
  Result :=False;
  try
    Initialise;
    StudyHasChanged;
    LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TModelYRCManager.GetYRCChannelYield(var AValue: double): boolean;
const OPNAME = 'TModelYRCManager.GetYRCChannelYield';
var
  LTargetYRC: double;
begin
  Result := False;
  try
    Result := TFilesActionYieldManager(FModelFilesActionManager).GetYieldChannelYield(AValue);
    if Result then
    begin
      LTargetYRC := YieldModelData.StudyConfigurationData.RunConfigurationData.SystemYRCAndPower.TargetYRCByIndex[1];
      AValue := LTargetYRC - (LTargetYRC * AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}

function TModelYRCManager.ResetState: boolean;
const OPNAME = 'TModelYRCManager.ResetState';
begin
    Result := inherited ResetState;
  try
    if  Result and Assigned(FFileEditManager) then
      Result := Result and FFileEditManager.ResetState;

    if Result and Assigned(FYieldReliabilityCurveManager) then
      Result := Result and FYieldReliabilityCurveManager.ResetState;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCManager.ModelName: string;
const OPNAME = 'TModelYRCManager.ModelName';
begin
  Result := '';
  try
    Result := CYRC;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TModelYRCManager.DoStationFilter(AChangeListID: integer): WordBool;
const OPNAME = 'TModelYRCManager.DoStationFilter';
begin
  Result := True;
  try //Do not insert code here
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.
