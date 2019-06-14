//
//
//  UNIT      : Contains TModelRainfallDataTabSheet Class
//  AUTHOR    : Sam Dhlamini (arivia.kom)
//  DATE      : 06/02/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UModelRainfallDataManager;

interface

uses                                            

  Contnrs,
  Classes,
  VCL.dialogs,
  UAbstractObject,
  UGenericModelLinkClasses,
  USystemModelManager,
  RainfallCom_TLB,
  VoaimsCom_TLB,
  UDataObjectRainfall,
  UMainMenuEventType,
  UHDYP08TabManager,
  UDataViewerManager,
  URainfallGaugeSelectionTabSheetManager,
  UPatchRTabSheetManager,
  URainfallGraphTabSheetManager,
  URainfallGaugeStatsSheetManager,
  URainfallPatchAdminSheetManager,
  URainfallZoneSheetManager,
  URainfallMenuItemManager,
  URainfallMultiGaugeComparitorManager,
  UMenuItemManager,
  URainfallCatchmentSummaryTabsheetManager,
  UDataSetType,
  URainfallStationFilterDialog;

type

  TModelRainfallDataManager = class(TSystemModelManager, IRainfallModel)
  protected
    FModelData                             : TDataObjectRainfall;

    FRainfallZoneSheetManager              : TRainfallZoneSheetManager;
//    FHDYP08TabManager                      : THDYP08TabManager;

    FRainfallGaugeSelectionTabSheetManager  : TRainfallGaugeSelectionTabSheetManager;
    FRainfallGaugeStatsSheetManager         : TRainfallGaugeStatsSheetManager;
    FRainfallGraphTabSheetManager           : TRainfallGraphTabSheetManager;
    FRainfallPatchAdminSheetManager         : TRainfallPatchAdminSheetManager;
    FPatchRTabSheetManager                  : TPatchRTabSheetManager;
    FTimeSeriesComparitorManager            : TDataViewerManager;
    FMenuItemManager                        : TRainfallMenuItemManager;
    FRainfallMultiGaugeComparitorManager    : TRainfallMultiGaugeComparitorManager;
    FRainfallCatchmentSummaryTabsheetManager: TRainfallCatchmentSummaryTabsheetManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateTimeSeriesComparitorManager;
    procedure RefreshMenuItems; override;
    procedure DoLaunchDocumentViewer(ADocumentKey : integer);
    function GetKeyValue (AKeyName   : string; AKeyValues : string) : string;
  public
    function ModelName: string; override;
    function Initialise: boolean; override;
    class function CanRainfallTabBeCreated: boolean;
    function PopulateDataList(AModelDataChangeType: TModelDataChangeType; AList: TList): Boolean; override;
    function LanguageHasChanged: Boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    function StudyHasChanged: Boolean; override;
    function ProcessEvent ( AEventType: integer; AData: TObject ): boolean; override;
    function ProcessCustomModelEvent(AData: TModelMenuData): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    function ShowFileEditor: boolean;
    function ShowGridEditor: boolean;
    function EditGridEditor(AData: TObject): boolean;
    function ShowGraph: boolean;
    function DoExportSingleFile : boolean;
    function DoImportSingleFile: boolean;
    function DoRefreshFileHints: boolean;
    function DoRunModel : boolean;
    function DoStationFilter(AChangeListID : integer) : WordBool;
    function FilterStations(AChangeListID : integer;
                                    AParamField   : string;
                                    AKeyValues    : string) :boolean;
    function CheckChangeListNameDoesNotExist(AStationName:string): boolean;
    function RefreshModelData: boolean;
    function ModelData: TInterfacedObject; override;
    procedure OnTabHasChanged(ASender: TObject); override;
    function EntityDescription (AFieldPropName : string;
                                AKeyValues     : string;
                                AFieldIndex    : string) : string; override;
    function GetBaseValue (AFieldPropName : string;
                           AKeyValues     : string;
                           AFieldIndex    : string) : string; override;
    procedure ViewDataGraph; safecall;
    procedure ViewGaugeStats; safecall;
    procedure ViewGaugeSelection; safecall;
    procedure ViewPatchAdmin; safecall;
    function GetMonthlyData (const AStationNumber : WideString;
                             const APatchName     : WideString) : WideString; safecall;
    function GetDailyData (const AStationNumber : WideString;
                           const APatchName     : WideString) : WideString; safecall;
    function GetModelDataSetKey : string; override;
    function GetChangeListWhereClause : string; override;
  end;

implementation

uses
  VCL.Controls,
  VCL.ComCtrls,
  SysUtils,
  UConstants,
  UAbstractComponent,
  UTreeViewTabSheet,
  URainfallGaugeSelectionValidator,
  URainfallGaugeStatsValidator,
  URainfallGraphValidator,
  URainfallPatchAdminValidator,
  UIsAcrobatReaderInstalled,
  UPDFDocumentLauncher,
  UErrorHandlingOperations,
  Math;

class function TModelRainfallDataManager.CanRainfallTabBeCreated: boolean;
const OPNAME = 'TModelRainfallDataManager.CanRainfallTabBeCreated';
begin
  Result := False;
  try
    Result := FileExists ( ExtractFilePath ( ApplicationExeName ) + 'bin\ModelRainfallData.dll')
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelRainfallDataManager.CreateMemberObjects;
const OPNAME = 'TModelRainfallDataManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FModelData := TDataObjectRainfall.Create(FAppModules);

    if Assigned(FAppModules.MainForm()) and
       Assigned(FAppModules.MainForm.PageControl) then
    begin
      FMenuItemManager := TRainfallMenuItemManager.Create(FAppModules);
      FRainfallGaugeSelectionTabsheetManager   := TRainfallGaugeSelectionTabSheetManager.Create(FAppModules);
      FRainfallGraphTabSheetManager            := TRainfallGraphTabSheetManager.Create(FAppModules);
      FRainfallMultiGaugeComparitorManager     := TRainfallMultiGaugeComparitorManager.Create(FAppModules);
      FPatchRTabSheetManager                   := TPatchRTabSheetManager.Create(FAppModules);
//      FHDYP08TabManager                      := THDYP08TabManager.Create(FAppModules);
      FRainfallGaugeStatsSheetManager          := TRainfallGaugeStatsSheetManager.Create(FAppModules);
      FRainfallPatchAdminSheetManager          := TRainfallPatchAdminSheetManager.Create(FAppModules);
      FRainfallZoneSheetManager                := TRainfallZoneSheetManager.Create(FAppModules);
      FTimeSeriesComparitorManager             := nil;
      FRainfallCatchmentSummaryTabsheetManager := TRainfallCatchmentSummaryTabsheetManager.Create(FAppModules);

      FRainfallGaugeSelectionTabsheetManager.TabSheet.PageControl    := FAppModules.MainForm.PageControl;
      FRainfallGaugeStatsSheetManager.TabSheet.PageControl           := FAppModules.MainForm.PageControl;
      FRainfallPatchAdminSheetManager.TabSheet.PageControl           := FAppModules.MainForm.PageControl;
      FPatchRTabSheetManager.TabSheet.PageControl                    := FAppModules.MAinForm.PageControl;
      FRainfallGraphTabSheetManager.TabSheet.PageControl             := FAppModules.MainForm.PageControl;
      FRainfallMultiGaugeComparitorManager.TabSheet.PageControl      := FAppModules.MainForm.PageControl;

      FRainfallZoneSheetManager.TabSheet.PageControl                 := FAppModules.MainForm.PageControl;
      FRainfallCatchmentSummaryTabsheetManager.TabSheet.PageControl  := FAppModules.MainForm.PageControl;
//      FHDYP08TabManager.TabSheet.PageControl                      := FAppModules.MainForm.PageControl;
      CreateTimeSeriesComparitorManager;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelRainfallDataManager.CreateTimeSeriesComparitorManager;
const OPNAME = 'TModelRainfallDataManager.CreateTimeSeriesComparitorManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\TimeSeriesComparitor.dll',
      TAbstractAppObject(FTimeSeriesComparitorManager), FAppModules, False, OPNAME);
    if Assigned(FTimeSeriesComparitorManager) then
    begin
      FOwnedAppObjects.Add(FTimeSeriesComparitorManager);
      FTimeSeriesComparitorManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelRainfallDataManager.DestroyMemberObjects;
const OPNAME = 'TModelRainfallDataManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if Assigned(FRainfallGaugeSelectionTabsheetManager) then
      FreeAndNil(FRainfallGaugeSelectionTabsheetManager);
    if Assigned(FRainfallGaugeStatsSheetManager) then
      FreeAndNil(FRainfallGaugeStatsSheetManager);
    if Assigned(FRainfallPatchAdminSheetManager) then
      FreeAndNil(FRainfallPatchAdminSheetManager);
    if Assigned(FPatchRTabSheetManager) then
      FreeAndNil(FPatchRTabSheetManager);
    if Assigned(FRainfallGraphTabSheetManager) then
      FreeAndNil(FRainfallGraphTabSheetManager);
    if Assigned(FRainfallMultiGaugeComparitorManager) then
      FreeAndNil(FRainfallMultiGaugeComparitorManager);  
//    if Assigned(FHDYP08TabManager) then
//      FreeAndNil(FHDYP08TabManager);
    if Assigned(FRainfallZoneSheetManager) then
      FreeAndNil(FRainfallZoneSheetManager);
    if Assigned(FMenuItemManager) then
      FreeAndNil(FMenuItemManager);
    if Assigned(FModelData) then
      FreeAndNil(FModelData);
    if Assigned(FRainfallCatchmentSummaryTabsheetManager) then
      FreeAndNil(FRainfallCatchmentSummaryTabsheetManager);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.DoExportSingleFile: boolean;
const OPNAME = 'TModelRainfallDataManager.DoExportSingleFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.DoImportSingleFile: boolean;
const OPNAME = 'TModelRainfallDataManager.DoImportSingleFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.DoRefreshFileHints: boolean;
const OPNAME = 'TModelRainfallDataManager.DoRefreshFileHints';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.DoRunModel: boolean;
const OPNAME = 'TModelRainfallDataManager.DoRunModel';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.EditGridEditor(AData: TObject): boolean;
const OPNAME = 'TModelRainfallDataManager.EditGridEditor';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.Initialise: boolean;
const OPNAME = 'TModelRainfallDataManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FRainfallGaugeSelectionTabsheetManager) then
      Result := Result and FRainfallGaugeSelectionTabsheetManager.Initialise;
    if Assigned(FPatchRTabSheetManager) then
      Result := Result and FPatchRTabSheetManager.Initialise;
    if Assigned ( FRainfallGraphTabSheetManager ) then
      Result := Result and FRainfallGraphTabSheetManager.Initialise;
    if Assigned(FRainfallMultiGaugeComparitorManager) then
      Result := Result and FRainfallMultiGaugeComparitorManager.Initialise;  
//    if Assigned ( FHDYP08TabManager ) then
//      Result := Result and FHDYP08TabManager.Initialise;
    if Assigned(FRainfallPatchAdminSheetManager) then
      Result := Result and FRainfallPatchAdminSheetManager.Initialise;
    if Assigned(FRainfallGaugeStatsSheetManager) then
      Result := Result and FRainfallGaugeStatsSheetManager.Initialise;
    if Assigned(FRainfallZoneSheetManager) then
      Result := Result and FRainfallZoneSheetManager.Initialise;
    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.Initialise;
    if Assigned(FRainfallCatchmentSummaryTabsheetManager) then
      Result := Result and FRainfallCatchmentSummaryTabsheetManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.LanguageHasChanged: Boolean;
const OPNAME = 'TModelRainfallDataManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FRainfallGaugeSelectionTabsheetManager) then
      Result := Result and FRainfallGaugeSelectionTabsheetManager.LanguageHasChanged;
    if Assigned(FPatchRTabSheetManager) then
      Result := Result and FPatchRTabSheetManager.LanguageHasChanged;
    if Assigned(FRainfallGraphTabSheetManager) then
      Result := Result and FRainfallGraphTabSheetManager.LanguageHasChanged;
    if Assigned(FRainfallMultiGaugeComparitorManager) then
      Result := Result and FRainfallMultiGaugeComparitorManager.LanguageHasChanged;
//    if Assigned(FHDYP08TabManager) then
//      Result := Result and FHDYP08TabManager.LanguageHasChanged;
    if Assigned(FRainfallGaugeStatsSheetManager) then
      Result := Result and FRainfallGaugeStatsSheetManager.LanguageHasChanged;
    if Assigned(FRainfallPatchAdminSheetManager) then
      Result := Result and FRainfallPatchAdminSheetManager.LanguageHasChanged;
    if Assigned ( FRainfallZoneSheetManager ) then
      Result := Result and FRainfallZoneSheetManager.LanguageHasChanged;
    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.LanguageHasChanged;
    if Assigned(FRainfallCatchmentSummaryTabsheetManager) then
      Result := Result and FRainfallCatchmentSummaryTabsheetManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.ModelData: TInterfacedObject;
const OPNAME = 'TModelRainfallDataManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.ModelName: string;
const OPNAME = 'TModelRainfallDataManager.ModelName';
begin
  Result := '';
  try
    Result := CRainfall;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelRainfallDataManager.OnTabHasChanged(ASender: TObject);
const OPNAME = 'TModelRainfallDataManager.OnTabHasChanged';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;

    if Assigned(FAppModules.MainForm.PageControl.ActivePage) and
      Assigned(FTimeSeriesComparitorManager) and
      Assigned(FTimeSeriesComparitorManager.TabSheet) then
    begin
      if (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FTimeSeriesComparitorManager.TabSheet) then
      begin
        FModelData.PopulateTreeviewWithSelectedProjectGauges(TTreeViewTabSheet(FTimeSeriesComparitorManager.TabSheet).TreeView);
      end;
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.PopulateDataList(
  AModelDataChangeType: TModelDataChangeType; AList: TList): Boolean;
const OPNAME = 'TModelRainfallDataManager.PopulateDataList';
begin
  Result := True;   // When implemented it must be false VGN 20040209
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.ProcessCustomModelEvent (AData : TModelMenuData) : boolean;
const OPNAME = 'TModelRainfallDataManager.ProcessCustomModelEvent';
begin
  Result := FALSE;
  try
    if Assigned(FAppModules.MainForm.PageControl.ActivePage) then
    begin
      if ((Assigned(FTimeSeriesComparitorManager)) AND
          (Assigned(FTimeSeriesComparitorManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FTimeSeriesComparitorManager.TabSheet)) then
        Result := FTimeSeriesComparitorManager.DoCustomTabSheetEvent(AData);

      if ((Assigned(FRainfallGaugeSelectionTabsheetManager)) AND
          (Assigned(FRainfallGaugeSelectionTabsheetManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FRainfallGaugeSelectionTabsheetManager.TabSheet)) then
        Result := FRainfallGaugeSelectionTabsheetManager.DoCustomTabSheetEvent(AData);

      if ((Assigned(FRainfallPatchAdminSheetManager)) AND
          (Assigned(FRainfallPatchAdminSheetManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FRainfallPatchAdminSheetManager.TabSheet)) then
        Result := FRainfallPatchAdminSheetManager.DoCustomTabSheetEvent(AData);

      if ((Assigned(FRainfallGaugeStatsSheetManager)) AND
          (Assigned(FRainfallGaugeStatsSheetManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FRainfallGaugeStatsSheetManager.TabSheet)) then
        Result := FRainfallGaugeStatsSheetManager.DoCustomTabSheetEvent(AData);

      if ((Assigned(FRainfallGraphTabSheetManager)) AND
          (Assigned(FRainfallGraphTabSheetManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FRainfallGraphTabSheetManager.TabSheet)) then
        Result := FRainfallGraphTabSheetManager.DoCustomTabSheetEvent(AData);

      if ((Assigned(FRainfallMultiGaugeComparitorManager)) AND
          (Assigned(FRainfallMultiGaugeComparitorManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FRainfallMultiGaugeComparitorManager.TabSheet)) then
        Result := FRainfallMultiGaugeComparitorManager.DoCustomTabSheetEvent(AData);
 
      if ((Assigned(FRainfallZoneSheetManager)) AND
          (Assigned(FRainfallZoneSheetManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FRainfallZoneSheetManager.TabSheet)) then
        Result := FRainfallZoneSheetManager.DoCustomTabSheetEvent(AData);
      if ((Assigned(FRainfallCatchmentSummaryTabsheetManager)) AND
        (Assigned(FRainfallCatchmentSummaryTabsheetManager.TabSheet)) AND
        (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FRainfallCatchmentSummaryTabsheetManager.TabSheet)) then
      Result := FRainfallCatchmentSummaryTabsheetManager.DoCustomTabSheetEvent(AData);
    end;
    if (NOT Result) then
      inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TModelRainfallDataManager.ProcessEvent';
var
  LCanChange : boolean;
begin
  Result := False;
  try
    case ( AEventType ) of
      CmeViewFileEdit                      : ShowFileEditor;
      CmeViewEditGrid                      : ShowGridEditor;
      CmeRunModel                          : DoRunModel;
      CmeExportFile                        : DoExportSingleFile;
      CmeImportFile                        : DoImportSingleFile;
      CmeRefreshFileHints                  : DoRefreshFileHints;
      CmeResultPageControlChanged          : OnTabHasChanged(nil);
      CmeChangeListStationFilter           : DoStationFilter(-1);
      CmeRainfallUserGuide ,
      CmeRainfallTrainingMaterial,
      CmeRainfallSummaryOfPatchROutput,
      CmeRainfallCLASSRAndPATCHRMethodology,
      CmeRainfallSummaryOfClassROutput,
      CmeRainfallSAWSNumbering              :DoLaunchDocumentViewer(AEventType);
      CmeResultPageControlChanging :
      begin
        LCanChange := True;
        OnTabChangeRequest ( nil, LCanChange );
      end;
    else
      Result := inherited ProcessEvent ( AEventType, AData );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TModelRainfallDataManager.RefreshMenuItems;
const OPNAME = 'TModelRainfallDataManager.RefreshMenuItems';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.RefreshModelData: boolean;
const OPNAME = 'TModelRainfallDataManager.RefreshModelData';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.ShowFileEditor: boolean;
const OPNAME = 'TModelRainfallDataManager.ShowFileEditor';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.ShowGraph: boolean;
const OPNAME = 'TModelRainfallDataManager.ShowGraph';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.ShowGridEditor: boolean;
const OPNAME = 'TModelRainfallDataManager.ShowGridEditor';
var
  lCanChange : boolean;
begin
  Result := False;
  try
    OnTabChangeRequest ( nil, lCanChange );
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.StudyDataHasChanged( AContext: TChangeContext;
                                                        AFieldName, AOldValue,
                                                        ANewValue: String ): Boolean;
const OPNAME = 'TModelRainfallDataManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'CHANGELISTAPPLY') OR
       ((UpperCase(AFieldName) = 'CHANGELIST') AND (AContext = sdccDelete)) OR
       (UpperCase(AFieldName) = 'CHANGELISTCOPY')  OR
       (UpperCase(AFieldName) = 'ELEMENTACTIVE') OR
       (UpperCase(AFieldName) = 'ELEMENTORDER') then
    begin
      if Assigned(FModelData) then
        Result := Result AND FModelData.RefreshStationData;
    end;
    if (UpperCase(AFieldName) = 'USER GAUGES') then
    begin
      if Assigned(FModelData) then
        Result := Result AND FModelData.RefreshUserGauges;
    end;

    if Assigned(FRainfallGaugeSelectionTabsheetManager) then
      Result := Result AND FRainfallGaugeSelectionTabsheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FPatchRTabSheetManager) then
      Result := Result and FPatchRTabSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FRainfallGraphTabSheetManager) then
      Result := Result AND FRainfallGraphTabSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FRainfallMultiGaugeComparitorManager) then
//      Result := Result and FRainfallMultiGaugeComparitorManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FRainfallZoneSheetManager) then
      Result := Result and FRainfallZoneSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FRainfallCatchmentSummaryTabsheetManager) then
      Result := Result and FRainfallCatchmentSummaryTabsheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError( E, OPNAME ); end;
end;

function TModelRainfallDataManager.StudyHasChanged: Boolean;
const OPNAME = 'TModelRainfallDataManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

    if Assigned(FModelData) then
      Result := FModelData.LoadData;

    if Assigned(FAppModules.MainForm()) then
    begin
      if (FAppModules.StudyArea.ScenarioCode = UAbstractObject.CProjectGauges) then
      begin
        if Assigned(FAppModules.MainForm()) and
           Assigned(FAppModules.MainForm.PageControl) then
        begin
          FRainfallZoneSheetManager.TabSheet.TabVisible                := True;
          FRainfallGaugeSelectionTabsheetManager.TabSheet.TabVisible   := TRUE;
          FRainfallGaugeStatsSheetManager.TabSheet.TabVisible          := TRUE;
          FRainfallPatchAdminSheetManager.TabSheet.TabVisible          := TRUE;
          FPatchRTabSheetManager.TabSheet.TabVisible                   := TRUE;
          FRainfallGraphTabSheetManager.TabSheet.TabVisible            := TRUE;
          FTimeSeriesComparitorManager.TabSheet.TabVisible             := TRUE;
          FRainfallMultiGaugeComparitorManager.TabSheet.TabVisible     := True;
          FRainfallCatchmentSummaryTabsheetManager.TabSheet.TabVisible := True;

          FAppModules.MainForm.PageControl.ActivePage := FRainfallGaugeSelectionTabsheetManager.TabSheet;
          OnTabHasChanged(Self);
        end;
        if Assigned(FRainfallGaugeSelectionTabsheetManager) then
          Result := Result and FRainfallGaugeSelectionTabsheetManager.StudyHasChanged;
        if Assigned(FRainfallGaugeStatsSheetManager) then
          Result := Result and FRainfallGaugeStatsSheetManager.StudyHasChanged;
        if Assigned(FRainfallPatchAdminSheetManager) then
          Result := Result and FRainfallPatchAdminSheetManager.StudyHasChanged;
        if Assigned(FPatchRTabSheetManager) then
          Result := Result and FPatchRTabSheetManager.StudyHasChanged;
        if Assigned(FRainfallGraphTabSheetManager) then
          Result := Result and FRainfallGraphTabSheetManager.StudyHasChanged;
        if Assigned(FTimeSeriesComparitorManager) then
          Result := Result and FTimeSeriesComparitorManager.StudyHasChanged;
        if Assigned(FRainfallMultiGaugeComparitorManager) then
          Result := Result and FRainfallMultiGaugeComparitorManager.StudyHasChanged;
        if Assigned(FRainfallZoneSheetManager) then
          Result := Result and FRainfallZoneSheetManager.StudyHasChanged;
        if Assigned(FRainfallCatchmentSummaryTabsheetManager) then
          Result := Result and FRainfallCatchmentSummaryTabsheetManager.StudyHasChanged;
      end;
      FRainfallGaugeSelectionTabsheetManager.TabSheet.PageIndex := 0;
      FRainfallGaugeStatsSheetManager.TabSheet.PageIndex := 1;
      FRainfallGraphTabSheetManager.TabSheet.PageIndex := 3;
      FRainfallMultiGaugeComparitorManager.TabSheet.PageIndex := 4;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TModelRainfallDataManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
    if (FAppModules.MainForm.PageControl.ActivePage = FRainfallGraphTabSheetManager.TabSheet) then
      Result := FRainfallGraphTabSheetManager.ProcessParameterChangeEvent;
//    if (FAppModules.MainForm.PageControl.ActivePage = FRainfallMultiGaugeComparitorManager.TabSheet) then
//      Result := FRainfallMultiGaugeComparitorManager.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelRainfallDataManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TModelRainfallDataManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
    if (FAppModules.MainForm.PageControl.ActivePage = FRainfallGraphTabSheetManager.TabSheet) then
      Result := FRainfallGraphTabSheetManager.ProcessMetaDataEvent;

//    if (FAppModules.MainForm.PageControl.ActivePage = FRainfallMultiGaugeComparitorManager.TabSheet) then
//      Result := FRainfallMultiGaugeComparitorManager.ProcessMetaDataEvent
//    else
      Result := Inherited ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelRainfallDataManager.GetKeyValue (AKeyName   : string;
                                                AKeyValues : string) : string;
const OPNAME = 'TModelRainfallDataManager.GetKeyValue';
var
  lPos : integer;
begin
  Result := '';
  try
    lPos := Pos(AKeyName, AKeyValues);
    if (lPos > 0) then
    begin
      AKeyValues := Copy(AKeyValues, lPos, Length(AKeyValues) - lPos + 1);
      lPos       := Pos('=', AKeyValues);
      if (lPos > 0) then
      begin
        AKeyValues := Copy(AKeyValues, lPos+1, Length(AKeyValues) - lPos);
        lPos       := Pos(',', AKeyValues);
        if (lPos > 0) then
          Result := Copy(AKeyValues, 1, lPos - 1)
        else
          Result := AKeyValues;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelRainfallDataManager.EntityDescription (AFieldPropName : string;
                                                      AKeyValues     : string;
                                                      AFieldIndex    : string) : string;
const OPNAME = 'TModelRainfallDataManager.EntityDescription';
var
  lFieldProperty : TAbstractFieldProperty;
  lStationID     : integer;
  lPatchID       : integer;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lYear          : integer;
  lKeyVal        : string;
begin
  Result := inherited EntityDescription(AFieldPropName, AKeyValues, AFieldIndex);
  try
    if (AFieldPropName <> '') then
    begin
      lFieldProperty := FAppModules.FieldProperties.FieldProperty(Trim(AFieldPropName));
      lYear          := 0;
      if (lFieldProperty <> nil) then
      begin
        if (lFieldProperty.FieldName = 'MonthlyRAWData') OR
           (lFieldProperty.FieldName = 'MonthlyRAWSign') OR
           (lFieldProperty.FieldName = 'MonthlyWRCData') OR
           (lFieldProperty.FieldName = 'MonthlyWRCSign') OR
           (lFieldProperty.FieldName = 'MonthlyPatchData') OR
           (lFieldProperty.FieldName = 'MonthlyPatchSign') then
        begin
          lStationID := StrToInt(GetKeyValue('StationID', AKeyValues));
          lKeyVal    := GetKeyValue('Year', AKeyValues);
          if (lKeyVal <> '') then
            lYear := StrToInt(lKeyVal);
          if (lStationID <> 0) then
            lStation := FModelData.GetStationDataByID(lStationID);
        end;
        if (lStation <> nil) then
        begin
          if (lFieldProperty.FieldName = 'MonthlyRAWData') OR
             (lFieldProperty.FieldName = 'MonthlyRAWSign') then
          begin
            Result := lStation.RainfallData.StationNumber + ' (RAW) ';
            if (lYear <> 0) then
              Result := Result + IntToStr(lYear) + '/' + AFieldIndex;
          end
          else
          if (lFieldProperty.FieldName = 'MonthlyWRCData') OR
             (lFieldProperty.FieldName = 'MonthlyWRCSign') OR
             (lFieldProperty.FieldName = 'MonthlyPatchData') OR
             (lFieldProperty.FieldName = 'MonthlyPatchSign') then
          begin
            lPatchID   := StrToInt(GetKeyValue('PatchID', AKeyValues));
            if (lPatchID <> 0) then
            begin
              lPatch := lStation.GetPatchWithID(lPatchID);
              if (lPatch <> nil) then
              begin
                if (lPatch.PatchTypeID = 1) then
                  Result := lStation.RainfallData.StationNumber + ' (WRC) '
                else
                  Result := lStation.RainfallData.StationNumber + ' (' + lPatch.PatchName + ') ';
                if (lYear <> 0) then
                  Result := Result + IntToStr(lYear) + '/' + AFieldIndex;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.GetBaseValue (AFieldPropName : string;
                                                 AKeyValues     : string;
                                                 AFieldIndex    : string) : string;
const OPNAME = 'TModelRainfallDataManager.GetBaseValue';
var
  lStationID     : integer;
  lPatchID       : integer;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lYear          : integer;
  lMonth         : integer;
  lRainfall      : double;
  lPatchSign     : WideString;
begin
  Result := '';
  try
    if (AFieldPropName <> '') then
    begin
      if (AFieldPropName = 'MonthlyRAWData') OR
         (AFieldPropName = 'MonthlyRAWSign') then
      begin
        lStationID := StrToInt(GetKeyValue('StationID', AKeyValues));
        lYear      := StrToInt(GetKeyValue('Year', AKeyValues));
        lMonth     := STrToInt(AFieldIndex);
        if (lStationID <> 0) then
        begin
          lStation := FModelData.GetStationDataByID(lStationID);
          if (lStation <> nil) then
          begin
            lStation.RainfallData.GetBaseDataForYearAndMonth(lYear, lMonth, lRainfall, lPatchSign);
            if (AFieldPropName = 'MonthlyRAWData') then
            begin
              if (lRainfall = NullFloat) then
                Result := ''
              else
                Result := Format('%6.1f', [lRainfall]);
            end
            else
              Result := lPatchSign;
          end;
        end;
      end
      else
      if (AFieldPropName = 'MonthlyWRCData') OR
         (AFieldPropName = 'MonthlyWRCSign') OR
         (AFieldPropName = 'MonthlyPatchData') OR
         (AFieldPropName = 'MonthlyPatchSign') then
      begin
        lStationID := StrToInt(GetKeyValue('StationID', AKeyValues));
        lPatchID   := StrToInt(GetKeyValue('PatchID', AKeyValues));
        lYear      := StrToInt(GetKeyValue('Year', AKeyValues));
        lMonth     := STrToInt(AFieldIndex);
        if ((lStationID <> 0) AND (lPatchID <> 0)) then
        begin
          lStation := FModelData.GetStationDataByID(lStationID);
          if (lStation <> nil) then
          begin
            lPatch := lStation.GetPatchWithID(lPatchID);
            if (lPatch <> nil) then
            begin
              lPatch.RainfallData.GetBaseDataForYearAndMonth(lYear, lMonth, lRainfall, lPatchSign);
              if (AFieldPropName = 'MonthlyWRCData') OR
                 (AFieldPropName = 'MonthlyPatchData') then
              begin
                if (lRainfall = NullFloat) then
                  Result := ''
                else
                  Result := Format('%6.1f', [lRainfall]);
              end
              else
                Result := lPatchSign;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelRainfallDataManager.ViewDataGraph;
const OPNAME = 'TModelRainfallDataManager.ViewDataGraph';
var
  lForm      : TAbstractForm;
  lValidator : TRainfallGraphValidator;
begin
  try
    LForm   := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      lValidator := TRainfallGraphValidator.Create(lForm,FAppModules);
      try
        if Assigned(lValidator.Panel) then
        begin
          lValidator.Panel.Parent  := lForm;
          lValidator.Panel.Align   := alClient;
          lValidator.Panel.Visible := True;
        end;
        lValidator.Initialise;
        lValidator.PopulateDataViewer;
        lValidator.LanguageHasChanged;
        lForm.ShowModal;
      finally
        lValidator.Free;
      end;
    finally
      lForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelRainfallDataManager.ViewGaugeStats;
const OPNAME = 'TModelRainfallDataManager.ViewGaugeStats';
var
  lForm      : TAbstractForm;
  lValidator : TRainfallGaugeStatsValidator;
begin
  try
    LForm   := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      lValidator := TRainfallGaugeStatsValidator.Create(lForm,FAppModules);
      try
        if Assigned(lValidator.Panel) then
        begin
          lValidator.Panel.Parent  := lForm;
          lValidator.Panel.Align   := alClient;
          lValidator.Panel.Visible := True;
        end;
        lValidator.Initialise;
        lValidator.PopulateDataViewer;
        lValidator.LanguageHasChanged;
        lForm.ShowModal;
      finally
        lValidator.Free;
      end;
    finally
      lForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelRainfallDataManager.ViewGaugeSelection;
const OPNAME = 'TModelRainfallDataManager.ViewGaugeSelection';
var
  lForm      : TAbstractForm;
  lValidator : TRainfallGaugeSelectionValidator;
begin
  try
    LForm   := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      lValidator := TRainfallGaugeSelectionValidator.Create(lForm,FAppModules);
      try
        if Assigned(lValidator.Panel) then
        begin
          lValidator.Panel.Parent  := lForm;
          lValidator.Panel.Align   := alClient;
          lValidator.Panel.Visible := True;
        end;
        lValidator.Initialise;
        lValidator.PopulateDataViewer;
        lValidator.LanguageHasChanged;
        lForm.ShowModal;
      finally
        lValidator.Free;
      end;
    finally
      lForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelRainfallDataManager.ViewPatchAdmin;
const OPNAME = 'TModelRainfallDataManager.ViewPatchAdmin';
var
  lForm      : TAbstractForm;
  lValidator : TRainfallPatchAdminValidator;
begin
  try
    LForm   := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      lValidator := TRainfallPatchAdminValidator.Create(lForm,FAppModules);
      try
        if Assigned(lValidator.Panel) then
        begin
          lValidator.Panel.Parent  := lForm;
          lValidator.Panel.Align   := alClient;
          lValidator.Panel.Visible := True;
        end;
        lValidator.Initialise;
        lValidator.PopulateDataViewer;
        lValidator.LanguageHasChanged;
        lForm.ShowModal;
      finally
        lValidator.Free;
      end;
    finally
      lForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelRainfallDataManager.GetMonthlyData (const AStationNumber : WideString;
                                                   const APatchName     : WideString) : WideString;
const OPNAME = 'TModelRainfallDataManager.GetMonthlyData';
begin
  Result := '';
  try
    Result := FModelData.GetMonthlyData(AStationNumber, APatchName);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.GetDailyData (const AStationNumber : WideString;
                                                 const APatchName     : WideString) : WideString;
const OPNAME = 'TModelRainfallDataManager.GetDailyData';
begin
  Result := '';
  try
    Result := FModelData.GetDailyData(AStationNumber, APatchName);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.GetModelDataSetKey : string;
const OPNAME = 'TModelRainfallDataManager.GetModelDataSetKey';
begin
  Result := '';
  try
    Result := 'Model=' + QuotedStr(FAppModules.StudyArea.ModelCode);
    {Result := 'Model='         + QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              'StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              'SubArea='       + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              'Scenario='      + QuotedStr(FAppModules.StudyArea.ScenarioCode);}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelRainfallDataManager.GetChangeListWhereClause: string;
const OPNAME = 'TModelRainfallDataManager.GetChangeListWhereClause';
begin
  Result := '';
  try
    Result := 'Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ' AND ' +
              'StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ' AND ' +
              '(SubArea  IS NOT NULL) AND ' +
              '(Scenario IS NOT NULL)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelRainfallDataManager.CheckChangeListNameDoesNotExist(AStationName: string): boolean;
const OPNAME = 'TModelRainfallDataManager.CheckChangeListNameDoesNotExist';
var
  LDataset    : TAbstractModelDataset;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'SELECT ListName FROM ChangeList ' ;
        lSQL := lSQL + ' WHERE '
                     + ' ListName = '         + QuotedStr(AStationName)
                     + ' AND Model = '        + QuotedStr(FAppModules.StudyArea.ModelCode)
                     + ' AND StudyAreaName = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)
                     + ' AND SubArea = '      + QuotedStr(FAppModules.StudyArea.SubAreaCode)
                     + ' AND Scenario = '     + QuotedStr(FAppModules.StudyArea.ScenarioCode);
        lDataset.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.Dataset.Open;
        lDataSet.Dataset.First;
        if ((LDataset.DataSet.Bof) and (LDataset.DataSet.Eof))then
          Result := True;
      end;
    finally
    FreeAndNil(LDataset);
    end

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelRainfallDataManager.DoStationFilter(AChangeListID: integer): WordBool;
const OPNAME = 'TModelRainfallDataManager.DoStationFilter';
var
  LStationFilterDialog  : TRainfallStationFilterDialog;
  LRainfallModelData    : IRainfallModelData;
  LStation              : IStationData;
  LPatch                : IPatchData;
  LChangeList           : IChangeList;
  LChangeGroup          : IChangeGroup;
  LKeyValues            : string;
  LStationName          : string;
begin
  Result := False;
  try
    LStationFilterDialog  := TRainfallStationFilterDialog.CreateWithoutDFM(nil,FAppModules);
    try
      if (Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
      begin
        lRainfallModelData := (FAppModules.Model.ModelData as IRainfallModelData);
        LStationFilterDialog.LanguageHasChanged;
        LStationFilterDialog.ShowModal;
        if LStationFilterDialog.ModalResult = mrYes then
        begin
          if (LStationFilterDialog.GraphTreeView.Selected.Level = 0) or
              (LStationFilterDialog.GraphTreeView.Selected.Level = 1)then
          begin
            LStation := (FAppModules.Model.ModelData as IRainfallModelData).
                             GetStationDataByID(lRainfallModelData.CurrentStationID);
            LStationName :=lStation.RainfallData.StationNumber;
            if (CheckChangeListNameDoesNotExist(LStationName))then
            begin
              if (FAppModules.Changes.ChangeGroups.Count <> 0) then
              begin
                LChangeList := FAppModules.Changes.DoCreateNewChangeList;
                LChangeList.ChangeListName := LStationName;
                LKeyValues  := 'StationID='+ IntToStr(lRainfallModelData.CurrentStationID);
                FilterStations( lChangeList.ChangeListID,'MonthlyRAWData',LKeyValues);
              end
              else
              begin
                LChangeGroup               := FAppModules.Changes.DoCreateNewChangeGroup;
                LChangeGroup.GroupName     := FAppModules.Language.GetString('ChangeLists.NewChangeGroup');

                LChangeList := FAppModules.Changes.DoCreateNewChangeList;
                LChangeList.ChangeListName := LStationName;
                LKeyValues  := 'StationID='+ IntToStr(lRainfallModelData.CurrentStationID);
                FilterStations( lChangeList.ChangeListID,'MonthlyRAWData',LKeyValues);
              end;
            end
            else
              ShowMessage(FAppModules.Language.GetString('Rainfall.FilterAlreadyExists'));
          end
          else
            if(LStationFilterDialog.GraphTreeView.Selected.Level > 1)then
            begin
              lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                                GetStationDataByID(lRainfallModelData.CurrentStationID);
              LPatch := LStation.GetPatchWithID(lRainfallModelData.CurrentPatchID);
              LStationName := lStation.RainfallData.StationNumber + ' ('+ lPatch.PatchName +')';
              if(CheckChangeListNameDoesNotExist(LStationName))then
              begin
                if (FAppModules.Changes.ChangeGroups.Count <> 0) then
                begin
                  LChangeList := FAppModules.Changes.DoCreateNewChangeList;
                  LChangeList.ChangeListName := LStationName;
                  LKeyValues :='PatchID=' + intToStr(lRainfallModelData.CurrentPatchID) +
                                ',StationID='+ IntToStr(lRainfallModelData.CurrentStationID);
                  if (lRainfallModelData.CurrentPatchID > 20000)then
                    FilterStations( lChangeList.ChangeListID,'MonthlyPatchData',LKeyValues)
                  else
                    FilterStations( lChangeList.ChangeListID,'MonthlyWRCData',LKeyValues)
                end
                else
                begin
                  LChangeGroup               := FAppModules.Changes.DoCreateNewChangeGroup;
                  LChangeGroup.GroupName     := FAppModules.Language.GetString('ChangeLists.NewChangeGroup');

                  LChangeList := FAppModules.Changes.DoCreateNewChangeList;
                  LChangeList.ChangeListName := LStationName;
                  LKeyValues  :='PatchID=' + intToStr(lRainfallModelData.CurrentPatchID) +
                                ',StationID='+ IntToStr(lRainfallModelData.CurrentStationID);
                  if (lRainfallModelData.CurrentPatchID > 20000)then
                    FilterStations( lChangeList.ChangeListID,'MonthlyPatchData',LKeyValues)
                  else
                    FilterStations( lChangeList.ChangeListID,'MonthlyWRCData',LKeyValues)
                end;
              end
              else
                ShowMessage(FAppModules.Language.GetString('Rainfall.FilterAlreadyExists'));
            end
        end;
      end;
    finally
     LStationFilterDialog.Free;
    end;

    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelRainfallDataManager.FilterStations(AChangeListID: integer;
  AParamField, AKeyValues: string): boolean;
const OPNAME = 'TModelRainfallDataManager.FilterStations';
var
  LDataset    : TAbstractModelDataset;
  LSQL        : string;
  LKeyValues  : string;
  LChangelist : IChangeList;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'SELECT * FROM ChangeParameter ' ;
        lSQL := lSQL + ' WHERE '
                     + ' Model = '            + QuotedStr(FAppModules.StudyArea.ModelCode)
                     + ' AND StudyAreaName = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)
                     + ' AND SubArea = '      + QuotedStr(FAppModules.StudyArea.SubAreaCode)
                     + ' AND Scenario = '     + QuotedStr(FAppModules.StudyArea.ScenarioCode)
                     + ' AND ParamField = '   + QuotedStr(AParamField)
                     + ' AND KeyValues LIKE ' + QuotedStr(AKeyValues + '%');
        lDataset.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.Dataset.Open;
        lDataSet.Dataset.First;

        while (NOT lDataSet.DataSet.Eof) do
        begin
          try
          LKeyValues  := Trim(lDataSet.DataSet.FieldByName('KeyValues').AsString);
          LChangeList := FAppModules.Changes.ChangeListWithID(AChangeListID);
          LChangelist.CreateNewParamChange(Trim(lDataSet.DataSet.FieldByName('ParamField').AsString),
                                           Trim(lDataSet.DataSet.FieldByName('KeyValues').AsString),
                                           Trim(lDataSet.DataSet.FieldByName('FieldIndex').AsString),
                                           Trim(lDataSet.DataSet.FieldByName('Absolut').AsString),
                                           Trim(lDataSet.DataSet.FieldByName('Change').AsString),
                                           Trim(lDataSet.DataSet.FieldByName('ParamDescr').AsString),
                                           True);
          LDataSet.DataSet.Next;
          except on E: Exception do ShowMessage(E.Message);
          end;
        end;
      Result := True;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TModelRainfallDataManager.DoLaunchDocumentViewer(ADocumentKey: integer);
const OPNAME = 'TModelRainfallDataManager.DoLaunchDocumentViewer';
begin
  try
    if IsAcrobatReaderInstalled then
begin
      if(ADocumentKey = CmeRainfallUserGuide) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\Rain IMS User Manual FinalDRAFT.pdf');
      if(ADocumentKey = CmeRainfallTrainingMaterial) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\Rainfall Training Material.pdf');
      if(ADocumentKey = CmeRainfallSummaryOfPatchROutput) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\Rainfall Summary of PatchR output.pdf');
      if(ADocumentKey = CmeRainfallCLASSRAndPATCHRMethodology) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\Rainfall CLASSR and PATCHR Methodology.pdf');
      if(ADocumentKey = CmeRainfallSummaryOfClassROutput) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\Rainfall Summary of ClassR output.pdf');
      if(ADocumentKey = CmeRainfallSAWSNumbering) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\Rainfall SAWS Numbering.pdf');
end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;







end.

