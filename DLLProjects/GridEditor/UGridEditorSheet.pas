//
//
//  UNIT      : Contains TGridEditorSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorSheet;

interface

uses
  // Delphi
  Classes,
  vcl.Controls,
  vcl.ComCtrls,

  // DWAF
  UAbstractObject,
  UViewDataItem,
  UGridEditorGrid,
  UDataViewerSheet,
  UDynamicTreeViewTabSheet,
  UAbstractGridData,
  UGridActionObject,
  UAbstractComponent,
  UHelpContexts,
  UGridEditorMenuItemManager;

type
  TDataViewerMode = (dvmNone, dvmGrid, dvmDialog);
  TGridEditorSheet = class(TDynamicTreeViewTabSheet)
  protected
    FGrid: TGridEditorGrid;
    procedure CreateMemberObjects; override;
    procedure SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset); override;
    function GetToolBar: TAbstractToolBar; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
    procedure CreateGridData(var ADataViewerMode:TDataViewerMode;ADataObject: TViewDataTreeNodeData);
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); override;
    procedure DoRotateView(ASender: TObject);

    function GetMenuItemManager: TGridEditorMenuItemManager;
    function FindDatasetRecord(AKeyProperties: TStrings): Boolean;
    procedure AssignHelpContext; override;
  public
    procedure DoOnHint; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function Initialise: boolean; override;
    function DoGridAction(AGridActionObject: TGridActionObject): boolean; override;
    procedure ProcessCustomEvent(AData: TObject); override;
    property MenuItemManager: TGridEditorMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  // Delphi
  SysUtils,
  Variants,

  // DWAF
  VoaimsCom_TLB,
  UUtilities,
  UConstants,
  UGridDataDataSets,
  UGridEditorStringGrid,
  UErrorHandlingOperations,
  UTreeViewTabSheet;

procedure TGridEditorSheet.CreateMemberObjects;
const OPNAME = 'TGridEditorSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    // Set ancestor properties.
    ShowHint := False;
    ParentShowHint := False;

    // Set defaults.
    FTabCaptionKey    := FAppModules.Language.GetString('TabCaption.GridEditorKey');
    FViewTypeConstant := FAppModules.Language.GetString('TabCaption.GridEditor');
    FModelTabSheetName := mtsnInput;

    // Create the grid.
    FGrid := TGridEditorGrid.Create(self, FAppModules);
    FGrid.Parent := self;
    FGrid.Align  := alClient;

    // Create the menu item manager.
    FMenuItemManager := TGridEditorMenuItemManager.Create(FAppModules);

    // Set toolbar the event handlers. Toolbar created by menu manager
    MenuItemManager.ToolBar.PivotBtn.OnClick := DoRotateView;
    MenuItemManager.SetPivotState(False);

  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridEditorSheet.AssignHelpContext;
const OPNAME = 'TGridEditorSheet.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_GridView);
    SetControlHelpContext(FTreeView,HC_GridViewTreeView);
    SetControlHelpContext(FGrid,HC_GridViewGrid);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridEditorSheet.Initialise: boolean;
const OPNAME = 'TGridEditorSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FGrid.Initialise;
    FMenuItemManager.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridEditorSheet.SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset);
const OPNAME = 'TGridEditorSheet.SetCurrentDataSet';
begin
  try
    inherited SetCurrentDataSet(ACurrentDataset);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.ProcessCustomEvent(AData: TObject);
const OPNAME = 'TGridEditorSheet.ProcessCustomEvent';
begin
  try
    case TGridEditorMenuData(AData).Action of
      geRotateView : DoRotateView(nil);
    else
      raise Exception.CreateFmt(FAppModules.Language.GetString('TGridEditorSheet.UnknownGridEditorEventType')
                                , [integer(TGridEditorMenuData(AData).Action)]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.DoRotateView(ASender: TObject);
const OPNAME = 'TGridEditorSheet.DoRotateView';
begin
  try
    if Assigned(FGrid.Grid) and FGrid.Grid.IsVisible then
      FGrid.DoRotateView(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TGridEditorSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.LanguageHasChanged: boolean;
const OPNAME = 'TGridEditorSheet.LanguageHasChanged';
begin
  Result := False;
  try
    inherited LanguageHasChanged;
    FGrid.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.ClearDataViewer;
const OPNAME = 'TGridEditorSheet.ClearDataViewer';
begin
  try
    inherited ClearDataViewer;
    if Assigned(FGrid) then
      FGrid.ClearDataViewer;
 //     FGrid.Grid.ClearDataViewer;
  //    FreeAndNil(FGrid.Grid);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridEditorSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TGridEditorSheet.PopulateDataViewer';
var
  LDataViewerMode:TDataViewerMode;
begin
  try
    inherited PopulateDataViewer(ADataObject);
    if Assigned(ADataObject) and Assigned(ADataObject.ViewDataNode) then
    begin
      CreateGridData(LDataViewerMode,ADataObject);
      if Assigned(FGrid.Grid) then
      begin
        FGrid.Grid.PopulateDataViewer;
        FGrid.Visible := FGrid.Grid.IsVisible;
        FGrid.Grid.UpdateHintText;
      end;

      case LDataViewerMode of
        dvmNone   :
          begin
            MenuItemManager.SetPivotState(False);
            ShowNoDataMessage;
          end;
        dvmGrid   : MenuItemManager.SetPivotState(Assigned(FGrid.Grid) and FGrid.Visible);
        dvmDialog :
        begin
          MenuItemManager.SetPivotState(False);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.CreateGridData(var ADataViewerMode:TDataViewerMode;ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TGridEditorSheet.CreateGridData';
var
  LContextData : string;
  LModelElementID,
  LIndex : integer;
  LClassMatchIndex : integer;
  LClassMatch : boolean;
begin
  ADataViewerMode := dvmNone;
  try

    // Cleanup olde visible elements external to self
    if not Assigned(FGrid.Grid) then
    begin
      for LIndex := 0 to Self.ControlCount - 1 do
      begin
        LClassMatch := false;
        for LClassMatchIndex := 0 to Self.ComponentCount - 1 do
          if Self.Components[LClassMatchIndex].ClassName = Self.Controls[LIndex].ClassName then
            LClassMatch := True;

        // Ask if the control parent is component matched to self
        if not LClassMatch then
          // cleanup last visible control(s) so new active control(s) is not compromised by over/inder lap
          Self.Controls[LIndex].Visible := false;
      end;
    end;

    // Make sure that the old object has been destroyed.
    FGrid.GridData  := nil;
    FGrid.Grid      := nil;
    FGrid.ViewMode  := vmCustomGrid;
    LModelElementID := ADataObject.ViewDataNode.Weighting;

    // Obtain a custom panel for Hydrology Allocation.
    if (ADataObject.ViewDataNode.ViewID = 'HydrologyAllocation') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnHydrologyAllocation,NullInteger);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for run configuration.
    if (ADataObject.ViewDataNode.ViewID = 'RunConfiguration') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnRunConfiguration,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for output configuration.
    if (ADataObject.ViewDataNode.ViewID = 'OutputConfiguration') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnOutputConfiguration,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for Reconciliation Analysis.
    if (ADataObject.ViewDataNode.ViewID = 'ReconciliationAnalysis') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReconciliationAnalysis,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for DroughtRestrictions.
    if (ADataObject.ViewDataNode.ViewID = 'DroughtRestrictions')and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnRestrictions,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'Curtailment') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnCurtailments,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for Channel Areas.
    if (ADataObject.ViewDataNode.ViewID = 'ChannelArea') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannelArea,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for Channel Areas.
    if (ADataObject.ViewDataNode.ViewID = 'ReservoirArea') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReservoirAreaGroup,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for water use proportioning.
    if (ADataObject.ViewDataNode.ViewID = 'WaterUseProportioning') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnWaterUseProportioning,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for master control configuration.
    if (ADataObject.ViewDataNode.ViewID = 'MasterControlConfiguration') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnMasterControlConfiguration,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for catchment proportions.
    if (ADataObject.ViewDataNode.ViewID = 'CatchmentProportions') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnCatchmentProportions,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for reservoir properties.
    if (ADataObject.ViewDataNode.ViewID = 'AReservoir') and (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReservoir,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for PCDDam properties.
    if (ADataObject.ViewDataNode.ViewID = 'PCDDam') and (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnPCDDam,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for UndegroundDam properties.
    if (ADataObject.ViewDataNode.ViewID = 'UndegroundDam') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnUndegroundDam,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'ModelCapability') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnModelCapability,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'MetaData') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnMetaData,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'RunTitle') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnRunTitle,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'ReservoirPenalty') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReservoirPenalty,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for nodes with inflow.
    if (ADataObject.ViewDataNode.ViewID = 'NodesWithInflow') AND (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithInflow,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for nodes without inflow.
    if (ADataObject.ViewDataNode.ViewID = 'NodesWithoutInFlow') AND (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithoutInFlow,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for power plants.
    if (ADataObject.ViewDataNode.ViewID = 'PowerPlants') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnPowerPlant,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for irrigation areas.
    if (ADataObject.ViewDataNode.ViewID = 'IrrigationAreas') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnIrrigationArea,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for irrigation blocks.
    if (ADataObject.ViewDataNode.ViewID = 'IrrigationBlock') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnIrrigationBlock,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for wetlands
    if (ADataObject.ViewDataNode.ViewID = 'Wetland') and(ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnWetland,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for yield model demand centres
    if (ADataObject.ViewDataNode.ViewID = 'YMDemandCentre') and  (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnYMDemandCentre,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for StreamFlowReduction
    if (ADataObject.ViewDataNode.ViewID = 'StreamFlowReduction') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnStreamFlowReduction,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for Mining
    if (ADataObject.ViewDataNode.ViewID = 'Mine') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnMine,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    //Groundwater subcatchment
    else
    if (ADataObject.ViewDataNode.ViewID = 'GroundwaterSubcatchment') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnGroundwaterSubcatchment,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for GroundWater
    if (ADataObject.ViewDataNode.ViewID = 'GroundWater') and (ADataObject.TreeNode.Level > 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnGroundWater,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'AquiferNode') and (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnAquiferNode,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'BaseFlowNode') and (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnBaseFlowNode,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'AbstractionNode') and (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnAbstractionNode,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'CollectionNode') and (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnCollectionNode,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails18') and (ADataObject.TreeNode.Level = 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnIFRFeaturesProperty,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for channel properties.
    if (ADataObject.TreeNode.Level > 1) and
       ((ADataObject.ViewDataNode.ViewID    = 'ChannelDetails2') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails5') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails6') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails7') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails8') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails9') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails10') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails11') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails12') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails18') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails19') or
        (ADataObject.ViewDataNode.ViewID    = 'ChannelDetails20')) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannel,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Create the grid data object for channel penalty levels.
    if (ADataObject.ViewDataNode.ViewID = 'ChannelPenalties') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannelPenalties,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'ModelViews') AND (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnModelViews,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'AllocationDefinition') AND (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnAllocationDefinition,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'SwitchDefinition') AND (ADataObject.TreeNode.Level > 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnSwitchDefinition,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    // Obtain a custom panel for Reconciliation Analysis.
    if (ADataObject.ViewDataNode.ViewID = 'GrowthFactors') then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnGrowthFactors,LModelElementID);
      if FAppModules.Model.ViewInputDialog(Self, LContextData, nil) then
        ADataViewerMode := dvmDialog;
    end
    else
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnInput,NullInteger);
      FAppModules.Model.ViewInputDialog(self, LContextData, nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.GetMenuItemManager: TGridEditorMenuItemManager;
const OPNAME = 'TGridEditorSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TGridEditorMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.DoGridAction(AGridActionObject: TGridActionObject): boolean;
const OPNAME = 'TGridEditorSheet.DoGridAction';
begin
  Result := False;
  try
    if inherited DoGridAction(AGridActionObject) then
    begin
       case AGridActionObject.Action of
         gaEdit, gaView :
           Result := FindDatasetRecord(AGridActionObject.DatasetProperties);
       end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.FindDatasetRecord(AKeyProperties: TStrings): Boolean;
const OPNAME = 'TGridEditorSheet.FindDatasetRecord';
var
  LKeyValues: Variant;
  LKeyFields: string;
  LCount: integer;
begin
  Result := False;
  try
    if (AKeyProperties.Count = 0) then
      Exit;
    if Assigned(FCurrentDataset) and
       Assigned(FCurrentDataset.DataSet()) and
       FCurrentDataset.DataSet.Active then
    begin
      LKeyValues := VarArrayCreate([0, AKeyProperties.Count-1], varOleStr);
      LKeyFields := '';
      for LCount := 0 to AKeyProperties.Count - 1 do
      begin
        LKeyFields := LKeyFields + AKeyProperties.Names[LCount] + ';';
        LKeyValues[LCount] := AKeyProperties.Values[AKeyProperties.Names[LCount]];
      end;
      Delete(LKeyFields, Length(LKeyFields), 1);
      Result := FCurrentDataset.DataSet.Locate(LKeyFields, LKeyValues, []);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TGridEditorSheet.DoTreeNodeAboutToChange';
begin
  try
    MenuItemManager.SetPivotState(False);
    if Assigned(FGrid.Grid) then
      FGrid.SaveState;
    if (FGrid.EditState <> esBrowse) then
      AAllowChange := False;
    inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.DoOnHint;
const OPNAME = 'TGridEditorSheet.DoOnHint';
begin
  try
    if Assigned(FGrid.Grid) then
      FGrid.UpdateHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.StudyHasChanged: boolean;
const OPNAME = 'TGridEditorSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.CanCopyToCLipboard: boolean;
const OPNAME = 'TGridEditorSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    if Assigned(FGrid.Grid) then
      Result := FGrid.Grid.IsVisible
    else
      Result := FAppModules.Model.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.CanExport: boolean;
const OPNAME = 'TGridEditorSheet.CanExport';
begin
  Result := False;
  try
    if Assigned(FGrid.Grid) then
      Result := FGrid.Grid.IsVisible
    else
      Result := FAppModules.Model.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorSheet.CanPrint: boolean;
const OPNAME = 'TGridEditorSheet.CanPrint';
begin
  Result := False;
  try
    if Assigned(FGrid.Grid) then
      Result := FGrid.Grid.IsVisible
    else
      Result := FAppModules.Model.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.DoCopyToCLipboard;
const OPNAME = 'TGridEditorSheet.DoCopyToClipboard';
begin
  try
    if Assigned(FGrid.Grid) then
      FGrid.DoCopyToClipboard
    else
      FAppModules.Model.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorSheet.DoExport(AFileName: string = '');
const OPNAME = 'TGridEditorSheet.DoExport';
begin
  try
    if Assigned(FGrid.Grid) then
      FGrid.DoExport(AFileName)
    else
      FAppModules.Model.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridEditorSheet.DoPrint;
const OPNAME = 'TGridEditorSheet.DoPrint';
begin
  try

    if Assigned(FGrid.Grid) then
      FGrid.DoPrint(FTreeView.Selected.Text)
    else
      FAppModules.Model.DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridEditorSheet.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TGridEditorSheet.StudyDataHasChanged';
var
  LIndex              : integer;
  LNode               : TTreeNode;
  LDataObject         : TViewDataTreeNodeData;
  LReservoir          : IReservoirData;
  LChannel            : IGeneralFlowChannel;
  LIrrigationArea     : IIrrigationArea;
  LIrrigationBlock    : IIrrigationBlock;
  LWetland            : IWetland;
  LYMDemandCentre     : IYMDemandCentre;
  LPowerPlant         : IPowerPlant;
  LNetworkItemName    : string;
  LIdentifier         : integer;
  lALlocDef           : IAllocationDefinition;
  lSwitchDef          : ISwitchDefinition;
  LMine               : IMine;
  LDroughtRestriction : IDroughtRestriction;
  LGroundWater        : IGroundWater;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    LNetworkItemName  := '';
    if (AFieldName = 'ReservoirName') then LNetworkItemName := 'RESERVOIR';
    if (AFieldName = 'ChannelName') then LNetworkItemName := 'CHANNEL';
    if (AFieldName = 'PowerPlantName') then LNetworkItemName := 'POWERPLANT';
    if (AFieldName = 'AreaName') then LNetworkItemName := 'IRRIGATIONAREA';
    if (AFieldName = 'IrrigationBlockName') then LNetworkItemName := 'IRRIGATIONBLOCK';
    if (AFieldName = 'AllocDefName') then LNetworkItemName := 'ALLOCATIONDEFINITION';
    if (AFieldName = 'SwitchDefFileName') then LNetworkItemName := 'SWITCHDEFINITION';
    if (AFieldName = 'WetlandName') then LNetworkItemName := 'WETLAND';
    if (AFieldName = 'YMDemandCentreName') then LNetworkItemName := 'YMDEMANDCENTRE';
    if (AFieldName = 'MineName') then LNetworkItemName := 'MINE';
    if (AFieldName = 'DroughtRestrictionName') then LNetworkItemName := 'DROUGHTRESTRICTIONS';
    if (AFieldName = 'GroundWaterName') then LNetworkItemName := 'GROUNDWATER';

    if (LNetworkItemName <> '') then
    begin
      for LIndex := 0 to FTreeView.Items.Count -1 do
      begin
        LNode := FTreeView.Items[LIndex];
        if Assigned(LNode) then
        begin
          if (LNode.Level = 0) then Continue;

          LDataObject := TViewDataTreeNodeData(LNode.Data);
          if Assigned(LDataObject) and Assigned(LDataObject.ViewDataNode) then
          begin
            LReservoir      := nil;
            LChannel        := nil;
            LIrrigationArea := nil;
            LIrrigationBlock  := nil;
            LWetland        := nil;
            LYMDemandCentre := nil;
            LPowerPlant     := nil;
            lALlocDef       := nil;
            LDroughtRestriction := nil;
            LGroundWater := nil;

            LIdentifier := LDataObject.ViewDataNode.Weighting;
            if (LNetworkItemName = 'RESERVOIR') and
               ((LDataObject.ViewDataNode.DataType = 'RESERVOIR') or
                (LDataObject.ViewDataNode.DataType = 'NODEWITHINFLOW') or
                (LDataObject.ViewDataNode.DataType = 'UNDEGROUNDDAM') or
                (LDataObject.ViewDataNode.DataType = 'PCDDAM') or
                (LDataObject.ViewDataNode.DataType = 'NODEWITHOUTINFLOW') OR
                (LDataObject.ViewDataNode.DataType = 'BASEFLOWNODE') or
                (LDataObject.ViewDataNode.DataType = 'ABSTRACTIONNODE') or
                (LDataObject.ViewDataNode.DataType = 'COLLECTIONNODE')) then
            begin
              LReservoir := (FAppModules.Model.ModelData as IYieldModelData).
                            NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LIdentifier];
              if (LReservoir <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LReservoir.ReservoirConfigurationData.ReservoirName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if (LNetworkItemName = 'CHANNEL') and (LDataObject.ViewDataNode.DataType = 'CHANNEL') then
            begin
              LChannel := (FAppModules.Model.ModelData as IYieldModelData).
                          NetworkElementData.ChannelList.ChannelByChannelNumber[LIdentifier];
              if (LChannel <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LChannel.ChannelName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'POWERPLANT') and (LDataObject.ViewDataNode.DataType = 'POWERPLANT') then
            begin
              LPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).
                             NetworkFeaturesData.PowerPlantList.PowerPlantByID[LIdentifier];
              if (LPowerPlant <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LPowerPlant.FeatureName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'IRRIGATIONAREA') and (LDataObject.ViewDataNode.DataType = 'IRRIGATIONAREA') then
            begin
              LIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).
                                 NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[LIdentifier];
              if (LIrrigationArea <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LIrrigationArea.FeatureName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'IRRIGATIONBLOCK') and (LDataObject.ViewDataNode.DataType = 'IRRIGATIONBLOCK') then
            begin
              LIrrigationBlock := (FAppModules.Model.ModelData as IYieldModelData).
                                  NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
              if (LIrrigationBlock <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption  := LIrrigationBlock.BlockName;
                LNode.Text                                := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if (LNetworkItemName = 'ALLOCATIONDEFINITION') AND (LDataObject.ViewDataNode.DataType = 'ALLOCATIONDEFINITION') then
            begin
              lALlocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                             AllocationDefinitionsList.AllocationDefinitionByID[LIdentifier];
              if (lALlocDef <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := lALlocDef.Name;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if (LNetworkItemName = 'SWITCHDEFINITION') AND (LDataObject.ViewDataNode.DataType = 'SWITCHDEFINITION') then
            begin
              lSwitchDef := (FAppModules.Model.ModelData as IPlanningModelData).
                              SwitchDefinitionsList.SwitchDefinitionByID[LIdentifier];
              if (lSwitchDef <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := lSwitchDef.SwitchDefFileName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'WETLAND') and (LDataObject.ViewDataNode.DataType = 'WETLAND') then
            begin
              LWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[LIdentifier];
              if (LWetland <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption  := LWetland.Name;
                LNode.Text                                := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'YMDEMANDCENTRE') and (LDataObject.ViewDataNode.DataType = 'YMDEMANDCENTRE') then
            begin
              LYMDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[LIdentifier];
              if (LYMDemandCentre <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption  := LYMDemandCentre.Name;
                LNode.Text                                := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'DROUGHTRESTRICTIONS') and (LDataObject.ViewDataNode.DataType = 'DROUGHTRESTRICTIONS') then
            begin
              LDroughtRestriction := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.CurtailmentAndDrought.DroughtRestrictionByID[LIdentifier];
              if (LDroughtRestriction <> nil) then
              begin
                LDataObject.ViewDataNode.OverrideCaption  := LDroughtRestriction.DroughtRestrictionName;
                LNode.Text                                := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'MINE') and (LDataObject.ViewDataNode.DataType = 'MINE') then
            begin
              LMine := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.MineList.MineByIdentifier[LIdentifier];
              if (LMine <> nil) and (LMine.MineName = ANewValue)then
              begin
                LDataObject.ViewDataNode.OverrideCaption  := LMine.MineName ;
                LNode.Text                                := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'GROUNDWATER') and (LDataObject.ViewDataNode.DataType = 'GROUNDWATER') then
            begin
              LGroundWater := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.
                              GroundWaterList.GroundWaterByID[LIdentifier];
              if (LGroundWater <> nil) and (LGroundWater.Name = ANewValue)then
              begin
                LDataObject.ViewDataNode.OverrideCaption  := LGroundWater.Name;
                LNode.Text                                := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
