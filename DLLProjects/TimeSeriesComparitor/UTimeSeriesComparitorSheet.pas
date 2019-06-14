//
//
//  UNIT      : Contains TTimeSeriesComparitorViewManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorSheet;

interface

uses
  Classes,
  vcl.Controls,
  vcl.ComCtrls,
  vcl.StdCtrls,
  vcl.Forms,
  VCLTee.TeEngine,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.Chart,

  UDataSetType,
  UViewDataItem,
  UViewDataList,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UDataViewerSheet,
  UMenuItemManager,

  UTimeSeriesComparitorLinkClasses,
  UTimeSeriesComparitorMenuItemManager,
  UTimeSeriesComparitorDisplayPanel,
  UTimeSeriesComparitorDataManager,
  UTimeSeriesComparitorChartManager,

  UTimeSeriesComparitorViewList,
  UTimeSeriesComparitorChartList,
  UTimeSeriesComparitorSeriesList;

type
  TGraphTreeItemDataObject = class(TAbstractAppObject)
  protected
    FLeftAxisCaption: string;
    FLeftAxisFormat: string;
    FLeftAxisUnits: string;
    FBottomAxisCaption: string;
    FBottomAxisFormat: string;
    FBottomAxisUnits: string;
    FGraphHint: string;
  public
    constructor Create(AAppModules: TAppModules);
    procedure PopulateTreeNodeDataObject(AViewID: string);
    property LeftAxisCaption: string read FLeftAxisCaption write FLeftAxisCaption;
    property LeftAxisFormat: string read FLeftAxisFormat write FLeftAxisFormat;
    property LeftAxisUnits: string read FLeftAxisUnits write FLeftAxisUnits;
    property BottomAxisCaption: string read FBottomAxisCaption write FBottomAxisCaption;
    property BottomAxisFormat: string read FBottomAxisFormat write FBottomAxisFormat;
    property BottomAxisUnits: string read FBottomAxisUnits write FBottomAxisUnits;
    property GraphHint: string read FGraphHint write FGraphHint;
  end;

type
  TTimeSeriesComparitorSheet = class(TDataViewerSheet)
  protected
    FDataManager: TTimeSeriesComparitorDataManager;
    FChartManager: TTimeSeriesComparitorChartManager;
    FDisplayPanel: TTimeSeriesComparitorDisplayPanel;
    FSystemSelect: Boolean;
//    FFirstTimeActive: boolean;
    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DeleteEmptyParentNodes; override;
    procedure AssignHelpContext; override;

    // Overriden from TDataViewerSheet.
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); override;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode); override;
    procedure PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData); override;
    procedure ProcessStudyHasChanged;
    procedure RestoreMenuStates;
    procedure SetupSystemMenuState;
    function GetMenuItemManager: TTimeSeriesComparitorMenuItemManager;
    function GetToolBar: TAbstractToolBar; override;

    function OnSelectView(AView: TTimeSeriesComparitorView): boolean;
    function OnSelectChart(AChart: TTimeSeriesComparitorChart): boolean;
    function OnSelectSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
    procedure OnCheckBoxClick(ASender : TObject);
    procedure CheckboxClickOrTreeNodeChange(AReservoirNodeNumber : integer);

    function FindTreeNode(ASeries: TTimeSeriesComparitorSeries): TTreeNode;
    procedure SetViewHasMetaData;
    procedure SetChartHasMetaData;
    procedure OnValidateCreateChart(Sender: TObject);
    procedure OnValidateRenameChart(Sender: TObject);
    procedure OnValidateDeleteChart(Sender: TObject);
    procedure OnValidateCreateView(Sender: TObject);
    procedure OnValidateRenameView(Sender: TObject);
    procedure OnValidateDeleteView(Sender: TObject);
    procedure SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset); override;
  public
    procedure SetMenuVisible(AVisible: boolean); override;

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;

    procedure PopulateTreeView; override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
    procedure TabHasChanged; override;

    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;

    //External event handlers
    procedure CreateView;
    procedure RenameView;
    procedure DeleteView;

    procedure CreateChart;
    procedure RenameChart;
    procedure DeleteChart;
    procedure AddChartToView;
    procedure RemoveChartFromView;

    procedure AddSeriesToChart;
    procedure RemoveSeriesFromChart;
    procedure SetSeriesColor;
    procedure OnChartClick(Sender: TObject);
    procedure OnChartSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
                                   Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SetChartName;
    procedure SaveView;
    procedure TSCToggleIndividualSeries;
    procedure ShowChartLegendDialog;
    function ProcessMetaDataEvent : boolean; override;
    function EntityDescription (AFieldPropName : string;
                                AKeyValues     : string;
                                AFieldIndex    : string) : string; override;
    property MenuItemManager: TTimeSeriesComparitorMenuItemManager read GetMenuItemManager;
    property ToolBar: TAbstractToolBar read GetToolBar;
  end;

implementation

uses
  Contnrs,
  SysUtils,
  System.UITypes,
  UStudyArea,
  VCL.Dialogs,
  UConstants,
  UTreeViewTabSheet,
  UErrorHandlingOperations,
  UTimeSeriesComparitorChartActionForm,
  UTimeSeriesComparitorChartLegendDialog,
  StrUtils;

var
  frmChartLegendDialog : TTimeSeriesComparitorChartLegendDialog;

procedure TTimeSeriesComparitorSheet.CreateMemberObjects;
const
  OPNAME = 'TTimeSeriesComparitorSheet.CreateMemberObjects';
begin
  try
//    FFirstTimeActive := True;
    FViewTypeConstant := 'Graph_1';
    FTabCaptionKey := 'TimeSeriesComparitor';

    inherited CreateMemberObjects;

    FViewTypeConstant := 'Graph';

    FMenuItemManager := TTimeSeriesComparitorMenuItemManager.Create(FAppModules);
    FDisplayPanel := TTimeSeriesComparitorDisplayPanel.Create(Self, FAppModules);
    FDataManager := TTimeSeriesComparitorDataManager.Create(FAppModules, FDisplayPanel);
    FDataManager.ChartDataManager.ChartClick := OnChartClick;
    FDataManager.ChartDataManager.ChartSeriesClick := OnChartSeriesClick;
    FDataManager.ChartDataManager.ChartMouseMove := OnChartMouseMove;

    FSystemSelect := FALSE;

    FDisplayPanel.Parent := Self;
    FDisplayPanel.Align := alClient;

    FDisplayPanel.SelectorsPanel.OnSelectView := OnSelectView;
    FDisplayPanel.SelectorsPanel.OnSelectChart := OnSelectChart;
    FDisplayPanel.SelectorsPanel.OnSelectSeries := OnSelectSeries;
    FDisplayPanel.SelectorsPanel.PlotReservoirFixedValues.OnClick := OnCheckBoxClick;

  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.DestroyMemberObjects;
const
  OPNAME = 'TTimeSeriesComparitorSheet.DestroyMemberObjects';
begin
  try
    try
      FreeAndNil(FDataManager);
    finally
      inherited DestroyMemberObjects;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.AssignHelpContext;
const
  OPNAME = 'TTimeSeriesComparitorSheet.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
    SetControlHelpContext(Self, HC_ChannelOutputOptions);
    SetControlHelpContext(FTreeView, HC_GraphViewTreeView);
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorSheet.LanguageHasChanged: boolean;
const
  OPNAME = 'TTimeSeriesComparitorSheet: LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FMenuItemManager.LanguageHasChanged;
    FDisplayPanel.LanguageHasChanged;
    FDataManager.LanguageHasChanged;
    SetViewHasMetaData;
    SetChartHasMetaData;
    Result := True;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TTimeSeriesComparitorSheet.StudyHasChanged: boolean;
const
  OPNAME = 'TTimeSeriesComparitorSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    ProcessStudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.ProcessStudyHasChanged;
const
  OPNAME = 'TTimeSeriesComparitorSheet.ProcessStudyHasChanged';
begin
  try
    FDisplayPanel.SelectorsPanel.HideNoDataMessage;
    FDataManager.StudyHasChanged;
    FDataManager.PopulatingAgent.PopulateNodesData(TreeView.Items);
    FMenuItemManager.Initialise;
    FDisplayPanel.Initialise;
    FDataManager.Initialise;

    FMenuItemManager.StudyHasChanged;
    FDisplayPanel.StudyHasChanged;
    FDataManager.RestoreScenarioData;
    FDisplayPanel.ChartPanel.SynchroniseChartsXAxis;

    FMenuItemManager.LanguageHasChanged;
    FDisplayPanel.LanguageHasChanged;
    FDataManager.LanguageHasChanged;

    RestoreMenuStates;
    SetupSystemMenuState;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const
  OPNAME = 'TTimeSeriesComparitorSheet.DoTreeNodeAboutToChange';
begin
  try
    if (not (csDestroying in ComponentState)) then
    begin
      FDisplayPanel.SelectorsPanel.EnablePlotReservoirCheckBox;
      if AAllowChange then
      begin
        FDisplayPanel.SelectorsPanel.HideNoDataMessage;
        if (FAppModules.Model() <> nil) then
          FAppModules.Model.ViewData.CurrentTreeNodeData := nil;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);
const OPNAME = 'TTimeSeriesComparitorSheet.DoTreeNodeHasChanged';
begin
  try
    inherited DoTreeNodeHasChanged(ASender, ANode);
    FDisplayPanel.SelectorsPanel.EnablePlotReservoirCheckBox;
    if(ANode <> nil) and (ANode.Data <> nil)then
      CheckboxClickOrTreeNodeChange(TViewDataTreeNodeData(ANode.Data).ViewDataNode.ElementID);
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.DeleteEmptyParentNodes;
const
  OPNAME = 'TTimeSeriesComparitorSheet.DeleteEmptyParentNodes';
var
  LCount, LDataSetIndex: integer;
  LNode: TTreeNode;
  LViewDataNode: TViewDataTreeNodeData;
  LCountainer: TObjectList;
begin
  try
    LCount := Self.NodeCount - 1;
    LCountainer := TObjectList.Create(False);
    try
      while ((LCount >= 0) and (Self.NodeCount > 0)) do
      begin
        LViewDataNode := TViewDataTreeNodeData(Self.NodeByIndex[LCount].Data);
        if (LViewDataNode <> nil) then
        begin
          for LDataSetIndex := 0 to LViewDataNode.ViewDataNode.ViewDataSetCount - 1 do
          begin
            if (LViewDataNode.TreeNode.Count = 0) and (LViewDataNode.ViewDataNode.ViewDataSetCount < 0) then
              LCountainer.Add(LViewDataNode.TreeNode)
            else
              if (LViewDataNode.TreeNode.Count = 0) and (LViewDataNode.ViewDataNode.ViewDataSetCount > 0) and
                (LViewDataNode.ViewDataNode.ViewDataSet[LDataSetIndex].SQLType <> 1) then
                LCountainer.Add(LViewDataNode.TreeNode);
          end;
        end;
        LCount := LCount - 1;
      end;

      FTreeView.Items.BeginUpdate;
      try
        for LCount := 0 to LCountainer.Count - 1 do
        begin
          LNode := TTreeNode(LCountainer.Items[LCount]);
          FTreeView.Items.Delete(LNode);
        end;
      finally
        FTreeView.Items.EndUpdate;
      end;
    finally
      FTreeView.Items.EndUpdate;
      LCountainer.Free;
    end;
    inherited DeleteEmptyParentNodes
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const
  OPNAME = 'TTimeSeriesComparitorSheet.PopulateDataViewer';
var
  LViewDataSet: TViewDataSet;
  LSeries: TTimeSeriesComparitorSeries;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    if StudyIsChanging then Exit;
    FDataManager.RemoveTemporaryCurrentSeries;
    FDataManager.RestoreCurrentSeries;
    FDisplayPanel.SelectorsPanel.HideNoDataMessage;

    if (FDataManager.CurrentChartData = nil) then
    begin
      FDisplayPanel.SelectorsPanel.ShowNoDataMessage(FAppModules.Language.GetString('TSCSheet.NoChart'));
    end
    else
    begin
      if (ADataObject <> nil) and (ADataObject.ViewDataNode <> nil) and
        (ADataObject.ViewDataNode.ViewDataSetCount = 1) and
        (ADataObject.ViewDataNode.ShowSQL) then
      begin
        FDataManager.CurrentChartData.Chart.BottomAxis.Title.Caption :=
          FAppModules.Language.GetString('TSCSheet.X_AxisLabel');
        LSeries := FDataManager.AddSeries(ADataObject);

        if (LSeries = nil) then
        begin
          LViewDataSet := ADataObject.ViewDataNode.ViewDataSet[0];
          if (LViewDataSet <> nil) then
            FDisplayPanel.SelectorsPanel.ShowNoDataMessage(FAppModules.Language.GetString('ViewData.' + LViewDataSet.NoDataMessage));
        end
        else
        begin
          if FDisplayPanel.SelectorsPanel.AddSeries(LSeries) then
            FDisplayPanel.SelectorsPanel.SelectCurrentSeries(LSeries);

          RestoreMenuStates;
          SetupSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.PopulateTreeView;
const
  OPNAME = 'TTimeSeriesComparitorSheet.PopulateTreeView';
var
  LIndex : integer;
  LNode  : TTreeNode;
begin
  inherited PopulateTreeView;
  try
    FTreeView.Items.BeginUpdate;
    try
      FAppModules.Model.ViewData.PopulateTreeNodes(FTreeView.Items, FViewTypeConstant);
      for LIndex := 0 to FTreeViewNodes.Count - 1 do
      begin
        LNode := NodeByIndex[LIndex];
        if (LNode <> nil) and (LNode.Data <> nil) then
          PopulateTreeNodeDataObject(TViewDataTreeNodeData(LNode.Data));
      end;
    finally
      FTreeView.Items.EndUpdate;
    end;
    DeleteEmptyParentNodes;
  except on E: Exception do HandleError(E, OPNAME);end;
end;

procedure TTimeSeriesComparitorSheet.PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TTimeSeriesComparitorSheet.PopulateTreeNodeDataObject';
var LGraphDatasetData: TGraphTreeItemDataObject;
begin
  try
    if (ADataObject <> nil) and
       (ADataObject.ViewDataNode <> nil) and
       (ADataObject.ViewDataNode.ViewDataSetCount > 0) then
    begin
      LGraphDatasetData := TGraphTreeItemDataObject.Create(FAppModules);
      LGraphDatasetData.PopulateTreeNodeDataObject(ADataObject.ViewDataNode.ViewID);
      ADataObject.Data  := LGraphDatasetData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSheet.GetMenuItemManager: TTimeSeriesComparitorMenuItemManager;
const
  OPNAME = 'TTimeSeriesComparitorSheet: GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TTimeSeriesComparitorMenuItemManager(FMenuItemManager);
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TTimeSeriesComparitorSheet.GetToolBar: TAbstractToolBar;
const
  OPNAME = 'TTimeSeriesComparitorSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.SetMenuVisible(AVisible: boolean);
const
  OPNAME = 'TTimeSeriesComparitorSheet.SetMenuVisible';
begin
  inherited SetMenuVisible(AVisible);
  try
    if Assigned(FMenuItemManager) and AVisible then
      RestoreMenuStates;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TTimeSeriesComparitorSheet.CanCopyToClipboard: boolean;
const
  OPNAME = 'TTimeSeriesComparitorSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := FDisplayPanel.ChartPanel.CanCopyToClipboard;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TTimeSeriesComparitorSheet.CanExport: boolean;
const
  OPNAME = 'TTimeSeriesComparitorSheet.CanExport';
begin
  Result := False;
  try
    Result := FDisplayPanel.ChartPanel.CanExport;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TTimeSeriesComparitorSheet.CanPrint: boolean;
const
  OPNAME = 'TTimeSeriesComparitorSheet.CanPrint';
begin
  Result := False;
  try
    Result := FDisplayPanel.ChartPanel.CanPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorSheet.DoCopyToClipboard;
const
  OPNAME = 'TTimeSeriesComparitorSheet.DoCopyToClipboard';
begin
  try
    FDisplayPanel.ChartPanel.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorSheet.DoExport(AFileName: string = '');
const
  OPNAME = 'TTimeSeriesComparitorSheet.DoExport';
begin
  try
    FDisplayPanel.ChartPanel.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorSheet.DoPrint;
const
  OPNAME = 'TTimeSeriesComparitorSheet.DoPrint';
begin
  try
    FDisplayPanel.ChartPanel.DoPrint;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorSheet.AddChartToView;
const
  OPNAME = 'TTimeSeriesComparitorSheet.AddChartToView';
begin
  try
    if FDataManager.AddCurrentChartToView then
    begin
      RestoreMenuStates;
      SetupSystemMenuState;
      MenuItemManager.SetMenuSaveView(msEnable);
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.RemoveChartFromView;
const
  OPNAME = 'TTimeSeriesComparitorSheet.RemoveChartFromView';
begin
  try
    if FDataManager.RemoveCurrentChartFromView then
    begin
      RestoreMenuStates;
      SetupSystemMenuState;
      MenuItemManager.SetMenuSaveView(msEnable);
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.AddSeriesToChart;
const
  OPNAME = 'TTimeSeriesComparitorSheet.AddSeriesToChart';
begin
  try
    MenuItemManager.SetMenuRemoveSeries(msDisable);
    MenuItemManager.SetMenuAddSeries(msDisable);
    if FDataManager.AddCurrentSeriesToChart then
    begin
      RestoreMenuStates;
      SetupSystemMenuState;
      MenuItemManager.SetMenuSaveView(msEnable);
    end;
    FDisplayPanel.SelectorsPanel.RepopulateSeriesComboBox;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.RemoveSeriesFromChart;
const
  OPNAME = 'TTimeSeriesComparitorSheet.RemoveSeriesFromChart';
begin
  try
    if (MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.ConfirmationOfDeletingSeries'),
                    mtConfirmation, [mbYes,mbNo,mbCancel],0) = mrYes) then
    begin
      MenuItemManager.SetMenuRemoveSeries(msDisable);
      MenuItemManager.SetMenuAddSeries(msDisable);
      if FDataManager.RemoveCurrentSeriesFromChart then
      begin
        FDisplayPanel.SelectorsPanel.SelectCurrentSeries(nil);
        if FDataManager.CurrentViewData.Normalised then
          FDataManager.CurrentChartData.Normalised := True;
        RestoreMenuStates;
        SetupSystemMenuState;
        MenuItemManager.SetMenuSaveView(msEnable);
      end;
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.SetChartName;
const
  OPNAME = 'TTimeSeriesComparitorSheet.SetChartName';
begin
  try
    FDisplayPanel.ChartPanel.SetChartName;
    MenuItemManager.SetMenuSaveView(msEnable);
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.SaveView;
const
  OPNAME = 'TTimeSeriesComparitorSheet.SaveView';
begin
  try
    FDataManager.SaveCurrentView;
    MenuItemManager.SetMenuSaveView(msDisable);
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.CreateChart;
const
  OPNAME = 'TTimeSeriesComparitorSheet.CreateChart';
var
  LViewName,
    LChartName: string;
  LChart: TTimeSeriesComparitorChart;
  LView: TTimeSeriesComparitorView;
begin
  try
    frmChartAction := TfrmChartAction.Create(nil, FAppModules);
    try
      frmChartAction.Action := cacCreateChart;
      frmChartAction.edtTop.Text := '';
      frmChartAction.edtBottom.Text := '';
      if Assigned(FDisplayPanel.SelectorsPanel.CurrentView) then
        frmChartAction.edtBottom.Text := FDisplayPanel.SelectorsPanel.CurrentView.ViewName;

      frmChartAction.btnOk.Default     := True;
      frmChartAction.btnCancel.Cancel  := True;

      frmChartAction.btnOk.OnClick := OnValidateCreateChart;
      frmChartAction.ShowModal;
      if (frmChartAction.ModalResult = mrOk) then
      begin
        FDisplayPanel.SelectorsPanel.HideNoDataMessage;
        FDataManager.RemoveTemporaryCurrentSeries;
        FDataManager.RemoveTemporaryCurrentChart;
        FDataManager.RestoreCurrentSeries;
        LViewName := Trim(frmChartAction.edtBottom.Text);
        LChartName := Trim(frmChartAction.edtTop.Text);
        if not FDataManager.ViewNameExists(LViewName) then
        begin
          LView := FDataManager.AddView(LViewName);
          if Assigned(LView) then
            if FDisplayPanel.SelectorsPanel.AddView(LView) then
              FDisplayPanel.SelectorsPanel.SelectCurrentView(LView);
        end;
        LChart := FDataManager.AddChart(LChartName);
        if Assigned(LChart) then
        begin
          if FDisplayPanel.SelectorsPanel.AddChart(LChart) then
            FDisplayPanel.SelectorsPanel.SelectCurrentChart(LChart);
          AddChartToView;
        end;
        RestoreMenuStates;
        SetupSystemMenuState;
        MenuItemManager.SetMenuSaveView(msEnable);
      end;
    finally
      FreeAndNil(frmChartAction);
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.RenameChart;
const
  OPNAME = 'TTimeSeriesComparitorSheet.RenameChart';
var
  LOldChartName,
    LNewChartName: string;
begin
  try
    if not Assigned(FDisplayPanel.SelectorsPanel.CurrentChart) then Exit;
    frmChartAction := TfrmChartAction.Create(nil, FAppModules);
    try
      frmChartAction.Action := cacRenameChart;
      frmChartAction.edtTop.Text := FDisplayPanel.SelectorsPanel.CurrentChart.ChartName;
      frmChartAction.edtBottom.Text := FDisplayPanel.SelectorsPanel.CurrentChart.ChartName;
      frmChartAction.btnOk.OnClick := OnValidateRenameChart;
      frmChartAction.ShowModal;
      if (frmChartAction.ModalResult = mrOk) then
      begin
        FDisplayPanel.SelectorsPanel.HideNoDataMessage;
        LNewChartName := Trim(frmChartAction.edtBottom.Text);
        LOldChartName := Trim(frmChartAction.edtTop.Text);
        if FDataManager.RenameChart(LOldChartName, LNewChartName) then
        begin
          FStudyIsChanging := True;
          try
            FDisplayPanel.SelectorsPanel.RenameChart(FDisplayPanel.SelectorsPanel.CurrentChart);
          finally
            FStudyIsChanging := False;
          end;
          FDisplayPanel.SelectorsPanel.SelectCurrentChart(FDisplayPanel.SelectorsPanel.CurrentChart);
        end;
      end;
      MenuItemManager.SetMenuSaveView(msEnable);
    finally
      FreeAndNil(frmChartAction);
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.DeleteChart;
const
  OPNAME = 'TTimeSeriesComparitorSheet.DeleteChart';
var
  LChartName: string;
begin
  try
    frmChartAction := TfrmChartAction.Create(nil, FAppModules);
    try
      frmChartAction.Action := cacDeleteChart;
      frmChartAction.edtTop.Text := FDisplayPanel.SelectorsPanel.CurrentChart.ChartName;
      frmChartAction.edtBottom.Text := FDisplayPanel.SelectorsPanel.CurrentView.ViewName;
      frmChartAction.btnOk.OnClick := OnValidateDeleteChart;
      frmChartAction.ShowModal;
      if (frmChartAction.ModalResult = mrOk) then
      begin
        FDisplayPanel.SelectorsPanel.HideNoDataMessage;
        LChartName := Trim(frmChartAction.edtTop.Text);
        if FDataManager.DeleteChart(LChartName) then
          if FDisplayPanel.SelectorsPanel.DeleteChart(FDisplayPanel.SelectorsPanel.CurrentChart) then
          begin
            FDisplayPanel.SelectorsPanel.SelectCurrentChart(FDataManager.CurrentChartData);
            if Assigned(FDataManager.CurrentChartData) then
              FDataManager.SelectChart(FDataManager.CurrentChartData);
          end;

        RestoreMenuStates;
        SetupSystemMenuState;
        MenuItemManager.SetMenuSaveView(msEnable);
      end;
    finally
      FreeAndNil(frmChartAction);
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.CreateView;
const
  OPNAME = 'TTimeSeriesComparitorSheet.CreateView';
var
  LViewName: string;
  LView: TTimeSeriesComparitorView;
begin
  try
    frmChartAction := TfrmChartAction.Create(nil, FAppModules);
    try
      frmChartAction.Action := cacCreateView;
      frmChartAction.edtTop.Text := '';
      frmChartAction.edtBottom.Text := '';
      frmChartAction.btnOk.OnClick := OnValidateCreateView;
      frmChartAction.ShowModal;
      if (frmChartAction.ModalResult = mrOk) then
      begin
        FDisplayPanel.SelectorsPanel.HideNoDataMessage;
        FDataManager.RemoveTemporaryCurrentSeries;
        FDataManager.RemoveTemporaryCurrentChart;
        FDataManager.RestoreCurrentSeries;
        LViewName := Trim(frmChartAction.edtTop.Text);
        LView := FDataManager.AddView(LViewName);
        if Assigned(LView) then
          if FDisplayPanel.SelectorsPanel.AddView(LView) then
          begin
            FDisplayPanel.SelectorsPanel.SelectCurrentView(LView);
            LView.Normalised :=
              TTimeSeriesComparitorMenuItemManager(FMenuItemManager).ToolBar.ToggleIndividualSeries.Down;
          end;

        RestoreMenuStates;
        SetupSystemMenuState;
        MenuItemManager.SetMenuSaveView(msEnable);
      end;
    finally
      FreeAndNil(frmChartAction);
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.RenameView;
const
  OPNAME = 'TTimeSeriesComparitorSheet.RenameView';
var
  LNewViewName,
    LOldViewName: string;
begin
  try
    if not Assigned(FDisplayPanel.SelectorsPanel.CurrentView) then Exit;
    frmChartAction := TfrmChartAction.Create(nil, FAppModules);
    try
      frmChartAction.Action := cacRenameView;
      frmChartAction.edtTop.Text := FDisplayPanel.SelectorsPanel.CurrentView.ViewName;
      frmChartAction.edtBottom.Text := FDisplayPanel.SelectorsPanel.CurrentView.ViewName;
      frmChartAction.btnOk.OnClick := OnValidateRenameView;
      frmChartAction.ShowModal;
      if (frmChartAction.ModalResult = mrOk) then
      begin
        LNewViewName := Trim(frmChartAction.edtBottom.Text);
        LOldViewName := Trim(frmChartAction.edtTop.Text);
        if FDataManager.RenameView(LOldViewName, LNewViewName) then
        begin
          FStudyIsChanging := True;
          try
            FDisplayPanel.SelectorsPanel.RenameView(FDisplayPanel.SelectorsPanel.CurrentView);
          finally
            FStudyIsChanging := False;
          end;
          FDisplayPanel.SelectorsPanel.SelectCurrentView(FDisplayPanel.SelectorsPanel.CurrentView);
        end;
      end;
      MenuItemManager.SetMenuSaveView(msEnable);
    finally
      FreeAndNil(frmChartAction);
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.DeleteView;
const
  OPNAME = 'TTimeSeriesComparitorSheet.DeleteView';
var
  LViewName: string;
begin
  try
    frmChartAction := TfrmChartAction.Create(nil, FAppModules);
    try
      frmChartAction.Action := cacDeleteView;
      frmChartAction.edtTop.Text := FDisplayPanel.SelectorsPanel.CurrentView.ViewName;
      frmChartAction.edtBottom.Text := '';
      frmChartAction.btnOk.OnClick := OnValidateDeleteView;
      frmChartAction.ShowModal;
      if (frmChartAction.ModalResult = mrOk) then
      begin
        FDisplayPanel.SelectorsPanel.HideNoDataMessage;
        FDataManager.RemoveTemporaryCurrentSeries;
        FDataManager.RemoveTemporaryCurrentChart;
        FDataManager.RestoreCurrentSeries;
        LViewName := Trim(frmChartAction.edtTop.Text);
        if FDataManager.DeleteView(LViewName) then
          if FDisplayPanel.SelectorsPanel.DeleteView(FDisplayPanel.SelectorsPanel.CurrentView) then
          begin
            FDisplayPanel.SelectorsPanel.SelectCurrentView(nil);
            FDisplayPanel.SelectorsPanel.SelectCurrentChart(nil);
            FDisplayPanel.SelectorsPanel.SelectCurrentSeries(nil);
          end;

        RestoreMenuStates;
        SetupSystemMenuState;
        if Assigned(FDataManager.CurrentViewData) then
          MenuItemManager.SetMenuSaveView(msEnable)
        else
          MenuItemManager.SetMenuSaveView(msDisable);
      end;
    finally
      FreeAndNil(frmChartAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSheet.SetViewHasMetaData;
const OPNAME = 'TTimeSeriesComparitorSheet.SetViewHasMetaData';
var
  lHasMetaData   : boolean;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (Assigned(FDataManager.CurrentViewData)) then
    begin
      lFieldIndex := '';
      lKeyValues  := 'Model='           + QuotedStr(FDataManager.CurrentViewData.ModelCode) +
                     ',StudyAreaName='  + QuotedStr(FDataManager.CurrentViewData.StudyAreaCode) +
                     ',SubArea='        + QuotedStr(FDataManager.CurrentViewData.SubAreaCode) +
                     ',ViewName='       + QuotedStr(FDataManager.CurrentViewData.ViewName);
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('TSCViewName');
      lHasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
      FDisplayPanel.SelectorsPanel.ViewComboBox.HasMetaData := lHasMetaData;
      FDisplayPanel.SelectorsPanel.ViewComboBox.Repaint;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorSheet.OnSelectView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorSheet.OnSelectView';
begin
  Result := False;
  try
    if StudyIsChanging then Exit;

    if FDisplayPanel.SelectorsPanel.ZoomCurrchartOnly.Checked then
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := True
    else
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := False;

    FDisplayPanel.SelectorsPanel.RepopulateChartComboBox;
    FDisplayPanel.SelectorsPanel.ViewComboBox.HasMetaData := FALSE;
    FDisplayPanel.SelectorsPanel.ViewComboBox.Repaint;

    if not Assigned(AView) then
    begin
      FDisplayPanel.SelectorsPanel.SelectCurrentChart(nil);
      RestoreMenuStates;
      SetupSystemMenuState;
      Result := True;
      Exit;
    end;

    if Assigned(FDataManager.CurrentViewData) and (FDataManager.CurrentViewData = AView) then
    begin
      FDisplayPanel.SelectorsPanel.SelectCurrentChart(FDataManager.CurrentChartData);
      FDisplayPanel.ChartPanel.SynchroniseChartsXAxis;
      RestoreMenuStates;
      SetupSystemMenuState;
      SetViewHasMetaData;
      Result := True;
      Exit;
    end;

    FDisplayPanel.SelectorsPanel.HideNoDataMessage;
    FDataManager.RemoveTemporaryCurrentSeries;
    FDataManager.RemoveTemporaryCurrentChart;
    FDataManager.RestoreCurrentSeries;
    MenuItemManager.ToggleViewEnabledState(False);
    MenuItemManager.ToggleChartEnabledState(False);
    MenuItemManager.ToggleSeriesEnabledState(False);
    Result := FDataManager.SelectView(AView);
    if Result then
    begin

      FDisplayPanel.SelectorsPanel.SelectCurrentChart(FDataManager.CurrentChartData);
      FDisplayPanel.ChartPanel.SynchroniseChartsXAxis;
      RestoreMenuStates;
      SetupSystemMenuState;
      SetViewHasMetaData;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorSheet.SetChartHasMetaData;
const OPNAME = 'TTimeSeriesComparitorSheet.SetChartHasMetaData';
var
  lHasMetaData   : boolean;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (Assigned(FDataManager.CurrentChartData)) then
    begin
      lFieldIndex := '';
      lKeyValues  := 'Model='           + QuotedStr(FDataManager.CurrentChartData.ModelCode) +
                     ',StudyAreaName='  + QuotedStr(FDataManager.CurrentChartData.StudyAreaCode) +
                     ',SubArea='        + QuotedStr(FDataManager.CurrentChartData.SubAreaCode) +
                     ',ChartName='      + QuotedStr(FDataManager.CurrentChartData.ChartName);
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('TSCChartName');
      lHasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
      FDisplayPanel.SelectorsPanel.ChartComboBox.HasMetaData := lHasMetaData;
      FDisplayPanel.SelectorsPanel.ChartComboBox.Repaint;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorSheet.OnSelectChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorSheet.OnSelectChart';
begin
  Result := False;
  try
    if StudyIsChanging then Exit;

    if FDisplayPanel.SelectorsPanel.ZoomCurrchartOnly.Checked then
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := True
    else
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := False;

    FDisplayPanel.SelectorsPanel.RepopulateSeriesComboBox;
    FDisplayPanel.SelectorsPanel.ChartComboBox.HasMetaData := FALSE;
    FDisplayPanel.SelectorsPanel.ChartComboBox.Repaint;
    if not Assigned(AChart) then
    begin
      FDisplayPanel.SelectorsPanel.SelectCurrentSeries(nil);
      RestoreMenuStates;
      SetupSystemMenuState;
      Result := True;
      Exit;
    end;

    if Assigned(FDataManager.CurrentChartData) and (FDataManager.CurrentChartData = AChart) then
    begin
      FDisplayPanel.SelectorsPanel.SelectCurrentSeries(AChart.CurrentSeries);
      FDisplayPanel.ChartPanel.SynchroniseChartsXAxis;
      RestoreMenuStates;
      SetupSystemMenuState;
      SetChartHasMetaData;
      Result := True;
      Exit;
    end;

    FDisplayPanel.SelectorsPanel.HideNoDataMessage;
    FDataManager.RemoveTemporaryCurrentSeries;
    FDataManager.RemoveTemporaryCurrentChart;
    FDataManager.RestoreCurrentSeries;
    MenuItemManager.ToggleChartEnabledState(False);
    MenuItemManager.ToggleSeriesEnabledState(False);
    Result := FDataManager.SelectChart(AChart);
    if Result then
    begin
      FDisplayPanel.SelectorsPanel.SelectCurrentSeries(AChart.CurrentSeries);
      FDisplayPanel.ChartPanel.SynchroniseChartsXAxis;
      RestoreMenuStates;
      SetupSystemMenuState;
      SetChartHasMetaData;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorSheet.FindTreeNode(ASeries: TTimeSeriesComparitorSeries): TTreeNode;
const
  OPNAME = 'TTimeSeriesComparitorSheet.FindTreeNode';
var
  LViewData: TViewDataTreeNodeData;
  LNode: TTreeNode;
  LIndex: integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((not Assigned(Result)) and (LIndex < Self.NodeCount)) do
    begin
      LNode := Self.NodeByIndex[LIndex];
      if (LNode.Data <> nil) then
      begin
        LViewData := TViewDataTreeNodeData(LNode.Data);
        if ((ASeries.ModelCode = FAppModules.StudyArea.ModelCode) and
          (ASeries.StudyAreaCode = FAppModules.StudyArea.StudyAreaCode) and
          (ASeries.SubAreaCode = FAppModules.StudyArea.SubAreaCode) and
          (ASeries.ScenarioCode = FAppModules.StudyArea.ScenarioCode) and
          (ASeries.ChartName = FDataManager.CurrentChartData.ChartName) and
          (ASeries.SeriesName = LViewData.ViewDataNode.OverrideCaption) and
          (ASeries.ParentID = LViewData.ViewDataNode.ParentID) and
          (ASeries.ViewID = LViewData.ViewDataNode.ViewID) and
          (ASeries.TopParentID = LViewData.ViewDataNode.TopParentID)) then
          Result := LNode;
      end;
      LIndex := LIndex + 1;
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TTimeSeriesComparitorSheet.OnSelectSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSheet.OnSelectSeries';
var
  LNode: TTreeNode;
begin
  Result := False;
  try
    if StudyIsChanging then Exit;

    if FDisplayPanel.SelectorsPanel.ZoomCurrchartOnly.Checked then
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := True
    else
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := False;
      
    if (not FSystemSelect) then
    begin
      FSystemSelect := TRUE;
      if Assigned(ASeries) then
      begin
        LNode := FindTreeNode(ASeries);
        if Assigned(LNode) then
          FTreeview.Selected := LNode
        else
          FTreeview.Selected := nil;
      end
      else
        FTreeview.Selected := nil;
      FSystemSelect := FALSE;

      if not Assigned(ASeries) then
      begin
        FDataManager.RestoreCurrentSeries;
        RestoreMenuStates;
        SetupSystemMenuState;
        Result := True;
        Exit;
      end;

      if Assigned(FDataManager.CurrentSeriesData) and (FDataManager.CurrentSeriesData = ASeries) then
      begin
        FDataManager.HighlightCurrentSeries;
        RestoreMenuStates;
        SetupSystemMenuState;
        if(FTreeview.Selected <> nil) then
          CheckboxClickOrTreeNodeChange(TViewDataTreeNodeData(FTreeview.Selected.Data).ViewDataNode.ElementID);
        Result := True;
        Exit;
      end;

      FDisplayPanel.SelectorsPanel.HideNoDataMessage;
      FDataManager.RemoveTemporaryCurrentSeries;
      FDataManager.RestoreCurrentSeries;
      MenuItemManager.ToggleSeriesEnabledState(False);
      Result := FDataManager.SelectSeries(ASeries);
      if Result then
      begin
        FDisplayPanel.ChartPanel.SynchroniseChartsXAxis;
        RestoreMenuStates;
        SetupSystemMenuState;
      end;
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.OnValidateCreateChart(Sender: TObject);
const
  OPNAME = 'TTimeSeriesComparitorSheet.OnValidateCreateChart';
begin
  try
    if (Trim(frmChartAction.edtTop.Text) = '') then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.EmptyChartName'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtTop;
      Exit;
    end;

    if (FDataManager.ChartNameExists(Trim(frmChartAction.edtTop.Text))) then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.ChartNameAlreadyExist'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtTop;
      Exit;
    end;
    if (FDataManager.ChartComponentNameExists(Trim(frmChartAction.edtTop.Text))) then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.ChartNameAlreadyExist'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtTop;
      Exit;
    end;
    if (Trim(frmChartAction.edtBottom.Text) = '') then
      frmChartAction.edtBottom.Text := frmChartAction.edtTop.Text;
    frmChartAction.ModalResult := mrOk;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.OnValidateCreateView(Sender: TObject);
const
  OPNAME = 'TTimeSeriesComparitorSheet.OnValidateCreateView';
begin
  try
    if (Trim(frmChartAction.edtTop.Text) = '') then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.EmptyViewName'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtTop;
      Exit;
    end;

    if (FDataManager.ViewNameExists(Trim(frmChartAction.edtTop.Text))) then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.ViewNameAlreadyExist'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtTop;
      Exit;
    end;
    frmChartAction.ModalResult := mrOk;

  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.OnValidateDeleteChart(Sender: TObject);
const
  OPNAME = 'TTimeSeriesComparitorSheet.OnValidateDeleteChart';
begin
  try
    frmChartAction.ModalResult := mrOk;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.OnValidateDeleteView(Sender: TObject);
const
  OPNAME = 'TTimeSeriesComparitorSheet.OnValidateDeleteView';
begin
  try
    frmChartAction.ModalResult := mrOk;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.OnValidateRenameChart(Sender: TObject);
const
  OPNAME = 'TTimeSeriesComparitorSheet.OnValidateRenameChart';
begin
  try
    if (Trim(frmChartAction.edtBottom.Text) = '') then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.EmptyChartName'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtBottom;
      Exit;
    end;
    if (FDataManager.ChartNameExists(Trim(frmChartAction.edtBottom.Text))) then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.ChartNameAlreadyExist'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtBottom;
      Exit;
    end;
    if (FDataManager.ChartComponentNameExists(Trim(frmChartAction.edtBottom.Text))) then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.ChartNameAlreadyExist'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtBottom;
      Exit;
    end;
    frmChartAction.ModalResult := mrOk;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.OnValidateRenameView(Sender: TObject);
const
  OPNAME = 'TTimeSeriesComparitorSheet.OnValidateRenameView';
begin
  try
    if (Trim(frmChartAction.edtBottom.Text) = '') then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.EmptyViewName'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtBottom;
      Exit;
    end;
    if (FDataManager.ViewNameExists(Trim(frmChartAction.edtBottom.Text))) then
    begin
      MessageDlg(FAppModules.Language.GetString('TimeSeriesComparitor.ViewNameAlreadyExist'), mtError, [mbOK], 0);
      frmChartAction.ActiveControl := frmChartAction.edtBottom;
      Exit;
    end;
    frmChartAction.ModalResult := mrOk;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.SetupSystemMenuState;
const
  OPNAME = 'TTimeSeriesComparitorSheet.SetupSystemMenuState';
begin
  try
    FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(CanCopyToClipboard);
    FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
    FAppModules.PrintManager.SetPrintEnabled(CanPrint);
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.RestoreMenuStates;
const
  OPNAME = 'TTimeSeriesComparitorSheet.RestoreMenuStates';
begin
  try
    MenuItemManager.ToggleAllEnabledState(False);
    if Assigned(FDataManager.CurrentViewData) then
    begin
      MenuItemManager.ToggleViewEnabledState(True);
      if (FDisplayPanel.ChartPanel.ChartCount > 0) then
      begin
        MenuItemManager.SetMenuChartName(msEnable);
        MenuItemManager.SetMenuShowChartLegendDialog(msEnable);
        MenuItemManager.SetTSCToggleIndividualSeries(msEnable);
      end;

      if Assigned(FDataManager.CurrentChartData) then
      begin
        MenuItemManager.ToggleChartEnabledState(True);
        if Assigned(FDataManager.CurrentSeriesData) then
        begin
          MenuItemManager.ToggleSeriesEnabledState(True);
          if FDataManager.CurrentSeriesData.AddedToChart then
            MenuItemManager.SetMenuAddSeries(msDisable)
          else
            MenuItemManager.SetMenuRemoveSeries(msDisable);
        end;
      end;
    end;
    MenuItemManager.SetMenuCreateChart(msEnable);
    MenuItemManager.SetMenuCreateView(msEnable);
  if FDisplayPanel.SelectorsPanel.SeriesComboBox.ItemIndex = 0 then
  begin
    MenuItemManager.SetMenuRemoveSeries(msDisable);
    MenuItemManager.SetMenuSeriesColor(msDisable);
    MenuItemManager.SetMenuAddSeries(msDisable);
  end
  else
    FDataManager.HighlightCurrentSeries;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.SetSeriesColor;
const
  OPNAME = 'TTimeSeriesComparitorSheet.SetSeriesColor';
var
  LColorSelector: TColorDialog;
begin
  try
    if Assigned(FDataManager.CurrentSeriesData) then
    begin
      LColorSelector := TColorDialog.Create(nil);
      try
        if LColorSelector.Execute then
        begin
          FDataManager.CurrentSeriesData.Color := LColorSelector.Color;
          FDataManager.Changed := True;
          MenuItemManager.SetMenuSaveView(msEnable);
        end;
      finally
        FreeAndNil(LColorSelector);
      end;
    end;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.TSCToggleIndividualSeries;
const OPNAME = 'TTimeSeriesComparitorSheet.TSCToggleIndividualSeries';
begin
  try
    if Assigned(FDataManager.CurrentViewData) then
      FDataManager.CurrentViewData.Normalised := not FDataManager.CurrentViewData.Normalised;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.ShowChartLegendDialog;
const OPNAME = 'TTimeSeriesComparitorSheet.ShowChartLegendDialog';
var
  LLegendAlignment : TLegendAlignment;
  LLegendVisible   : boolean;
begin
  try
    if Assigned(FDataManager.CurrentChartData) then
    begin
      frmChartLegendDialog          := TTimeSeriesComparitorChartLegendDialog.CreateWithoutDFM(nil, FAppModules);
      frmChartLegendDialog.Width    := Self.Width div 3;
      frmChartLegendDialog.Height   := Self.Height div 2;
      frmChartLegendDialog.Position := poScreenCenter;
      frmChartLegendDialog.Initialise;
      frmChartLegendDialog.LanguageHasChanged;
      FDataManager.CurrentChartData.GetChartLegendProperties(LLegendAlignment, LLegendVisible);
      frmChartLegendDialog.LegendAlignment := LLegendAlignment;
      frmChartLegendDialog.LegendVisible   := LLegendVisible;
      if FDataManager.CurrentSeriesData <> nil then
        frmChartLegendDialog.SeriesName := FDataManager.CurrentSeriesData.SeriesName;

      if Assigned(frmChartLegendDialog) then
        frmChartLegendDialog.SetEnabled((FAppModules.User.UserRights in CUR_EditData) and
                                        (FAppModules.StudyArea <> nil ) and
                                        (not (FAppModules.StudyArea.ScenarioLocked)));
      frmChartLegendDialog.ShowModal;
      if (frmChartLegendDialog.ModalResult = mrOk) then
      begin
        LLegendAlignment := frmChartLegendDialog.LegendAlignment;
        LLegendVisible   := frmChartLegendDialog.LegendVisible;

        if FDataManager.CurrentSeriesData <> nil then
        begin
          FDataManager.CurrentSeriesData.SeriesName := frmChartLegendDialog.SeriesName;
          FDataManager.CurrentChartData.SetChartLegend ( LLegendAlignment, LLegendVisible,
                                                         frmChartLegendDialog.SeriesName,
                                                         FDataManager.CurrentSeriesData);


        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSheet.TabHasChanged;
const
  OPNAME = 'TTimeSeriesComparitorSheet.TabHasChanged';
begin
  try
    if (FAppModules.StudyArea.ModelCode = CRainfall) then
    begin
//      if FFirstTimeActive then
//      begin
        ProcessStudyHasChanged;
//      end;
    end;
    inherited;
//    FFirstTimeActive := False;
  except on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TTimeSeriesComparitorSheet.OnChartClick(Sender: TObject);
const
  OPNAME = 'TTimeSeriesComparitorSheet.OnChartClick';
var
  lChartName : string;
  lIndex     : integer;
  lCbxName   : string;
  lFound     : boolean;
begin
  try
    if FDisplayPanel.SelectorsPanel.ZoomCurrchartOnly.Checked then
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := True
    else
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := False;

    if (Sender is TAbstractChart) then
    begin
      LChartName := TAbstractChart(Sender).Name;
      lFound := FALSE;
      lIndex := 0;
      while ((NOT lFound) AND (lIndex < FDisplayPanel.SelectorsPanel.ChartComboBox.Items.Count)) do
      begin
        lCbxName := FDisplayPanel.SelectorsPanel.ChartComboBox.Items[lIndex];
        if (FDataManager.ChartDataManager.Strip(lCbxName) = lChartName) then
          lFound := TRUE
        else
          lIndex := lIndex + 1;
      end;
      if (lFound) then
      begin
        FDisplayPanel.SelectorsPanel.ChartComboBox.ItemIndex := lIndex;
        FDisplayPanel.SelectorsPanel.ChartComboBox.OnSelect(FDisplayPanel.SelectorsPanel.ChartComboBox);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSheet.OnChartSeriesClick(Sender: TCustomChart; Series: TChartSeries;
                                                        ValueIndex: Integer; Button: TMouseButton;
                                                        Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TTimeSeriesComparitorSheet.OnChartSeriesClick';
begin
  try
    if FDisplayPanel.SelectorsPanel.ZoomCurrchartOnly.Checked then
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := True
    else
      FDisplayPanel.ChartPanel.ZoomActiveChartOnly := False;

    FDisplayPanel.SelectorsPanel.RepopulateSeriesComboBox;
    FDisplayPanel.SelectorsPanel.SeriesComboBox.ItemIndex := (Sender.SeriesList.IndexOf(Series) + 1);
    FDisplayPanel.SelectorsPanel.ComboBoxSelect(FDisplayPanel.SelectorsPanel.SeriesComboBox);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSheet.OnChartMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TTimeSeriesComparitorSheet.OnChartMouseMove';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSheet.ProcessMetaDataEvent : boolean;
const OPNAME = 'TTimeSeriesComparitorSheet.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  Result := FALSE;
  try
    lFieldIndex := '';
    if (FDisplayPanel.SelectorsPanel.ActiveComboBox = FDisplayPanel.SelectorsPanel.ViewComboBox) then
    begin
      lFieldIndex := '';
      lKeyValues  := 'Model='           + QuotedStr(FDataManager.CurrentViewData.ModelCode) +
                     ',StudyAreaName='  + QuotedStr(FDataManager.CurrentViewData.StudyAreaCode) +
                     ',SubArea='        + QuotedStr(FDataManager.CurrentViewData.SubAreaCode) +
                     ',ViewName='       + QuotedStr(FDataManager.CurrentViewData.ViewName);
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('TSCViewName');
      FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
      SetViewHasMetaData;
      Result := TRUE;
    end
    else
    if (FDisplayPanel.SelectorsPanel.ActiveComboBox = FDisplayPanel.SelectorsPanel.ChartComboBox) then
    begin
      lFieldIndex := '';
      lKeyValues  := 'Model='           + QuotedStr(FDataManager.CurrentChartData.ModelCode) +
                     ',StudyAreaName='  + QuotedStr(FDataManager.CurrentChartData.StudyAreaCode) +
                     ',SubArea='        + QuotedStr(FDataManager.CurrentChartData.SubAreaCode) +
                     ',ChartName='      + QuotedStr(FDataManager.CurrentChartData.ChartName);
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('TSCChartName');
      FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
      SetChartHasMetaData;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSheet.EntityDescription (AFieldPropName : string;
                                                       AKeyValues     : string;
                                                       AFieldIndex    : string) : string;
const OPNAME = 'TTimeSeriesComparitorSheet.EntityDescription';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := '';
  try
    if (AFieldPropName <> '') then
    begin
      lFieldProperty := FAppModules.FieldProperties.FieldProperty(Trim(AFieldPropName));
      if (lFieldProperty <> nil) then
      begin
        if (lFieldProperty.DataClassName = 'TTimeSeriesComparitorChart') then
          Result := FDataManager.CurrentChartData.ChartName
        else
        if (lFieldProperty.DataClassName = 'TTimeSeriesComparitorView') then
          Result := FDataManager.CurrentViewData.ViewName;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TGraphTreeItemDataObject }

constructor TGraphTreeItemDataObject.Create(AAppModules: TAppModules);
const OPNAME = 'TGraphTreeItemDataObject.Create';
begin
  try
    inherited Create(AAppModules);
    FLeftAxisCaption      := '';
    FLeftAxisFormat       := '';
    FLeftAxisUnits        := '';
    FBottomAxisCaption    := '';
    FBottomAxisFormat     := '';
    FBottomAxisUnits      := '';
    FGraphHint            := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGraphTreeItemDataObject.PopulateTreeNodeDataObject(AViewID: string);
const OPNAME = 'TGraphTreeItemDataObject.PopulateTreeNodeDataObject';
var LFieldPropertyX, LFieldPropertyY, LFieldPropertyGraph: TAbstractFieldProperty;
begin
  try
    if Assigned(FAppModules.FieldProperties()) then
    begin
      LFieldPropertyX     := FAppModules.FieldProperties.FieldProperty(AViewID + 'X');
      LFieldPropertyY     := FAppModules.FieldProperties.FieldProperty(AViewID + 'Y');
      LFieldPropertyGraph := FAppModules.FieldProperties.FieldProperty(AViewID + 'G');
      if Assigned(LFieldPropertyGraph) then
      begin
        GraphHint := LFieldPropertyGraph.FieldDescription;
      end;
      if Assigned(LFieldPropertyX) then
      begin
        BottomAxisCaption := LFieldPropertyX.FieldDescription;
        BottomAxisUnits   := LFieldPropertyX.FieldUnits;
        BottomAxisFormat  := LFieldPropertyX.FormatStringGraph;
      end;
      if Assigned(LFieldPropertyY) then
      begin
        LeftAxisCaption := LFieldPropertyY.FieldDescription;
        LeftAxisUnits   := LFieldPropertyY.FieldUnits;
        LeftAxisFormat  := LFieldPropertyY.FormatStringGraph;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSheet.SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset);
const OPNAME = 'TTimeSeriesComparitorSheet.SetCurrentDataSet';
begin
  inherited;
  try
    if Assigned(FDataManager) then
    begin
      FDataManager.PopulatingAgent.CurrentDataset := ACurrentDataset;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSheet.OnCheckBoxClick(ASender : TObject);
const OPNAME = 'TTimeSeriesComparitorSheet.OnCheckBoxClick';
var
  LNode : TTreeNode;
begin
  try
    LNode := FTreeView.Selected;
    if(LNode <> nil) then
      CheckboxClickOrTreeNodeChange(TViewDataTreeNodeData(LNode.Data).ViewDataNode.ElementID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSheet.CheckboxClickOrTreeNodeChange(AReservoirNodeNumber : integer);
const OPNAME = 'TTimeSeriesComparitorSheet.CheckboxClickOrTreeNodeChange';
var
  LCurrentView : TTimeSeriesComparitorView;
  LIndex       : integer;
  LChart       : TTimeSeriesComparitorChart;
begin
  try
    LCurrentView := FDisplayPanel.SelectorsPanel.CurrentView;
    if(LCurrentView <> nil) then
    begin
      for LIndex := 0 to LCurrentView.ChartList.ChartCount - 1 do
      begin                             
        LChart := LCurrentView.ChartList.ChartByIndex[LIndex];
        if(LChart <> nil) then
        begin
          LChart.ShowLineSeries := FDisplayPanel.SelectorsPanel.PlotReservoirFixedValues.Checked;
          LChart.SelectAndShowSeriesBySeriesName(AReservoirNodeNumber);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

