//
//
//  UNIT      : Contains TResultYRCSheet Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/08/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UResultYRCSheet;

interface

uses
  Types,
  VCL.Controls,
  Classes,
  VCLTee.TeExport,
  VCLTee.TeeProcs,
  VCLTee.Chart,
  VCLTee.Series,
  VCL.StdCtrls,
  VCL.ComCtrls,
  VCL.Menus,
  VCL.Buttons,
  VCL.Dialogs,
  VCLTee.TeEngine,
  UYRCSeries,
  UViewDataItem,
  UViewDataList,
  UAbstractObject,
  UAbstractYRCData,
  UAbstractComponent,
  UDataComponent,
  UYRCSelectorsPanel,
  UMenuItemManager,
  UYRCMenuItemManager,
  UAbstractModelData,
  UAbstractFileNamesObject,
  UAbstractYRCSheet;

type
  TResultYRCSheet = class(TAbstractYRCSheet)
  protected
    FHintWin            : THintWindow;
    FSumOutFileStatus   : TLabel;
    FSumOutFileMessg    : string;
    FYRCSelectorsPanel  : TYRCSelectorsPanel;
    FMenuItemManager    : TYRCMenuItemManager;
    FClickedPart        : TChartClickedPart;
    FBusyPopulating     : boolean;
    FCreateAsComponent  : boolean;
    FOldXPosition               : Single;
    FOldYPosition               : Single;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetMenuItemManager: TMenuItemManager;override;
    function GetToolBar: TAbstractToolBar; override;
    function YRCGraphDataObject:TAbstractYRCGraphDataObject;
    procedure DoShow; override;

    function DeleteChart: boolean;
    function CreateChart: boolean;
    function WasStochasticRan: boolean;
    function ChartEditingAllowed : boolean;
    function DisplayYRCSettings(AMessage: string): boolean;
    procedure PopulateTargetDraftSelector;
    procedure RefreshTargetDraftFormula;
    procedure ShowPopupMenu;
    procedure OnChartOnDblClick(Sender: TObject);
    procedure StopSeriesEditing;
    procedure RefreshLegend(AInPlaneMode : boolean);
    procedure OnGetDisplayLegendText(Sender: TCustomAxisPanel; LegendStyle: TLegendStyle; Index: Integer; var LegendText: String);
    procedure OnGetSeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
    procedure OnChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OnChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OnChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OnChartSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer;
                                 Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnSeriesPointerClick(Sender: TCustomSeries; ValueIndex: LongInt; X, Y: Integer);
    function SetZoomIndex(AIndex: integer): boolean;
    procedure OnZoomChangeRequested(Sender: TObject);
    procedure OnZoomPerformed(Sender: TObject);
    procedure OnUndoZoomPerformed(Sender: TObject);
    procedure OnShowTargetDraftsSelectionChanged(Sender : TObject);
    procedure OnCheckBoxChanged(ASender: TObject);
    procedure OnAssuranceIntervalSelectorClick(Sender: TObject);
    procedure OnSetChartMaxYValue(Sender: TObject);
    procedure SetChartMode(AMode:TChartMode);
    procedure SetChartEditMode(AMode:TChartEditMode);
    procedure OnPlaneSelectionChange(ASelection: integer);
    procedure OnPlottingBaseSelectionChange(ASelection: integer);
    procedure OnTargetDraftSelectionChange(ASelectionIndeces: TIntegerArray);
    procedure UpdateZoomIndex;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules;ACreateAsComponent: boolean); reintroduce; virtual;
    function StudyHasChanged: boolean;override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue, ANewValue: string): boolean; override;
    procedure TabHasChanged; override;

    procedure OnChartEditPopupClick(Sender : TObject);
    procedure OnEditYValue(Sender : TObject);
    procedure OnForceCurveThrough100(Sender : TObject);
    procedure OnPopup(Sender : TObject);
    procedure TogglePlaneMode;override;
    procedure ToggleChartMode;override;
    procedure ToggleCurveManipulation;override;
    procedure ResetChartData;override;
    procedure LoadChart;override;
    function  ApplySelectedPlane: boolean;
    function  ShowPlaneSelection: boolean;
    function  ManiplateTargetDraftDeterministic: boolean;
    function  ManiplateTargetDraftRegression: boolean;
    procedure SetMenuVisible(AVisible: boolean); override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DeleteSeletedTargetDraft;
    function LoadCoefficientFile:boolean;
    function  ProcessMetaDataEvent : boolean; override;
    function  EntityDescription (AFieldPropName : string;AKeyValues : string;AFieldIndex: string) : string; override;
    property MenuItemManager: TMenuItemManager read GetMenuItemManager;
    property ToolBar : TAbstractToolBar Read GetToolBar;
  end;

implementation

uses
  Math,
  SysUtils,
  VCL.Graphics,
  Windows,
  VCL.Clipbrd,
  VCL.Printers,
  UYRCChart,
  UFileNames,
  UConstants,
  UUtilities,
  VoaimsCom_TLB,
  UMainMenuEventType,
  UYRCModelDataObject,
  UGenericModelLinkClasses,
  UTargetDraftSelector,
  UErrorHandlingOperations,
  UTreeViewTabSheet;

procedure TResultYRCSheet.CreateMemberObjects;
const OPNAME = 'TResultYRCSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FBusyPopulating := False;
    FTabCaptionKey := 'YRC';
    //FViewTypeConstant := 'YRC';
    Self.BevelInner := bvLowered;
    //Self.Color := clLime;
    FMenuItemManager := nil;
    FClickedPart.Part            :=  cpNone;
    if not FCreateAsComponent then
      FMenuItemManager             := TYRCMenuItemManager.Create(FAppModules);
    FSumOutFileStatus            := TLabel.Create(Self);
    FSumOutFileStatus.Parent     := Self;
    FSumOutFileStatus.Align      := alBottom;
    FSumOutFileStatus.Visible    := False;
    FSumOutFileStatus.Font.Color := clRed;
    FSumOutFileMessg             := '';

    FYRCSelectorsPanel        := TYRCSelectorsPanel.Create(Self,FAppModules);
    FYRCSelectorsPanel.Parent := Self;
    FYRCSelectorsPanel.Align  := alTop;
    FYRCSelectorsPanel.Height := 64;
    FYRCSelectorsPanel.ShowSelector(False);
    FYRCSelectorsPanel.ShowTargetDraftSelector(False);
    FYRCSelectorsPanel.OnTargetDraftSelectionChange := Self.OnTargetDraftSelectionChange;
    FYRCSelectorsPanel.OnPlaneSelectorChange := Self.OnPlaneSelectionChange;
    FYRCSelectorsPanel.OnPlottingBaseSelectorChange := Self.OnPlottingBaseSelectionChange;
    FYRCSelectorsPanel.AssuranceIntervalSelector.OnClick := self.OnAssuranceIntervalSelectorClick;
    FYRCSelectorsPanel.ChartZoom.OnSelect := OnZoomChangeRequested;
    FYRCSelectorsPanel.btnSetYMax.OnClick := OnSetChartMaxYValue;
    FYRCSelectorsPanel.ChkBoxLabelOff.OnClick := OnCheckBoxChanged;
    FYRCSelectorsPanel.ChkBoxShowXY.OnClick := OnCheckBoxChanged;
    //FYRCSelectorsPanel.TargetDraftSelector.AssuranceIntervalSelector.
    //FYRCSelectorsPanel.ShowTargetDrafts.ItemIndex := 0;
    FYRCSelectorsPanel.ShowTargetDrafts.Enabled := False;
    FYRCSelectorsPanel.ShowTargetDrafts.OnSelect := OnShowTargetDraftsSelectionChanged;
    FYRCSelectorsPanel.Visible := not FCreateAsComponent;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.DestroyMemberObjects;
const OPNAME = 'TResultYRCSheet.DestroyMemberObjects';
begin
  try
    if (FMenuItemManager <> nil) then
      FMenuItemManager.Free;
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TResultYRCSheet.LanguageHasChanged: boolean;
const OPNAME = 'TResultYRCSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FYRCSelectorsPanel.LanguageHasChanged;
    if Assigned(FChart) then
    Result := Result and FChart.LanguageHasChanged;
    if Assigned(FMenuItemManager) then
    Result := Result and FMenuItemManager.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.GetMenuItemManager: TMenuItemManager;
const OPNAME = 'TResultYRCSheet: GetMenuItemManager';
begin
  Result := nil;
  try
    Result := FMenuItemManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TResultYRCSheet.GetToolBar';
begin
  Result := nil;
  try
    if (FMenuItemManager <> nil) then
      Result :=  FMenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.YRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TResultYRCSheet.YRCGraphDataObject';
begin
  Result := nil;
  try
    Result := TYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.PopulateTargetDraftSelector;
const OPNAME = 'TResultYRCSheet.PopulateTargetDraftSelector';
var
 LPlane : TAbstractYRCPlane;
 LTargetDrafts: array of Double;
 LIndex: integer;
begin
  try
    if (YRCGraphDataObject <> nil)  and (YRCGraphDataObject.PlanesCount > 0) then
    begin
      LPlane := YRCGraphDataObject.SelectedPlane;
      if Assigned(LPlane) then
      begin
        SetLength(LTargetDrafts,LPlane.TargetDraftCount);
        for LIndex := 0 to LPlane.TargetDraftCount - 1 do
          LTargetDrafts[LIndex] := LPlane.TargetDraft[LIndex].TargetDraftYValue;
        FYRCSelectorsPanel.PopulateTargetDraft(LTargetDrafts);
        if not FBusyPopulating then
          FYRCSelectorsPanel.TargetDraftSelector.SelectByIndex(0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.ResetChartData;
const OPNAME = 'TResultYRCSheet.ResetChartData';
Var
  LTargetDraft : TAbstractYRCTargetDraft;
begin
  inherited;
  try
    if Assigned(FChart) then
    begin
      if not FBusyPopulating then
        YRCGraphDataObject.ResetSelectedTargetDraftPoints;
      FChart.ChartDataHasBeenReset;

      LTargetDraft := YRCGraphDataObject.SelectedTargetDraft;
      if(LTargetDraft <> nil) and (LTargetDraft.YValueAt100 <> NullFloat) then
        FChart.YRCPopupMenu.ReturnValue := FormatFloat('#0.00',LTargetDraft.YValueAt100)
      else
        FChart.YRCPopupMenu.ReturnValue := '?';
      FChart.YRCPopupMenu.OnYValueChange(FChart.YRCPopupMenu.ReturnValue);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.LoadChart;
const OPNAME = 'TResultYRCSheet.LoadChart';
begin
  inherited;
  try
    DeleteChart;
    if Assigned(FChart) then
    begin
      FBusyPopulating := True;
      try
        if (Trim(YRCGraphDataObject.ErrorMsg) = '') and (not WasStochasticRan) then
          YRCGraphDataObject.ErrorMsg := 'CaptionHistoric';
        if(Trim(YRCGraphDataObject.ErrorMsg) <> '')  then
        begin
          DisplayYRCSettings(YRCGraphDataObject.ErrorMsg);
          FYRCSelectorsPanel.Height := 125;
          FYRCSelectorsPanel.ErrorMesg.Visible := True;
        end else begin
          DisplayYRCSettings('');
          FYRCSelectorsPanel.Height := 64;
          CreateChart;
          FYRCSelectorsPanel.TargetDraftSelectorMode := smSingleSelect;
          FYRCSelectorsPanel.SelectPlane(YRCGraphDataObject.PeriodLength);
          FYRCSelectorsPanel.SelectPlottingBase(YRCGraphDataObject.PlottingBase);
          FYRCSelectorsPanel.ShowTargetDrafts.ItemIndex := ord(YRCGraphDataObject.YRCChartProperties.ShowTargetDrafts);
          OnShowTargetDraftsSelectionChanged(FYRCSelectorsPanel.ShowTargetDrafts);
        end;
      finally
        FBusyPopulating := False;
      end;
      SetChartMode(YRCGraphDataObject.YRCChartProperties.ChartMode);
      SetChartEditMode(YRCGraphDataObject.YRCChartProperties.ChartEditMode);
      SetZoomIndex(YRCGraphDataObject.YRCChartProperties.ZoomIndex);
      FYRCSelectorsPanel.TargetDraftSelector.SelectByIndex(YRCGraphDataObject.SelectedTargetDraftIndex+1);
    end;
  except on E : Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.StudyHasChanged: boolean;
const OPNAME = 'TResultYRCSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    LoadChart;
    DisplayYRCSettings('CaptionNormal');
    if Assigned(FChart) then
      FChart.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultYRCSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TResultYRCSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AContext = sdccDelete) and (AFieldName = 'YRCData') then
    begin
      DeleteChart;
      FYRCSelectorsPanel.Height := 125;
      DisplayYRCSettings('CaptionNormal');
    end;
    if(AContext = sdccSaveData) and (AFieldName = 'YRCData') then
       if (FMenuItemManager <> nil) then
         FMenuItemManager.SetMenuDeleteChart(msEnable);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.DeleteChart: boolean;
const OPNAME = 'TResultYRCSheet.DeleteChart';
begin
  Result := False;
  try
    if (FMenuItemManager <> nil) then
    begin
      FMenuItemManager.SetLoadCoefficientFile(msDisable);
      FMenuItemManager.SetMenuTogglePlaneDown(False);
      FMenuItemManager.SetMenuToggleCurveManipulationDown(False);
      //FMenuItemManager.SetMenuToggleChartDown(False);
      FMenuItemManager.SetAll(msDisable);
      FMenuItemManager.SetLoadFromDB(msEnable);
      FMenuItemManager.SetLoadFromFile(msEnable);
    end;

    FYRCSelectorsPanel.ShowSelector(False);
    FYRCSelectorsPanel.ShowTargetDraftSelector(False);
    FYRCSelectorsPanel.ChkBoxLabelOff.Checked := False;
    FYRCSelectorsPanel.ChkBoxShowXY.Checked := False;

    //FYRCSelectorsPanel.ErrorMesg.Lines.Clear;
    FClickedPart.Part :=  cpNone;
    FSumOutFileStatus.Visible := False;
    FSumOutFileStatus.Caption := '';
    FSumOutFileMessg := '';

    // Deletechart.

    if Assigned(FChart) then
    begin
      FChart.RemoveAllSeries;
      FChart.Parent := nil;
      FreeAndNil(FChart);
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultYRCSheet.CreateChart: boolean;
const OPNAME = 'TResultYRCSheet.CreateChart';
begin
  Result := False;
  try
    if (YRCGraphDataObject <> nil) and YRCGraphDataObject.Loaded  and   WasStochasticRan then
    begin
      if not Assigned(FChart) then
        FChart := TYRCChart.Create(nil, FAppModules);
      FChart.Parent := Self;
      FChart.Legend.ShadowSize := 0;
      FChart.Legend.LegendStyle     := lsSeries;
      FChart.Align := alClient;
      FChart.RemoveAllSeries;
      FChart.Legend.Visible := True;

      if (YRCGraphDataObject.PlanesCount > 0) then
      begin
        FChart.InitialiseChart;
        FChart.OnClickSeries                         := OnChartSeriesClick;
        FChart.OnMouseMove                           := OnChartMouseMove;
        FChart.OnGetLegendText                       := OnGetDisplayLegendText;
        FChart.OnMouseUp                             := OnChartMouseUp;
        FChart.OnClickSeries                         := OnChartSeriesClick;
        FChart.OnMouseDown                           := OnChartMouseDown;
        FChart.OnSeriesClickPointer                  := OnSeriesPointerClick;
        FChart.OnZoom                                := OnZoomPerformed;
        FChart.OnUndoZoom                            := OnUndoZoomPerformed;
        FChart.OnScroll                              := OnZoomPerformed;
        FChart.OnDblClick                            := OnChartOnDblClick;

        FChart.YRCPopupMenu.OnPopup                           := OnPopup;
        FChart.YRCPopupMenu.EditYValue.OnClick                := OnEditYValue;
        FChart.YRCPopupMenu.ForceCurveThrough100.OnClick      := OnForceCurveThrough100;
        FChart.YRCPopupMenu.RegressionStartEditing.OnClick    := OnChartEditPopupClick;
        FChart.YRCPopupMenu.RegressionEndEditing.OnClick      := OnChartEditPopupClick;
        FChart.YRCPopupMenu.DeterministicStartEditing.OnClick := OnChartEditPopupClick;
        FChart.YRCPopupMenu.DeterministicEndEditing.OnClick   := OnChartEditPopupClick;
        FChart.YRCPopupMenu.HideRawDataPoints.OnClick         := OnChartEditPopupClick;
        FChart.YRCPopupMenu.HideRawDataLines.OnClick          := OnChartEditPopupClick;
        FChart.YRCPopupMenu.HideFittedPoints.OnClick          := OnChartEditPopupClick;

        //Populate chart properties
        FYRCSelectorsPanel.LanguageHasChanged;
        FYRCSelectorsPanel.PopulateChartZoom;
        FYRCSelectorsPanel.ShowSelector(True);
        FYRCSelectorsPanel.PopulatePlottingBaseSelector(1,100);
        FYRCSelectorsPanel.PopulatePlaneSelector(-1,-1);
        FYRCSelectorsPanel.PopulatePlaneSelector(YRCGraphDataObject.MinYearNumber,
                           YRCGraphDataObject.MaxYearNumber);

        if (YRCGraphDataObject.PlanesCount > 1) then
        begin
          if (FMenuItemManager <> nil) then
          begin
            FYRCSelectorsPanel.PlaneSelector.SetEnabled(FMenuItemManager.InPlaneMode);
            FYRCSelectorsPanel.PlottingBaseSelector.SetEnabled(FMenuItemManager.InPlaneMode);
          end;
        end
        else
        begin
          FYRCSelectorsPanel.PlaneSelector.SetEnabled(False);
          FYRCSelectorsPanel.PlottingBaseSelector.SetEnabled(True);
        end;


        if (FMenuItemManager <> nil) then
        begin
          FMenuItemManager.SetMenuTogglePlaneMode(msEnable);
          FMenuItemManager.SetMenuTogglePlaneDown(True);
          FMenuItemManager.SetMenuToggleCurveManipulationDown(True);
          if YRCGraphDataObject.SavedToDB then
            FMenuItemManager.SetMenuDeleteChart(msEnable)
          else
            FMenuItemManager.SetMenuDeleteChart(msDisable);
        end;
        FChart.MaxYValueHasChanged;
        FChart.AssuranceIntervalHasChanged;

        if (Trim(FSumOutFileMessg) <> '') then
        begin
          FSumOutFileStatus.Visible := True;
          FSumOutFileStatus.Caption := FAppModules.Language.GetString(FSumOutFileMessg);
        end
        else
        begin
          FYRCSelectorsPanel.ChkBoxShowXY.Checked := YRCGraphDataObject.YRCChartProperties.ShowCursorPosition;
        end;
        FYRCSelectorsPanel.ChkBoxLabelOff.Checked := not YRCGraphDataObject.YRCChartProperties.ShowFirmYieldLabels;
        FYRCSelectorsPanel.ChkBoxShowXY.Checked   := YRCGraphDataObject.YRCChartProperties.ShowCursorPosition;
      end;
    end;

    if (FMenuItemManager <> nil) then
    begin
      FMenuItemManager.SetLoadCoefficientFile(msDisable);
      RefreshLegend(FMenuItemManager.InPlaneMode);
      FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(CanCopyToCLipboard);
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
    end;
    if Assigned(FAppModules.PrintManager()) then
      FAppModules.PrintManager.SetPrintEnabled(CanPrint);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.TogglePlaneMode;
const OPNAME = 'TResultYRCSheet.TogglePlaneMode';
var
  LResult: boolean;
begin
  inherited TogglePlaneMode;
  try
    if (YRCGraphDataObject <> nil)  then
    //  (YRCGraphDataObject.PlanesCount > 0)then
    begin

      if (FMenuItemManager <> nil) and FMenuItemManager.InPlaneMode then
        LResult := ShowPlaneSelection
      else
        LResult := ApplySelectedPlane;

      if LResult then
      begin
        FYRCSelectorsPanel.ShowSelector(LResult);
        FYRCSelectorsPanel.ShowTargetDraftSelector(LResult);

        if (FMenuItemManager <> nil) and FMenuItemManager.InPlaneMode then
        begin
          FMenuItemManager.SetMenuResetChartData(msDisable);
          FMenuItemManager.SetMenuSaveChart(msDisable);
          FMenuItemManager.SetMenuTogglePlaneMode(msEnable);
          {if (YRCGraphDataObject.PlanesCount > 1) then
            FMenuItemManager.SetMenuTogglePlaneMode(msEnable)
          else
            FMenuItemManager.SetMenuTogglePlaneMode(msDisable); }
          FYRCSelectorsPanel.PlaneSelector.SetEnabled(LResult and (YRCGraphDataObject.PlanesCount > 1));
          FYRCSelectorsPanel.PlottingBaseSelector.SetEnabled(LResult and (YRCGraphDataObject.MaxYearNumber > 1));
          FYRCSelectorsPanel.TargetDraftSelector.SingleSelector.Enabled := False;
          FYRCSelectorsPanel.ShowTargetDrafts.Enabled := False;
        end
        else
        begin
          if (FMenuItemManager <> nil) then
            FMenuItemManager.SetMenuSaveChart(msEnable);
          FYRCSelectorsPanel.PlaneSelector.SetEnabled(False);
          FYRCSelectorsPanel.PlottingBaseSelector.SetEnabled(False);
          FYRCSelectorsPanel.TargetDraftSelector.SingleSelector.Enabled := True;
          FYRCSelectorsPanel.ShowTargetDrafts.Enabled := (YRCGraphDataObject.SelectedTargetDraftIndex >= 0);
        end
      end;
      if (FMenuItemManager <> nil) then
        RefreshLegend(FMenuItemManager.InPlaneMode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.ShowPlaneSelection: boolean;
const OPNAME = 'TResultYRCSheet.ShowPlaneSelection';
begin
  Result := False;
  try
    if not FBusyPopulating then
    begin
      FYRCSelectorsPanel.SelectAllTargetDrafts;
      FYRCSelectorsPanel.ShowTargetDrafts.ItemIndex := 0;
      OnShowTargetDraftsSelectionChanged(FYRCSelectorsPanel.ShowTargetDrafts);
      SetChartMode(cmPlane);
      SetChartEditMode(tdmNone);
    end;
    if (FMenuItemManager <> nil) then
    begin
      FMenuItemManager.SetMenuToggleChartMode(msDisable);
      FMenuItemManager.SetMenuToggleCurveManipulationMode(msDisable);
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TResultYRCSheet.ApplySelectedPlane: boolean;
const OPNAME = 'TResultYRCSheet.ApplySelectedPlane';
begin
  Result := False;
  try
    if Assigned(FChart) then
    begin
      FChart.RepopulateChart;
      FChart.ApplySelectedPlotPlane;
      if (FMenuItemManager <> nil) then
      begin
        FMenuItemManager.SetMenuToggleChartMode(msEnable);
        FMenuItemManager.SetMenuToggleCurveManipulationMode(msEnable);
      end;
      PopulateTargetDraftSelector;
      if not FBusyPopulating then
      begin
        FYRCSelectorsPanel.SelectAllTargetDrafts;
        FYRCSelectorsPanel.ShowTargetDrafts.ItemIndex := 0;
        OnShowTargetDraftsSelectionChanged(FYRCSelectorsPanel.ShowTargetDrafts);
        SetChartMode(cmView);
        SetChartEditMode(tdmNone);
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.ToggleChartMode;
const OPNAME = 'TResultYRCSheet.ToggleChartMode';
begin
  inherited;
  try
    if (FMenuItemManager <> nil) then
    if FMenuItemManager.RegressionMode then
       ManiplateTargetDraftRegression
    else
       ManiplateTargetDraftDeterministic;

    if Assigned(FYRCSelectorsPanel.TargetDraftSelector.SingleSelector) or
       Assigned(FYRCSelectorsPanel.TargetDraftSelector.MultipleSelect) then
    RefreshTargetDraftFormula;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.ManiplateTargetDraftDeterministic: boolean;
const OPNAME = 'TResultYRCSheet.ManiplateTargetDraftDeterministic';
var
  LTargetDraft: TAbstractYRCTargetDraft;
begin
  Result := False;
  try
    if (FMenuItemManager <> nil) then
      FMenuItemManager.SetLoadCoefficientFile(msEnable);
    SetChartMode(cmManipulating);
    SetChartEditMode(tdmDeterministic);
    if (YRCGraphDataObject <> nil)  and (YRCGraphDataObject.PlanesCount > 0) then
    begin
      LTargetDraft := YRCGraphDataObject.SelectedTargetDraft;
      if Assigned(LTargetDraft) then
        LTargetDraft.TargetDraftSavedMode := tdmDeterministic;
      if Assigned(FChart) then
        FChart.StartEditing;
      Result := True;
   end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TResultYRCSheet.ManiplateTargetDraftRegression: boolean;
const OPNAME = 'TResultYRCSheet.ManiplateTargetDraftRegression';
var
  LTargetDraft: TAbstractYRCTargetDraft;
begin
  Result := False;
  try
    SetChartMode(cmManipulating);
    SetChartEditMode(tdmRegression);

    if (YRCGraphDataObject <> nil)  and (YRCGraphDataObject.PlanesCount > 0) then
    begin
      LTargetDraft := YRCGraphDataObject.SelectedTargetDraft;
      if Assigned(LTargetDraft) then
        LTargetDraft.TargetDraftSavedMode := tdmRegression;
      if Assigned(FChart) then
        FChart.StartEditing;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.StopSeriesEditing;
const OPNAME = 'TResultYRCSheet.StopSeriesEditing';
begin
  try
    SetChartMode(cmView);
    SetChartEditMode(tdmNone);
    if Assigned(FChart) then
      FChart.StopEditing;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TResultYRCSheet.WasStochasticRan: boolean;
const OPNAME = 'TResultYRCSheet.WasStochasticRan';
var
  LPane: TAbstractYRCPlane;
  LTargetDraft: TAbstractYRCTargetDraft;
begin
  Result := False;
  try
    if (YRCGraphDataObject <> nil) and  YRCGraphDataObject.Loaded then
    begin
      if (YRCGraphDataObject.PlanesCount > 0)then
      begin
        LPane := TAbstractYRCPlane(YRCGraphDataObject.YRCPlane[0]);
        if Assigned(LPane) and (LPane.TargetDraftCount > 0) then
        begin
          LTargetDraft := TAbstractYRCTargetDraft(LPane.TargetDraft[0]);
          Result := Assigned(LTargetDraft.OriginalPointsArrayObject) and
                   (Length(LTargetDraft.OriginalPointsArrayObject.YRCRecordPointArray) > 1);
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TResultYRCSheet.SetMenuVisible';
begin
  try
    if Assigned(FMenuItemManager) then
      if AVisible then
        FMenuItemManager.Show
      else
        FMenuItemManager.Hide;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.DoCopyToCLipboard;
const OPNAME = 'TResultYRCSheet.DoCopyToCLipboard';
begin
  try
    if Assigned(FChart) then
      FChart.DoCopyToCLipboard;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.DoExport(AFileName: string = '');
const OPNAME = 'TResultYRCSheet.DoExport';
begin
  try
    if Assigned(FChart) then
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FChart));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.OnChartSeriesClick(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TResultYRCSheet.OnChartSeriesClick';
begin
  try
    if (Button = mbLeft) then
    begin
      if (FAppModules.User.UserRights in CUR_EditData) and
         (FAppModules.StudyArea <> nil) and
         (not (FAppModules.StudyArea.ScenarioLocked)) then
      begin
        if (Series is TPointSeries) then
        begin
          if (YRCGraphDataObject.YRCChartProperties.ChartMode = cmManipulating) then
          begin
            if (YRCGraphDataObject.YRCChartProperties.ChartEditMode = tdmRegression) then
            begin
              FChart.DeleteRegressionPoint(ValueIndex);
              RefreshTargetDraftFormula;
            end
            else
            if(YRCGraphDataObject.YRCChartProperties.ChartEditMode = tdmDeterministic) then
            begin
              //FChartSelection.ChartEditMode := tdmNone;
              FChart.DragPointIndex := ValueIndex;
              RefreshTargetDraftFormula;
            end;
          end;
        end
        else
        if (Series is TYRCLineSeries) then
        begin
          if(YRCGraphDataObject.SelectedPlane <> nil) then
             YRCGraphDataObject.SelectedPlane.TargetDraftIndex := TYRCLineSeries(Series).TargetDraftIndex;
          FYRCSelectorsPanel.TargetDraftSelector.SelectByIndex((YRCGraphDataObject.SelectedTargetDraftIndex + 1));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnEditYValue(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnEditYValue';
var
  LValue: string;
  LReturnValue : double;
  LTargetDraft : TAbstractYRCTargetDraft;
begin
  try
    LTargetDraft := YRCGraphDataObject.SelectedTargetDraft;
    if(LTargetDraft <> nil) and (LTargetDraft.YValueAt100 <> NullFloat) then
      FChart.YRCPopupMenu.ReturnValue := FormatFloat('#0.00',LTargetDraft.YValueAt100)
    else
      FChart.YRCPopupMenu.ReturnValue := '?';
    if(FChart.YRCPopupMenu.ReturnValue = '?') then
      LValue := InputBox(FAppModules.Language.GetString('YieldReliability.InputEditYvalue'), 'Y-Value', '0.00')
    else
      LValue := InputBox(FAppModules.Language.GetString('YieldReliability.InputEditYvalue'), 'Y-Value', FLoatToStr(LTargetDraft.YValueAt100));

    LReturnValue := StrToFloatDef(LValue,0.0);

    if LTargetDraft.YValueAt100Added  then
    begin
      if (LReturnValue = LTargetDraft.YValueAt100) then Exit;
    end
    else
    begin
      if(LReturnValue = 0.0) then Exit;
    end;

    if (LReturnValue = 0.0) then
    begin
      LTargetDraft.YValueAt100 := NullFloat;
      FChart.YRCPopupMenu.ForceCurveThrough100.Checked := False;
      LTargetDraft.ForceCurveThrough100 := False;
    end
    else
    begin
      LTargetDraft.YValueAt100 := LReturnValue;
      FChart.YRCPopupMenu.ForceCurveThrough100.Checked := False;
      OnForceCurveThrough100(nil);
    end;

    FChart.RefreshRegressionSeries;
    RefreshTargetDraftFormula;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnForceCurveThrough100(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnForceCurveThrough100';
var
  LTargetDraft : TAbstractYRCTargetDraft;
begin
  try
    LTargetDraft := YRCGraphDataObject.SelectedTargetDraft;
    if(LTargetDraft <> nil) then
    begin
      FChart.YRCPopupMenu.ForceCurveThrough100.Checked := not FChart.YRCPopupMenu.ForceCurveThrough100.Checked;
      LTargetDraft.ForceCurveThrough100 := FChart.YRCPopupMenu.ForceCurveThrough100.Checked;
      FChart.RefreshRegressionSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnPopup(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnPopup';
Var
  LTargetDraft : TAbstractYRCTargetDraft;
begin
  try
    LTargetDraft := YRCGraphDataObject.SelectedTargetDraft;
    if(LTargetDraft <> nil)then
    begin
      FChart.YRCPopupMenu.CurveFitted.Checked           := LTargetDraft.CurveFitted;
      FChart.YRCPopupMenu.AddYValueAtXEquals100.Checked := LTargetDraft.YValueAt100Added;
      FChart.YRCPopupMenu.AddAdditionalPoints.Checked   := LTargetDraft.RegressionPointsAdded;
      FChart.YRCPopupMenu.ManipulateCurvePoints.Checked := LTargetDraft.DeterministicPointsChanged;
      FChart.YRCPopupMenu.ForceCurveThrough100.Enabled  := LTargetDraft.YValueAt100Added;
      FChart.YRCPopupMenu.ForceCurveThrough100.Checked  := LTargetDraft.ForceCurveThrough100;
      FChart.YRCPopupMenu.HideRawDataPoints.Checked     := YRCGraphDataObject.YRCChartProperties.HideRawPoints;
      FChart.YRCPopupMenu.HideRawDataLines.Checked      := YRCGraphDataObject.YRCChartProperties.HideRawLines;
      FChart.YRCPopupMenu.HideFittedPoints.Checked      := YRCGraphDataObject.YRCChartProperties.HideFittedPoints;

      if LTargetDraft.YValueAt100Added then
        FChart.YRCPopupMenu.ReturnValue := FormatFloat('#0.00',LTargetDraft.YValueAt100)
      else
        FChart.YRCPopupMenu.ReturnValue := '?';
      FChart.YRCPopupMenu.OnYValueChange(FChart.YRCPopupMenu.ReturnValue);
    end;
    case YRCGraphDataObject.YRCChartProperties.ChartMode of
      cmView         :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
                FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
                FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
                FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
                FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
              end;
            tdmDeterministic :
              begin
                FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
                FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
                FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
                FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
              end;
            tdmRegression    :
              begin
                FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
                FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
                FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
                FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
              end;
          end;//case
        end;
      cmManipulating :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
                FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
                FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
                FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
                FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
              end;
            tdmDeterministic :
              begin
                FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
                FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
                FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := False;
                FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := True;
              end;
            tdmRegression    :
              begin
                FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := False;
                FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := True;
                FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
                FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
              end;
          end;//case
        end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnChartOnDblClick(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnChartOnDblClick';
var
  LText : string;
begin
  try
    FChart.CalcClickedPart(Point(Trunc(FOldXPosition),Trunc(FOldYPosition)),FClickedPart);
    if (FClickedPart.Part =  cpSeriesMarks) then
    begin
      LText := InputBox('Rename label text','Enter new label text','');
      if(LText <> '') then
      begin
        FClickedPart.ASeries.Labels[1] := LText;
        FClickedPart.ASeries.Marks.Positions[1].Custom := True;
      end;
      FChart.SaveCustomLabelsData;
      FClickedPart.ASeries.ParentChart.CancelMouse:=True;
      FClickedPart.Part := cpNone;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TResultYRCSheet.OnChartMouseMove(Sender: TObject;Shift: TShiftState; X, Y: integer);
const OPNAME = 'TResultYRCSheet.OnChartMouseMove';
var
  //LDistance: integer;
  XValue,YValue: double;
  LDifX : Integer;
  LDifY : Integer;
  LPosition: TSeriesMarkPosition;
begin
  try
    if (FClickedPart.Part =  cpSeriesMarks) then
    begin
      LPosition := FClickedPart.ASeries.Marks.Positions[FClickedPart.PointIndex];
      With LPosition do
      begin
        LDifX  := Trunc(X-FOldXPosition);
        LDifY  := Trunc(Y-FOldYPosition);
        Custom := True;
        Inc(TPoint(LeftTop).X,LDifX);
        Inc(TPoint(LeftTop).Y,LDifY);
        Inc(TPoint(ArrowTo).X,LDifX);
        Inc(TPoint(ArrowTo).Y,LDifY);
        FOldXPosition := X;
        FOldYPosition := Y;
        FClickedPart.ASeries.ParentChart.CancelMouse:=True;
        FClickedPart.ASeries.Repaint;
      end;
      FChart.SaveCustomLabelsData;
    end;
    if YRCGraphDataObject.YRCChartProperties.ShowCursorPosition then
    begin
      FChart.CalculateChartValuesFromPoint(X,Y,XValue,YValue);
      FSumOutFileStatus.Caption := 'X = '+ FormatFloat('###,##0.00',XValue) + ' Y = '+ FormatFloat('###,##0.00',YValue);
    end;
    if (ssLeft in Shift) then
    begin
      if (FAppModules.User.UserRights in CUR_EditData) and
         (FAppModules.StudyArea <> nil) and
         (not (FAppModules.StudyArea.ScenarioLocked)) then
      begin
        if (YRCGraphDataObject.YRCChartProperties.ChartEditMode = tdmDeterministic) then
        begin
          if FChart.PointWithinChartRect(X, Y) then
            FChart.OnUpdateDeterministicPoint(X, Y)
          else
          begin
            //FChartSelection.ChartEditMode := tdmNone;
            FChart.DragPointIndex            := -1;
          end;
          RefreshTargetDraftFormula;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.DisplayYRCSettings(AMessage: string): boolean;
const OPNAME = 'TResultYRCSheet.DisplayYRCSettings';
var
  LFieldName: string;
  LErrors,
  LWarnings: TStringList;
  LIndex: integer;
  LConfigurationData: IRunConfigurationData;
begin
  Result := False;
  try
    //YRCGraphDataObject := YRCGraphDataObject;
    FYRCSelectorsPanel.ErrorMesg.Lines.Clear;

    if (AMessage <> '') then
    begin
      FYRCSelectorsPanel.ErrorMesg.SelAttributes.Color := clBlack;
      FYRCSelectorsPanel.ErrorMesg.SelAttributes.Style := [fsBold];
      FYRCSelectorsPanel.ErrorMesg.Lines.Add(FAppModules.Language.GetString('TYRCSelectorsPanel.' + AMessage));
      FYRCSelectorsPanel.ErrorMesg.Lines.Add('');
      if (AMessage = 'CaptionNoDbData') then Exit;
    end;
    //begin
    //  FYRCSelectorsPanel.ErrorMesg.SelAttributes.Color := clBlack;
    //  FYRCSelectorsPanel.ErrorMesg.Lines.Add(AMessage);
    //end
    //else
    //if (not YRCGraphDataObject.Loaded)  then
    //   (not WasStochasticRan) then
    //begin
      LErrors   := TStringList.Create;
      LWarnings := TStringList.Create;
      FYRCSelectorsPanel.ErrorMesg.SelAttributes.Style := [];
      try
        LConfigurationData := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;

        //Errors
        if(LConfigurationData.OutputSummaryLevel <> 0) then
        begin
          LFieldName := FAppModules.Language.GetString('TField.SummaryLevel');
          LErrors.Add('  '+LFieldName + ' = ' + IntToStr(LConfigurationData.OutputSummaryLevel) + ' it should be zero.');
        end;

        if (LConfigurationData.RunSequenceType <> 'S') then
          LErrors.Add(FAppModules.Language.GetString('YieldReliability.ErrorHistoricMode'));

        if (LConfigurationData.CalculateHistoricFirmYield <> 0)then
            LErrors.Add(FAppModules.Language.GetString('YieldReliability.ErrorHistoricFirm'));

        if(LErrors.Count > 0) then
          LErrors.Insert(0,FAppModules.Language.GetString('YieldReliability.ErrorYRCUtility'));

        //Warnings
        if (not LConfigurationData.MultiplePeriodLengths) then
        begin
            LWarnings.Add(FAppModules.Language.GetString('YieldReliability.WarningLength'));
            LWarnings.Add(FAppModules.Language.GetString('YieldReliability.HintLength'));
        end;
        if (LConfigurationData.NumberOfSequencesInAnalysis < 3) or
           (LConfigurationData.NumberOfSequencesInAnalysis > 10)  then
        begin
            LWarnings.Add(FAppModules.Language.GetString('YieldReliability.HintNumberOfLoad'));
        end;
        if(LWarnings.Count > 0) then
          LWarnings.Insert(0,FAppModules.Language.GetString('YieldReliability.WarningPlotPlane'));

        if(LErrors.Count > 0) then
        begin
          FYRCSelectorsPanel.ErrorMesg.SelAttributes.Color := clBlack;
          FYRCSelectorsPanel.ErrorMesg.Lines.Add(LErrors[0]);
          for LIndex := 1 to LErrors.Count-1 do
          begin
           FYRCSelectorsPanel.ErrorMesg.SelAttributes.Color := clRed;
            FYRCSelectorsPanel.ErrorMesg.Lines.Add(LErrors[LIndex]);
          end;
        end;
        if(LWarnings.Count > 0) then
        begin
          FYRCSelectorsPanel.ErrorMesg.SelAttributes.Color := clBlack;
          FYRCSelectorsPanel.ErrorMesg.Lines.Add(LWarnings[0]);
          for LIndex := 1 to LWarnings.Count-1 do
          begin
            FYRCSelectorsPanel.ErrorMesg.SelAttributes.Color := clTeal;
            FYRCSelectorsPanel.ErrorMesg.Lines.Add(LWarnings[LIndex]);
          end;
        end;
      finally
        LErrors.Free;
        LWarnings.Free;
      end;
    //end;
    FYRCSelectorsPanel.ErrorMesg.Visible := True;
    FYRCSelectorsPanel.ErrorMesg.SelAttributes.Color := clBlack;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnGetSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TResultYRCSheet.OnGetSeriesMarkText';
var
  LYieldMarkFormatStr: string;
begin
  try

    // Save values locally.
    LYieldMarkFormatStr := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.YieldFormatStr);
    if(Trim(LYieldMarkFormatStr) <> '') then
          MarkText := Format(LYieldMarkFormatStr,[Sender.YValues.Value[ValueIndex]]);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.OnGetDisplayLegendText(Sender: TCustomAxisPanel;LegendStyle: TLegendStyle;
                                      Index: Integer; var LegendText: String);
const OPNAME = 'TResultYRCSheet.OnGetDisplayLegendText';
begin
  try

    if (FMenuItemManager <> nil) then
    if not (FMenuItemManager.InPlaneMode) then
    begin
      LegendText := '';
      if (Index = 0) then
        LegendText := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.LegendCaption);
    end
    else
    begin

    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.OnChartMouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
const OPNAME = 'TResultYRCSheet.OnChartMouseUp';
begin
  try
    FClickedPart.Part :=  cpNone;
    if (YRCGraphDataObject.YRCChartProperties.ChartEditMode = tdmDeterministic) then
    begin
      if (FAppModules.User.UserRights in CUR_EditData) and
         (FAppModules.StudyArea <> nil) and
         (not (FAppModules.StudyArea.ScenarioLocked)) then
      begin
        //FChartSelection.ChartEditMode := tdmNone;
        FChart.DragPointIndex := -1;
        RefreshTargetDraftFormula;
      end;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.OnChartMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
const OPNAME = 'TResultYRCSheet.OnChartMouseDown';
//var
//  LPoint : TPoint;
begin
  try
    FOldXPosition :=X;
    FOldYPosition :=Y;

    if Assigned(FHintWin) then
    begin
      FHintWin.Hide;
      FHintWin := nil;
    end;

    if (Button = mbLeft) then
    begin
      FChart.CalcClickedPart(Point(Trunc(x),Trunc(y)),FClickedPart);
    end
    else
    if (Button = mbRight) then
    begin
      if (FAppModules.User.UserRights in CUR_EditData) and
         (FAppModules.StudyArea <> nil) and
         (not (FAppModules.StudyArea.ScenarioLocked)) then
      begin
        case YRCGraphDataObject.YRCChartProperties.ChartMode of
          cmPlane        :
            begin
            end;
          cmView         :
            begin
              case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
                tdmNone          :
                  begin
                    if ChartEditingAllowed then
                    begin
                      FChart.YRCPopupMenu.EnableAllItems;
                      ShowPopupMenu;
                    end;
                  end;
                tdmDeterministic :
                  begin
                    if ChartEditingAllowed then
                    begin
                      FChart.YRCPopupMenu.EnableAllItems;
                      ShowPopupMenu;
                    end;
                  end;
                tdmRegression    :
                  begin
                    if ChartEditingAllowed then
                    begin
                      FChart.YRCPopupMenu.EnableAllItems;
                      ShowPopupMenu;
                    end;
                  end;
              end;//case
            end;
          cmManipulating :
            begin
              case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
                tdmNone          :
                  begin
                    if ChartEditingAllowed then
                    begin
                      FChart.YRCPopupMenu.EnableAllItems;
                      ShowPopupMenu;
                    end;
                  end;
                tdmDeterministic :
                  begin
                    if ChartEditingAllowed then
                    begin
                      FChart.YRCPopupMenu.EnableAllItems;
                      ShowPopupMenu;
                    end;
                  end;
                tdmRegression    :
                  begin
                    if ChartEditingAllowed then
                    begin
                      if FChart.PointWithinChartRect(X,Y) then
                      begin
                        FChart.AddRegressionPoint(X,Y);
                        RefreshTargetDraftFormula;
                      end
                      else
                      begin
                        FChart.YRCPopupMenu.EnableAllItems;
                        ShowPopupMenu;
                      end;
                    end;
                  end;
              end;//case
            end;
        end;//case
      end;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.RefreshTargetDraftFormula;
const OPNAME = 'TResultYRCSheet.RefreshTargetDraftFormula';
begin
  try
    if (YRCGraphDataObject.SelectedTargetDraft = nil) then
      FYRCSelectorsPanel.TargetDraftFormula.Caption :=
      FAppModules.Language.GetString('YieldReliability.PanelCaption')
    else
      FYRCSelectorsPanel.TargetDraftFormula.Caption :=
      YRCGraphDataObject.SelectedTargetDraft.Formula;

    FYRCSelectorsPanel.Resize;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.ShowPopupMenu;
const OPNAME = 'TResultYRCSheet.ShowPopupMenu';
var
  LPoint : TPoint;
begin
  try
    if (FMenuItemManager <> nil) and (YRCGraphDataObject.SelectedTargetDraft <> nil) and
       (FMenuItemManager.ToolBar.GetMenuToggleCurveManipulationDown) then
    begin
      GetCursorPos(LPoint);
      FChart.CancelMouse := True;
      FChart.YRCPopupMenu.Popup(LPoint.X, LPoint.Y);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.ToggleCurveManipulation;
const OPNAME = 'TResultYRCSheet.ToggleCurveManipulation';
begin
  try
    {if FChart.InRegressionUpdateMode then
      OnChartEditPopupClick(FChart.YRCPopupMenu.RegressionEndEditing)
    else
    if FChart.InDeterministicUpdateMode then
      OnChartEditPopupClick(FChart.YRCPopupMenu.DeterministicEndEditing);
    if not FMenuItemManager.ToolBar.GetMenuToggleCurveManipulationDown then
    begin
      if FChart.InRegressionUpdateMode then
        OnChartEditPopupClick(FChart.YRCPopupMenu.RegressionEndEditing)
      else
      if FChart.InDeterministicUpdateMode then
        OnChartEditPopupClick(FChart.YRCPopupMenu.DeterministicEndEditing)
    end
    else
      OnChartEditPopupClick(FChart.YRCPopupMenu.RegressionStartEditing);}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultYRCSheet.ChartEditingAllowed: boolean;
const OPNAME = 'TResultYRCSheet.ChartEditingAllowed';
begin
  Result := False;
  try
    if (FMenuItemManager <> nil) then
    begin
      if FMenuItemManager.InPlaneMode then
        Result := False
      else
      if(YRCGraphDataObject.SelectedTargetDraft = nil) then
        Result := False
      else
        Result := FMenuItemManager.ToolBar.GetMenuToggleCurveManipulationDown
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultYRCSheet.ProcessMetaDataEvent : boolean;
const OPNAME = 'TResultYRCSheet.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  LChart         : TYRCChart;
begin
  Result := FALSE;
  try
    if(FChart <> nil) then
    begin
      LChart := FChart;
      lFieldIndex := '';
      lKeyValues  := 'Model='           + QuotedStr(FAppModules.StudyArea.ModelCode) +
                     ',StudyAreaName='  + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                     ',SubArea='        + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                     ',Scenario='       + QuotedStr(FAppModules.StudyArea.ScenarioCode);
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('YRCChart');
      FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
      LChart.HasMetaData := (FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil);
      LChart.Invalidate;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.EntityDescription (AFieldPropName : string;
                                            AKeyValues     : string;
                                            AFieldIndex    : string) : string;
const OPNAME = 'TResultYRCSheet.EntityDescription';
begin
  Result := '';
  try
    Result := 'Yield Reliability Curve';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.TabHasChanged;
const OPNAME = 'TResultYRCSheet.TabHasChanged';
begin
  try
    DisplayYRCSettings('CaptionNormal');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.OnSeriesPointerClick(Sender: TCustomSeries;ValueIndex, X, Y: Integer);
const OPNAME = 'TResultYRCSheet.OnSeriesPointerClick';
var
  LHintStr    : string;
  LHeight,
  LPlaneIndex,
  LTargetDraftIndex  : integer;
  LPoint      : TPoint;
  LRect,
  LHntRect    : TRect;
  LWND        : HWND;
  LRecInt,
  LExProb     : double;
begin
  try
    if Assigned(FHintWin) then
    begin
      FHintWin.Hide;
      FHintWin := nil;
    end;
    if (Sender is TYRCLineSeries) and
       (ValueIndex >= 0)then
    begin
      GetCursorPos(LPoint);

      LWND                 := WindowFromPoint(LPoint);
      LPlaneIndex          := TYRCLineSeries(Sender).PlaneIndex;
      LTargetDraftIndex    := ValueIndex;
      LExProb              := TYRCLineSeries(Sender).XScreenToValue(TYRCLineSeries(Sender).CalcXPos(ValueIndex));
      LRecInt              := YRCGraphDataObject.CalculateRIOnTargetDraft(LPlaneIndex, LTargetDraftIndex, LExProb);
      LHintStr             := Format(FAppModules.Language.GetString('YieldReliability.HintStr'), [LRecInt]);
      if (LWND > 0) then
      begin
        FHintWin           := THintWindow.Create(FChart);
        FHintWin.Parent    := TWinControl(FChart);
        LHntRect           := FHintWin.CalcHintRect(150, LHintStr, nil);
        FHintWin.Color     := clInfoBk;
        LHeight            := LHntRect.Bottom - LHntRect.Top;
        LRect.Left         := LPoint.x;
        LRect.Top          := LPoint.y + LHeight;
        LRect.Bottom       := LRect.Top + LHeight + 2;
        LRect.Right        := LRect.Left + LHntRect.right - LHntRect.Left + 5;
        If length(LHintStr) > 0 then
          FHintWin.ActivateHint(LRect, LHintStr);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.RefreshLegend(AInPlaneMode: boolean);
const OPNAME = 'TResultYRCSheet.RefreshLegend';
begin
  try
    if Assigned(FChart) then
    begin
      //if Assigned(FChart.DummyLineSeries) then
      //  FChart.DummyLineSeries.ShowInLegend := not AInPlaneMode;
      //FChart.Legend.
      if AInPlaneMode then
      begin
        FChart.Legend.Alignment         := laRight;
        FChart.Legend.Symbol.Visible    := True;
        FChart.Legend.Symbol.Continuous := True;
        FChart.Legend.Symbol.Squared    := True;
        FChart.Legend.Symbol.DefaultPen := True;
      end
      else
      begin
        FChart.Legend.Alignment         := laBottom;
        FChart.Legend.Symbol.Visible    := False;
        FChart.Legend.Symbol.Continuous := False;
        FChart.Legend.Symbol.Squared    := False;
        FChart.Legend.Symbol.DefaultPen := True;
      end;
      FChart.Legend.Symbol.Visible    := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.DoShow;
const OPNAME = 'TResultYRCSheet.DoShow';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.DeleteSeletedTargetDraft;
const OPNAME = 'TResultYRCSheet.DeleteSeletedTargetDraft';
begin
  try
    if (FChart <> nil)  then
    begin
      if(YRCGraphDataObject.PlaneIndex >= 0) and (YRCGraphDataObject.SelectedTargetDraftIndex >= 0) then
      begin
        if YRCGraphDataObject.DeleteTargetDraft(YRCGraphDataObject.SelectedTargetDraftIndex) then
        begin
          if Assigned(FChart) then
          begin
            FChart.RepopulateChart;
            FChart.ApplySelectedPlotPlane;
            SetChartMode(cmView);
            SetChartEditMode(tdmNone);

            if not FBusyPopulating then
            begin
              PopulateTargetDraftSelector;
              FYRCSelectorsPanel.SelectAllTargetDrafts;
              FYRCSelectorsPanel.ShowTargetDrafts.ItemIndex := 0;
              OnShowTargetDraftsSelectionChanged(FYRCSelectorsPanel.ShowTargetDrafts);
            end;
            if (FMenuItemManager <> nil) then
            begin
              FMenuItemManager.SetMenuToggleChartMode(msEnable);
              FMenuItemManager.SetMenuToggleCurveManipulationMode(msEnable);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//_________________________________________________________________________________________________________________

procedure TResultYRCSheet.UpdateZoomIndex;
const OPNAME = 'TResultYRCSheet.UpdateZoomIndex';
begin
  try
    if Assigned(FChart) then
    begin
      if(YRCGraphDataObject.YRCChartProperties.ZoomIndex in [0,1,2]) then
      begin
        YRCGraphDataObject.YRCChartProperties.LeftAxisMinimum := 0.0;
        YRCGraphDataObject.YRCChartProperties.LeftAxisMaximum := YRCGraphDataObject.YRCChartProperties.MaxYValue;

        YRCGraphDataObject.YRCChartProperties.RightAxisMinimum := 0.0;
        YRCGraphDataObject.YRCChartProperties.RightAxisMaximum := YRCGraphDataObject.YRCChartProperties.MaxYValue;
      end;
      FChart.ZoomIndexHasChanged;
      FChart.SaveState;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TResultYRCSheet.OnZoomChangeRequested(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnZoomChangeRequested';
begin
  try
    if FBusyPopulating then Exit;
    if (YRCGraphDataObject = nil)  then  Exit;
    FBusyPopulating := True;
    try
      YRCGraphDataObject.YRCChartProperties.ZoomIndex := FYRCSelectorsPanel.ChartZoom.ItemIndex;
      if(FYRCSelectorsPanel.ChartZoom.ItemIndex = 3) then Exit;
      UpdateZoomIndex;
    finally
      FBusyPopulating := False;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.OnUndoZoomPerformed(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnUndoZoomPerformed';
begin
  try
    SetZoomIndex(0);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.OnZoomPerformed(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnZoomPerformed';
var
  LIndex: integer;
begin
  try
    if FBusyPopulating then Exit;
    FChart.ZoomHasChanged;
    if(FChart.BottomAxis.Minimum   = 0.00) and (FChart.BottomAxis.Maximum   = 100.00) then
      LIndex := 0
    else if(FChart.BottomAxis.Minimum   = 75.00) and (FChart.BottomAxis.Maximum   = 100.00) then
      LIndex := 1
    else if(FChart.BottomAxis.Minimum   = YRCGraphDataObject.YRCChartProperties.ZoomValue) and
           (FChart.BottomAxis.Maximum   = 100.00) then
      LIndex := 2
    else
      LIndex := 3;

    FChart.SaveState;
    SetZoomIndex(LIndex);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TResultYRCSheet.SetZoomIndex(AIndex: integer):boolean;
const OPNAME = 'TResultYRCSheet.SetZoomIndex';
begin
  Result := False;
  try
    YRCGraphDataObject.YRCChartProperties.ZoomIndex := AIndex;
    if(YRCGraphDataObject.YRCChartProperties.ZoomIndex = AIndex) then
    begin
      FYRCSelectorsPanel.ChartZoom.ItemIndex := AIndex;
      FYRCSelectorsPanel.ChartZoom.Text      := FYRCSelectorsPanel.ChartZoom.Items[AIndex];
      if not FBusyPopulating then
      begin
        FBusyPopulating := True;
        try
          UpdateZoomIndex;
          Result := True;
        finally
          FBusyPopulating := False;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TResultYRCSheet.OnSetChartMaxYValue(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnSetChartMaxYValue';
var
  LValue: string;
  LNewValue: double;
begin
  try
    if (YRCGraphDataObject = nil)  then  Exit;

    LValue    := InputBox('Enter the new Y-Axis maximum value.','Value: ',
                 FloatToStr(YRCGraphDataObject.YRCChartProperties.MaxYValue));
    LNewValue :=  StrToFloatDef(LValue,YRCGraphDataObject.YRCChartProperties.MaxYValue);
    if not FBusyPopulating then
      YRCGraphDataObject.YRCChartProperties.MaxYValue := LNewValue;
    if Assigned(FChart) then
    begin
      FChart.MaxYValueHasChanged;
      FChart.SaveState;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.OnAssuranceIntervalSelectorClick(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnAssuranceIntervalSelectorClick';
var
  LSavedValues,
  LYearValues : TIntegerArray;
  LIndex: integer;
  LForm : TAbstractForm;
  LAssuranceSelector  : TRISelector;
begin
  try
    if (YRCGraphDataObject = nil)  then  Exit;

    LForm                     := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    LAssuranceSelector        := TRISelector.Create(LForm,FAppModules);
    try
      LAssuranceSelector.Parent := LForm;
      LAssuranceSelector.Align  := alClient;
      SetLength(LSavedValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalSavedArray));
      SetLength(LYearValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalYearsArray));

      for LIndex := Low(LSavedValues) to High(LSavedValues) do
        LSavedValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalSavedArray[LIndex];
      for LIndex := Low(LYearValues) to High(LYearValues) do
        LYearValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalYearsArray[LIndex];

      LAssuranceSelector.LanguageHasChanged;
      LAssuranceSelector.PopulateRecurrance(LYearValues, LSavedValues, YRCGraphDataObject.SelectedPlane.PlaneYears,WasStochasticRan);
      LForm.Width := 406;
      LForm.ShowModal;
      if(LForm.ModalResult = mrOk) then
      begin
        SetLength(LSavedValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalSavedArray));
        SetLength(LYearValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalYearsArray));

        for LIndex := Low(LSavedValues) to High(LSavedValues) do
          LSavedValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalSavedArray[LIndex];
        for LIndex := Low(LYearValues) to High(LYearValues) do
          LYearValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalYearsArray[LIndex];

        LAssuranceSelector.ReadRecurranceSaved(LSavedValues, LYearValues);
        if not FBusyPopulating then
          YRCGraphDataObject.SelectedAssuranceIntervalSavedArray := LSavedValues;
      if Assigned(FChart) then
        FChart.AssuranceIntervalHasChanged;
      end;
    finally
      FreeAndNil(LAssuranceSelector);
      FreeAndNil(LForm);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TResultYRCSheet.OnCheckBoxChanged(ASender: TObject);
const OPNAME = 'TResultYRCSheet.OnCheckBoxChanged';
var
  LCheckBox :TCheckBox;
begin
  try
    if (YRCGraphDataObject = nil)  then  Exit;

    if (ASender.ClassName = 'TCheckBox') then
    begin
      LCheckBox := TCheckBox(ASender);
      if(LCheckBox = FYRCSelectorsPanel.ChkBoxLabelOff) then
      begin
        if not FBusyPopulating then
          YRCGraphDataObject.YRCChartProperties.ShowFirmYieldLabels := not LCheckBox.Checked;
        if Assigned(FChart) then
           FChart.ShowSeriesMarksHasChanged;
      end;

      if(LCheckBox = FYRCSelectorsPanel.ChkBoxShowXY) then
      begin
        if not FBusyPopulating then
          YRCGraphDataObject.YRCChartProperties.ShowCursorPosition := FYRCSelectorsPanel.ChkBoxShowXY.Checked;
        FSumOutFileStatus.Caption := '';
        FSumOutFileStatus.Visible := YRCGraphDataObject.YRCChartProperties.ShowCursorPosition;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultYRCSheet.OnShowTargetDraftsSelectionChanged(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnShowTargetDraftsSelectionChanged';
begin
  try
    if not FBusyPopulating then
      YRCGraphDataObject.YRCChartProperties.ShowTargetDrafts :=
        TShowTargetDrafts(FYRCSelectorsPanel.ShowTargetDrafts.ItemIndex);
    if Assigned(FChart) then
       FChart.TargetDraftSelectionHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultYRCSheet.OnChartEditPopupClick(Sender: TObject);
const OPNAME = 'TResultYRCSheet.OnChartEditPopupClick';
begin
  try
    if (Sender is TMenuItem) then
    begin
      case TMenuItem(Sender).Tag of
       1: begin
            FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := False;
            FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := True;
            FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
            FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
            FChart.YRCPopupMenu.HideRawDataPoints.Enabled         := False;
            FChart.YRCPopupMenu.HideRawDataLines.Enabled          := False;
            ManiplateTargetDraftRegression;
          end;
       2: begin
            FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
            FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
            FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
            FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
            FChart.YRCPopupMenu.HideRawDataPoints.Enabled         := True;
            FChart.YRCPopupMenu.HideRawDataLines.Enabled          := True;
            StopSeriesEditing;
          end;
       3: begin
            FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := False;
            FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := True;
            FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
            FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
            FChart.YRCPopupMenu.HideRawDataPoints.Enabled         := False;
            FChart.YRCPopupMenu.HideRawDataLines.Enabled          := False;
            ManiplateTargetDraftDeterministic;
          end;
       4: begin
            FChart.YRCPopupMenu.DeterministicStartEditing.Enabled := True;
            FChart.YRCPopupMenu.DeterministicEndEditing.Enabled   := False;
            FChart.YRCPopupMenu.RegressionStartEditing.Enabled    := True;
            FChart.YRCPopupMenu.RegressionEndEditing.Enabled      := False;
            FChart.YRCPopupMenu.HideRawDataPoints.Enabled         := True;
            FChart.YRCPopupMenu.HideRawDataLines.Enabled          := True;
            StopSeriesEditing;
          end;
        5: begin
             FChart.YRCPopupMenu.HideRawDataPoints.Checked := not FChart.YRCPopupMenu.HideRawDataPoints.Checked;
             if not FBusyPopulating then
               YRCGraphDataObject.YRCChartProperties.HideRawPoints := FChart.YRCPopupMenu.HideRawDataPoints.Checked;
             FChart.RawPointsVisibilityHasChanged;
           end;
        6: begin
             FChart.YRCPopupMenu.HideRawDataLines.Checked := not FChart.YRCPopupMenu.HideRawDataLines.Checked;
             if not FBusyPopulating then
               YRCGraphDataObject.YRCChartProperties.HideRawLines := FChart.YRCPopupMenu.HideRawDataLines.Checked;
             FChart.RawLinesVisibilityHasChanged;
           end;
        7: begin
             FChart.YRCPopupMenu.HideFittedPoints.Checked := not FChart.YRCPopupMenu.HideFittedPoints.Checked;
             if not FBusyPopulating then
               YRCGraphDataObject.YRCChartProperties.HideFittedPoints := FChart.YRCPopupMenu.HideFittedPoints.Checked;
             FChart.FittedPointsVisibilityHasChanged;
           end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnTargetDraftSelectionChange(ASelectionIndeces: TIntegerArray);
const OPNAME = 'TResultYRCSheet.OnTargetDraftSelectionChange';
var
  LTargetDraft        : TAbstractYRCTargetDraft;
begin
  try
    if (YRCGraphDataObject <> nil)  and (YRCGraphDataObject.PlanesCount > 0) then
    begin
      if not FBusyPopulating then
          if(YRCGraphDataObject.SelectedPlane <> nil) then
             YRCGraphDataObject.SelectedPlane.TargetDraftIndex := FYRCSelectorsPanel.TargetDraftSelector.SingleSelector.ItemIndex - 1;

      if(YRCGraphDataObject.SelectedTargetDraftIndex = (FYRCSelectorsPanel.TargetDraftSelector.SingleSelector.ItemIndex - 1)) then
      begin
        if (YRCGraphDataObject.SelectedTargetDraftIndex < 0) then
        begin
          if not FBusyPopulating then
          begin
            SetChartMode(cmView);
            SetChartEditMode(tdmRegression);
          end;
          if (FMenuItemManager <> nil) then
          begin
            FMenuItemManager.SetMenuToggleChartMode(msDisable);
            FMenuItemManager.SetMenuResetChartData(msDisable);
            //FMenuItemManager.SetMenuToggleCurveManipulationMode(msDisable);
            FMenuItemManager.SetMenuEditYValueMode(msDisable);
            FMenuItemManager.SetMenuStartRegressionEditMode(msDisable);
            FMenuItemManager.SetMenuEndRegressionEditMode(msDisable);
            FMenuItemManager.SetMenuStartDeterministicEditMode(msDisable);
            FMenuItemManager.SetMenuEndDeterministicEditMode(msDisable);
            FMenuItemManager.SetMenuDeleteTargetDraft(msDisable);
            FMenuItemManager.SetMenuToggleCurveManipulationDown(False);
          end;

          if not FBusyPopulating then
          begin
            FYRCSelectorsPanel.ShowTargetDrafts.ItemIndex := 0;
            OnShowTargetDraftsSelectionChanged(FYRCSelectorsPanel.ShowTargetDrafts);
          end;
          FChart.YRCPopupMenu.DisableAllItems;
          //FYRCSelectorsPanel.TargetDraftSelector.SingleSelector.Enabled := False;
        end
        else
        begin
          if not FBusyPopulating then
          begin
            SetChartMode(cmView);
            SetChartEditMode(tdmRegression);
          end;

          LTargetDraft := YRCGraphDataObject.SelectedTargetDraft;
          if(LTargetDraft = nil) then
            if (LTargetDraft.TargetDraftSavedMode = tdmDeterministic) then
                SetChartEditMode(tdmDeterministic);
          if (FMenuItemManager <> nil) then
          begin
            FMenuItemManager.SetMenuToggleChartMode(msEnable);
            FMenuItemManager.SetMenuResetChartData(msEnable);
            //FMenuItemManager.SetMenuToggleCurveManipulationMode(msEnable);

            FMenuItemManager.SetMenuEditYValueMode(msEnable);
            FMenuItemManager.SetMenuStartRegressionEditMode(msEnable);
            FMenuItemManager.SetMenuEndRegressionEditMode(msEnable);
            FMenuItemManager.SetMenuStartDeterministicEditMode(msEnable);
            FMenuItemManager.SetMenuEndDeterministicEditMode(msEnable);
            FMenuItemManager.SetMenuDeleteTargetDraft(msEnable);
            FMenuItemManager.SetMenuToggleCurveManipulationDown(True);
          end;
          FYRCSelectorsPanel.ShowTargetDrafts.Enabled := True;
          FChart.YRCPopupMenu.EnableAllItems;
        end;
        if Assigned(FChart) then FChart.TargetDraftSelectionHasChanged;
          if (FMenuItemManager <> nil) then
            FMenuItemManager.SetLoadCoefficientFile(msDisable);
        RefreshTargetDraftFormula;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.SetChartMode(AMode: TChartMode);
const OPNAME = 'TResultYRCSheet.SetChartMode';
begin
  try
    if not FBusyPopulating then YRCGraphDataObject.YRCChartProperties.ChartMode := AMode;
        if Assigned(FChart) then  FChart.ChartModeHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.SetChartEditMode(AMode: TChartEditMode);
const OPNAME = 'TResultYRCSheet.SetChartEditMode';
begin
  try
    if not FBusyPopulating then YRCGraphDataObject.YRCChartProperties.ChartEditMode := AMode;
        if Assigned(FChart) then  FChart.EditModeHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnPlaneSelectionChange(ASelection: integer);
const OPNAME = 'TResultYRCSheet.OnPlaneSelectionChange';
begin
  try
    if not FBusyPopulating then YRCGraphDataObject.PeriodLength := ASelection;
    if Assigned(FChart) then  FChart.SelectedPeriodLengthHasChanged;

    if (FMenuItemManager <> nil) then
    begin
      FMenuItemManager.SetMenuTogglePlaneMode(msEnable);
      FMenuItemManager.SetLoadCoefficientFile(msDisable);

      if(YRCGraphDataObject.PlaneIndex >= 0) then
      begin
        //FMenuItemManager.SetMenuTogglePlaneMode(msEnable);
        FMenuItemManager.SetMenuToggleCurveManipulationMode(msEnable);
      end
      else
      begin
        //FMenuItemManager.SetMenuTogglePlaneMode(msDisable);
        FMenuItemManager.SetMenuToggleCurveManipulationMode(msDisable);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultYRCSheet.OnPlottingBaseSelectionChange(ASelection: integer);
const OPNAME = 'TResultYRCSheet.OnPlottingBaseSelectionChange';
begin
  try
    if not FBusyPopulating then YRCGraphDataObject.PlottingBase := ASelection;
    if Assigned(FChart) then FChart.SelectedPlottingBaseHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultYRCSheet.LoadCoefficientFile: boolean;
const OPNAME = 'TResultYRCSheet.LoadCoefficientFile';
begin
  Result := False;
  try
    if not FBusyPopulating then
    begin
      if(YRCGraphDataObject.SelectedPlane <> nil) then
        Result := YRCGraphDataObject.SelectedPlane.LoadCoefficientFile;
      if Result then
      begin
         if Assigned(FChart) then
           FChart.RefreshDeterministicSeries;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TResultYRCSheet.Create(AOwner: TComponent; AAppModules: TAppModules; ACreateAsComponent: boolean);
const OPNAME = 'TResultYRCSheet.LoadCoefficientFile';
begin
  try
    FCreateAsComponent := ACreateAsComponent;
    inherited Create(AOwner,AAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

