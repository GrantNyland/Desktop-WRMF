//
//
//  UNIT      : Contains TYRCFirmYieldSheet Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 19/07/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UYRCFirmYieldSheet;

interface

uses
  Classes,
  VCLTee.Chart,
  VCLTee.Series,
  VCL.Controls,  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCLTee.TeEngine,
  VCLTee.TeExport,

  VCLTee.TeeProcs,
  UYRCSeries,
  UYRCFirmYieldPanel,
  UDataEditComponent,
  UMenuItemManager,
  UAbstractObject,
  UDataComponent,
  UAbstractComponent,
  UYRCFirmYieldChart,
  UAbstractYRCData,
  UAbstractModelData,
  UFirmYieldMenuItemManager;

type

  TYRCFirmYieldSheet = class(TAbstractTabSheet)
  protected
    FOldStudyCode,
    FOldSubAreaCode    : string;
    FYRCSelectorsPanel : TYRCFirmYieldPanel;
    FChart             : TYRCFirmYieldChart;
    FMenuItemManager   : TFirmYieldMenuItemManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnZoomChangeRequested(Sender: TObject);
    procedure OnAssuranceIntervalSelectorClick(Sender: TObject);
    procedure OnSetChartMaxYValue(Sender: TObject);
    function YRCGraphDataObject:TAbstractYRCGraphDataObject;
    function GetMenuItemManager: TFirmYieldMenuItemManager;
    function GetToolBar: TAbstractToolBar; override;
    procedure OnChartSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; x, y: Integer);
    procedure SetMenuInitialState;
  public
    procedure Resize; override;
    function StudyHasChanged: boolean;override;
    function LanguageHasChanged: boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard;override;
    procedure DoExport(AFileName: string = '');override;
    procedure DoPrint; override;
    procedure DoNewChart;
    procedure DoDeleteChart;
    procedure DoOpenChart;
    procedure DoSaveChart;
    procedure DoAddSeries;
    procedure DoDeleteSeries;
    procedure SetMenuVisible(AVisible: boolean); override;
    property MenuItemManager: TFirmYieldMenuItemManager read GetMenuItemManager;
    property ToolBar : TAbstractToolBar Read GetToolBar;
  end;

implementation

uses
  VCL.Graphics,
  VCLTee.TeeStore,
  Types,
  SysUtils,
  Windows,
  VCL.Dialogs,
  VCL.Clipbrd,
  VCL.Printers,
  UConstants,
  UMainMenuEventType,
  UYRCModelDataObject,
  UErrorHandlingOperations;


procedure TYRCFirmYieldSheet.CreateMemberObjects;
const OPNAME = 'TYRCFirmYieldSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FOldStudyCode      := '';
    FOldSubAreaCode    := '';
    FTabCaptionKey     := 'FirmYields';
    FYRCSelectorsPanel := TYRCFirmYieldPanel.Create(self,FAppModules);
    FChart             := nil;
    FMenuItemManager   := TFirmYieldMenuItemManager.Create(FAppModules);
    FYRCSelectorsPanel.Parent := Self;

    FYRCSelectorsPanel.btnAssuranceInterval.OnClick := OnAssuranceIntervalSelectorClick;
    FYRCSelectorsPanel.ChartZoom.OnSelect := OnZoomChangeRequested;
    FYRCSelectorsPanel.btnSetYMax.OnClick := OnSetChartMaxYValue;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DestroyMemberObjects;
const OPNAME = 'TYRCFirmYieldSheet.DestroyMemberObjects';
begin
    inherited DestroyMemberObjects;
  try
    FMenuItemManager.Free;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYRCFirmYieldSheet.LanguageHasChanged: boolean;
const OPNAME = 'TYRCFirmYieldSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FChart) then
      FChart.LanguageHasChanged;
    FYRCSelectorsPanel.LanguageHasChanged;
    FMenuItemManager.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldSheet.StudyHasChanged: boolean;
const OPNAME = 'TYRCFirmYieldSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if(FAppModules.StudyArea.StudyAreaCode <> FOldStudyCode) or
      (FAppModules.StudyArea.SubAreaCode   <> FOldSubAreaCode) then
      DoDeleteChart;
    FOldStudyCode      := FAppModules.StudyArea.StudyAreaCode;
    FOldSubAreaCode    := FAppModules.StudyArea.SubAreaCode;

    if Assigned(FChart) then
      FChart.StudyHasChanged;
    FYRCSelectorsPanel.StudyHasChanged;
    FMenuItemManager.StudyHasChanged;
    SetMenuInitialState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldSheet.GetMenuItemManager: TFirmYieldMenuItemManager;
const OPNAME = 'TYRCFirmYieldSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := FMenuItemManager;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TYRCFirmYieldSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := FMenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldSheet.Resize;
const OPNAME = 'TYRCFirmYieldSheet.Resize';
begin
  inherited Resize;
  try
    FYRCSelectorsPanel.Align := alNone;
    FYRCSelectorsPanel.Align := alTop;
    if Assigned(FChart) then
    begin
      FChart.Align := alNone;
      FChart.Align := alClient;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYRCFirmYieldSheet.YRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TYRCFirmYieldSheet.YRCGraphDataObject';
begin
  Result := nil;
  try
    if (FAppModules.Model.ModelData is TYRCModelDataObject) then
      Result := TYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TYRCFirmYieldSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFirmYieldSheet.CanExport: boolean;
const OPNAME = 'TYRCFirmYieldSheet.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFirmYieldSheet.CanPrint: boolean;
const OPNAME = 'TYRCFirmYieldSheet.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCFirmYieldSheet.DoPrint;
const OPNAME = 'TYRCFirmYieldSheet.DoPrint';
begin
  try
    if Assigned(FChart) and (Printer.Printers.Count > 0) then
    begin
      FChart.PrintMargins       := Rect(0,0,0,0);
      FChart.PrintProportional  := False;
      FChart.Print;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoCopyToCLipboard;
const OPNAME = 'TYRCFirmYieldSheet.DoCopyToCLipboard';
begin
  try
    if Assigned(FChart) then
      FChart.DoCopyToCLipboard;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoExport(AFileName: string = '');
const OPNAME = 'TYRCFirmYieldSheet.DoExport';
begin
  try
    if Assigned(FChart) then
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FChart));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.OnZoomChangeRequested(Sender: TObject);
const OPNAME = 'TYRCFirmYieldSheet.OnZoomChangeRequested';
var
  LZoomPerc: double;
begin
  try
    if Assigned(FChart) and FChart.Visible then
    begin
      if (FYRCSelectorsPanel.ChartZoom.ItemIndex = 0) then
      begin
        FChart.BottomAxis.Automatic := False;
        FChart.BottomAxis.Minimum   := 0.00;
        FChart.BottomAxis.Maximum   := 100.00;
        FChart.BottomAxis.AdjustMaxMin;
      end
      else if (FYRCSelectorsPanel.ChartZoom.ItemIndex = 1) then
      begin
        FChart.BottomAxis.Automatic := False;
        FChart.BottomAxis.Minimum   := 75.00;
        FChart.BottomAxis.Maximum   := 100.00;
      end
      else if (FYRCSelectorsPanel.ChartZoom.ItemIndex = 2) then
      begin
        LZoomPerc := StrToFloatDef(InputBox('Zoom Selection','Enter start zoom (0..99)','75.00'),100.00);
        if(LZoomPerc >= 0.0) and (LZoomPerc < 100.0) then
        begin
          FChart.BottomAxis.Automatic := False;
          FChart.BottomAxis.Minimum   := LZoomPerc;
          FChart.BottomAxis.Maximum   := 100.00;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.OnAssuranceIntervalSelectorClick(Sender: TObject);
const OPNAME = 'TYRCFirmYieldSheet.OnAssuranceIntervalSelectorClick';
var
  LSavedValues,
  LYearValues : TIntegerArray;
  LIndex: integer;
  LForm : TAbstractForm;
  LAssuranceSelector : TRISelector;
begin
  if not Assigned(FChart) then Exit;
  try
    LForm                     := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    LAssuranceSelector        := TRISelector.Create(LForm,FAppModules);
    LAssuranceSelector.Parent := LForm;
    LAssuranceSelector.Align  := alClient;
    try
      SetLength(LSavedValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalSavedArray));
      SetLength(LYearValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalYearsArray));

      for LIndex := Low(LSavedValues) to High(LSavedValues) do
        LSavedValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalSavedArray[LIndex];
      for LIndex := Low(LYearValues) to High(LYearValues) do
        LYearValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalYearsArray[LIndex];

      LAssuranceSelector.LanguageHasChanged;
      LAssuranceSelector.PopulateRecurrance(LYearValues, LSavedValues, YRCGraphDataObject.SelectedPlane.PlaneYears,True);
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
        YRCGraphDataObject.SelectedAssuranceIntervalSavedArray := LSavedValues;
        FChart.SetAssuranceIntervals;
      end;
    finally
      FreeAndNil(LAssuranceSelector);
      FreeAndNil(LForm);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.OnSetChartMaxYValue(Sender: TObject);
const OPNAME = 'TYRCFirmYieldSheet.OnSetChartMaxYValue';
var
  LValue: string;
  LOldValue,
  LNewValue: double;
begin
  try
    if(FChart <> nil) and FChart.Visible then
    begin
      LOldValue := FChart.LeftAxis.Maximum;
      LValue    := InputBox('Enter the new Y-Axis maximum value.','Value: ',FloatToStr(LOldValue));
      LNewValue :=  StrToFloatDef(LValue,LOldValue);
      if(LNewValue <> LOldValue) then
        FChart.SetChartMaxYValue(LNewValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoAddSeries;
const OPNAME = 'TYRCFirmYieldSheet.DoAddSeries';
var
  LFileName: string;
begin
  try
    if Assigned(FChart) then
    begin
      if PromptForFileName(LFileName,'Firm Yield Series Data Files|*.csv','csv','Firm Yield Series Data File Selection','',False) then
      begin
       FChart.AddFirmYieldSeries(LFileName);
       FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(CanCopyToCLipboard);
       FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
       FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoDeleteSeries;
const OPNAME = 'TYRCFirmYieldSheet.DoDeleteSeries';
begin
  try
    if Assigned(FChart) then
    begin
      if (FChart.SelectedFirmYieldSeries <> nil) then
        FChart.DeleteSelectFirmYieldSeries;
      if (FChart.SelectedFirmYieldSeries = nil) then
        FMenuItemManager.SetMenuDeleteSeries(msDisable);

      FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(CanCopyToCLipboard);
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(CanExport);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoNewChart;
const OPNAME = 'TYRCFirmYieldSheet.DoNewChart';
begin
  try
    if Assigned(FChart) then
      FreeAndNil(FChart);
    FChart         := TYRCFirmYieldChart.Create(Self,FAppModules);
    FChart.Parent  := Self;
    FChart.OnClickSeries := OnChartSeriesClick;
    FChart.InitialiseChart;
    FChart.LanguageHasChanged;
    Self.Resize;
    SetMenuInitialState
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoDeleteChart;
const OPNAME = 'TYRCFirmYieldSheet.DoDeleteChart';
begin
  try
    if Assigned(FChart) then
    begin
      FChart.Parent  := nil;
      FreeAndNil(FChart);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoSaveChart;
const OPNAME = 'TYRCFirmYieldSheet.DoSaveChart';
var
  LFileName: string;
begin
  try
    if Assigned(FChart) then
    begin
      if PromptForFileName(LFileName,'Chart Files|*.tee','tee','Chart File Selection','',False) then
      begin
        SaveChartToFile(VCLTee.Chart.TCustomChart(FChart),LFileName,True,True);
        FChart.SaveChartLabels(LFileName);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.DoOpenChart;
const OPNAME = 'TYRCFirmYieldSheet.DoOpenChart';
var
  LLineData,
  LFileName: string;
  LContents: TStringList;
begin
  try
    if PromptForFileName(LFileName,'Chart Files|*.tee','tee','Chart File Selection','',False) then
    begin
      LContents := TStringList.Create;
      try
        LContents.LoadFromFile(LFileName);
        LLineData := LContents.CommaText;
        LLineData := StringReplace(LLineData,'TYRCPointSeries','TPointSeries',[rfReplaceAll, rfIgnoreCase]);
        LLineData := StringReplace(LLineData,'TYRCLineSeries','TLineSeries',[rfReplaceAll, rfIgnoreCase]);
        LContents.CommaText := LLineData;
        LContents.SaveToFile(LFileName);
      finally
        LContents.Free;
      end;

      DoNewChart;
      FChart.LoadChartLabels(LFileName);
      LoadChartFromFile(TCustomChart(FChart), LFileName);
      FChart.ClassifySeries;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYRCFirmYieldSheet.SetMenuInitialState;
const OPNAME = 'TYRCFirmYieldSheet.SetMenuInitialState';
begin
  try
    if (Assigned(FMenuItemManager) and Assigned(FYRCSelectorsPanel) and (YRCGraphDataObject <> nil)) then
    begin
      if (YRCGraphDataObject.PlanesCount = 0) or (YRCGraphDataObject.PlanesCount = 0) or
        (not (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        if Assigned(FChart) then
          FChart.Enabled := False;
        FMenuItemManager.SetMenuNewChart(msDisable);
        FMenuItemManager.SetMenuOpenChart(msDisable);
        FMenuItemManager.SetMenuSaveChart(msDisable);
        FMenuItemManager.SetMenuAddSeries(msDisable);
        FMenuItemManager.SetMenuDeleteSeries(msDisable);
        FYRCSelectorsPanel.btnSetYMax.Enabled := False;
        FYRCSelectorsPanel.ChartZoom.Enabled := False;
        FYRCSelectorsPanel.btnAssuranceInterval.Enabled := False;
        FYRCSelectorsPanel.ChartZoomLabel.Enabled := False;
      end else begin
        if Assigned(FChart) then
          FChart.Enabled := True;
        FMenuItemManager.SetMenuNewChart(msEnable);
        FMenuItemManager.SetMenuOpenChart(msEnable);
        FMenuItemManager.SetMenuSaveChart(msEnable);
        FMenuItemManager.SetMenuAddSeries(msEnable);
        FMenuItemManager.SetMenuDeleteSeries(msDisable);
        FYRCSelectorsPanel.btnSetYMax.Enabled := True;
        FYRCSelectorsPanel.ChartZoom.Enabled := True;
        FYRCSelectorsPanel.btnAssuranceInterval.Enabled := True;
        FYRCSelectorsPanel.ChartZoomLabel.Enabled := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCFirmYieldSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TYRCFirmYieldSheet.SetMenuVisible';
begin
  try
    SetMenuInitialState;
    if Assigned(FMenuItemManager) then
      if AVisible then
        FMenuItemManager.Show
      else
        FMenuItemManager.Hide;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCFirmYieldSheet.OnChartSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer;
          Button: TMouseButton; Shift: TShiftState; x, y: Integer);
const OPNAME = 'TYRCFirmYieldSheet.OnChartSeriesClick';
begin
  try
    if (Button = mbLeft) then
    begin
      if (Series is TLineSeries) then
      begin
        FChart.SelectFirmYieldSeries(TLineSeries(Series));
        if(FChart.SelectedFirmYieldSeries <> nil) then
          FMenuItemManager.SetMenuDeleteSeries(msEnable)
        else
         FMenuItemManager.SetMenuDeleteSeries(msDisable);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

