unit UOutputComparisonChannelValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Graphics,
  VCLTee.TeEngine,
  VCLTee.Series,
  VCLTee.Chart,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputComparisonData,
  UOutputGraphDialog;

type
  TOutputComparisonChannelValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData     : TOutputDataType;
    FLoadCase            : integer;
    FSequence            : integer;
    FMonth               : integer;
    FUnits               : TOutputUnits;
    FValueType           : TOutputValueType;
    FTimeStep            : TOutputTimeStep;
    FHighLight           : WordBool;
    FMonthly             : WordBool;
    FAnnually            : WordBool;
    FDisplayMonth        : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnViewDataTypeChange(Sender: TObject);
    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure ClearChart;
    procedure SetCurrentViewData;
    function  ElementName: string;
    procedure PopulateChannelData(ASeries : integer; AData: TStrings;ASeriesName : string);
    procedure FomartMonthlyChart;
    procedure GetSelectionData;
    procedure GetChartLegend(AChart: TFieldChart);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TOutputGraphDialog;
    procedure DoOnDataChange(Sender : TObject);
    procedure SetChartLegend(ALegendAlignment: TLegendAlignment; ALegendVisible: boolean);

  end;

implementation

uses
  SysUtils,
  VCL.Forms,
  VCLTee.TeeProcs,
  UConstants,
  UWetland,
  UOutputData,
  UDataSetType,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UStringListOfStringLists,
  Math,
  VCL.Dialogs,
  UUtilities,
  UOutputComparitorChartLegendDialog,
  URunConfigurationData;

{ TOutputComparisonChannelValidator }

procedure TOutputComparisonChannelValidator.CreateMemberObjects;
const OPNAME = 'TOutputComparisonChannelValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputGraphDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.cmbViewDataType.OnSelect:= OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    OnDataChange := DoOnDataChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.DestroyMemberObjects;
const OPNAME = 'TOutputComparisonChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TOutputComparisonChannelValidator.Initialise: boolean;
const OPNAME = 'TOutputComparisonChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputComparisonChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or (UpperCase(AFieldName) = 'HYDROSEQCOUNT')
      or (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputComparisonChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComparisonChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputComparisonChannel');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.SaveState: boolean;
const OPNAME = 'TOutputComparisonChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.ViewDialog : TOutputGraphDialog;
const OPNAME = 'TOutputComparisonChannelValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputComparisonChannelValidator.GetNextSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputComparisonChannelValidator.GetPreviousSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  - 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.ClearDataViewer;
const OPNAME = 'TOutputComparisonChannelValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ViewDialog.cmbViewDataType.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.PopulateDataViewer;
const OPNAME = 'TOutputComparisonChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputComparisonChannelValidator.PopulateDialogSelectors';
var
  LViewData    : string;
begin
  try
    LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
    ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
    if(ViewDialog.cmbViewDataType.Items.Count > 0) then
    begin
      ViewDialog.cmbViewDataType.ItemIndex := 0;
      SetCurrentViewData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.RePopulateDataViewer;
const OPNAME = 'TOutputComparisonChannelValidator.RePopulateDataViewer';
var
  LDataContainer : TStringList;
  LChannel : IGeneralFlowChannel;
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LChannelList : TStringList;
  LElements,
  LCount,
  LIndex : integer;
  LRIndex : integer;
  LSeriesName,
  LErrors: string;
begin
  try
    ClearChart;
    GetSelectionData;
    if (FCurrentViewData <> btNone) then
    begin
      LDataContainer := TStringList.Create;
      try
        LErrors := '';
        ViewDialog.ShowError(LErrors);
        case FCurrentViewData of
          btMonthlyAverageChannelFlow:
          begin
            LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
            LElements := 0;
            if (LOutputComparisonList <> nil) and (LOutputComparisonList.GetOutputComparisonDataCount > 1) then
            begin
              for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
              begin
                LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
                LElements := LElements + LOutputComparisonData.ChannelCount;
              end;
              ViewDialog.Elements := LElements;
              ViewDialog.PrepareChart;
              LCount := 0;
              for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
              begin
                LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
                LChannelList := TStringList.Create;
                try
                  LChannelList.CommaText := LOutputComparisonData.ChannelList;
                  for LRIndex := 0 to LChannelList.Count-1 do
                  begin
                    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                                .ChannelList.ChannelByChannelNumber[strToInt(LChannelList[LRIndex])];
                    if LChannel <> nil then
                    begin
                      if LOutputComparisonData.OutputData.CastSummaryOutputData.GetBlockData(LDataContainer,
                        btMonthlyAverageChannelFlow,strToInt(LChannelList[LRIndex]),LErrors) then
                      begin
                        LSeriesName := LOutputComparisonData.OutputFileName+ ' - '+LChannel.ChannelName;
                        PopulateChannelData(LCount,LDataContainer,LSeriesName);
                        inc(LCount);
                      end;  
                    end;
                  end;
                finally
                  LChannelList.Free;
                end;
              end;
              FomartMonthlyChart;
            end
            else
              ViewDialog.ShowError(LErrors);
          end;
        end;
      finally
        LDataContainer.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.SetCurrentViewData;
const OPNAME = 'TOutputComparisonChannelValidator.SetCurrentViewData';
var
  LIndex: integer;
begin
  try
    LIndex := Integer(ViewDialog.cmbViewDataType.Items.Objects[ViewDialog.cmbViewDataType.ItemIndex]);
    if(LIndex < 0) then
      LIndex := 0;
    FCurrentViewData := TOutputDataType(LIndex);
    ViewDialog.FullLevelLineSeries.Active := False;
    ViewDialog.DeadLineSeries.Active      := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputComparisonChannelValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
    ViewDialog.FullLevelLineSeries.Active := False;
    ViewDialog.DeadLineSeries.Active      := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.ClearChart;
const OPNAME = 'TOutputComparisonChannelValidator.ClearChart';
begin
  try
    ViewDialog.LineSeries.Clear;
    ViewDialog.LineSeries.Active := False;
    ViewDialog.DeadLineSeries.Clear;
    ViewDialog.DeadLineSeries.Active := False;
    ViewDialog.FullLevelLineSeries.Clear;
    ViewDialog.FullLevelLineSeries.Active := False;
    ViewDialog.Chart.LeftAxis.Title.Caption := '';
    ViewDialog.Chart.BottomAxis.Title.Caption := '';
    ViewDialog.Chart.Title.Text.Clear;
    ViewDialog.Chart.UndoZoom;
    ViewDialog.Chart.Legend.Visible := False;
    ViewDialog.ClearChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonChannelValidator.ElementName: string;
const OPNAME = 'TOutputComparisonChannelValidator.ElementName';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputComparisonChannelValidator.PopulateChannelData(ASeries : integer; AData: TStrings;ASeriesName : string);
const OPNAME = 'TOutputComparisonChannelValidator.PopulateChannelData';
var
  LMonthNumbers    : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex,
  LCount           : integer;
  lMonthLabel      : string;
  LMonthlyValues   : TStringList;
  LMonthlyValue    : double;
  LTotal           : double;
begin
  try
    if(AData.Count > 0) then
    begin
      ViewDialog.Chart.Visible := True;
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TSCSheet.X_AxisLabel');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;
      ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.00';
      ViewDialog.Chart.LeftAxis.TitleSize        := 1;
      LStartMonth := FAppModules.StudyArea.CalendarStartMonth-1;
      LMonthlyValue := 0;
      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;
      LMonthlyValues := TStringList.Create;
      try
        for LIndex := 0 to AData.Count -1 do
        begin
          LTotal := 0.0;
          LMonthlyValues.CommaText := AData.Strings[LIndex];
          LYear := StrToInt(LMonthlyValues.Strings[0]);
          for LCount := 1 to LMonthlyValues.Count -2 do
          begin
            if (FTimeStep = otsAnnual) OR (FDisplayMonth = 0) OR (FDisplayMonth = LCount) then
            begin
              LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);
              if (FUnits = ouPerSecond) then
              begin
                LMonthlyValue   := LMonthlyValue;
              end;
              if (FUnits = ouMcmPerMonthOrYear) then
              begin
                LMonthlyValue   := (LMonthlyValue *  86400.0) /1000000.0;
              end;
              if (FUnits = ouMegaLitersPerDay) then
              begin
                LMonthlyValue :=(LMonthlyValue *  86400.0) / 1000.0;
              end;
              if (FUnits = ouPercentage) then
              begin
                  LMonthlyValue := LMonthlyValue/LMonthlyValue * 100.0;
              end;
              if (FDisplayMonth > 0) then
              begin
                if (FDisplayMonth <= 3) then
                  lMonthLabel := IntToStr(LYear) + '/' + Format('%2.2d', [FDisplayMonth + 9])
                else
                  lMonthLabel := IntToStr(LYear + 1) + '/' + Format('%2.2d', [FDisplayMonth - 3]);
              end
              else
              if (FDisplayMonth = 0) then
                lMonthLabel := IntToStr(LYear);
              if (FTimeStep <> otsAnnual) then
              begin
                ViewDialog.ComparisonSeries[ASeries].AddY(LMonthlyValue,lMonthLabel);
                ViewDialog.ComparisonSeries[ASeries].Title  := ASeriesName;
              end;

            end;
            LTotal :=  LTotal + (LMonthlyValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount]);
          end;

          LTotal := LTotal / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
          if (FTimeStep = otsAnnual) then
          begin
            lMonthLabel := IntToStr(LYear);
            ViewDialog.ComparisonSeries[ASeries].AddY(LTotal,lMonthLabel);
            ViewDialog.ComparisonSeries[ASeries].Title  := ASeriesName;
          end;
        end;
      finally
        LMonthlyValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputComparisonChannelValidator.FomartMonthlyChart;
const OPNAME = 'TOutputComparisonChannelValidator.FomartMonthlyChart';
var
  LFactor : double;
  LIndex : integer;
  LMax : double;
begin
  try
    if (FUnits = ouPercentage) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supply')
    else
    if (FUnits = ouPerSecond) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supplym3')
    else
    if (FUnits = ouMegaLitersPerDay) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMegaL')
    else
    ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMillionPerMonth');

    if (FUnits = ouPercentage) then
    begin
      ViewDialog.Chart.LeftAxis.SetMinMax( 0, 105 );
      for LIndex := 0 to ViewDialog.Elements-1 do
        ViewDialog.Chart.LeftAxis.SetMinMax(0,105);
    end
    else
    begin
      LFactor := ViewDialog.LineSeries.YValues.MaxValue;
      LFactor := LFactor * 0.05;
      ViewDialog.Chart.LeftAxis.SetMinMax(0,ViewDialog.LineSeries.YValues.MaxValue + LFactor);
      LMax := 105;
      for LIndex := 0 to ViewDialog.Elements-1 do
      begin
        LFactor := ViewDialog.ComparisonSeries[LIndex].YValues.MaxValue;
        if ViewDialog.ComparisonSeries[LIndex].YValues.MaxValue > LMax then
          LMax := ViewDialog.ComparisonSeries[LIndex].YValues.MaxValue;
      end;
      LFactor := LFactor * 0.05;
      ViewDialog.Chart.LeftAxis.SetMinMax(0,LMax + LFactor);
    end;
    GetChartLegend(ViewDialog.Chart);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.GetChartLegend(AChart: TFieldChart);
const OPNAME = 'TOutputComparisonChannelValidator.GetChartLegend';
var
  LIndex : integer;
  LTitle : string;
begin
  try
    if AChart.SeriesCount > 1 then
    begin
      AChart.Legend.Show;
      AChart.Legend.Alignment := laBottom;
    end;

    for LIndex := 0 to AChart.SeriesCount-1 do
    begin
      LTitle := AChart.Series[LIndex].Title;
      if (Trim(LTitle)= '') then
        AChart.Series[LIndex].ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputComparisonChannelValidator.OnBtnDataSelectionClick';
begin
  try
   TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdChannelComparison,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonChannelValidator.GetSelectionData;
const OPNAME = 'TOutputComparisonChannelValidator.GetSelectionData';
var
  LDataSelection : IOutputDataSelection;
begin
  try
    LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
    if (LDataSelection <> nil) then
    begin
      FLoadCase     := LDataSelection.LoadCase;
      FSequence     := LDataSelection.Sequence;
      FMonth        := LDataSelection.Month;
      FUnits        := LDataSelection.Units;
      FValueType    := LDataSelection.ValueType;
      FTimeStep     := LDataSelection.TimeStep;
      FHighLight    := LDataSelection.Highlight;
      FDisplayMonth := LDataSelection.DisplayMonth;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputComparisonChannelValidator.DoOnDataChange(Sender: TObject);
const OPNAME = 'TOutputComparisonChannelValidator.DoOnDataChange';
var
  LLegendAlignment : TLegendAlignment;
  frmChartLegendDialog : TOutputComparitorChartLegendDialog;
  LLegendVisible : boolean;
begin
  try
    if Sender <> nil then Exit;
    if Assigned(ViewDialog.Chart) and (ViewDialog.Chart.Visible) then
    begin
      frmChartLegendDialog := TOutputComparitorChartLegendDialog.CreateWithoutDFM(nil, FAppModules);
      try
        frmChartLegendDialog.Width    := ViewDialog.Width div 3;
        frmChartLegendDialog.Height   := ViewDialog.Height div 2;
        frmChartLegendDialog.Position := poScreenCenter;
        frmChartLegendDialog.Initialise;
        frmChartLegendDialog.LanguageHasChanged;
        frmChartLegendDialog.LegendAlignment := ViewDialog.Chart.Legend.Alignment;
        frmChartLegendDialog.LegendVisible   := ViewDialog.Chart.Legend.Visible;
        if Assigned(frmChartLegendDialog) then
          frmChartLegendDialog.SetEnabled((FAppModules.User.UserRights in CUR_EditData) and
                                          (FAppModules.StudyArea <> nil ) and
                                          (not (FAppModules.StudyArea.ScenarioLocked)));
        frmChartLegendDialog.ShowModal;
        if (frmChartLegendDialog.ModalResult = mrOk) then
        begin
          LLegendAlignment := frmChartLegendDialog.LegendAlignment;
          LLegendVisible   := frmChartLegendDialog.LegendVisible;
          SetChartLegend(LLegendAlignment, LLegendVisible);
        end;
      finally
        if Assigned(frmChartLegendDialog) then
          FreeAndNil(frmChartLegendDialog);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputComparisonChannelValidator.SetChartLegend(ALegendAlignment: TLegendAlignment; ALegendVisible: boolean);
const OPNAME = 'TOutputComparisonChannelValidator.SetChartLegend';
begin
  try
    ViewDialog.Chart.Legend.Alignment := ALegendAlignment;
    ViewDialog.Chart.Legend.Visible   := ALegendVisible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
