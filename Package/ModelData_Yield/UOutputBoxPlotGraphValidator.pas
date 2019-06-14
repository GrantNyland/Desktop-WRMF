//
//
//  UNIT      : Contains the class TOutputBoxPlotGraphValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputBoxPlotGraphValidator;

interface

uses
  Windows,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Graphics,
  VCLTee.TeeProcs,
  Contnrs,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  VCLTee.TeeBoxPlot,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UComplienceData,
  UBoxPlotData,
  UOutputBoxPlotGraphDialog;

type
  TOutputBoxPlotGraphValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData   : TOutputDataType;
    FLineSeriesValues  : TStringList;
    FLoadCase          : integer;
    FBoxPlotValues     : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnViewDataTypeChange(Sender: TObject);
    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure OnBtnLegendClick(Sender: TObject);
    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure PopulateChartData;
    procedure PopulateBoxPlotChart;
    //procedure PopulateWhiskerChart;

    procedure ClearChart;
    procedure SetCurrentViewData;
    function  ElementName: string;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TOutputBoxPlotGraphDialog;
  end;

implementation

uses
  VCLTee.Series,
  SysUtils,
  UConstants,
  UOutputData,
  UDataSetType,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, VCLTee.Chart;

{ TOutputBoxPlotGraphValidator }

procedure TOutputBoxPlotGraphValidator.CreateMemberObjects;
const OPNAME = 'TOutputBoxPlotGraphValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FLineSeriesValues  := TStringList.Create;
    FBoxPlotValues     := TStringList.Create;
    FPanel             := TOutputBoxPlotGraphDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.cmbViewDataType.OnSelect := OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.BtnLegend.OnClick := OnBtnLegendClick;

  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TOutputBoxPlotGraphValidator.DestroyMemberObjects;
const OPNAME = 'TOutputBoxPlotGraphValidator.DestroyMemberObjects';
begin
  try
    FreeAndNil(FBoxPlotValues);
    FreeAndNil(FLineSeriesValues);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.Initialise: boolean;
const OPNAME = 'TOutputBoxPlotGraphValidator.Initialise';
var
  LSelectedLoadCase : string;
begin
  Result := inherited Initialise;
  try
    FLineSeriesValues.Sorted     := True;
    FLineSeriesValues.Duplicates := dupIgnore;
    FCurrentViewData             := btNone;
    LSelectedLoadCase := FAppModules.ViewIni.ReadString('TBoxPlotSeriesSelector','SelectedLoadCase','');
    if Trim(LSelectedLoadCase) = '' then Exit;
    while (Pos(' ', LSelectedLoadCase) > 0) do
      Delete(LSelectedLoadCase, Pos(' ', LSelectedLoadCase), 1);
    while CharInSet(LSelectedLoadCase[1],[ 'A'..'Z','a'..'z' ]) do
     Delete(LSelectedLoadCase,1, 1);
   LSelectedLoadCase := Trim(LSelectedLoadCase);
   if LSelectedLoadCase <> '' then
     FLoadCase := StrToInt(LSelectedLoadCase);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputBoxPlotGraphValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or (UpperCase(AFieldName) = 'HYDROSEQCOUNT') or
      (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
    begin
      if(UpperCase(AFieldName) = 'LOADCASESCOUNT') then
        FAppModules.ViewIni.WriteInteger('TBoxPlotSeriesSelector','LoadCase', StrToInt(ANewValue));
      PopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputBoxPlotGraphValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputBoxPlotGraphValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputBoxPlotGraph');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.SaveState: boolean;
const OPNAME = 'TOutputBoxPlotGraphValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.ViewDialog : TOutputBoxPlotGraphDialog;
const OPNAME = 'TOutputBoxPlotGraphValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputBoxPlotGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputBoxPlotGraphValidator.PopulateDialogSelectors';
var
  LViewData: string;
  LRunConfigurationData : IRunConfigurationData;
begin
  try
    ViewDialog.cmbViewDataType.Clear;
    if (FIdentifier >= 0)  and (NetworkElementType <> votNone)then
    begin
      case NetworkElementType of
        votMasterControl:
        begin
          //LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
          //ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
        end;
        votReservoir,votReservoirAreaGroup:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile1');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirVolume)));
          LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
          if LRunConfigurationData <> nil then
            FAppModules.ViewIni.WriteInteger('TBoxPlotSeriesSelector','LoadCase', LRunConfigurationData.NrOfActiveLoadCases);

          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile2');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirElevation)));
        end;
        votNodeWithInflow:
        begin
        end;
        votNodeWithoutInflow:
        begin
        end;
        votChannel:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
        end;
        votIrrigationArea:
        begin
        end;
        votPowerPlant:
        begin
        end;
        votWetland:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile1');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirVolume)));
          LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
          if LRunConfigurationData <> nil then
            FAppModules.ViewIni.WriteInteger('TBoxPlotSeriesSelector','LoadCase', LRunConfigurationData.NrOfActiveLoadCases);
        end;
        votChannelArea:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
        end;
      end;
      if(ViewDialog.cmbViewDataType.Items.Count > 0) then
      begin
        ViewDialog.cmbViewDataType.ItemIndex := 0;
        SetCurrentViewData;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.RePopulateDataViewer;
const OPNAME = 'TOutputBoxPlotGraphValidator.RePopulateDataViewer';
var
  LErrors: string;
begin
  try
    ClearChart;
    if(FIdentifier >= 0) and (NetworkElementType = votChannelArea) and (FCurrentViewData <> btNone) then
    begin
      LErrors := '';
      ViewDialog.ShowError(LErrors);
      if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetChannelAreaBoxPlotData(
        FBoxPlotValues,FCurrentViewData,FIdentifier,FLoadCase,LErrors)) then
      begin
        PopulateChartData;
      end
      else
      begin
        ViewDialog.ShowError(LErrors);
      end;
    end
    else
    if(FIdentifier >= 0) and (NetworkElementType = votReservoirAreaGroup) and (FCurrentViewData <> btNone) then
    begin
      LErrors := '';
      ViewDialog.ShowError(LErrors);
      if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetReservoirAreaBoxPlotData(
        FBoxPlotValues,FCurrentViewData,FIdentifier,FLoadCase,LErrors)) then
      begin
        PopulateChartData;
      end
      else
      begin
        ViewDialog.ShowError(LErrors);
      end;
    end
    else if(FIdentifier >= 0) and (NetworkElementType <> votNone) and (FCurrentViewData <> btNone) then
    begin
      LErrors := '';
      ViewDialog.ShowError(LErrors);
      if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBoxPlotData(
        FBoxPlotValues,FCurrentViewData,FIdentifier,FLoadCase,LErrors)) then
      begin
        PopulateChartData;
      end
      else
      begin
        ViewDialog.ShowError(LErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.PopulateDataViewer;
const OPNAME = 'TOutputBoxPlotGraphValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.ClearDataViewer;
const OPNAME = 'TOutputBoxPlotGraphValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearChart;
    ViewDialog.cmbViewDataType.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.PopulateChartData;
const OPNAME = 'TOutputBoxPlotGraphValidator.PopulateChartData';
begin
  try
    ClearChart;
    //if (ViewDialog.cmbViewDataType.ItemIndex = 0) then
      PopulateBoxPlotChart;
    //else
    //  PopulateWhiskerChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.PopulateBoxPlotChart;
const OPNAME = 'TOutputBoxPlotGraphValidator.PopulateBoxPlotChart';
var
  LIndex               : Integer;
  LIndex2              : Integer;
  LBoxValues           : TBoxPlotData;
  LBoxSeries           : TBoxSeries;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LIQR                 : double;
  LLengthBoxValueArray,
  LMonthCount,
  LMonthType           : Integer;
  LMedianIndex         : Integer;
  LMedian              : double;
  LUnits               : TOutputUnits;
  LChartCaption        : string;
begin
  try
    ViewDialog.Chart.BottomAxis.DateTimeFormat := 'yyyy/mmm';
    if NetworkElementType = votChannel then
    begin
      LUnits        := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Units;
      LChartCaption := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
      case LUnits of
        ouPerSecond         : ViewDialog.Chart.LeftAxis.Title.Caption := LChartCaption + ' ('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
        ouMcmPerMonthOrYear : ViewDialog.Chart.LeftAxis.Title.Caption := LChartCaption + ' ('+FAppModules.Language.GetString('MasterControl.M3perMonth')+')';
        ouMegaLitersPerDay  : ViewDialog.Chart.LeftAxis.Title.Caption := LChartCaption + ' ('+FAppModules.Language.GetString('MasterControl.MegaL')+')';
      end;
    end
    else
      ViewDialog.Chart.LeftAxis.Title.Caption    := FAppModules.Language.GetString('OutputBoxPlotGraphChart.LeftAxisCaption');

    ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.000';
    ViewDialog.Chart.BottomAxis.Title.Caption  := 'Boxplots derived from '+
                                                 IntToStr(TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis)
                                                 + ' sequences';
    //ViewDialog.Chart.LeftAxis.Title.Caption   := FAppModules.Language.GetString('ChartCaption.DeficitEvents');
    //ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('ChartCaption.DeficitDuration');

    LBoxValues      := TBoxPlotData.Create;
    LLineSeriesList := TObjectList.Create(False);
    LMonthType      := FAppModules.ViewIni.ReadInteger('TBoxPlotSeriesSelector','SelectedMonthType', 0);
    try
      if (FLineSeriesValues.Count > 0) then
      begin
        for LIndex := 0 to FLineSeriesValues.Count -1 do
        begin
          LLineSeries := TLineSeries.Create(ViewDialog.Chart);
          LLineSeries.ParentChart := ViewDialog.Chart;
          LLineSeriesList.Add(LLineSeries);
          if(LMonthType = 0) then
          begin
            LLineSeries.XValues.DateTime := True;
          end;
        end;

        LMonthCount := 0;
        for LIndex := 0 to FBoxPlotValues.Count - 1 do
        begin
          LBoxValues.Populate(FBoxPlotValues[LIndex]);
          LMonthCount := LMonthCount + 1;
          if LBoxValues.Populated then
          begin
            for LIndex2 := 0 to LLineSeriesList.Count -1 do
            begin
              LLineSeriesPerc      := StrToFloat(FLineSeriesValues[LIndex2]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              if(LLineSeriesPercValue <> NullFloat) then
              begin
                LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex2]);

                if(LMonthType = 0) then
                begin
                  LLineSeries.AddXY(LBoxValues.XValueDate,LLineSeriesPercValue);
                end
                else
                if(LMonthType = 1) then
                begin
                  LLineSeries.AddXY(LMonthCount,LLineSeriesPercValue);
                end;
              end;
            end;
          end;
        end;
      end;

      LMonthCount := 0;
      for LIndex := 0 to FBoxPlotValues.Count - 1 do
      begin
        LBoxValues.Populate(FBoxPlotValues[LIndex]);
        LLengthBoxValueArray := Length(LBoxValues.YValues);
        if LBoxValues.Populated then
        begin
          LBoxSeries := ViewDialog.CreateBoxPlotSeries;
          LMonthCount := LMonthCount + 1;

          if(LMonthType = 0) then
          begin
            LBoxSeries.XValues.DateTime := True;
            LBoxSeries.Position         := LBoxValues.XValueDate;
          end
          else
          if(LMonthType = 1) then
          begin
            LBoxSeries.Position := LMonthCount;
          end;

          LBoxSeries.AddArray(LBoxValues.YValues);
          LBoxSeries.RecalcStats;
          LBoxSeries.UseCustomValues := True;

          LMedianIndex    := LLengthBoxValueArray div 2;
          if Odd(LLengthBoxValueArray) then
            LMedian := LBoxValues.YValues[LMedianIndex]
          else
            LMedian := 0.5 * (LBoxValues.YValues[LMedianIndex-1] + LBoxValues.YValues[LMedianIndex]);

          LBoxSeries.Median       := LMedian;
          LBoxSeries.Quartile1    := LBoxValues.PercValue(25);
          LBoxSeries.Quartile3    := LBoxValues.PercValue(75);

          LIQR  := LBoxValues.PercValue(95) - LBoxValues.PercValue(5);

          LBoxSeries.InnerFence1    := LBoxSeries.Quartile1 - (LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.InnerFence3    := LBoxSeries.Quartile3 + (LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.OuterFence1    := LBoxSeries.Quartile1 - (2 * LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.OuterFence3    := LBoxSeries.Quartile3 + (2 * LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.AdjacentPoint1 := LBoxValues.PercValue(5);
          LBoxSeries.AdjacentPoint3 := LBoxValues.PercValue(95);

          LBoxSeries.MildOut.Visible  := False;
          LBoxSeries.ExtrOut.Visible  := False;
        end;
      end;
    finally
      LBoxValues.Free;
      LLineSeriesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputBoxPlotGraphValidator.ClearChart;
const OPNAME = 'TOutputBoxPlotGraphValidator.ClearChart';
begin
  try
    ViewDialog.Chart.SeriesList.Clear;
    ViewDialog.Chart.UndoZoom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputBoxPlotGraphValidator.OnBtnDataSelectionClick';
var
  LForm : TAbstractForm;
  LSeriesSelector:TBoxPlotSeriesSelector;
begin
  try
    LForm                     := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    LSeriesSelector           := TBoxPlotSeriesSelector.Create(LForm,FAppModules);
    LSeriesSelector.Parent := LForm;
    LSeriesSelector.Align  := alClient;
    try
      LSeriesSelector.LanguageHasChanged;
      LForm.Caption := 'Select Box plot line series %';
      LForm.ShowModal;
      if(LForm.ModalResult = mrOk) then
      begin
        FLineSeriesValues.CommaText  := LSeriesSelector.SeriesValuesCommatext;
        FLoadCase                    := LSeriesSelector.LoadCase;
        RePopulateDataViewer;
      end;
    finally
      FreeAndNil(LForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.OnBtnLegendClick(Sender: TObject);
const OPNAME = 'TOutputBoxPlotGraphValidator.OnBtnLegendClick';
var
  LForm : TAbstractForm;
  LImage: TImage;
begin
  try
    LForm := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    try
      LImage := TImage.Create(LForm);
      LImage.Parent      := LForm;
      LImage.Transparent := True;
      LImage.AutoSize    := True;
      LImage.Center      := True;
      LImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'BOXPLOTLEGEND');
      LForm.ClientHeight := LImage.Height;
      LForm.ClientWidth  := LImage.Height;
      LImage.Align       := alClient;
      LForm.Caption      := FAppModules.Language.GetString('FormCaption.BoxPlotSeries');
      LForm.LanguageHasChanged;
      LForm.ShowModal;
    finally
      FreeAndNil(LForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.SetCurrentViewData;
const OPNAME = 'TOutputBoxPlotGraphValidator.SetCurrentViewData';
var
  LIndex: integer;
begin
  try
    LIndex := Integer(ViewDialog.cmbViewDataType.Items.Objects[ViewDialog.cmbViewDataType.ItemIndex]);
    if(LIndex < 0) then
      LIndex := 0;
    FCurrentViewData := TOutputDataType(LIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputBoxPlotGraphValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.ElementName: string;
const OPNAME = 'TOutputBoxPlotGraphValidator.ElementName';
begin
  Result := '';
  try
    case FNetworkElementType of
      votMasterControl:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier].ChannelName;
      end;
      votReservoir:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier].ReservoirConfigurationData.ReservoirName;
      end;
      votNodeWithInflow:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.NodeWithInflowByIdentifier[FIdentifier].ReservoirConfigurationData.ReservoirName;
      end;
      votNodeWithoutInflow:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.NodeWithoutInflowByIdentifier[FIdentifier].ReservoirConfigurationData.ReservoirName;
      end;
      votChannel:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier].ChannelName;
      end;
      votIrrigationArea:
      begin
      end;
      votPowerPlant:
      begin
      end;
      votWetland:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.WetlandList.WetlandByNodeNumber[FIdentifier].Name;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.CanExport: boolean;
const OPNAME = 'TOutputBoxPlotGraphValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphValidator.CanPrint: boolean;
const OPNAME = 'TOutputBoxPlotGraphValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputBoxPlotGraphValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphValidator.DoPrint;
const OPNAME = 'TOutputBoxPlotGraphValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

