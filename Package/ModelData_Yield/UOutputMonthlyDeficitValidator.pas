//
//
//  UNIT      : Contains the class TOutputMonthlyDeficitValidator.
//  AUTHOR    : Oagilwe Segola(ARIVIA)
//  DATE      : 2009/01/26
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputMonthlyDeficitValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  ContNrs,
  VCLTee.Series,
  VCLTee.TeEngine,

  VCLTee.TeeBoxPlot,
  UBoxPlotData,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputMonthlyDeficitDialog;


type
  TOutputMonthlyDeficitValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData      : TOutputDataType;
    FPrevViewData         : TOutputDataType;
    FUseUnits             : TOutputUnits;
    FRILineSeriesList     : TStringList;
    FRIArray              : TIntegerArray;
    FMonthNameArray       : TMonthNamesArray;
    FMonthEventCount      : TIntegerArray;
    FLoadCase             : integer;
    FSequence             : integer;
    FMonth                : integer;
    FUnits                : TOutputUnits;
    FValueType            : TOutputValueType;
    FTimeStep             : TOutputTimeStep;
    FHighLight            : WordBool;
    FDisplayMonth         : integer;
    FBoxPlotPopulated     : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnBtnDataSelectionClick (Sender: TObject);
    procedure OnRadioGroupViewClick (Sender: TObject);
    procedure OnDrawBoxplotLabels(Sender: TChartAxis; Var X, Y, Z: Integer; Var Text: String; Var DrawLabel: Boolean);
    procedure RePopulateDataViewer;
    procedure ClearBarChart;
    procedure ClearBoxPlotChart;
    procedure PopulateBarChart;
    procedure PopulateBoxPlotChart;
    procedure PopulateMonthNameAndEventArray(ADataContainer, ASupplyDeficitData : TStringList);
    procedure GetSelectionData;
    function StochasticRun : boolean;
  public
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TOutputMonthlyDeficitDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  VCLTee.Chart,
  UOutputData,
  UConstants,
  VCLTee.TeeShape,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  Math, URunConfigurationData;

{ TOutputMonthlyDeficitValidator }

procedure TOutputMonthlyDeficitValidator.CreateMemberObjects;
const OPNAME = 'TOutputMonthlyDeficitValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputMonthlyDeficitDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.RadioGrpView.OnClick     := OnRadioGroupViewClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.DestroyMemberObjects;
const OPNAME = 'TOutputMonthlyDeficitValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.Initialise: boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
    FBoxPlotPopulated := False;
    ViewDialog.RadioGrpView.Visible :=  StochasticRun;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.DeficitDurations');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.SaveState: boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.ViewDialog : TOutputMonthlyDeficitDialog;
const OPNAME = 'TOutputMonthlyDeficitValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputMonthlyDeficitDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.ClearDataViewer;
const OPNAME = 'TOutputMonthlyDeficitValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearBarChart;
    ClearBoxPlotChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.PopulateDataViewer;
const OPNAME = 'TOutputMonthlyDeficitValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.RePopulateDataViewer;
const OPNAME = 'TOutputMonthlyDeficitValidator.RePopulateDataViewer';
var
  LChannel           : IGeneralFlowChannel;
  LError             : string;
  LDataContainer,
  LSupplyDeficitData : TStringList;
begin
  try
    LDataContainer     := TStringList.Create;
    LSupplyDeficitData := TStringList.Create;
    try
      if((NetworkElementType = votChannel) or (NetworkElementType = votMasterControl))then
      begin
        LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
        GetSelectionData;
        ClearBarChart;
        if(LChannel <> nil) then
        begin
          if (TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetComplianceGridBlockData(
              LDataContainer,LSupplyDeficitData,FLoadCase,FSequence,FIdentifier,LError)) then
          begin
            ViewDialog.BarChart.Visible := True;
            PopulateMonthNameAndEventArray(LDataContainer,LSupplyDeficitData);
            PopulateBarChart;
          end
          else
          begin
            ViewDialog.BarChart.Visible := False;
            ViewDialog.ShowError(LError);
          end;
        end;
        FBoxPlotPopulated := False;
        OnRadioGroupViewClick(nil);
      end;
    finally
      FreeAndNil(LDataContainer);
      FreeAndNil(LSupplyDeficitData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.ClearBarChart;
const OPNAME = 'TOutputMonthlyDeficitValidator.ClearBarChart';
begin
  try
    ViewDialog.BarSeries.Clear;
    ViewDialog.BarSeries.Active               := False;
    ViewDialog.BarChart.LeftAxis.Title.Caption   := '';
    ViewDialog.BarChart.BottomAxis.Title.Caption := '';
    ViewDialog.BarChart.Title.Text.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.ClearBoxPlotChart;
const OPNAME = 'TOutputMonthlyDeficitValidator.ClearBoxPlotChart';
begin
  try
    ViewDialog.BoxPlotChart.SeriesList.Clear;
    ViewDialog.BoxPlotChart.LeftAxis.Title.Caption   := '';
    ViewDialog.BoxPlotChart.BottomAxis.Title.Caption := '';
    ViewDialog.BoxPlotChart.Title.Text.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.PopulateBarChart;
const OPNAME = 'TOutputMonthlyDeficitValidator.PopulateBarChart';
var
  LChannel  : IGeneralFlowChannel;
  LIndex    : integer;
  //LMaxValue : double;
begin
  try
    ClearBarChart;
    ViewDialog.BarChart.LeftAxis.Increment := 1.0;
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                      NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if(LChannel <> nil) then
    begin
      ViewDialog.BarChart.Title.Text.Add(FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.MonthlyDeficitGraph') +
                                      LChannel.ChannelName + ')');
      ViewDialog.BarChart.Title.Visible := True;
    end;

    ViewDialog.BarChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.Months');
    ViewDialog.BarChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.MonthlyDeficitEvents');
    ViewDialog.BarChart.LeftAxis.AxisValuesFormat := '######';

    ViewDialog.BarSeries.Marks.Visible   := False;
    ViewDialog.BarSeries.SeriesColor     := clBlue;
    ViewDialog.BarSeries.BarWidthPercent := 50;
    ViewDialog.BarSeries.XValues.Order   := loAscending;
    ViewDialog.BarSeries.YValues.Order   := loNone;
    ViewDialog.BarSeries.Active          := True;

    for LIndex := 0 to Length(FMonthNameArray) - 1 do
    begin
      ViewDialog.BarSeries.AddY(FMonthEventCount[LIndex],FMonthNameArray[LIndex], clblue);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.CanExport: boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.CanPrint: boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputMonthlyDeficitValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.DoPrint;
const OPNAME = 'TOutputMonthlyDeficitValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputMonthlyDeficitValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdMonthlyDeficit,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyDeficitValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyDeficitValidator.GetSelectionData;
const OPNAME = 'TOutputMonthlyDeficitValidator.GetSelectionData';
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

procedure TOutputMonthlyDeficitValidator.PopulateMonthNameAndEventArray(ADataContainer, ASupplyDeficitData : TStringList);
const OPNAME = 'TOutputMonthlyDeficitValidator.PopulateMonthNameAndEventArray';
var
  LIndex,
  LDataIndex,
  LStartMonthNumber   : integer;
  LMonthName          : string;
  LSensetiveMonthDeficit,
  LMonthDeficitValues : TStringList;
begin
  try
    LMonthDeficitValues   := TStringList.Create;
    LSensetiveMonthDeficit := TStringList.Create;
    SetLength(FMonthNameArray,12);
    SetLength(FMonthEventCount,12);
    for LIndex := 0 to 11 do
    begin
      FMonthNameArray[LIndex] := '';
      FMonthEventCount[LIndex] := 0;
    end;

    try
      LStartMonthNumber := TYieldModelDataObject(FAppModules.Model.ModelData).GetRunConfigurationData.StartMonthNumber;
      for LIndex := 1 to 12 do
      begin
        LMonthName := TYieldModelDataObject(FAppModules.Model.ModelData).GetRunConfigurationData.MonthNameByIndex[LStartMonthNumber];
        FMonthNameArray[LIndex - 1] := LMonthName;
        Inc(LStartMonthNumber);
        if(LStartMonthNumber > 12) then
          LStartMonthNumber := 1;
      end;

      for LDataIndex := 0 to ADataContainer.Count - 1 do
      begin
        LMonthDeficitValues.CommaText := ADataContainer[LDataIndex];
        LSensetiveMonthDeficit.CommaText := ASupplyDeficitData[LDataIndex];
        for LIndex := 1 to 12 do
        begin
          if(LIndex < LSensetiveMonthDeficit.Count)then
            if (LSensetiveMonthDeficit[LIndex] = 'Y') then
              Inc(FMonthEventCount[LIndex - 1]);
        end;
      end;

    finally
      FreeAndNil(LMonthDeficitValues);
      FreeAndNil(LSensetiveMonthDeficit);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputMonthlyDeficitValidator.PopulateBoxPlotChart;
const OPNAME = 'TOutputMonthlyDeficitValidator.PopulateBoxPlotChart';
var
  LChannel        : IGeneralFlowChannel;
  LCount,
  LIndex          : integer;
  LDataContainer  : TStringList;
  LSequenceCount  : integer;
  LSequenceNumber : integer;
  LError          : string;
  LDeficitData    : TStringList;
  LBoxValues      : TBoxPlotData;
  LBoxSeries      : TBoxSeries;
  LIQR            : double;
  LMedianIndex    : Integer;
  LMedian         : double;
  LCountsArray    : TTwoDimensionIntegerArray;
begin
  try
    if (not FBoxPlotPopulated) and StochasticRun then
    begin
      ClearBoxPlotChart;
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                        NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
      if(LChannel <> nil) then
      begin
        ViewDialog.BoxPlotChart.Title.Text.Add(FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.MonthlyDeficitGraph') +
                                        LChannel.ChannelName + ')');
        ViewDialog.BoxPlotChart.Title.Visible := True;
      end;

      ViewDialog.BoxPlotChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.Months');
      ViewDialog.BoxPlotChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.MonthlyDeficitEvents');
      //ViewDialog.BoxPlotChart.LeftAxis.AxisValuesFormat := '######';

      LSequenceCount  := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis;
      SetLength(LCountsArray,12,LSequenceCount);

      ViewDialog.BoxPlotChart.BottomAxis.AxisValuesFormat := '##0';
      ViewDialog.BoxPlotChart.BottomAxis.OnDrawLabel := OnDrawBoxplotLabels;

      LDeficitData   := TStringList.Create;
      LDataContainer := TStringList.Create;
      LBoxValues     := TBoxPlotData.Create;
      try
        LSequenceNumber := 0;
        for LIndex := 1 to LSequenceCount do
        begin
          if(LSequenceCount > 10) then
            LSequenceNumber := LSequenceNumber + 1
          else
            LSequenceNumber := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.SequenceToBeAnalysedByIndex[LIndex];

          if (TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetComplianceGridBlockData(
              LDataContainer,LDeficitData,FLoadCase,LSequenceNumber,FIdentifier,LError)) then
          begin
            PopulateMonthNameAndEventArray(LDataContainer,LDeficitData);
            for LCount := Low(FMonthEventCount) to High(FMonthEventCount) do
              LCountsArray[LCount,LSequenceNumber-1] := FMonthEventCount[LCount];
          end;
        end;

        for LIndex := 1 to 12 do
        begin
          LBoxValues.Populate(LIndex-1,LCountsArray);
          //LBoxValues.RemoveZeroValues;

          LBoxSeries := ViewDialog.CreateBoxPlotSeries;
          LBoxSeries.XValues.DateTime := False;
          LBoxSeries.Position         := LBoxValues.XValueDate+1;
          LBoxSeries.AddArray(LBoxValues.YValues);
          LBoxSeries.RecalcStats;
          LBoxSeries.UseCustomValues := True;
          //LBoxSeries.ColorEachPoint   := True;
          LBoxSeries.Box.Size := 8;

          LMedianIndex    := Length(LBoxValues.YValues) div 2;
          if Odd(Length(LBoxValues.YValues)) then
            LMedian := LBoxValues.YValues[LMedianIndex]
          else
            LMedian := 0.5 * (LBoxValues.YValues[LMedianIndex-1] + LBoxValues.YValues[LMedianIndex]);

          LBoxSeries.Median       := LMedian;
          //LBoxSeries.MedianPen.Width := 2;
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

      finally
        Finalize(LCountsArray);
        LDataContainer.Free;
        LDeficitData.Free;
        LBoxValues.Free;
      end;
      FBoxPlotPopulated := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputMonthlyDeficitValidator.StochasticRun: boolean;
const OPNAME = 'TOutputMonthlyDeficitValidator.StochasticRun';
begin
  Result := False;
  try
    Result := (TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.RunSequenceType = 'S') and
              (TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.NumberOfSequencesInAnalysis > 1);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputMonthlyDeficitValidator.OnRadioGroupViewClick(Sender: TObject);
const OPNAME = 'TOutputMonthlyDeficitValidator.OnRadioGroupViewClick';
begin
  try
    if(ViewDialog.RadioGrpView.ItemIndex = 1) then
    begin
      if not FBoxPlotPopulated then
         PopulateBoxPlotChart;
      ViewDialog.BarChart.Visible := False;
      ViewDialog.BoxPlotChart.Visible := True;
    end
    else
    begin
      ViewDialog.BarChart.Visible := True;
      ViewDialog.BoxPlotChart.Visible := False;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputMonthlyDeficitValidator.OnDrawBoxplotLabels(Sender: TChartAxis; var X, Y, Z: Integer;
          var Text: String;var DrawLabel: Boolean);
const OPNAME = 'TOutputMonthlyDeficitValidator.OnDrawBoxplotLabels';
var
  LIndex : integer;
begin
  try
    LIndex := StrToIntDef(Text,NullInteger);
    if(LIndex <> NullInteger) then
    begin
      LIndex := LIndex-1;
      if(LIndex >= Low(FMonthNameArray)) and (LIndex <= High(FMonthNameArray)) then
      Text := FMonthNameArray[LIndex];
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
