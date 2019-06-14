//
//
//  UNIT      : Contains the class TOutputDeficitDurationValidator.
//  AUTHOR    : Oagilwe Segola(ARIVIA)
//  DATE      : 2009/01/22
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputDeficitDurationValidator;

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
  UOutputDeficitDurationDialog;

type
  TOutputDeficitDurationValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData      : TOutputDataType;
    FPrevViewData         : TOutputDataType;
    FUseUnits             : TOutputUnits;
    FRILineSeriesList     : TStringList;
    FDeficitDurationArray : TIntegerArray;
    FNumberOfDeficitArray : TIntegerArray;
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

    procedure RePopulateDataViewer;
    procedure ClearBarChart;
    procedure ClearBoxPlotChart;
    procedure PopulateBarChart;
    procedure PopulateBoxPlotChart;
    function  DeficitDurationArrayIndex(ADeficitDuration : integer) : integer;
    procedure PopulateEventsArrayWithData(ADataContainer, ASupplyDeficitData : TStringList);
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
    function ViewDialog: TOutputDeficitDurationDialog;
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
  Math;

{ TOutputDeficitDurationValidator }

procedure TOutputDeficitDurationValidator.CreateMemberObjects;
const OPNAME = 'TOutputDeficitDurationValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputDeficitDurationDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.RadioGrpView.OnClick     := OnRadioGroupViewClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.DestroyMemberObjects;
const OPNAME = 'TOutputDeficitDurationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.Initialise: boolean;
const OPNAME = 'TOutputDeficitDurationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData  := btNone;
    FPrevViewData     := btNone;
    FBoxPlotPopulated := False;
    ViewDialog.RadioGrpView.Visible :=  StochasticRun;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputDeficitDurationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDeficitDurationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TOutputDeficitDurationValidator.DeficitDurations');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.SaveState: boolean;
const OPNAME = 'TOutputDeficitDurationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.ViewDialog : TOutputDeficitDurationDialog;
const OPNAME = 'TOutputDeficitDurationValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputDeficitDurationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.ClearDataViewer;
const OPNAME = 'TOutputDeficitDurationValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearBarChart;
    ClearBoxPlotChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.PopulateDataViewer;
const OPNAME = 'TOutputDeficitDurationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.RePopulateDataViewer;
const OPNAME = 'TOutputDeficitDurationValidator.RePopulateDataViewer';
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
            PopulateEventsArrayWithData(LDataContainer,LSupplyDeficitData);
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

procedure TOutputDeficitDurationValidator.ClearBarChart;
const OPNAME = 'TOutputDeficitDurationValidator.ClearBarChart';
begin
  try
    ViewDialog.BarSeries.Clear;
    ViewDialog.BarSeries.Active               := False;
    ViewDialog.BarChart.LeftAxis.Title.Caption   := '';
    ViewDialog.BarChart.BottomAxis.Title.Caption := '';
    ViewDialog.BarChart.Title.Text.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.ClearBoxPlotChart;
const OPNAME = 'TOutputDeficitDurationValidator.ClearBoxPlotChart';
begin
  try
    ViewDialog.BoxPlotChart.SeriesList.Clear;
    ViewDialog.BoxPlotChart.LeftAxis.Title.Caption   := '';
    ViewDialog.BoxPlotChart.BottomAxis.Title.Caption := '';
    ViewDialog.BoxPlotChart.Title.Text.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.PopulateBarChart;
const OPNAME = 'TOutputDeficitDurationValidator.PopulateBarChart';
var
  LChannel  : IGeneralFlowChannel;
  LIndex    : integer;
  LMaxValue : double;
begin
  try
    ClearBarChart;
    ViewDialog.BarChart.LeftAxis.Increment := 1.0;
    ViewDialog.BarChart.BottomAxis.Increment := 1.0;
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                      NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if(LChannel <> nil) then
    begin
      ViewDialog.BarChart.Title.Text.Add(FAppModules.Language.GetString('TOutputDeficitDurationValidator.DeficitDurationGraph') +
                                      LChannel.ChannelName + ')');
      ViewDialog.BarChart.Title.Visible := True;
    end;

    ViewDialog.BarChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TOutputDeficitDurationValidator.DeficitDuration');
    ViewDialog.BarChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('TOutputDeficitDurationValidator.MonthlyDeficitEvents');
    ViewDialog.BarChart.LeftAxis.AxisValuesFormat := '######';

    ViewDialog.BarSeries.Marks.Visible   := False;
    ViewDialog.BarSeries.SeriesColor     := clBlue;
    ViewDialog.BarSeries.BarWidthPercent := 30;
    ViewDialog.BarSeries.XValues.Order   := loAscending;
    ViewDialog.BarSeries.YValues.Order   := loNone;
    ViewDialog.BarSeries.Active          := True;

    for LIndex := 0 to Length(FDeficitDurationArray) - 1 do
      ViewDialog.BarSeries.AddXY(FDeficitDurationArray[LIndex], FNumberOfDeficitArray[LIndex],'',clblue);

    LMaxValue := ViewDialog.BarSeries.YValues.MaxValue + ViewDialog.BarSeries.YValues.MinValue;
    ViewDialog.BarChart.LeftAxis.SetMinMax(0,LMaxValue);
    ViewDialog.BarChart.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.CanExport: boolean;
const OPNAME = 'TOutputDeficitDurationValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.CanPrint: boolean;
const OPNAME = 'TOutputDeficitDurationValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputDeficitDurationValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.DoPrint;
const OPNAME = 'TOutputDeficitDurationValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputDeficitDurationValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdDeficitDuration,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputDeficitDurationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationValidator.GetSelectionData;
const OPNAME = 'TOutputDeficitDurationValidator.GetSelectionData';
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

function TOutputDeficitDurationValidator.DeficitDurationArrayIndex(ADeficitDuration : integer) : integer;
const OPNAME = 'TOutputDeficitDurationValidator.DeficitDurationArrayIndex';
var
  LIndex : integer;
begin
  Result := -1;
  try
    for LIndex := Low(FDeficitDurationArray) to High(FDeficitDurationArray) do
    begin
      if(ADeficitDuration = FDeficitDurationArray[LIndex]) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputDeficitDurationValidator.PopulateEventsArrayWithData(ADataContainer, ASupplyDeficitData : TStringList);
const OPNAME = 'TOutputDeficitDurationValidator.PopulateEventsArrayWithData';
var
  LIndex,
  LMonthIndex,
  LDeficitDurationIndex,
  LDeficitDuration  : integer;
  LSensetiveMonthDeficit,
  LMonthDeficitValues : TStringList;
begin
  try
    LMonthDeficitValues   := TStringList.Create;
    LSensetiveMonthDeficit := TStringList.Create;
    SetLength(FDeficitDurationArray,0);
    SetLength(FNumberOfDeficitArray,0);
    try
      LDeficitDuration            := 0;
      for LIndex := 0 to ADataContainer.Count - 1 do
      begin
        LMonthDeficitValues.CommaText := ADataContainer[LIndex];
        LSensetiveMonthDeficit.CommaText := ASupplyDeficitData[LIndex];
        for LMonthIndex := 1 to LMonthDeficitValues.Count - 2 do
        begin
          {if(StrToFloat(LMonthDeficitValues[LMonthIndex]) > 0.0) then
            Inc(LDeficitDuration)}
          if(LSensetiveMonthDeficit[LMonthIndex] = 'Y') then
            Inc(LDeficitDuration)
          else
          begin
            if (LDeficitDuration > 0) then
            begin
              LDeficitDurationIndex := DeficitDurationArrayIndex(LDeficitDuration);
              if(LDeficitDurationIndex < 0) then
              begin
                SetLength(FDeficitDurationArray,Length(FDeficitDurationArray)+1);
                SetLength(FNumberOfDeficitArray,Length(FNumberOfDeficitArray)+1);
                FDeficitDurationArray[Length(FDeficitDurationArray)-1] := LDeficitDuration;
                FNumberOfDeficitArray[Length(FNumberOfDeficitArray)-1] := 1;
              end
              else
              begin
                FNumberOfDeficitArray[LDeficitDurationIndex] := FNumberOfDeficitArray[LDeficitDurationIndex]+1;
              end;
              LDeficitDuration := 0;
            end;
          end;
        end;
      end;
      if (LDeficitDuration > 0) then
      begin
        LDeficitDurationIndex := DeficitDurationArrayIndex(LDeficitDuration);
        if(LDeficitDurationIndex < 0) then
        begin
          SetLength(FDeficitDurationArray,Length(FDeficitDurationArray)+1);
          SetLength(FNumberOfDeficitArray,Length(FNumberOfDeficitArray)+1);
          FDeficitDurationArray[Length(FDeficitDurationArray)-1] := LDeficitDuration;
          FNumberOfDeficitArray[Length(FNumberOfDeficitArray)-1] := 1;
        end
        else
        begin
          FNumberOfDeficitArray[LDeficitDurationIndex] := FNumberOfDeficitArray[LDeficitDurationIndex]+1;
        end;
      end;
    finally
      FreeAndNil(LMonthDeficitValues);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputDeficitDurationValidator.StochasticRun: boolean;
const OPNAME = 'TOutputDeficitDurationValidator.StochasticRun';
begin
  Result := False;
  try
    Result := (TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.RunSequenceType = 'S') and
              (TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.NumberOfSequencesInAnalysis > 1);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputDeficitDurationValidator.OnRadioGroupViewClick(Sender: TObject);
const OPNAME = 'TOutputDeficitDurationValidator.OnRadioGroupViewClick';
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

procedure TOutputDeficitDurationValidator.PopulateBoxPlotChart;
const OPNAME = 'TOutputDeficitDurationValidator.PopulateBoxPlotChart';
var
  LChannel        : IGeneralFlowChannel;
  LCount,
  LIndex          : integer;
  LDataContainer  : TStringList;
  LSequenceCount  : integer;
  LSequenceNumber : integer;
  LError          : string;
  LDeficitData    : TStringList;
  LBoxPlotData    : TBoxPlotData;
  LBoxSeries      : TBoxSeries;
  LIQR            : double;
  LMedianIndex    : Integer;
  LMedian         : double;
  LBoxPlotDataList   : TBoxPlotDataList;

begin
  try
    if (not FBoxPlotPopulated) and StochasticRun then
    begin
      ClearBoxPlotChart;
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                        NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
      if(LChannel <> nil) then
      begin
        ViewDialog.BoxPlotChart.Title.Text.Add(FAppModules.Language.GetString('TOutputDeficitDurationValidator.DeficitDurationGraph') +
                                        LChannel.ChannelName + ')');
        ViewDialog.BoxPlotChart.Title.Visible := True;
      end;

      ViewDialog.BoxPlotChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TOutputDeficitDurationValidator.DeficitDurationBoxPlot');
      ViewDialog.BoxPlotChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('TOutputDeficitDurationValidator.MonthlyDeficitEvents');
      //ViewDialog.BoxPlotChart.LeftAxis.AxisValuesFormat := '######';

      LSequenceCount  := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis;

      LDeficitData     := TStringList.Create;
      LDataContainer   := TStringList.Create;
      LBoxPlotDataList := TBoxPlotDataList.Create;
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
            PopulateEventsArrayWithData(LDataContainer,LDeficitData);
            for LCount := Low(FDeficitDurationArray) to High(FDeficitDurationArray) do
            begin
               LBoxPlotData  := LBoxPlotDataList.CreateBoxPlotData(FDeficitDurationArray[LCount]);
               LBoxPlotData.AddValue(FNumberOfDeficitArray[LCount]);
            end;
          end;
        end;

        for LIndex := 0 to LBoxPlotDataList.Count-1 do
        begin
          LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
          LBoxPlotData.SortData;
          LBoxSeries := ViewDialog.CreateBoxPlotSeries;
          LBoxSeries.XValues.DateTime := False;
          LBoxSeries.Position         := LBoxPlotData.XValueDouble;
          LBoxSeries.AddArray(LBoxPlotData.YValues);
          LBoxSeries.RecalcStats;
          LBoxSeries.UseCustomValues := True;
          LBoxSeries.Box.Size := 8;

          LMedianIndex    := Length(LBoxPlotData.YValues) div 2;
          if Odd(Length(LBoxPlotData.YValues)) then
            LMedian := LBoxPlotData.YValues[LMedianIndex]
          else
            LMedian := 0.5 * (LBoxPlotData.YValues[LMedianIndex-1] + LBoxPlotData.YValues[LMedianIndex]);

          LBoxSeries.Median       := LMedian;
          //LBoxSeries.MedianPen.Width := 2;
          LBoxSeries.Quartile1    := LBoxPlotData.PercValue(25);
          LBoxSeries.Quartile3    := LBoxPlotData.PercValue(75);
          LIQR  := LBoxPlotData.PercValue(95) - LBoxPlotData.PercValue(5);

          LBoxSeries.InnerFence1    := LBoxSeries.Quartile1 - (LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.InnerFence3    := LBoxSeries.Quartile3 + (LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.OuterFence1    := LBoxSeries.Quartile1 - (2 * LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.OuterFence3    := LBoxSeries.Quartile3 + (2 * LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.AdjacentPoint1 := LBoxPlotData.PercValue(5);
          LBoxSeries.AdjacentPoint3 := LBoxPlotData.PercValue(95);

          LBoxSeries.ExtrOut.Visible  := False;
          LBoxSeries.ExtrOut.Style    := psStar;
          LBoxSeries.MildOut.Visible  := False;
          LBoxSeries.MildOut.Style    := psStar;
          LBoxSeries.RecalcStats;
        end;
      finally
        LDataContainer.Free;
        LDeficitData.Free;
        LBoxPlotDataList.Free;
      end;

      FBoxPlotPopulated := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
