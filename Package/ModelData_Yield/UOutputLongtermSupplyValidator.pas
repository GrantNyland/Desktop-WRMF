//
//
//  UNIT      : Contains the class TOutputLongtermSupplyValidator.
//  AUTHOR    : Samuel M Dhlamini (ARIVIA)
//  DATE      : 2009/20/11
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputLongtermSupplyValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.Graphics,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  ContNrs,
  VCLTee.Series,
  VCLTee.TeEngine,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UIFRFeatures,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputDistributionCurveDialog,
  UReconciliationAnalysisValidator;

type
  TComplianceArray = array of double;
  TOutputLongtermSupplyValidator = class(TAbstractOutputDialogValidator)
  protected
    FHintWin             : THintWindow;
    FCurrentViewData     : TOutputDataType;
    FPrevViewData        : TOutputDataType;
    FUseUnits            : TOutputUnits;
    FSelectedRIArray     : TIntegerArray;
    FDefaultRIArray      : TIntegerArray;
    FRIArray             : TIntegerArray;
    FProportionArray     : TComplianceArray;
    FComplianceArray     : TComplianceArray;
    FRILineSeriesList    : TStringList;
    FComplianceShapeList : TObjectList;
    FEventNoAction       : boolean;
    FRISelector          : TRISelector;
    FLoadCase            : integer;
    FSequence            : integer;
    FMonth               : integer;
    FUnits               : TOutputUnits;
    FValueType           : TOutputValueType;
    FTimeStep            : TOutputTimeStep;
    FHighLight           : WordBool;
    FDisplayMonth        : integer;
    FShowCompliance      : Boolean;
    FXValue,
    FActualDemand        : double;
    FAssurenceDialog     : TReconciliationAnalysisValidator;
    FValueList           : TStringList;
    FLessValueFound      : boolean;
    FFactor              : double;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnShowComplianceClick(Sender : TObject);

    procedure OnShowRISelectorClick(Sender : TObject);
    procedure OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
    procedure OnBtnDataSelectionClick (Sender: TObject);
    procedure OnViewDataTypeChange(Sender: TObject);

    procedure GetChartLegend(AChart : TFieldChart);
    function GetBarColorByYValue(AYValue, AXValue : double) : TColor;

    procedure RePopulateDataViewer;
    procedure PopulateDialogSelectors;
    procedure SetCurrentViewData;
    procedure ClearChart;
    function  InArray(AValue : integer; AArray : TIntegerArray) : boolean;
    function  LoadRecurrenceIntervals : boolean;
    function  SeriesByName(AName : string) : TLineSeries;
    procedure PopulateChartData (ADataStringList : TStringList;AChannel : IGeneralFlowChannel);
    procedure PopulateChart(ADataStringList : TStringList;AChannel : IGeneralFlowChannel);

    procedure PopulateRecurrenceIntervals;
    function CalculateXPointStochastic (APointYears, APlaneYears : integer): double;
    procedure CalculateCompliance;

    procedure ClearRecurrenceIntervals;
    procedure ToggleSelectedRecurrenceIntervals(AYearValues, ASavedValues:TIntegerArray);
    procedure ConfigureRILineSeriers(ARISeries : TLineSeries);
    procedure ConfigureBarSeriers(ABarSeries: TBarSeries; AColor : TColor);
    procedure GetSelectionData;
    procedure ClearCompliance;
    procedure PopulateCompliance;

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
    function ViewDialog: TOutputDistributionCurveDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Dialogs,
  VCLTee.Chart,
  VCL.Forms,
  Windows,
  UOutputData,
  UConstants,
  UDataSetType,
  VCLTee.TeeShape,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, VCL.CheckLst;

{ TOutputLongtermSupplyValidator }

procedure TOutputLongtermSupplyValidator.CreateMemberObjects;
const OPNAME = 'TOutputLongtermSupplyValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FEventNoAction := False;
    FPanel := TOutputDistributionCurveDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.cmbViewDataType.OnSelect := OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.BtnRISelector.OnClick    := OnShowRISelectorClick;
    ViewDialog.BtnCompliance.OnClick    := OnShowComplianceClick;

    FRILineSeriesList    := TStringList.Create;
    FValueList           := TStringList.Create;
    FComplianceShapeList := TObjectList.Create(TRUE);
    LoadRecurrenceIntervals;
    FRISelector := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.DestroyMemberObjects;
const OPNAME = 'TOutputLongtermSupplyValidator.DestroyMemberObjects';
begin
  try
    FRILineSeriesList := nil;
    FreeAndNil(FValueList);
    FreeAndNil(FComplianceShapeList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.LoadRecurrenceIntervals: boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.LoadRecurrenceIntervals';
var
  lIndex      : integer;
  lConfig     : IWaterDemandConfiguration;
  lRICount    : integer;
  lRIVal      : integer;
  lRIStr      : string;
  lRIList     : TStringList;
  lCheckIndex : integer;
begin
  Result := False;
  try
    lConfig  := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WaterDemandConfiguration;
    lRICount := lConfig.RiskCriteriaCount;
    lRIList  := TStringList.Create;
    lRIList.Sorted := TRUE;
    try
      lRIList.CommaText := '0005,0010,0020,0050,0100,0200,0500';
      for lIndex := 1 to lRICount do
      begin
        lRIVal := Trunc(lConfig.RecurrenceIntervalByIndex[lIndex]);
        lRIStr := Format('%4.4d', [lRIVal]);
        if (lRIList.IndexOf(lRIStr) < 0) then
          lRIList.Add(lRIStr);
      end;
      SetLength(FRIArray, lRIList.Count);
      SetLength(FSelectedRIArray, lRIList.Count);
      SetLength(FDefaultRIArray, lRIList.Count);
      for lIndex := 1 to lRIList.Count do
      begin
        lRIVal := StrToInt(lRIList.Strings[lIndex - 1]);
        FRIArray[lIndex - 1]        := lRIVal;
        FDefaultRIArray[lIndex - 1] := lRIVal;
        FSelectedRIArray[lIndex - 1]:= 0;
      end;
      for lIndex := 1 to lRICount do
      begin
        lRIVal := Trunc(lConfig.RecurrenceIntervalByIndex[lIndex]);
        lRIStr := Format('%4.4d', [lRIVal]);
        lCheckIndex := lRIList.IndexOf(lRIStr);
        if (lCheckIndex >= 0) then
          FSelectedRIArray[lCheckIndex] := lRIVal;
      end;
    finally
      FreeAndNil(lRIList);
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputLongtermSupplyValidator.OnShowRISelectorClick(Sender: TObject);
const OPNAME = 'TOutputLongtermSupplyValidator.OnShowRISelectorClick';
var
  LSavedValues,
  LYearValues : TIntegerArray;
  LIndex: integer;
  LForm : TAbstractForm;
  LNoOfYears : integer;
begin
  if not Assigned(ViewDialog.IFRCurves.IFRCurveChart) then Exit;
  if (FTimeStep <> otsSequence) then
  begin
    ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg1'));
    Exit;
  end;
  try
    LForm              := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    FRISelector        := TRISelector.Create(LForm, FAppModules);
    FRISelector.Parent := LForm;
    FRISelector.Align  := alClient;
    try
      SetLength(LSavedValues, Length(FSelectedRIArray));
      SetLength(LYearValues, Length(FRIArray));
      LNoOfYears := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;

      for LIndex := Low(LSavedValues) to High(LSavedValues) do
        LSavedValues[LIndex] := FSelectedRIArray[LIndex];
      for LIndex := Low(LYearValues) to High(LYearValues) do
        LYearValues[LIndex] := FRIArray[LIndex];

      FRISelector.LanguageHasChanged;
      FRISelector.PopulateRecurrance(LYearValues, LSavedValues, LNoOfYears, TRUE);
      //FRISelector.RISelector.OnClick := OnRISelectorClick;
      //FRISelector.BtnAdd.OnClick     := OnRISelectorBtnAddClick;
      //FRISelector.BtnDelete.OnClick  := OnRISelectorBtnDeleteClick;
      LForm.ShowModal;
      if(LForm.ModalResult = mrOk) then
      begin
        SetLength(LSavedValues, Length(FSelectedRIArray));
        SetLength(LYearValues, Length(FRIArray));

        for LIndex := Low(LSavedValues) to High(LSavedValues) do
          LSavedValues[LIndex] := FSelectedRIArray[LIndex];
        for LIndex := Low(LYearValues) to High(LYearValues) do
          LYearValues[LIndex] := FRIArray[LIndex];

        FRISelector.ReadRecurranceSaved(LSavedValues, LYearValues);
        for LIndex := Low(FSelectedRIArray) to High(FSelectedRIArray) do
          FSelectedRIArray[LIndex] := LSavedValues[LIndex];
        PopulateRecurrenceIntervals;
        FShowCompliance := False;
        PopulateCompliance;
      end;
    finally
      FreeAndNil(FRISelector);
      FreeAndNil(LForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.InArray(AValue : integer; AArray : TIntegerArray) : boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.InArray';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := Low(AArray) to High(AArray) do
      if (AValue = AArray[LIndex]) then
      begin
        Result := True;
        Break;
      end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TOutputLongtermSupplyValidator.Initialise: boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.Initialise';
var
  LIndex : integer;
  LRunConfig : IRunConfigurationData;
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
    FShowCompliance  := FALSE;
    PopulateRecurrenceIntervals;
    ViewDialog.ViewDataType.Visible := True;
    ViewDialog.ViewDataLabel.Visible := True;
    ViewDialog.ViewDataType.Items.AddObject('ALL',TObject(0));
    LRunConfig := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;
    if LRunConfig <> nil then
    begin
      for LIndex := 1 to LRunConfig.NrOfActiveLoadCases do
        ViewDialog.ViewDataType.Items.AddObject('Target Draft '+IntToStr(LIndex)+' ('+
        FloatToStr(LRunConfig.TargetYieldByIndex[LIndex])+')',TObject(LIndex));
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputFixLongtermSupply');
    ViewDialog.BtnCompliance.Caption := 'Show Assurance';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.SaveState: boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.ViewDialog : TOutputDistributionCurveDialog;
const OPNAME = 'TOutputLongtermSupplyValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputDistributionCurveDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.ClearDataViewer;
const OPNAME = 'TOutputLongtermSupplyValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.PopulateDataViewer;
const OPNAME = 'TOutputLongtermSupplyValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.RePopulateDataViewer;
const OPNAME = 'TOutputLongtermSupplyValidator.RePopulateDataViewer';
var
  LChannel       : IGeneralFlowChannel;
  LError         : string;
  LDataStringList: TStringList;
begin
  try
    LDataStringList := TStringList.Create;
    try
      GetSelectionData;
      if (FIdentifier >= 0) AND (NetworkElementType <> votNone) AND
              (FLoadCase > 0) AND (FSequence >= 0) then
      begin
        FErrorMessage := '';
        ViewDialog.ShowError('');
        LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
        if (lChannel <> nil) then
        begin
          if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetLongtermSupplyData(
             btAnualFirmSelectedYieldDemands,FIdentifier,LDataStringList,LError,FActualDemand) then
          begin
            ViewDialog.IFRCurves.IFRCurveChart.Visible := True;
            PopulateChartData(LDataStringList,lChannel);
            PopulateRecurrenceIntervals;
            PopulateCompliance;
          end
          else
          if lChannel.MasterControlFeature <> nil then
          begin
            if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetLongtermSupplyData(
             btAnualFirmYieldDemands,FIdentifier,LDataStringList,LError,FActualDemand) then
            begin
              ViewDialog.IFRCurves.IFRCurveChart.Visible := True;
              PopulateChartData(LDataStringList,lChannel);
              PopulateRecurrenceIntervals;
              PopulateCompliance;
            end
          end
          else
          begin
           ViewDialog.IFRCurves.IFRCurveChart.Visible := False;
           ViewDialog.ShowError(LError);
          end;
        end;
      end;
    finally
      FreeAndNil(LDataStringList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.ClearChart;
const OPNAME = 'TOutputLongtermSupplyValidator.ClearChart';
begin
  try
    ViewDialog.IFRCurves.ClearChart;
    ViewDialog.IFRCurves.Visible  := False;
    ViewDialog.LineSeries.Clear;
    ViewDialog.LineSeries.Active := False;
    ViewDialog.DemandLineSeries.Clear;
    ViewDialog.DemandLineSeries.Active := False;
    ViewDialog.Chart.LeftAxis.Title.Caption := '';
    ViewDialog.Chart.BottomAxis.Title.Caption := '';
    ViewDialog.Chart.Foot.Text.Clear;
    ViewDialog.Chart.Title.Text.Clear;
    ClearRecurrenceIntervals;
    ClearCompliance;
    ViewDialog.Chart.Legend.Visible := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputLongtermSupplyValidator.PopulateChartData (ADataStringList : TStringList;
                                                               AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputLongtermSupplyValidator.PopulateChartData';
begin
  try
    ClearChart;
    case FTimeStep of
      otsSequence:
        if (AChannel.ChannelType in
           [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,
            ctIrrigationBlockInflowChannel,ctIFRChannel]) then
          PopulateChart(ADataStringList, AChannel)

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.PopulateChart(ADataStringList : TStringList;AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputLongtermSupplyValidator.ToggleSelectedRecurrenceIntervals';
var
  LYearsCount  : integer;
  LCount       : integer;
  LIndex       : integer;
  LXValue      : double;
  LValues      : TStringList;
  LMaxFactor   : double;
begin
  try
    ViewDialog.Chart.LeftAxis.Automatic := False;
    ViewDialog.Chart.Visible := false;
    ViewDialog.IFRCurves.Visible := True;
    ViewDialog.IFRCurves.IFRCurveChart.Visible := True;
    LYearsCount := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
    ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Clear;

    if (FUnits = ouPercentage) then
      ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Add('Variable Longterm Curve For Demand Channel ('+ AChannel.ChannelName + ')')
    else
      ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Add('Constant Longterm Curve For Demand Channel ('+ AChannel.ChannelName + ')');

    ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Add('Number of years analysed = '+ IntToStr(LYearsCount));

    ViewDialog.IFRCurves.IFRCurveChart.Foot.Text.Clear;

    if(FTimeStep= otsSequence) then
      ViewDialog.IFRCurves.IFRCurveChart.Foot.Text.Add('Reliability of  Supply %'{FAppModules.Language.GetString('DistributionCurve.Sequences')});

    ViewDialog.IFRCurves.IFRCurveChart.Title.Visible := TRUE;
    ViewDialog.IFRCurves.IFRCurveChart.Foot.Visible := TRUE;

    if (FUnits = ouPercentage) then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supply')
    else
    if (FUnits = ouPerSecond) then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supplym3')
    else
    if (FUnits = ouMegaLitersPerDay) then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMegaL')
    else
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMillion');

    ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.AxisValuesFormat := '######0.000';
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Increment := 5;
    LValues      := TStringList.Create;
    FFactor := 1;
    LMaxFactor := FFactor;
    ViewDialog.IFRCurves.ClearChart;
    ViewDialog.IFRCurves.PrepareChart;
    FLessValueFound := False;
    try
      if ADataStringList.Count>0 then
      begin
        if ViewDialog.ViewDataType.ItemIndex = 0 then
        begin
          ViewDialog.IFRCurves.IFRCurveChart.Legend.Visible := True;
          ViewDialog.IFRCurves.IFRCurveChart.Legend.Alignment := laBottom;
          FValueList.Clear;
          for LIndex  := 0 to ADataStringList.Count - 1 do
          begin
            LValues.CommaText := ADataStringList[LIndex];
            if ((LValues.Count-1)>0) then
            begin
              if Trim(LValues[0]) <> '' then
              begin
                FActualDemand := StrToFloat(LValues[0]);
                LValues.Delete(0);
              end;
              for LCount := 0 to LValues.Count-1 do
              begin
                LXValue := (LCount)/(LValues.Count) * 100;
                ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex].Visible        := True;
                ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex].SeriesColor     := C_Colors[LIndex];
                ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex].Title := ViewDialog.ViewDataType.Items[LIndex+1];
                ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex].AddXY(LXValue, StrToFloat(LValues[LCount]));
                FValueList.Add(InttoStr(Trunc(LXValue))+'='+FormatFloat('00.0000',StrToFloat(LValues[LCount])));
                FFactor := ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex].YValues.MaxValue;
                if FFactor>LMaxFactor then
                  LMaxFactor := FFactor;
              end;
            end;
          end;
          GetChartLegend(ViewDialog.IFRCurves.IFRCurveChart);
        end
        else
        if ViewDialog.ViewDataType.ItemIndex > 0 then
        begin
          ViewDialog.IFRCurves.IFRCurveChart.Legend.Visible := False;
          FValueList.Clear;
          LValues.CommaText := ADataStringList[ViewDialog.ViewDataType.ItemIndex-1];
          if ((LValues.Count-1)>0) then
          begin
            if Trim(LValues[0]) <> '' then
            begin
              FActualDemand := StrToFloat(LValues[0]);
              LValues.Delete(0);
            end;

            for LCount := 0 to LValues.Count-1 do
            begin
              LXValue := (LCount)/(LValues.Count) * 100;
              ViewDialog.IFRCurves.DefinedLineSeriesArray[ViewDialog.ViewDataType.ItemIndex-1].Visible := True;
              ViewDialog.IFRCurves.DefinedLineSeriesArray[ViewDialog.ViewDataType.ItemIndex-1].SeriesColor := C_Colors[ViewDialog.ViewDataType.ItemIndex-1];
              ViewDialog.IFRCurves.DefinedLineSeriesArray[ViewDialog.ViewDataType.ItemIndex-1].AddXY(LXValue, StrToFloat(LValues[LCount]));
              FValueList.Add(InttoStr(Trunc(LXValue))+'='+FormatFloat('00.0000',StrToFloat(LValues[LCount])));

              FFactor := ViewDialog.IFRCurves.DefinedLineSeriesArray[ViewDialog.ViewDataType.ItemIndex-1].YValues.MaxValue;
              if FFactor>LMaxFactor then
                LMaxFactor := FFactor;
            end;
          end;
        end;
      end;
    finally
      LValues.Free;
    end;

    if (FUnits = ouPercentage) then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.SetMinMax(0, 105)
    else
    begin
      FFactor := FFactor * 0.05;
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.SetMinMax(0, LMaxFactor + FFactor);
    end;
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.SetMinMax(0,100);
    FFactor := LMaxFactor;
    ViewDialog.IFRCurves.IFRCurveChart.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputLongtermSupplyValidator.ToggleSelectedRecurrenceIntervals(AYearValues,ASavedValues: TIntegerArray);
const OPNAME = 'TOutputLongtermSupplyValidator.ToggleSelectedRecurrenceIntervals';
var
  LLineSeries : TLineSeries;
  LIndex      : integer;
begin
  try
    if (Length(AYearValues) > 0) and
       (Length(ASavedValues) > 0) then
    for LIndex := 0 to Length(AYearValues) - 1 do
    begin
      LLineSeries := SeriesByName('1_in_' + IntToStr(AYearValues[LIndex]));
      if Assigned(LLineSeries) then
      begin
        if (ASavedValues[LIndex] = 0) then
          LLineSeries.Active := False
        else
          LLineSeries.Active := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.CalculateXPointStochastic (APointYears : integer;
                                                                      APlaneYears : integer): double;
const OPNAME = 'TOutputLongtermSupplyValidator.CalculateXPointStochastic';
begin
  Result := -1;
  try
    if (APlaneYears>0) then
    begin
      Result := 1 - (1 - Power(1 - (1/APlaneYears),APointYears));
      Result := Result * 100;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.PopulateRecurrenceIntervals;
const OPNAME = 'TOutputLongtermSupplyValidator.PopulateRecurrenceIntervals';
var
  LLineSeries : TLineSeries;
  LIndex,
  LYearsCount : integer;
  LXValue,
  LMinYValue,
  LMaxYValue  : double;
  lMonthlyAnnual : TOutputTimeStep;
begin
  try
    ClearRecurrenceIntervals;
    lMonthlyAnnual := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
    if (lMonthlyAnnual = otsSequence) then
    begin
      if (Length(FRIArray) > 0) and
         (Length(FSelectedRIArray) > 0) then
      begin
        LYearsCount := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
        LMinYValue := ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Minimum;
        LMaxYValue := ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Maximum;
        for LIndex := Low(FRIArray) to High(FRIArray) do
        begin
          LLineSeries              := TLineSeries.Create(ViewDialog.IFRCurves.IFRCurveChart);
          LLineSeries.ParentChart  := ViewDialog.IFRCurves.IFRCurveChart;
          LLineSeries.Active       := False;
          LLineSeries.Title        := '1_in_' + IntToStr(FRIArray[LIndex]);
          ConfigureRILineSeriers(LLineSeries);
          FRILineSeriesList.AddObject(IntToStr(FRILineSeriesList.Count+1), LLineSeries);
          ViewDialog.IFRCurves.IFRCurveChart.AddSeries(LLineSeries);
          LXValue := CalculateXPointStochastic(LYearsCount, FRIArray[LIndex]);
          LLineSeries.AddXY(LXValue, LMinYValue);
          LLineSeries.AddXY(LXValue, LMaxYValue);

        end;
        ToggleSelectedRecurrenceIntervals(FRIArray, FSelectedRIArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.ClearRecurrenceIntervals;
const OPNAME = 'TOutputLongtermSupplyValidator.ClearRecurrenceIntervals';
var
  LLineSeries : TLineSeries;
begin
  try
    while (FRILineSeriesList.Count > 0) do
    begin
      LLineSeries := TLineSeries(FRILineSeriesList.Objects[0]);
      LLineSeries := TLineSeries(FRILineSeriesList.Objects[0]);

      LLineSeries.Clear;
      LLineSeries.Active      := False;
      LLineSeries.ParentChart := nil;
      ViewDialog.IFRCurves.IFRCurveChart.RemoveSeries(LLineSeries);
      FRILineSeriesList.Delete(0);
      FreeAndNil(LLineSeries);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.ConfigureRILineSeriers(ARISeries: TLineSeries);
const OPNAME = 'TOutputLongtermSupplyValidator.ConfigureRILineSeriers';
begin
  try
    if Assigned(ARISeries) then
    begin
      ARISeries.XValues.Order    := loNone;
      ARISeries.Pointer.Visible  := False;
      ARISeries.Marks.Visible    := True;
      ARISeries.Marks.Clip       := True;
      ARISeries.ShowInLegend     := False;
      ARISeries.SeriesColor      := clBlue;
      ARISeries.LinePen.Width    := 1;
      ARISeries.OnGetMarkText    := OnGetLineSeriesMarkText;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.ConfigureBarSeriers(ABarSeries: TBarSeries;AColor : TColor);
const OPNAME = 'TOutputLongtermSupplyValidator.ConfigureBarSeriers';
begin
  try
    if Assigned(ABarSeries) then
    begin
      ABarSeries.XValues.Order    := loNone;

      ABarSeries.Marks.Visible    := False;
      ABarSeries.Marks.Clip       := True;
      ABarSeries.ShowInLegend     := False;

      ABarSeries.SeriesColor      := AColor;

      ABarSeries.MultiBar         := mbStacked;
      ABarSeries.BarWidthPercent  := 3;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputLongtermSupplyValidator.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TOutputLongtermSupplyValidator.OnGetLineSeriesMarkText';
var
  LYearsMarkFormatStr: string;
  LCount,
  LIndex: integer;
  LSeries: TChartSeries;
  LZoomPerc,
  LYValue : Double;
  LXPos,
  LYPos: Longint;
begin
  try
    MarkText := '';
    if (ValueIndex <> 1) then
      Exit;
    if not Assigned(ViewDialog.IFRCurves.IFRCurveChart) then
      Exit;
    LIndex := -1;
    for LCount := 0 to FRILineSeriesList.Count - 1 do
    begin
      LSeries := TChartSeries(FRILineSeriesList.Objects[LCount]);
      if(LSeries = Sender) then
      begin
        LIndex := LCount;
        Break;
      end;
    end;
    LYearsMarkFormatStr := '';
    if (LIndex >= 0) and
       (LIndex < Length(FRIArray)) then
    begin
      LYearsMarkFormatStr := FAppModules.Language.GetString('OutputReview.YearsFormatStr');
      LYearsMarkFormatStr := Format(LYearsMarkFormatStr,[1, FRIArray[LIndex]]);
    end;

    if (LYearsMarkFormatStr <> '') then
    begin
      LYValue  := Sender.YValues.MaxValue - Sender.YValues.MinValue;
      LYValue  := LYValue/3.0;
      LZoomPerc := Sender.ParentChart.BottomAxis.Maximum - Sender.ParentChart.BottomAxis.Minimum;
      LZoomPerc := LZoomPerc/100;
      LXPos   := Sender.CalcXPosValue(Sender.XValue[ValueIndex] - LZoomPerc);
      LYPos   := Sender.CalcYPosValue(LYValue);
      ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.DrawAxisLabel(LXPos,LYPos,90,LYearsMarkFormatStr);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.SeriesByName(AName: string): TLineSeries;
const OPNAME = 'TOutputLongtermSupplyValidator.SeriesByName';
var
  LIndex : integer;
  LSeries : TLineSeries;
begin
  Result := nil;
  try
    for LIndex := 0 to ViewDialog.IFRCurves.IFRCurveChart.SeriesCount - 1 do
    begin
      LSeries := TLineSeries(ViewDialog.IFRCurves.IFRCurveChart.Series[LIndex]);
      if LSeries.Title = AName then
      begin
        Result := LSeries;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.CanExport: boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.CanPrint: boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputLongtermSupplyValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.DoPrint;
const OPNAME = 'TOutputLongtermSupplyValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputLongtermSupplyValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.
    ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdChannelDemands,
    FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputLongtermSupplyValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputLongtermSupplyValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.OnShowComplianceClick (Sender : TObject);
const OPNAME = 'TOutputLongtermSupplyValidator.OnShowComplianceClick';
var
  LChannel  : IGeneralFlowChannel;
  LForm : TAssuranceForm;
begin
  try
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (LChannel <> nil) then
    begin
      LForm := TAssuranceForm.CreateWithoutDFM(nil,FAppModules);
      try
        LForm.Initialise;
        LForm.LanguageHasChanged;
        FAssurenceDialog := TReconciliationAnalysisValidator.Create(LForm,FAppModules);
        FAssurenceDialog.Panel.Parent := LForm;
        FAssurenceDialog.Panel.Align  := alClient;
        FAssurenceDialog.Initialise;
        FAssurenceDialog.PopulateDataViewer;
        FAssurenceDialog.LanguageHasChanged;
        FAssurenceDialog.ReconciliationAnalysisDialog.ReconciliationAnalysisCheck.Visible := False;
        LForm.ShowModal;
        if(LForm.ModalResult = mrOk) then
        begin
          if (FTimeStep <> otsSequence) then
            ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg1'))
          else
          begin
            if (LChannel.MasterControlFeature <> nil) then
              FShowCompliance := True
            else
            if (LChannel.MinMaxFlowConstraint <> nil) then
            begin
              if (LChannel.WaterDemandFeature = nil) then
                ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg2'))
              else
                FShowCompliance := True;
            end
            else
            if (LChannel.SpecifiedDemandFeature <> nil) then
            begin
              if (LChannel.WaterDemandFeature = nil) then
                ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg3'))
              else
                FShowCompliance := True
            end;
          end;
        end;
      finally
        FreeAndNil(FAssurenceDialog);
        FreeAndNil(LForm);
      end;
    end;
    if ViewDialog.ViewDataType.ItemIndex = 0 then
      FShowCompliance := False;
    LoadRecurrenceIntervals;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.CalculateCompliance;
const OPNAME = 'TOutputLongtermSupplyValidator.CalculateCompliance';
var
  LYieldModelData : IYieldModelData;
  LConfig         : IWaterDemandConfiguration;
  LChannel        : IGeneralFlowChannel;
  LRICount        : integer;
  LCategoryCount  : integer;
  LRequired       : double;
  //LProportion     : IWaterUseOutputProportion;
  LRIIndex        : integer;
  LCategoryIndex  : integer;
  LCategory       : IWaterDemandCategory;
  LTotal          : double;
begin
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LConfig         := lYieldModelData.NetworkFeaturesData.WaterDemandConfiguration;
    LChannel        := lYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (LChannel <> nil) and (LConfig <> nil) then
    begin
      LRequired := FActualDemand;
      if (FUnits = ouPercentage) then
        LRequired := 100.0;

      if (FUnits = ouMegaLitersPerDay) then
        LRequired := LRequired * (365.25 * 86400.0) / 1000.0;
      if (FUnits = ouMcmPerMonthOrYear) then
        LRequired := LRequired * (365.25 * 86400.0) /1000000.0;

      LRICount       := LConfig.RiskCriteriaCount;
      LCategoryCount := LConfig.DemandCategoryCount;
      //LProportion    := LConfig.WaterUseOutputProportionByChannelNumber[FIdentifier];
      SetLength(FComplianceArray, LRICount + 1);
      for LRIIndex := LRICount downto 1 do
      begin
        FComplianceArray[LRIIndex] := 0;
        LTotal := 0.0;

        for LCategoryIndex := 1 to LCategoryCount do
        begin
          LCategory := LConfig.DemandCategoryByID[LCategoryIndex];
          LTotal    := LTotal +
                       (LCategory.DemandPortionByIndex[LRIIndex]{ *
                        LProportion.ProportionByIndex[LCategoryIndex]});
        end;

        FComplianceArray[LRIIndex] := LTotal * LRequired;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.ClearCompliance;
const OPNAME = 'TOutputLongtermSupplyValidator.ClearCompliance';
var
  LBarSeries      : TBarSeries;
begin
  try
    while (FComplianceShapeList.Count > 0) do
    begin
      LBarSeries      := TBarSeries(FComplianceShapeList.Items[0]);
      if LBarSeries <> nil then
      begin
        LBarSeries.Active      := False;
        LBarSeries.ParentChart := nil;
        LBarSeries.Clear;
        ViewDialog.IFRCurves.IFRCurveChart.RemoveSeries(LBarSeries);
        FComplianceShapeList.Delete(0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.PopulateCompliance;
const OPNAME = 'TOutputLongtermSupplyValidator.PopulateCompliance';
var
  LYieldModelData : IYieldModelData;
  LConfig         : IWaterDemandConfiguration;
  LIndex          : integer;
  LYearsCount     : integer;
  LXCentre        : double;
  LRInterval      : integer;
  LBarSeries      : TBarSeries;
  LMaxYValue      : double;
  LBarColor       : TColor;
  LCumYValue      : double;
  LOldCursor      : TCursor;
  //LFactor : double;
  //LMaxFactor : double;
begin
  try
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    ClearCompliance;
    if ViewDialog.ViewDataType.ItemIndex = 0 then
      FShowCompliance := False;
    if (FShowCompliance) then
    begin
      LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      LConfig         := lYieldModelData.NetworkFeaturesData.WaterDemandConfiguration;
      LYearsCount     := lYieldModelData.RunConfigurationData.YearsInAnalysis;
      CalculateCompliance;
      LCumYValue := 0.0;
      //LFactor := 1;
      //LMaxFactor := FFactor;
      for LIndex := High(FComplianceArray) downto Low(FComplianceArray) do
      begin
        if (FComplianceArray[LIndex] > 0) then
        begin
          LMaxYValue := FComplianceArray[LIndex];
          LCumYValue := LCumYValue + LMaxYValue;
          LRInterval := Trunc(LConfig.RecurrenceIntervalByIndex[LIndex]);
          if (InArray(LRInterval, FSelectedRIArray)) then
          begin
            LXCentre := CalculateXPointStochastic(LYearsCount, Trunc(LConfig.RecurrenceIntervalByIndex[LIndex]));
            CalculateXPointStochastic(LYearsCount, FRIArray[LIndex]);
            LBarSeries := TBarSeries.Create(ViewDialog.IFRCurves.IFRCurveChart);
            LBarSeries.ParentChart   := ViewDialog.IFRCurves.IFRCurveChart;
            ViewDialog.IFRCurves.IFRCurveChart.AddSeries(LBarSeries);
            FComplianceShapeList.Add(LBarSeries);
            LBarColor := GetBarColorByYValue(LCumYValue,Trunc(LXCentre));
            ConfigureBarSeriers(LBarSeries,LBarColor);
            LBarSeries.AddXY(LXCentre, LMaxYValue);
            //LMaxFactor := LMaxYValue;
            {if FFactor>LMaxFactor then
              LMaxFactor := FFactor;
             }
            {if LMaxFactor<LCumYValue then
              LMaxFactor := LCumYValue;
             }
          end;
        end;
      end;
      if FFactor<LCumYValue then
        LMaxYValue := FFactor+LCumYValue
      else
        LMaxYValue := FFactor;

      if (FUnits = ouPercentage) then
        ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.SetMinMax(0, 105)
      else
      begin
        FFactor := FFactor * 0.05;
        ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.SetMinMax(0, LMaxYValue + FFactor);
      end;
      ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.SetMinMax(0,100);
      ViewDialog.IFRCurves.IFRCurveChart.Repaint;

    end;


    Screen.Cursor := LOldCursor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.GetSelectionData;
const OPNAME = 'TOutputLongtermSupplyValidator.GetSelectionData';
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

procedure TOutputLongtermSupplyValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputLongtermSupplyValidator.PopulateDialogSelectors';
begin
  try
    if (FIdentifier >= 0)  and (NetworkElementType <> votNone)then
      FCurrentViewData := btAnualFirmSelectedYieldDemands;
    OnViewDataTypeChange(nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputLongtermSupplyValidator.SetCurrentViewData;
const OPNAME = 'TOutputLongtermSupplyValidator.SetCurrentViewData';
begin
  try
    if (ViewDialog.cmbViewDataType.ItemIndex < 0) then
      ViewDialog.cmbViewDataType.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputLongtermSupplyValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputLongtermSupplyValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputLongtermSupplyValidator.GetChartLegend(AChart: TFieldChart);
const OPNAME = 'TOutputLongtermSupplyValidator.GetChartLegend';
var
  LIndex : integer;
  LCount : integer;
begin
  try
    LCount := 0;
    for LIndex := 0 to AChart.SeriesCount-1 do
    begin
       if (AChart.Series[LIndex] is TLineSeries) and (AChart.Series[LIndex].Title<>'')then
       begin
           AChart.Series[LIndex].ShowInLegend := True;
           AChart.Series[LIndex].Color        := C_Colors[LCount];
           LCount := LCount+1;
       end
       else
         AChart.Series[LIndex].ShowInLegend := False;

       if not (AChart.Series[LIndex] is TLineSeries) then
         AChart.Series[LIndex].ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputLongtermSupplyValidator.GetBarColorByYValue(AYValue, AXValue : double) : TColor;
const OPNAME = 'TOutputLongtermSupplyValidator.GetBarColorByYValue';
var
  LIndex : integer;
  LYValue : double;
  LXValue : double;
begin
  Result := clBlue;
  try
    for LIndex := 0 to FValueList.Count-1 do
    begin
      if Trim(FValueList[LIndex]) <> '' then
      begin
        LXValue := StrToFloat(FValueList.Names[LIndex]);
        if LXValue >= AXValue then
        begin
          LYValue := StrToFloat(FValueList.Values[FloatToStr(LXValue)]);
          if AYValue > LYValue then
          begin
            Result := clRed;
            Break;
          end
          else
            Break
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
end.

