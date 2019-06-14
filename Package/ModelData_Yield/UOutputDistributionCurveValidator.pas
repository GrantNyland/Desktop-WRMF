//
//
//  UNIT      : Contains the class TOutputDistributionCurveValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputDistributionCurveValidator;

interface

uses
  Classes,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  ContNrs,
  VCLTee.Series,
//  VCLTee.TeEngine,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UIFRFeatures,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputDistributionCurveDialog;

type
  TComplianceArray = array of double;
  TSupplyData = class
    FMonth : integer;
    FYear  : integer;
    FValue : double;
end;
type
  TOutputDistributionCurveValidator = class(TAbstractOutputDialogValidator)
  protected
    FHintWin             : THintWindow;
    FCurrentViewData     : TOutputDataType;
    FPrevViewData        : TOutputDataType;
    FUseUnits            : TOutputUnits;
    FSelectedRIArray     : TIntegerArray;
    FDefaultRIArray      : TIntegerArray;
    FRIArray             : TIntegerArray;
    FComplianceArray     : TComplianceArray;
    FRILineSeriesList    : TStringList;
    FSupplyListList      : TStringList;
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
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnShowComplianceClick(Sender : TObject);
    procedure OnShowRISelectorClick(Sender : TObject);
    procedure OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
    procedure OnBtnDataSelectionClick (Sender: TObject);
    procedure OnViewDataTypeChange(Sender: TObject);
    procedure DoOnGetSupplyFailureMarkText(Sender: TChartSeries; ValueIndex: Integer; Var MarkText: String);
    procedure DoOnclick(Sender: TChartSeries; ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnChartMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure OnChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure DoOnChartExit(Sender : TObject);

    procedure PopulateChannelSupplyChartData(AData: TStrings);
    procedure PopulateRequirement(AIFRFeature: TIFRFeature);
    procedure PopulateDefinedIFR(AIFRFeature: TIFRFeature);
    procedure PopulateReferenceFlow(AIFRFeature : TIFRFeature);

    procedure GetSortedData(AData: TStrings;AMonth : integer);
    procedure GetMonthData(AData: TStrings;AMonth : integer);
    function ConvertToMcM(AValue : double; AIndex : integer) : double;
    function GetMonthDescription(AMonthIndex : integer) : string;
    procedure GetChartLegend(AChart : TFieldChart);

    procedure RePopulateDataViewer;
    procedure PopulateDialogSelectors;
    procedure SetCurrentViewData;
    procedure ClearChart;
    function  ElementName: string;
    function  InArray(AValue : integer; AArray : TIntegerArray) : boolean;
    function  LoadRecurrenceIntervals : boolean;
    function  SeriesByName(AName : string) : TLineSeries;
    procedure InsertIntoArray(var AArray : TIntegerArray; const AValue : integer);
    procedure DeleteFromArray(var AArray : TIntegerArray; const AValue : integer);
    procedure PopulateChartData (ADataStringList : TStringList;
                                 AChannel : IGeneralFlowChannel);
    procedure PopulateRecurrenceIntervals;
    function CalculateXPointHistoric (AYearsAnalysed, ARecurrenceInterval : integer): double;
    function CalculateXPointStochastic (APointYears, APlaneYears : integer): double;
    procedure ClearRecurrenceIntervals;
    procedure ToggleSelectedRecurrenceIntervals(AYearValues, ASavedValues:TIntegerArray);
    procedure ConfigureRILineSeriers(ARISeries : TLineSeries);

    procedure PopulateAnnualChartForYield(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateAnnualChartForDemand(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateAnnualChartForMinMax(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateAnnualChartForDiversion(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateAnnualChartForLoss(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateAnnualChartForMinFlow(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateAnnualIrrigationChart(ADataStringList: TStringList;AChannel : IGeneralFlowChannel);


    procedure PopulateMonthlyGeneralFlowChannelChart(ADataStringList : TStringList; AChannel: IGeneralFlowChannel);

    procedure PopulateMonthlyChartForYield(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateMonthlyIrrigationBlockChannelChart (ADataStringList: TStringList;AChannel : IGeneralFlowChannel);
    procedure PopulateSequencialChart(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);

    procedure PopulateMonthlyChartForDemand(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateMonthlyChartForMinMax(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateMonthlyChartForDiversion(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateMonthlyChartForLoss(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);
    procedure PopulateMonthlyChartForMinFlow(ADataStringList: TStringList; AChannel: IGeneralFlowChannel);

    procedure PopulateMonthlyChart (ASortedValues,ADemandValues : TStringList;
                                    AChannel      : IGeneralFlowChannel);
    procedure PopulateMonthlyChannelAreaChart(ADataStringList : TStringList);

    procedure PopulateAnnualChart (ASortedValues : TStringList;
                                   AChannel      : IGeneralFlowChannel);
    procedure PopulateChannelDemandData(AValues : TStringList; AChannel      : IGeneralFlowChannel);
    procedure CalculateCompliance;
    procedure PopulateCompliance;
    procedure GetSelectionData;
    procedure ClearCompliance;
    procedure AddAllSupplySequencesData(ANumberOfSequences : integer; AData: TStrings);
    procedure AddDemandsToSequence(AData: TStrings);
    procedure GetDemandsAndSupplyFromAllSequences(AData: TStrings;AChannel      : IGeneralFlowChannel);
    procedure GetSortedDemandSupplyData(ASupplyData,ASortedDemandValues,AChannelDemandValues : TStrings);
    procedure PopulateDemandSupplyData(ADataStringList,ASortedDemandValues : TStrings;
                                       AChannel      : IGeneralFlowChannel);


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
  VCL.Graphics,
  VCL.Dialogs,
  Windows,
  UOutputData,
  UConstants,
  UDataSetType,
  VCLTee.TeeShape,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, VCL.CheckLst;

{ TOutputDistributionCurveValidator }

procedure TOutputDistributionCurveValidator.CreateMemberObjects;
const OPNAME = 'TOutputDistributionCurveValidator.CreateMemberObjects';
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
    FSupplyListList      := TStringList.Create;
    FComplianceShapeList := TObjectList.Create(TRUE);
    LoadRecurrenceIntervals;
    FRISelector := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.DestroyMemberObjects;
const OPNAME = 'TOutputDistributionCurveValidator.DestroyMemberObjects';
begin
  try
    FRILineSeriesList := nil;
    FreeAndNil(FSupplyListList);
    FreeAndNil(FComplianceShapeList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.LoadRecurrenceIntervals: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.LoadRecurrenceIntervals';
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

procedure TOutputDistributionCurveValidator.OnShowRISelectorClick(Sender: TObject);
const OPNAME = 'TOutputDistributionCurveValidator.OnShowRISelectorClick';
var
  LSavedValues,
  LYearValues : TIntegerArray;
  LIndex: integer;
  LForm : TAbstractForm;
  LNoOfYears : integer;
begin
  if not Assigned(ViewDialog.Chart) then Exit;
  if (FTimeStep <> otsAnnual) then
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
        PopulateCompliance;
      end;
    finally
      FreeAndNil(FRISelector);
      FreeAndNil(LForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.InArray(AValue : integer; AArray : TIntegerArray) : boolean;
const OPNAME = 'TOutputDistributionCurveValidator.InArray';
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

procedure TOutputDistributionCurveValidator.InsertIntoArray(var AArray : TIntegerArray; const AValue : integer);
const OPNAME = 'TOutputDistributionCurveValidator.InsertIntoArray';
var
  LIndex,
  LCount,
  LPos,
  LInt : integer;
  LArray : TIntegerArray;
begin
  try
    if not InArray(AValue, AArray) then
    begin
      LPos := -1;
      if (AValue < AArray[Low(AArray)]) then
        LPos := Low(AArray)
      else
      if (AValue > AArray[High(AArray)]) then
        LPos := High(AArray) + 1
      else
      for LIndex := 0 to Length(AArray) - 2 do
      begin
        LInt := AArray[LIndex];
        if (AValue > LInt) and
           (AValue < AArray[LIndex + 1]) then
        begin
          LPos := LIndex + 1;
          Break;
        end;
      end;
      SetLength(AArray, Length(AArray) + 1);
      if (LPos >= 0) then
      begin
        for LIndex := High(AArray) downto LPos do
          AArray[LIndex] := AArray[LIndex - 1];
        AArray[LPos] := AValue;
        SetLength(LArray, Length(FSelectedRIArray) + 1);
        LCount := 0;
        for LIndex := Low(LArray) to High(LArray) do
        begin
          if (LIndex = LPos) then
            LArray[LIndex] := 0
          else
          begin
            LArray[LIndex] := FSelectedRIArray[LCount];
            LCount := LCount + 1;
          end;
        end;
        for LIndex := Low(FSelectedRIArray) to High(FSelectedRIArray) do
          FSelectedRIArray[LIndex] := LArray[LIndex];
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputDistributionCurveValidator.DeleteFromArray(var AArray : TIntegerArray; const AValue : integer);
const OPNAME = 'TOutputDistributionCurveValidator.DeleteFromArray';
var
  LIndex,
  LPos : integer;
  LArray : TIntegerArray;
begin
  try
    if not InArray(AValue, FDefaultRIArray) then
    begin
      LPos := -1;
      for LIndex := 0 to High(AArray) do
      begin
        if (AValue = AArray[LIndex]) then
        begin
          LPos := LIndex;
          Break;
        end;
      end;
      if (LPos >= 0) then
      begin
        AArray[LPos] := 0;
        for LIndex := LPos to (High(AArray) - 1) do
          AArray[LIndex] := AArray[LIndex + 1];
        AArray[High(AArray)] := 0;
        SetLength(LArray, Length(FSelectedRIArray));
        for LIndex := 0 to (High(LArray) - 1) do
          if (LIndex >= LPos) then
            LArray[LIndex] := FSelectedRIArray[LIndex + 1]
          else
            LArray[LIndex] := FSelectedRIArray[LIndex];
        SetLength(LArray, Length(LArray) - 1);
        for LIndex := Low(FSelectedRIArray) to High(FSelectedRIArray) do
          FSelectedRIArray[Lindex] := LArray[LIndex];
        SetLength(AArray, Length(AArray) - 1);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputDistributionCurveValidator.Initialise: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
    FShowCompliance  := FALSE;
    PopulateRecurrenceIntervals;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputDistributionCurve');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.SaveState: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.ViewDialog : TOutputDistributionCurveDialog;
const OPNAME = 'TOutputDistributionCurveValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputDistributionCurveDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.ClearDataViewer;
const OPNAME = 'TOutputDistributionCurveValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearChart;
    ViewDialog.cmbViewDataType.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateDataViewer;
const OPNAME = 'TOutputDistributionCurveValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.RePopulateDataViewer;
const OPNAME = 'TOutputDistributionCurveValidator.RePopulateDataViewer';
var
  LChannel       : IGeneralFlowChannel;
  LError         : string;
  LDataStringList: TStringList;
begin
  try
    LDataStringList := TStringList.Create;
    try
      if (NetworkElementType = votChannelArea) then
      begin
        if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetChannelAreaData(
                                 LDataStringList,btMonthlyAverageChannelFlow,FIdentifier,LError)) then
        begin
          ViewDialog.Chart.Visible := True;
          PopulateMonthlyChannelAreaChart(LDataStringList);
        end
        else
        begin
          ViewDialog.Chart.Visible := False;
          ViewDialog.ShowError(LError);
        end;
      end
      else

      begin
        LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
        GetSelectionData;
        (*
        if(LChannel.ChannelType = 8) and (LChannel.IFRFeature <> nil) then
        begin
          if FEventNoAction then Exit;
          ClearChart;
          if(FIdentifier      >= 0) and
            (NetworkElementType   <> votNone) and
            (FCurrentViewData <> btNone) then
          begin
            LError := '';
            ViewDialog.ShowError(LError);
            case FCurrentViewData of
              btMonthlyAverageChannelFlow :
              begin
                ViewDialog.CurvesDescrLabel.Caption :=
                                          FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedIFRDescr');
                if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                   LDataStringList,FCurrentViewData,FIdentifier,LError)) then
                  PopulateChannelSupplyChartData(LDataStringList)

              else
                ViewDialog.ShowError(LError);
              end;
              {
              btIFRRequirement :
              begin
                ViewDialog.CurvesDescrLabel.Caption :=
                                         FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFRDescr');
                LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                              CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
                PopulateRequirement(LIFRFeature);
              end;
              btDefinedIFR :
              begin
                ViewDialog.CurvesDescrLabel.Caption :=
                                         FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFRDescr');
                LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
                PopulateDefinedIFR(LIFRFeature);
              end;
              btIFRFlow:
              begin
                ViewDialog.CurvesDescrLabel.Caption :=
                                         FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlowDescr');
                LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                               CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
                PopulateReferenceFlow(LIFRFeature);
              end;
              }
            end
          end;
        end
        else
        *)
        if (LChannel.ChannelType  = 12) or (LChannel.ChannelType  = 10) or
           (LChannel.ChannelType  = 15) then
        begin

          if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                                   LDataStringList,btMonthlyAverageChannelFlow,FIdentifier,LError)) then
          begin
            ViewDialog.Chart.Visible := True;
            PopulateMonthlyGeneralFlowChannelChart(LDataStringList,LChannel);
          end
          else
          begin
            ViewDialog.Chart.Visible := False;
            ViewDialog.ShowError(LError);
          end;
        end
        else if (FIdentifier >= 0) AND (NetworkElementType <> votNone) AND
                (FLoadCase > 0) AND (FSequence >= 0) then
        begin
          FErrorMessage := '';
          ViewDialog.ShowError('');
          lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
          if (lChannel <> nil) then
          begin
            if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockDataSet(
               btMonthlyAverageChannelFlow,FIdentifier,LDataStringList,LError) then
            begin
              ViewDialog.Chart.Visible := True;
              PopulateChartData(LDataStringList,lChannel);
              PopulateRecurrenceIntervals;
              PopulateCompliance;
            end
            else
            begin
             ViewDialog.Chart.Visible := False;
             ViewDialog.ShowError(LError);
            end;
          end;
        end;
      end;
      if ViewDialog.DemandLineSeries.Active then
      begin
        ViewDialog.LineSeries.Active    := (FValueType = ovtDemandAndSupply);
        ViewDialog.LineSeries.Name := 'Supply';
        ViewDialog.LineSeries.OnGetMarkText := DoOnGetSupplyFailureMarkText;
        ViewDialog.LineSeries.OnClick := DoOnclick;
        ViewDialog.DemandLineSeries.Name := 'Demand';
        ViewDialog.Chart.OnMouseDown := OnChartMouseDown;
        ViewDialog.Chart.OnMouseMove := OnChartMouseMove;
        //ViewDialog.StudyHasChanged := DoOnChartExit;
        ViewDialog.Chart.Legend.Visible := ViewDialog.DemandLineSeries.Active and
                                           ViewDialog.LineSeries.Active and (FUnits<>ouPercentage);

      end;
    finally
      FreeAndNil(LDataStringList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.ClearChart;
const OPNAME = 'TOutputDistributionCurveValidator.ClearChart';
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

function TOutputDistributionCurveValidator.ElementName: string;
const OPNAME = 'TOutputDistributionCurveValidator.ElementName';
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
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateChartData (ADataStringList : TStringList;
                                                               AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateChartData';
begin
  try
    ClearChart;
    case FTimeStep of
      otsMonthly :
        if (AChannel.MasterControlFeature <> nil) then
          PopulateMonthlyChartForYield(ADataStringList, AChannel)
        else
        if (AChannel.SpecifiedDemandFeature <> nil) then
          PopulateMonthlyChartForDemand(ADataStringList, AChannel)
        else
        if (AChannel.MinMaxFlowConstraint <> nil) and (AChannel.IFRFeature = nil) then
          PopulateMonthlyChartForMinMax(ADataStringList, AChannel)
        else
        if (AChannel.DiversionFeature <> nil) then
          PopulateMonthlyChartForDiversion(ADataStringList, AChannel)
        else
        if (AChannel.LossFeature <> nil) then
          PopulateMonthlyChartForLoss(ADataStringList, AChannel)
        else
        if (AChannel.MinimumFlowConstraint <> nil) then
          PopulateMonthlyChartForMinFlow(ADataStringList, AChannel)
        else
        if (AChannel.IFRFeature <> nil) then
          PopulateMonthlyChartForMinMax(ADataStringList, AChannel)
        else
        if (AChannel.ChannelType = 14) then
          PopulateMonthlyIrrigationBlockChannelChart(ADataStringList, AChannel);
      otsAnnual  :
        if (AChannel.MasterControlFeature <> nil) then
          PopulateAnnualChartForYield(ADataStringList, AChannel)
        else
        if (AChannel.SpecifiedDemandFeature <> nil) then
          PopulateAnnualChartForDemand(ADataStringList, AChannel)
        else
        if (AChannel.MinMaxFlowConstraint <> nil) then
          PopulateAnnualChartForMinMax(ADataStringList, AChannel)
        else
        if (AChannel.DiversionFeature <> nil) then
          PopulateAnnualChartForDiversion(ADataStringList, AChannel)
        else
        if (AChannel.LossFeature <> nil) then
          PopulateAnnualChartForLoss(ADataStringList, AChannel)
        else
        if (AChannel.MinimumFlowConstraint <> nil) then
          PopulateAnnualChartForMinFlow(ADataStringList, AChannel)
        else
        if (AChannel.ChannelType = 14) then
          PopulateAnnualIrrigationChart(ADataStringList, AChannel);
      otsSequence:
        if (AChannel.MasterControlFeature <> nil) or (AChannel.ChannelType = 14) or
          ((AChannel.MinMaxFlowConstraint <> nil) and (AChannel.IFRFeature = nil)) then
          PopulateSequencialChart(ADataStringList, AChannel)


    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateAnnualChartForYield (ADataStringList: TStringList;
                                                                         AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualChartForYield';
var
  LChannelDemandValues,
  LSortedDemandValues,
  lSortedValues : TStringList;
  LRequired     : double;
  LProduced     : double;
  LYValue       : double;
  LCount        : integer;
  LDoDemands    : boolean;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    LSortedDemandValues :=  TStringList.Create;
    try
      PopulateChannelDemandData(LChannelDemandValues,AChannel);
      LDoDemands :=  (LChannelDemandValues.Count>0);
      if (LDoDemands) then
      begin
        GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
        PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
      end
      else
      begin
        lSortedValues.Sorted := True;
        lSortedValues.Duplicates := dupAccept;
        if (ADataStringList <> nil) then
        begin
          ViewDialog.LineSeries.Active := True;
          for LCount := 0 to ADataStringList.Count - 1 do
          begin
            if pos('AVE:',ADataStringList.Strings[LCount]) > 0 then
            begin
              LProduced := StrToFloat(copy(ADataStringList.Strings[LCount],5,length(ADataStringList.Strings[LCount])));
              if (FUnits = ouPercentage) then
              begin
                LRequired := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.TargetYieldByIndex[FLoadCase];
                LRequired := (LRequired * 1000000.0)/(365.25 * 86400.0);
                if(LRequired > 0) then
                  LYValue := LProduced/(LRequired) * 100.0
                else
                  LYValue := 0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                LYValue := LProduced;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                LYValue := LProduced * (365.25* 86400.0) / 1000.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              begin
                LYValue := LProduced * (365.25 * 86400.0) /1000000.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end;
            end;

          end;
          PopulateAnnualChart(lSortedValues, AChannel);
        end;
      end;
    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      LSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateAnnualIrrigationChart (ADataStringList: TStringList;
                                                                         AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualIrrigationChart';
var
  LChannelDemandValues,
  LSortedDemandValues,
  lSortedValues : TStringList;
  LRequired     : double;
  LProduced     : double;
  LYValue       : double;
  LCount        : integer;
  LDoDemands    : boolean;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    LSortedDemandValues :=  TStringList.Create;
    try
      PopulateChannelDemandData(LChannelDemandValues,AChannel);
      LDoDemands :=  (LChannelDemandValues.Count>0);
      if (LDoDemands) then
      begin
        GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
        PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
      end
      else
      begin
        lSortedValues.Sorted := True;
        lSortedValues.Duplicates := dupAccept;
        if (ADataStringList <> nil) then
        begin
          ViewDialog.LineSeries.Active := True;
          for LCount := 0 to ADataStringList.Count - 1 do
          begin
            if pos('AVE:',ADataStringList.Strings[LCount]) > 0 then
            begin
              LProduced := StrToFloat(copy(ADataStringList.Strings[LCount],5,length(ADataStringList.Strings[LCount])));
              if (FUnits = ouPercentage) then
              begin
                LRequired := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.TargetYieldByIndex[FLoadCase];
                LRequired := (LRequired * 1000000.0)/(365.25 * 86400.0);
                if(LRequired > 0) then
                  LYValue := LProduced/(LRequired) * 100.0
                else
                  LYValue := 0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                LYValue := LProduced;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                LYValue := LProduced * (365.25* 86400.0) / 1000.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              begin
                LYValue := LProduced * (365.25 * 86400.0) /1000000.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end;
            end;

          end;
          PopulateAnnualChart(lSortedValues, AChannel);
        end;
      end;
    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      LSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDistributionCurveValidator.PopulateAnnualChart (ASortedValues : TStringList;
                                                                 AChannel      : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualChart';
var
  LYearsCount  : integer;
  LCount       : integer;
  LIndex       : integer;
  LFactor      : double;
  LXValue      : double;
begin
  try
    lCount := 0;
    ViewDialog.Chart.LeftAxis.Automatic := False;
    lYearsCount := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;

    ViewDialog.Chart.Title.Text.Add('Annual distribution curve ('+ AChannel.ChannelName + ')');
    ViewDialog.Chart.Title.Text.Add('Number of years analysed = '+ IntToStr(LYearsCount));
    //ViewDialog.Chart.Foot.Text.Add('Years (as % of total number analysed)');
    if (FTimeStep= otsMonthly) then
      ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Months'))
    else
    if (FTimeStep= otsAnnual) then
      ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Annual'))
    else
    if(FTimeStep= otsSequence) then
      ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Sequences'));

    ViewDialog.Chart.Title.Visible := TRUE;
    ViewDialog.Chart.Foot.Visible := TRUE;

    if (FUnits = ouPercentage) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supply')
    else
    if (FUnits = ouPerSecond) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supplym3')
    else
    if (FUnits = ouMegaLitersPerDay) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMegaL')
    else
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMillion');

    ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.000';

    for lIndex  := ASortedValues.Count - 1 downto 0 do
    begin
      lCount  := lCount + 1;
      lXValue := (lCount / ASortedValues.Count) * 100;
      ViewDialog.LineSeries.AddXY(lXValue, StrToFloat(ASortedValues[LIndex]));
    end;
    if (FUnits = ouPercentage) then
      ViewDialog.Chart.LeftAxis.SetMinMax(0, 105)
    else
    begin
      lFactor := ViewDialog.LineSeries.YValues.MaxValue;
      lFactor := lFactor * 0.05;
      ViewDialog.Chart.LeftAxis.SetMinMax(0, ViewDialog.LineSeries.YValues.MaxValue + lFactor);
    end;
    ViewDialog.Chart.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDistributionCurveValidator.PopulateDemandSupplyData(ADataStringList,ASortedDemandValues : TStrings;
                                                                     AChannel      : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateDemandSupplyData';
var
  lIndex      : integer;
  lCount      : integer;
  lYearsCount : integer;
  lXValue     : double;
  LFactor     : double;
  LDateStr    : string;
  LSeriesIndex : integer;
  lSortedSupplyValues    : TStringList;
begin
  try
    if FValueType in [ovtDeficits,ovtDemand,ovtDemandAndSupply] then
    begin
      lCount := 0;
      ViewDialog.Chart.LeftAxis.Automatic := True;
      lYearsCount := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
      if(AChannel <> nil) then
      begin
        ViewDialog.Chart.Title.Text.Add('Monthly distribution curve ('+ AChannel.ChannelName + ')');
        ViewDialog.Chart.Title.Text.Add('Number of month analysed = 12*'+IntToStr(LYearsCount)+' = '+ IntToStr(LYearsCount*12));

        //ViewDialog.Chart.Foot.Text.Add('Months (as % of total number analysed)');
        if (FTimeStep= otsMonthly) then
          ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Months'))
        else
        if (FTimeStep= otsAnnual) then
          ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Annual'))
        else
        if(FTimeStep= otsSequence) then
          ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Sequences'));

        ViewDialog.Chart.Title.Visible := TRUE;
        ViewDialog.Chart.Foot.Visible := TRUE;
      end;

      if (FUnits = ouPercentage) then
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.DemandSupply')
      else
      if (FUnits = ouPerSecond) then
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.DemandSupplym3')
      else
      if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.DemandSupplyMegaL')
      else
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.DemandSupplyMillion');

      if (FValueType=ovtDemand) then
      begin
        if (FUnits = ouPercentage) then
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Demand')
        else
        if (FUnits = ouPerSecond) then
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Demandm3')
        else
        if (FUnits = ouMegaLitersPerDay) then
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.DemandMegaL')
        else
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.DemandMillion');

      end;

      ViewDialog.DemandLineSeries.Active := True;
      ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.000';
      lSortedSupplyValues := TStringList.Create;
      try
        lSortedSupplyValues.Sorted := True;
        lSortedSupplyValues.Duplicates  := dupAccept;
        for lIndex  := ASortedDemandValues.Count -1 downto 0 do
        begin
          lSortedSupplyValues.Add(FormatFloat('000000000000.000',TSupplyData(ASortedDemandValues.Objects[lIndex]).FValue));
        end;

        FSupplyListList.Clear;
        for lIndex  := ASortedDemandValues.Count -1 downto 0 do
        begin
          lCount  := lCount + 1;
          lXValue := (LCount / ASortedDemandValues.Count) * 100;
          if (FValueType=ovtDemandAndSupply)and(FUnits = ouPercentage) then
          begin
            if (StrToFloat(ASortedDemandValues[lIndex])>0) then
              ViewDialog.DemandLineSeries.AddXY(lXValue,StrToFloat(ASortedDemandValues[lIndex]));
          end
          else
            ViewDialog.DemandLineSeries.AddXY(lXValue, StrToFloat(ASortedDemandValues[lIndex]));
          if (FValueType=ovtDemandAndSupply) and (FUnits<>ouPercentage) then
          begin
            LDateStr := FormatDateTime('mmm yyyy',encodedate(TSupplyData(ASortedDemandValues.Objects[lIndex]).FYear,
                                      TSupplyData(ASortedDemandValues.Objects[lIndex]).FMonth,1));
            LSeriesIndex := ViewDialog.LineSeries.AddXY(lXValue, StrToFloat(lSortedSupplyValues[lIndex]));
            //LSeriesIndex := ViewDialog.LineSeries.AddXY(lXValue, TSupplyData(ASortedDemandValues.Objects[lIndex]).FValue);
            FSupplyListList.AddObject(IntToStr(LSeriesIndex),
            TSupplyData(ASortedDemandValues.Objects[lIndex]));

          end;
        end;
      finally
        lSortedSupplyValues.Free;
      end;

      if (FUnits = ouPercentage) then
        ViewDialog.Chart.LeftAxis.SetMinMax( 0, 105 )
      else
      begin
        LFactor := ViewDialog.DemandLineSeries.YValues.MaxValue;
        LFactor := LFactor * 0.05;
        ViewDialog.Chart.LeftAxis.SetMinMax( 0, ViewDialog.DemandLineSeries.YValues.MaxValue+ LFactor);

      end;
      ViewDialog.Chart.Repaint;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDistributionCurveValidator.AddAllSupplySequencesData(ANumberOfSequences : integer; AData: TStrings);
const OPNAME = 'TOutputDistributionCurveValidator.AddAllSupplySequencesData';
var
  LIndex : integer;
  //LTimeStepperLabel : string;
  LDataStringList : TStringList;
  //LSourceDataList : TStringList;
  LDestDataList   : TStringList;
  LError : string;
  //LMonth : integer;
  //LDestinationValue,
  //LSourceValue : double;
  LDataSelection : IOutputDataSelection;
begin
  try
    LDataStringList := TStringList.Create;
    //LSourceDataList := TStringList.Create;
    LDestDataList   := TStringList.Create;
    try
      AData.Clear;
      //LTimeStepperLabel := FAppModules.Model.ModelName + '_TimeStepper';
      LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
      for LIndex := 1 to ANumberOfSequences do
      begin
        //FAppModules.ViewIni.WriteInteger(LTimeStepperLabel,'Sequence',LIndex);
        LDataSelection.Sequence :=  LIndex;
        GetSelectionData;
        LDataStringList.Clear;
        if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockDataSet(
                 btMonthlyAverageChannelFlow,FIdentifier,LDataStringList,LError) then
        begin
           LDestDataList.AddStrings(LDataStringList)

          {if LDestDataList.Count=0 then
            LDestDataList.AddStrings(LDataStringList)
          else
          begin
            for LMonth := 0 to LDataStringList.Count-1 do
            begin
              if LMonth>= LDestDataList.Count then break;
              if pos('AVE:',LDataStringList.Strings[LMonth]) > 0 then
              begin
                LSourceValue := StrToFloat(copy(LDataStringList.Strings[LMonth],5,length(LDataStringList.Strings[LMonth])));
                LDestinationValue := StrToFloat(copy(LDestDataList.Strings[LMonth],5,length(LDestDataList.Strings[LMonth]))); //StrToFloat(LDestDataList[LMonth]);
                LDestDataList[LMonth] := FormatFloat('AVE:##0.000',LSourceValue+LDestinationValue);
              end
              else
              begin
                LSourceValue      := StrToFloat(LDataStringList[LMonth]);
                LDestinationValue := StrToFloat(LDestDataList[LMonth]);
                LDestDataList[LMonth] := FormatFloat('##0.000',LSourceValue+LDestinationValue);
              end;
            end;
          end;}
        end
        else
          ShowMessage(LError);
      end;
      AData.Assign(LDestDataList);
    finally
      FreeAndNil(LDataStringList);
      //FreeAndNil(LSourceDataList);
      FreeAndNil(LDestDataList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.GetDemandsAndSupplyFromAllSequences(AData: TStrings;AChannel      : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.GetDemandsAndSupplyFromAllSequences';
var
  LIndex : integer;

  LDataStringList : TStringList;
  LSourceDataList : TStringList;
  LDestDataList   : TStringList;
  LYearValues     : TStringList;
  //LStringValue,
  //LData,
  LError : string;
  //LMonthIndex,
  LMonth : integer;
  //LDestinationValue,
  //LSourceValue : double;
  LDataSelection : IOutputDataSelection;
  LNumberOfSequences : integer;
  LYearIndex : integer;
  LYear : integer;
begin
  try
    LDataStringList := TStringList.Create;
    LSourceDataList := TStringList.Create;
    LDestDataList   := TStringList.Create;
    LYearValues     := TStringList.Create;
    try
      AData.Clear;
      LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
      LNumberOfSequences := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis;
      for LIndex := 1 to LNumberOfSequences do
      begin
        LDataSelection.Sequence :=  LIndex;
        GetSelectionData;
        LDataStringList.Clear;
        if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDemandOutputData.GetChannelDemandValues(
           LDataStringList,AChannel.ChannelNumber,LError) then
        begin
          //if LDestDataList.Count=0 then
          //begin
            //LYear := 0;
            for LYearIndex := 0 to LDataStringList.Count-2 do
            begin
              LYearValues.CommaText := LDataStringList[LYearIndex];
              //if (LYearIndex=0)or(LYearIndex <> LDataStringList.Count-1) then
                LYear := StrToInt(LYearValues[0]);
              for LMonth := 1 to 12 do
                LDestDataList.AddObject(LYearValues[LMonth]+'_'+IntToStr(LMonth),TObject(LYear));
            end;
          //end;
          {else
          begin
            LMonthIndex := 0;
            for LYearIndex := 0 to LDataStringList.Count-1 do
            begin
              LYearValues.CommaText := LDataStringList[LYearIndex];
              if LYearIndex>= LDestDataList.Count then break;
              for LMonth := 1 to 12 do
              begin
                if LMonthIndex>=LDestDataList.Count then Break;
                LData := LDestDataList[LMonthIndex];
                if (Pos('_',LData)<0) then Break;
                LStringValue    := Copy(LData,1,Pos('_',LData)-1);
                LSourceValue     := StrToFloatDef(LStringValue,0.0);
                LDestinationValue := StrToFloat(LYearValues[LMonth]);
                LDestDataList.Strings[LMonthIndex] := FloatToStr(LSourceValue+LDestinationValue)+'_'+IntToStr(LMonth);
                LMonthIndex := LMonthIndex+1;
              end

            end;
          end;}
        end;
      end;
      AData.Assign(LDestDataList);
    finally
      FreeAndNil(LDataStringList);
      FreeAndNil(LSourceDataList);
      FreeAndNil(LDestDataList);
      FreeAndNil(LYearValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDistributionCurveValidator.AddDemandsToSequence(AData: TStrings);
const OPNAME = 'TOutputDistributionCurveValidator.AddDemandsToSequence';
var
  LSeq : integer;
  LStringValue : string;
  LFloatValue : double;
  LData : string;
  LIndex : integer;
  LMonth : string;
begin
  try
    LSeq := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis;
    if (LSeq>=0) and (AData.Count>0)  then
    begin
      try
        for LIndex := 0 to AData.Count-1 do
        begin
          LData := AData.Strings[LIndex];
          if (Pos('_',LData)<0) then Continue;
          LStringValue    := Copy(LData,Pos('_',LData)+1,Pos('_',LData)-1);
          LMonth := LStringValue;
          LStringValue    := Copy(LData,1,Pos('_',LData)-1);
          LFloatValue     := StrToFloatDef(LStringValue,0.0);
          AData.Strings[LIndex] := FloatToStr(LFloatValue*LSeq)+'_'+LMonth;
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.GetSortedDemandSupplyData(ASupplyData,ASortedDemandValues,AChannelDemandValues : TStrings);
const OPNAME = 'TOutputDistributionCurveValidator.GetSortedDemandSupplyData';
var
  LMonthData : TStringList;
  LData : string;
  LIndex : integer;
  LStringValue : string;
  LSupplyValue,
  LAvgFloatValue,
  LAvgSupplyValue,
  LFloatValue : double;
  LMonth,
  LError,
  LYear : integer;
  LSupplyData : TSupplyData;
  LCount : integer;
  LMonthIndex : integer;
  LDaysPerMonth : array[1..12] of double;
begin
  try
    LMonthData := TStringList.Create;
    try
      LMonthData.Sorted := True;
      LMonthData.Duplicates := dupAccept;
      ASortedDemandValues.Clear;
      LAvgFloatValue := 0;
      LAvgSupplyValue := 0;
      for LCount := 1 to 12 do
        LDaysPerMonth[LCount] := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData.MonthDaysByIndex[LCount];
      LCount := 0;
      LMonthIndex := 0;
      for LIndex := 0 to AChannelDemandValues.Count-1 do
      begin
        LMonthIndex := LMonthIndex + 1;
        if(LMonthIndex > 12) then
          LMonthIndex := 1;
        LData := AChannelDemandValues.Strings[LIndex];
        LYear := Integer(AChannelDemandValues.Objects[LIndex]);
        if (Pos('_',LData)<0) then Continue;
        LStringValue    := Copy(LData,Pos('_',LData)+1,Pos('_',LData)-1);
        LMonth := StrToIntDef(LStringValue,0);
        LStringValue    := Copy(LData,1,Pos('_',LData)-1);
        LFloatValue     := StrToFloatDef(LStringValue,0.0);

        if (FDisplayMonth=0) then
        begin
          if (ASupplyData.Count-1>LCount) then
          begin

            Val(ASupplyData[LCount],LSupplyValue,LError);

            if LError <> 0 then
            begin
              LCount := LCount+1;
              Val(ASupplyData[LCount],LSupplyValue,LError);
            end;

            LSupplyData := TSupplyData.Create;
            LSupplyData.FMonth := LMonth;
            LSupplyData.FYear := LYear;
            if LSupplyValue>LFloatValue then
              LSupplyValue := LFloatValue;
            LSupplyData.FValue := StrToFloat(FormatFloat('00000000000000.000',LSupplyValue));

            if (FUnits = ouPerSecond) then
            begin
              if (FValueType = ovtDeficits) then
              begin
                if LFloatValue > 0 then
                  LFloatValue := LFloatValue - LSupplyValue;
              end;
            end
            else
            if (FUnits = ouPercentage) then
            begin
              if (FValueType = ovtDeficits) then
              begin
                if LFloatValue > 0 then
                  LFloatValue := (LFloatValue-LSupplyValue)/LFloatValue*100.0;
              end;
              if (FValueType = ovtDemand) then
              begin
                if LFloatValue > 0 then
                  LFloatValue := (LFloatValue-LSupplyValue)/LFloatValue*100.0;
              end;
              if (FValueType=ovtDemandAndSupply) then
              begin
                if LFloatValue > 0 then
                  LFloatValue := LSupplyValue/LFloatValue*100.0;
              end;
            end
            else
            if (FUnits=ouMegaLitersPerDay) then
            begin
              LSupplyData.FValue := StrToFloat(FormatFloat('00000000000000.000',LSupplyValue * (365.25* 86400.0) / 1000.0));
              LFloatValue := LFloatValue * (365.25* 86400.0) / 1000.0;

              if (FValueType = ovtDeficits) then {ovtDeficit}
              begin
                if LFloatValue > 0 then
                  LFloatValue := (LFloatValue - LSupplyData.FValue);
              end;
            end
            else
            if (FUnits = ouMcmPerMonthOrYear) then
            begin
              LSupplyData.FValue  := LSupplyValue * (LDaysPerMonth[LMonthIndex] * 86400.0) /1000000.0;
              LFloatValue := LFloatValue * (LDaysPerMonth[LMonthIndex] * 86400.0) /1000000.0;
              //LSupplyData.FValue := StrToFloat(FormatFloat('00000000000000.000',LSupplyValue * (365.25 * 86400.0) /1000000.0));
              //LFloatValue := LFloatValue * (365.25 * 86400.0) /1000000.0;
              if (FValueType = ovtDeficits) then {ovtDeficit}
              begin
                if LFloatValue > 0 then
                  LFloatValue := (LFloatValue - LSupplyData.FValue);
              end;
            end;
            if FTimeStep=otsAnnual then
            begin
              LAvgFloatValue := LAvgFloatValue+LFloatValue;
              LAvgSupplyValue := LAvgSupplyValue+LSupplyData.FValue;
              if LMonth = 12 then
              begin
                LAvgFloatValue := (LAvgFloatValue/LMonth);
                LAvgSupplyValue := (LAvgSupplyValue/LMonth);
                if LAvgSupplyValue>LAvgFloatValue then
                  LAvgSupplyValue := LAvgFloatValue;
                LSupplyData.FValue := StrToFloat(FormatFloat('00000000000000.000',LAvgSupplyValue));
                LMonthData.AddObject(FormatFloat('00000000000000.000',LAvgFloatValue),LSupplyData);
                LAvgFloatValue := 0;
                LAvgSupplyValue := 0;
              end;
            end
            else
            begin
              LMonthData.AddObject(FormatFloat('00000000000000.000',LFloatValue),LSupplyData);
            end;
          end
          else
          if (ASupplyData.Count=0)then
          begin
            if (FUnits = ouMegaLitersPerDay) then
              LFloatValue := LFloatValue * (365.25* 86400.0) / 1000.0
            else
            if (FUnits = ouMcmPerMonthOrYear) then
              LFloatValue := LFloatValue * (365.25 * 86400.0) /1000000.0;

            if FTimeStep=otsAnnual then
            begin
              LAvgFloatValue := LAvgFloatValue+LFloatValue;
              if LMonth = 12 then
              begin
                LAvgFloatValue := (LAvgFloatValue/LMonth)* 365.25;
                LMonthData.Add(FormatFloat('00000000000000.000',LAvgFloatValue));
                LAvgFloatValue := 0;
              end;
            end
            else
              LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
          end;
        end
        else
        if (FDisplayMonth>0) then
        begin
          if (FDisplayMonth=LMonth) then
          begin
            if ASupplyData.Count-1>LCount then
            begin
              if (LCount>11) then
                LCount := LCount+1;
              Val(ASupplyData[LCount],LSupplyValue,LError);
              if LError <> 0 then
              begin
                LCount := LCount+1;
                Val(ASupplyData[LCount],LSupplyValue,LError);
              end;
              LSupplyData := TSupplyData.Create;
              LSupplyData.FMonth := LMonth;
              LSupplyData.FYear := LYear;
              if LSupplyValue>LFloatValue then
                LSupplyValue := LFloatValue;
              LSupplyData.FValue := StrToFloat(FormatFloat('00000000000000.000',LSupplyValue));
              if (FUnits = ouMegaLitersPerDay) then
              begin
                LSupplyData.FValue := StrToFloat(FormatFloat('00000000000000.000',LSupplyValue * (365.25* 86400.0) / 1000.0));
                LFloatValue := LFloatValue * (365.25* 86400.0) / 1000.0;
                if (FValueType = ovtDeficits) then {ovtDeficit}
                begin
                  if LFloatValue > 0 then
                    LFloatValue := (LFloatValue - LSupplyData.FValue);
                end;
              end
              else
              if (FUnits = ouMcmPerMonthOrYear) then
              begin
                LSupplyData.FValue := StrToFloat(FormatFloat('00000000000000.000',LSupplyValue * (365.25 * 86400.0) /1000000.0));
                LFloatValue := LFloatValue * (365.25 * 86400.0) /1000000.0;
                if (FValueType = ovtDeficits) then {ovtDeficit}
                begin
                  if LFloatValue > 0 then
                    LFloatValue := (LFloatValue - LSupplyData.FValue);
                end;
              end;
              LMonthData.AddObject(FormatFloat('00000000000000.000',LFloatValue),LSupplyData);
            end
            else
            if (ASupplyData.Count=0)then
            begin
              if (FUnits = ouMegaLitersPerDay) then
                LFloatValue := LFloatValue * (365.25* 86400.0) / 1000.0
              else
              if (FUnits = ouMcmPerMonthOrYear) then
                LFloatValue := LFloatValue * (365.25 * 86400.0) /1000000.0;
              LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
            end;
          end;
        end;
        LCount := LCount+1;
      end;
      ASortedDemandValues.Assign(LMonthData);
    finally
      FreeAndNil(LMonthData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateSequencialChart(ADataStringList: TStringList;
                                                                          AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateSequencialChart';
var
  LNumberOfSequences : integer;
begin
  try
    LNumberOfSequences := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis;
    if (LNumberOfSequences>1) and (FTimeStep = otsSequence) then  // and (FValueType<>ovtDemand)
      AddAllSupplySequencesData(LNumberOfSequences,ADataStringList);
    if (ADataStringList.Count>0) or (FValueType = ovtDemand) then
      PopulateMonthlyChartForYield(ADataStringList,AChannel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChartForYield (ADataStringList: TStringList;
                                                                          AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChartForYield';
var
  LMonthIndex    : integer;
  LIndex         : integer;
  lSortedValues  : TStringList;
  LRequired      : double;
  LProduced      : double;
  LDaysInMonth   : double;
  LFactor        : double;
  LYValue        : double;
  LCount         : integer;
  LPos           : integer;
  LPosIndex       : integer;
  LDoDemands      : boolean;
  LChannelDemandValues   : TStringList;
  lSortedDemandValues    : TStringList;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    lSortedDemandValues  := TStringList.Create;
    try
      if (ADataStringList <> nil) then
      begin
        ViewDialog.LineSeries.Active := True;
        PopulateChannelDemandData(LChannelDemandValues,AChannel);
        LDoDemands :=  (LChannelDemandValues.Count>0);
        if (LDoDemands)then
        begin
          GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
          PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
        end
        else
        begin
          lSortedValues.Sorted := True;
          lSortedValues.Duplicates := dupAccept;
          LPos := -1;
          for LCount := 0 to (ADataStringList.Count div 13) -1 do
          begin
            for LIndex := 0 to 12 do
            begin
              LPos := LPos + 1;
              if (LIndex <> 12) then
              begin
                if (FDisplayMonth = 0) OR (FDisplayMonth = LIndex + 1) then
                begin

                  LProduced := StrToFloat(ADataStringList.Strings[LPos]);
                  if (FUnits = ouPercentage) then
                  begin
                    LMonthIndex := LIndex + 1;
                    LRequired   := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.TargetYieldByIndex[FLoadCase];
                    LFactor     := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByIndex[0].FactorByMonth[LMonthIndex];
                    LRequired   := (LRequired * 1000000.0)/(365.25 * 86400.0);
                    if(LRequired > 0) then
                      LYValue := LProduced/(LRequired * LFactor) * 100.0
                    else
                      LYValue := 0;
                    lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                  end
                  else
                  if (FUnits = ouPerSecond) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := LProduced;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := LProduced;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  if (FUnits = ouMegaLitersPerDay) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(86400.0) / 1000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lProduced *(86400.0) / 1000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lProduced *(86400.0) / 1000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  begin
                    LMonthIndex  := LIndex + 1;
                    LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LMonthIndex];
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(LDaysInMonth * 86400.0) / 1000000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lProduced * (LDaysInMonth * 86400.0) / 1000000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lProduced * (LDaysInMonth * 86400.0) / 1000000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end;
                end;
              end;
            end;
          end;
          PopulateMonthlyChart(lSortedValues,lSortedDemandValues, AChannel);
        end;
      end;
    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      lSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDistributionCurveValidator.PopulateMonthlyIrrigationBlockChannelChart (ADataStringList: TStringList;
                                                                          AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyIrrigationBlockChannelChart';
var
  LMonthIndex    : integer;
  LIndex         : integer;
  lSortedValues  : TStringList;
  LRequired      : double;
  LProduced      : double;
  LDaysInMonth   : double;
  LFactor        : double;
  LYValue        : double;
  LCount         : integer;
  LPos           : integer;
  LPosIndex       : integer;
  LDoDemands      : boolean;
  LChannelDemandValues   : TStringList;
  lSortedDemandValues    : TStringList;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    lSortedDemandValues  := TStringList.Create;
    try
      if (ADataStringList <> nil) then
      begin
        ViewDialog.LineSeries.Active := True;
        PopulateChannelDemandData(LChannelDemandValues,AChannel);
        LDoDemands :=  (LChannelDemandValues.Count>0);
        if (LDoDemands)then
        begin
          GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
          PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
        end
        else
        begin
          lSortedValues.Sorted := True;
          lSortedValues.Duplicates := dupAccept;
          LPos := -1;
          for LCount := 0 to (ADataStringList.Count div 13) -1 do
          begin
            for LIndex := 0 to 12 do
            begin
              LPos := LPos + 1;
              if (LIndex <> 12) then
              begin
                if (FDisplayMonth = 0) OR (FDisplayMonth = LIndex + 1) then
                begin

                  LProduced := StrToFloat(ADataStringList.Strings[LPos]);
                  if (FUnits = ouPercentage) then
                  begin
                    LMonthIndex := LIndex + 1;
                    LRequired   := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.TargetYieldByIndex[FLoadCase];
                    LFactor     := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByIndex[0].FactorByMonth[LMonthIndex];
                    LRequired   := (LRequired * 1000000.0)/(365.25 * 86400.0);
                    if(LRequired > 0) then
                      LYValue := LProduced/(LRequired * LFactor) * 100.0
                    else
                      LYValue := 0;
                    lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                  end
                  else
                  if (FUnits = ouPerSecond) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := LProduced;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := LProduced;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  if (FUnits = ouMegaLitersPerDay) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(86400.0) / 1000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lProduced *(86400.0) / 1000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lProduced *(86400.0) / 1000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  begin
                    LMonthIndex  := LIndex + 1;
                    LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LMonthIndex];
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(LDaysInMonth * 86400.0) / 1000000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lProduced * (LDaysInMonth * 86400.0) / 1000000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lProduced * (LDaysInMonth * 86400.0) / 1000000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end;
                end;
              end;
            end;
          end;
          PopulateMonthlyChart(lSortedValues,lSortedDemandValues, AChannel);
        end;
      end;
    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      lSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChartForDemand (ADataStringList: TStringList;
                                                                           AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChartForDemand';
var
  lTimeStep       : integer;
  LIndex          : integer;
  lSortedValues   : TStringList;
  LYValue         : double;
  lDemand         : double;
  lSupply         : double;
  lDemandFeature  : ISpecifiedDemandFeature;
  lDemandStr      : WideString;
  lMonthDays      : array [1..12] of double;
  lYieldModelData : IYieldModelData;
  LCount          : integer;
  LPos            : integer;
  LPosIndex       : integer;
  LDoDemands      : boolean;
  LChannelDemandValues   : TStringList;
  lSortedDemandValues    : TStringList;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    lSortedDemandValues  := TStringList.Create;
    try
      lDemandFeature := AChannel.SpecifiedDemandFeature;
      if (ADataStringList <> nil) then
      begin
        ViewDialog.LineSeries.Active := True;
        PopulateChannelDemandData(LChannelDemandValues,AChannel);
        LDoDemands :=  (LChannelDemandValues.Count>0);
        if (LDoDemands)then
        begin
          GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
          PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
        end
        else
        begin
          lSortedValues.Sorted := True;
          lSortedValues.Duplicates := dupAccept;

          lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
          for LIndex := 1 to 12 do
            lMonthDays[lIndex] := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
          lTimeStep := 0;
          LPos := -1;
          for LCount := 0 to (ADataStringList.Count div 13) - 1 do
          begin
            for LIndex := 0 to 12 do
            begin
              LPos := LPos + 1;
              if (LIndex <> 12) then
              begin
                if (FDisplayMonth = 0) OR (FDisplayMonth = LIndex + 1) then
                begin
                  lTimeStep  := lTimeStep + 1;
                  lSupply    := StrToFloat(ADataStringList.Strings[LPos]);
                  if (FUnits = ouPercentage) then
                  begin
                    if (lDemandFeature.GetMonthlyDemand(lTimeStep, lDemandStr)) then
                      lDemand := Round((StrToFloat(lDemandStr) * 1000000 / (lMonthDays[lIndex+1] * 86400.0)) * 1000) / 1000
                    else
                      lDemand := lSupply;

                    if lDemand > 0  then
                      LYValue := lSupply / lDemand * 100.0
                    else
                      LYValue := 0;
                    lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                  end
                  else
                  if (FUnits = ouPerSecond) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lSupply;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lSupply;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  if (FUnits = ouMegaLitersPerDay) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(86400.0) / 1000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lSupply *(86400.0) / 1000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lSupply *(86400.0) / 1000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(lMonthDays[lIndex] * 86400.0) / 1000000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lSupply * (lMonthDays[lIndex] * 86400.0) / 1000000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lSupply * (lMonthDays[lIndex] * 86400.0) / 1000000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end;
                end;
              end;
            end;
          end;
          PopulateMonthlyChart(lSortedValues,lSortedDemandValues, AChannel);
        end;
      end;
    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      lSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateAnnualChartForDemand (ADataStringList: TStringList;
                                                                          AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualChartForDemand';
var
  LChannelDemandValues,
  LSortedDemandValues,
  lSortedValues  : TStringList;
  lRequired      : double;
  lProduced      : double;
  lYValue        : double;
  lDemandFeature : ISpecifiedDemandFeature;
  lDemandStr     : WideString;
  lYear          : integer;
  LCount         : integer;
  LDoDemands     : boolean;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    LSortedDemandValues :=  TStringList.Create;
    try
      PopulateChannelDemandData(LChannelDemandValues,AChannel);
      LDoDemands :=  (LChannelDemandValues.Count>0);
      if (LDoDemands)then
      begin
        GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
        PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
      end
      else
      begin
        lSortedValues.Sorted := True;
        lSortedValues.Duplicates := dupAccept;
        lDemandFeature := AChannel.SpecifiedDemandFeature;
        if (ADataStringList <> nil) then
        begin
          lYear := (FAppModules.Model.ModelData as IYieldModelData).
                     RunConfigurationData.StartYearOther;
          lYear := lYear - 1;
          ViewDialog.LineSeries.Active := True;
          for LCount := 0 to ADataStringList.Count - 1 do
          begin
            if pos('AVE:',ADataStringList.Strings[LCount]) > 0 then
            begin
              lProduced := StrToFloat(copy(ADataStringList.Strings[LCount],5,length(ADataStringList.Strings[LCount])));
              lYear     := lYear + 1;
              if (FUnits = ouPercentage) then
              begin
                if (lDemandFeature.GetAnnualDemand(lYear, lDemandStr)) then
                  lRequired := Round((StrToFloat(lDemandStr) * 1000000 / (365.25 * 86400.0)) * 1000) / 1000
                else
                  lRequired := lProduced;

                if(LRequired > 0) then
                  lYValue := lProduced / lRequired * 100.0
                else
                  lYValue := 0;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                lYValue := lProduced;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                lYValue := lProduced * (365.25 * (86400.0)) / 1000.0;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end
              else
              begin
                lYValue := lProduced * (365.25 * 86400.0) / 1000000.0;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end;
            end;
          end;
          PopulateAnnualChart(lSortedValues, AChannel);
         end;
       end;
    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      LSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChartForMinMax (ADataStringList: TStringList;
                                                                           AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChartForMinMax';
var
  LMonthIndex     : integer;
  LIndex          : integer;
  LPosIndex       : integer;
  lSortedValues   : TStringList;
  lRequired       : double;
  lProduced       : double;
  LYValue         : double;
  lMonthDays      : array [1..12] of double;
  lYieldModelData : IYieldModelData;
  lMinMaxFeature  : IMinMaxFlowConstraint;
  LCount          : integer;
  LPos            : integer;
  LDoDemands      : boolean;
  LChannelDemandValues   : TStringList;
  lSortedDemandValues    : TStringList;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    lSortedDemandValues  := TStringList.Create;
    try
      lMinMaxFeature := AChannel.MinMaxFlowConstraint;
      if (ADataStringList <> nil) then
      begin
        lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        for LIndex := 1 to 12 do
          lMonthDays[lIndex] := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
        ViewDialog.LineSeries.Active := True;
        PopulateChannelDemandData(LChannelDemandValues,AChannel);
        LDoDemands :=  (LChannelDemandValues.Count>0);
        if (LDoDemands)then
        begin
          GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
          PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
        end
        else
        begin
          lSortedValues.Sorted := True;
          lSortedValues.Duplicates := dupAccept;
          LPos := -1;
          for LCount := 0 to (ADataStringList.Count div 13) -1 do
          begin
            for LIndex := 0 to 12 do
            begin
              LPos := LPos + 1;
              if (LIndex <> 12) then
              begin
                if (FDisplayMonth = 0) OR (FDisplayMonth = LIndex + 1) then
                begin
                  lProduced  := StrToFloat(ADataStringList.Strings[LPos]);
                  if (FUnits = ouPercentage) then
                  begin
                    LMonthIndex := LIndex + 1;
                    lRequired   := lMinMaxFeature.FlowConstraintByArcMonth[1, LMonthIndex];
                    if(LRequired > 0) then
                      LYValue := LProduced / LRequired * 100.0
                    else
                      LYValue := 0;
                    lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                  end
                  else
                  if (FUnits = ouPerSecond) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := LProduced;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := LProduced;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  if (FUnits = ouMegaLitersPerDay) then
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(86400.0) / 1000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lProduced *(86400.0) / 1000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lProduced *(86400.0) / 1000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end
                  else
                  begin
                    if LDoDemands then
                    begin
                      LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                      LYValue := LYValue *(lMonthDays[lIndex] * 86400.0) / 1000000.0;
                      LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                      LYValue := lProduced * (lMonthDays[lIndex] * 86400.0) / 1000000.0;
                      lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                    end
                    else
                    begin
                      LYValue := lProduced * (lMonthDays[lIndex] * 86400.0) / 1000000.0;
                      lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                    end;
                  end;
                end;
              end;
            end;
          end;
          PopulateMonthlyChart(lSortedValues,lSortedDemandValues, AChannel);
        end;
      end;
    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      lSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateAnnualChartForMinMax (ADataStringList: TStringList;
                                                                          AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualChartForMinMax';
var
  lIndex          : integer;
  LChannelDemandValues,
  LSortedDemandValues,
  lSortedValues   : TStringList;
  lRequired       : double;
  lProduced       : double;
  lYValue         : double;
  lMonthDays      : array [1..12] of double;
  lMonthDemands   : array [1..12] of double;
  lYieldModelData : IYieldModelData;
  lMinMaxFeature  : IMinMaxFlowConstraint;
  LCount          : integer;
  LDoDemands      : boolean;
begin
  try
    lSortedValues := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    LSortedDemandValues := TStringList.Create;
    try
      lSortedValues.Sorted := True;
      lSortedValues.Duplicates := dupAccept;
      lMinMaxFeature := AChannel.MinMaxFlowConstraint;
      if (ADataStringList <> nil) then
      begin
        PopulateChannelDemandData(LChannelDemandValues,AChannel);
        LDoDemands :=  (LChannelDemandValues.Count>0);
        if (LDoDemands)then
        begin
          GetSortedDemandSupplyData(ADataStringList,LSortedDemandValues,LChannelDemandValues);
          PopulateDemandSupplyData(ADataStringList,LSortedDemandValues,AChannel);
        end
        else
        begin

          lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
          lRequired := 0.0;
          for LIndex := 1 to 12 do
          begin
            lMonthDays[lIndex]    := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
            lMonthDemands[lIndex] := lMinMaxFeature.FlowConstraintByArcMonth[1, lIndex];
            lRequired             := lRequired + (lMonthDemands[lIndex] * 86400.0 * lMonthDays[lIndex]);
          end;
          lRequired := lRequired / (365.25 * 86400.0);
          ViewDialog.LineSeries.Active := True;
          for LCount := 0 to ADataStringList.Count - 1 do
          begin
            if pos('AVE:',ADataStringList.Strings[LCount]) > 0 then
            begin
              lProduced := StrToFloat(copy(ADataStringList.Strings[LCount],5,length(ADataStringList.Strings[LCount])));
              if (FUnits = ouPercentage) then
              begin
                if(LRequired > 0) then
                  lYValue := lProduced / lRequired * 100.0
                else
                  lYValue := 0;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                LYValue := lProduced;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                lYValue := lProduced * (365.25 * (86400.0)) / 1000.0;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end
              else
              begin
                lYValue := lProduced * (365.25 * 86400.0) / 1000000.0;
                lSortedValues.Add(FormatFloat('000000000000.000', lYValue));
              end;
            end;
          end;
          PopulateAnnualChart(lSortedValues, AChannel);
        end;
     end;

    finally
      lSortedValues.Free;
      LChannelDemandValues.Free;
      LSortedDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChart (ASortedValues,ADemandValues : TStringList;
                                                                  AChannel      : IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChart';
var
  lIndex      : integer;
  lCount      : integer;
  lYearsCount : integer;
  lXValue     : double;
  lFactor     : double;
  LDoDemand   : boolean;
begin
  try
    lCount := 0;
    ViewDialog.Chart.LeftAxis.Automatic := True;
    lYearsCount := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
    if(AChannel <> nil) then
    begin
      ViewDialog.Chart.Title.Text.Add('Monthly distribution curve ('+ AChannel.ChannelName + ')');
      ViewDialog.Chart.Title.Text.Add('Number of month analysed = 12*'+IntToStr(LYearsCount)+' = '+ IntToStr(LYearsCount*12));
      //ViewDialog.Chart.Foot.Text.Add('Months (as % of total number analysed)');
      if (FTimeStep= otsMonthly) then
        ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Months'))
      else
      if (FTimeStep= otsAnnual) then
        ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Annual'))
      else
      if(FTimeStep= otsSequence) then
        ViewDialog.Chart.Foot.Text.Add(FAppModules.Language.GetString('DistributionCurve.Sequences'));

      ViewDialog.Chart.Title.Visible := TRUE;
      ViewDialog.Chart.Foot.Visible := TRUE;
    end;

    if (FUnits = ouPercentage) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supply')
    else
    if (FUnits = ouPerSecond) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supplym3')
    else
    if (FUnits = ouMegaLitersPerDay) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMegaL')
    else
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMillion');

    ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.000';

    LDoDemand := Assigned(ADemandValues) and (ASortedValues.Count = ADemandValues.Count);
    ViewDialog.DemandLineSeries.Active := LDoDemand;
    for lIndex  := ASortedValues.Count -1 downto 0 do
    begin
      lCount  := lCount + 1;
      lXValue := (LCount / ASortedValues.Count) * 100;
      ViewDialog.LineSeries.AddXY(lXValue, StrToFloat(ASortedValues[lIndex]));
      if LDoDemand then
      begin
        ViewDialog.DemandLineSeries.AddXY(lXValue, StrToFloat(ADemandValues[lIndex]));
      end;
    end;
    if (FUnits = ouPercentage) then
      ViewDialog.Chart.LeftAxis.SetMinMax( 0, 105 )
    else
    begin
      LFactor := ViewDialog.LineSeries.YValues.MaxValue;
      LFactor := LFactor * 0.05;
      ViewDialog.Chart.LeftAxis.SetMinMax( 0, ViewDialog.LineSeries.YValues.MaxValue+ LFactor);
    end;
    ViewDialog.Chart.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChannelAreaChart (ADataStringList : TStringList);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChannelAreaChart';
var
  LIndex          : integer;
  LLineData,
  lSortedValues   : TStringList;
  lProduced       : double;
  LYValue         : double;
  lMonthDays      : array [1..12] of double;
  lYieldModelData : IYieldModelData;
  LCount          : integer;
begin
  try
    lSortedValues := TStringList.Create;
    LLineData     := TStringList.Create;
    try
      lSortedValues.Sorted := True;
      lSortedValues.Duplicates := dupAccept;
      if (ADataStringList <> nil) then
      begin
        lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        for LIndex := 1 to 12 do
          lMonthDays[lIndex] := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
        ViewDialog.LineSeries.Active := True;

        for LCount := 0 to ADataStringList.Count -1 do
        begin
          LLineData.CommaText := ADataStringList[LCount];

          for LIndex := 1 to 12 do
          begin
            if(LIndex >= LLineData.Count) then Break;
            if (FDisplayMonth = 0) OR (FDisplayMonth = LIndex) then
            begin
              lProduced  := StrToFloat(LLineData.Strings[LIndex]);
              if (FUnits = ouPercentage) then
              begin
                LYValue   := LProduced/100.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                LYValue := LProduced;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                LYValue := lProduced *(86400.0) / 1000.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              begin
                LYValue := lProduced * (lMonthDays[lIndex] * 86400.0) / 1000000.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end;
            end;
          end;
        end;
        PopulateMonthlyChart(lSortedValues,nil, nil);
      end;
    finally
      lSortedValues.Free;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyGeneralFlowChannelChart (ADataStringList : TStringList;
          AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyGeneralFlowChannelChart';
var
  LIndex          : integer;
  LLineData,
  lSortedValues   : TStringList;
  lProduced       : double;
  LYValue         : double;
  lMonthDays      : array [1..12] of double;
  lYieldModelData : IYieldModelData;
  LCount          : integer;
  LPosIndex       : integer;
  LDoDemands      : boolean;
  LChannelDemandValues   : TStringList;
  lSortedDemandValues    : TStringList;
  LPos                   : integer;
begin
  try
    lSortedValues := TStringList.Create;
    LLineData     := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    lSortedDemandValues  := TStringList.Create;
    try
      if (ADataStringList <> nil) then
      begin
        lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        for LIndex := 1 to 12 do
          lMonthDays[lIndex] := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
        ViewDialog.LineSeries.Active := True;

        PopulateChannelDemandData(LChannelDemandValues,AChannel);
        LDoDemands :=  ((LChannelDemandValues.Count div 13) = ADataStringList.Count);
        if LDoDemands then
        begin
          lSortedDemandValues.Sorted := True;
          lSortedDemandValues.Duplicates := dupAccept;
        end
        else
        begin
          lSortedValues.Sorted := True;
          lSortedValues.Duplicates := dupAccept;
        end;

        LPos := 0;
        for LCount := 0 to ADataStringList.Count -1 do
        begin
          LLineData.CommaText := ADataStringList[LCount];

          for LIndex := 1 to 12 do
          begin
            if(LIndex >= LLineData.Count) then Break;
            if (FDisplayMonth = 0) OR (FDisplayMonth = LIndex) then
            begin
              lProduced  := StrToFloat(LLineData.Strings[LIndex]);
              if (FUnits = ouPercentage) then
              begin
                LYValue   := LProduced/100.0;
                lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                if LDoDemands then
                begin
                  LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                  LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                  LYValue := LProduced;
                  lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                end
                else
                begin
                  LYValue := LProduced;
                  lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end;
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                if LDoDemands then
                begin
                  LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                  LYValue := LYValue *(86400.0) / 1000.0;
                  LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                  LYValue := lProduced *(86400.0) / 1000.0;
                  lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                end
                else
                begin
                  LYValue := lProduced *(86400.0) / 1000.0;
                  lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end;
              end
              else
              begin
                if LDoDemands then
                begin
                  LYValue := StrToFloatDef(LChannelDemandValues[LPos],0.0);
                  LYValue := LYValue *(lMonthDays[lIndex] * 86400.0) / 1000000.0;
                  LPosIndex := lSortedDemandValues.Add(FormatFloat('000000000000.000',LYValue));
                  LYValue := lProduced * (lMonthDays[lIndex] * 86400.0) / 1000000.0;
                  lSortedValues.Insert(LPosIndex,FormatFloat('000000000000.000',LYValue));
                end
                else
                begin
                  LYValue := lProduced * (lMonthDays[lIndex] * 86400.0) / 1000000.0;
                  lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end;
              end;
            end;
            LPos := LPos + 1;
          end;
          LPos := LPos + 1;
        end;
        PopulateMonthlyChart(lSortedValues,lSortedDemandValues, nil);
      end;
    finally
      lSortedValues.Free;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.ToggleSelectedRecurrenceIntervals(AYearValues,ASavedValues: TIntegerArray);
const OPNAME = 'TOutputDistributionCurveValidator.ToggleSelectedRecurrenceIntervals';
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

function TOutputDistributionCurveValidator.CalculateXPointHistoric (AYearsAnalysed      : integer;
                                                                    ARecurrenceInterval : integer): double;
const OPNAME = 'TOutputDistributionCurveValidator.CalculateXPointHistoric';
begin
  Result := -1;
  try
    Result := AYearsAnalysed - (AYearsAnalysed / ARecurrenceInterval);
    Result := Result / AYearsAnalysed;
    Result := Result * 100;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.CalculateXPointStochastic (APointYears : integer;
                                                                      APlaneYears : integer): double;
const OPNAME = 'TOutputDistributionCurveValidator.CalculateXPointStochastic';
begin
  Result := -1;
  try
    Result := 1 - (1 - Power(1 - (1/APointYears),APlaneYears));
    Result := Result * 100;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateRecurrenceIntervals;
const OPNAME = 'TOutputDistributionCurveValidator.PopulateRecurrenceIntervals';
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
    if (lMonthlyAnnual = otsAnnual) then
    begin
      if (Length(FRIArray) > 0) and
         (Length(FSelectedRIArray) > 0) then
      begin
        LYearsCount := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
        LMinYValue := ViewDialog.Chart.LeftAxis.Minimum;
        LMaxYValue := ViewDialog.Chart.LeftAxis.Maximum;
        for LIndex := Low(FRIArray) to High(FRIArray) do
        begin
          LLineSeries              := TLineSeries.Create(ViewDialog.Chart);
          LLineSeries.ParentChart  := ViewDialog.Chart;
          LLineSeries.Active       := False;
          LLineSeries.Title        := '1_in_' + IntToStr(FRIArray[LIndex]);
          ConfigureRILineSeriers(LLineSeries);
          FRILineSeriesList.AddObject(IntToStr(FRILineSeriesList.Count+1), LLineSeries);
          ViewDialog.Chart.AddSeries(LLineSeries);
          LXValue := CalculateXPointHistoric(LYearsCount, FRIArray[LIndex]);
          LLineSeries.AddXY(LXValue, LMinYValue);
          LLineSeries.AddXY(LXValue, LMaxYValue);
        end;
        ToggleSelectedRecurrenceIntervals(FRIArray, FSelectedRIArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.ClearRecurrenceIntervals;
const OPNAME = 'TOutputDistributionCurveValidator.ClearRecurrenceIntervals';
var
  LLineSeries : TLineSeries;
begin
  try
    while (FRILineSeriesList.Count > 0) do
    begin
      LLineSeries := TLineSeries(FRILineSeriesList.Objects[0]);
      LLineSeries.Clear;
      LLineSeries.Active      := False;
      LLineSeries.ParentChart := nil;
      ViewDialog.Chart.RemoveSeries(LLineSeries);
      FRILineSeriesList.Delete(0);
      FreeAndNil(LLineSeries);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.ConfigureRILineSeriers(ARISeries: TLineSeries);
const OPNAME = 'TOutputDistributionCurveValidator.ConfigureRILineSeriers';
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

procedure TOutputDistributionCurveValidator.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TOutputDistributionCurveValidator.OnGetLineSeriesMarkText';
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

    if not Assigned(ViewDialog.Chart) then
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
      ViewDialog.Chart.BottomAxis.DrawAxisLabel(LXPos,LYPos,90,LYearsMarkFormatStr);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDistributionCurveValidator.DoOnGetSupplyFailureMarkText(Sender: TChartSeries; ValueIndex: Integer; Var MarkText: String);
const OPNAME = 'TOutputDistributionCurveValidator.DoOnGetSupplyFailureMarkText';
var
  LYearsMarkFormatStr : string;
  LIndex: integer;
  LSupplyData : TSupplyData;
begin
  try
    MarkText := '';
    {if not FSeriesMarksVisible then
      Exit;}
    if (ValueIndex = 0) then Exit;
     

    if not Assigned(ViewDialog.Chart) then
      Exit;
    LSupplyData := nil;
    if ValueIndex<FSupplyListList.count-1 then
    begin
      LIndex := FSupplyListList.IndexOf(FSupplyListList[ValueIndex]);
      if LIndex>-1 then
        LSupplyData := TSupplyData(FSupplyListList.Objects[LIndex]);

      if (LIndex >= 0) and (LSupplyData <> nil) then
      begin
        LYearsMarkFormatStr := FormatDateTime('mmm yyyy',encodedate(LSupplyData.FYear,LSupplyData.FMonth,1));
      end;

      if (LYearsMarkFormatStr <> '') then
      begin
        MarkText := LYearsMarkFormatStr;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
procedure TOutputDistributionCurveValidator.DoOnSeriesPointerClick(Sender: TCustomSeries; ValueIndex: LongInt; X, Y: Integer);
const OPNAME = 'TOutputDistributionCurveValidator.DoOnSeriesPointerClick';
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
  LIndex : integer;
  LSupplyData : TSupplyData;
  LYearsMarkFormatStr : string;
begin
  try
    if Assigned(FHintWin) then
    begin
      FHintWin.Hide;
      FHintWin := nil;
    end;
    if (Sender is TLineSeries) and
       (ValueIndex >= 0)then
    begin
      GetCursorPos(LPoint);

      LWND                 := WindowFromPoint(LPoint);
      //LPlaneIndex          := TLineSeries(Sender).PlaneIndex;
      LExProb              := TLineSeries(Sender).XScreenToValue(TLineSeries(Sender).CalcXPos(ValueIndex));
      //LRecInt              := YRCGraphDataObject.CalculateRIOnTargetDraft(LPlaneIndex, LTargetDraftIndex, LExProb);
      if ValueIndex<FSupplyListList.count-1 then
      begin
        LIndex := FSupplyListList.IndexOf(FSupplyListList[ValueIndex]);
        if LIndex>-1 then
          LSupplyData := TSupplyData(FSupplyListList.Objects[LIndex]);

        if (LIndex >= 0) and (LSupplyData <> nil) then
        begin
          LYearsMarkFormatStr := FormatDateTime('mmm yyyy',encodedate(LSupplyData.FYear,LSupplyData.FMonth,1));
        end;

      end;

      LHintStr := LYearsMarkFormatStr;
      if (LWND > 0) then
      begin
        FHintWin           := THintWindow.Create(ViewDialog.Chart);
        FHintWin.Parent    := ViewDialog.Chart;
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
*)

function TOutputDistributionCurveValidator.SeriesByName(AName: string): TLineSeries;
const OPNAME = 'TOutputDistributionCurveValidator.SeriesByName';
var
  LIndex : integer;
  LSeries : TLineSeries;
begin
  Result := nil;
  try
    for LIndex := 0 to ViewDialog.Chart.SeriesCount - 1 do
    begin
      LSeries := TLineSeries(ViewDialog.Chart.Series[LIndex]);
      if LSeries.Title = AName then
      begin
        Result := LSeries;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.CanExport: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.CanPrint: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputDistributionCurveValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.DoPrint;
const OPNAME = 'TOutputDistributionCurveValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputDistributionCurveValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.
    ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdDistributionCurve,
    FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputDistributionCurveValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.OnShowComplianceClick (Sender : TObject);
const OPNAME = 'TOutputDistributionCurveValidator.OnShowComplianceClick';
var
  lChannel  : IGeneralFlowChannel;
begin
  try
    if (FShowCompliance) then
      FShowCompliance := FALSE
    else
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        if (FTimeStep <> otsAnnual) then
        begin
          ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg1'));
        end
        else
        begin
          if (lChannel.MasterControlFeature <> nil) then
            FShowCompliance := TRUE
          else
          if (lChannel.MinMaxFlowConstraint <> nil) then
          begin
            if (lChannel.WaterDemandFeature = nil) then
            begin
              {lMessage := 'The channel does not have a Water Demand Feature. ' + #10#13 +
                          'Create a Water Demand Feature for the channel and then ' + #10#13 +
                          'specify Output Water Use Proportions for the channel.';}
              ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg2'));
            end
            else
              FShowCompliance := TRUE;
          end
          else
          if (lChannel.SpecifiedDemandFeature <> nil) then
          begin
            if (lChannel.WaterDemandFeature = nil) then
            begin
              {lMessage := 'The channel does not have a Water Demand Feature. ' + #10#13 +
                          'Create a Water Demand Feature for the channel and then ' + #10#13 +
                          'specify Output Water Use Proportions for the channel.';}
              ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg3'));
            end
            else
            begin
              if (FUnits <> ouPercentage) then
              begin
                {lMessage := 'The compliance graph is only implemented for Units = Percentage ' + #10#13 +
                            'for Specified Demand Channels.' + #10#13 +
                            'Select output data and set Units to Percentage.';}
                ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg4'));
              end
              else
                FShowCompliance := TRUE
            end;
          end;
        end;
      end;
    end;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.CalculateCompliance;
const OPNAME = 'TOutputDistributionCurveValidator.CalculateCompliance';
var
  lYieldModelData : IYieldModelData;
  lConfig         : IWaterDemandConfiguration;
  lChannel        : IGeneralFlowChannel;
  lRICount        : integer;
  lCategoryCount  : integer;
  lRequired       : double;
  lProportion     : IWaterUseOutputProportion;
  lRIIndex        : integer;
  lCategoryIndex  : integer;
  lCategory       : IWaterDemandCategory;
  lTotal          : double;
  lMonthDays      : array [1..12] of double;
  lMonthDemands   : array [1..12] of double;
  lMinMaxFeature  : IMinMaxFlowConstraint;
  lIndex          : integer;
begin
  try
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    lConfig         := lYieldModelData.NetworkFeaturesData.WaterDemandConfiguration;
    lChannel        := lYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lConfig <> nil) then
    begin
      lRequired := 0;
      if (lChannel.MasterControlFeature <> nil) then
      begin
        if (FUnits = ouPercentage) then
          lRequired := 100.0
        else
        begin
          lRequired := lYieldModelData.RunConfigurationData.TargetYieldByIndex[FLoadCase];
          if (FUnits = ouPerSecond) then
            lRequired := (lRequired * 1000000.0)/(365.25 * 86400.0);
          if (FUnits = ouMegaLitersPerDay) then
            lRequired := (lRequired * 1000.0)/(365.25 * (86400.0));
        end;
      end
      else
      if (lChannel.MinMaxFlowConstraint <> nil) then
      begin
        if (FUnits = ouPercentage) then
          lRequired := 100.0
        else
        begin
          lMinMaxFeature := lChannel.MinMaxFlowConstraint;
          for lIndex := 1 to 12 do
          begin
            lMonthDays[lIndex]    := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
            lMonthDemands[lIndex] := lMinMaxFeature.FlowConstraintByArcMonth[1, lIndex];
            lRequired             := lRequired + (lMonthDemands[lIndex] * 86400.0 * lMonthDays[lIndex]);
          end;
          if (FUnits = ouPerSecond) then
            lRequired := lRequired / (365.25 * 86400.0);
          if (FUnits = ouMegaLitersPerDay) then
            lRequired := lRequired /1000.0
          else
            lRequired := lRequired / 1000000.0;
        end;
      end
      else
      if (lChannel.SpecifiedDemandFeature <> nil) then
        lRequired := 100.0;

      lRICount       := lConfig.RiskCriteriaCount;
      lCategoryCount := lConfig.DemandCategoryCount;
      lProportion    := lConfig.WaterUseOutputProportionByChannelNumber[FIdentifier];
      SetLength(FComplianceArray, lRICount + 1);
      for lRIIndex := lRICount downto 1 do
      begin
        FComplianceArray[lRIIndex] := 0;
        lTotal := 0.0;
        for lCategoryIndex := 1 to lCategoryCount do
        begin
          lCategory := lConfig.DemandCategoryByID[lCategoryIndex];
          lTotal    := lTotal +
                       (lCategory.DemandPortionByIndex[lRIIndex] *
                        lProportion.ProportionByIndex[lCategoryIndex]);
        end;
        FComplianceArray[lRIIndex] := lTotal * lRequired;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.ClearCompliance;
const OPNAME = 'TOutputDistributionCurveValidator.ClearCompliance';
var
  lShape : TChartShape;
begin
  try
    while (FComplianceShapeList.Count > 0) do
    begin
      lShape := TChartShape(FComplianceShapeList.Items[0]);
      lShape.Active      := False;
      lShape.ParentChart := nil;
      lShape.Clear;
      ViewDialog.Chart.RemoveSeries(lShape);
      FComplianceShapeList.Delete(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateCompliance;
const OPNAME = 'TOutputDistributionCurveValidator.PopulateCompliance';
var
  lYieldModelData : IYieldModelData;
  lConfig         : IWaterDemandConfiguration;
  lIndex          : integer;
  lShape          : TChartShape;
  lBottom         : double;
  lTop            : double;
  lYearsCount     : integer;
  lXCentre        : double;
  lRInterval      : integer;
begin
  try
    ClearCompliance;
    if (FShowCompliance) then
    begin
      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      lConfig         := lYieldModelData.NetworkFeaturesData.WaterDemandConfiguration;
      lYearsCount     := lYieldModelData.RunConfigurationData.YearsInAnalysis;
      CalculateCompliance;
      lBottom := 0.0;
      for lIndex := High(FComplianceArray) downto Low(FComplianceArray) do
      begin
        if (FComplianceArray[lIndex] > 0) then
        begin
          lTop       := lBottom + FComplianceArray[lIndex];
          lRInterval := Trunc(lConfig.RecurrenceIntervalByIndex[lIndex]);
          if (InArray(lRInterval, FSelectedRIArray)) then
          begin
            lXCentre := CalculateXPointHistoric(lYearsCount, Trunc(lConfig.RecurrenceIntervalByIndex[lIndex]));
            lShape := TChartShape.Create(ViewDialog.Chart);
            FComplianceShapeList.Add(lShape);
            ViewDialog.Chart.AddSeries(lShape);
            lShape.Marks.Visible := False;
            lShape.ShowInLegend  := False;
            lShape.Color         := clYellow;
            lShape.Style         := chasRectangle;
            lShape.XYStyle       := xysAxis;
            lShape.X0            := lXCentre - 0.25;
            lShape.X1            := lXCentre + 0.25;
            lShape.Y0            := lBottom;
            lShape.Y1            := lTop;
          end;
          lBottom              := lTop;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.GetSelectionData;
const OPNAME = 'TOutputDistributionCurveValidator.GetSelectionData';
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

procedure TOutputDistributionCurveValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputDistributionCurveValidator.PopulateDialogSelectors';
var
  //LViewData    : string;
  LIndex       : integer;
  LSelectIndex : integer;
  LChannel : IGeneralFlowChannel;
begin
  try
    if (FIdentifier >= 0)  and (NetworkElementType <> votNone)then
    begin
      case NetworkElementType of
        votMasterControl:
        begin
          FCurrentViewData := btMonthlyAverageChannelFlow;
        end;
        votReservoir:
        begin
        end;
        votNodeWithInflow:
        begin
        end;
        votNodeWithoutInflow:
        begin
        end;
        votChannel:
        begin
          FCurrentViewData := btMonthlyAverageChannelFlow;
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                 .ChannelList.ChannelByChannelNumber[FIdentifier];
          if (LChannel <> nil) then
          begin
            if(LChannel.ChannelType = 8) and (LChannel.IFRFeature <> nil) then
            begin
             {
              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFR');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btDefinedIFR)));

              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlow');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRFlow)));

              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRRequirement)));

              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedIFR');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
              }

              ViewDialog.BtnRISelector.Visible := True;//False;
              ViewDialog.BtnCompliance.Visible := True;//False;
              ViewDialog.ViewDataType.Visible  := False;//True;
              ViewDialog.ViewDataLabel.Visible := False;//True;
              ViewDialog.CurvesDescrLabel.Visible := False;//True;
              ViewDialog.IFRCurves.IFRCurveChart.Visible := False;//True;

         end;
          end;
        end;
        votIrrigationArea:
        begin
        end;
        votPowerPlant:
        begin
        end;
      end;
      if(ViewDialog.cmbViewDataType.Items.Count > 0) then
      begin
        //ViewDialog.cmbViewDataType.ItemIndex := 0;
        LSelectIndex := -1;
        if (FPrevViewData <> btNone) then
        begin
          for LIndex := 0 to ViewDialog.cmbViewDataType.Items.Count - 1 do
          begin
            if (integer(FPrevViewData) = integer(ViewDialog.cmbViewDataType.Items.Objects[LIndex])) then
            begin
              LSelectIndex := LIndex;
              Break;
            end;
          end;
        end;
        if (LSelectIndex >= 0 ) then
          ViewDialog.cmbViewDataType.ItemIndex := LSelectIndex
        else
        if (ViewDialog.cmbViewDataType.Items.Count > 0) then
          ViewDialog.cmbViewDataType.ItemIndex := 0;
        SetCurrentViewData;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.SetCurrentViewData;
const OPNAME = 'TOutputDistributionCurveValidator.SetCurrentViewData';
var
  LIndex: integer;
begin
  try
    if (ViewDialog.cmbViewDataType.ItemIndex >= 0) then
    begin
      LIndex := Integer(ViewDialog.cmbViewDataType.Items.Objects[ViewDialog.cmbViewDataType.ItemIndex]);
      FCurrentViewData := TOutputDataType(LIndex);
    end
    else
      FCurrentViewData := btNone;
    FPrevViewData := FCurrentViewData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateChannelSupplyChartData(AData: TStrings);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateChannelSupplyChartData';
var
  LPercentile: double;
  LMonth,
  LIndex : integer;
  LYValue : double;
  LSupplyValue : double;
  LMonthData,
  LSupplyData : TStringList;
  LIFRFeature : TIFRFeature;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRCurves.Visible := True;
    ViewDialog.IFRCurves.PrepareChart;
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
    if FUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRSupply'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUnits = ouPerSecond then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRSupply'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Title.Caption :=
                   FAppModules.Language.GetString('TIFRSiteDialog.ExceedenceProbability');

    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Increment  := 10;
    ViewDialog.IFRCurves.SuppliedIFRLineSeries.Visible         := True;
    ViewDialog.IFRCurves.SuppliedIFRPointSeries.Visible        := True;
    ViewDialog.IFRCurves.SuppliedIFRLineSeries.Color           := clRed;
    ViewDialog.IFRCurves.SuppliedIFRPointSeries.Color          := clRed;
    ViewDialog.IFRCurves.SuppliedIFRPointSeries.Pointer.Style  := psCircle;
    ViewDialog.IFRCurves.IFRCurveChart.Legend.Visible := False;
    if LIFRFeature <> nil then
    begin
      LSupplyData := TStringList.Create;
      LMonthData :=  TStringList.Create;
      try
        ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
        if (FDisplayMonth = 0) then
        begin
        for LMonth := 1 to 12 do
        begin
          LSupplyData.Clear;
          LSupplyData.CommaText := AData.CommaText;
          GetMonthData(LSupplyData,LMonth);
          for LIndex := 0 to LSupplyData.Count-1 do
          begin
            if (Trim(LSupplyData[LIndex]) <> '') then
            begin
              LSupplyValue := StrToFloat(LSupplyData[LIndex]);
              LSupplyValue := ConvertToMcM(LSupplyValue,LMonth);
              LMonthData.Add(FloatToStr(LSupplyValue));
            end;
          end;
        end;
        GetSortedData(LMonthData,0);
        for LIndex := 0 to LMonthData.Count-1 do
        begin
          LPercentile := (LIndex/LMonthData.Count)*100;
          LSupplyValue := StrToFloat(LMonthData[LIndex]);
          ViewDialog.IFRCurves.SuppliedIFRLineSeries.AddXY(LPercentile,LSupplyValue,'', clRed);
        end;
        end
        else
        if (FDisplayMonth > 0) then
        begin
          LSupplyData.Clear;
          LSupplyData.CommaText := AData.CommaText;
          GetSortedData(LSupplyData,FDisplayMonth);
          for LIndex := 0 to LSupplyData.Count-1 do
          begin
            LPercentile := (LIndex/LSupplyData.Count) * 100;
            LYValue := StrToFloat(LSupplyData[LIndex]);
            LYValue := ConvertToMcM(LYValue,FDisplayMonth);
            ViewDialog.IFRCurves.SuppliedIFRLineSeries.AddXY(LPercentile,LYValue,'', clRed);
            ViewDialog.IFRCurves.SuppliedIFRPointSeries.AddXY(LPercentile,LYValue,'', clRed);
          end;
        end;
      finally
        LSupplyData.Free;
        LMonthData.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputDistributionCurveValidator.PopulateRequirement(AIFRFeature: TIFRFeature);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateRequirement';
var
  LStringValue: string;
  LFloatValue : double;
  LReferenceFlowData,
  LMonthData,
  LLineData   : TStringList;
  LRequirement,
  LPercentile: double;
  LMonth,
  LIndex : integer;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRCurves.Visible := True;
    ViewDialog.IFRCurves.PrepareChart;
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Title.Caption :=
                   FAppModules.Language.GetString('TIFRSiteDialog.ExceedenceProbability');
    if FUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                       Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRRequirement'),
                       [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUnits = ouPerSecond then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                       Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRRequirement'),
                       [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Increment   := 10;
    ViewDialog.IFRCurves.RequiredIFRPointSeries.Pointer.Style := psCircle;
    ViewDialog.IFRCurves.RequiredIFRLineSeries.Visible        := True;
    ViewDialog.IFRCurves.RequiredIFRPointSeries.Visible       := True;
    ViewDialog.IFRCurves.RequiredIFRLineSeries.Color          := clRed;
    ViewDialog.IFRCurves.RequiredIFRLineSeries.Color          := clRed;
    ViewDialog.IFRCurves.IFRCurveChart.Legend.Visible         := False;
    if(AIFRFeature = nil) then
      Exit;
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    LReferenceFlowData := TStringList.Create;
    try
      ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
      if (FDisplayMonth = 0) then
      begin
        for LMonth := 1 to 12 do
        begin
          LReferenceFlowData.Clear;
          if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
            Exit;
          GetMonthData(LReferenceFlowData,LMonth);
          for LIndex := 0 to LReferenceFlowData.Count-1 do
          begin
            LRequirement := StrToFloat(FormatFloat('00000000000000.000',
                    AIFRFeature.GetRequirementFlowFromReferenceFlow(LMonth,StrToFloat(LReferenceFlowData[LIndex]))));
            LRequirement := ConvertToMcM(LRequirement,LMonth);
            LMonthData.Add(FloatToStr(LRequirement));
          end;
        end;
        GetSortedData(LMonthData,0);
        for LIndex := 0 to LMonthData.Count-1 do
        begin
          LPercentile  := (LIndex/LMonthData.Count)*100;
          LRequirement := StrToFloat(LMonthData[LIndex]);
          if(LRequirement <> NullFloat) then
          begin
            ViewDialog.IFRCurves.RequiredIFRLineSeries.AddXY(LPercentile,LRequirement,'',clRed);
          end;
        end;
      end
      else
      if (FDisplayMonth > 0) then
      begin
        if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
          Exit;

        LMonthData.Sorted := True;
        LMonthData.Duplicates := dupAccept;
        for LIndex := 0 to LReferenceFlowData.Count-1 do
        begin
          LLineData.CommaText := LReferenceFlowData.Strings[LIndex];
          LStringValue    := LLineData[FDisplayMonth];
          LFloatValue     := StrToFloat(LStringValue);
          LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
        end;
        LLineData.Clear;
        for LIndex := LMonthData.Count-1 downto 0 do
          LLineData.Add(LMonthData[LIndex]);
        for LIndex := 0 to LLineData.Count-1 do
        begin
          LPercentile  := (LIndex/LLineData.Count)*100;
          LRequirement := StrToFloat(FormatFloat('00000000000000.000',
                     AIFRFeature.GetRequirementFlowFromReferenceFlow(FDisplayMonth,StrToFloat(LLineData[LIndex]))));
          if(LRequirement <> NullFloat) then
          begin
            LRequirement := ConvertToMcM(LRequirement,FDisplayMonth);
            ViewDialog.IFRCurves.RequiredIFRLineSeries.AddXY(LPercentile,LRequirement,'',clRed);
            ViewDialog.IFRCurves.RequiredIFRPointSeries.AddXY(LPercentile,LRequirement,'',clRed);
          end;
        end;
      end;
    finally
        LMonthData.Free;
        LLineData.Free;
        LReferenceFlowData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputDistributionCurveValidator.PopulateDefinedIFR(AIFRFeature: TIFRFeature);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateDefinedIFR';
var
  LIndex : integer;
  LCount : integer;
  LXValues,
  LYValues : double;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRCurves.Visible := True;
    ViewDialog.IFRCurves.PrepareChart;
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
    if FUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRFeatureDialog.DefinedIFR'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUnits = ouPerSecond then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRFeatureDialog.DefinedIFR'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Increment  := 10;
    ViewDialog.IFRCurves.DefinedIFRLineSeries.Visible        := True;
    ViewDialog.IFRCurves.DefinedIFRPointSeries.Visible       := True;
    ViewDialog.IFRCurves.DefinedIFRLineSeries.Color          := clRed;
    ViewDialog.IFRCurves.DefinedIFRPointSeries.Color         := clRed;
    ViewDialog.IFRCurves.DefinedIFRPointSeries.Pointer.Style := psCircle;
    ViewDialog.IFRCurves.IFRCurveChart.Legend.Visible        := False;
    if AIFRFeature <> nil then
    begin
      ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
      if FDisplayMonth = 0 then
      begin
        ViewDialog.IFRCurves.IFRCurveChart.Legend.Visible    := True;
        ViewDialog.IFRCurves.IFRCurveChart.Legend.Alignment   := laBottom;
        GetChartLegend(ViewDialog.IFRCurves.IFRCurveChart);
       for LIndex := 1 to 12 do
        begin
          ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex-1].Visible         := True;
          ViewDialog.IFRCurves.DefinedPointSeriesArray[LIndex-1].Visible        := True;
          ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex-1].SeriesColor     := C_Colors[LIndex-1];
          ViewDialog.IFRCurves.DefinedPointSeriesArray[LIndex-1].SeriesColor    := C_Colors[LIndex-1];
          ViewDialog.IFRCurves.DefinedPointSeriesArray[LIndex-1].Pointer.Style  := C_PointStyle[LIndex-1];
          for LCount := 12 downto 1 do
          begin
            LXValues :=  StrToFloat(FormatFloat('00000000000000.000',AIFRFeature.ExceedencePercentageByIndex[LCount]));
            LYValues :=  StrToFloat(FormatFloat('00000000000000.000',AIFRFeature.ReleaseByIndexAndMonth[LCount,LIndex]));
            if (LYValues <> NullFloat) AND
               (LXValues <> NullFloat) then
            begin
              LXValues := ConvertToMcM(LXValues,LIndex);
              LYValues := ConvertToMcM(LYValues,LIndex);
              ViewDialog.IFRCurves.DefinedLineSeriesArray[LIndex-1].AddXY(LXValues,LYValues);
              ViewDialog.IFRCurves.DefinedPointSeriesArray[LIndex-1].AddXY(LXValues,LYValues);
            end;
          end;
        end;
      end
      else
      if (FDisplayMonth > 0) then
      begin
        for LCount := 12 downto 1 do
        begin
        LYValues := StrToFloat(FormatFloat('00000000000000.000',AIFRFeature.ReleaseByIndexAndMonth[LCount,FDisplayMonth]));
        LXValues := StrToFloat(FormatFloat('00000000000000.000',AIFRFeature.ExceedencePercentageByIndex[LCount]));
        if(LYValues <> NullFloat) AND
          (LXValues <> NullFloat) then
          begin
            LXValues := ConvertToMcM(LXValues,FDisplayMonth);
            LYValues := ConvertToMcM(LYValues,FDisplayMonth);
            ViewDialog.IFRCurves.DefinedIFRLineSeries.AddXY(LXValues,LYValues);
            ViewDialog.IFRCurves.DefinedIFRPointSeries.AddXY(LXValues,LYValues);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputDistributionCurveValidator.PopulateReferenceFlow(AIFRFeature: TIFRFeature);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateReferenceFlow';
var
  LStringValue: string;
  LFloatValue : double;
  LReferenceFlowData,
  LMonthData,
  LLineData   : TStringList;
  LYValue,
  LPercentile: double;
  LMonth,
  LIndex : integer;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRCurves.Visible := True;
    ViewDialog.IFRCurves.PrepareChart;
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Title.Caption :=
                   FAppModules.Language.GetString('TIFRSiteDialog.ExceedenceProbability');
    if FUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedReferenceFlow'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUnits = ouPerSecond then
      ViewDialog.IFRCurves.IFRCurveChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedReferenceFlow'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRCurves.IFRCurveChart.BottomAxis.Increment := 10;
    ViewDialog.IFRCurves.ReferenceFlowLineSeries.Visible        := True;
    ViewDialog.IFRCurves.ReferenceFlowPointSeries.Visible       := True;
    ViewDialog.IFRCurves.ReferenceFlowLineSeries.Color          := clRed;
    ViewDialog.IFRCurves.ReferenceFlowPointSeries.Color         := clRed;
    ViewDialog.IFRCurves.ReferenceFlowPointSeries.Pointer.Style := psCircle;
    ViewDialog.IFRCurves.IFRCurveChart.Legend.Visible := False;
    if(AIFRFeature = nil) then
      Exit;
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    LReferenceFlowData := TStringList.Create;
    try
      ViewDialog.IFRCurves.IFRCurveChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
      if (FDisplayMonth = 0) then
      begin
        for LMonth := 1 to 12 do
        begin
          LReferenceFlowData.Clear;
          if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
            Exit;
          GetMonthData(LReferenceFlowData,LMonth);
          for LIndex := 0 to LReferenceFlowData.Count-1 do
          begin
            LYValue := StrToFloat(FormatFloat('00000000000000.000',StrToFloat(LReferenceFlowData[LIndex])));
            LYValue := ConvertToMcM(LYValue,LMonth);
            LMonthData.Add(FloatToStr(LYValue));
          end;
        end;
        GetSortedData(LMonthData,0);
        for LIndex := 0 to LMonthData.Count-1 do
        begin
          LPercentile := (LIndex/LMonthData.Count)*100;
          LYValue := StrToFloat(LMonthData[LIndex]);
          ViewDialog.IFRCurves.ReferenceFlowLineSeries.AddXY(LPercentile,LYValue,'',clRed);
        end;
      end
      else
      if FDisplayMonth > 0 then
      begin
        if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
          Exit;
        LMonthData.Sorted := True;
        LMonthData.Duplicates := dupAccept;
        for LIndex := 0 to LReferenceFlowData.Count-1 do
        begin
          LLineData.CommaText := LReferenceFlowData.Strings[LIndex];
          LStringValue    := LLineData[FDisplayMonth];
          LFloatValue     := StrToFloat(LStringValue);
          LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
        end;

        LLineData.Clear;
        for LIndex := LMonthData.Count-1 downto 0 do
          LLineData.Add(LMonthData[LIndex]);

        for LIndex := 0 to LLineData.Count-1 do
        begin
          LPercentile := (LIndex/LLineData.Count)*100;
          LYValue := StrToFloat(LLineData[LIndex]);
          LYValue := ConvertToMcM(LYValue,FDisplayMonth);
          ViewDialog.IFRCurves.ReferenceFlowLineSeries.AddXY(LPercentile,LYValue,'',clRed);
          ViewDialog.IFRCurves.ReferenceFlowPointSeries.AddXY(LPercentile,LYValue,'',clRed);
        end;
      end;
    finally
      LMonthData.Free;
      LLineData.Free;
      LReferenceFlowData.Free;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputDistributionCurveValidator.ConvertToMcM(AValue : double; AIndex : integer) : double;
const OPNAME = 'TOutputDistributionCurveValidator.ConvertToMcM';
var
  LDaysInMonth : double;
begin
  Result :=  AValue;
  try
    LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[AIndex];
    case FUnits of
      ouMcmPerMonthOrYear :
      Result := StrToFloat(FormatFloat('###0.00',((AValue*LDaysInMonth*24*60*60)/Power(10,6))));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputDistributionCurveValidator.GetSortedData(AData: TStrings;AMonth : integer);
const OPNAME = 'TOutputDistributionCurveValidator.GetSortedData';
var
  LMonthData,
  LLineData   : TStringList;
  LIndex : integer;
  LStringValue : string;
  LFloatValue : double;
  LYear : integer;
begin
  try
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    try
      LMonthData.Sorted     := True;
      LMonthData.Duplicates := dupAccept;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData.Strings[LIndex];
        LStringValue        := LLineData[AMonth];
        if AMonth > 0 then
        begin
          LYear           := StrToInt(LLineData[0]);
          LFloatValue     := StrToFloat(LStringValue);
          LMonthData.AddObject(FormatFloat('00000000000000.000',LFloatValue),TObject(LYear));
        end
        else
        if AMonth = 0 then
        begin
          LFloatValue := StrToFloat(LStringValue);
          LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
        end;
      end;
      AData.Clear;
      if AMonth > 0 then
      begin
        for LIndex := LMonthData.Count-1 downto 0 do
        begin
          LYear := integer(LMonthData.Objects[LIndex]);
          AData.AddObject(LMonthData[LIndex], TObject(LYear));
        end;
      end
      else
      if AMonth = 0 then
      begin
        for LIndex := LMonthData.Count-1 downto 0 do
          AData.Add(LMonthData[LIndex]);
      end;

     finally
       FreeAndNil(LMonthData);
       FreeAndNil(LLineData);
     end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputDistributionCurveValidator.GetMonthData(AData: TStrings;AMonth : integer);
const OPNAME = 'TOutputDistributionCurveValidator.GetMonthData';
var
  LMonthData,
  LLineData   : TStringList;
  LIndex      : integer;
  LYear       : integer;
begin
  try
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    try
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData.Strings[LIndex];
        if LLineData.Count > 0 then
        begin
          LYear           := StrToInt(LLineData[0]);
          LMonthData.AddObject(FormatFloat('00000000000000.000',StrToFloat(LLineData[AMonth])),TObject(LYear));
        end;
      end;
      AData.Clear;
      for LIndex := 0 to LMonthData.Count-1 do
      begin
        LYear := integer(LMonthData.Objects[LIndex]);
        AData.AddObject(LMonthData[LIndex], TObject(LYear));
      end;
     finally
       FreeAndNil(LMonthData);
       FreeAndNil(LLineData);
     end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputDistributionCurveValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputDistributionCurveValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveValidator.GetMonthDescription(AMonthIndex: integer): string;
const OPNAME = 'TOutputDistributionCurveValidator.GetMonthDescription';
const C_MonthsDescr : array [0..12] of string = ('FOR ALL MONTHS','OCTOBER','NOVEMBER','DECEMBER','JANUARY','FEBRUARY'
                                                 ,'MARCH','APRIL','MAY','JUNE','JULY','AUGUST','SEPTEMBER');
begin
  try
    Result := C_MonthsDescr[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.GetChartLegend(AChart: TFieldChart);
const OPNAME = 'TOutputDistributionCurveValidator.GetChartLegend';
var
  LIndex : integer;
  LSystemConfig : IRunConfigurationData;
  LCount : integer;
begin
  try
    LSystemConfig := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    LCount := 0;
    for LIndex := 0 to AChart.SeriesCount-1 do
    begin
       if (AChart.Series[LIndex] is TLineSeries) then
       begin
         if LCount <= 11 then
         begin
           AChart.Series[LIndex].ShowInLegend := True;
           AChart.Series[LIndex].Color        := C_Colors[LCount];
           AChart.Series[LIndex].Title        := LSystemConfig.MonthNameByIndex[LCount+1];
           LCount := LCount+1;
         end
         else
           AChart.Series[LIndex].ShowInLegend := False;
       end
       else
         AChart.Series[LIndex].ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateAnnualChartForDiversion(ADataStringList: TStringList;
                                                                            AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualChartForDiversion';
var
  LIndex                : integer;
  LSortedValues         : TStringList;
  LRequired             : double;
  LProduced             : double;
  LYValue               : double;
  LMonthDays            : array [1..12] of double;
  LMonthDiversionDemand : array [1..12] of double;
  LYieldModelData       : IYieldModelData;
  LDiversionFeature     : IDiversionFeature;
  LCount                : integer;
begin
  try
    LSortedValues := TStringList.Create;
    try
      LSortedValues.Sorted     := True;
      LSortedValues.Duplicates := dupAccept;
      LDiversionFeature        := AChannel.DiversionFeature;
      if(ADataStringList <> nil) then
      begin
        LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        LRequired := 0.0;
        for LIndex := 1 to 12 do
        begin
          LMonthDays[LIndex]            := LYieldModelData.RunConfigurationData.MonthDaysByIndex[LIndex];
          LMonthDiversionDemand[LIndex] := LDiversionFeature.DiversionDemandByIndex[LIndex];
          LRequired                     := LRequired + (LMonthDiversionDemand[LIndex] * 86400.0 * LMonthDays[LIndex]);
        end;
        LRequired := LRequired / (365.25 * 86400.0);
        ViewDialog.LineSeries.Active := True;
        for LCount := 0 to ADataStringList.Count - 1 do
        begin
          if pos('AVE:',ADataStringList.Strings[LCount]) > 0 then
          begin
            LProduced := StrToFloat(copy(ADataStringList.Strings[LCount],5,length(ADataStringList.Strings[LCount])));
            if(FUnits = ouPercentage) then
            begin
              if(LRequired > 0) then
                LYValue := LProduced / LRequired * 100.0
              else
                LYValue := 0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            if(FUnits = ouPerSecond) then
            begin
              LYValue := LProduced;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            if(FUnits = ouMegaLitersPerDay) then
            begin
              LYValue := LProduced * (365.25 * (86400.0)) / 1000.0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            begin
              LYValue := LProduced * (365.25 * 86400.0) / 1000000.0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end;
          end;
        end;
        PopulateAnnualChart(LSortedValues, AChannel);
       end;
    finally
      LSortedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChartForDiversion(ADataStringList: TStringList;
                                                                             AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChartForDiversion';
var
  LMonthIndex       : integer;
  LIndex            : integer;
  LSortedValues     : TStringList;
  LRequired         : double;
  LProduced         : double;
  LYValue           : double;
  LMonthDays        : array [1..12] of double;
  LYieldModelData   : IYieldModelData;
  LDiversionFeature : IDiversionFeature;
  LCount            : integer;
  LPos              : integer;
begin
  try
    LSortedValues := TStringList.Create;
    try
      LSortedValues.Sorted := True;
      LSortedValues.Duplicates := dupAccept;
      LDiversionFeature := AChannel.DiversionFeature;
      if(ADataStringList <> nil) then
      begin
        LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        for LIndex := 1 to 12 do
          LMonthDays[lIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
        ViewDialog.LineSeries.Active := True;

        LPos := -1;
        for LCount := 0 to (ADataStringList.Count div 13) -1 do
        begin
          for LIndex := 0 to 12 do
          begin
            LPos := LPos + 1;
            if (LIndex <> 12) then
            begin
              if(FDisplayMonth = 0) OR (FDisplayMonth = LIndex + 1) then
              begin
                LProduced  := StrToFloat(ADataStringList.Strings[LPos]);
                if(FUnits = ouPercentage) then
                begin
                  LMonthIndex := LIndex + 1;
                  LRequired   := LDiversionFeature.DiversionDemandByIndex[LMonthIndex];
                  if(LRequired > 0) then
                    LYValue := LProduced / LRequired * 100.0
                  else
                    LYValue := 0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                if(FUnits = ouPerSecond) then
                begin
                  LYValue := LProduced;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                if (FUnits = ouMegaLitersPerDay) then
                begin
                  LYValue := LProduced *(86400.0) / 1000.0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                begin
                  LYValue := LProduced * (LMonthDays[LIndex] * 86400.0) / 1000000.0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end;
              end;
            end;
          end;
        end;
        PopulateMonthlyChart(LSortedValues,nil, AChannel);
      end;
    finally
      LSortedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateAnnualChartForLoss(ADataStringList: TStringList;
                                                                       AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualChartForLoss';
var
  LIndex          : integer;
  LSortedValues   : TStringList;
  LRequired       : double;
  LProduced       : double;
  LYValue         : double;
  LMonthDays      : array [1..12] of double;
  LMonthLoss      : array [1..12] of double;
  LYieldModelData : IYieldModelData;
  LLossFeature    : ILossFeature;
  LCount          : integer;
begin
  try
    LSortedValues := TStringList.Create;
    try
      LSortedValues.Sorted := True;
      LSortedValues.Duplicates := dupAccept;
      LLossFeature := AChannel.LossFeature;
      if(ADataStringList <> nil) then
      begin
        LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        LRequired := 0.0;
        for LIndex := 1 to 12 do
        begin
          LMonthDays[LIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[LIndex];
          LMonthLoss[LIndex] := LLossFeature.WaterLossByMonth[LIndex];
          LRequired          := LRequired + (LMonthLoss[LIndex] * 86400.0 * LMonthDays[LIndex]);
        end;
        LRequired := LRequired / (365.25 * 86400.0);
        ViewDialog.LineSeries.Active := True;
        for LCount := 0 to ADataStringList.Count - 1 do
        begin
          if pos('AVE:',ADataStringList.Strings[LCount]) > 0 then
          begin
            LProduced := StrToFloat(copy(ADataStringList.Strings[LCount],5,Length(ADataStringList.Strings[LCount])));
            if(FUnits = ouPercentage) then
            begin
              if(LRequired > 0) then
                LYValue := LProduced / LRequired * 100.0
              else
                LYValue := 0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            if(FUnits = ouPerSecond) then
            begin
              LYValue := LProduced;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            if(FUnits = ouMegaLitersPerDay) then
            begin
              LYValue := LProduced * (365.25 * (86400.0)) / 1000.0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            begin
              LYValue := LProduced * (365.25 * 86400.0) / 1000000.0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end;
          end;
        end;
        PopulateAnnualChart(LSortedValues, AChannel);
       end;
    finally
      LSortedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChartForLoss(ADataStringList: TStringList;
                                                                        AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChartForLoss';
var
  LMonthIndex     : integer;
  LIndex          : integer;
  LSortedValues   : TStringList;
  LRequired       : double;
  LProduced       : double;
  LYValue         : double;
  LMonthDays      : array [1..12] of double;
  LYieldModelData : IYieldModelData;
  LLossFeature    : ILossFeature;
  LCount          : integer;
  LPos            : integer;
begin
  try
    LSortedValues := TStringList.Create;
    try
      LSortedValues.Sorted := True;
      LSortedValues.Duplicates := dupAccept;
      LLossFeature := AChannel.LossFeature;
      if(ADataStringList <> nil) then
      begin
        LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        for LIndex := 1 to 12 do
          LMonthDays[LIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[LIndex];
        ViewDialog.LineSeries.Active := True;

        LPos := -1;
        for LCount := 0 to (ADataStringList.Count div 13) -1 do
        begin
          for LIndex := 0 to 12 do
          begin
            LPos := LPos + 1;
            if (LIndex <> 12) then
            begin
              if(FDisplayMonth = 0) OR (FDisplayMonth = LIndex + 1) then
              begin
                LProduced  := StrToFloat(ADataStringList.Strings[LPos]);
                if(FUnits = ouPercentage) then
                begin
                  LMonthIndex := LIndex + 1;
                  LRequired   := LLossFeature.WaterLossByMonth[LMonthIndex];// FlowConstraintByArcMonth[1, LMonthIndex];
                  if(LRequired > 0) then
                    LYValue := LProduced / LRequired * 100.0
                  else
                    LYValue := 0;
                  lSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                if(FUnits = ouPerSecond) then
                begin
                  LYValue := LProduced;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                if(FUnits = ouMegaLitersPerDay) then
                begin
                  LYValue := lProduced *(86400.0) / 1000.0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                begin
                  LYValue := LProduced * (LMonthDays[LIndex] * 86400.0) / 1000000.0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end;
              end;
            end;
          end;
        end;
        PopulateMonthlyChart(LSortedValues,nil, AChannel);
      end;
    finally
      LSortedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateAnnualChartForMinFlow(ADataStringList: TStringList;
                                                                          AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateAnnualChartForMinFlow';
var
  LIndex              : integer;
  LSortedValues       : TStringList;
  LRequired           : double;
  LProduced           : double;
  LYValue             : double;
  LMonthDays          : array [1..12] of double;
  LMonthMinFlowDemand : array [1..12] of double;
  LYieldModelData     : IYieldModelData;
  LMinimumFlowFeature : IMinimumFlowConstraint;
  LCount              : integer;
begin
  try
    LSortedValues := TStringList.Create;
    try
      LSortedValues.Sorted     := True;
      LSortedValues.Duplicates := dupAccept;
      LMinimumFlowFeature      := AChannel.MinimumFlowConstraint;
      if(ADataStringList <> nil) then
      begin
        LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        LRequired := 0.0;
        for LIndex := 1 to 12 do
        begin
          LMonthDays[LIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[LIndex];
          LMonthMinFlowDemand[LIndex] := LMinimumFlowFeature.MinimumFlowDemandByMonth[LIndex];
          LRequired          := LRequired + (LMonthMinFlowDemand[LIndex] * 86400.0 * LMonthDays[LIndex]);
        end;
        LRequired := LRequired / (365.25 * 86400.0);
        ViewDialog.LineSeries.Active := True;
        for LCount := 0 to ADataStringList.Count - 1 do
        begin
          if pos('AVE:',ADataStringList.Strings[LCount]) > 0 then
          begin
            LProduced := StrToFloat(copy(ADataStringList.Strings[LCount],5,Length(ADataStringList.Strings[LCount])));
            if(FUnits = ouPercentage) then
            begin
              if(LRequired > 0) then
                LYValue := LProduced / LRequired * 100.0
              else
                LYValue := 0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            if(FUnits = ouPerSecond) then
            begin
              LYValue := LProduced;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            if(FUnits = ouMegaLitersPerDay) then
            begin
              LYValue := LProduced * (365.25 * (86400.0)) / 1000.0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end
            else
            begin
              LYValue := LProduced * (365.25 * 86400.0) / 1000000.0;
              LSortedValues.Add(FormatFloat('000000000000.000', LYValue));
            end;
          end;
        end;
        PopulateAnnualChart(LSortedValues, AChannel);
       end;
    finally
      LSortedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateMonthlyChartForMinFlow(ADataStringList: TStringList;
                                                                           AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateMonthlyChartForMinFlow';
var
  LMonthIndex         : integer;
  LIndex              : integer;
  LSortedValues       : TStringList;
  LRequired           : double;
  LProduced           : double;
  LYValue             : double;
  LMonthDays          : array [1..12] of double;
  LYieldModelData     : IYieldModelData;
  LMinimumFlowFeature : IMinimumFlowConstraint;
  LCount              : integer;
  LPos                : integer;
begin
  try
    LSortedValues := TStringList.Create;
    try
      LSortedValues.Sorted     := True;
      LSortedValues.Duplicates := dupAccept;
      LMinimumFlowFeature      := AChannel.MinimumFlowConstraint;
      if(ADataStringList <> nil) then
      begin
        LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        for LIndex := 1 to 12 do
          LMonthDays[LIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[LIndex];
        ViewDialog.LineSeries.Active := True;

        LPos := -1;
        for LCount := 0 to (ADataStringList.Count div 13) -1 do
        begin
          for LIndex := 0 to 12 do
          begin
            LPos := LPos + 1;
            if (LIndex <> 12) then
            begin
              if(FDisplayMonth = 0) OR (FDisplayMonth = LIndex + 1) then
              begin
                LProduced  := StrToFloat(ADataStringList.Strings[LPos]);

                if(FUnits = ouPercentage) then
                begin
                  LMonthIndex := LIndex + 1;
                  LRequired   := LMinimumFlowFeature.MinimumFlowDemandByMonth[LMonthIndex];
                  if(LRequired > 0) then
                    LYValue := LProduced / LRequired * 100.0
                  else
                    LYValue := 0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                  
                end

                else
                if(FUnits = ouPerSecond) then
                begin
                  LYValue := LProduced;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                if(FUnits = ouMegaLitersPerDay) then
                begin
                  LYValue := LProduced *(86400.0) / 1000.0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end
                else
                begin
                  LYValue := LProduced * (LMonthDays[LIndex] * 86400.0) / 1000000.0;
                  LSortedValues.Add(FormatFloat('000000000000.000',LYValue));
                end;

              end;
            end;
          end;
        end;
        PopulateMonthlyChart(LSortedValues,nil, AChannel);
      end;
    finally
      LSortedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.PopulateChannelDemandData(AValues : TStringList; AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputDistributionCurveValidator.PopulateChannelDemandData';
var
  LYearValues,
  LChannelDemandValues   : TStringList;
  LYearIndex             : integer;
  LMonthIndex            : integer;
  LErrors                : string;
  LYear                  : integer;
  LDemandValues          : TStringList;
begin
  try
    AValues.Clear;
    if(FValueType in [ovtDeficits,ovtDemand,ovtDemandAndSupply])  and {((FUnits <> ouPercentage))and}
     (AChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
    begin
      LYearValues   := TStringList.Create;
      LChannelDemandValues   := TStringList.Create;
      LDemandValues    := TStringList.Create;
      try
        if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDemandOutputData.GetChannelDemandValues(
           LChannelDemandValues,AChannel.ChannelNumber,LErrors) then
        begin
          LYear := 0;
          for LYearIndex := 0 to LChannelDemandValues.Count-2 do  //Remove average at the bottom 
          begin
            LYearValues.CommaText := LChannelDemandValues[LYearIndex];
            if (LYearIndex=0)or(LYearIndex <> LChannelDemandValues.Count-1) then
              LYear := StrToInt(LYearValues[0]);
            for LMonthIndex := 1 to 12 do
              LDemandValues.AddObject(LYearValues[LMonthIndex]+'_'+IntToStr(LMonthIndex),TObject(LYear));
            //LDemandValues.Add('AVE:')
          end;
          AValues.Assign(LDemandValues);
        end;
        //What are we doing here? DSR
        //if (FTimeStep = otsSequence) and (AChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel])then
        //  AddDemandsToSequence(AValues)
        //else
        if(FTimeStep = otsSequence) and (AChannel.ChannelType in [ctMasterControlChannel,ctMinimumFlowChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel,ctDemandCentreReturnFlowChannel]) then
          GetDemandsAndSupplyFromAllSequences(AValues,AChannel)
      finally
        LChannelDemandValues.Free;
        LYearValues.Free;
        LDemandValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.OnChartMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
const OPNAME = 'TOutputDistributionCurveValidator.OnChartMouseDown';
begin
  try

    if Assigned(FHintWin)  then
    begin
      FHintWin.Hide;
      FHintWin := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.OnChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TOutputDistributionCurveValidator.OnChartMouseDown';
begin
  try
    {if Assigned(FHintWin) then
    begin
      FHintWin.Hide;
      FHintWin := nil;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.DoOnChartExit(Sender : TObject);
const OPNAME = 'TOutputDistributionCurveValidator.DoOnChartExit';
begin
  try

    if Assigned(FHintWin) then
    begin
      FHintWin.Hide;
      FHintWin := nil;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveValidator.DoOnclick(Sender: TChartSeries; ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TOutputDistributionCurveValidator.DoOnclick';
var
  LHintStr    : string;
  LHeight     : integer;
  LPoint      : TPoint;
  LRect,
  LHntRect    : TRect;
  LWND        : HWND;
  LMonth : word;
  LIndex : integer;
  LSupplyData : TSupplyData;
  LYearsMarkFormatStr : string;
  LYValue : string;
begin
  try
    if Assigned(FHintWin) then
    begin
      FHintWin.Hide;
      FHintWin := nil;
    end;
    if (Sender is TLineSeries) and
       (ValueIndex >= 0)then
    begin
      GetCursorPos(LPoint);

      LWND := WindowFromPoint(LPoint);
      LIndex := -1;
      if ValueIndex<FSupplyListList.Count then
      begin
        LYValue := FSupplyListList[ValueIndex];
        LIndex := FSupplyListList.IndexOf(LYValue);
      end;
      if (LIndex > -1) and (LIndex<FSupplyListList.Count) then
      begin
        LSupplyData := TSupplyData(FSupplyListList.Objects[LIndex]);
        if (LIndex >= 0) and (LSupplyData <> nil) then
        begin
          LMonth := LSupplyData.FMonth;
          if LMonth<=3then
            LMonth := LMonth+9
          else
            LMonth := LMonth-3;
          LYearsMarkFormatStr := FormatDateTime('mmm yyyy',encodedate(LSupplyData.FYear,LMonth,1));
        end;

      end;

      LHintStr := LYearsMarkFormatStr;
      if (LWND > 0) then
      begin
        FHintWin           := THintWindow.Create(ViewDialog.Chart);
        FHintWin.Parent    := VCL.Controls.TWinControl(ViewDialog.Chart);
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

end.

