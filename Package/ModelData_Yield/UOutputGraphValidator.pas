//
//
//  UNIT      : Contains the class TOutputGraphValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputGraphValidator;

interface

uses
  Windows,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.Dialogs,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UDDTSData,
  UDDTSDataObject,
  UYieldContextValidationType,
  UOutputGraphDialog;

type
  TOutputGraphValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData     : TOutputDataType;
    FPrevViewData        : TOutputDataType;
    FLoadCase            : integer;
    FSequence            : integer;
    FMonth               : integer;
    FUnits               : TOutputUnits;
    FValueType           : TOutputValueType;
    FDefValueType        : TOutputValueType;
    FTimeStep            : TOutputTimeStep;
    FHighLight           : WordBool;
    FMonthly             : WordBool;
    FAnnually            : WordBool;
    FDisplayMonth        : integer;
    FPlotActualVolume    : boolean;
    FAtherChannelType    : boolean;
    procedure CreateMemberObjects; override;
    procedure OnViewDataTypeChange(Sender: TObject);
    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure OnPlotActualVolumeClick(Sender: TObject);
    procedure DoShowExceedenceProbabilyty(Sender: TObject);
    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure RePopulateDDTSOutputGraph;
    procedure PopulateTargetRelease(AOutputDataList : TDDTSOutputDataList);
    procedure PopulateChannelChartData(AData: TStrings; AChannel : IGeneralFlowChannel);
    procedure PopulateDemandChartData(AChannel : IGeneralFlowChannel);
    procedure PopulateChannelChart(AData: TStrings);
    procedure PopulateChannelAreaChart(AData: TStrings);
    procedure PopulateReservoirChart(AData: TStrings);
    procedure PopulateReservoirAreaChart(AData: TStrings);
    procedure GetDamLevelsDataSet(AData: TAbstractModelDataset);
    procedure PopulateDamLevelsChartData(AData: TAbstractModelDataset);
    function GetScenarioWhereClause: string;

    procedure FomartMonthlyChart;
    procedure FomartChartToActualVolumeSeries;
    procedure ClearChart;
    procedure SetCurrentViewData;
    procedure GetSelectionData;
    function GetChannelNumberByChannelArea(AChannelAreaID: integer): integer;
    function GetReservoirNumberByGroupID(AGroupID: integer): integer;
    function  ElementName: string;
    procedure ChangeViewDataLabel;
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
    function ViewDialog: TOutputGraphDialog;

  end;

implementation

uses
  Math,
  VCLTee.TeEngine,
  VCLTee.Chart,
  VCLTee.Series,
  SysUtils,
  vcl.Graphics,
  UFileNames,
  UOutputData,
  UWetland,
  UDataSetType,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UNetworkFeaturesData,

  UNetworkElementData, UChannelData, UReservoirData;

{ TOutputGraphValidator }

procedure TOutputGraphValidator.CreateMemberObjects;
const OPNAME = 'TOutputGraphValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputGraphDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.cmbViewDataType.OnSelect:= OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.PlotActualVolume.OnClick := OnPlotActualVolumeClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.Initialise: boolean;
const OPNAME = 'TOutputGraphValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
    ViewDialog.PlotActualVolume.Visible := True;    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputGraphValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or (UpperCase(AFieldName) = 'HYDROSEQCOUNT') or
      (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputGraphValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FPlotActualVolume := (FAppModules.ViewIni.ReadString('TBoxPlotSeriesSelector','PlotActualVolume','') = 'Y');
    if FPlotActualVolume then
      ViewDialog.PlotActualVolume.Checked := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputGraphValidator.LanguageHasChanged';
begin
  Result := False;
  try
    if(NetworkElementType in [votMasterControl,votChannel]) then
      TabShetCaption := 'Time Series Graph'
    else
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputGraph');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.SaveState: boolean;
const OPNAME = 'TOutputGraphValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.ViewDialog : TOutputGraphDialog;
const OPNAME = 'TOutputGraphValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.ClearDataViewer;
const OPNAME = 'TOutputGraphValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearChart;
    ViewDialog.cmbViewDataType.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateDataViewer;
const OPNAME = 'TOutputGraphValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputGraphValidator.PopulateDialogSelectors';
var
  LViewData: string;
  lIndex       : integer;
  lSelectIndex : integer;
begin
  try
    ViewDialog.cmbViewDataType.Items.Clear;
    if(FAppModules.Model.ModelName = CDDTS) then
    begin
      LViewData := 'Storage volume';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(0)));
      LViewData := 'Inflows';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(1)));
      LViewData := 'Rainfall on dam surface';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(2)));
      LViewData := 'Evaporation from dam surface';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(3)));
      LViewData := 'Dam spills';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(4)));
      LViewData := 'EWR Supply';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(5)));
      LViewData := 'EWR Target / Release';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(6)));

      LViewData := 'D/s Target / Supply';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(7)));

      LViewData := 'Abstruction Target / Supply';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(8)));

      ViewDialog.BtnDataSelection.Visible := False;
      ViewDialog.BtnDataSelection.OnClick := DoShowExceedenceProbabilyty;
      ViewDialog.PlotActualVolume.Visible := False;
      ViewDialog.cmbViewDataType.ItemIndex := 0;
      Exit;
    end;



    if (FIdentifier >= 0)  and (NetworkElementType <> votNone)then
    begin
      case NetworkElementType of
        votMasterControl:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
        end;
        votReservoir,votReservoirAreaGroup:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile1');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirVolume)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile2');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirElevation)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile3');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btNetBasinRunoffIntoResArea)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile4');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btRainfallOnReservoirSurface)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile5');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btGrossEvaporationLossFromReservoir)));
        end;
        votNodeWithInflow:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile3');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btNetBasinRunoffIntoResArea)));
        end;
        votNodeWithoutInflow:
        begin
        end;
        votChannel,votChannelArea:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
        end;
        votIrrigationArea:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile9');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageIrrigationDeficits)));
        end;
        votPowerPlant:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile8');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageStackedEnergy)));
        end;
        votWetland:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile1');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirVolume)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile2');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirElevation)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile3');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btNetBasinRunoffIntoResArea)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile4');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btRainfallOnReservoirSurface)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile5');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btGrossEvaporationLossFromReservoir)));
        end;
      end;
      if (ViewDialog.cmbViewDataType.Items.Count > 0) then
      begin
//        ViewDialog.cmbViewDataType.ItemIndex := 0;
        lSelectIndex := -1;
        if (FPrevViewData <> btNone) then
        begin
          for lIndex := 0 to ViewDialog.cmbViewDataType.Items.Count - 1 do
          begin
            if (Integer(FPrevViewData) = Integer(ViewDialog.cmbViewDataType.Items.Objects[lIndex])) then
            begin
              lSelectIndex := lIndex;
              Break;
            end;
          end;
        end;
        if (lSelectIndex >= 0) then
          ViewDialog.cmbViewDataType.ItemIndex := lSelectIndex
        else
        if (ViewDialog.cmbViewDataType.Items.Count > 0) then
          ViewDialog.cmbViewDataType.ItemIndex := 0;
        SetCurrentViewData;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.RePopulateDataViewer;
const OPNAME = 'TOutputGraphValidator.RePopulateDataViewer';
var
  LErrors        : string;
  LDataContainer : TStringList;
  LDataSet       : TAbstractModelDataset;
  //LIrrigationArea : IIrrigationArea;
begin
  try

    if(FAppModules.Model.ModelName = CDDTS) then
    begin
      RePopulateDDTSOutputGraph;
      Exit;
    end;

    ClearChart;
    GetSelectionData;
    if(FIdentifier      >= 0) and
      (NetworkElementType   <> votNone) and
      (FCurrentViewData <> btNone) then
    begin
      LDataContainer := TStringList.Create;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LErrors := '';
        ViewDialog.ShowError(LErrors);
        if (NetworkElementType = votChannelArea) then
        begin
          if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetChannelAreaData(
            LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
              PopulateChannelAreaChart(LDataContainer)
        end
        else
        if (NetworkElementType = votReservoirAreaGroup) then
        begin
          if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetReservoirAreaData(
            LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
              PopulateReservoirAreaChart(LDataContainer)
        end
        else
        if (NetworkElementType = votIrrigationArea) then
        begin
          {LIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationAreaList.
                             IrrigationAreaByNodeNumber[FIdentifier];
          if(LIrrigationArea <> nil) then
          begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,FCurrentViewData,LIrrigationArea.FeatureID,LErrors)) then
                PopulateReservoirChart(LDataContainer);
          end;}
        end
        else
        if (NetworkElementType = votPowerPlant) then
        begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
                PopulateReservoirChart(LDataContainer);
        end
        else
        begin
          if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
            LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
          begin
            ViewDialog.Chart.Visible := True;
            if (FCurrentViewData in [btMonthlyAverageChannelFlow]) then
              PopulateChannelChart(LDataContainer)
            else
            if (NetworkElementType in [votReservoir,votWetland,votNodeWithInflow]) then
            begin
              PopulateReservoirChart(LDataContainer);
              FPlotActualVolume := (FAppModules.ViewIni.ReadString('TBoxPlotSeriesSelector','PlotActualVolume','') = 'Y');
              if FPlotActualVolume then
              begin
                ViewDialog.PlotActualVolume.Checked := True;
                GetDamLevelsDataSet(LDataSet);
                if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
                  PopulateDamLevelsChartData(LDataSet);
              end;
            end;
          end
          else
          begin
            ViewDialog.ShowError(LErrors);
            ViewDialog.Chart.Visible := False;
          end;
        end;
      finally
        LDataContainer.Free;
        LDataSet.Free;
      end;
    end;
    ChangeViewDataLabel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.RePopulateDDTSOutputGraph;
const OPNAME = 'TOutputGraphValidator.RePopulateDDTSOutputGraph';
var
  LSOutputDataList : TDDTSOutputDataList;
  LSOutputData : TDDTSOutputData;
  LIndex : integer;
  LValueFromDamRelease,
  LValueFromDamSpills,
  LValueFromDSRunoff,
  LValueDSTarget,
  LValueDSSupply,
  LValueAbsTarget,
  LValueAbsSupply,
  LValue : double;

begin
  try
    ClearChart;

    LSOutputDataList :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSOutputDataList;
    if LSOutputDataList <> nil then
    begin

      ViewDialog.ClearChart;
      ViewDialog.Elements := 0;
      //ViewDialog.Chart.ClearChart;
      ViewDialog.Chart.Visible := True;
      ViewDialog.LineSeries.Active := True;
      ViewDialog.LineSeries.XValues.DateTime := True;
      ViewDialog.DeadLineSeries.Active := True;
      ViewDialog.DeadLineSeries.XValues.DateTime := True;
      ViewDialog.FullLevelLineSeries.Active := True;
      ViewDialog.FullLevelLineSeries.XValues.DateTime := True;
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text;
      ViewDialog.Chart.BottomAxis.Title.Caption := 'Day'; //FAppModules.Language.GetString('TSCSheet.X_AxisLabel');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;
      ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.00';
      ViewDialog.Chart.LeftAxis.TitleSize        := 1;
      ViewDialog.Chart.LeftAxis.Automatic := True;

      case ViewDialog.cmbViewDataType.ItemIndex of
      5:
      begin
        ViewDialog.Elements := 3;
        ClearChart;
        ViewDialog.PrepareChart;
      end;
      // EWR Target / Release
      6:
      begin
        ViewDialog.Elements := 2;
        ClearChart;
        ViewDialog.PrepareChart;
        PopulateTargetRelease(LSOutputDataList);
      end;

      7,8:
      begin
        ViewDialog.Elements := 2;
        ClearChart;
        ViewDialog.PrepareChart;
      end;
      end;

      for LIndex := 0 to LSOutputDataList.Count-1 do
      begin
        LSOutputData := LSOutputDataList.OutputDataByIndex[LIndex];
        LValue := 0;
        case ViewDialog.cmbViewDataType.ItemIndex of
          // Storage volume
          0: LValue := LSOutputData.ColumnAJ;
          //   Inflows
          1: LValue := LSOutputData.ColumnI;
          //  Rainfall on dam surface
          2: LValue := LSOutputData.ColumnH;
          //  Evaporation from dam surface
          3: LValue := LSOutputData.ColumnM;
          // Dam spills
          4: LValue := LSOutputData.ColumnAL;

          5: // EWR Supply
          begin

           // EWR Supply From Dam Release
             LValueFromDamRelease := LSOutputData.ColumnX;
             ViewDialog.ComparisonSeries[0].AddXY(LSOutputData.ColumnA,LValueFromDamRelease);
             ViewDialog.ComparisonSeries[0].Title := 'From Dam Release';
             ViewDialog.ComparisonSeries[0].SeriesColor := clRed;

          // EWR Supply From Dam Spills
             LValueFromDamSpills := LSOutputData.ColumnV;
             ViewDialog.ComparisonSeries[1].AddXY(LSOutputData.ColumnA,LValueFromDamSpills);
             ViewDialog.ComparisonSeries[1].Title := 'From Dam Spills';
             ViewDialog.ComparisonSeries[1].SeriesColor := clBlue;
          // EWR Supply From D/s Runoff
             LValueFromDSRunoff := LSOutputData.ColumnU;
             ViewDialog.ComparisonSeries[2].AddXY(LSOutputData.ColumnA,LValueFromDSRunoff);
             ViewDialog.ComparisonSeries[2].Title := 'From D/s Runoff';
             ViewDialog.ComparisonSeries[2].SeriesColor := clGreen;
             ViewDialog.Chart.Legend.Visible := True;
             ViewDialog.Chart.Legend.Alignment := laRight;

          end;

          7: // D/s Target / Supply
          begin
            // D/s Target
            LValueDSTarget := LSOutputData.ColumnAC;
            ViewDialog.ComparisonSeries[0].AddXY(LSOutputData.ColumnA,LValueDSTarget);
            ViewDialog.ComparisonSeries[0].SeriesColor := clMaroon;

          // D/s Supply
            LValueDSSupply := LSOutputData.ColumnAD;
            ViewDialog.ComparisonSeries[1].AddXY(LSOutputData.ColumnA,LValueDSSupply);
            ViewDialog.ComparisonSeries[1].SeriesColor := clRed;
            ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text;
          end;

          8: //  Abstruction Target / Supply
          begin
            //  Abstruction Target
            LValueAbsTarget := LSOutputData.ColumnAF;
            ViewDialog.ComparisonSeries[0].AddXY(LSOutputData.ColumnA,LValueAbsTarget);
            ViewDialog.ComparisonSeries[0].SeriesColor := clMaroon;
          //  Abstruction Supply
            LValueAbsSupply := LSOutputData.ColumnAG;
            ViewDialog.ComparisonSeries[1].AddXY(LSOutputData.ColumnA,LValueAbsSupply);
            ViewDialog.ComparisonSeries[1].SeriesColor := clRed;
            ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text;
          end;

        end;
        case ViewDialog.cmbViewDataType.ItemIndex of
          0,1,2,3,4: ViewDialog.LineSeries.AddXY(LSOutputData.ColumnA,LValue);
        end;

      end;

      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text;


    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.GetScenarioWhereClause: string;
const OPNAME = 'TOutputGraphValidator.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.GetDamLevelsDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TOutputGraphValidator.GetDamLevelsDataSet';
var
  LIndex         : integer;
  LSQL           : string;
  LReservoir     : IReservoirData;
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoir <> nil) then
      begin
        LFileNameObject := nil;
        LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
        for LIndex:= 0 to LFileNamesList.FilesCount-1 do
        begin
          if(UpperCase(lReservoir.ReservoirConfigurationData.DamLevelsFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].ShortName)) then
          begin
            LFileNameObject := LFileNamesList.FileNameObject[LIndex];
            Break;
          end;
        end;
        if(LFileNameObject <> nil) then
        begin
          ViewDialog.Chart.Visible := True;
          ViewDialog.PlotActualVolumeSeries.Active := True;
          ViewDialog.PlotActualVolumeSeries.XValues.DateTime := True;
          LSQL := 'SELECT YearValue as [Year],'+
                  'MonthValue01 as Value01,'+
                  'MonthValue02 as Value02,'+
                  'MonthValue03 as Value03,'+
                  'MonthValue04 as Value04,'+
                  'MonthValue05 as Value05,'+
                  'MonthValue06 as Value06,'+
                  'MonthValue07 as Value07,'+
                  'MonthValue08 as Value08,'+
                  'MonthValue09 as Value09,'+
                  'MonthValue10 as Value10,'+
                  'MonthValue11 as Value11,'+
                  'MonthValue12 as Value12 '+
                  ' FROM HistoricDamLevels A WHERE ' +
                   GetScenarioWhereClause +
                  ' AND FileName = '+QuotedStr(LFileNameObject.FileName);
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TOutputGraphValidator.PopulateDamLevelsChartData(AData: TAbstractModelDataset);
const OPNAME = 'TOutputGraphValidator.PopulateDamLevelsChartData';
var
  LYear,
  LIndex        : integer;
  LMonthlyValue : double;
  LDate         : TDateTime;
  LFieldName    : string;
begin
  try
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      AData.DataSet.First;
      While not AData.DataSet.Eof do
      begin
        LYear := AData.DataSet.FieldByName('Year').AsInteger;
        if FDisplayMonth = 0 then
        begin
          for LIndex := 1 to 12 do
          begin
            LFieldName    := Format('%s%2.2d',['Value',LIndex]);
            if not AData.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
              ViewDialog.PlotActualVolumeSeries.AddY(LMonthlyValue,IntToStr(LYear));
            end;
          end;
        end
        else
        begin
          for LIndex := 1 to 12 do
          begin
            if FDisplayMonth = LIndex then
            begin
              LFieldName    := Format('%s%2.2d',['Value',LIndex]);
              if not AData.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
                if (FDisplayMonth <= 3) then
                  LDate := EncodeDate(LYear,FDisplayMonth + 9,1)
                else
                  LDate := EncodeDate(LYear+1,FDisplayMonth - 3,1);
                ViewDialog.PlotActualVolumeSeries.AddXY(LDate,LMonthlyValue);
              end;
            end;
          end;
        end;
        AData.DataSet.Next;
      end;
    end;
    FomartChartToActualVolumeSeries;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputGraphValidator.SetCurrentViewData;
const OPNAME = 'TOutputGraphValidator.SetCurrentViewData';
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

procedure TOutputGraphValidator.GetSelectionData;
const OPNAME = 'TOutputGraphValidator.GetSelectionData';
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
     FDefValueType := FValueType;
     FTimeStep     := LDataSelection.TimeStep;
     FHighLight    := LDataSelection.Highlight;
     FDisplayMonth := LDataSelection.DisplayMonth;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputGraphValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
    ViewDialog.FullLevelLineSeries.Active := False;
    ViewDialog.DeadLineSeries.Active      := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.OnPlotActualVolumeClick(Sender: TObject);
const OPNAME = 'TOutputGraphValidator.OnPlotActualVolumeClick';
begin
  try
    if not ViewDialog.PlotActualVolume.Checked then
      FAppModules.ViewIni.WriteString('TBoxPlotSeriesSelector','PlotActualVolume','N')
    else
      FAppModules.ViewIni.WriteString('TBoxPlotSeriesSelector','PlotActualVolume','Y');
    StudyHasChanged;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateChannelChartData(AData: TStrings;AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputGraphValidator.PopulateChannelChartData';
var
  LMonthNumbers     : array[1..12] of integer;
  lMonthDays        : array[1..12] of double;
  LYear,
  LStartMonth,
  LIndex,
  LCount            : integer;
  LMonthlyValues    : TStringList;
  LMonthlyValue     : double;
  LNeedDemand       : boolean;
  LDemand,
  LMonthValue       : double;
  LFactor         : double;
  LTotal            : double;
  lDemandStr        : WideString;
  LErrors           : string;
  LDate             : TDateTime;
  LDemandFeature    : ISpecifiedDemandFeature;
  LMinMaxFeature    : IMinMaxFlowConstraint;
  LYieldModelData   : TYieldModelDataObject;
begin
  try
    ClearChart;
    LYieldModelData := TYieldModelDataObject(FAppModules.Model.ModelData);
    LDemandFeature  := AChannel.SpecifiedDemandFeature;
    LMinMaxFeature  := AChannel.MinMaxFlowConstraint;
    ViewDialog.ShowError(LErrors);
    if(AData.Count > 0) then
    begin
      ViewDialog.Chart.Visible := True;
      ViewDialog.LineSeries.Active := True;
      ViewDialog.LineSeries.XValues.DateTime := True;
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TSCSheet.X_AxisLabel');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;
      ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.00';
      ViewDialog.Chart.LeftAxis.TitleSize        := 1;
      ViewDialog.Chart.Title.Text.Text := ElementName + '(Sequence = '+
        IntToStr(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.LoadCase) +
        ' Load Case = '+IntToStr(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.LoadCase) + ')';
      ViewDialog.LineSeries.Name := 'Supply';

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      for LIndex := 1 to 12 do
      begin
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
        LStartMonth := LStartMonth + 1;
      end;

      for LIndex := 1 to 12 do
      begin
        lMonthDays[lIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
      end;
      FAtherChannelType := False;
      LNeedDemand := (FUnits = ouPercentage) or (FValueType = ovtDeficits);
      LMonthValue := 0.0;
      LDemand     := 0.0;
      FAtherChannelType := (AChannel.ChannelType <> 2) and (AChannel.ChannelType <> 8) and
                           (AChannel.ChannelType <> 11) and (AChannel.ChannelType <> 14);
      LMonthlyValues := TStringList.Create;
      try
        for LIndex := 0 to AData.Count -1 do   {Years}
        begin
          LMonthlyValues.CommaText := AData.Strings[LIndex];
          LYear := StrToInt(LMonthlyValues.Strings[0]);
          LTotal := 0.0;
          for LCount := 1 to LMonthlyValues.Count -2 do {Months}
          begin
            if (FTimeStep = otsAnnual) OR (FDisplayMonth = 0) OR (FDisplayMonth = LCount) then
            begin
              LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);
              if LNeedDemand then
              begin
                if (AChannel <> nil) then
                begin
                  if (AChannel.ChannelType = 2) then
                  begin
                    LDemand := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.TargetYieldByIndex[FLoadCase];
                    LFactor := lYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByIndex[0].FactorByMonth[LCount];
                    LDemand := (LDemand * LFactor * 1000000.0)/(365.25 * 86400.0);  {m3/s}
                  end
                  else if (AChannel.ChannelType = 8) then
                  begin
                    if (LMinMaxFeature <> nil) then
                    begin
                      //lMonthDays[LCount]    := lYieldModelData.RunConfigurationData.MonthDaysByIndex[LCount];
                      //lMonthDemands[LCount] := lMinMaxFeature.FlowConstraintByArcMonth[1, LCount];
                      LDemand := LMinMaxFeature.FlowConstraintByArcMonth[1, LCount];
                    end;
                  end
                  else if (AChannel.ChannelType = 11) then
                  begin
                    if (LDemandFeature <> nil) then
                    begin
                      if (LDemandFeature.GetAnnualDemand(lYear, lDemandStr)) then
                        LDemand := Round((StrToFloat(lDemandStr) * 1000000 / (365.25 * 86400.0)) * 1000) / 1000
                    end;
                  end
                  else
                  begin
                    if FAtherChannelType then
                    begin
                      if (FValueType in [ovtDeficits,ovtDemand]) then
                      begin
                        FDefValueType := FValueType;
                        FValueType := ovtSupply;
                      end;
                    end;
                  end;
                end;
              end;

              if (FUnits = ouPercentage) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                begin
                  if LDemand > 0 then
                    LMonthValue := (LDemand - LMonthlyValue)/LDemand * 100.0
                  else
                    LMonthValue := 0;
                end
                else if (FValueType = ovtDemand) then {ovtDemand}
                begin
                  if LMonthlyValue > 0 then
                    LMonthValue := (LDemand - LMonthlyValue)/LMonthlyValue * 100.0
                  else
                    LMonthValue := 0;
                end
                else {ovtSupply}
                begin
                  if (LDemand > 0) then
                    LMonthValue := LMonthlyValue/LDemand * 100
                  else
                    LMonthValue := 0;
                end;
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                  LMonthValue := LDemand - LMonthlyValue
                else if (FValueType = ovtDemand) then {ovtDemand}
                  LMonthValue := LDemand
                else {ovtSupply}
                  LMonthValue := LMonthlyValue;
              end
              else
              if (FUnits = ouMcmPerMonthOrYear) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                  LMonthValue  := (LDemand - LMonthlyValue) * (lMonthDays[LCount] * 86400.0) /1000000.0
                else if (FValueType = ovtDemand) then {ovtDemand}
                  LMonthValue  := (LDemand) * (lMonthDays[LCount] * 86400.0) /1000000.0
                else {ovtSupply}
                  LMonthValue  := LMonthlyValue * (lMonthDays[LCount] * 86400.0) /1000000.0;
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                  LMonthValue  := (LDemand - LMonthlyValue) * ( 86400.0) / 1000.0
                else if (FValueType = ovtDemand) then {ovtDemand}
                  LMonthValue  := (LDemand) * ( 86400.0) / 1000.0
                else {ovtSupply}
                  LMonthValue  := LMonthlyValue * (86400.0) / 1000.0;
              end;


              if (FDisplayMonth > 0) then
              begin
                if (FDisplayMonth <= 3) then
                  LDate := EncodeDate(LYear,FDisplayMonth + 9,1)
                else
                  LDate := EncodeDate(LYear + 1,FDisplayMonth - 3,1);
              end
              else
              begin
                if(LMonthNumbers[1] <> 1) and (LMonthNumbers[LCount] = 1) then
                  LYear := LYear + 1;
                LDate := EncodeDate(LYear,LMonthNumbers[LCount],1);
              end;

              if (FTimeStep <> otsAnnual) then
                ViewDialog.LineSeries.AddXY(LDate,LMonthValue);
            end;
            LTotal :=  LTotal + (LMonthValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount]);
          end;

          LTotal := LTotal / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
          if (FTimeStep = otsAnnual) then
          begin
            LDate := EncodeDate(LYear,FAppModules.StudyArea.CalendarStartMonth,1);
            ViewDialog.LineSeries.AddXY(LDate,LTotal);
          end;

        end;
          FomartMonthlyChart;
        if FAtherChannelType then
        begin
          if (FDefValueType = ovtDeficits) then
            FValueType := FDefValueType;
        end;

      finally
        LMonthlyValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateChannelChart(AData: TStrings);
const OPNAME = 'TOutputGraphValidator.PopulateChannelChart';
var
  LChannel    : IGeneralFlowChannel;
begin
  try
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                .ChannelList.ChannelByChannelNumber[FIdentifier];
    if (LChannel <> nil) then
    begin
      PopulateChannelChartData(AData,LChannel);
      if (FValueType in [ovtDemandAndSupply,ovtDemand]) and
         (lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
      begin
        PopulateDemandChartData(LChannel);
        ViewDialog.LineSeries.Active    := (FValueType = ovtDemandAndSupply);
        ViewDialog.Chart.Legend.Visible := ViewDialog.DemandLineSeries.Active and ViewDialog.LineSeries.Active;
        ViewDialog.DemandLineSeries.Name := 'Demand';
        if(ViewDialog.Chart.LeftAxis.Maximum < ViewDialog.DemandLineSeries.MaxYValue) then
          ViewDialog.Chart.LeftAxis.Maximum := ViewDialog.DemandLineSeries.MaxYValue + (0.05 * ViewDialog.DemandLineSeries.MaxYValue);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateChannelAreaChart(AData: TStrings);
const OPNAME = 'TOutputGraphValidator.PopulateChannelAreaChart';
var
  LChannel : IGeneralFlowChannel;
begin
  try
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
              .CastChannelList.CastChannelNumberFromChannelID[FIdentifier];
    if (LChannel <> nil) then
    begin
      PopulateChannelChartData(AData,LChannel);
      if (FUnits = ouPercentage) then
        ShowMessage(FAppModules.Language.GetString('Message.PercentageNotAllowed'))
      else if( FValueType = ovtDeficits) then
        ShowMessage((FAppModules.Language.GetString('Message.DeficitNotAllowed')));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.ClearChart;
const OPNAME = 'TOutputGraphValidator.ClearChart';
begin
  try
    ViewDialog.LineSeries.Clear;
    ViewDialog.DemandLineSeries.Clear;
    ViewDialog.LineSeries.Active := False;
    ViewDialog.DemandLineSeries.Active := False;
    ViewDialog.DeadLineSeries.Clear;
    ViewDialog.DeadLineSeries.Active := False;
    ViewDialog.FullLevelLineSeries.Clear;
    ViewDialog.FullLevelLineSeries.Active := False;
    ViewDialog.PlotActualVolumeSeries.Clear;
    ViewDialog.PlotActualVolumeSeries.Active := False;

    ViewDialog.Chart.LeftAxis.Title.Caption := '';
    ViewDialog.Chart.BottomAxis.Title.Caption := '';
    ViewDialog.Chart.Title.Text.Clear;
    ViewDialog.Chart.UndoZoom;
    ViewDialog.Chart.Legend.Visible := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.ElementName: string;
const OPNAME = 'TOutputGraphValidator.ElementName';
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

function TOutputGraphValidator.CanExport: boolean;
const OPNAME = 'TOutputGraphValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.CanPrint: boolean;
const OPNAME = 'TOutputGraphValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputGraphValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.DoPrint;
const OPNAME = 'TOutputGraphValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.DoShowExceedenceProbabilyty(Sender: TObject);
const OPNAME = 'TOutputGraphValidator.OnBtnDataSelectionClick';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputGraphValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdGraph,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.FomartMonthlyChart;
const OPNAME = 'TOutputGraphValidator.FomartMonthlyChart';
var
  LMaxFactor : double;
  LMinFactor : double;
begin
  try
    {if (FUnits = ouPercentage) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supply')
    else
    if (FUnits = ouPerSecond) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.Supplym3')
    else
    if (FUnits = ouMegaLitersPerDay) then
      ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMegaL')
    else
    ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DistributionCurve.SupplyMillionPerMonth');
    }
    if (FUnits = ouPercentage) then
    begin
      LMinFactor := ViewDialog.LineSeries.YValues.MinValue;
      LMinFactor := LMinFactor * 0.05;
      ViewDialog.Chart.LeftAxis.SetMinMax(ViewDialog.LineSeries.YValues.MinValue - LMinFactor, 105);
    end
    else
    begin
      LMaxFactor := ViewDialog.LineSeries.YValues.MaxValue;
      LMinFactor := ViewDialog.LineSeries.YValues.MinValue;
      LMaxFactor := LMaxFactor * 0.05;
      LMinFactor := LMinFactor * 0.05;
      ViewDialog.Chart.LeftAxis.SetMinMax(ViewDialog.LineSeries.YValues.MinValue - LMinFactor, ViewDialog.LineSeries.YValues.MaxValue + LMaxFactor);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.FomartChartToActualVolumeSeries;
const OPNAME = 'TOutputGraphValidator.FomartChartToActualVolumeSeries';
begin
{var
  LFactor : double;
begin
  try
    if (FUnits = ouPercentage) then
      ViewDialog.Chart.LeftAxis.SetMinMax(0,105)
    else
    begin

      LFactor := ViewDialog.PlotActualVolumeSeries.YValues.MaxValue;
      LFactor := LFactor * 0.05;
      ViewDialog.Chart.LeftAxis.SetMinMax(0,ViewDialog.PlotActualVolumeSeries.YValues.MaxValue + LFactor);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;}
end;


procedure TOutputGraphValidator.PopulateReservoirChart(AData: TStrings);
const OPNAME = 'TOutputGraphValidator.PopulateReservoirChart';
var
  LMonthNumbers    : array[1..12] of integer;
  LYear,
  LStartMonth,
  LIndex,
  LCount           : integer;
  LDate            : TDateTime;
  LMonthlyValues   : TStringList;
  LMonthlyValue    : double;
  LReservoirVolume : double;
  LFullElevation,
  LFE              : double;
  LTotal           : double;
  LDeadStorageLvl,
  LDLS             : double;
  LWetland         : TWetland;
  LReservoirData   : IReservoirData;
  LNodeWithInflowData   : IReservoirData;
  LDeadStorageLevel     : double;
  LBottomLevel          : double;  
begin
  try
    ClearChart;
    if(AData.Count > 0) then
    begin
      ViewDialog.Chart.Visible := True;
      ViewDialog.LineSeries.Active := True;
      ViewDialog.LineSeries.XValues.DateTime := True;
      ViewDialog.DeadLineSeries.Active := True;
      ViewDialog.DeadLineSeries.XValues.DateTime := True;
      ViewDialog.FullLevelLineSeries.Active := True;
      ViewDialog.FullLevelLineSeries.XValues.DateTime := True;
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TSCSheet.X_AxisLabel');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;
      ViewDialog.Chart.LeftAxis.AxisValuesFormat := '######0.00';
      ViewDialog.Chart.LeftAxis.TitleSize        := 1;
      ViewDialog.Chart.Title.Text.Text := ElementName + '(Sequence = '+
        IntToStr(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.LoadCase) +
        ' Load Case = '+IntToStr(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.LoadCase) + ')';

      LReservoirVolume := 0.0;
      LFullElevation   := 0.0;
      LFE              := 0.0;
      LDeadStorageLvl  := 0.0;
      LDLS             := 0.0;
      LReservoirData        := nil;
      LWetland              := nil;
      LNodeWithInflowData   := nil;
      if (NetworkElementType = votReservoir) then
      begin
        LReservoirData  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
        LDeadStorageLvl := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
        LFullElevation  := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
        LFE   := LFullElevation;
        LDLS  := LDeadStorageLvl;
      end
      else
      if (NetworkElementType = votReservoirAreaGroup) then
      begin
        LReservoirData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.CastReservoirByReservoirGroupID[FIdentifier];
        LDeadStorageLvl := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
        LFullElevation  := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
        LFE   := LFullElevation;
        LDLS  := LDeadStorageLvl;
      end
      else if (NetworkElementType = votReservoir) then
      begin
        LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastWetlandList.CastWetlandByNodeNumber(FIdentifier);
        LDeadStorageLvl := 0;
        LFullElevation  := 0;
        LFE   := LFullElevation;
        LDLS  := LDeadStorageLvl;
      end
      else if (NetworkElementType = votNodeWithInflow) then
      begin
        LNodeWithInflowData  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
        LDeadStorageLvl := 0;
        LFullElevation  := 0;
        LFE   := LFullElevation;
        LDLS  := LDeadStorageLvl;
      end;

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      for LIndex := 1 to 12 do
      begin
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
        LStartMonth := LStartMonth + 1;
      end;

      LMonthlyValue := 0;
      LMonthlyValues := TStringList.Create;
      try
        for LIndex := 0 to AData.Count -1 do  {For every year }
        begin
          LTotal := 0.0;
          LMonthlyValues.CommaText := AData.Strings[LIndex];
          LYear := StrToInt(LMonthlyValues.Strings[0]);
          for LCount := 1 to LMonthlyValues.Count -2 do {For every month }
          begin
            if (FTimeStep = otsAnnual) OR (FDisplayMonth = 0) OR (FDisplayMonth = LCount) then
            begin
              //LDate := EncodeDate(LYear,LMonthNumbers[LCount-1],1);
              LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);

              if (FUnits = ouPerSecond) then
              begin
                LMonthlyValue   := LMonthlyValue;
                LDeadStorageLvl := LDLS;
                LFullElevation  := LFE;
              end;

              if (FUnits = ouMcmPerMonthOrYear) then
              begin
                LMonthlyValue   := (LMonthlyValue *  86400.0) /1000000.0;
                LDeadStorageLvl := (LDLS *  86400.0) /1000000.0;
                LFullElevation  := (LFE *  86400.0) /1000000.0;
              end;

              if (FUnits = ouMegaLitersPerDay) then
              begin
                LMonthlyValue :=(LMonthlyValue *  86400.0) / 1000.0;
                LDeadStorageLvl := (LDLS *  86400.0) /1000.0;
                LFullElevation  := (LFE *  86400.0) /1000.0;
              end;

              if (FUnits = ouPercentage) then
              begin
                if (FCurrentViewData = btMonthEndReservoirVolume) then
                begin
                  if (LReservoirData <> nil) then
                    LReservoirVolume :=  LReservoirData.ReservoirConfigurationData.VolumeWhenFull
                  else if (LWetland <> nil) then
                    LReservoirVolume := LWetland.StorageVolume
                  else if (LNodeWithInflowData <> nil) then
                    LReservoirVolume := 1.0;

                  LMonthlyValue    :=  LMonthlyValue/LReservoirVolume * 100.0;
                  
                end
                else if (FCurrentViewData = btMonthEndReservoirElevation) then
                begin
                  if (LReservoirData <> nil) then
                  begin
                    LBottomLevel := LReservoirData.ReservoirZoneElevationsData.BottomOfReservoir.Elevation;
                    if (LFullElevation-LBottomLevel) > 0 then
                      LMonthlyValue := (LMonthlyValue-LBottomLevel)/(LFullElevation-LBottomLevel) * 100.0;
                  end
                  else
                    LMonthlyValue  := LMonthlyValue/LFullElevation * 100.0;
                end
                else
                begin

                  if (LMonthlyValue <> 0.0) then
                    LMonthlyValue := LMonthlyValue/LMonthlyValue * 100.0;
                end;
              end;

              if (FUnits = ouLivePercentage) then
              begin

                if (FCurrentViewData = btMonthEndReservoirVolume) then
                begin
                  if (LReservoirData <> nil) then
                  begin
                    LReservoirVolume := LReservoirData.ReservoirConfigurationData.VolumeWhenFull;
                    LDeadStorageLevel := LReservoirData.GetReservoirVolumeByElevation(LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);
                    if (LReservoirVolume-LDeadStorageLevel)>0 then
                      LMonthlyValue    :=  (LMonthlyValue-LDeadStorageLevel)/(LReservoirVolume-LDeadStorageLevel) * 100.0;
                                      end
                  else if (LWetland <> nil) then
                  begin
                    LReservoirVolume := LWetland.StorageVolume;
                    LMonthlyValue    :=  LMonthlyValue/LReservoirVolume * 100.0;
                  end
                  else if (LNodeWithInflowData <> nil) then
                  begin
                    LReservoirVolume := 1.0;
                    LMonthlyValue    :=  LMonthlyValue/LReservoirVolume * 100.0;
                  end;

                end


                else if (FCurrentViewData = btMonthEndReservoirElevation) then
                begin
                  if LReservoirData <> nil then
                  begin
                    LDeadStorageLvl := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
                    LFullElevation  := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
                    if (LFullElevation-LDeadStorageLvl)>0 then
                      LMonthlyValue  := (LMonthlyValue-LDeadStorageLvl)/(LFullElevation-LDeadStorageLvl) * 100.0;
                  end;
                end
                else
                begin
                  if (LMonthlyValue <> 0.0) then
                    LMonthlyValue := LMonthlyValue/LMonthlyValue * 100.0;
                end;


              end;


              if (FDisplayMonth > 0) then
              begin
                if (FDisplayMonth <= 3) then
                  LDate := EncodeDate(LYear,FDisplayMonth + 9,1)
                else
                  LDate := EncodeDate(LYear + 1,FDisplayMonth - 3,1);
              end
              else
              begin
                if(LMonthNumbers[1] <> 1) and (LMonthNumbers[LCount] = 1) then
                  LYear := LYear + 1;
                LDate := EncodeDate(LYear,LMonthNumbers[LCount],1);
              end;

              ViewDialog.DeadLineSeries.AddXY(LDate,LDeadStorageLvl);
              ViewDialog.FullLevelLineSeries.AddXY(LDate,LFullElevation);

              if (FTimeStep <> otsAnnual) then
                ViewDialog.LineSeries.AddXY(LDate,LMonthlyValue);

            end;
            LTotal :=  LTotal + (LMonthlyValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount]);
          end; {end month - loop}
          LTotal := LTotal / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
          //LYear := LYear + 1;
          if (FTimeStep = otsAnnual) then
          begin
            LDate := EncodeDate(LYear,FAppModules.StudyArea.CalendarStartMonth,1);
            ViewDialog.LineSeries.AddXY(LDate,LTotal);
          end;
        end; {end year loop}
        FomartMonthlyChart;
      finally
        LMonthlyValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateTargetRelease(AOutputDataList: TDDTSOutputDataList);
const OPNAME = 'TOutputGraphValidator.PopulateTargetRelease';
var
  LIndex : integer;
  LOutputData : TDDTSOutputData;
  LEWRTargetData : TStringList;
  LReleaseData : TStringList;
  LValues  : TStringList;
  LValue : double;
  LXValue : double;
  LMaxFactor : double;
  LFactor : double;
begin
  try
    if AOutputDataList <> nil then
    begin
      LValues  := TStringList.Create;
      LEWRTargetData := TStringList.Create;
      LEWRTargetData.Sorted := True;
      LEWRTargetData.Duplicates := dupAccept;
      LReleaseData := TStringList.Create;
      LReleaseData.Sorted := True;
      LReleaseData.Duplicates := dupAccept;
     // ViewDialog.Chart.LeftAxis.Automatic := False;
      ViewDialog.Chart.BottomAxis.LabelsAngle := 0;
      ViewDialog.Chart.BottomAxis.Increment := 10;
      LMaxFactor := 1;
      try
        for LIndex := 0 to AOutputDataList.Count-1 do
        begin
          LOutputData := AOutputDataList.OutputDataByIndex[LIndex];
          LValue := LOutputData.ColumnS;
          LEWRTargetData.Add(FormatFloat('0000.0000',LValue));
          LValue := LOutputData.ColumnAB;
          LReleaseData.Add(FormatFloat('0000.0000',LValue));
        end;
        LValues.Clear;
        for LIndex := LEWRTargetData.Count-1 downto 0  do
          LValues.Add(LEWRTargetData[LIndex]);

        for LIndex := 0 to LValues.Count-1  do
        begin
          LXValue := (LIndex)/(LValues.Count) * 100;
          ViewDialog.ComparisonSeries[0].XValues.DateTime := False;
          ViewDialog.ComparisonSeries[0].AddXY(LXValue, StrToFloat(LValues[LIndex]));
          ViewDialog.ComparisonSeries[0].SeriesColor := clBlack;
        end;
        LFactor := ViewDialog.ComparisonSeries[0].YValues.MaxValue;
        if LFactor>LMaxFactor then
          LMaxFactor := LFactor;
        LValues.Clear;
        for LIndex := LReleaseData.Count-1 downto 0  do
          LValues.Add(LReleaseData[LIndex]);

        for LIndex := 0 to LValues.Count-1  do
        begin
          LXValue := (LIndex)/(LValues.Count) * 100;
          ViewDialog.ComparisonSeries[1].XValues.DateTime := False;
          ViewDialog.ComparisonSeries[1].AddXY(LXValue, StrToFloat(LValues[LIndex]));
          ViewDialog.ComparisonSeries[1].SeriesColor := clRed;
        end;
        LFactor := ViewDialog.ComparisonSeries[1].YValues.MaxValue;
        if LFactor>LMaxFactor then
          LMaxFactor := LFactor;

        LFactor := LFactor * 0.05;
        ViewDialog.Chart.LeftAxis.SetMinMax(0, LMaxFactor + LFactor);
        ViewDialog.Chart.BottomAxis.Title.Caption := 'Exeedence Probability';
      finally
        FreeAndNil(LEWRTargetData);
        FreeAndNil(LReleaseData);
        FreeAndNil(LValues);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateReservoirAreaChart(AData: TStrings);
const OPNAME = 'TOutputGraphValidator.PopulateReservoirAreaChart';
var
  LReservoirData : IReservoirData;
begin
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
              .CastReservoirList.CastReservoirByReservoirGroupID[FIdentifier];
    if (LReservoirData <> nil) then
    begin
      PopulateReservoirChart(AData);
      if (FUnits = ouPercentage) then
        ShowMessage(FAppModules.Language.GetString('Message.PercentageNotAllowed'))
      else if( FValueType = ovtDeficits) then
        ShowMessage((FAppModules.Language.GetString('Message.DeficitNotAllowed')));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.ChangeViewDataLabel;
const OPNAME = 'TOutputGraphValidator.ChangeViewDataLabel';
begin
  try
    ViewDialog.Chart.LeftAxis.Title.Caption := '';
    ViewDialog.UnitsLabel.Caption:= '';
    if (FCurrentViewData = btMonthlyAverageChannelFlow)  then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perMonth')+')'
      else if (FUnits = ouPercentage) or (FUnits = ouLivePercentage)then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('ChangeLists.Percentage')+')'
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.MegaL')+')'
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text + ' ' + ViewDialog.UnitsLabel.Caption;
    end;

    if (FCurrentViewData = btMonthEndReservoirElevation) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('TField.m')+')'
      else if (FUnits = ouPercentage) or (FUnits = ouLivePercentage)then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('ChangeLists.Percentage')+')'
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('TField.m')+')';
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text + ' ' + ViewDialog.UnitsLabel.Caption;
    end;

    if (FCurrentViewData = btMonthEndReservoirVolume) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+LowerCase(FAppModules.Language.GetString('TField.MCM'))+')'
      else if (FUnits = ouPercentage) or (FUnits = ouLivePercentage) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('ChangeLists.Percentage')+')'
      else
        ViewDialog.UnitsLabel.Caption := '('+LowerCase(FAppModules.Language.GetString('TField.MCM'))+')';
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text + ' ' + ViewDialog.UnitsLabel.Caption;
     end;

    if (FCurrentViewData = btNetBasinRunoffIntoResArea) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perMonth')+')'
      else if (FUnits = ouPercentage) or (FUnits = ouLivePercentage) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('ChangeLists.Percentage')+')'
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.MegaL')+')'
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text + ' ' + ViewDialog.UnitsLabel.Caption;
    end;

    if (FCurrentViewData = btRainfallOnReservoirSurface) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perMonth')+')'
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('ChangeLists.Percentage')+')'
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.MegaL')+')'
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text + ' ' + ViewDialog.UnitsLabel.Caption;
    end;

    if (FCurrentViewData = btGrossEvaporationLossFromReservoir) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perMonth')+')'
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('ChangeLists.Percentage')+')'
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.MegaL')+')'
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
      ViewDialog.Chart.LeftAxis.Title.Caption := ViewDialog.cmbViewDataType.Text + ' ' + ViewDialog.UnitsLabel.Caption;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.GetChannelNumberByChannelArea(AChannelAreaID: integer): integer;
const OPNAME = 'TOutputGraphValidator.GetChannelNumberByChannelArea';
var
  LChannel : IGeneralFlowChannel;
begin
  Result := 0;
  try
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
                .CastChannelList.CastChannelNumberFromChannelID[FIdentifier];
    if (LChannel <> nil) then
      Result := LChannel.ChannelNumber;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphValidator.GetReservoirNumberByGroupID(AGroupID: integer): integer;
const OPNAME = 'TOutputGraphValidator.GetReservoirNumberByGroupID';
var
  LReservoirData : IReservoirData;
begin
  Result := 0;
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
                .CastReservoirList.CastReservoirByReservoirGroupID[AGroupID];
    if (LReservoirData <> nil) then
      Result := LReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphValidator.PopulateDemandChartData(AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputGraphValidator.PopulateDemandChartData';
var
  LIndex,
  LCount             : integer;
  LDate              : TDateTime;
  LErrors            : string;
  LYear              : word;
  LStartMonth        : word;
  LMonthlyValues,
  LChannelDemandValues : TStringList;
  LYieldModelData      : TYieldModelDataObject;
  LMonthlyValue        : double;
  lMonthDays           : array [1..12] of double;
  LMonthNumbers     : array[1..12] of integer;
begin
  try
    if (FUnits = ouPercentage) then Exit;
    LChannelDemandValues := TStringList.Create;
    LMonthlyValues := TStringList.Create;
    try
      LYieldModelData := TYieldModelDataObject(FAppModules.Model.ModelData);
      LYieldModelData.OutputData.CastDemandOutputData.GetChannelDemandValues(LChannelDemandValues,AChannel.ChannelNumber,LErrors);
      if(LChannelDemandValues.Count > 0) then
      begin

        LStartMonth := FAppModules.StudyArea.CalendarStartMonth;
        for LIndex := 1 to 12 do
        begin
          if(LStartMonth > 12) then
             LStartMonth := 1;
          LMonthNumbers[LIndex] := LStartMonth;
          LStartMonth := LStartMonth + 1;
        end;

        ViewDialog.DemandLineSeries.Active := True;
        ViewDialog.DemandLineSeries.XValues.DateTime := True;
        for LIndex := 1 to 12 do
        begin
          lMonthDays[lIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
        end;

        for LIndex := 0 to LChannelDemandValues.Count -2 do   {Years}
        begin
          LMonthlyValues.CommaText := LChannelDemandValues.Strings[LIndex];
          LYear                    := StrToInt(LMonthlyValues.Strings[0]);
          for LCount := 1 to 12 do {Months}
          begin
            if(LCount >= LMonthlyValues.Count) then Break;
            if (FTimeStep = otsAnnual) OR (FDisplayMonth = 0) OR (FDisplayMonth = LCount) then
            begin
              LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);
              if (FUnits = ouMcmPerMonthOrYear) then
              begin
                  LMonthlyValue  := LMonthlyValue * (lMonthDays[LCount] * 86400.0) /1000000.0;
              end
              else if (FUnits = ouMegaLitersPerDay) then
              begin
                  LMonthlyValue  := LMonthlyValue * (86400.0) / 1000.0;
              end;

              if (FDisplayMonth > 0) then
              begin
                if (FDisplayMonth <= 3) then
                  LDate := EncodeDate(LYear,FDisplayMonth + 9,1)
                else
                  LDate := EncodeDate(LYear + 1,FDisplayMonth - 3,1);
              end
              else
              begin
                if(LMonthNumbers[1] <> 1) and (LMonthNumbers[LCount] = 1) then
                  LYear := LYear + 1;
                LDate := EncodeDate(LYear,LMonthNumbers[LCount],1);
              end;

              ViewDialog.DemandLineSeries.AddXY(LDate,LMonthlyValue);
            end;
          end;
        end;
      end;
    finally
      LMonthlyValues.Free;
      LChannelDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

