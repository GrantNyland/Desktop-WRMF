
unit UOutputWRPMBoxPlotGraphValidator;

interface

uses
  Windows,
  Classes,
  VCLTee.Chart,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  VCLTee.TeeBoxPlot,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Graphics,
  Contnrs,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UOutputBoxPlotGraphDialog;

type
  TOutputWRPMBoxPlotGraphValidator = class(TAbstractOutputDialogValidator)
  private
    procedure GetDamLevelsDataSet(AData: TAbstractModelDataset);
    function GetScenarioWhereClause: string;
    procedure PopulateDamLevelsChartData(AData: TAbstractModelDataset);
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure OnBtnLegendClick(Sender: TObject);
    procedure OnViewDataTypeChange(Sender: TObject);
    procedure OnShowDamLevelSeriesClick(Sender: TObject);

    procedure PopulateDamLevelSeries;
    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure PopulatePltGraph(AData : TStrings);
    procedure PopulateResGraph(AData : TStrings);
    procedure PopulateSysGraph(AData : TStrings;AColumnName: string);
    procedure PopulateInterBasinSupportGraph(AData : TStrings);
    procedure PopulatePmpGraph(AData : TStrings);
    function CalculateBoxWidth(ABoxCount : integer): integer;
    function ViewDialog: TOutputBoxPlotGraphDialog;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

  end;

implementation

uses
  VCLTee.Series,
  SysUtils,
  Math,
  UFileNames,
  VoaimsCom_TLB,
  UConstants,
  UOutputData,
  UDataSetType,
  UYieldModelDataObject,
  UWRPMOutputSettings,
  UWRPMPostProcessorData,
  UBoxPlotData,
  UBoxPlot13Chart,
  UPlanningModelDataObject,
  UAbstractFileNamesObject,
  UAllocationDefinitionData,
  UErrorHandlingOperations;

{ TOutputWRPMBoxPlotGraphValidator }

procedure TOutputWRPMBoxPlotGraphValidator.CreateMemberObjects;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputBoxPlotGraphDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.DestroyMemberObjects;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.Initialise: Boolean;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ViewDialog.cmbViewDataType.OnSelect   := OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick   := OnBtnDataSelectionClick;
    ViewDialog.BtnLegend.OnClick          := OnBtnLegendClick;
    ViewDialog.ShowDamLevelChkBox.OnClick := OnShowDamLevelSeriesClick;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'HYDROSEQCOUNT') or
      (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputBoxPlotGraph');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.ViewDialog : TOutputBoxPlotGraphDialog;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputBoxPlotGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulateDialogSelectors';
var
  LIndex       : integer;
  LName        : string;
  LSysPrefix   : string;
  LFileName    : TAbstractModelFileName;
  LModelData   : TPlanningModelDataObject;
begin
  try
    ViewDialog.BtnDataSelection.Enabled := True;
    ViewDialog.cmbViewDataType.Visible  := False;
    ViewDialog.ViewDataLabel.Visible    := False;
    ViewDialog.cmbViewDataType.Clear;

    case NetworkElementType of
      votMasterControl,votReviewSubSystemStorage,votReviewDamStorage:
      begin
      end;
      votReviewTotalSystemStorage:
      begin
      end;
      votReviewSubSystemCurtailment:
      begin
      end;
      votTotalSystemCurtailment:
      begin
      end;
      votReviewDemandSupply:
      begin
      end;
      votReviewInterBasinSupport:
      begin
        LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
        LSysPrefix := UpperCase(LModelData.DataFilePath.DataFilePrefix)+ 'RES';
        for LIndex := 0 to LModelData.FileNamesObject.OutputFileNames.Count - 1 do
        begin
          LFileName := LModelData.FileNamesObject.OutputFileNames.FileNameObject[LIndex];
          if Assigned(LFileName) and FileExists(LFileName.FileName) then
          begin
            LName := UpperCase(ExtractFileName(LFileName.ShortName));
            if(Pos(LSysPrefix,LName) > 0) then
              ViewDialog.cmbViewDataType.Items.Add(LName);
          end;
        end;
        ViewDialog.cmbViewDataType.Visible   := True;
        ViewDialog.ViewDataLabel.Visible     := True;
        ViewDialog.cmbViewDataType.ItemIndex := 0;
      end;
      votChannel:
      begin
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.OnBtnDataSelectionClick';
begin
  try
    TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdWRPMGraph,btNone,ovtNone);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.OnViewDataTypeChange';
begin
  try
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.OnBtnLegendClick(Sender: TObject);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.OnBtnLegendClick';
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
      LImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'WRPMBoxPlotLegend');
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

procedure TOutputWRPMBoxPlotGraphValidator.PopulateDamLevelSeries;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulateDamLevelSeries';
var
  LDataSet : TAbstractModelDataset;
begin
  try
    ViewDialog.DamLevelSeries.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      GetDamLevelsDataSet(LDataSet);
      if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
        PopulateDamLevelsChartData(LDataSet);

      finally
        LDataSet.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.GetDamLevelsDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.GetDamLevelsDataSet';
var
  LIndex         : integer;
  LSQL           : string;
  lReservoir     : IReservoirData;
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
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

          //if(Pos(LFileNameObject.ShortName,ViewDialog.Chart.Title.Text.Text) = 0) then
          //  ViewDialog.Chart.Title.Text.Add(LFileNameObject.ShortName);
          //ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.DamLevel');
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
                  ' AND FileName = '+QuotedStr(LFileNameObject.ShortName);
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.GetScenarioWhereClause: string;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.GetScenarioWhereClause';
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

procedure TOutputWRPMBoxPlotGraphValidator.PopulateDamLevelsChartData(AData: TAbstractModelDataset);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulateDamLevelsChartData';
var
  LYear,
  LIndex: integer;
  LMonthlyValue : double;
  LDate: TDateTime;
  LFieldName: string;
begin
  try
    //ClearChart;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.DamLevelSeries.Visible := True;
      ViewDialog.DamLevelSeries.XValues.DateTime := True;
      //ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      //ViewDialog.Chart.BottomAxis.LabelsAngle := 90;

      AData.DataSet.First;
      While not AData.DataSet.Eof do
      begin
        LYear := AData.DataSet.FieldByName('Year').AsInteger;
        for LIndex := 1 to 12 do
        begin
          LDate := EncodeDate(LYear,LIndex,1);
          LFieldName    := Format('%s%2.2d',['Value',LIndex]);
          if not AData.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
            ViewDialog.DamLevelSeries.AddXY(LDate,LMonthlyValue);
          end;
        end;
        AData.DataSet.Next;
      end;
    end;
    ViewDialog.Chart.LeftAxis.AdjustMaxMin;
    ViewDialog.Chart.BottomAxis.AdjustMaxMin;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.PopulateDataViewer;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.ClearDataViewer;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LIndex := ViewDialog.Chart.SeriesList.Count-1 downto 0 do
    begin
      if(ViewDialog.Chart.SeriesList.Items[LIndex].Name <> 'DamVolume') then
        ViewDialog.Chart.SeriesList.Delete(LIndex);
    end;

    //ViewDialog.Chart.SeriesList.Clear;
    ViewDialog.Chart.UndoZoom;
    ViewDialog.cmbViewDataType.Items.Clear;
    ViewDialog.ShowDamLevelChkBox.Visible := False;
    ViewDialog.DamLevelSeries.Visible     := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.RePopulateDataViewer;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.RePopulateDataViewer';
var
  LFileName : string;
  LOutputData : TStringList;
  LModelData : TPlanningModelDataObject;
begin
  try
    LOutputData := TStringList.Create;
    try
      LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
      case NetworkElementType of
        votMasterControl,votReviewDamStorage,votReviewSubSystemStorage:
        begin
          if LModelData.CastWRPMPostProcessorData.GetPltFileElementData(LOutputData,FIdentifier,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulatePltGraph(LOutputData);
          end;
        end;
        votReviewTotalSystemStorage:
        begin
          if LModelData.CastWRPMPostProcessorData.GetSysFileElementData(LOutputData,FIdentifier,ofcnSystemStorage,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateSysGraph(LOutputData,ofcnSystemStorage);
          end;
        end;
        votReviewSubSystemCurtailment:
        begin
          LFileName := LModelData.CastWRPMPostProcessorData.GetSubSystemSysFileName(FElementName);
          if LModelData.CastWRPMPostProcessorData.GetSysFileElementData(LOutputData,FIdentifier,FElementName,LFileName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateSysGraph(LOutputData,FElementName);
          end;
        end;
        votTotalSystemCurtailment:
        begin
          if LModelData.CastWRPMPostProcessorData.GetSysFileElementData(LOutputData,FIdentifier,ofcnSystemCurtailment,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateSysGraph(LOutputData,ofcnSystemCurtailment);
          end;
        end;
        votReviewDemandSupply:
        begin
          if LModelData.CastWRPMPostProcessorData.GetResFileElementData(LOutputData,FIdentifier,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateResGraph(LOutputData);
          end;
        end;
        votReviewInterBasinSupport:
        begin
          LFileName := ViewDialog.cmbViewDataType.Text;
          if LModelData.CastWRPMPostProcessorData.GetInterBasinSupportFileElementData(LOutputData,FIdentifier,FElementName,LFileName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateInterBasinSupportGraph(LOutputData);
          end;
        end;
        votChannel:
        begin
          if LModelData.CastWRPMPostProcessorData.GetPmpFileElementData(LOutputData,FIdentifier,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulatePmpGraph(LOutputData);
          end;
        end;
      end;
      ViewDialog.Chart.LeftAxis.AdjustMaxMin;
      ViewDialog.Chart.BottomAxis.AdjustMaxMin;
    finally
      LOutputData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputWRPMBoxPlotGraphValidator.PopulatePltGraph(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulatePltGraph';
var
  LWidth           : double;
  LIndex           : integer;
  LDate            : TDate;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
  LBoxChart        : TBoxPlotChart;
  LOutputTimeStep  : TOutputTimeStep;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    LBoxChart        := TBoxPlotChart.Create;
    try
      LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;

      ViewDialog.DamLevelSeries.Visible     := (NetworkElementType = votReviewDamStorage);
      ViewDialog.ShowDamLevelChkBox.Visible := (NetworkElementType = votReviewDamStorage);

      ViewDialog.Chart.Title.Text.Clear;
      case NetworkElementType of
        votMasterControl: ViewDialog.Chart.Title.Text.Add('Master Control');
        votReviewDamStorage: ViewDialog.Chart.Title.Text.Add('Dam Storage');
        votReviewSubSystemStorage:ViewDialog.Chart.Title.Text.Add('Sub-System Storage');
      end;

      if(LOutputTimeStep = otsMonthly) then
      begin
        ViewDialog.Chart.BottomAxis.Title.Caption := 'Months';
        ViewDialog.Chart.BottomAxis.DateTimeFormat := 'yyyy/MM';

        case NetworkElementType of
          votMasterControl:
          begin
            ViewDialog.Chart.Title.Text.Add(FElementName+'(Monthly)');
            case TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.Get_GetSelection.ValueType of
              ovtSupply: ViewDialog.Chart.LeftAxis.Title.Caption    := 'Supply (m³/s)';
              ovtDemand: ViewDialog.Chart.LeftAxis.Title.Caption    := 'Demand (m³/s)';
              ovtAllocated: ViewDialog.Chart.LeftAxis.Title.Caption := 'Allocation (m³/s)';
            end;//case
          end;
          votReviewDamStorage:
          begin
            ViewDialog.Chart.Title.Text.Add(FElementName);
            ViewDialog.Chart.LeftAxis.Title.Caption    := 'Storage (Mm³)';
          end;
          votReviewSubSystemStorage:
          begin
            ViewDialog.Chart.Title.Text.Add(FElementName);
            ViewDialog.Chart.LeftAxis.Title.Caption    := 'Storage (Mm³)';
          end;
        end
      end
      else if(LOutputTimeStep = otsAnnual) then
      begin
        ViewDialog.Chart.BottomAxis.Title.Caption := 'Years';
        ViewDialog.Chart.BottomAxis.DateTimeFormat := 'yyyy';

        if(NetworkElementType = votMasterControl) then
        begin
          ViewDialog.Chart.Title.Text.Add(FElementName+'(Annual)');
          case TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.Get_GetSelection.ValueType of
            ovtSupply: ViewDialog.Chart.LeftAxis.Title.Caption    := 'Supply (Mm³/a)';
            ovtDemand: ViewDialog.Chart.LeftAxis.Title.Caption    := 'Demand (Mm³/a)';
            ovtAllocated: ViewDialog.Chart.LeftAxis.Title.Caption := 'Allocation (Mm³/a)';
          end;//case
        end;
      end;

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LDate               := StrToDateDef(LLineData[1],0.0);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);
        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      LBoxChart.SetTheChart(ViewDialog.Chart);
      LWidth := CalculateBoxWidth(LBoxPlotDataList.Count);

      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LBoxChart.AddPoint(LBoxPlotData.XValueDate,LBoxPlotData.PercValue(Pecentile13[01]),
                                                   LBoxPlotData.PercValue(Pecentile13[02]),
                                                   LBoxPlotData.PercValue(Pecentile13[03]),
                                                   LBoxPlotData.PercValue(Pecentile13[04]),
                                                   LBoxPlotData.PercValue(Pecentile13[05]),
                                                   LBoxPlotData.PercValue(Pecentile13[06]),
                                                   LBoxPlotData.PercValue(Pecentile13[07]),
                                                   LBoxPlotData.PercValue(Pecentile13[08]),
                                                   LBoxPlotData.PercValue(Pecentile13[09]),
                                                   LBoxPlotData.PercValue(Pecentile13[10]),
                                                   LBoxPlotData.PercValue(Pecentile13[11]),
                                                   LBoxPlotData.PercValue(Pecentile13[12]),
                                                   LBoxPlotData.PercValue(Pecentile13[13]),
                           0.0,LWidth,MaxValue(LBoxPlotData.YValues),aCustomVertAxis);
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.PopulateResGraph(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulateResGraph';
var
  LYear,LMonth     : integer;
  LColIndex        : integer;
  LIndex           : integer;
  LDate            : TDate;
  LWidth           : double;
  LElementName     : string;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
  LValueType       : TOutputValueType;
  LBoxChart        : TBoxPlotChart;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    LBoxChart        := TBoxPlotChart.Create;
    try
      LValueType  := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.Get_GetSelection.ValueType;
      ViewDialog.Chart.Title.Text.Clear;
      ViewDialog.Chart.Title.Text.Add('Demand/Supply');
      LElementName  := Copy(FElementName,1,Length(FElementName)-4);
      case LValueType of
        ovtSupply    : ViewDialog.Chart.Title.Text.Add(LElementName +' - (Supply)');
        ovtDemand    : ViewDialog.Chart.Title.Text.Add(LElementName +' - (Demand)');
        ovtAllocated : ViewDialog.Chart.Title.Text.Add(LElementName +' - (Allocation)');
      end;
      ViewDialog.Chart.BottomAxis.Title.Caption := 'Years';
      ViewDialog.Chart.BottomAxis.DateTimeFormat := 'yyyy';
      ViewDialog.Chart.LeftAxis.Title.Caption    := 'Volume (MCM)';

      LColIndex   := 4;
      case LValueType of
        ovtDemand    : LColIndex   := 2;
        ovtAllocated : LColIndex   := 3;
        ovtSupply    : LColIndex   := 4;
      end;

      LMonth      := FAppModules.StudyArea.CalendarStartMonth;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LYear               := StrToInt(LLineData[1]);
        LDate               := EncodeDate(LYear,LMonth,01);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);

        LBoxPlotData.AddValue(StrToFloat(LLineData[LColIndex]));
      end;

      LBoxChart.SetTheChart(ViewDialog.Chart);
      LWidth := CalculateBoxWidth(LBoxPlotDataList.Count);

      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LBoxChart.AddPoint(LBoxPlotData.XValueDate,LBoxPlotData.PercValue(Pecentile13[01]),
                                                   LBoxPlotData.PercValue(Pecentile13[02]),
                                                   LBoxPlotData.PercValue(Pecentile13[03]),
                                                   LBoxPlotData.PercValue(Pecentile13[04]),
                                                   LBoxPlotData.PercValue(Pecentile13[05]),
                                                   LBoxPlotData.PercValue(Pecentile13[06]),
                                                   LBoxPlotData.PercValue(Pecentile13[07]),
                                                   LBoxPlotData.PercValue(Pecentile13[08]),
                                                   LBoxPlotData.PercValue(Pecentile13[09]),
                                                   LBoxPlotData.PercValue(Pecentile13[10]),
                                                   LBoxPlotData.PercValue(Pecentile13[11]),
                                                   LBoxPlotData.PercValue(Pecentile13[12]),
                                                   LBoxPlotData.PercValue(Pecentile13[13]),
                           0.0,LWidth,MaxValue(LBoxPlotData.YValues),aCustomVertAxis);
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.PopulateInterBasinSupportGraph(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulateInterBasinSupportGraph';
var
  LYear,LMonth     : integer;
  LWidth           : double;
  LIndex           : integer;
  LDate            : TDate;
  LFileName        : string;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
  LBoxChart        : TBoxPlotChart;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    LBoxChart        := TBoxPlotChart.Create;
    try
      LFileName  := ViewDialog.cmbViewDataType.Text;
      LFileName  := Copy(LFileName,1,Length(LFileName)-4);
      ViewDialog.Chart.Title.Text.Clear;
      ViewDialog.Chart.Title.Text.Add(FElementName +' ('+LFileName+')');
      ViewDialog.Chart.BottomAxis.Title.Caption := 'Years';
      ViewDialog.Chart.BottomAxis.DateTimeFormat := 'yyyy';
      ViewDialog.Chart.LeftAxis.Title.Caption    := 'Volume (MCM)';

      LMonth                 := FAppModules.StudyArea.CalendarStartMonth;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LYear               := StrToInt(LLineData[1]);
        LDate               := EncodeDate(LYear,LMonth,01);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);
        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      LBoxChart.SetTheChart(ViewDialog.Chart);
      LWidth := CalculateBoxWidth(LBoxPlotDataList.Count);

      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LBoxChart.AddPoint(LBoxPlotData.XValueDate,LBoxPlotData.PercValue(Pecentile13[01]),
                                                   LBoxPlotData.PercValue(Pecentile13[02]),
                                                   LBoxPlotData.PercValue(Pecentile13[03]),
                                                   LBoxPlotData.PercValue(Pecentile13[04]),
                                                   LBoxPlotData.PercValue(Pecentile13[05]),
                                                   LBoxPlotData.PercValue(Pecentile13[06]),
                                                   LBoxPlotData.PercValue(Pecentile13[07]),
                                                   LBoxPlotData.PercValue(Pecentile13[08]),
                                                   LBoxPlotData.PercValue(Pecentile13[09]),
                                                   LBoxPlotData.PercValue(Pecentile13[10]),
                                                   LBoxPlotData.PercValue(Pecentile13[11]),
                                                   LBoxPlotData.PercValue(Pecentile13[12]),
                                                   LBoxPlotData.PercValue(Pecentile13[13]),
                           0.0,LWidth,MaxValue(LBoxPlotData.YValues),aCustomVertAxis);
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.CanExport: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.CanPrint: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.DoPrint;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.PopulatePmpGraph(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulatePmpGraph';
var
  LWidth           : double;
  LIndex           : integer;
  LDate            : TDate;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
  LBoxChart        : TBoxPlotChart;
  LOutputTimeStep  : TOutputTimeStep;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    LBoxChart        := TBoxPlotChart.Create;
    try
      LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;

      ViewDialog.Chart.Title.Text.Clear;
      ViewDialog.Chart.Title.Text.Add('Pumping Channel');
      if(LOutputTimeStep = otsMonthly) then
      begin
        ViewDialog.Chart.LeftAxis.Title.Caption      := 'Supply(m³/s)';
        ViewDialog.Chart.BottomAxis.Title.Caption    := 'Date';
        ViewDialog.Chart.BottomAxis.DateTimeFormat   := 'yyyy/mm';
        ViewDialog.Chart.Title.Text.Add(FElementName +' (Monthly)')
      end
      else if(LOutputTimeStep = otsAnnual) then
      begin
        ViewDialog.Chart.LeftAxis.Title.Caption      := 'Supply(Mm³/a)';
        ViewDialog.Chart.BottomAxis.Title.Caption    := 'Date';
        ViewDialog.Chart.BottomAxis.DateTimeFormat   := 'yyyy';
        ViewDialog.Chart.Title.Text.Add(FElementName +' (Annual Actual)');
      end
      else if(LOutputTimeStep = otsMonthlyCumulative) then
      begin
        ViewDialog.Chart.LeftAxis.Title.Caption      := 'Supply(Mm³/a)';
        ViewDialog.Chart.BottomAxis.Title.Caption    := 'Date';
        ViewDialog.Chart.BottomAxis.DateTimeFormat   := 'yyyy';
        ViewDialog.Chart.Title.Text.Add(FElementName +' (Monthly Cumulative)');
      end
      else if(LOutputTimeStep = otsAnnualCumulative) then
      begin
        ViewDialog.Chart.LeftAxis.Title.Caption      := 'Supply(Mm³/a)';
        ViewDialog.Chart.BottomAxis.Title.Caption    := 'Date';
        ViewDialog.Chart.BottomAxis.DateTimeFormat   := 'yyyy';
        ViewDialog.Chart.Title.Text.Add(FElementName +' (Annual Cumulative)');
      end;

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LDate               := StrToDateDef(LLineData[1],0.0);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);
        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      LBoxChart.SetTheChart(ViewDialog.Chart);
      LWidth := CalculateBoxWidth(LBoxPlotDataList.Count);

      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LBoxChart.AddPoint(LBoxPlotData.XValueDate,LBoxPlotData.PercValue(Pecentile13[01]),
                                                   LBoxPlotData.PercValue(Pecentile13[02]),
                                                   LBoxPlotData.PercValue(Pecentile13[03]),
                                                   LBoxPlotData.PercValue(Pecentile13[04]),
                                                   LBoxPlotData.PercValue(Pecentile13[05]),
                                                   LBoxPlotData.PercValue(Pecentile13[06]),
                                                   LBoxPlotData.PercValue(Pecentile13[07]),
                                                   LBoxPlotData.PercValue(Pecentile13[08]),
                                                   LBoxPlotData.PercValue(Pecentile13[09]),
                                                   LBoxPlotData.PercValue(Pecentile13[10]),
                                                   LBoxPlotData.PercValue(Pecentile13[11]),
                                                   LBoxPlotData.PercValue(Pecentile13[12]),
                                                   LBoxPlotData.PercValue(Pecentile13[13]),
                           0.0,LWidth,MaxValue(LBoxPlotData.YValues),aCustomVertAxis);
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.PopulateSysGraph(AData: TStrings;AColumnName: string);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.PopulateSysGraph';
var
  LYear,LMonth      : integer;
  LWidth            : double;
  LIndex            : integer;
  LValue            : double;
  LDate             : TDate;
  LFileName         : string;
  LMonthNo          : string;
  LLineData         : TStringList;
  LBoxPlotData      : TBoxPlotData;
  LBoxPlotDataList  : TBoxPlotDataList;
  LBoxChart         : TBoxPlotChart;
  LCurtailmentLevelSeries : TLineSeries;
  LAllocationDef    : TAllocationDefinition;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    LBoxChart        := TBoxPlotChart.Create;
    try
      ViewDialog.Chart.Title.Text.Clear;
      if(NetworkElementType = votReviewTotalSystemStorage) then
      begin
        LFileName  := FElementName;
        LFileName  := Copy(LFileName,1,Length(LFileName)-4);
        LMonthNo     := IntToStr(TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.DecisionMonth);
        if(LMonthNo = '0') then
          LMonthNo := 'Average';
        ViewDialog.Chart.Title.Text.Add('Total System Storage');
        ViewDialog.Chart.Title.Text.Add(LFileName + ' - Decision Month ('+ LMonthNo +')');
        ViewDialog.Chart.LeftAxis.Title.Caption    := 'Storage Mm³';
      end
      else if(NetworkElementType = votReviewSubSystemCurtailment) then
      begin
        LMonthNo     := IntToStr(TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.DecisionMonth);
        if(LMonthNo = '0') then
          LMonthNo := 'Average';
        ViewDialog.Chart.Title.Text.Add('Sub-System Curtailment');
        ViewDialog.Chart.Title.Text.Add(FElementName + ' - Decision Month ('+ LMonthNo +')');
        ViewDialog.Chart.LeftAxis.Title.Caption    := 'Curtailment Level';
      end;

      ViewDialog.Chart.BottomAxis.Title.Caption := 'Years';
      ViewDialog.Chart.BottomAxis.DateTimeFormat := 'yyyy';

      LMonth  := FAppModules.StudyArea.CalendarStartMonth;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LYear := StrToInt(LLineData[1]);
        LDate := EncodeDate(LYear,LMonth,01);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);
        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      LBoxChart.SetTheChart(ViewDialog.Chart);
      LWidth := CalculateBoxWidth(LBoxPlotDataList.Count);

      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LBoxChart.AddPoint(LBoxPlotData.XValueDate,LBoxPlotData.PercValue(Pecentile13[01]),
                                                   LBoxPlotData.PercValue(Pecentile13[02]),
                                                   LBoxPlotData.PercValue(Pecentile13[03]),
                                                   LBoxPlotData.PercValue(Pecentile13[04]),
                                                   LBoxPlotData.PercValue(Pecentile13[05]),
                                                   LBoxPlotData.PercValue(Pecentile13[06]),
                                                   LBoxPlotData.PercValue(Pecentile13[07]),
                                                   LBoxPlotData.PercValue(Pecentile13[08]),
                                                   LBoxPlotData.PercValue(Pecentile13[09]),
                                                   LBoxPlotData.PercValue(Pecentile13[10]),
                                                   LBoxPlotData.PercValue(Pecentile13[11]),
                                                   LBoxPlotData.PercValue(Pecentile13[12]),
                                                   LBoxPlotData.PercValue(Pecentile13[13]),
                           0.0,LWidth,MaxValue(LBoxPlotData.YValues),aCustomVertAxis);
      end;

      if(NetworkElementType in [votReviewSubSystemCurtailment,votTotalSystemCurtailment]) and (LBoxPlotDataList.Count > 0)then
      begin
        LCurtailmentLevelSeries := TLineSeries.Create(ViewDialog.Chart);
        LCurtailmentLevelSeries.ParentChart := ViewDialog.Chart;
        LCurtailmentLevelSeries.SeriesColor := clRed;
        LValue        := 0;
        if(NetworkElementType = votTotalSystemCurtailment) then
        begin
          for LIndex := 0 to TPlanningModelDataObject(FAppModules.Model.ModelData).CastAllocationDefinitionsList.AllocationDefinitionCount-1 do
          begin
            LAllocationDef := TPlanningModelDataObject(FAppModules.Model.ModelData).CastAllocationDefinitionsList.CastAllocationDefinitionByIndex(LIndex);
            LValue := Max(LAllocationDef.NrOfAllocationLevels,LValue);
          end;
        end
        else
        begin
          LAllocationDef := nil;
          if(FTreeNodeIndex >= 0) then
            LAllocationDef := TPlanningModelDataObject(FAppModules.Model.ModelData).CastAllocationDefinitionsList.CastAllocationDefinitionByIndex(FTreeNodeIndex);
          if(LAllocationDef = nil) then
            LAllocationDef := TPlanningModelDataObject(FAppModules.Model.ModelData).CastAllocationDefinitionsList.CastAllocationDefinitionByIndex(0);
          if(LAllocationDef <> nil) then
          LValue        := LAllocationDef.NrOfAllocationLevels;
        end;
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[0];
        LCurtailmentLevelSeries.AddXY(LBoxPlotData.XValueDate,LValue);
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LBoxPlotDataList.Count-1];
        LCurtailmentLevelSeries.AddXY(LBoxPlotData.XValueDate,LValue);
      end;

    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGraphValidator.CalculateBoxWidth(ABoxCount: integer): integer;
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.CalculateBoxWidth';
begin
  Result := 100;
  try
    if(ABoxCount > 00) then
      Result := 100;
    if(ABoxCount > 12) then
      Result := 075;
    if(ABoxCount > 20) then
      Result := 050;
    if(ABoxCount > 30) then
      Result := 025;
    if(ABoxCount > 50) then
      Result := 015;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGraphValidator.OnShowDamLevelSeriesClick(Sender: TObject);
const OPNAME = 'TOutputWRPMBoxPlotGraphValidator.OnShowDamLevelSeriesClick';
begin
  try
    if(ViewDialog.ShowDamLevelChkBox.Checked) then
    begin
     PopulateDamLevelSeries;
     ViewDialog.DamLevelSeries.Visible := True;
    end
    else
     ViewDialog.DamLevelSeries.Visible := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

