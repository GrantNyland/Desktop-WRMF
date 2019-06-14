
unit UOutputDemandChannelsGraphSummaryValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCLTee.Series,
  VCLTee.TeEngine,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UIFRFeatures,
  UYieldContextValidationType,
  ULongtermSupplyData,
  UOutputDemandChannelsGraphSummaryDialog;

type

  TComplianceArray = array of double;

  TOutputDemandChannelsGraphSummaryValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData     : TOutputDataType;
    FPrevViewData        : TOutputDataType;
    FLoadCase            : integer;
    FSequence            : integer;
    FMonth               : integer;
    FUnits               : TOutputUnits;
    FValueType           : TOutputValueType;
    FTimeStep            : TOutputTimeStep;
    FHighLight           : WordBool;
    FDisplayMonth        : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnViewDataTypeChange(Sender: TObject);
    procedure DoClickChannels(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure PopulateGraph(ALongtermGraph : TLongtermGraph;AChannelNumber : integer);
    procedure DeleteGraph(ALongtermGraph : TLongtermGraph;AChannelNumber : integer);
    procedure GetSelectionData;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    function GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TOutputDemandChannelsGraphSummaryDialog;overload;
  end;

implementation

uses
  Windows,
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  VCL.Forms,
  UConstants,
  UOutputData,
  UDataSetType,
  UOutputGridValidator,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, URunConfigurationData, VCL.Grids;

{ TOutputDemandChannelsGraphSummaryValidator }

procedure TOutputDemandChannelsGraphSummaryValidator.CreateMemberObjects;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputDemandChannelsGraphSummaryDialog.Create(FPanelOwner,FAppModules);

    ViewDialog.lsvDemandChannels.OnClick := DoClickChannels;
    ViewDialog.ViewDataType.OnSelect := OnViewDataTypeChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.DestroyMemberObjects;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.CreateMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.Initialise: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.Initialise';
var
  LIndex : integer;
  LRunConfig : IRunConfigurationData;
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
    LRunConfig := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;
    if LRunConfig <> nil then
    begin
      for LIndex := 1 to LRunConfig.NrOfActiveLoadCases do
      begin
        ViewDialog.ViewDataType.Items.AddObject('Target Draft '+IntToStr(LIndex)+' ('+
        FloatToStr(LRunConfig.TargetYieldByIndex[LIndex])+')',TObject(LIndex));
      end;
    end;

    if (ViewDialog.ViewDataType.ItemIndex < 0) then
      ViewDialog.ViewDataType.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.DoClickChannels(Sender: TObject);
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.DoClickChannels';
var
  LChannelNumber : integer;
  LOldCursor : TCursor;
  LLongtermGraph : TLongtermGraph;
begin
  try
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LChannelNumber := integer(ViewDialog.lsvDemandChannels.Items.Objects[ViewDialog.lsvDemandChannels.ItemIndex]);
    if ViewDialog.lsvDemandChannels.Checked[ViewDialog.lsvDemandChannels.ItemIndex] then
    begin
      Identifier := LChannelNumber;
      LLongtermGraph := ViewDialog.AddGraph(LChannelNumber);
      if LLongtermGraph <> nil then
        PopulateGraph(LLongtermGraph,LChannelNumber);
    end
    else
    begin
      Identifier := LChannelNumber;
      LLongtermGraph := ViewDialog.LongtermGraphsByChannelNo[LChannelNumber];
      DeleteGraph(LLongtermGraph,LChannelNumber);
    end;
    ViewDialog.Resize;
    Screen.Cursor := LOldCursor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDemandChannelsGraphSummaryValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.OnViewDataTypeChange';
begin
  try
    if (ViewDialog.ViewDataType.ItemIndex < 0) then
      ViewDialog.ViewDataType.ItemIndex := 0;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputDemandChannelsGraphSummaryValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputDemandChannelSummaryGraph');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.SaveState: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.ViewDialog : TOutputDemandChannelsGraphSummaryDialog;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputDemandChannelsGraphSummaryDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.GetNextSignificantRecord';
begin
  Result := 0;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.GetPreviousSignificantRecord';
begin
  Result := 0;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.ClearDataViewer;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ViewDialog.ClearGraphs;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.PopulateDataViewer;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.PopulateGraph(ALongtermGraph : TLongtermGraph;AChannelNumber : integer);
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.PopulateGraph';
var
  LLongtermSupply : TLongtermSupply;
  LIndex : integer;
  LChannel : IGeneralFlowChannel;
  LLongtermSeries : TLineSeries;
  LLineSeries : TLineSeries;
  LYValue,
  LXValue : double;
  LMinYValue,
  LMaxFactor,
  LCumYValue,
  LMaxYValue  : double;
  LFactor : double;
  LBarSeries : TBarSeries;
  LColor : TColor;
  LSelectedValues : TStringList;
begin
  try
    if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData <> nil then
    begin
      LLongtermSupply := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.GetLongtermSupplyByChannel(AChannelNumber,ViewDialog.ViewDataType.ItemIndex);
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if (LLongtermSupply = nil) then
      begin
        TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.CreateLongtermSupply(AChannelNumber);
        LLongtermSupply := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.GetLongtermSupplyByChannel(AChannelNumber,ViewDialog.ViewDataType.ItemIndex);
      end;
      if (LLongtermSupply <> nil) and (LChannel <> nil) and (ALongtermGraph <> nil) then
      begin
        ALongtermGraph.Title.Text.Add( LChannel.ChannelName);
        ALongtermGraph.Legend.Visible := False;
        ALongtermGraph.View3D                      := False;
        ALongtermGraph.LeftAxis.AxisValuesFormat   := '###,###,##0.00';
        ALongtermGraph.BottomAxis.AxisValuesFormat := '##0';
        ALongtermGraph.LeftAxis.TitleSize        := 1;
        LLongtermSeries := TLineSeries.Create(ALongtermGraph);
        LLongtermSeries.ParentChart := ALongtermGraph;
        LMaxFactor := 0;
        LFactor := 0;
        for LIndex := Low(LLongtermSupply.Supply) to High(LLongtermSupply.Supply) do
        begin
          LXValue := (LIndex)/(High(LLongtermSupply.Supply)+1)*100;
          LLongtermSeries.AddXY(LXValue,LLongtermSupply.Supply[LIndex]);
          LFactor := LLongtermSeries.YValues.MaxValue;
          if LFactor>LMaxFactor then
            LMaxFactor := LFactor;
        end;
        if (FUnits = ouPercentage) then
          ALongtermGraph.LeftAxis.SetMinMax(0, 105)
        else
        begin
          LFactor := LFactor * 0.05;
          ALongtermGraph.LeftAxis.SetMinMax(0, LMaxFactor + lFactor);
        end;
        ALongtermGraph.BottomAxis.SetMinMax(0,100);
        ALongtermGraph.Repaint;
        LCumYValue := 0;
        if LLongtermSupply.FStackedValues.Count>0 then
          LCumYValue := StrTofloat(LLongtermSupply.FStackedValues[LLongtermSupply.FStackedValues.Count-1]);
        if LMaxFactor< LCumYValue then
          LMaxFactor := LCumYValue;
        ALongtermGraph.LeftAxis.SetMinMax(0, LMaxFactor + lFactor);
        LMinYValue := ALongtermGraph.LeftAxis.Minimum;
        LMaxYValue := ALongtermGraph.LeftAxis.Maximum;
        LCumYValue := 0;
        if High(LLongtermSupply.DemandSplit)>0 then
        begin
          for LIndex := High(TYieldModelDataObject(FAppModules.Model.ModelData).
            OutputData.CastLongtermSupplyData.ComplianceAssurance) downto
            Low(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.
            CastLongtermSupplyData.ComplianceAssurance) do
          begin
            LLineSeries := TLineSeries.Create(ALongtermGraph);
            LLineSeries.ParentChart := ALongtermGraph;
            LLineSeries.Active := True;
            if (LIndex<TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FSplits.Count-1) and (LIndex>0) then
              LLineSeries.Title := '1_in_' + TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FSplits[LIndex];
            ALongtermGraph.ConfigureRILineSeriers(LLineSeries);
            ALongtermGraph.AddLineSeries(IntToStr(ALongtermGraph.RILineSeriesListCount+1), LLineSeries);
            ALongtermGraph.AddSeries(LLineSeries);
            LXValue := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.ComplianceAssurance[LIndex];
            LLineSeries.AddXY(LXValue, LMinYValue);
            LLineSeries.AddXY(LXValue, LMaxYValue);
            LBarSeries := TBarSeries.Create(ALongtermGraph);
            LBarSeries.ParentChart   := ALongtermGraph;
            ALongtermGraph.AddSeries(LBarSeries);
            LBarSeries.XValues.Order    := loNone;
            LBarSeries.Marks.Visible    := False;
            LBarSeries.Marks.Clip       := True;
            LBarSeries.ShowInLegend     := False;
            LYValue := LLongtermSupply.DemandSplit[LIndex];
            LCumYValue := LCumYValue + LLongtermSupply.DemandSplit[LIndex];
            LColor := clBlue;
            if not TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.SetSupplyCriteriaStatus(LLongtermSupply,LCumYValue,Trunc(LXValue)) then
              LColor := clRed;
            LBarSeries.SeriesColor      := LColor;
            LBarSeries.MultiBar         := mbStacked;
            LBarSeries.BarWidthPercent  := 3;
            LBarSeries.AddXY(LXValue, LYValue);
          end;
          ALongtermGraph.Repaint;
        end;
        LSelectedValues := TStringList.Create;
        try
          for LIndex := 0 to ViewDialog.lsvDemandChannels.Count-1 do
          begin
            if ViewDialog.lsvDemandChannels.Checked[LIndex] then
              LSelectedValues.Add(IntToStr(integer(ViewDialog.lsvDemandChannels.Items.Objects[LIndex])));
          end;
          FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Model',FAppModules.StudyArea.ModelCode);
          FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','StudyAreaName',FAppModules.StudyArea.StudyAreaCode);
          FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SubArea',FAppModules.StudyArea.SubAreaCode);
          FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Scenario',FAppModules.StudyArea.ScenarioCode);
          FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SelectedValues',LSelectedValues.CommaText);
        finally
          LSelectedValues.Free;
        end;
      end
      else
      begin
        ShowMessage('This channel is not in the output');
        if ViewDialog.lsvDemandChannels.ItemIndex >= 0 then
          ViewDialog.lsvDemandChannels.Checked[ViewDialog.lsvDemandChannels.ItemIndex] := False;
        ViewDialog.DeleteGraph(ALongtermGraph);
        //PopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.DeleteGraph(ALongtermGraph : TLongtermGraph;AChannelNumber : integer);
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.DeleteGraph';
var
  LIndex : integer;
  LSelectedValues : TStringList;
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSelectedData : string;
begin
  try
    LSelectedValues := TStringList.Create;
    try
      LModel := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Model','');
      LStudyAreaName := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','StudyAreaName','');
      LSubArea := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SubArea','');
      LScenario := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Scenario','');
      if (FAppModules.StudyArea.ModelCode = LModel) and (FAppModules.StudyArea.StudyAreaCode = LStudyAreaName) and
         (FAppModules.StudyArea.SubAreaCode= LSubArea) and
         (FAppModules.StudyArea.ScenarioCode = LScenario) then
        LSelectedData := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SelectedValues','');
       LSelectedValues.CommaText := LSelectedData;
      LIndex := LSelectedValues.IndexOf
               (IntToStr(integer(ViewDialog.lsvDemandChannels.Items.
                      Objects[ViewDialog.lsvDemandChannels.ItemIndex])));
      if LIndex >-1 then
      begin
        LSelectedValues.Delete(LIndex);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Model',FAppModules.StudyArea.ModelCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','StudyAreaName',FAppModules.StudyArea.StudyAreaCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SubArea',FAppModules.StudyArea.SubAreaCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Scenario',FAppModules.StudyArea.ScenarioCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SelectedValues',LSelectedValues.CommaText);
        PopulateDataViewer;
      end;
    finally
      LSelectedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.RePopulateDataViewer;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.RePopulateDataViewer';
var
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
  LIndex : integer;
  LChannelNumber : integer;
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSelectedData : string;
  LSelectedValues : TStringList;
  LLongtermGraph : TLongtermGraph;
begin
  try
    LSelectedValues := TStringList.Create;
    try
      GetSelectionData;

      LModel := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Model','');
      LStudyAreaName := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','StudyAreaName','');
      LSubArea := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SubArea','');
      LScenario := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Scenario','');
      if (FAppModules.StudyArea.ModelCode = LModel) and (FAppModules.StudyArea.StudyAreaCode = LStudyAreaName) and
       (FAppModules.StudyArea.SubAreaCode= LSubArea) and
       (FAppModules.StudyArea.ScenarioCode = LScenario) then
        LSelectedData := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SelectedValues','');
        
      LSelectedValues.CommaText := LSelectedData;
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      ViewDialog.lsvDemandChannels.Clear;
      for LIndex := 0 to LChannelList.ChannelCount-1 do
      begin
        LChannel := LChannelList.ChannelByIndex[LIndex];
        if LChannel <> nil then
        begin
          if LChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,
                                      ctDemandChannel,ctIrrigationBlockInflowChannel,
                                      ctIFRChannel] then
            ViewDialog.lsvDemandChannels.AddItem(LChannel.ChannelName,TObject(LChannel.ChannelNumber));
          if LSelectedValues.IndexOf(IntToStr(LChannel.ChannelNumber))>=0 then
            ViewDialog.lsvDemandChannels.Checked[ViewDialog.lsvDemandChannels.Items.IndexOf(LChannel.ChannelName)] := True;
        end;
      end;
      for LIndex := 0 to LSelectedValues.Count-1 do
      begin
       if Trim(LSelectedValues[LIndex]) <> '' then
        begin
          LChannelNumber := StrToInt(LSelectedValues[LIndex]);
          Identifier := LChannelNumber;
          LLongtermGraph := ViewDialog.AddGraph(LChannelNumber);
          PopulateGraph(LLongtermGraph,LChannelNumber);
        end;
      end;
      ViewDialog.Resize;
    finally
      LSelectedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.DoPrint;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.DoPrint';

begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.CanExport: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryValidator.CanPrint: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryValidator.GetSelectionData;
const OPNAME = 'TOutputDemandChannelsGraphSummaryValidator.GetSelectionData';
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
     FUnits        := ouPerSecond;
     FValueType    := LDataSelection.ValueType;
     FTimeStep     := LDataSelection.TimeStep;
     FHighLight    := LDataSelection.Highlight;
     FDisplayMonth := LDataSelection.DisplayMonth;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

