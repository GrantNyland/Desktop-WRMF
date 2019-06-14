//
//
//  UNIT      : Contains TTimeSeriesComparitorDataManager Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2003/03/28
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorDataManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  UAbstractObject,
  UViewDataItem,

  UTimeSeriesComparitorLinkClasses,
  UTimeSeriesComparitorDisplayPanel,
  UTimeSeriesComparitorSubAreaDataManager,
  UTimeSeriesComparitorScenarioDataManager,
  UTimeSeriesComparitorPopulatingAgent,

  UTimeSeriesComparitorViewManager,
  UTimeSeriesComparitorViewList,

  UTimeSeriesComparitorChartManager,
  UTimeSeriesComparitorChartList,

  UTimeSeriesComparitorSeriesManager,
  UTimeSeriesComparitorSeriesList;
type

  TTimeSeriesComparitorDataManager = class(TAbstractAppObject)
  protected
    FChanged: boolean;
    FDisplayPanel    : TTimeSeriesComparitorDisplayPanel;

    FSubareaDataManager: TTimeSeriesComparitorSubareaDataManager;
    FScenarioViewData : TTimeSeriesComparitorScenarioViewData;
    FPopulatingAgent:TTimeSeriesComparitorPopulatingAgent;

    FSeriesDataManager: TTimeSeriesComparitorSeriesManager;
    FCurrentSeries: TTimeSeriesComparitorSeries;

    FChartDataManager: TTimeSeriesComparitorChartManager;
    FCurrentChart: TTimeSeriesComparitorChart;

    FViewDataManager: TTimeSeriesComparitorViewManager;
    FCurrentView: TTimeSeriesComparitorView;

    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;


    function UpdateScenarioView(ACurrentViewName: string): boolean;

    function HideCurrentViewCharts: boolean;
    function ShowCurrentViewCharts: boolean;

  public
    constructor Create(AAppModules: TAppModules; ADisplayPanel : TTimeSeriesComparitorDisplayPanel); reintroduce;

    function Initialise: boolean;override;
    function StudyHasChanged: boolean; override;
    function SubAreaHasChanged : boolean;
    function RestoreScenarioData: boolean;
    function SaveCurrentView: boolean;

    function GetCurrentViewName: string;
    function GetCurrentChartName: string;
    function GetCurrentScenarioViewName: string;

    function GetSubAreViewNames(AViewNamesList: TStringList): boolean;
    function GetSubAreaChartNames(AChartNamesList: TStringList): boolean;
    function GetViewChartNames(AViewName: string; AChartNamesList: TStringList): boolean;


    function AddView(ACurrentViewName: string): TTimeSeriesComparitorView;
    function SelectView(AView: TTimeSeriesComparitorView): boolean;
    function RenameView(AOldViewName,ACurrentViewName: string): boolean;
    function DeleteView(ACurrentViewName: string): boolean;

    function AddChart(ACurrentChartName: string): TTimeSeriesComparitorChart;
    function SelectChart(AChart: TTimeSeriesComparitorChart): boolean;
    function RenameChart(AOldChartName,ACurrentChartName: string): boolean;
    function DeleteChart(ACurrentChartName: string): boolean;
    function RemoveCurrentChartFromView: boolean;
    function AddCurrentChartToView: boolean;
    function RemoveTemporaryCurrentChart: boolean;

    function AddSeries(ADataObject: TViewDataTreeNodeData): TTimeSeriesComparitorSeries;
    function SelectSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
    function RemoveCurrentSeriesFromChart: boolean;
    function AddCurrentSeriesToChart: boolean;
    function RemoveTemporaryCurrentSeries: boolean;

    function ViewNameExists(AViewName: string): boolean;
    function ChartNameExists(AChartName: string): boolean;
    function ChartComponentNameExists(AChartName: string): boolean;
    function RestoreCurrentSeries: boolean;
    function HighlightCurrentSeries: boolean;
    function PopulateEmptySeriesForCurrentScenario: boolean;

    property CurrentSeriesData : TTimeSeriesComparitorSeries read FCurrentSeries;
    property CurrentChartData  : TTimeSeriesComparitorChart  read FCurrentChart;
    property CurrentViewData   : TTimeSeriesComparitorView   read FCurrentView;
    property ChartDataManager  : TTimeSeriesComparitorChartManager read FChartDataManager;
    property PopulatingAgent   : TTimeSeriesComparitorPopulatingAgent read FPopulatingAgent write FPopulatingAgent;
    property Changed           : boolean read FChanged write FChanged;
  end;

implementation

uses
  VCLTee.Series,
  VCL.Forms,
  SysUtils,
  UErrorHandlingOperations, UTimeSeriesComparitorSelectorPanel, VCL.StdCtrls;


{ TTimeSeriesComparitorDataManager }

constructor TTimeSeriesComparitorDataManager.Create(AAppModules: TAppModules; ADisplayPanel : TTimeSeriesComparitorDisplayPanel);
const OPNAME = 'TTimeSeriesComparitorDataManager.CreateMemberObjects';
begin
  inherited Create(AAppModules);
  try
    FDisplayPanel := ADisplayPanel;
 except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorDataManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorDataManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCurrentSeries           := nil;
    FCurrentChart            := nil;
    FCurrentView             := nil;
    FChanged                 := False;
    FSubareaDataManager      := TTimeSeriesComparitorSubareaDataManager.Create(FAppModules);
    FScenarioViewData        := TTimeSeriesComparitorScenarioViewData.Create(FAppModules);
    FPopulatingAgent         := TTimeSeriesComparitorPopulatingAgent.Create(FAppModules);;

    FSeriesDataManager       := TTimeSeriesComparitorSeriesManager.Create(FAppModules);
    FChartDataManager        := TTimeSeriesComparitorChartManager.Create(FAppModules);
    FViewDataManager         := TTimeSeriesComparitorViewManager.Create(FAppModules);
 except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorDataManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorDataManager.DestroyMemberObjects';
begin
  try
    FCurrentSeries  := nil;
    FCurrentChart   := nil;
    FCurrentView    := nil;
    FreeAndNil(FScenarioViewData);
    FreeAndNil(FSubareaDataManager);
    FreeAndNil(FPopulatingAgent);

    FreeAndNil(FSeriesDataManager);
    FreeAndNil(FChartDataManager);
    FreeAndNil(FViewDataManager);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.Initialise';
begin
  Result := inherited Initialise;
  try
    //FScenarioViewData.Initialise;
    //FSubareaDataManager.Initialise;
    FSeriesDataManager.Initialise;
    FChartDataManager.Initialise;
    FViewDataManager.Initialise;
    //FPopulatingAgent.Initialise;
    FCurrentSeries := nil;
    FCurrentChart := nil;
    FCurrentView := nil;
    FChanged := False;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTimeSeriesComparitorDataManager.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FScenarioViewData.StudyHasChanged;
    FSubareaDataManager.StudyHasChanged;
    FPopulatingAgent.StudyHasChanged;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.GetCurrentViewName: string;
const OPNAME = 'TTimeSeriesComparitorDataManager.GetCurrentViewName';
begin
  Result := '';
  try
    if Assigned(FCurrentView) then
    Result := FCurrentView.ViewName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.GetCurrentChartName: string;
const OPNAME = 'TTimeSeriesComparitorDataManager.GetCurrentChartName';
begin
  Result := '';
  try
    if Assigned(FCurrentChart) then
      Result := FCurrentChart.ChartName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.GetSubAreViewNames(AViewNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.GetSubAreViewNames';
begin
  Result := False;
  try
    AViewNamesList.Clear;
    Result := FViewDataManager.ViewNames(AViewNamesList);
    if (AViewNamesList.Count = 0) then
       AViewNamesList.Add('')
    else
    if (AViewNamesList.Count > 0) and (AViewNamesList.Strings[0] <> '') then
       AViewNamesList.Insert(0,'');
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.GetViewChartNames(AViewName: string;AChartNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.GetViewChartNames';
var
  LViewData: TTimeSeriesComparitorView;
begin
  Result := False;
  try
    AChartNamesList.Clear;
    LViewData := FViewDataManager.ViewDataByName[AViewName];
    if Assigned(LViewData) then
    begin
      Result := LViewData.ChartList.ChartNames(AChartNamesList);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.GetSubAreaChartNames(AChartNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.GetSubAreaChartNames';
begin
  Result := False;
  try
    Result := FChartDataManager.ChartNames(AChartNamesList);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.AddChart(ACurrentChartName: string): TTimeSeriesComparitorChart;
const OPNAME = 'TTimeSeriesComparitorDataManager.AddChart';
begin
  Result := nil;
  try
    if Assigned(FCurrentView) then
    begin
      Result := FChartDataManager.CreateChartData(ACurrentChartName);
      FChanged := FChanged or Assigned(Result);
      //if Assigned(Result) then
      //  FCurrentSeries := nil;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.HideCurrentViewCharts: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.HideCurrentViewCharts';
var
  LIndex: integer;
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    if Assigned(FCurrentView) then
    begin
      for LIndex := 0 to FCurrentView.ChartList.ChartCount -1 do
      begin
        LChartData := FCurrentView.ChartList.ChartByIndex[LIndex];
        if Assigned(LChartData) then
          FDisplayPanel.ChartPanel.RemoveChartFromPanel(LChartData.Chart);
      end;
      FCurrentChart := nil;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.ShowCurrentViewCharts: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.ShowCurrentViewCharts';
var
  LIndex: integer;
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    if Assigned(FCurrentView) then
    begin
      FCurrentChart := nil;
      for LIndex := 0 to FCurrentView.ChartList.ChartCount -1 do
      begin
        LChartData := FCurrentView.ChartList.ChartByIndex[LIndex];
        if Assigned(LChartData) then
        begin
          if (FCurrentView.SavedChartName = LChartData.ChartName) then
          begin
            FCurrentChart := LChartData;
            FDisplayPanel.ChartPanel.ShowChartInPanel(LChartData.Chart, TRUE);
          end
          else
            FDisplayPanel.ChartPanel.ShowChartInPanel(LChartData.Chart, FALSE);
        end;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.SelectChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.SelectChart';
begin
  Result := False;
  try
    if Assigned(FCurrentView) then
    begin
      if Assigned(FCurrentChart) then
        FDisplayPanel.ChartPanel.ShowChartInPanel(FCurrentChart.Chart, FALSE);
      FCurrentChart := FChartDataManager.SelectChartData(AChart.ChartName);
      Result := Assigned(FCurrentChart);
      Result := Result and FCurrentView.SelectChart(FCurrentChart);
      Result := Result and FDisplayPanel.ChartPanel.ShowChartInPanel(FCurrentChart.Chart, TRUE);
      if Result then
      begin
        FCurrentSeries := FCurrentChart.CurrentSeries;
      end;
      //Result := Result and FDisplayPanel.ChartPanel.ShowChartInPanel(FCurrentChart.Chart);
      //Result := Result and FCurrentView.AddChart(FCurrentChart);
    end;
    FChanged := FChanged or Result;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.RenameChart(AOldChartName,ACurrentChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RenameChart';
begin
  Result := False;
  try
    Result := FChartDataManager.RenameChartData(AOldChartName,ACurrentChartName);
    if Assigned(FCurrentView) and (FCurrentView.SavedChartName = AOldChartName) then
      FCurrentView.SavedChartName := ACurrentChartName;

    FChanged := FChanged or Result;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.DeleteChart(ACurrentChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.DeleteChart';
var
  LCount: integer;
  LSeries : TTimeSeriesComparitorSeries;
begin
  Result := False;
  try
    if Assigned(FCurrentView) then
    begin
      FCurrentChart := FChartDataManager.SelectChartData(ACurrentChartName);
      if Assigned(FCurrentView) then
      begin
        for LCount := 0 to FCurrentChart.SeriesList.SeriessCount - 1 do
        begin
          LSeries := FCurrentChart.SeriesList.SeriesByIndex[LCount];
          FDisplayPanel.SelectorsPanel.DeleteSeries(LSeries);
          FSeriesDataManager.DeleteSeriesData(LSeries);
        end;
        Result := FDisplayPanel.ChartPanel.RemoveChartFromPanel(FCurrentChart.Chart);
        Result := Result and FCurrentView.RemoveChartDataFromView(FCurrentChart);
        Result := Result and  FViewDataManager.RemoveChartFromViews(FCurrentChart);
        Result := Result and FChartDataManager.DeleteChartData(FCurrentChart);
        FCurrentChart := FCurrentView.CurrentChart;
        if Assigned(FCurrentChart) then
          FCurrentSeries := FCurrentChart.CurrentSeries
        else
          FCurrentSeries := nil;
      end;
      FChanged := FChanged or Result;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.AddView(ACurrentViewName: string): TTimeSeriesComparitorView;
const OPNAME = 'TTimeSeriesComparitorDataManager.AddView';
begin
  Result := nil;
  try
    //if Assigned(FCurrentView) then
    //  HideCurrentViewCharts;

    Result := FViewDataManager.CreateViewData(ACurrentViewName);
    FChanged := FChanged or Assigned(Result);

    //if Assigned(Result) then
    //begin
    //   FScenarioViewData.SenarioViewName := '';
    //   FCurrentChart := nil;
    //   FCurrentView  := nil;
    //end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.SelectView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.SelectView';
var
  LCount : integer;
  LChart : TTimeSeriesComparitorChart;
begin
  Result := False;                                           
  try

    if Assigned(FCurrentView) then
      HideCurrentViewCharts;

    FCurrentView := FViewDataManager.SelectViewData(AView.ViewName);
    Result := Assigned(FCurrentView);
    if Result then
    begin
      for LCount := 0 to FCurrentView.ChartList.ChartCount -1 do
      begin
         LChart := FCurrentView.ChartList.ChartByIndex[LCount];
         if (LChart = FCurrentView.CurrentChart) then
           FDisplayPanel.ChartPanel.ShowChartInPanel(LChart.Chart, TRUE)
         else
           FDisplayPanel.ChartPanel.ShowChartInPanel(LChart.Chart, FALSE);
      end;

      FScenarioViewData.SenarioViewName := AView.ViewName;
      FCurrentChart := FCurrentView.CurrentChart;

      FDisplayPanel.ChartPanel.HeaderCaption.Caption := FCurrentView.HeaderCaption;
      FDisplayPanel.ChartPanel.FooterCaption.Caption := FCurrentView.FooterCaption;
    end;
    FChanged := FChanged or Result;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.RenameView(AOldViewName,ACurrentViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RenameView';
begin
  Result := False;
  try
    Result := FViewDataManager.RenameViewData(AOldViewName,ACurrentViewName);
    if Result then
    begin
      FChartDataManager.RenameViewData(AOldViewName,ACurrentViewName);
    end;

    if Result and Assigned(FCurrentView) and (FCurrentView.ViewName = ACurrentViewName) then
       FScenarioViewData.SenarioViewName := ACurrentViewName;

    FChanged := FChanged or Result;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.DeleteView(ACurrentViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.DeleteView';
begin
  Result := False;
  try
    if Assigned(FCurrentView) and (FCurrentView.ViewName = ACurrentViewName) then
    begin
      HideCurrentViewCharts;
      Result := FViewDataManager.DeleteViewData(FCurrentView);
      FScenarioViewData.SenarioViewName := '';
      FCurrentChart  := nil;
      FCurrentView   := nil;
      FCurrentSeries := nil;
    end
    else
    begin
      Result := FViewDataManager.DeleteViewData(ACurrentViewName);
    end;
    if Result then
    begin
      FChartDataManager.DeleteViewData(ACurrentViewName);
    end;
    FDisplayPanel.ChartPanel.HeaderCaption.Caption := '';
    FDisplayPanel.ChartPanel.FooterCaption.Caption := '';
    FChanged := FChanged or Result;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.GetCurrentScenarioViewName: string;
const OPNAME = 'TTimeSeriesComparitorDataManager.GetCurrentScenarioViewName';
begin
  Result := '';
  try
    Result := FScenarioViewData.SenarioViewName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.UpdateScenarioView(ACurrentViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.UpdateScenarioView';
begin
  Result := False;
  try
    FScenarioViewData.ModelCode       := FAppModules.StudyArea.ModelCode;
    FScenarioViewData.StudyAreaCode   := FAppModules.StudyArea.StudyAreaCode;
    FScenarioViewData.SubAreaCode     := FAppModules.StudyArea.SubAreaCode;
    FScenarioViewData.ScenarioCode    := FAppModules.StudyArea.ScenarioCode;
    FScenarioViewData.SenarioViewName := ACurrentViewName;
    Result := True;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.SubAreaHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.SubAreaHasChanged';
begin
  Result := False;
  try
    Result := FSubareaDataManager.SubAreaHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.ChartNameExists(AChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.ChartNameExists';
begin
  Result := False;
  try
    Result := Assigned(FChartDataManager.ChartDataByName[AChartName]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.ChartComponentNameExists(AChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.ChartComponentNameExists';
var
  lIndex  : integer;
begin
  Result := False;
  try
    AChartName := FChartDataManager.Strip(AChartName);
    lIndex := 0;
    while ((NOT Result) AND (lIndex < FChartDataManager.ChartDataList.ChartCount)) do
    begin
      if (FChartDataManager.ChartDataByIndex[lIndex].Chart.Name = AChartName) then
        Result := TRUE
      else
        lIndex := lIndex + 1;  
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.ViewNameExists(AViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.ViewNameExists';
begin
  Result := False;
  try
    Result := Assigned(FViewDataManager.ViewDataByName[AViewName]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.AddCurrentChartToView: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.AddCurrentChartToView';
begin
  Result := False;
  try
    if Assigned(FCurrentView) and Assigned(FCurrentChart) then
    begin
      FCurrentView.AddChartDataToView(FCurrentChart);
      Result := True;
    end;
    FChanged := FChanged or Result;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.RemoveCurrentChartFromView: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RemoveCurrentChartFromView';
begin
  Result := False;
  try
    if Assigned(FCurrentView) and Assigned(FCurrentChart) then
    begin
      FDisplayPanel.ChartPanel.RemoveChartFromPanel(FCurrentChart.Chart);
      FCurrentView.RemoveChartDataFromView(FCurrentChart);
      FCurrentChart := FCurrentView.CurrentChart;
      if Assigned(FCurrentChart) then
        FCurrentSeries := FCurrentChart.CurrentSeries
      else
        FCurrentSeries := nil;
      Result := True;
    end;
    FChanged := FChanged or Result;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.AddCurrentSeriesToChart: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.AddCurrentSeriesToChart';
begin
  Result := False;
  try
    if Assigned(FCurrentChart) and Assigned(FCurrentSeries) then
    begin
      FCurrentChart.AddSeriesToChart(FCurrentSeries);
      Result := True;
    end;
    FChanged := FChanged or Result;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.RemoveCurrentSeriesFromChart: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RemoveCurrentSeriesFromChart';
begin
  Result := False;
  try
    if Assigned(FCurrentChart) and Assigned(FCurrentSeries) then
    begin
      FDisplayPanel.SelectorsPanel.DeleteSeries(FCurrentSeries);
      FCurrentChart.RemoveSeriesFromChart(FCurrentSeries);
      FSeriesDataManager.DeleteSeriesData(FCurrentSeries);
      FCurrentSeries := FCurrentChart.CurrentSeries;
      Result := True;
      FChanged := FChanged or Result;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.RemoveTemporaryCurrentSeries: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RemoveTemporaryCurrentSeries';
begin
  Result := False;
  try
    if Assigned(FCurrentChart) and Assigned(FCurrentSeries) and (not FCurrentSeries.AddedToChart) then
    begin
      FDisplayPanel.SelectorsPanel.DeleteSeries(FCurrentSeries);
      FCurrentChart.RemoveSeriesFromChart(FCurrentSeries);
      FSeriesDataManager.DeleteSeriesData(FCurrentSeries);
      FCurrentSeries := FCurrentChart.CurrentSeries;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.RemoveTemporaryCurrentChart: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RemoveTemporaryCurrentChart';
begin
  Result := False;
  try
    if Assigned(FCurrentView) and Assigned(FCurrentChart) and
       (not FCurrentChart.ChartAddedToView(FCurrentView.ViewName)) then
    begin
      FDisplayPanel.ChartPanel.RemoveChartFromPanel(FCurrentChart.Chart);
      FCurrentView.RemoveChartDataFromView(FCurrentChart);
      FCurrentChart := FCurrentView.CurrentChart;
      if Assigned(FCurrentChart) then
        FCurrentSeries := FCurrentChart.CurrentSeries
      else
        FCurrentSeries := nil;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.AddSeries(ADataObject: TViewDataTreeNodeData): TTimeSeriesComparitorSeries;
const OPNAME = 'TTimeSeriesComparitorDataManager.AddSeries';
begin
  Result := nil;
  try
    if Assigned(FCurrentChart) and Assigned(ADataObject) and Assigned(ADataObject.ViewDataNode) then
    begin
      //Select series that is in the chart
      Result := FCurrentChart.SeriesList.SeriesByName(FAppModules.StudyArea.ModelCode,
                                                       FAppModules.StudyArea.StudyAreaCode,
                                                       FAppModules.StudyArea.SubAreaCode,
                                                       FAppModules.StudyArea.ScenarioCode,
                                                       FCurrentChart.ChartName,
                                                       ADataObject.ViewDataNode.OverrideCaption,
                                                       ADataObject.ViewDataNode.ViewID,
                                                       ADataObject.ViewDataNode.ParentID,
                                                       ADataObject.ViewDataNode.TopParentID);
(*
      if not Assigned(Result) then
      begin
        //Select series that is in the series list
        FSeriesDataManager.SeriesDataList.SeriesByName(FAppModules.StudyArea.ModelCode,
                                                       FAppModules.StudyArea.StudyAreaCode,
                                                       FAppModules.StudyArea.SubAreaCode,
                                                       FAppModules.StudyArea.ScenarioCode,
                                                       FCurrentChart.ChartName,
                                                       ADataObject.ViewDataNode.OverrideCaption,
                                                       ADataObject.ViewDataNode.ViewID,
                                                       ADataObject.ViewDataNode.ParentID,
                                                       ADataObject.ViewDataNode.TopParentID);
*)
      if Assigned(Result) then
      begin
        FCurrentChart.AddSeries(Result);
      end
      else
      begin
        //Create a new series.
        Result := FSeriesDataManager.CreateSeriesData(FAppModules.StudyArea.ModelCode,
                                            FAppModules.StudyArea.StudyAreaCode,
                                            FAppModules.StudyArea.SubAreaCode,
                                            FAppModules.StudyArea.ScenarioCode,
                                            FCurrentChart.ChartName,
                                            ADataObject.ViewDataNode.OverrideCaption,
                                            ADataObject.ViewDataNode.ViewID,
                                            ADataObject.ViewDataNode.ParentID,
                                            ADataObject.ViewDataNode.TopParentID);
        if Assigned(Result) then
        begin
          if FPopulatingAgent.PopulateCurrentSeries(0,Result,ADataObject,FCurrentView) then
          begin
            Result.CommaTextCaption:= FPopulatingAgent.SeriesCommaTextCaption(ADataObject);
            FCurrentChart.AddSeries(Result);
            Result.Color := Result.LineSeries.SeriesColor;
          end;
        end
      end;
    end;
    FChanged := FChanged or Assigned(Result);
//    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorDataManager.SaveCurrentView: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.SaveCurrentView';
begin
  Result := False;
  try
    Result := FPopulatingAgent.SaveSubAreaDataToDatabase(FDisplayPanel,FSubareaDataManager,FScenarioViewData,
                                                         FSeriesDataManager,FCurrentSeries,
                                                         FChartDataManager,FCurrentChart,
                                                         FViewDataManager,FCurrentView);
    if Result then
      FChanged := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.RestoreScenarioData: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RestoreScenarioData';
begin
  Result := False;
  try
    Result := FPopulatingAgent.PopulateSubAreaDataFromDatabase(FDisplayPanel,FSubareaDataManager,FScenarioViewData,
                                                               FSeriesDataManager,FCurrentSeries,
                                                               FChartDataManager,FCurrentChart,
                                                               FViewDataManager,FCurrentView);
    if Assigned(FCurrentView) then
    begin
      FDisplayPanel.ChartPanel.HeaderCaption.Caption := FCurrentView.HeaderCaption;
      FDisplayPanel.ChartPanel.FooterCaption.Caption := FCurrentView.FooterCaption;
      FDisplayPanel.SelectorsPanel.SelectCurrentView(FCurrentView);
      SelectView(FCurrentView);
    end;
    FChanged := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.SelectSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.SelectSeries';
var
  LSeries: TTimeSeriesComparitorSeries;
begin
  Result := False;
  try
    if Assigned(FCurrentChart) then
    begin
      LSeries := FSeriesDataManager.SeriesDataList.SeriesByCommaTextCaption(FCurrentChart.ChartName,ASeries.CommaTextCaption);
      if Assigned(LSeries) then
      begin
        if FCurrentChart.SelectSeries(LSeries, TRUE) then
        FCurrentSeries := LSeries;
        Result := True;
      end;
    end;
    FChanged := FChanged or Result;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.RestoreCurrentSeries: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.RestoreCurrentSeries';
begin
  Result := False;
  try
    if Assigned(FCurrentSeries) then
    begin
      FCurrentSeries.LineSeries.LinePen.Width := 0;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.HighlightCurrentSeries: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.HighlightCurrentSeries';
begin
  Result := False;
  try
    if Assigned(FCurrentSeries) then
    begin
      FCurrentSeries.LineSeries.LinePen.Width := 2;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorDataManager.PopulateEmptySeriesForCurrentScenario: boolean;
const OPNAME = 'TTimeSeriesComparitorDataManager.PopulateEmptySeriesForCurrentScenario';
begin
  Result := False;
  try
    FPopulatingAgent.PopulateEmptySeriesForCurrentScenario(FSeriesDataManager,FCurrentView);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

