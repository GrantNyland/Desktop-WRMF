//
//
//  UNIT      : Contains TimeSeriesComparitorViewList Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorViewList;

interface
uses
  Classes,
  vcl.Controls,
  vcl.ComCtrls,
  vcl.StdCtrls,
  Contnrs,
  VCLTee.TeEngine,
  UAbstractObject,
  UTimeSeriesComparitorChartList,
  UTimeSeriesComparitorChartPanel;

type
  TTimeSeriesComparitorView = Class(TAbstractAppObject)
  protected
    FChartList: TimeSeriesComparitorChartList;
    FModelCode: String;
    FStudyAreaCode: String;
    FSubAreaCode: String;
    FViewName: string;
    FSavedChartName: string;
    FDateCreated  : TDateTime;
    FHeaderCaption,
    FFooterCaption: string;
    FChanged: boolean;
    FNormalised : boolean;
    FYAxisPosition : TVertAxis;
    FCurrentChart: TTimeSeriesComparitorChart;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetViewName(AViewName : string);
    procedure SetDateCreated(ADateCreated : TDateTime);
    procedure SetHeaderCaption(AHeaderCaption : string);
    procedure SetFooterCaption(AFooterCaption : string);
    procedure SetCurrentChart(AChart: TTimeSeriesComparitorChart);
    procedure SetNormalized(const AValue: boolean);
    procedure SetYAxisPosition(const AValue: TVertAxis);
  public
    function AddChart(AChart: TTimeSeriesComparitorChart): boolean;
    function SelectChart(AChart: TTimeSeriesComparitorChart): boolean;
    function AddChartDataToView(AChart: TTimeSeriesComparitorChart): boolean;
    function RemoveChartDataFromView(AChart: TTimeSeriesComparitorChart): boolean;

    property ModelCode: string read FModelCode write FModelCode;
    property StudyAreaCode: string read FStudyAreaCode write FStudyAreaCode;
    property SubAreaCode: string read FSubAreaCode write FSubAreaCode;
    property ViewName: string read FViewName write SetViewName;
    property SavedChartName: string read FSavedChartName write FSavedChartName;
    property DateCreated: TDateTime read FDateCreated write SetDateCreated;
    property HeaderCaption: string read FHeaderCaption write SetHeaderCaption;
    property FooterCaption: string read FFooterCaption write SetFooterCaption;
    property Changed: boolean read FChanged write FChanged;
    property ChartList: TimeSeriesComparitorChartList read FChartList;
    property CurrentChart: TTimeSeriesComparitorChart read FCurrentChart write SetCurrentChart;
    property Normalised : boolean read FNormalised write SetNormalized;
    property YAxisPosition : TVertAxis read FYAxisPosition write SetYAxisPosition;
  end;

  TimeSeriesComparitorViewList = class(TAbstractAppObject)
  protected
    FViewList: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetViewByIndex(AViewIndex: integer): TTimeSeriesComparitorView;
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(AOwn: boolean);
  public
    function Initialise: boolean; override;
    function AddView(AView: TTimeSeriesComparitorView): boolean;
    function RemoveView(AView: TTimeSeriesComparitorView): boolean;
    function ViewsCount: integer;
    function ViewIndex(AView: TTimeSeriesComparitorView): integer;
    function ViewExist(AViewName: string): boolean;
    function ViewByName(AViewName: String): TTimeSeriesComparitorView;
    function ViewNames(ANamesList: TStringList): boolean;
    property ViewByIndex[AViewIndex: integer]: TTimeSeriesComparitorView read GetViewByIndex;
    property OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
  end;

implementation
uses
  SysUtils,
  System.Types,
  VCLTee.Series,
  UErrorHandlingOperations;


{ TTimeSeriesComparitorView }

procedure TTimeSeriesComparitorView.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorView.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCurrentChart     := nil;
    FYAxisPosition    := aLeftAxis;
    FChartList        := TimeSeriesComparitorChartList.Create(FAppModules);
    FModelCode        := '';
    FStudyAreaCode    := '';
    FSubAreaCode      := '';
    FFooterCaption    := '';
    FViewName         := '';
    FDateCreated      := Now;
    FHeaderCaption    := '';
    FFooterCaption    := '';
    FNormalised       := False;
    FChanged          := False;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorView.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorView.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FCurrentChart     := nil;
    FreeAndNil(FChartList);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorView.SetHeaderCaption(AHeaderCaption: string);
const OPNAME = 'TTimeSeriesComparitorView.SetHeaderCaption';
begin
  try
    FHeaderCaption := AHeaderCaption;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorView.SetDateCreated(ADateCreated: TDateTime);
const OPNAME = 'TTimeSeriesComparitorView.SetDateCreated';
begin
  try
    FDateCreated := ADateCreated;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorView.SetViewName(AViewName: string);
const OPNAME = 'TTimeSeriesComparitorView.SetViewName';
begin
  try
    FViewName := AViewName;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TTimeSeriesComparitorView.SetFooterCaption(AFooterCaption: string);
const OPNAME = 'TTimeSeriesComparitorView.SetFooterCaption';
begin
  try
    FFooterCaption := AFooterCaption;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorView.AddChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorView.AddChart';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      if not FChartList.ChartExist(AChart.ChartName) then
        FChartList.AddChart(AChart);
      AChart.Normalised := FNormalised;
      FChanged := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorView.SelectChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorView.SelectChart';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      if not FChartList.ChartExist(AChart.ChartName) then
        AddChart(AChart);
      FCurrentChart := AChart;
      FSavedChartName := AChart.ChartName;
      FChanged := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorView.AddChartDataToView(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorView.AddChartDataToView';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      if not FChartList.ChartExist(AChart.ChartName) then
        FChartList.AddChart(AChart);
      AChart.AddChartToView(Self.FViewName);
      FChanged := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorView.RemoveChartDataFromView(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorView.RemoveChartDataFromView';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      if FChartList.ChartExist(AChart.ChartName) then
        FChartList.RemoveChart(AChart);
      AChart.RemoveChartFromView(Self.FViewName);
      if (FChartList.ChartCount > 0) then
        SetCurrentChart(FChartList.ChartByIndex[0])
      else
      begin
        FCurrentChart := nil;
        FSavedChartName := '';
      end;
      FChanged := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorView.SetCurrentChart(AChart: TTimeSeriesComparitorChart);
const OPNAME = 'TTimeSeriesComparitorView.SetCurrentChart';
begin
  try
    FCurrentChart := AChart;
    if Assigned(FCurrentChart) then
      FSavedChartName := FCurrentChart.ChartName
    else
      FSavedChartName := '';
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorView.SetNormalized(const AValue: boolean);
const OPNAME = 'TTimeSeriesComparitorView.SetNormalized';
var
  LIndex : integer;
begin
  try
    FNormalised := AValue;
    for LIndex := 0 to FChartList.ChartCount - 1 do
      FChartList.ChartByIndex[LIndex].Normalised := AValue;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TimeSeriesComparitorViewList }

procedure TimeSeriesComparitorViewList.CreateMemberObjects;
const OPNAME = 'TimeSeriesComparitorViewList.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FViewList := TObjectList.Create(False);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TimeSeriesComparitorViewList.DestroyMemberObjects;
const OPNAME = 'TimeSeriesComparitorViewList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FViewList);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.Initialise: boolean;
const OPNAME = 'TimeSeriesComparitorViewList.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewList.Clear;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.GetViewByIndex(AViewIndex: integer): TTimeSeriesComparitorView;
const OPNAME = 'TimeSeriesComparitorViewList.GetViewByIndex';
begin
  Result := nil;
  try
    if (AViewIndex >= 0) and (AViewIndex < FViewList.Count) then
      Result := TTimeSeriesComparitorView(FViewList.Items[AViewIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.AddView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TimeSeriesComparitorViewList.AddView';
begin
  Result := False;
  try
    if Assigned(AView) and (not ViewExist(AView.ViewName)) then
    begin
      FViewList.Add(AView);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.RemoveView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TimeSeriesComparitorViewList.RemoveView';
begin
  Result := False;
  try
    if Assigned(AView) and ViewExist(AView.ViewName)then
    begin
      FViewList.Remove(AView);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.ViewsCount: integer;
const OPNAME = 'TimeSeriesComparitorViewList.ViewsCount';
begin
  Result := 0;
  try
    Result := FViewList.Count;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.ViewIndex(AView: TTimeSeriesComparitorView): integer;
const OPNAME = 'TimeSeriesComparitorViewList.ViewIndex';
var
  LIndex: integer;
begin
  Result := -1;
  try
    for LIndex := 0 to FViewList.Count -1 do
    begin
      if(AView = GetViewByIndex(LIndex)) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.ViewByName(AViewName: String): TTimeSeriesComparitorView;
const OPNAME = 'TimeSeriesComparitorViewList.ViewByName';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FViewList.Count - 1 do
    begin
      if Assigned(ViewByIndex[LIndex]) and (ViewByIndex[LIndex].ViewName = AViewName) then
      begin
        Result := ViewByIndex[LIndex];
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.ViewNames(ANamesList: TStringList): boolean;
const OPNAME = 'TimeSeriesComparitorViewList.ViewNames';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(ANamesList) then
    begin
      ANamesList.Clear;
      for LIndex := 0 to FViewList.Count - 1 do
      begin
        if Assigned(ViewByIndex[LIndex]) then
          ANamesList.Add(ViewByIndex[LIndex].ViewName);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.ViewExist(AViewName: string): boolean;
const OPNAME = 'TimeSeriesComparitorViewList.ViewExist';
var
  LIndex: integer;
  LView: TTimeSeriesComparitorView;
begin
  Result := False;
  try
    if (AViewName <> '') then
    begin
      for LIndex := 0 to FViewList.Count -1 do
      begin
        LView := GetViewByIndex(LIndex);
        if(LView.ViewName  = AViewName) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorViewList.GetOwnsObjects: boolean;
const OPNAME = 'TimeSeriesComparitorViewList.GetOwnsObjects';
begin
  Result := False;
  try
    Result := FViewList.OwnsObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorView.SetYAxisPosition(const AValue: TVertAxis);
const OPNAME = 'TTimeSeriesComparitorView.SetYAxisPosition';
begin
  try
    if Assigned(FCurrentChart) then
      if (FYAxisPosition <> AValue) then
        FYAxisPosition := AValue;
    if Assigned(FCurrentChart.CurrentSeries) and
     (not(FCurrentChart.CurrentSeries.AddedToChart)) then
      FCurrentChart.CurrentSeries.LineSeries.VertAxis := FYAxisPosition;
    FCurrentChart.ShowAxisLabels;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TimeSeriesComparitorViewList.SetOwnsObjects(AOwn: boolean);
const OPNAME = 'TimeSeriesComparitorViewList.SetOwnsObjects';
begin
  try
    FViewList.OwnsObjects := AOwn;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
