//
//
//  UNIT      : Contains TimeSeriesComparitorChartList Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorChartList;

interface
uses
  Classes,
  vcl.Graphics,
  vcl.Dialogs,
  vcl.Controls,
  vcl.ComCtrls,
  vcl.StdCtrls,
  Contnrs,
  VCLTee.TeEngine,
  VCLTee.Series,
  VCLTee.Chart,
  UAbstractObject,
  UAbstractComponent,
  UTimeSeriesComparitorSeriesList;

type
  TTimeSeriesComparitorChart = Class(TAbstractAppObject)
  protected
    FSeriesList      : TTimeSeriesComparitorSeriesList;
    FBottomAxisMin,
    FBottomAxisMax,
    FDateCreated     : TDateTime;
    FModelCode,
    FStudyAreaCode,
    FSubAreaCode,
    FChartName,
    FSavedSeriesName,
    FHeaderCaption,
    FFooterCaption   : string;
    FAddedToViewList : TStringList;
    FChanged,
    FShowLineSeries,
    FNormalised      : boolean;
    FChart           : TAbstractChart;
    FLeftAxisMin,
    FLeftAxisMax,
    FRightAxisMin,
    FRightAxisMax    : double;
    FCurrentSeries   : TTimeSeriesComparitorSeries;
    FFSLLineSeries,
    FDSLLineSeries,
    FBOTLineSeries : TLineSeries;
    
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetChartName(AChartName : string);
    procedure SetSavedSeriesName(ASavedSeriesName : string);
    procedure SetDateCreated(ADateCreated : TDateTime);
    procedure SetHeaderCaption(AHeaderCaption : string);
    procedure SetFooterCaption(AFooterCaption : string);
    procedure SetNormalised(const AValue: boolean);
    procedure ReAdjustYAxis(ANormalised : boolean);
    procedure SeriesOnMouseEnter(Sender : TObject);
    procedure SeriesOnMouseLeave(Sender : TObject);
    procedure DoOnGetAxisLabel(Sender : TChartAxis; aSeries : TChartSeries;
                               aValueIndex : integer; var aLabelText : String);
    procedure OnGetLineSeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
    procedure ClearReservoirLineSeries;
    function GetMinimumSeriesXValue : double;
    function GetMaximumSeriesXValue : double;
    function IsSeletedSeriesReservoirElevation : boolean;
    procedure PrepareReservoirLineSeries;
  public
    procedure SelectAndShowSeriesBySeriesName(AReservoirNodeNumber : integer);
    function AddSeries(ASeries: TTimeSeriesComparitorSeries):boolean;
    function SelectSeries(ASeries   : TTimeSeriesComparitorSeries;
                         ASelected : Boolean) : boolean;
    function RemoveSeriesFromChart(ASeries: TTimeSeriesComparitorSeries): boolean;
    function AddSeriesToChart(ASeries: TTimeSeriesComparitorSeries): boolean;
    function ChartAddedToView(AViewName : string): boolean;
    function AddChartToView(AViewName : string): boolean;
    function RemoveChartFromView(AViewName : string): boolean;
    procedure GetChartLegendProperties(var ALegendAlignment : TLegendAlignment; var ALegendVisible : boolean);
    procedure SetChartLegend(ALegendAlignment: TLegendAlignment; ALegendVisible: boolean;
                             ASeriesName : string; ASeries : TTimeSeriesComparitorSeries);
    procedure ShowAxisLabels;
    procedure SetChartAxis;
    
    property ModelCode       : string                          read FModelCode       write FModelCode;
    property StudyAreaCode   : string                          read FStudyAreaCode   write FStudyAreaCode;
    property SubAreaCode     : string                          read FSubAreaCode     write FSubAreaCode;
    property ChartName       : string                          read FChartName       write SetChartName;
    property SavedSeriesName : string                          read FSavedSeriesName write SetSavedSeriesName;
    property DateCreated     : TDateTime                       read FDateCreated     write SetDateCreated;
    property HeaderCaption   : string                          read FHeaderCaption   write SetHeaderCaption;
    property FooterCaption   : string                          read FFooterCaption   write SetFooterCaption;
    property Changed         : boolean                         read FChanged         write FChanged;
    property SeriesList      : TTimeSeriesComparitorSeriesList read FSeriesList;
    property ChartInViewList : TStringList                     read FAddedToViewList;
    property Chart           : TAbstractChart                  read FChart           write FChart;
    property CurrentSeries   : TTimeSeriesComparitorSeries     read FCurrentSeries;
    property Normalised      : boolean                         read FNormalised      write SetNormalised;
    property LeftAxisMin     : double                          read FLeftAxisMin     write FLeftAxisMin;
    property LeftAxisMax     : double                          read FLeftAxisMax     write FLeftAxisMax;
    property BottomAxisMin   : TDateTime                       read FBottomAxisMin   write FBottomAxisMin;
    property BottomAxisMax   : TDateTime                       read FBottomAxisMax   write FBottomAxisMax;
    property RightAxisMin    : double                          read FRightAxisMin    write FRightAxisMin;
    property RightAxisMax    : double                          read FRightAxisMax    write FRightAxisMax;
    property ShowLineSeries  : boolean                         read FShowLineSeries  write FShowLineSeries;
  end;

  TimeSeriesComparitorChartList = class(TAbstractAppObject)
  protected
    FChartList: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetChartByIndex(AChartIndex: integer): TTimeSeriesComparitorChart;
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(AOwn: boolean);
  public
    function Initialise: boolean; override;
    function AddChart(AChart: TTimeSeriesComparitorChart): boolean;
    function RemoveChart(AChart: TTimeSeriesComparitorChart): boolean;
    function ChartCount: integer;
    function ChartIndex(AChart: TTimeSeriesComparitorChart): integer;
    function ChartExist(AChartName: string): boolean;
    function ChartByName(AChartName: String): TTimeSeriesComparitorChart;
    function ChartNames(ANamesList: TStringList): boolean;
    property ChartByIndex[AChartIndex: integer]: TTimeSeriesComparitorChart read GetChartByIndex;
    property OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
  end;

implementation
uses
  SysUtils,
  System.Types,
  Math,
  UErrorHandlingOperations,
  VoaimsCom_TLB, DateUtils;

{ TTimeSeriesComparitorChart }

procedure TTimeSeriesComparitorChart.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChart.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCurrentSeries := nil;
    FModelCode       := '';
    FStudyAreaCode   := '';
    FSubAreaCode     := '';
    FFooterCaption   := '';
    FChartName       := '';
    FSavedSeriesName := '';
    FDateCreated     := Now;
    FHeaderCaption   := '';
    FFooterCaption   := '';
    FNormalised      := False;
    FAddedToViewList := TStringList.Create;
    FAddedToViewList.Sorted := True;
    FAddedToViewList.Duplicates := dupIgnore;
    FChanged         := False;
    FSeriesList      := TTimeSeriesComparitorSeriesList.Create(FAppModules);
    FChart           := TAbstractChart.Create(nil,FAppModules);
    FChart.View3D    := False;
    //FChart.Hint      := '';
    FChart.ShowHint  := False;
    FChart.Legend.Alignment   := laLeft;    
    FChart.Legend.LegendStyle := lsSeries;
    FChart.Legend.ShadowSize  := 0;
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
    begin
      FChart.Legend.ColorWidth  := 35;
    end
    else
      FChart.Legend.ColorWidth  := 3;
    FChart.Legend.Visible        := True;

    FChart.OnGetAxisLabel := DoOnGetAxisLabel;
    FChart.BottomAxis.AxisValuesFormat := '0000000000';

    FChart.BottomAxis.Automatic := True;
    FChart.BottomAxis.LabelsAngle := 90;
    FChart.BottomAxis.TickLength  := 6;
    FChart.BottomAxis.MinorTickCount  := 4;
    FChart.BottomAxis.Title.Caption := '';
    FChart.LeftAxis.AxisValuesFormat := '### ###0.##';
    //FChart.View3DWalls          := False;
    //FChart.BackWall.Brush.Color := clWhite;
    //FChart.BackWall.Color       := clWhite;
    //FChart.BackColor            := clWhite;

    FFSLLineSeries                  := TLineSeries.Create(nil);
    FFSLLineSeries.SeriesColor      := clFuchsia;
    FFSLLineSeries.LinePen.Style    := psSolid;
    FFSLLineSeries.LinePen.Width    := 1;
    FFSLLineSeries.XValues.DateTime := True;
    FFSLLineSeries.XValues.Order    := loNone;
    FFSLLineSeries.Pointer.Visible  := False;
    FFSLLineSeries.Marks.Visible    := True;
    FFSLLineSeries.Marks.Clip       := True;
    FFSLLineSeries.OnGetMarkText    := OnGetLineSeriesMarkText;
    FFSLLineSeries.Name             := 'FSL';

    FDSLLineSeries                  := TLineSeries.Create(nil);
    FDSLLineSeries.SeriesColor      := clBlue;
    FDSLLineSeries.LinePen.Style    := psSolid;
    FDSLLineSeries.LinePen.Width    := 1;
    FDSLLineSeries.XValues.DateTime := True;
    FDSLLineSeries.XValues.Order    := loNone;
    FDSLLineSeries.Pointer.Visible  := False;
    FDSLLineSeries.Marks.Visible    := True;
    FDSLLineSeries.Marks.Clip       := True;
    FDSLLineSeries.OnGetMarkText    := OnGetLineSeriesMarkText;
    FDSLLineSeries.Name             := 'DSL';

    FBOTLineSeries                  := TLineSeries.Create(nil);
    FBOTLineSeries.SeriesColor      := clGreen;
    FBOTLineSeries.LinePen.Style    := psSolid;
    FBOTLineSeries.LinePen.Width    := 1;
    FBOTLineSeries.XValues.DateTime := True;
    FBOTLineSeries.XValues.Order    := loNone;
    FBOTLineSeries.Pointer.Visible  := False;
    FBOTLineSeries.Marks.Visible    := True;
    FBOTLineSeries.Marks.Clip       := True;
    FBOTLineSeries.OnGetMarkText    := OnGetLineSeriesMarkText;
    FBOTLineSeries.Name             := 'BOT';
    
    ClearReservoirLineSeries;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChart.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FCurrentSeries := nil;
    FChart.RemoveAllSeries;
    FreeAndNil(FChart);
    FreeAndNil(FSeriesList);
    FreeAndNil(FAddedToViewList);
    if FFSLLineSeries <> nil then
    begin
      FFSLLineSeries.Active      := False;
      FFSLLineSeries.ParentChart := nil;
    end;
    if Assigned(FBOTLineSeries) then
    begin
      FBOTLineSeries.Active      := False;
      FBOTLineSeries.ParentChart := nil;
    end;
    if Assigned(FDSLLineSeries) then
    begin
      FDSLLineSeries.Active      := False;
      FDSLLineSeries.ParentChart := nil;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChart.ChartAddedToView(AViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.ChartAddedToView';
begin
  Result := False;
  try
    Result := (FAddedToViewList.IndexOf(AViewName) >= 0);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetHeaderCaption(AHeaderCaption: string);
const OPNAME = 'TTimeSeriesComparitorChart.SetHeaderCaption';
begin
  try
    FHeaderCaption := AHeaderCaption;
    FChart.Title.Text.Text := AHeaderCaption;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetDateCreated(ADateCreated: TDateTime);
const OPNAME = 'TTimeSeriesComparitorChart.SetDateCreated';
begin
  try
    FDateCreated := ADateCreated;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetChartName(AChartName: string);
const OPNAME = 'TTimeSeriesComparitorChart.SetChartName';
begin
  try
    FChartName := AChartName;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetFooterCaption(AFooterCaption: string);
const OPNAME = 'TTimeSeriesComparitorChart.SetFooterCaption';
begin
  try
    FFooterCaption := AFooterCaption;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChart.SelectSeries (ASeries   : TTimeSeriesComparitorSeries;
                                                  ASelected : Boolean): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.SelectSeries';
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      if not SeriesList.SeriesExist(ASeries) then
        AddSeries(ASeries);

      if(FChart.SeriesCount = 0) then
      begin
        FChart.BottomAxis.Minimum := Min(FChart.BottomAxis.Minimum, ASeries.LineSeries.MinXValue);
        FChart.BottomAxis.Maximum := Max(FChart.BottomAxis.Maximum, ASeries.LineSeries.MaxXValue);
      end;

      FChart.AddSeries(ASeries.LineSeries);

      if(SeriesList.SeriessCount = 1) then
      begin
        if FNormalised then
          ASeries.Normalised := FNormalised;
      end
      else
        if FNormalised then
          ASeries.Normalised := True;

      ReAdjustYAxis(FNormalised);

      FChart.BottomAxis.Minimum := Min(FChart.BottomAxis.Minimum, ASeries.LineSeries.MinXValue);
      FChart.BottomAxis.Maximum := Max(FChart.BottomAxis.Maximum, ASeries.LineSeries.MaxXValue);

      if ASelected then
      begin
        FCurrentSeries := ASeries;
        FSavedSeriesName := ASeries.CommaTextCaption;
        ASeries.LineSeries.LinePen.Width := 2;
      end
      else
        ASeries.LineSeries.LinePen.Width := 1;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChart.RemoveSeriesFromChart(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.RemoveSeriesFromChart';
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      if SeriesList.SeriesExist(ASeries) then
        FSeriesList.RemoveSeries(ASeries);
      FChart.RemoveSeries(ASeries.LineSeries);
      FCurrentSeries := nil;
      FSavedSeriesName := '';
      Result := True;
      ReAdjustYAxis(FNormalised);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChart.AddSeriesToChart(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.AddSeriesToChart';
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      ASeries.AddedToChart := True;
      ReAdjustYAxis(FNormalised);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChart.AddChartToView(AViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.AddChartToView';
begin
  Result := False;
  try
    if (AViewName <> '') then
    begin
      FAddedToViewList.Add(AViewName);
      ReAdjustYAxis(FNormalised);
      FChanged := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChart.RemoveChartFromView(AViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.RemoveChartFromView';
begin
  Result := False;
  try
    if (AViewName <> '') then
    begin
      if(FAddedToViewList.IndexOf(AViewName) >= 0) then
      begin
        FAddedToViewList.Delete(FAddedToViewList.IndexOf(AViewName));
        FChanged := True;
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetSavedSeriesName(ASavedSeriesName: string);
const OPNAME = 'TTimeSeriesComparitorChart.SetSavedSeriesName';
begin
  try
    FSavedSeriesName := ASavedSeriesName;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetNormalised(const AValue: boolean);
const OPNAME = 'TTimeSeriesComparitorChart.SetNormalised';
var
  LIndex: integer;
begin
  try
    FChart.LeftAxis.Automatic := not AValue;
    FChart.RightAxis.Automatic := not AValue;

    for LIndex := 0 to SeriesList.SeriessCount -1 do
      SeriesList.SeriesByIndex[LIndex].Normalised := AValue;

    FNormalised := AValue;
    ReAdjustYAxis(FNormalised);
    ShowAxisLabels;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.GetChartLegendProperties(var ALegendAlignment: TLegendAlignment; var ALegendVisible: boolean);
const OPNAME = 'TTimeSeriesComparitorChart.GetChartLegendProperties';
begin
  try
    ALegendAlignment := FChart.Legend.Alignment;
    ALegendVisible   := FChart.Legend.Visible;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetChartLegend(ALegendAlignment: TLegendAlignment; ALegendVisible: boolean;
                                                    ASeriesName : string; ASeries : TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorChart.SetChartLegend';
var
  LIndex : integer;
  LSeriesName : string;
  function FormatSeriesName(ASeriesName : string) : string;
  const OPNAME = 'UTimeSeriesComparitorChartList.FormatSeriesName';
  var
    LTmpStr : string;
    LPos : integer;
  begin
    Result := '';
    LTmpStr := ASeriesName;
    LPos    := 21;
    Delete(LTmpStr, LPos, (Length(LTmpStr) - LPos) + 1);
    Result  := LTmpStr;
  end;
begin
  try
    FChart.Legend.Alignment := ALegendAlignment;
    FChart.Legend.Visible   := ALegendVisible;
    for LIndex := 0 to FChart.SeriesCount - 1 do
    begin
      LSeriesName := FChart.Series[LIndex].Title;
      if UpperCase(FormatSeriesName(ASeries.SeriesName)) = UpperCase(LSeriesName) then
      begin
        FChart.Series[LIndex].Title := ASeriesName;
        ASeries.SeriesName := ASeriesName;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.ReAdjustYAxis(ANormalised : boolean);
const OPNAME = 'TTimeSeriesComparitorChart.ReAdjustYAxis';
begin
  try
    if not ANormalised then
    begin
      FChart.LeftAxis.AdjustMaxMin;
      FChart.RightAxis.AdjustMaxMin;
      FChart.LeftAxis.Increment := (Abs(FChart.LeftAxis.Maximum - FChart.LeftAxis.Minimum))*(10/100);
      FChart.LeftAxis.Maximum := FChart.LeftAxis.Maximum  + ((FChart.LeftAxis.Maximum * 5) / 100);

      FChart.RightAxis.Increment := (Abs(FChart.RightAxis.Maximum - FChart.RightAxis.Minimum))*(10/100);
      FChart.RightAxis.Maximum := FChart.RightAxis.Maximum  + ((FChart.RightAxis.Maximum * 5) / 100);
    end
    else
    begin
      FChart.LeftAxis.Minimum    := 0.0;
      FChart.LeftAxis.Maximum    := 105.0;
      FChart.LeftAxis.Increment  := 10.0;
      FChart.RightAxis.Minimum   := 0.0;
      FChart.RightAxis.Maximum   := 105.0;
      FChart.RightAxis.Increment := 10.0;
      FChart.LeftAxis.AdjustMaxMin;
      FChart.RightAxis.AdjustMaxMin;
    end;
    ShowAxisLabels;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetChartAxis;
const OPNAME = 'TTimeSeriesComparitorChart.SetChartAxis';
begin
  try
    if Assigned(FChart) then
    begin
      FChart.LeftAxis.Automatic := False;
      FChart.LeftAxis.Increment := (Abs(LeftAxisMax - LeftAxisMin))*(10/100);
      FChart.LeftAxis.Maximum := LeftAxisMax;
      FChart.LeftAxis.Minimum := LeftAxisMin;
      FChart.BottomAxis.Automatic := False;
      FChart.BottomAxis.Increment := (Abs(BottomAxisMax - BottomAxisMin))*(10/100);
      FChart.BottomAxis.Maximum := BottomAxisMax;
      FChart.BottomAxis.Minimum := BottomAxisMin;
      FChart.RightAxis.Automatic := False;
      FChart.RightAxis.Increment := (Abs(RightAxisMax - RightAxisMin))*(10/100);
      FChart.RightAxis.Maximum := RightAxisMax;
      FChart.RightAxis.Minimum := RightAxisMin;
      FChart.LeftAxis.AdjustMaxMin;
      FChart.RightAxis.AdjustMaxMin;
      FChart.BottomAxis.AdjustMaxMin;
      FChart.Zoomed := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.ShowAxisLabels;
const OPNAME = 'TTimeSeriesComparitorChart.ShowAxisLabels';
var
  LXIndex : integer;
  LXSeries,
  LYSeries : TTimeSeriesComparitorSeries;
  LPrintLeftAxis,
  LPrintRightAxis : boolean;
  LAxisCaption,
  LUnitString : string;
begin
  try
    LXSeries := nil;
    LAxisCaption := '';
    LPrintLeftAxis  := True;
    LPrintRightAxis := True;
    for LXIndex := 0 to FChart.SeriesCount - 1 do
    begin
      if not Assigned(LXSeries) then
        LXSeries := TTimeSeriesComparitorSeries(FSeriesList.SeriesByIndex[LXIndex])
      else
        Break;
      if Assigned(LXSeries) then
      begin  
        if LXSeries.LineSeries.VertAxis <> VCLTee.TeEngine.TVertAxis.aLeftAxis {aLeftAxis} then
        begin
          LXSeries := nil;
          Continue;
        end;
      end;
    end;
    if Assigned(LXSeries) then
    begin
      for LXIndex := Pred(FChart.SeriesCount) downto 0 do
      begin
        LYSeries := TTimeSeriesComparitorSeries(FSeriesList.SeriesByIndex[LXIndex]);
        if Assigned(LYSeries) then
        begin
          if LYSeries.LineSeries.VertAxis <> VCLTee.TeEngine.TVertAxis.aLeftAxis {aLeftAxis} then
            Continue;

          if UpperCase(LXSeries.Units) <>
             UpperCase(LYSeries.Units) then
          begin
            LPrintLeftAxis := False;
            Break;
          end;
        end;
      end;
    end
    else
      LPrintLeftAxis := False;
    if Assigned(LXSeries) then
    begin
      LAxisCaption := LXSeries.AxisCaption;
      LUnitString  := Trim('(' + LXSeries.Units + ')');
      Delete(LAxisCaption, Pos(LUnitString, LAxisCaption), Length(LUnitString));
    end;
    if LPrintLeftAxis and
      (FChart.SeriesCount > 0) then
    begin
      if not FNormalised then
        FChart.LeftAxis.Title.Caption := LXSeries.AxisCaption
      else
        FChart.LeftAxis.Title.Caption := LAxisCaption;
    end
    else
      FChart.LeftAxis.Title.Caption := '';
    LXSeries := nil;
    for LXIndex := 0 to FChart.SeriesCount - 1 do
    begin
      if not Assigned(LXSeries) then
        LXSeries := TTimeSeriesComparitorSeries(FSeriesList.SeriesByIndex[LXIndex])
      else
        break;
      if Assigned(LXSeries) then
      begin
        if LXSeries.LineSeries.VertAxis <> VCLTee.TeEngine.TVertAxis.aRightAxis then
        begin
          LXSeries := nil;
          Continue;
        end;
      end;
    end;
    if Assigned(LXSeries) then
    begin
      for LXIndex := Pred(FSeriesList.SeriessCount) downto 0 do
      begin
        LYSeries := TTimeSeriesComparitorSeries(FSeriesList.SeriesByIndex[LXIndex]);
        if LYSeries.LineSeries.VertAxis <> VCLTee.TeEngine.TVertAxis.aRightAxis then
          Continue;
        if UpperCase(LXSeries.Units) <>
           UpperCase(LYSeries.Units) then
        begin
          LPrintRightAxis := False;
          Break;
        end;
      end;
    end
    else
      LPrintRightAxis := False;
    if Assigned(LXSeries) then
    begin
      LAxisCaption := LXSeries.AxisCaption;
      LUnitString  := Trim('(' + LXSeries.Units + ')');
      Delete(LAxisCaption, Pos(LUnitString, LAxisCaption), Length(LUnitString));
    end;
    if LPrintRightAxis and
      (FChart.SeriesCount > 0) then
    begin
      if not FNormalised then
        FChart.RightAxis.Title.Caption := LXSeries.AxisCaption
      else
        FChart.RightAxis.Title.Caption := LAxisCaption;
    end
    else
      FChart.RightAxis.Title.Caption := '';

  if (FChart.SeriesCount = 0) then
    FChart.BottomAxis.Title.Caption := ''
  else
    FChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('TSCSheet.X_AxisLabel');

  FChart.RightAxis.TitleSize := 1;
  FChart.LeftAxis.TitleSize := 1;
  FChart.RightAxis.LabelsSize := 45;
  FChart.LeftAxis.LabelsSize := 45;

  FChart.RightAxis.AxisValuesFormat := '###0.00';
  FChart.LeftAxis.AxisValuesFormat  := '###0.00';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SeriesOnMouseEnter(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorChart.SeriesOnMouseEnter';
var
  LHintString : string;
  LXVal,
  LYVal : double;
begin
  try
    if Assigned(FChart) then
    begin
      if (Sender is TTimeSeriesLineSeries) then
      begin
        TTimeSeriesLineSeries(Sender).GetCursorValues(LXVal, LYVal);
        LHintString := TTimeSeriesLineSeries(Sender).SeriesHint + #13#10' Y = ' +
                       Format('%f', [LYVal]) + #13#10' X = ' +
                       Format('%s', [FormatDateTime('dd/mm/yyyy', FloatToDateTime(LXVal))]);
        //FChart.Hint     := LHintString;   come back
        FChart.ShowHint := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SeriesOnMouseLeave(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorChart.SeriesOnMouseLeave';
begin
  try
    if Assigned(FChart) then
    begin
      //FChart.Hint     := ''; come back
      FChart.ShowHint := False;
    end
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.DoOnGetAxisLabel(Sender : TChartAxis; aSeries : TChartSeries;
                                                      aValueIndex : integer; var aLabelText : String);
const OPNAME = 'TTimeSeriesComparitorChart.DoOnGetAxisLabel';
begin
  try
    if (Assigned(FChart)) and (Sender = FChart.BottomAxis) then
    begin
      if Assigned(FAppModules) and Assigned(FAppModules.Model()) and
        (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
      begin
        if Assigned(aSeries) then
          aLabelText := aSeries.XValues.ToString;//(aValueIndex);
      end;
      if(aLabelText <> '') then
      begin
        if(Pos('/',aLabelText) > 0) then
          aLabelText := FormatDateTime('dd/mm/yyyy',StrToDateTime(aLabelText))
        else
          aLabelText := FormatDateTime('dd/mm/yyyy', StrToFloat(aLabelText))
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorChart.IsSeletedSeriesReservoirElevation: boolean;
const OPNAME = 'TTimeSeriesComparitorChart.IsSeletedSeriesReservoirElevation';
var
  LSeriesNames    : TStringList;
  LReservoirValue : string;
begin
  Result := False;
  try
    if(FCurrentSeries <> nil) then
    begin
      LSeriesNames := TStringList.Create;
      try
        LSeriesNames.CommaText := FCurrentSeries.CommaTextCaption;
        if(LSeriesNames.Count >= 3) then
        begin
          LReservoirValue := LSeriesNames[2];
          LReservoirValue := UpperCase(LReservoirValue);
          Result := (LReservoirValue = 'MONTH-END RESERVOIR ELEVATION(M)');
        end;
      finally
        FreeAndNil(LSeriesNames);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TimeSeriesComparitorChartList }

procedure TimeSeriesComparitorChartList.CreateMemberObjects;
const OPNAME = 'TimeSeriesComparitorChartList.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FChartList := TObjectList.Create(False);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TimeSeriesComparitorChartList.DestroyMemberObjects;
const OPNAME = 'TimeSeriesComparitorChartList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChartList);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.Initialise: boolean;
const OPNAME = 'TimeSeriesComparitorChartList.Initialise';
begin
  Result := inherited Initialise;
  try
    FChartList.Clear;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.GetChartByIndex(AChartIndex: integer): TTimeSeriesComparitorChart;
const OPNAME = 'TimeSeriesComparitorChartList.GetChartByIndex';
begin
  Result := nil;
  try
    if (AChartIndex >= 0) and (AChartIndex < FChartList.Count) then
      Result := TTimeSeriesComparitorChart(FChartList.Items[AChartIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.AddChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TimeSeriesComparitorChartList.AddChart';
begin
  Result := False;
  try
    if Assigned(AChart) and (not ChartExist(AChart.ChartName)) then
    begin
      FChartList.Add(AChart);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.RemoveChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TimeSeriesComparitorChartList.RemoveChart';
begin
  Result := False;
  try
    if Assigned(AChart) and ChartExist(AChart.ChartName)then
    begin
      FChartList.Remove(AChart);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.ChartCount: integer;
const OPNAME = 'TimeSeriesComparitorChartList.ChartCount';
begin
  Result := 0;
  try
    Result := FChartList.Count;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.ChartIndex(AChart: TTimeSeriesComparitorChart): integer;
const OPNAME = 'TimeSeriesComparitorChartList.ChartIndex';
var
  LIndex: integer;
begin
  Result := -1;
  try
    for LIndex := 0 to FChartList.Count -1 do
    begin
      if(AChart = GetChartByIndex(LIndex)) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.ChartByName(AChartName: String): TTimeSeriesComparitorChart;
const OPNAME = 'TimeSeriesComparitorChartList.ChartByName';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FChartList.Count - 1 do
    begin
      if Assigned(ChartByIndex[LIndex]) AND (ChartByIndex[LIndex].ChartName = AChartName) then
      begin
        Result := ChartByIndex[LIndex];
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.ChartNames(ANamesList: TStringList): boolean;
const OPNAME = 'TimeSeriesComparitorChartList.ChartNames';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(ANamesList) then
    begin
      ANamesList.Clear;
      for LIndex := 0 to FChartList.Count - 1 do
      begin
        if Assigned(ChartByIndex[LIndex]) then
          ANamesList.Add(ChartByIndex[LIndex].ChartName);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.ChartExist(AChartName: string): boolean;
const OPNAME = 'TimeSeriesComparitorChartList.ChartExist';
var
  LIndex: integer;
  LChart: TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    if (AChartName <> '') then
    begin
      for LIndex := 0 to FChartList.Count -1 do
      begin
        LChart := GetChartByIndex(LIndex);
        if(LChart.ChartName  = AChartName) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorChartList.GetOwnsObjects: boolean;
const OPNAME = 'TimeSeriesComparitorChartList.GetOwnsObjects';
begin
  Result := False;
  try
    Result := FChartList.OwnsObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TimeSeriesComparitorChartList.SetOwnsObjects(AOwn: boolean);
const OPNAME = 'TimeSeriesComparitorChartList.SetOwnsObjects';
begin
  try
    FChartList.OwnsObjects := AOwn;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChart.AddSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.AddSeries';
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      ASeries.LineSeries.OnMouseEnter := Self.SeriesOnMouseEnter;
      ASeries.LineSeries.OnMouseLeave := Self.SeriesOnMouseLeave;

      if not SeriesList.SeriesExist(ASeries) then
        FSeriesList.AddSeries(ASeries);
      ReAdjustYAxis(FNormalised);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TTimeSeriesComparitorChart.OnGetLineSeriesMarkText';
var
  LDisplayStr : string;
  LXValue     : Double;
  LXPos,
  LYPos       : Longint;
begin
  try
    MarkText := '';
    if(ValueIndex <> 0) then Exit;

    LDisplayStr := '';
    if Sender = FFSLLineSeries then
      LDisplayStr := FAppModules.Language.GetString('TTimeSeriesComparitorChartPanel.FSLMarkText')
    else if Sender = FDSLLineSeries then
      LDisplayStr := FAppModules.Language.GetString('TTimeSeriesComparitorChartPanel.DSLMarkText')
    else if Sender = FBOTLineSeries then
      LDisplayStr := FAppModules.Language.GetString('TTimeSeriesComparitorChartPanel.BOTMarkText')
    else
      LDisplayStr := Format(FAppModules.Language.GetString('TTimeSeriesComparitorChartPanel.OperatingLevel'),[Sender.Tag]);

    if (LDisplayStr <> '') then
    begin
      LXValue := Sender.XValues.MaxValue - Sender.XValues.MinValue;
      LXValue := LXValue/2.5;
      LYPos   := Sender.CalcYPosValue(Sender.YValue[ValueIndex] + 1.5);
      LXPos   := Sender.CalcXPosValue(LXValue);
      FChart.Canvas.Font.Style := [fsBold];
      FChart.BottomAxis.DrawAxisLabel(LXPos + 150,LYPos, 0,LDisplayStr);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChart.ClearReservoirLineSeries;
const OPNAME = 'TTimeSeriesComparitorChart.ClearReservoirLineSeries';
begin
  try
    if Assigned(FFSLLineSeries) then
    begin
      FFSLLineSeries.Clear;
      FFSLLineSeries.Active := False;
    end;
    if Assigned(FDSLLineSeries) then
    begin
      FDSLLineSeries.Clear;
      FDSLLineSeries.Active := False;
    end;
    if Assigned(FBOTLineSeries) then
    begin
      FBOTLineSeries.Clear;
      FBOTLineSeries.Active := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChart.GetMinimumSeriesXValue: double;
const OPNAME = 'TTimeSeriesComparitorChart.GetMinimumSeriesXValue';
var
  LIndex        : integer;
  LMinValue,
  LNewMinValue  : double;
begin
  Result := 0.0;
  try
    LMinValue := 0.0;
    if FChart <> nil then
    begin
      for LIndex := 0 to FSeriesList.SeriessCount - 1 do
      begin
        LNewMinValue := FChart.Series[LIndex].XValues.MinValue;
        if(LMinValue = 0) then
          LMinValue := LNewMinValue
        else
        begin
          if(LMinValue > LNewMinValue) then
            LMinValue := LNewMinValue;
        end;
      end;
      Result := LMinValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChart.GetMaximumSeriesXValue : double;
const OPNAME = 'TTimeSeriesComparitorChart.GetMaximumSeriesXValue';
var
  LIndex        : integer;
  LMaxValue,
  LNewMaxValue  : double;
begin
  Result := 0.0;
  try
    LMaxValue := 0.0;
    if FChart <> nil then
    begin
      for LIndex := 0 to FSeriesList.SeriessCount - 1 do
      begin
        LNewMaxValue := FChart.Series[LIndex].XValues.MaxValue;
        if(LMaxValue = 0) then
          LMaxValue := LNewMaxValue
        else
        begin
          if(LMaxValue > LNewMaxValue) then
            LMaxValue := LNewMaxValue;
        end;
      end;
      Result := LMaxValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChart.PrepareReservoirLineSeries;
const OPNAME = 'TTimeSeriesComparitorChart.PrepareReservoirLineSeries';
var
  LResult : boolean;
begin
  try
    LResult := IsSeletedSeriesReservoirElevation;
    FFSLLineSeries.Active      := FShowLineSeries and (FSeriesList.SeriessCount = 1) and LResult;
    FDSLLineSeries.Active      := FShowLineSeries and (FSeriesList.SeriessCount = 1) and LResult;
    FBOTLineSeries.Active      := FShowLineSeries and (FSeriesList.SeriessCount = 1) and LResult;
    FFSLLineSeries.ParentChart := FChart;
    FDSLLineSeries.ParentChart := FChart;
    FBOTLineSeries.ParentChart := FChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChart.SelectAndShowSeriesBySeriesName(AReservoirNodeNumber : integer);
const OPNAME = 'TTimeSeriesComparitorChart.SelectAndShowSeriesBySeriesName';
var
  LReservoirList          : IReservoirDataList;
  LReservoirData          : IReservoirData;
  LFullStorageLevel,
  LDeadStorageLevel,
  LMinValue,
  LMaxValue,
  LBottomOfReservoirLevel : double;
begin
   try
    ClearReservoirLineSeries;
    if(FShowLineSeries and (FSeriesList.SeriessCount = 1)) then
    begin
      LReservoirList := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList;
      if(LReservoirList <> nil) then
      begin
        LReservoirData := LReservoirList.ReservoirOrNodeByIdentifier[AReservoirNodeNumber];
        if(LReservoirData <> nil) then
        begin
          LFullStorageLevel       := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
          LDeadStorageLevel       := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
          LBottomOfReservoirLevel := LReservoirData.ReservoirZoneElevationsData.BottomOfReservoir.Elevation;

          LMinValue := GetMinimumSeriesXValue;
          LMaxValue := GetMaximumSeriesXValue;

          FFSLLineSeries.AddXY(LMinValue,LFullStorageLevel);
          FFSLLineSeries.AddXY(LMaxValue,LFullStorageLevel);
          FDSLLineSeries.AddXY(LMinValue,LDeadStorageLevel);
          FDSLLineSeries.AddXY(LMaxValue,LDeadStorageLevel);
          FBOTLineSeries.AddXY(LMinValue,LBottomOfReservoirLevel);
          FBOTLineSeries.AddXY(LMaxValue,LBottomOfReservoirLevel);
        end;
      end;
    end;
    PrepareReservoirLineSeries;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
