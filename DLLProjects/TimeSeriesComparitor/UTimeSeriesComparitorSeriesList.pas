//
//
//  UNIT      : Contains TTimeSeriesComparitorSeriesList Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorSeriesList;

interface
uses
  VCLTee.Series,
  VCLTee.TeEngine,
  VCLTee.Chart,
  Classes,
  vcl.Controls,
  vcl.ComCtrls,
  vcl.StdCtrls,
  Contnrs,
  UAbstractObject,
  UViewDataItem;

type
  TTimeSeriesLineSeries = class(TLineSeries)
  protected
    FSeriesHint   : string;
  public
    property SeriesHint : string read FSeriesHint write FSeriesHint;
  end;

  TTimeSeriesComparitorSeries = Class(TAbstractAppObject)
  protected
    FModelCode: String;
    FStudyAreaCode: String;
    FSubAreaCode: String;
    FScenarioCode: String;
    FChartName: string;
    FSeriesName: string;
    FViewID: string;
    FParentID: string;
    FTopParentID: string;
    FCommaTextCaption: string;
    FColor  : integer;
    FAddedToChart,
    FChanged: boolean;
    FLineSeries: TTimeSeriesLineSeries;
    FAbsoluteMaxValue: double;
    FUnits : string;
    FNormalised : boolean;
    FAxisCaption : string;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure SetCommaTextCaption(ACommaTextCaption : string);
    procedure SetChartName(AChartName : string);
    procedure SetSeriesName(ASeriesName : string);
    procedure SetViewID(AViewID : string);
    procedure SetParentID(AParentID : string);
    procedure SetTopParentID(ATopParentID : string);
    procedure SetColor(AColor : integer);
    procedure SetAddedToChart(AAddedToChart : boolean);
    procedure SetAbsoluteMaxValue(AValue: double);
    procedure SetUnits(AValue : string);
    procedure SetNormalised(AValue: boolean);
    procedure SetAxisCaption(AValue: string);
    function CheckForEquality(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string): boolean;
  public
     function CommaTextName: string;
     function CommaTextNameExcScenario: string;
     property CommaTextCaption    : string  read FCommaTextCaption write SetCommaTextCaption;
     property ModelCode           : string  read FModelCode write FModelCode;
     property StudyAreaCode       : string  read FStudyAreaCode write FStudyAreaCode;
     property SubAreaCode         : string  read FSubAreaCode write FSubAreaCode;
     property ScenarioCode        : string  read FScenarioCode write FScenarioCode;
     property ChartName           : string  read FChartName write SetChartName;
     property SeriesName          : string  read FSeriesName write SetSeriesName;
     property ViewID              : string  read FViewID  write SetViewID;
     property ParentID            : string  read FParentID  write SetParentID;
     property TopParentID         : string  read FTopParentID write SetTopParentID;
     property Color               : integer read FColor write SetColor;
     property AddedToChart        : boolean read FAddedToChart write SetAddedToChart;
     property Changed             : boolean read FChanged write FChanged;
     property LineSeries          : TTimeSeriesLineSeries read FLineSeries;
     property AbsoluteMaxValue    : double read FAbsoluteMaxValue write SetAbsoluteMaxValue;
     property Units               : string read FUnits write SetUnits;
     property Normalised          : boolean read FNormalised write SetNormalised;
     property AxisCaption         : string read FAxisCaption write SetAxisCaption;
  end;

  TTimeSeriesComparitorSeriesList = class(TAbstractAppObject)
  protected
    FSeriesList: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetSeriesByIndex(ASeriesIndex: integer): TTimeSeriesComparitorSeries;
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(AOwn: boolean);
  public
    function Initialise: boolean; override;
    function SeriessCount: integer;

    function AddSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
    function RemoveSeries(ASeries: TTimeSeriesComparitorSeries): boolean;

    function SeriesIndex(ASeries: TTimeSeriesComparitorSeries): integer;
    function SeriesExist(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string): boolean;overload;
    function SeriesExist(ASeries: TTimeSeriesComparitorSeries): boolean;overload;
    function SeriesByName(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string): TTimeSeriesComparitorSeries;
    function SeriesByCommaTextCaption(AChartName,ACommaTextCaption: string): TTimeSeriesComparitorSeries;
    function SeriesNames(ANamesList: TStringList): boolean;
    property SeriesByIndex[ASeriesIndex: integer]: TTimeSeriesComparitorSeries read GetSeriesByIndex;
    property OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
  end;

implementation
uses
  SysUtils,
  System.Types,
  UErrorHandlingOperations, StrUtils;

{ TTimeSeriesComparitorSeries }

procedure TTimeSeriesComparitorSeries.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSeries.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FLineSeries       := TTimeSeriesLineSeries.Create(nil);
    FLineSeries.Cursor := crHandPoint;
    FLineSeries.XValues.DateTime := False;
    FModelCode        := '';
    FStudyAreaCode    := '';
    FSubAreaCode      := '';
    FScenarioCode     := '';
    FSeriesName       := '';
    FViewID           := '';
    FParentID         := '';
    FTopParentID      := '';
    FCommaTextCaption := '';
    //FColor           := 0;
    FAddedToChart     := False;
    FChanged          := False;
    FAbsoluteMaxValue := 0.0;
    FUnits            := '';
    FNormalised       := False;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSeries.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if(FLineSeries.ParentChart = nil) then
      FreeAndNil(FLineSeries);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetAddedToChart(AAddedToChart: boolean);
const OPNAME = 'TTimeSeriesComparitorSeries.SetAddedToChart';
begin
  try
    FAddedToChart := AAddedToChart;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetColor(AColor: integer);
const OPNAME = 'TTimeSeriesComparitorSeries.SetColor';
begin
  try
    FColor := AColor;
    if(FLineSeries.SeriesColor <> AColor) then
      FLineSeries.SeriesColor := AColor;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetSeriesName(ASeriesName: string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetSeriesName';
begin
  try
    FSeriesName := ASeriesName;
    if Assigned(FLineSeries) then
    begin
      FLineSeries.Title := ASeriesName;
      FLineSeries.RefreshSeries;
    end;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetViewID(AViewID: string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetViewID';
begin
  try
    FViewID := AViewID;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetParentID(AParentID : string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetParentID';
begin
  try
    FParentID := AParentID;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetTopParentID(ATopParentID : string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetTopParentID';
begin
  try
    FTopParentID := ATopParentID;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetAbsoluteMaxValue(AValue: double);
const OPNAME = 'TTimeSeriesComparitorSeries.SetAbsoluteMaxValue';
begin
  try
    FAbsoluteMaxValue := AValue;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetUnits(AValue: string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetUnits';
begin
  try
    FUnits := AValue;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetNormalised(AValue: boolean);
const OPNAME = 'TTimeSeriesComparitorSeries.SetNormalised';
var
  LIndex: integer;
begin
  try
    if FAbsoluteMaxValue = 0 then
      FAbsoluteMaxValue := 1; //Prevent divide by zero error
    if(FNormalised <> AValue) then
    begin
      for LIndex := 0 to FLineSeries.YValues.Count -1 do
        if AValue then
          FLineSeries.YValues[LIndex] := (FLineSeries.YValues[LIndex]/ FAbsoluteMaxValue) * 100.0
         else
          FLineSeries.YValues[LIndex] := (FLineSeries.YValues[LIndex]* FAbsoluteMaxValue) / 100.0;
      FNormalised := AValue;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeries.CheckForEquality(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSeries.CheckForEquality';
begin
  Result := False;
  try
    Result := (AModelCode     = Self.ModelCode) and
              (AStudyAreaCode = Self.StudyAreaCode) and
              (ASubAreaCode   = Self.SubAreaCode) and
              (AScenarioCode  = Self.ScenarioCode) and
              (AChartName     = Self.ChartName) and
              (ASeriesName    = Self.SeriesName) and
              (AViewID        = Self.ViewID) and
              (AParentID      = Self.ParentID) and
              (ATopParentID   = Self.TopParentID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetChartName(AChartName: string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetChartName';
begin
  try
    FChartName := AChartName;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetCommaTextCaption(ACommaTextCaption: string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetCommaTextCaption';
begin
  try
    FCommaTextCaption := ACommaTextCaption;
    FLineSeries.FSeriesHint := ACommaTextCaption;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeries.CommaTextName: string;
const OPNAME = 'TTimeSeriesComparitorSeries.CommaTextName';
var
  LNames: TStringList;
begin
  Result := '';
  try
    LNames := TStringList.Create;
    try
      LNames.Add(Self.FScenarioCode);
      LNames.Add(Self.FTopParentID);
      LNames.Add(Self.FParentID);
      LNames.Add(Self.FViewID);
      LNames.Add(Self.FSeriesName);
      Result := LNames.CommaText;
    finally
      FreeAndNil(LNames);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeries.CommaTextNameExcScenario: string;
const OPNAME = 'TTimeSeriesComparitorSeries.CommaTextNameExcScenario';
var
  LNames: TStringList;
begin
  Result := '';
  try
    LNames := TStringList.Create;
    try
      LNames.Add(Self.FTopParentID);
      LNames.Add(Self.FParentID);
      LNames.Add(Self.FViewID);
      LNames.Add(Self.FSeriesName);
      Result := LNames.CommaText;
    finally
      FreeAndNil(LNames);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeries.SetAxisCaption(AValue: string);
const OPNAME = 'TTimeSeriesComparitorSeries.SetAxisCaption';
begin
  try
    FAxisCaption := AValue;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorSeriesList }

procedure TTimeSeriesComparitorSeriesList.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSeriesList.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSeriesList := TObjectList.Create(False);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeriesList.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSeriesList.DestroyMemberObjects';
begin
  try
    FSeriesList.Clear;
    FreeAndNil(FSeriesList);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesList.Initialise';
begin
  Result := inherited Initialise;
  try
    FSeriesList.Clear;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.GetSeriesByIndex(ASeriesIndex: integer): TTimeSeriesComparitorSeries;
const OPNAME = 'TTimeSeriesComparitorSeriesList.GetSeriesByIndex';
begin
  Result := nil;
  try
    if (ASeriesIndex >= 0) and (ASeriesIndex < FSeriesList.Count) then
      Result := TTimeSeriesComparitorSeries(FSeriesList.Items[ASeriesIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.AddSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesList.AddSeries';
begin
  Result := False;
  try
    if Assigned(ASeries) and (not SeriesExist(ASeries)) then
    begin
      FSeriesList.Add(ASeries);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.RemoveSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesList.RemoveSeries';
begin
  Result := False;
  try
    if Assigned(ASeries) and SeriesExist(ASeries)then
    begin
      FSeriesList.Remove(ASeries);
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.SeriessCount: integer;
const OPNAME = 'TTimeSeriesComparitorSeriesList.SeriessCount';
begin
  Result := 0;
  try
    Result := FSeriesList.Count;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.SeriesIndex(ASeries: TTimeSeriesComparitorSeries): integer;
const OPNAME = 'TTimeSeriesComparitorSeriesList.SeriesIndex';
var
  LIndex: integer;
begin
  Result := -1;
  try
    for LIndex := 0 to FSeriesList.Count -1 do
    begin
      if(ASeries = GetSeriesByIndex(LIndex)) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.SeriesByName(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
         AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string): TTimeSeriesComparitorSeries;
const OPNAME = 'TTimeSeriesComparitorSeriesList.SeriesByName';
var
  LIndex: integer;
  LSeries: TTimeSeriesComparitorSeries;
begin
  Result := nil;
  try
    if(ASeriesName <> '') then
    begin
      for LIndex := 0 to FSeriesList.Count -1 do
      begin
        LSeries := GetSeriesByIndex(LIndex);
        if LSeries.CheckForEquality(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
                   AChartName,ASeriesName,AViewID,AParentID,ATopParentID) then
        begin
          Result := LSeries;
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.SeriesNames(ANamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesList.SeriesNames';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(ANamesList) then
    begin
      ANamesList.Clear;
      for LIndex := 0 to FSeriesList.Count - 1 do
      begin
        if Assigned(SeriesByIndex[LIndex]) then
          ANamesList.Add(SeriesByIndex[LIndex].SeriesName);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.SeriesExist(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesList.SeriesExist';
begin
  Result := False;
  try
    Result := Assigned(SeriesByName(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
                       AChartName,ASeriesName,AViewID,AParentID,ATopParentID));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.SeriesExist(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesList.SeriesExist';
begin
  Result := False;
  try
    if assigned(ASeries) then
    begin
    Result := Assigned(SeriesByName(ASeries.ModelCode,ASeries.StudyAreaCode,ASeries.SubAreaCode,
              ASeries.ScenarioCode,ASeries.ChartName,ASeries.SeriesName,ASeries.ViewID,ASeries.ParentID,ASeries.TopParentID));
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.GetOwnsObjects: boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesList.GetOwnsObjects';
begin
  Result := False;
  try
    Result := FSeriesList.OwnsObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeriesList.SetOwnsObjects(AOwn: boolean);
const OPNAME = 'TTimeSeriesComparitorSeriesList.SetOwnsObjects';
begin
  try
    FSeriesList.OwnsObjects := AOwn;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesList.SeriesByCommaTextCaption(AChartName,ACommaTextCaption: string): TTimeSeriesComparitorSeries;
const OPNAME = 'TTimeSeriesComparitorSeriesList.SeriesByCommaTextCaption';
var
  LIndex: integer;
  LSeries: TTimeSeriesComparitorSeries;
begin
  Result := nil;
  try
    if(ACommaTextCaption <> '') then
    begin
      for LIndex := 0 to FSeriesList.Count -1 do
      begin
        LSeries := GetSeriesByIndex(LIndex);
        if (LSeries.FCommaTextCaption = ACommaTextCaption) and (LSeries.FChartName = AChartName)then
        begin
          Result := LSeries;
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


end.
