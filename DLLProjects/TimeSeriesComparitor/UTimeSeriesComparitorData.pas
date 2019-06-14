//
//
//  UNIT      : Contains UTimeSeriesComparitorChartNames Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2003/03/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorData;

interface

uses
  Classes,
  Contnrs,
  ComCtrls;
type


  TimeSeriesComparitorCaptionData = Class(TObject)
  protected
    FChanged: boolean;
    FTopCaptionName: String;
    FBottomCaptionName: String;
    FLeftCaptionName: String;
  public
     constructor Create;
     function AssignFrom(ACaptionData: TimeSeriesComparitorCaptionData): boolean;

     property Changed: boolean read FChanged;
     property TopCaptionName: string read FTopCaptionName write FTopCaptionName;
     property BottomCaptionName: string read FBottomCaptionName write FBottomCaptionName;
     property LeftCaptionName: string read FLeftCaptionName write FLeftCaptionName;
  end;

  TTimeSeriesComparitorSeriesData = Class(TObject)
  protected
    FSeriesName: String;
    FChanged: boolean;
    FSeries: TLineSeries;
    procedure SetSeriesName(ASeriesName: string);
  public
     constructor Create;
     Destructor Destroy; override;
     property Changed: boolean read FChanged;
     property SeriesName: string read FSeriesName write SetSeriesName;
     property Series: TCustomSeries read FSeries;
  end;

  TTimeSeriesComparitorChartData = Class(TObject)
  protected
    FChanged: boolean;
    FCaptionData : TimeSeriesComparitorCaptionData;
    FChartName : string;
    FCurrentSeries : TTimeSeriesComparitorSeriesData;
    FSeriesDataList: TObjectList;
    function  GetSeriesByName(ASeriesName: string):TTimeSeriesComparitorSeriesData;
    function  GetSeriesByIndex(AIndex: integer):TTimeSeriesComparitorSeriesData;
  public
     constructor Create;
     Destructor Destroy; override;

     function AddSeries(ASeries: TTimeSeriesComparitorSeriesData): boolean;
     function DeleteSeries(var ASeries: TTimeSeriesComparitorSeriesData): boolean;
     function SelectSeries(ASeries: TTimeSeriesComparitorSeriesData): boolean;
     function RenameSeries(ASeries: TTimeSeriesComparitorSeriesData;ANewSeriesName: string): boolean;

     function GetSeriesNamesList(ASeriesNamesList: TStringList): boolean;
     function CurrentSeriesName: string;

     property Changed: boolean read FChanged;
     property ChartName: string read FChartName write FChartName;
     property CaptionData: TimeSeriesComparitorCaptionData read FCaptionData;
     property CurrentSeries: TTimeSeriesComparitorSeriesData read FCurrentSeries;
     property SeriesDataList: TObjectList read FSeriesDataList;
     property SeriesByName[ASeriesName: string] :TTimeSeriesComparitorSeriesData  read GetSeriesByName;
     property SeriesByIndex[AIndex: integer] :TTimeSeriesComparitorSeriesData  read GetSeriesByIndex;
  end;

  TTimeSeriesComparitorViewData = Class(TObject)
  protected
    FChanged: boolean;
    FViewName: string;
    FCaptionData : TimeSeriesComparitorCaptionData;
    FCurrentChart : TTimeSeriesComparitorChartData;
    FChartDataList: TObjectList;
    function  GetChartByName(AChartName: string):TTimeSeriesComparitorChartData;
    function  GetChartByIndex(AIndex: integer)  :TTimeSeriesComparitorChartData;
  public
     constructor Create;
     Destructor Destroy; override;

     function GetChartNamesList(AChartNamesList: TStringList): boolean;
     function CurrentChartName: string;
     function UpdateCaptions(AChartCaptionList: TStringList): boolean;
     function GetCaptions(AChartCaptionList: TStringList): boolean;

     function AddChart(AChart: TTimeSeriesComparitorChartData): boolean;
     function SelectChart(AChart: TTimeSeriesComparitorChartData): boolean;
     function DeleteChart(var AChart: TTimeSeriesComparitorChartData): boolean;
     function RenameChart(AChart:TTimeSeriesComparitorChartData;ANewChartName: string): boolean;

     property Changed: boolean read FChanged;
     property ViewName: string read FViewName write FViewName;
     property CaptionData: TimeSeriesComparitorCaptionData read FCaptionData;
     property CurrentChart : TTimeSeriesComparitorChartData read FCurrentChart;
     property ChartDataList: TObjectList read FChartDataList;
     property ChartByName[AChartName: string]: TTimeSeriesComparitorChartData  read GetChartByName;
     property ChartByIndex[AIndex: integer]   : TTimeSeriesComparitorChartData  read GetChartByIndex;
  end;

  TTimeSeriesComparitorSubAreData = Class(TObject)
  protected
    FCurrentView: TTimeSeriesComparitorViewData;

    FViewDataList,
    FChartDataList: TObjectList;

    function  GetViewByName(AChartName: string):TTimeSeriesComparitorViewData;
    function  GetViewByIndex(AIndex: integer)  :TTimeSeriesComparitorViewData;

    function  GetChartByName(AChartName: string):TTimeSeriesComparitorChartData;
    function  GetChartByIndex(AIndex: integer)  :TTimeSeriesComparitorChartData;
  public
     constructor Create;
     Destructor Destroy; override;
     function Initialise: boolean;
     function GetSubAreaViewNames(AViewNamesList: TStringList): boolean;
     function GetSubAreaChartNames(AChartNamesList: TStringList): boolean;
     function UpdateCaptions(AChartNamesList: TStringList): boolean;
     function GetCaptions(AChartNamesList: TStringList): boolean;

     function SelectView(AViewName: string): boolean;
     function AddView(AViewName: string): boolean;
     function DeleteView(AViewName: string): boolean;
     function RenameView(AOldViewName,ANewViewName: string): boolean;

     function SelectChart(AChartName: string): boolean;
     function AddChart(AChartName: string): boolean;
     function DeleteChart(AChartName: string): boolean;
     function RenameChart(AOldChartName,ANewChartName: string): boolean;


     function CurrentViewName: string;
     property CurrentView: TTimeSeriesComparitorViewData read FCurrentView write FCurrentView;
     property ViewDataList: TObjectList read FViewDataList;
     property ChartDataList: TObjectList read FChartDataList;

     property ViewByName[AViewName: string]: TTimeSeriesComparitorViewData  read GetViewByName;
     property ViewByIndex[AIndex: integer]   : TTimeSeriesComparitorViewData  read GetViewByIndex;
     property ChartByName[AChartName: string]: TTimeSeriesComparitorChartData  read GetChartByName;
     property ChartByIndex[AIndex: integer]   : TTimeSeriesComparitorChartData  read GetChartByIndex;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TimeSeriesComparitorCaptionData }

constructor TimeSeriesComparitorCaptionData.Create;
const OPNAME = 'TimeSeriesComparitorCaptionData.Create';
begin
  Inherited Create;
  try
    FChanged := False;
    FTopCaptionName    := '';
    FBottomCaptionName := '';
    FLeftCaptionName   := '';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TimeSeriesComparitorCaptionData.AssignFrom(ACaptionData: TimeSeriesComparitorCaptionData): boolean;
const OPNAME = 'TimeSeriesComparitorCaptionData.AssignFrom';
begin
  Result := False;
  try
    if Assigned(ACaptionData) then
    begin
      FTopCaptionName    := ACaptionData.TopCaptionName;
      FBottomCaptionName := ACaptionData.BottomCaptionName;
      FLeftCaptionName   := ACaptionData.FLeftCaptionName;
      FChanged := True;
      Result   := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorSeriesData }

constructor TTimeSeriesComparitorSeriesData.Create;
const OPNAME = 'TTimeSeriesComparitorSeriesData.Create';
begin
  Inherited Create;
  try
    FChanged := False;
    FSeriesName := '';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TTimeSeriesComparitorSeriesData.Destroy;
const OPNAME = 'TTimeSeriesComparitorSeriesData.Destroy';
begin
  try
  inherited Destroy;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeriesData.SetSeriesName(ASeriesName: string);
const OPNAME = 'TTimeSeriesComparitorSeriesData.SetSeriesName';
begin
  try
   FSeriesName := ASeriesName;
   FChanged    := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorChartData }

constructor TTimeSeriesComparitorChartData.Create;
const OPNAME = 'TTimeSeriesComparitorChartData.Create';
begin
  Inherited Create;
  try
    FChanged := False;
    FChartName      := '';
    FCurrentSeries  := nil;
    FCaptionData    := TimeSeriesComparitorCaptionData.Create;
    FSeriesDataList := TObjectList.Create(False);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TTimeSeriesComparitorChartData.Destroy;
const OPNAME = 'TTimeSeriesComparitorChartData.Destroy';
begin
  try
    FCurrentSeries  := nil;
    FreeAndNil(FSeriesDataList);
    FreeAndNil(FCaptionData);
    inherited Destroy;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.GetSeriesByIndex(AIndex: integer): TTimeSeriesComparitorSeriesData;
const OPNAME = 'TTimeSeriesComparitorChartData.GetSeriesByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FSeriesDataList.Count) then
      Result := TTimeSeriesComparitorSeriesData(FSeriesDataList.Items[AIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.GetSeriesByName(ASeriesName: string): TTimeSeriesComparitorSeriesData;
const OPNAME = 'TTimeSeriesComparitorChartData.GetSeriesByName';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorSeriesData;
begin
  Result := nil;
  try
    for LIndex := 0 to FSeriesDataList.Count -1 do
    begin
      LObject := SeriesByIndex[LIndex];
      if (Assigned(LObject) and (LObject.SeriesName = ASeriesName)) then
      begin
        Result := LObject;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.GetSeriesNamesList(ASeriesNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorChartData.GetSeriesNamesList';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorSeriesData;
begin
  Result := False;
  try
    if Assigned(ASeriesNamesList) then
    begin
      ASeriesNamesList.Clear;
      for LIndex := 0 to FSeriesDataList.Count -1 do
      begin
        LObject := SeriesByIndex[LIndex];
        if Assigned(LObject) then
          ASeriesNamesList.Add(LObject.SeriesName);
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.AddSeries(ASeries: TTimeSeriesComparitorSeriesData): boolean;
const OPNAME = 'TTimeSeriesComparitorChartData.AddSeries';
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      if not Assigned(SeriesByName[ASeries.SeriesName]) then
      begin
        FSeriesDataList.Add(ASeries);
      end;
      Result := SelectSeries(ASeries);
      FChanged    := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.SelectSeries(ASeries: TTimeSeriesComparitorSeriesData): boolean;
const OPNAME = 'TTimeSeriesComparitorChartData.SelectSeries';
var
  LObject  : TTimeSeriesComparitorSeriesData;
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      LObject := SeriesByName[ASeries.SeriesName];
      if(Assigned(LObject)) then
      begin
        FCurrentSeries := LObject;
        FChanged    := True;
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.DeleteSeries(var ASeries: TTimeSeriesComparitorSeriesData): boolean;
const OPNAME = 'TTimeSeriesComparitorChartData.DeleteSeries';
var
  LObject  : TTimeSeriesComparitorSeriesData;
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      LObject := SeriesByName[ASeries.SeriesName];
      if Assigned(LObject) then
      begin
        FSeriesDataList.Remove(LObject);
        ASeries := nil;
        FCurrentSeries := nil;
      end;
      FChanged    := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.RenameSeries(ASeries: TTimeSeriesComparitorSeriesData;ANewSeriesName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorChartData.RenameSeries';
var
  LObject  : TTimeSeriesComparitorSeriesData;
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      LObject := SeriesByName[ASeries.SeriesName];
      if Assigned(LObject) then
      begin
        LObject.SeriesName := ANewSeriesName;
        SelectSeries(LObject);
      end;
      FChanged    := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartData.CurrentSeriesName: string;
const OPNAME = 'TTimeSeriesComparitorChartData.CurrentSeriesName';
begin
  Result := '';
  try
    if Assigned(CurrentSeries) then
      Result := CurrentSeries.SeriesName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorViewData }

constructor TTimeSeriesComparitorViewData.Create;
const OPNAME = 'TTimeSeriesComparitorViewData.Create';
begin
  Inherited Create;
  try
    FChanged := False;
    FViewName       := '';
    FCaptionData    := TimeSeriesComparitorCaptionData.Create;
    FCurrentChart   := nil;
    FChartDataList := TObjectList.Create(False);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TTimeSeriesComparitorViewData.Destroy;
const OPNAME = 'TTimeSeriesComparitorViewData.Destroy';
begin
  try
    FCurrentChart := nil;
    FreeAndNil(FCaptionData);
    FreeAndNil(FChartDataList);
    inherited Destroy;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.GetChartByIndex(AIndex: integer): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorViewData.GetChartByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FChartDataList.Count) then
      Result := TTimeSeriesComparitorChartData(FChartDataList.Items[AIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.GetChartByName(AChartName: string): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorViewData.GetChartByName';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := nil;
  try
    for LIndex := 0 to FChartDataList.Count -1 do
    begin
      LObject := ChartByIndex[LIndex];
      if (Assigned(LObject) and (LObject.FChartName = AChartName)) then
      begin
        Result := LObject;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.GetChartNamesList(AChartNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorViewData.GetChartNamesList';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(AChartNamesList) then
    begin
      AChartNamesList.Clear;
      for LIndex := 0 to FChartDataList.Count -1 do
      begin
        LObject := ChartByIndex[LIndex];
        if Assigned(LObject) then
          AChartNamesList.Add(LObject.FChartName);
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.AddChart(AChart: TTimeSeriesComparitorChartData): boolean;
const OPNAME = 'TTimeSeriesComparitorViewData.AddChart';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      if not Assigned(ChartByName[AChart.ChartName]) then
      begin
        FChartDataList.Add(AChart);
      end;
      Result := SelectChart(AChart);
      FChanged := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.DeleteChart(var AChart: TTimeSeriesComparitorChartData): boolean;
const OPNAME = 'TTimeSeriesComparitorViewData.DeleteChart';
var
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      LObject := ChartByName[AChart.ChartName];
      if Assigned(LObject) then
      begin
        FChartDataList.Remove(LObject);
        AChart := nil;
        FCurrentChart := nil;
        Result := True;
        FChanged := True;
      end;
    end;
    except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.RenameChart(AChart:TTimeSeriesComparitorChartData;ANewChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorViewData.RenameChart';
var
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      LObject := ChartByName[AChart.ChartName];
      if Assigned(LObject) then
      begin
        LObject.ChartName := ANewChartName;
        SelectChart(AChart);
      end;
      Result := True;
      FChanged := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.SelectChart(AChart: TTimeSeriesComparitorChartData): boolean;
const OPNAME = 'TTimeSeriesComparitorViewData.SelectChart';
var
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      LObject := ChartByName[AChart.ChartName];
      if Assigned(LObject) then
      begin
        FCurrentChart := LObject;
        FChanged := True;
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.CurrentChartName: string;
const OPNAME = 'TTimeSeriesComparitorViewData.CurrentChartName';
begin
  Result := '';
  try
    if Assigned(CurrentChart) then
      Result := CurrentChart.ChartName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTimeSeriesComparitorViewData.GetCaptions(AChartCaptionList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorViewData.GetCaptions';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(AChartCaptionList) then
    begin
      AChartCaptionList.Clear;
      AChartCaptionList.AddObject(FViewName,FCaptionData);
      for LIndex := 0 to FChartDataList.Count -1 do
      begin
        LObject := ChartByIndex[LIndex];
        if Assigned(LObject) then
          AChartCaptionList.AddObject(LObject.FChartName,LObject.CaptionData);
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewData.UpdateCaptions(AChartCaptionList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorViewData.UpdateCaptions';
var
  LIndex   : integer;
  LChartObject  : TTimeSeriesComparitorChartData;
  LCaptionObject  : TimeSeriesComparitorCaptionData;
begin
  Result := False;
  try
    if Assigned(AChartCaptionList) then
    begin
      for LIndex := 0 to AChartCaptionList.Count -1 do
      begin
        LCaptionObject := TimeSeriesComparitorCaptionData(AChartCaptionList.Objects[LIndex]);
        if(LIndex = 0) then
        begin
          FCaptionData.AssignFrom(LCaptionObject);
        end
        else
        begin
          LChartObject   := ChartByName[AChartCaptionList.Strings[LIndex]];
          if Assigned(LChartObject) and Assigned(LCaptionObject) then
          begin
            LChartObject.FCaptionData.AssignFrom(LCaptionObject);
          end;
        end;
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorSubAreData }

constructor TTimeSeriesComparitorSubAreData.Create;
const OPNAME = 'TTimeSeriesComparitorSubAreData.Create';
begin
  Inherited Create;
  try
    FCurrentView      := nil;
    FViewDataList     := TObjectList.Create(True);
    FChartDataList    := TObjectList.Create(True);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TTimeSeriesComparitorSubAreData.Destroy;
const OPNAME = 'TTimeSeriesComparitorSubAreData.Destroy';
begin
  try
    FreeAndNil(FChartDataList);
    FreeAndNil(FViewDataList);
    inherited Destroy;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.Initialise';
begin
  Result := False;
  try
    FCurrentView      := nil;
    FViewDataList.Clear;
    FChartDataList.Clear;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.GetViewByIndex(AIndex: integer): TTimeSeriesComparitorViewData;
const OPNAME = 'TTimeSeriesComparitorSubAreData.GetViewByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FViewDataList.Count) then
      Result := TTimeSeriesComparitorViewData(FViewDataList.Items[AIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.GetViewByName(AChartName: string): TTimeSeriesComparitorViewData;
const OPNAME = 'TTimeSeriesComparitorSubAreData.GetViewByName';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorViewData;
begin
  Result := nil;
  try
    for LIndex := 0 to FViewDataList.Count -1 do
    begin
      LObject := ViewByIndex[LIndex];
      if (Assigned(LObject) and (LObject.FViewName = AChartName)) then
      begin
        Result := LObject;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.AddView(AViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.AddView';
var
  LObject  : TTimeSeriesComparitorViewData;
begin
  Result := False;
  try
    if (AViewName <> '') and (not Assigned(ViewByName[AViewName])) then
    begin
      LObject := TTimeSeriesComparitorViewData.Create;
      LObject.ViewName := AViewName;
      FViewDataList.Add(LObject);
      FCurrentView := LObject;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.DeleteView(AViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.DeleteView';
var
  LObject  : TTimeSeriesComparitorViewData;
begin
  Result := False;
  try
    LObject := ViewByName[AViewName];
    if Assigned(LObject) then
    begin
      FViewDataList.Remove(LObject);
      FCurrentView := nil;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.RenameView(AOldViewName, ANewViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.RenameView';
var
  LObject  : TTimeSeriesComparitorViewData;
begin
  Result := False;
  try
    LObject := ViewByName[AOldViewName];
    if Assigned(LObject) then
    begin
      LObject.ViewName := ANewViewName;
      FCurrentView := LObject;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.GetSubAreaViewNames(AViewNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.GetSubAreaViewNames';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorViewData;
begin
  Result := False;
  try
    if Assigned(AViewNamesList) then
    begin
      AViewNamesList.Clear;
      for LIndex := 0 to FViewDataList.Count -1 do
      begin
        LObject := ViewByIndex[LIndex];
        if Assigned(LObject) then
          AViewNamesList.Add(LObject.FViewName);
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.GetSubAreaChartNames(AChartNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.GetSubAreaChartNames';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(AChartNamesList) then
    begin
      AChartNamesList.Clear;
      for LIndex := 0 to FChartDataList.Count -1 do
      begin
        LObject := ChartByIndex[LIndex];
        if Assigned(LObject) then
          AChartNamesList.Add(LObject.FChartName);
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.GetChartByIndex(AIndex: integer): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorSubAreData.GetChartByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FChartDataList.Count) then
      Result := TTimeSeriesComparitorChartData(FChartDataList.Items[AIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.GetChartByName(AChartName: string): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorSubAreData.GetChartByName';
var
  LIndex   : integer;
  LObject  : TTimeSeriesComparitorChartData;
begin
  Result := nil;
  try
    for LIndex := 0 to FChartDataList.Count -1 do
    begin
      LObject := ChartByIndex[LIndex];
      if (Assigned(LObject) and (LObject.FChartName = AChartName)) then
      begin
        Result := LObject;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.SelectView(AViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.SelectView';
var
  LObject  : TTimeSeriesComparitorViewData;
begin
  Result := False;
  try
    if (AViewName <> '') then
    begin
      LObject := ViewByName[AViewName];
      if Assigned(LObject) then
      begin
        FCurrentView := LObject;
        Result := True;
      end;
    end
    else
    begin
      FCurrentView := nil;
      Result := True;
    end;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.AddChart(AChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.AddChart';
var
  LObject : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    LObject := ChartByName[AChartName];
    if not Assigned(LObject) then
    begin
      LObject := TTimeSeriesComparitorChartData.Create;
      LObject.ChartName := AChartName;
      FChartDataList.Add(LObject);
      Result := True;
    end
    else
    Result := True;
    if Assigned(FCurrentView) then
    begin
      Result := FCurrentView.AddChart(LObject);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.DeleteChart(AChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.DeleteChart';
var
  LObject : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(FCurrentView) then
    begin
      LObject := ChartByName[AChartName];
      if Assigned(LObject) then
      begin
        Result := FCurrentView.DeleteChart(LObject);
        FChartDataList.Remove(LObject);
      end
      else
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.RenameChart(AOldChartName,ANewChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.RenameChart';
var
  LObject : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(FCurrentView) then
    begin
      LObject := ChartByName[AOldChartName];
      if Assigned(LObject) then
      begin
        Result := FCurrentView.RenameChart(LObject,ANewChartName);
      end
      else
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.SelectChart(AChartName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.SelectChart';
var
  LObject : TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if Assigned(FCurrentView) then
    begin
      LObject := ChartByName[AChartName];
      if Assigned(LObject) then
      begin
        Result := FCurrentView.SelectChart(LObject);
      end
      else
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.CurrentViewName: string;
const OPNAME = 'TTimeSeriesComparitorSubAreData.CurrentViewName';
begin
  Result := '';
  try
    if Assigned(FCurrentView) then
      Result := FCurrentView.ViewName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.GetCaptions(AChartNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.GetCaptions';
begin
  Result := False;
  try
    if Assigned(AChartNamesList) then
    begin
      AChartNamesList.Clear;
      if Assigned(FCurrentView) then
      begin
        Result := FCurrentView.GetCaptions(AChartNamesList);
      end
      else
        Result := True;
    end
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreData.UpdateCaptions(AChartNamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreData.UpdateCaptions';
begin
  Result := False;
  try
    if Assigned(AChartNamesList) then
    begin
      if Assigned(FCurrentView) then
      begin
        Result := FCurrentView.UpdateCaptions(AChartNamesList);
      end
      else
        Result := True;
    end
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

