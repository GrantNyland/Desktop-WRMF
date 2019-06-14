//
//
//  UNIT      : Contains TTimeSeriesComparitorChartManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/19
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorChartManager;

interface
uses
  DB,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  Contnrs,
  VCLTee.Chart,
  UAbstractObject,
  UTimeSeriesComparitorChartList,
  UTimeSeriesComparitorSeriesList;

type
  //TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Single) of object;
  TTimeSeriesComparitorChartManager = class(TAbstractAppObject)
  protected
    FChartDataList: TimeSeriesComparitorChartList;
    FChartClick : TNotifyEvent;
    FChartClickSeries : TChartClickSeries;
    FChartMouseMove : TMouseMoveEvent; //TMouseMoveEvent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetChartDataByIndex(AChartDataIndex: integer): TTimeSeriesComparitorChart;
    function GetChartDataByName(AChartName: string): TTimeSeriesComparitorChart;
  public
    function Initialise: boolean; override;
    function CreateChartData(AChartName: string): TTimeSeriesComparitorChart;
    function SelectChartData(AChartName: string): TTimeSeriesComparitorChart;
    function DeleteChartData(AChartName: string): boolean; overload;
    function DeleteChartData(var AChart: TTimeSeriesComparitorChart): boolean; overload;
    function RenameChartData(AOldChartName, ANewChartName: string): boolean;
    function ChartNames(ANamesList: TStringList): boolean;
    function Strip(AChartName: string): string;

    function DeleteViewData(AViewName: string): boolean;
    function RenameViewData(AOldViewName, ANewViewName: string): boolean;

    property ChartDataByIndex[AChartDataIndex: integer]: TTimeSeriesComparitorChart read GetChartDataByIndex;
    property ChartDataByName[ChartName: string]: TTimeSeriesComparitorChart read GetChartDataByName;
    property ChartDataList: TimeSeriesComparitorChartList read FChartDataList;
    property ChartClick : TNotifyEvent read FChartClick write FChartClick;
    property ChartSeriesClick : TChartClickSeries read FChartClickSeries write FChartClickSeries;
    property ChartMouseMove : TMouseMoveEvent read FChartMouseMove write FChartMouseMove;
  end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorChartManager }

procedure TTimeSeriesComparitorChartManager.CreateMemberObjects;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FChartDataList := TimeSeriesComparitorChartList.Create(FAppModules);
    FChartDataList.OwnsObjects := True;
    FChartClick := nil;
    FChartClickSeries := nil;
    FChartMouseMove := nil;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

procedure TTimeSeriesComparitorChartManager.DestroyMemberObjects;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChartDataList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.Initialise: boolean;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.Initialise';
begin
  Result := inherited Initialise;
  try
    FChartDataList.Initialise;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.GetChartDataByIndex(AChartDataIndex: integer): TTimeSeriesComparitorChart;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.GetChartDataByIndex';
begin
  Result := nil;
  try
    Result := FChartDataList.ChartByIndex[AChartDataIndex];
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.ChartNames(ANamesList: TStringList): boolean;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.ChartNames';
begin
  Result := False;
  try
    Result := FChartDataList.ChartNames(ANamesList);
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.GetChartDataByName(AChartName: string): TTimeSeriesComparitorChart;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.GetChartDataByName';
begin
  Result := nil;
  try
    Result := FChartDataList.ChartByName(AChartName);
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.Strip(AChartName: string): string;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.Strip';
var
  nIndexA: integer;
  nPos: integer;
  sResult: string;
  sChar: string;
begin
  Result := '';
  try
    sResult := 'C';
    for nIndexA := 1 to Length(AChartName) do
    begin
      sChar := Copy(AChartName, nIndexA, 1);
      nPos := Pos(sChar, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFHIJKLMNOPQRSTUVWXYZ');
      if (nPos > 0) then
        sResult := sResult + sChar;
    end;
    Result := sResult;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.CreateChartData(AChartName: string): TTimeSeriesComparitorChart;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.CreateChartData';
var
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := nil;
  try
    if (AChartName <> '') then
    begin
      LChartData := GetChartDataByName(AChartName);
      if not Assigned(LChartData) then
      begin
        LChartData := TTimeSeriesComparitorChart.Create(FAppModules);
        LChartData.ModelCode := FAppModules.StudyArea.ModelCode;
        LChartData.StudyAreaCode := FAppModules.StudyArea.StudyAreaCode;
        LChartData.SubAreaCode := FAppModules.StudyArea.SubAreaCode;
        LChartData.ChartName := AChartName;
        LChartData.Chart.Name := Strip(AChartName);
        LChartData.Chart.Title.Text.Text := AChartName;
        if Assigned(FChartClick) then
          LChartData.Chart.OnClick := FChartClick;
        if Assigned(FChartClickSeries) then
          LChartData.Chart.OnClickSeries := FChartClickSeries;
        if Assigned(FChartMouseMove) then
          LChartData.Chart.OnMouseMove := FChartMouseMove;
        FChartDataList.AddChart(LChartData);
      end;
      Result := LChartData;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.DeleteChartData(AChartName: string): boolean;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.DeleteChartData';
var
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    if (AChartName <> '') then
    begin
      LChartData := GetChartDataByName(AChartName);
      if Assigned(LChartData) then
      begin
        FChartDataList.RemoveChart(LChartData);
        LChartData.Free;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.SelectChartData(AChartName: string): TTimeSeriesComparitorChart;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.SelectChartData';
var
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := nil;
  try
    if (AChartName <> '') then
    begin
      LChartData := GetChartDataByName(AChartName);
      if Assigned(LChartData) then
      begin
        Result := LChartData;
      end
      else
        Result := CreateChartData(AChartName);
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.RenameChartData(AOldChartName, ANewChartName: string): boolean;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.RenameChartData';
var
  LCount: integer;
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    if (AOldChartName <> '') then
    begin
      LChartData := GetChartDataByName(AOldChartName);
      if Assigned(LChartData) then
      begin
        LChartData.ChartName := ANewChartName;
        LChartData.Chart.Name := Strip(ANewChartName);
        if (Trim(LChartData.Chart.Title.Text.Text) = AOldChartName) then
          LChartData.Chart.Title.Text.Text := ANewChartName;
        for LCount := 0 to LChartData.SeriesList.SeriessCount - 1 do
          LChartData.SeriesList.SeriesByIndex[LCount].ChartName := ANewChartName;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.DeleteChartData(var AChart: TTimeSeriesComparitorChart): boolean;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.DeleteChartData';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      Result := FChartDataList.RemoveChart(AChart);
      if Result then
      begin
        AChart := nil;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.DeleteViewData(AViewName: string): boolean;
const
  OPNAME = 'TTimeSeriesComparitorChartManager.DeleteViewData';
var
  LIndex,
    LCount: integer;
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    if (AViewName <> '') then
    begin
      for LCount := 0 to FChartDataList.ChartCount - 1 do
      begin
        LChartData := FChartDataList.ChartByIndex[LCount];
        if Assigned(LChartData) then
        begin
          LIndex := LChartData.ChartInViewList.IndexOf(AViewName);
          if (LIndex >= 0) then
          begin
            LChartData.ChartInViewList.Delete(LIndex);
            Break;
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

function TTimeSeriesComparitorChartManager.RenameViewData(AOldViewName, ANewViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorChartManager.RenameViewData';
var
  LIndex,
    LCount: integer;
  LChartData: TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    if (AOldViewName <> '') then
    begin
      for LCount := 0 to FChartDataList.ChartCount - 1 do
      begin
        LChartData := FChartDataList.ChartByIndex[LCount];
        if Assigned(LChartData) then
        begin
          LIndex := LChartData.ChartInViewList.IndexOf(AOldViewName);
          if (LIndex >= 0) then
          begin
            LChartData.ChartInViewList.Delete(LIndex);
            LChartData.ChartInViewList.Add(ANewViewName);
            Break;
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME);
  end;
end;

end.

