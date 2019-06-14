//
//
//  UNIT      : Contains TTimeSeriesComparitorSeriesManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/19
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorSeriesManager;

interface
uses
  DB,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  Contnrs,
  UAbstractObject,
  UTimeSeriesComparitorSeriesList;

type

  TTimeSeriesComparitorSeriesManager = class(TAbstractAppObject)
  protected
    FSeriesDataList: TTimeSeriesComparitorSeriesList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function SeriesCount: integer;
    function SeriessInChartCount: integer;
    function CreateSeriesData(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string):TTimeSeriesComparitorSeries; overload;
    function CreateSeriesData(AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string):TTimeSeriesComparitorSeries; overload;
    function DeleteSeriesData(var ASeries: TTimeSeriesComparitorSeries):boolean; overload;
    function SeriesNames(ANamesList: TStringList): boolean;
    property SeriesDataList: TTimeSeriesComparitorSeriesList read FSeriesDataList;

    //function DeleteSeriesData(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
    //         ASeriesName,AViewID,AParentID,ATopParentID: string):boolean; overload;
    //function AddSeriesDataToChart(ASeries: TTimeSeriesComparitorSeries): boolean;
    //function RemoveSeriesDataFromChart(ASeries: TTimeSeriesComparitorSeries): boolean;
  end;

implementation
uses
  SysUtils,
  UViewDataItem,
  UErrorHandlingOperations;


{ TTimeSeriesComparitorSeriesManager }

procedure TTimeSeriesComparitorSeriesManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSeriesDataList    := TTimeSeriesComparitorSeriesList.Create(FAppModules);
    FSeriesDataList.OwnsObjects := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSeriesManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSeriesDataList);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesManager.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.Initialise';
begin
  Result := inherited Initialise;
  try
    FSeriesDataList.Initialise;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesManager.SeriesCount: integer;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.SeriesCount';
begin
  Result := 0;
  try
    Result := FSeriesDataList.SeriessCount;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTimeSeriesComparitorSeriesManager.SeriesNames(ANamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.SeriesNames';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(ANamesList) then
    begin
      ANamesList.Clear;
      for LIndex := 0 to FSeriesDataList.SeriessCount - 1 do
      begin
          ANamesList.Add(FSeriesDataList.SeriesByIndex[LIndex].SeriesName);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesManager.SeriessInChartCount: integer;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.SeriessInChartCount';
var
  LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0 to FSeriesDataList.SeriessCount - 1 do
    begin
      if FSeriesDataList.SeriesByIndex[LIndex].AddedToChart then
        Result := Result + 1;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesManager.CreateSeriesData(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID: string): TTimeSeriesComparitorSeries;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.CreateSeriesData';
var
  LSeriesData: TTimeSeriesComparitorSeries;
begin
  Result := nil;
  try
    if(ASeriesName <> '') then
    begin
      LSeriesData := FSeriesDataList.SeriesByName(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             AChartName,ASeriesName,AViewID,AParentID,ATopParentID);
      if not Assigned(LSeriesData) then
      begin
        LSeriesData := TTimeSeriesComparitorSeries.Create(FAppModules);
        LSeriesData.ModelCode     := AModelCode;
        LSeriesData.StudyAreaCode := AStudyAreaCode;
        LSeriesData.SubAreaCode   := ASubAreaCode;
        LSeriesData.ScenarioCode  := AScenarioCode;
        LSeriesData.ChartName     := AChartName;
        LSeriesData.SeriesName    := ASeriesName;
        LSeriesData.ParentID      := AParentID;
        LSeriesData.ViewID        := AViewID;
        LSeriesData.TopParentID   := ATopParentID;
        LSeriesData.Changed       := False;
        FSeriesDataList.AddSeries(LSeriesData);
      end;
      Result := LSeriesData;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesManager.CreateSeriesData(AChartName,ASeriesName,AViewID, AParentID, ATopParentID: string): TTimeSeriesComparitorSeries;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.CreateSeriesData';
var
  LSeriesData: TTimeSeriesComparitorSeries;
begin
  Result := nil;
  try
    if(ASeriesName <> '') then
    begin
      LSeriesData := FSeriesDataList.SeriesByName(FAppModules.StudyArea.ModelCode,
                     FAppModules.StudyArea.StudyAreaCode,
                     FAppModules.StudyArea.SubAreaCode,
                     FAppModules.StudyArea.SubAreaCode,
                     AChartName,ASeriesName,AViewID,AParentID,ATopParentID);
      if not Assigned(LSeriesData) then
      begin
        LSeriesData := TTimeSeriesComparitorSeries.Create(FAppModules);
        LSeriesData.ModelCode     := FAppModules.StudyArea.ModelCode;
        LSeriesData.StudyAreaCode := FAppModules.StudyArea.StudyAreaCode;
        LSeriesData.SubAreaCode   := FAppModules.StudyArea.SubAreaCode;
        LSeriesData.ScenarioCode  := FAppModules.StudyArea.ScenarioCode;
        LSeriesData.ChartName     := AChartName;
        LSeriesData.SeriesName    := ASeriesName;
        LSeriesData.ParentID      := AParentID;
        LSeriesData.ViewID        := AViewID;
        LSeriesData.TopParentID   := ATopParentID;
        LSeriesData.Changed       := False;
        FSeriesDataList.AddSeries(LSeriesData);
      end;
      Result := LSeriesData;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSeriesManager.DeleteSeriesData(var ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.DeleteSeriesData';
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      Result := FSeriesDataList.RemoveSeries(ASeries);
      if Result then
      begin
        ASeries := nil;
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{function TTimeSeriesComparitorSeriesManager.RemoveSeriesDataFromChart(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.RemoveSeriesDataFromChart';
begin
  Result := False;
  try
    FSeriesDataList.RemoveSeries(ASeries);
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;}

{function TTimeSeriesComparitorSeriesManager.DeleteSeriesData(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             ASeriesName,AViewID,AParentID,ATopParentID: string): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.DeleteSeriesData';
var
  LSeriesData: TTimeSeriesComparitorSeries;
begin
  Result := False;
  try
    if(ASeriesName <> '') then
    begin
      LSeriesData := FSeriesDataList.SeriesByName(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode,
             ASeriesName,AViewID,AParentID,ATopParentID);
      if Assigned(LSeriesData) then
      begin
        FSeriesDataList.RemoveSeries(LSeriesData);
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;}

{function TTimeSeriesComparitorSeriesManager.AddSeriesDataToChart(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSeriesManager.AddSeriesDataToChart';
begin
  Result := False;
  try
    if Assigned(ASeries)  then
    begin
      ASeries.AddedToChart  := True;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;}

end.
