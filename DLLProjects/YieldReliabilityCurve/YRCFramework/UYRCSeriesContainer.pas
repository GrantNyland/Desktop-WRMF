//
//
//  UNIT      : Contains TYRCSeriesContainer Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCSeriesContainer;

interface

uses
  Classes,
  Contnrs,
  Graphics,
  Series,
  Chart,
  TeEngine,
  TeeShape,
  UYRCDataObject,
  UAbstractObject,
  UAbstractComponent;

type

  TYRCSeriesContainer = class(TAbstractAppObject)
  protected
    FSeriesList : TStringList;
    FLoadCases  : TLoadCases;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetSeriesByIndex(AIndex: Integer): TChartSeries ; virtual;
    function GetSeriesByName(AName: string): TChartSeries ; virtual;
    function InitialiseSeries(AYRCDataObject:TYRCDataObject): boolean; virtual; abstract;
    function RedrawSeries(AIndex: integer): boolean;
  public
    ProcessSeriesClickMessage: TChartClickSeries;
    function RepaintSeries: boolean; virtual;
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; virtual;
    property SeriesByIndex[AIndex: Integer]: TChartSeries read GetSeriesByIndex;
    property SeriesByName[AName: string]: TChartSeries read GetSeriesByName;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TYRCSeriesContainer }

procedure TYRCSeriesContainer.CreateMemberObjects;
const OPNAME = 'TYRCSeriesContainer.CreateMemberObjects';
begin
  inherited;
  try
    ProcessSeriesClickMessage := nil;
    FLoadCases  := TLoadCases.Create;
    FSeriesList := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSeriesContainer.DestroyMemberObjects;
const OPNAME = 'TYRCSeriesContainer.DestroyMemberObjects';
begin
  try
    FLoadCases.Free;
    FSeriesList.Free;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSeriesContainer.GetSeriesByIndex(AIndex: Integer): TChartSeries;
const OPNAME = 'TYRCSeriesContainer.GetSeriesByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FSeriesList.Count) then
    begin
      Result := TChartSeries(FSeriesList.Objects[AIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSeriesContainer.GetSeriesByName(AName: string): TChartSeries;
const OPNAME = 'TYRCSeriesContainer.GetSeriesByName';
begin
  Result := nil;
  try
    if(FSeriesList.IndexOf(AName) <> -1) then
      Result := GetSeriesByIndex(FSeriesList.IndexOf(AName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSeriesContainer.RedrawSeries(AIndex: integer): boolean;
const OPNAME = 'TYRCSeriesContainer.RedrawSeries';
var
  LSeries: TChartSeries;
begin
  Result := False;
  try
    LSeries := GetSeriesByIndex(AIndex);
    if Assigned(LSeries) then
    begin
      LSeries.Active := False;
      LSeries.Active := True;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSeriesContainer.RepaintSeries: boolean;
const OPNAME = 'TYRCSeriesContainer.RepaintSeries';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSeriesContainer.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCSeriesContainer.ShowSeries';
var
  LCount: integer;
  LSeries: TChartSeries;
begin
  Result := False;
  try
    FLoadCases.Clear;
    for LCount := 0 to FSeriesList.Count - 1 do
    begin
      LSeries := TChartSeries(FSeriesList.Objects[LCount]);
      if Assigned(LSeries) then
      begin
        TChart(LSeries.Owner).RemoveSeries(LSeries);
        LSeries.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
