//
//
//  UNIT      : Contains TYRCContainerAbstractLineSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerAbstractLineSeries;

interface

uses
  Classes,
  Contnrs,
  VCL.Graphics,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  VCLTee.Series,
  UYRCSeries,
  UAbstractObject,
  UAbstractYRCData,
  UYRCContainerAbstractSeries,
  UAbstractModelData;

type
  TYRCContainerAbstractLineSeries = class(TYRCContainerAbstractSeries)
  protected
    FLineSeriesList : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetLineSeriesByIndex(AIndex: Integer): TYRCLineSeries ; virtual;
    function GetLineSeriesByName(AName: string): TYRCLineSeries ; virtual;
    procedure SetLineSeriesProperties(ALineSeries: TYRCLineSeries);virtual;
    procedure PopulateLineSeriesWithTargetDraft(ASeriesIndex: integer;ALineSeries: TYRCLineSeries; ATargetDraft:TAbstractYRCTargetDraft);virtual;
    procedure PopulateLineSeriesWithPlane(ASeriesIndex: integer;ALineSeries: TYRCLineSeries; APlane:TAbstractYRCPlane);virtual;
    function ClearSeriesList: boolean; override;
    function RepaintLineSeries(ASeries: TYRCLineSeries;APenWidth: TPenWidth): boolean; virtual;
    function IndexOfSeries(ASeries : TYRCLineSeries) : integer;
    property LineSeriesByIndex[AIndex: Integer]: TYRCLineSeries read GetLineSeriesByIndex;
    property LineSeriesByName[AName: string]: TYRCLineSeries read GetLineSeriesByName;
    function DisplayAllSeries:boolean;override;
    function HideAllSeries:boolean;override;
  public
    function CalcMaxYValue: double;override;
    function CalcMinYValue: double;override;
    function LineSeriesCount: integer;
    function VisibleLineSeriesCount: integer;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations, Math;


{ TYRCContainerAbstractLineSeries }

procedure TYRCContainerAbstractLineSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerAbstractLineSeries.CreateMemberObjects';
begin
  inherited;
  try
    FLineSeriesList := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractLineSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerAbstractLineSeries.DestroyMemberObjects';
begin
  try
    ClearSeriesList;
    FLineSeriesList.Free;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.GetLineSeriesByIndex(AIndex: Integer): TYRCLineSeries;
const OPNAME = 'TYRCContainerAbstractLineSeries.GetLineSeriesByIndex';
//var
//  LCount : integer;
//  LSeries : TYRCLineSeries;
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FLineSeriesList.Count) then
    begin
      Result := TYRCLineSeries(FLineSeriesList.Objects[AIndex]);
    end;
    {for LCount := 0 to FLineSeriesList.Count - 1 do
    begin
      LSeries := TYRCLineSeries(FLineSeriesList.Objects[LCount]);
      if (LSeries.TargetDraftIndex = AIndex) and
         (LSeries.PlaneIndex = YRCGraphDataObject.PlaneIndex) then
      begin
        Result := LSeries;
        Break;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.GetLineSeriesByName(AName: string): TYRCLineSeries;
const OPNAME = 'TYRCContainerAbstractLineSeries.GetLineSeriesByName';
begin
  Result := nil;
  try
    if(FLineSeriesList.IndexOf(AName) <> -1) then
      Result := GetLineSeriesByIndex(FLineSeriesList.IndexOf(AName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.RepaintLineSeries(ASeries: TYRCLineSeries;APenWidth: TPenWidth): boolean;
const OPNAME = 'TYRCContainerAbstractLineSeries.RepaintLineSeries';
var
  LActive: boolean;
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      LActive        := ASeries.Active;
      ASeries.Active := False;
      case APenWidth of
        pwThick: ASeries.LinePen.Width := 2;
        pwThin:  ASeries.LinePen.Width := 1;
      end;//case
      ASeries.Active := LActive;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.DisplayAllSeries: boolean;
const OPNAME = 'TYRCContainerAbstractLineSeries.DisplayAllSeries';
var
  LCount: integer;
  LSeries: TYRCLineSeries;
begin
  Result := False;
  try
    HideAllSeries;
    for LCount := 0 to FLineSeriesList.Count - 1 do
    begin
      LSeries := GetLineSeriesByIndex(LCount);
      if Assigned(LSeries) then
        LSeries.Active := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.HideAllSeries: boolean;
const OPNAME = 'TYRCContainerAbstractLineSeries.HideAllSeries';
var
  LCount: integer;
  LSeries: TYRCLineSeries;
begin
  Result := False;
  try
    for LCount := 0 to FLineSeriesList.Count - 1 do
    begin
      LSeries := GetLineSeriesByIndex(LCount);
      if Assigned(LSeries) then
      begin
        LSeries.Active := False;
      end;
    end;
    
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.ClearSeriesList: boolean;
const OPNAME = 'TYRCContainerAbstractLineSeries.ClearSeriesList';
var
  //LCount: integer;
  LSeries: TYRCLineSeries;
begin
  Result := inherited ClearSeriesList;
  try
    while (FLineSeriesList.Count > 0) do
    begin
      LSeries := TYRCLineSeries(FLineSeriesList.Objects[0]);
      if Assigned(LSeries) and  Assigned(FOwnerChart) then
      begin
        FOwnerChart.RemoveSeries(LSeries);
        FLineSeriesList.Delete(0);
        LSeries.Free;
      end;
    end;
    FLineSeriesList.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractLineSeries.PopulateLineSeriesWithTargetDraft(ASeriesIndex: integer; ALineSeries: TYRCLineSeries;
  ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerAbstractLineSeries.PopulateLineSeriesWithTargetDraft';
begin
  try
    ALineSeries.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractLineSeries.PopulateLineSeriesWithPlane(ASeriesIndex: integer; ALineSeries: TYRCLineSeries;
          APlane: TAbstractYRCPlane);
const OPNAME = 'TYRCContainerAbstractLineSeries.PopulateLineSeriesWithPlane';
begin
  try
    ALineSeries.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractLineSeries.SetLineSeriesProperties(ALineSeries: TYRCLineSeries);
const OPNAME = 'TYRCContainerAbstractLineSeries.SetLineSeriesProperties';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.IndexOfSeries(ASeries: TYRCLineSeries): integer;
const OPNAME = 'TYRCContainerAbstractLineSeries.IndexOfSeries';
var
  LIndex : integer;
begin
  Result := -1;
  try
    for LIndex := 0 to FLineSeriesList.Count - 1 do
      if (ASeries = GetLineSeriesByIndex(LIndex)) then
      begin
        Result := LIndex;
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TYRCContainerAbstractLineSeries.GetCount: integer;
const OPNAME = 'TYRCContainerAbstractLineSeries.GetCount';
begin
  Result := 0;
  try
    if Assigned(FLineSeriesList) then
      Result := FLineSeriesList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TYRCContainerAbstractLineSeries.LineSeriesCount: integer;
const OPNAME = 'TYRCContainerAbstractLineSeries.LineSeriesCount';
begin
  Result := 0;
  try
    Result := FLineSeriesList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.VisibleLineSeriesCount: integer;
const OPNAME = 'TYRCContainerAbstractLineSeries.VisibleLineSeriesCount';
var
  LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0 to FLineSeriesList.Count-1 do
    begin
      if LineSeriesByIndex[LIndex].Visible then
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.CalcMaxYValue: double;
const OPNAME = 'TYRCContainerAbstractLineSeries.CalcMaxYValue';
var
  LIndex: integer;
begin
  Result := MinDouble;
  try
    for LIndex := 0 to FLineSeriesList.Count-1 do
    begin
      if (LineSeriesByIndex[LIndex].YValues.MaxValue > Result) then
        Result := LineSeriesByIndex[LIndex].YValues.MaxValue ;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractLineSeries.CalcMinYValue: double;
const OPNAME = 'TYRCContainerAbstractLineSeries.CalcMinYValue';
var
  LIndex: integer;
begin
  Result := MaxDouble;
  try
    for LIndex := 0 to FLineSeriesList.Count-1 do
    begin
      if (LineSeriesByIndex[LIndex].YValues.MinValue < Result) then
        Result := LineSeriesByIndex[LIndex].YValues.MinValue ;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
