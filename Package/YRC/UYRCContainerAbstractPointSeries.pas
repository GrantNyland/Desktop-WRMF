//
//
//  UNIT      : Contains TYRCContainerAbstractPointSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerAbstractPointSeries;

interface

uses
  Classes,
  Contnrs,
  VCL.Graphics,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  UYRCSeries,
  UAbstractObject,
  UAbstractYRCData,
  UYRCContainerAbstractSeries,
  UAbstractModelData;

type
  TYRCContainerAbstractPointSeries = class(TYRCContainerAbstractSeries)
  protected
    FPointSeriesList : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetPointSeriesByIndex(AIndex: Integer): TYRCPointSeries ; virtual;
    function GetPointSeriesByTargetDraftIndex(AIndex: Integer): TYRCPointSeries ; virtual;
    function GetPointSeriesByName(AName: string): TYRCPointSeries ; virtual;
    procedure SetPointSeriesProperties(APointSeries: TYRCPointSeries);virtual;
    procedure PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer;APointSeries: TYRCPointSeries; ATargetDraft:TAbstractYRCTargetDraft);virtual;
    procedure PopulatePointSeriesWithPlane(ASeriesIndex: integer;APointSeries: TYRCPointSeries; APlane:TAbstractYRCPlane);virtual;
    function ClearSeriesList: boolean; override;
    function RepaintPointSeries(ASeries: TYRCPointSeries;APenWidth: TPenWidth): boolean; virtual;
    property PointSeriesByIndex[AIndex: Integer]: TYRCPointSeries read GetPointSeriesByIndex;
    property PointSeriesByTargetDraft[AIndex: Integer]: TYRCPointSeries read GetPointSeriesByTargetDraftIndex;
    property PointSeriesByName[AName: string]: TYRCPointSeries read GetPointSeriesByName;
    function DisplayAllSeries:boolean;override;
    function HideAllSeries:boolean;override;
  public
    function CalcMaxYValue: double;override;
    function CalcMinYValue: double;override;
  end;

implementation

uses
  Math,
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerAbstractPointSeries }

procedure TYRCContainerAbstractPointSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerAbstractPointSeries.CreateMemberObjects';
begin
  inherited;
  try
    FPointSeriesList := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractPointSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerAbstractPointSeries.DestroyMemberObjects';
begin
  try
    ClearSeriesList;
    FPointSeriesList.Free;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.GetPointSeriesByIndex(AIndex: Integer): TYRCPointSeries;
const OPNAME = 'TYRCContainerAbstractPointSeries.GetPointSeriesByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FPointSeriesList.Count) then
    begin
      Result := TYRCPointSeries(FPointSeriesList.Objects[AIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.GetPointSeriesByTargetDraftIndex(AIndex: Integer): TYRCPointSeries;
const OPNAME = 'TYRCContainerAbstractPointSeries.GetPointSeriesByTargetDraftIndex';
var
  LCount : integer;
  LSeries : TYRCPointSeries;
begin
  Result := nil;
  try
    for LCount := 0 to FPointSeriesList.Count - 1 do
    begin
      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LCount]);
      if (LSeries.TargetDraftIndex = AIndex) and
         (LSeries.PlaneIndex = YRCGraphDataObject.PlaneIndex) then
      begin
        Result := LSeries;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.GetPointSeriesByName(AName: string): TYRCPointSeries;
const OPNAME = 'TYRCContainerAbstractPointSeries.GetPointSeriesByName';
begin
  Result := nil;
  try
    if(FPointSeriesList.IndexOf(AName) <> -1) then
      Result := GetPointSeriesByIndex(FPointSeriesList.IndexOf(AName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.RepaintPointSeries(ASeries: TYRCPointSeries;APenWidth: TPenWidth): boolean;
const OPNAME = 'TYRCContainerAbstractPointSeries.RepaintPointSeries';
var
  LActive: boolean;
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      LActive := ASeries.Active;
      ASeries.Active := False;
      case APenWidth of
        pwThick: begin
                   ASeries.Pointer.HorizSize := 4;
                   ASeries.Pointer.VertSize  := 4;
                 end;
        pwThin:  begin
                   ASeries.Pointer.HorizSize := 2;
                   ASeries.Pointer.VertSize  := 2;
                 end;
      end;//case
      ASeries.Active := LActive;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.DisplayAllSeries: boolean;
const OPNAME = 'TYRCContainerAbstractPointSeries.DisplayAllSeries';
var
  LIndex: integer;
  LSeries: TYRCPointSeries;
begin
  Result := False;
  try
    HideAllSeries;
    for LIndex := 0 to FPointSeriesList.Count - 1 do
    begin
      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LIndex]);
      if Assigned(LSeries) then
        LSeries.Active := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.HideAllSeries: boolean;
const OPNAME = 'TYRCContainerAbstractPointSeries.HideAllSeries';
var
  LIndex: integer;
  LSeries: TYRCPointSeries;
begin
  Result := False;
  try
    for LIndex := 0 to FPointSeriesList.Count - 1 do
    begin
      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LIndex]);
      if Assigned(LSeries) then
        LSeries.Active := False;
    end; 
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.ClearSeriesList: boolean;
const OPNAME = 'TYRCContainerAbstractPointSeries.ClearSeriesList';
var
  LCount: integer;
  LSeries: TYRCPointSeries;
begin
  Result := inherited ClearSeriesList;
  try
    for LCount := 0 to FPointSeriesList.Count - 1 do
    begin
      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LCount]);
      if Assigned(LSeries) and Assigned(FOwnerChart) then
      begin
        FOwnerChart.RemoveSeries(LSeries);
        LSeries.Free;
      end;
    end;
    FPointSeriesList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCContainerAbstractPointSeries.SetPointSeriesProperties(APointSeries: TYRCPointSeries);
const OPNAME = 'TYRCContainerAbstractPointSeries.SetPointSeriesProperties';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractPointSeries.PopulatePointSeriesWithPlane(ASeriesIndex: integer; APointSeries: TYRCPointSeries;
          APlane: TAbstractYRCPlane);
const OPNAME = 'TYRCContainerAbstractPointSeries.PopulatePointSeriesWithPlane';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractPointSeries.PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer; APointSeries: TYRCPointSeries;
          ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerAbstractPointSeries.PopulatePointSeriesWithTargetDraft';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.CalcMaxYValue: double;
const OPNAME = 'TYRCContainerAbstractPointSeries.CalcMaxYValue';
var
  LIndex: integer;
begin
  Result := MinDouble;
  try
    for LIndex := 0 to FPointSeriesList.Count-1 do
    begin
      if (PointSeriesByIndex[LIndex].YValues.MaxValue > Result) then
        Result := PointSeriesByIndex[LIndex].YValues.MaxValue ;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractPointSeries.CalcMinYValue: double;
const OPNAME = 'TYRCContainerAbstractPointSeries.CalcMinYValue';
var
  LIndex: integer;
begin
  Result := MaxDouble;
  try
    for LIndex := 0 to FPointSeriesList.Count-1 do
    begin
      if (PointSeriesByIndex[LIndex].YValues.MinValue < Result) then
        Result := PointSeriesByIndex[LIndex].YValues.MinValue ;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
