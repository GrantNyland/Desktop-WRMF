//
//
//  UNIT      : Contains TYRCContainerPureRegressionSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerPureRegressionSeries;

interface

uses
  Classes,
  Contnrs,
  Graphics,
  Series,
  Chart,
  TeEngine,
  TeeShape,
  UAbstractObject,
  UAbstractYRCData,
  UCurveFittingOperations,
  UYRCContainerAbstractLineSeries;

type
  TYRCContainerPureRegressionSeries = class(TYRCContainerAbstractLineSeries)
    procedure SetSeriesProperties(ALineSeries: TLineSeries);
    procedure PopulateSeries(ALineSeries: TLineSeries; ATargetDraft:TAbstractYRCTargetDraft);
  public
    function CreateSeries(AOwner: TChart; AYRCGraphDataObject:TYRCGraphDataObject): boolean; override;
    procedure OnTargetDraftSelectionChange(ASelectionIndeces: TIntegerArray);override;
    procedure OnPlaneSelectionChange (APlaneIndex : integer);override;
    function ApplySelectedPlotPlane: boolean;
    function ShowPlotPlaneSelection: boolean;
  end;

implementation

uses
  Math,
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerPureRegressionSeries }

procedure TYRCContainerPureRegressionSeries.OnPlaneSelectionChange (APlaneIndex : integer);
const OPNAME = 'TYRCContainerPureRegressionSeries.OnPlaneSelectionChange';
begin
  inherited OnPlaneSelectionChange(APlaneIndex);
  try
    ClearSeriesList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionSeries.OnTargetDraftSelectionChange(ASelectionIndeces: TIntegerArray);
const OPNAME = 'TYRCContainerPureRegressionSeries.OnTargetDraftSelectionChange';
begin
  inherited OnTargetDraftSelectionChange(ASelectionIndeces);
  try
    if (Length(ASelectionIndeces) > 0) then
    begin
      HideAllSeries;
      RedrawLineSeries(ASelectionIndeces[0]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerPureRegressionSeries.CreateSeries(AOwner: TChart; AYRCGraphDataObject: TYRCGraphDataObject): boolean;
const OPNAME = 'TYRCContainerPureRegressionSeries.CreateSeries';
var
  LTargetDraftIndex: integer;
  LSeries: TLineSeries;
  LPlane :TAbstractYRCPlane;
  LTargetDraft :TAbstractYRCTargetDraft;
begin
  Result := inherited CreateSeries(AOwner,AYRCGraphDataObject);
  try
    Result := Result and ClearSeriesList;
    if Result and (FPlaneIndex >= 0) and
       (FPlaneIndex < AYRCGraphDataObject.PlanesCount) and
       (AYRCGraphDataObject.PlanesCount > 0) then
    begin
      LPlane := AYRCGraphDataObject.YRCPlane[FPlaneIndex];
      for LTargetDraftIndex := 0 to LPlane.TargetDraftCount -1 do
      begin
        LTargetDraft := LPlane.TargetDraft[LTargetDraftIndex];
        if Assigned(LTargetDraft) then
        begin
          LSeries := TLineSeries.Create(nil);
          FLineSeriesList.AddObject(IntToStr(LTargetDraftIndex),LSeries);
          AOwner.AddSeries(LSeries);
          LSeries.Active := False;
          SetSeriesProperties(LSeries);
          PopulateSeries(LSeries,LTargetDraft);
        end;
      end;
      Result := True;
      FSeriesCreated := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionSeries.SetSeriesProperties(ALineSeries: TLineSeries);
const OPNAME = 'TYRCContainerPureRegressionSeries.SetSeriesProperties';
begin
  try
    if Assigned(ALineSeries) then
    begin
      ALineSeries.ShowInLegend := False;
      ALineSeries.Pointer.Visible := False;
      ALineSeries.LinePen.Style := psDot;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionSeries.PopulateSeries( ALineSeries: TLineSeries; ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerPureRegressionSeries.PopulateSeries';
var
  LCount: integer;
  LYValue: double;
  LXValue: double;
  LConstantsArray: array [0..3] of double;
begin
  try
    if Assigned(ALineSeries) and Assigned(ATargetDraft) and ATargetDraft.PureRegressionConstants.Valid then
    begin
      ALineSeries.Clear;

      LConstantsArray[0] := ATargetDraft.PureRegressionConstants.AValue;
      LConstantsArray[1] := ATargetDraft.PureRegressionConstants.BValue;
      LConstantsArray[2] := ATargetDraft.PureRegressionConstants.CValue;
      LConstantsArray[3] := ATargetDraft.PureRegressionConstants.DValue;

      LCount := Ceil(ATargetDraft.TargetDraftXValue);
      for LCount := LCount to 100 do
      begin
        // Y = Ax*3 + Bx*2 +Cx + D
        LXValue := LCount;
        LYValue := 0.0;
        for LIndex := 0 to 3 do
          LYValue := LYValue + (LConstantsArray[LIndex] * Power(LXValue,LIndex));

        ALineSeries.AddXY(LXValue,LYValue);
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerPureRegressionSeries.ShowPlotPlaneSelection: boolean;
const OPNAME = 'TYRCContainerPureRegressionSeries.ShowPlotPlaneSelection';
begin
  Result := inherited ShowPlotPlaneSelection;
  try
    Result := Result and HideAllSeries;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCContainerPureRegressionSeries.ApplySelectedPlotPlane: boolean;
const OPNAME = 'TYRCContainerPureRegressionSeries.ApplySelectedPlotPlane';
begin
  Result := inherited ApplySelectedPlotPlane;
  try
    if Result and   (not FSeriesCreated) then
      Result := CreateSeries(FOwnerChart,FYRCGraphDataObject);
    Result := Result and HideAllSeries;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
