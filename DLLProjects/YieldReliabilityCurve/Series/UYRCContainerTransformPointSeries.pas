//
//
//  UNIT      : Contains TYRCContainerTransformPointSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerTransformPointSeries;

interface

uses
  Classes,
  Contnrs,
  Graphics,
  Series,
  Chart,
  TeEngine,
  TeeShape,
  UYRCSeries,
  UAbstractObject,
  UAbstractYRCData,
  UYRCContainerAbstractPointSeries,
  UAbstractModelData;

type

  TYRCContainerTransformPointSeries = class(TYRCContainerAbstractPointSeries)
  protected
    //FYieldFormatStr: string;
    //FShowYieldValue: boolean;
    //FCurrentPlane: integer;
    procedure CreateMemberObjects; override;
    procedure ConfigureSeries(APointSeries: TYRCPointSeries);
    procedure OnGetSeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
    function ConvertToRequiredPlotPlane(AActualExceedanceProbabilty: double;
                                        AActualPlotPlane, ARequiredPlotPlane: integer): double;
    procedure SetPointSeriesProperties(ASeriesIndex: integer;APointSeries: TYRCPointSeries);override;
    procedure PopulatePointSeries(ASeriesIndex: integer;APointSeries: TYRCPointSeries; ATargetDraft:TAbstractYRCTargetDraft);override;
  public
    function LanguageHasChanged: boolean; override;
    function CreateSeries(AOwner: TChart; AYRCGraphDataObject:TAbstractYRCGraphDataObject): boolean; override;
    procedure OnPlotPlaneSelectionChange(APlotPlane: integer);override;
    procedure OnPeriodSelectionChange(APeriod: integer); override;

    function ApplySelectedPlotPlane: boolean;override;
    function ShowPlotPlaneSelection: boolean;override;
    //function ManiplateTargetDraftDeterministic: boolean;override;
    //function ManiplateTargetDraftRegression: boolean;override;
    function ReloadSeries(ASeriesIndex: integer): boolean; override;
    function PointsVisible : boolean; override;
    //property ShowYieldValue: boolean read FShowYieldValue write FShowYieldValue;
  end;

implementation

uses
  Math,
  SysUtils,
  UAbstractYRCModelDataObject,
  UErrorHandlingOperations, UYRCContainerAbstractSeries;


{ TYRCContainerTransformPointSeries }

procedure TYRCContainerTransformPointSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerTransformPointSeries.CreateMemberObjects';
begin
  inherited;
  try
    //FYieldFormatStr := '';
    //FShowYieldValue := False;
    //FCurrentPlane := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerTransformPointSeries.LanguageHasChanged: boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.LanguageHasChanged';
var
  LCount: integer;
  LActive:Boolean;
  LSeries: TYRCPointSeries;
begin
  Result := inherited LanguageHasChanged;
  try
    for LCount := 0 to FPointSeriesList.Count - 1 do
    begin
      LSeries := PointSeriesByIndex[LCount];
      if assigned(LSeries) then
      begin
        LActive := LSeries.Active;
        ToggleSeries(LCount,not LActive);
        ToggleSeries(LCount,LActive);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerTransformPointSeries.CreateSeries(AOwner: TChart;AYRCGraphDataObject: TAbstractYRCGraphDataObject): boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.CreateSeries';
var
  LCount,
  LIndex,
  LPlaneIndex,
  LTmpPlaneIndex : integer;
  LPlane:TAbstractYRCPlane;
  LPointSeries: TYRCPointSeries;
  LTargetDraft : TAbstractYRCTargetDraft;
  LXValue,
  LYValue : double;
begin
  Result := inherited CreateSeries(AOwner,AYRCGraphDataObject);
  try
    Result := Result and ClearSeriesList;
    if Result then
    begin
      //FYieldFormatStr := AYRCGraphDataObject.YRCLanguageStrings.YieldFormatStr;
      LTmpPlaneIndex := AYRCGraphDataObject.PlaneIndex;
      for LPlaneIndex := 0 to AYRCGraphDataObject.PlanesCount - 1 do
      begin
        LPlane := TAbstractYRCPlane(AYRCGraphDataObject.YRCPlane[LPlaneIndex]);
        AYRCGraphDataObject.PlaneIndex := LPlaneIndex;
        if Assigned(LPlane) then
        begin
          for LCount := 0 to LPlane.TargetDraftCount - 1 do
          begin
            LTargetDraft := LPlane.TargetDraft[LCount];
            if Assigned(LTargetDraft) then
            begin
              LPointSeries := TYRCPointSeries.Create(AOwner);
              LPointSeries.Active := False;
              LPointSeries.ShowInLegend := False;
              FPointSeriesList.AddObject(IntToStr(LPlaneIndex),LPointSeries);
              LPointSeries.SeriesIndex := FPointSeriesList.Count - 1;
              LPointSeries.TargetDraftIndex := LCount;
              LPointSeries.PlaneIndex := LPlaneIndex;
              AOwner.AddSeries(LPointSeries);
              ConfigureSeries(LPointSeries);
              {if ((LPointSeries.SeriesColor = clYellow) or
                  (LPointSeries.SeriesColor = clWhite) or
                  (LPointSeries.SeriesColor = clLime))  then
                LPointSeries.SeriesColor := clBlack;}
              for LIndex := Low(LTargetDraft.PureRegressionPointsArray.YRCRecordPointArray) to
                  High(LTargetDraft.PureRegressionPointsArray.YRCRecordPointArray) do
              begin
                LXValue := LTargetDraft.PureRegressionPointsArray.YRCRecordPointArray[LIndex].XValue;
                LYValue := LTargetDraft.PureRegressionPointsArray.YRCRecordPointArray[LIndex].YValue;
                LPointSeries.AddXY(LXValue, LYValue);
              end;
            end;
          end;
        end;
      end;
      AYRCGraphDataObject.PlaneIndex := LTmpPlaneIndex;
      Result := True;
      FSeriesCreated := True;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTransformPointSeries.ConfigureSeries(APointSeries: TYRCPointSeries);
const OPNAME = 'TYRCContainerTransformPointSeries.ConfigureSeries';
begin
  try
    if assigned(APointSeries) then
    begin
      APointSeries.OnGetMarkText          := OnGetSeriesMarkText;
      //APointSeries.Pointer.Brush.Color    := clBlack;
      //APointSeries.Pointer.Style          := psTriangle;
      APointSeries.Pointer.Visible        := True;
      APointSeries.ShowInLegend           := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTransformPointSeries.OnGetSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer;
          var MarkText: String);
const OPNAME = 'TYRCContainerTransformPointSeries.OnGetSeriesMarkText';
var
  LYieldMarkFormatStr: string;
  LChartSeries: TChartSeries;
begin
  try
    MarkText := '';
    LChartSeries := GetPointSeriesByIndex(FCurrentPlane);
    if FShowYieldValue and Assigned(LChartSeries) and (LChartSeries = Sender)then
    begin
      // Save values locally.
      LYieldMarkFormatStr := FAppModules.Language.GetString(FYieldFormatStr);
      if(Trim(LYieldMarkFormatStr) <> '') then
            MarkText := Format(LYieldMarkFormatStr,[Sender.YValues.Value[ValueIndex]]);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTransformPointSeries.OnPlotPlaneSelectionChange(APlotPlane: integer);
const OPNAME = 'TYRCContainerTransformPointSeries.OnPlotPlaneSelectionChange';
var
 LCount : integer;
 LPointSeries : TYRCPointSeries;
begin
  inherited OnPlotPlaneSelectionChange(APlotPlane);
  try
    FShowYieldValue := False;
    FCurrentPlane   := APlotPlane;

    for LCount := 0 to FPointSeriesList.Count-1 do
    begin
      LPointSeries := PointSeriesByIndex[LCount];
      if (LCount = APlotPlane) then
      begin
        //FShowYieldValue := True;
        RepaintPointSeries(LPointSeries,pwThick);
      end
      else
      begin
        LPointSeries.OnGetMarkText := nil;
        RepaintPointSeries(LPointSeries,pwThin);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTransformPointSeries.OnPeriodSelectionChange(APeriod: integer);
const OPNAME = 'TYRCContainerTransformPointSeries.OnPeriodSelectionChange';
var
  LCount: integer;
  LIndex: integer;
  LPlane:TAbstractYRCPlane;
  LConvertedXValue: double;
  LCurrentPointSeries: TYRCPointSeries;
begin
  try
    //Repopulate

    for LCount := 0 to FPointSeriesList.Count -1 do
    begin
      LPlane := TAbstractYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject.YRCPlane[LCount];
      if Assigned(LPlane) then
      begin
        LCurrentPointSeries := PointSeriesByIndex[LCount];
        if Assigned(LCurrentPointSeries) then
        begin
          LCurrentPointSeries.Clear;
          for LIndex := 0 to High(LPlane.YXPointArrayObject.YRCRecordPointArray) do
          begin
            LConvertedXValue := ConvertToRequiredPlotPlane(
              LPlane.YXPointArrayObject.YRCRecordPointArray[LIndex].XValue,
              LPlane.PlaneYears, APeriod);
            LCurrentPointSeries.AddXY(
              LConvertedXValue,
//              LPlane.YXPointArrayObject.YRCRecordPointArray[LIndex].XValue,
              LPlane.YXPointArrayObject.YRCRecordPointArray[LIndex].YValue);
          end;
        end;
      end;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCContainerTransformPointSeries.ConvertToRequiredPlotPlane(AActualExceedanceProbabilty: double;
         AActualPlotPlane, ARequiredPlotPlane: integer): double;
const OPNAME = 'TYRCContainerTransformPointSeries.ConvertToRequiredPlotPlane';
var
  LExponent: double;
begin
  Result := 0.0;
  try
    LExponent := ARequiredPlotPlane / AActualPlotPlane;
    Result    := Power((AActualExceedanceProbabilty / 100.0), LExponent) * 100.0;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCContainerTransformPointSeries.ShowPlotPlaneSelection: boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.ShowPlotPlaneSelection';
var
  LTargetDraftCount : integer;
begin
  Result := inherited ShowPlotPlaneSelection;
  try
    FPlaneIndex := FYRCGraphDataObject.PlaneIndex;  
    if(FPlaneIndex > 0) and (FPlaneIndex <= FYRCGraphDataObject.PlanesCount) then
    begin
      LTargetDraftCount := FYRCGraphDataObject.YRCPlane[FPlaneIndex].TargetDraftCount;
      //Result := Result and DisplayAllSeries(FPlaneIndex, LTargetDraftCount, False);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCContainerTransformPointSeries.ApplySelectedPlotPlane: boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.ApplySelectedPlotPlane';
var
  LTargetDraftCount : integer;
begin
  Result := inherited ApplySelectedPlotPlane;
  try
    FPlaneIndex := FYRCGraphDataObject.PlaneIndex;  
    if(FPlaneIndex > 0) and (FPlaneIndex <= FYRCGraphDataObject.PlanesCount) then
    begin
      LTargetDraftCount := FYRCGraphDataObject.YRCPlane[FPlaneIndex].TargetDraftCount;
      Result := Result and HideAllSeries(FPlaneIndex, LTargetDraftCount);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TYRCContainerTransformPointSeries.ManiplateTargetDraftDeterministic: boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.ManiplateTargetDraftDeterministic';
var
  LTargetDraftCount : integer;
begin
  Result := inherited ManiplateTargetDraftDeterministic;
  try
    FPlaneIndex := FYRCGraphDataObject.PlaneIndex;  
    LTargetDraftCount := FYRCGraphDataObject.YRCPlane[FPlaneIndex].TargetDraftCount;
    Result := Result and HideAllSeries(FPlaneIndex, LTargetDraftCount);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCContainerTransformPointSeries.ManiplateTargetDraftRegression: boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.ManiplateTargetDraftRegression';
var
  LTargetDraftCount : integer;
begin
  Result := inherited ManiplateTargetDraftRegression;
  try
    FPlaneIndex := FYRCGraphDataObject.PlaneIndex;  
    LTargetDraftCount := FYRCGraphDataObject.YRCPlane[FPlaneIndex].TargetDraftCount;
    Result := Result and HideAllSeries(FPlaneIndex, LTargetDraftCount);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TYRCContainerTransformPointSeries.ReloadSeries(ASeriesIndex: integer): boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.ReloadSeries';
var
  LPointSeries : TYRCPointSeries;
  LTargetDraft : TAbstractYRCTargetDraft;
  LPlane       : TAbstractYRCPlane;
  LPlaneIndex,
  LTargetDraftIndex  : integer;
begin
  Result := False;
  try
    LPointSeries := GetPointSeriesByIndex(ASeriesIndex);
    if Assigned(FYRCGraphDataObject.SelectedPlane) then
    begin
      LPlaneIndex := FYRCGraphDataObject.PlaneIndex;
      LPlane := FYRCGraphDataObject.YRCPlane[LPlaneIndex];
      if Assigned(LPlane) then
      begin
        LTargetDraftIndex := LPointSeries.TargetDraftIndex;

        LTargetDraft := TAbstractYRCTargetDraft(FYRCGraphDataObject.YRCTargetDraft[LPlaneIndex,LTargetDraftIndex]);
        if Assigned(LPointSeries) and Assigned(LTargetDraft) then
        begin
          SetPointSeriesProperties(ASeriesIndex, LPointSeries);
          PopulatePointSeries(ASeriesIndex,LPointSeries,LTargetDraft);
          Result := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTransformPointSeries.PopulatePointSeries(ASeriesIndex: integer; APointSeries: TYRCPointSeries;
  ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerTransformPointSeries.PopulatePointSeries';
var
  LPointsArray :TYRCRecordPointArray;
  LPointIndex: integer;
begin
  LPointsArray := nil;
  try
    if Assigned(APointSeries) and Assigned(ATargetDraft) then
    begin
      APointSeries.Clear;
      LPointsArray := ATargetDraft.PureRegressionPointsArray.YRCRecordPointArray;
      if (Transformed) then
      begin
        for LPointIndex := Low(LPointsArray) to High(LPointsArray) do
        begin
          if (LPointsArray[LPointIndex].YValue > 0.0000) then
            APointSeries.AddXY(LPointsArray[LPointIndex].XValue,LPointsArray[LPointIndex].YValue);
        end;
      end
      else
      begin
        for LPointIndex := (Low(LPointsArray) + 1) to High(LPointsArray) do
        begin
          if (LPointsArray[LPointIndex].YValue >= 0.0) then
            APointSeries.AddXY(LPointsArray[LPointIndex].XValue,LPointsArray[LPointIndex].YValue);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTransformPointSeries.SetPointSeriesProperties(ASeriesIndex: integer; APointSeries: TYRCPointSeries);
const OPNAME = 'TYRCContainerTransformPointSeries.SetPointSeriesProperties';
begin
  inherited SetPointSeriesProperties(ASeriesIndex,APointSeries);
  try
    if Assigned(APointSeries) then
    begin
      APointSeries.ShowInLegend  := False;
      APointSeries.Pointer.Style := psTriangle;
      APointSeries.Pointer.VertSize := 2;
      APointSeries.Pointer.HorizSize := 2;
      if (APointSeries.Color = clWhite)  or
         (APointSeries.Color = clWindow) or
         (APointSeries.Color = clYellow) then
        APointSeries.Color := clBlack;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerTransformPointSeries.PointsVisible: boolean;
const OPNAME = 'TYRCContainerTransformPointSeries.PointsVisible';
var
  LCount : integer;
  LSeries : TYRCPointSeries;
begin
  Result := inherited PointsVisible;
  try
    if FPointSeriesList.Count > 0 then
    begin
      for LCount := 0 to Pred(FPointSeriesList.Count) do
      begin
        LSeries := GetPointSeriesByIndex(LCount);
        if Assigned(LSeries) then
        begin
          if LSeries.Active then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
