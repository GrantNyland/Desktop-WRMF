//
//
//  UNIT      : Contains TYRCContainerRegressionFunctionSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerRegressionFunctionSeries;

interface

uses
  Classes,
  Controls,
  Contnrs,
  Windows,
  Graphics,
  Series,
  Chart,
  DBChart,
  TeEngine,
  TeeShape,
  teeprocs,
  UYRCDataObject,
  UAbstractObject,
  UYRCContainerAbstractFunctionSeries;

type

  TYRCContainerRegressionFunctionSeries = class(TYRCContainerAbstractFunctionSeries)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function InitialiseSeries: boolean; override;
    function PopulateLineSeriesFromPointSeries(APointSeries: TPointSeries; ALineSeries: TLineSeries): boolean; override;
  public
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
    procedure ProcessPointSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
              Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OnTargetDraftSelectionChange(ASelectionIndeces: array of integer); override;
  end;

implementation

uses
  Math,
  SysUtils,
  Messages,
  UCurveFittingOperations,
  UErrorHandlingOperations;


{ TYRCContainerRegressionFunctionSeries }

procedure TYRCContainerRegressionFunctionSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerRegressionFunctionSeries.CreateMemberObjects';
begin
  inherited;
  try
    FYRCFunctionSeriesNodeEditor := TYRCRegressionFunctionSeriesNodeEditor.Create(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionFunctionSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerRegressionFunctionSeries.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FYRCFunctionSeriesNodeEditor);
end;

function TYRCContainerRegressionFunctionSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerRegressionFunctionSeries.ShowSeries';
{var
  LLineSeries: TLineSeries;
  LPointSeries: TPointSeries;
  LCount: integer;
  LSeriesValues : TFunctionConstants;
}
begin
  Result := inherited ShowSeries(AOwner,AYRCDataObject);;
  try
    FFunctionPointSeries.Clear;
    FFunctionLineSeries.Clear;
    if Result and Assigned(AOwner) and Assigned(AYRCDataObject) then
    begin
      {for LCount := 0 to AYRCDataObject.FunctionConstantsList.Count -1 do
      begin
        LSeriesValues   := TFunctionConstants.Create;
        LSeriesValues.CopyValues(AYRCDataObject.FunctionConstantsList.FunctionConstants[LCount]);
        FLoadCases.Add(LSeriesValues);

        LLineSeries := TLineSeries.Create(AOwner);
        FFunctionLineSeries.Add(LLineSeries);
        AOwner.AddSeries(LLineSeries);

        LPointSeries := TPointSeries.Create(AOwner);
        FFunctionPointSeries.Add(LPointSeries);
        AOwner.AddSeries(LPointSeries);
      end;
      }
      Result := InitialiseSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerRegressionFunctionSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerRegressionFunctionSeries.InitialiseSeries';
var
  LCount: integer;
  LLineSeries: TLineSeries;
  LPointSeries: TPointSeries;
  LSeriesValues : TFunctionConstants;
begin
  Result := False;
  try
    for LCount := 0 to FLoadCases.Count - 1 do
    begin
      LSeriesValues   := TFunctionConstants(FLoadCases.Items[LCount]);
      if Assigned(LSeriesValues) then
      begin
        LPointSeries := TPointSeries(FFunctionPointSeries.Items[LCount]);
        LLineSeries  := TLineSeries(FFunctionLineSeries.Items[LCount]);

        if Assigned(LPointSeries) and Assigned(LLineSeries) then
        begin
          LPointSeries.Active := False;
          LPointSeries.Pointer.Style := psRectangle;
          LPointSeries.ShowInLegend := False;

          LLineSeries.Active := False;
          LLineSeries.ShowInLegend := False;

          LPointSeries.Clear;
          LPointSeries.AddXY(LSeriesValues.MinPoint.XValue, LSeriesValues.MinPoint.YValue);
          LPointSeries.AddXY(LSeriesValues.SecondPoint.XValue, LSeriesValues.SecondPoint.YValue);
          LPointSeries.AddXY(LSeriesValues.ThirdPoint.XValue, LSeriesValues.ThirdPoint.YValue);
          LPointSeries.AddXY(LSeriesValues.MaxPoint.XValue, LSeriesValues.MaxPoint.YValue);

          if not PopulateLineSeriesFromPointSeries(LPointSeries,LLineSeries) then
            Exit;

          LPointSeries.Active := True;
          LLineSeries.Active := False;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerRegressionFunctionSeries.PopulateLineSeriesFromPointSeries(
         APointSeries: TPointSeries; ALineSeries: TLineSeries): boolean;
const OPNAME = 'TYRCContainerRegressionFunctionSeries.PopulateLineSeriesFromPointSeries';
var
  LCount: integer;
  LYValue: double;
  LXValue: double;
  LConstantsArray: array [0..3] of double;
  LPointsArray: array [0..7] of double;
  LAValue, LBValue, LCValue, LDValue: Double;
begin
  Result := False;
  try
    if Assigned(ApointSeries) and Assigned(ALineSeries) then
    begin

      ALineSeries.Active := False;
      ALineSeries.Clear;

      if (APointSeries. XValues.Count = 4) then
      begin
        LConstantsArray[0] := 0.0;
        LConstantsArray[1] := 0.0;
        LConstantsArray[2] := 0.0;
        LConstantsArray[3] := 0.0;

        LPointsArray[0] := ApointSeries.YValues.Value[0];
        LPointsArray[1] := ApointSeries.XValues.Value[0];
        LPointsArray[2] := ApointSeries.YValues.Value[1];
        LPointsArray[3] := ApointSeries.XValues.Value[1];
        LPointsArray[4] := ApointSeries.YValues.Value[2];
        LPointsArray[5] := ApointSeries.XValues.Value[2];
        LPointsArray[6] := ApointSeries.YValues.Value[3];
        LPointsArray[7] := ApointSeries.XValues.Value[3];

        Result := CalculateCurveCoefficientsDelphi(ctTrinomialDeterministic,LConstantsArray,LPointsArray);
        if Result then
        begin
          LDValue := LConstantsArray[0];
          LCValue := LConstantsArray[1];
          LBValue := LConstantsArray[2];
          LAValue := LConstantsArray[3];
          for LCount := 0 to 100 do
          begin
            // Y = Ax*3 + Bx*2 +Cx + D
            LXValue := LCount;
            LYValue := (LAValue * (LXValue*3)) +
                       (LBValue * (LXValue*2))  +
                       (LCValue * (LXValue)) +
                        LDValue;
            ALineSeries.AddXY(LXValue,LYValue);

          end;
          ALineSeries.Active := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionFunctionSeries.ProcessPointSeriesClick(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: LongInt; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerRegressionFunctionSeries.ProcessPointSeriesClick';
var
  LSeriesIndex: integer;
  LCursorPoint,
  LChartTopLeft: TPoint;
  LCurrentPoint: TXYZValue;
  //LPointSeries: TPointSeries;
begin
  try
    if Assigned(Sender) and  Assigned(Series) and (Series is TPointSeries) then
    begin
      LSeriesIndex := FunctionSeriesIndex(FFunctionPointSeries,Series);
      if (LSeriesIndex >= 0) then
      begin

        //LPointSeries := TPointSeries(Series);

        LCursorPoint.X  := X;
        LChartTopLeft.Y := Y;
        ClientToScreen(Sender.Handle,LCursorPoint);

        LChartTopLeft.X := Sender.Left;
        LChartTopLeft.Y := Sender.Top;
        ClientToScreen(Sender.Handle,LChartTopLeft);


        FYRCFunctionSeriesNodeEditor.SetPositionCloseToCursor(
          LCursorPoint.X,
          LChartTopLeft.Y,
          LChartTopLeft.X,
          LChartTopLeft.Y,
          Sender.Height,
          Sender.Width);


        if(ValueIndex = 0) then
         FYRCFunctionSeriesNodeEditor.SetDisableOptions([doXValue,doYValue,doCountValue])
        else if(ValueIndex = (Series.XValues.Count-1)) then
         FYRCFunctionSeriesNodeEditor.SetDisableOptions([doXValue,doCountValue])
        else
         FYRCFunctionSeriesNodeEditor.SetDisableOptions([doCountValue]);

        LCurrentPoint := TXYZValue.Create;
        try

          LCurrentPoint.XValue := Series.XValues.Value[ValueIndex];
          LCurrentPoint.YValue := Series.YValues.Value[ValueIndex];
          LCurrentPoint.ZValue := 0.0;
          FYRCFunctionSeriesNodeEditor.CurrentPoint.CopyValues(LCurrentPoint);
          FYRCFunctionSeriesNodeEditor.ShowModal;

          if (FYRCFunctionSeriesNodeEditor.ModalResult = mrOK) then
          begin
            Series.Active := False;
            try
              LCurrentPoint.CopyValues(FYRCFunctionSeriesNodeEditor.CurrentPoint);
              Series.XValue[ValueIndex] := LCurrentPoint.XValue;
              Series.YValue[ValueIndex] := LCurrentPoint.YValue;
            finally
              Series.Active := True;
            end;

          end;
        finally
          LCurrentPoint.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionFunctionSeries.OnTargetDraftSelectionChange(ASelectionIndeces: array of integer);
const OPNAME = 'TYRCContainerRegressionFunctionSeries.OnTargetDraftSelectionChange';
begin
//HACK: finish
end;

end.
