//
//
//  UNIT      : Contains TYRCContainerDeterministicFunctionSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerDeterministicFunctionSeries;

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

  TYRCContainerDeterministicFunctionSeries = class(TYRCContainerAbstractFunctionSeries)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function InitialiseSeries: boolean; override;
    procedure EditValidate(APointValue: TXYZValue; var AValueWithError: integer); override;
    procedure EditPoint(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer); override;
    function PopulateLineSeriesFromPointSeries(APointSeries: TPointSeries; ALineSeries: TLineSeries): boolean;override;
    procedure StartDragingPoint(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);override;
  public
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
    procedure OnTargetDraftSelectionChange(ASelectionIndeces: array of integer); override;
    procedure ProcessMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);override;
    procedure ProcessPointSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
              Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

implementation

uses
  Math,
  SysUtils,
  Messages,
  UCurveFittingOperations,
  UErrorHandlingOperations,
  UYRCContainerAbstractSeries;


{ TYRCContainerDeterministicFunctionSeries }

procedure TYRCContainerDeterministicFunctionSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.CreateMemberObjects';
begin
  inherited;
  try
    FYRCFunctionSeriesNodeEditor := TYRCDeterministicFunctionSeriesNodeEditor.Create(nil);
    FYRCFunctionSeriesNodeEditor.ValidateFunctionPoint := EditValidate;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicFunctionSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FYRCFunctionSeriesNodeEditor);
end;

function TYRCContainerDeterministicFunctionSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.ShowSeries';
var
  LLineSeries: TLineSeries;
  LPointSeries: TPointSeries;
  LCount: integer;
  LSeriesValues : TFunctionConstants;
begin
  Result := inherited ShowSeries(AOwner,AYRCDataObject);
  try
    FFunctionPointSeries.Clear;
    FFunctionLineSeries.Clear;
    if Result and Assigned(AOwner) and Assigned(AYRCDataObject) then
    begin
      for LCount := 0 to AYRCDataObject.FunctionConstantsList.Count -1 do
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
      Result := InitialiseSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerDeterministicFunctionSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.InitialiseSeries';
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
          LLineSeries.Active := True;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerDeterministicFunctionSeries.PopulateLineSeriesFromPointSeries(
         APointSeries: TPointSeries; ALineSeries: TLineSeries): boolean;
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.PopulateLineSeriesFromPointSeries';
var
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

      if (APointSeries.XValues.Count = 4) then
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
          LAValue := LConstantsArray[3];
          LBValue := LConstantsArray[2];
          LCValue := LConstantsArray[1];
          LDValue := LConstantsArray[0];

          LXValue := APointSeries.XValues.MinValue;
          while (LXValue < APointSeries.XValues.MaxValue) do
          begin
            // Y = Ax**3 + Bx**2 +Cx + D
            LYValue := (LAValue * (LXValue*LXValue*LXValue)) +
                       (LBValue * (LXValue*LXValue))  +
                       (LCValue * (LXValue)) +
                        LDValue;
            //if (LYValue > APointSeries.YValues.MaxValue) then
            //    LYValue := APointSeries.YValues.MaxValue;
            //if (LYValue < APointSeries.YValues.MinValue) then
            //    LYValue := APointSeries.YValues.MinValue;

            ALineSeries.AddXY(LXValue,LYValue);
            LXValue := LXValue + 1.0;
          end;

          LXValue := APointSeries.XValues.MaxValue;
          LYValue := (LAValue * (LXValue*LXValue*LXValue)) +
                     (LBValue * (LXValue*LXValue))  +
                     (LCValue * (LXValue)) +
                      LDValue;
          ALineSeries.AddXY(LXValue,LYValue);
          ALineSeries.Active := True;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicFunctionSeries.EditValidate(APointValue: TXYZValue; var AValueWithError: integer);
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.EditValidate';
var
  LAfterPointValue,
  LBeforePointValue: TXYZValue;
begin
  AValueWithError := 0;
  try
    if Assigned(APointValue) and Assigned(FCurrentTargetDraftPointSeries) and
       (FCurrentEditPointIndex >= 0) and (FCurrentEditPointIndex <= 3)then
    begin
      LAfterPointValue := nil;
      LBeforePointValue:= nil;

      if (FCurrentEditPointIndex > 0) then
        LBeforePointValue := TXYZValue.Create;
      if (FCurrentEditPointIndex < 3) then
        LAfterPointValue := TXYZValue.Create;

      if Assigned(LBeforePointValue) then
      begin
        LBeforePointValue.XValue := FCurrentTargetDraftPointSeries.XValues.Value[FCurrentEditPointIndex -1];
        LBeforePointValue.YValue := FCurrentTargetDraftPointSeries.YValues.Value[FCurrentEditPointIndex -1];
        if(LBeforePointValue.XValue >= APointValue.XValue) then
          AValueWithError := 1;
        if(LBeforePointValue.YValue < APointValue.YValue) then
          AValueWithError := 2;

      end;
      if(AValueWithError <> 0) then
        Exit;

      if Assigned(LAfterPointValue) then
      begin
        LAfterPointValue.XValue := FCurrentTargetDraftPointSeries.XValues.Value[FCurrentEditPointIndex +1];
        LAfterPointValue.YValue := FCurrentTargetDraftPointSeries.YValues.Value[FCurrentEditPointIndex +1];
        if(LAfterPointValue.XValue <= APointValue.XValue) then
          AValueWithError := 1;
        if(LAfterPointValue.YValue > APointValue.YValue) then
          AValueWithError := 2;
      end
      else
      begin
        if(APointValue.XValue <> 100.0) then
          AValueWithError := 1
        else
        if(APointValue.YValue < 0.0) then
          AValueWithError := 2;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicFunctionSeries.OnTargetDraftSelectionChange(ASelectionIndeces: array of integer);
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.OnTargetDraftSelectionChange';
var
  LLineSeries: TLineSeries;
  LIndex: integer;
begin
  try
  if Assigned(FCurrentTargetDraftLineSeries) then
    RepaintSeries(FCurrentTargetDraftLineSeries,pwThin);

  if(Length(ASelectionIndeces) > 0) then
  begin
    LIndex := ASelectionIndeces[0];
    if(LIndex >= 0) and (LIndex < FFunctionLineSeries.Count) then
    begin
      if Assigned(FFunctionLineSeries.Items[LIndex]) then
      begin
        LLineSeries := TLineSeries(FFunctionLineSeries.Items[LIndex]);
        if RepaintSeries(LLineSeries,pwThick) then
        begin
         FCurrentTargetDraftLineSeries := LLineSeries;
         FCurrentTargetDraftPointSeries := TPointSeries(FFunctionPointSeries.Items[LIndex]);
         BringSeriesToFront(FCurrentTargetDraftPointSeries);
        end;
      end;
    end;
  end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicFunctionSeries.EditPoint(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.EditPoint';
var
  LCursorPoint,
  LChartTopLeft: TPoint;
  LCurrentPoint: TXYZValue;
begin
  try
    if Assigned(Sender) and  Assigned(Series) and (Series is TPointSeries) and
       (Series = FCurrentTargetDraftPointSeries)then
    begin
      LCursorPoint.X  := X;
      LChartTopLeft.Y := Y;
      ClientToScreen(Sender.Handle,LCursorPoint);

      LChartTopLeft.X := Sender.Left;
      LChartTopLeft.Y := Sender.Top;
      ClientToScreen(Sender.Handle,LChartTopLeft);


      FYRCFunctionSeriesNodeEditor.SetPositionCloseToCursor(LCursorPoint.X,
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
            PopulateLineSeriesFromPointSeries(FCurrentTargetDraftPointSeries,FCurrentTargetDraftLineSeries);
          finally
            Series.Active := True;
          end;
        end;
      finally
        LCurrentPoint.Free;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicFunctionSeries.ProcessPointSeriesClick(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.ProcessPointSeriesClick';
var
  LSeriesIndex: integer;
begin
  inherited;
  try
    if(Series = FCurrentTargetDraftPointSeries)then
    begin
      LSeriesIndex := FunctionSeriesIndex(FFunctionPointSeries,Series);
      if (LSeriesIndex >= 0) then
      begin
        FCurrentTargetDraftLineSeries  := TLineSeries(FFunctionLineSeries.Items[LSeriesIndex]);
        FCurrentTargetDraftPointSeries := TPointSeries(FFunctionPointSeries.Items[LSeriesIndex]);
        FCurrentEditPointIndex:= ValueIndex;
      end;

      case Button of
        mbLeft  :StartDragingPoint(Sender,Series,ValueIndex,Button,Shift,X, Y);
        mbRight :
          begin
            EditPoint(Sender,Series,ValueIndex,Button,Shift,X, Y);
            FCurrentEditPointIndex:= -1;
          end;
      end;//case
    end;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TYRCContainerDeterministicFunctionSeries.StartDragingPoint(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.StartDragingPoint';
begin
  try
    if(ValueIndex <> 0) then
    begin
      UpdateDragMode(True);
      FDragValuesIndex       := -1;
      FStartDragPoint.X      := 0;
      FStartDragPoint.Y      := 0;
      FStartDragPointValue.XValue   := Series.XValue[ValueIndex];
      FStartDragPointValue.YValue   := Series.YValue[ValueIndex];
      if ChartValuesToPoint(TChart(Sender),FStartDragPoint,FStartDragPointValue) then
      begin
        FCurrentDragPointValue.CopyValues(FStartDragPointValue);
        FCurrentDragPoint.X := FStartDragPoint.X;
        FCurrentDragPoint.Y := FStartDragPoint.Y;
      end
      else
      UpdateDragMode(False);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicFunctionSeries.ProcessMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerDeterministicFunctionSeries.ProcessMouseMove';
var
  LEditError: integer;
begin
  inherited;
  try
    if FInDragMode then
    begin
      FCurrentDragPoint.X := X;
      FCurrentDragPoint.Y := Y;
      if ChartPointToValues(TChart(Sender),FCurrentDragPoint,FCurrentDragPointValue) then
      begin
        if(FCurrentEditPointIndex = 3) then
         FCurrentDragPointValue.XValue := 100.0;
        LEditError := 0;
        EditValidate(FCurrentDragPointValue,LEditError);
        if(LEditError = 0) then
        begin
          FCurrentTargetDraftPointSeries.Active := False;
          try
            FCurrentTargetDraftPointSeries.XValues[FCurrentEditPointIndex] := FCurrentDragPointValue.XValue;
            FCurrentTargetDraftPointSeries.YValues[FCurrentEditPointIndex] := FCurrentDragPointValue.YValue;
            PopulateLineSeriesFromPointSeries(FCurrentTargetDraftPointSeries,FCurrentTargetDraftLineSeries)
          finally
            FCurrentTargetDraftPointSeries.Active := True;
          end;
        end
        else
        begin
         UpdateDragMode(False);
         Beep;
        end;

        FCurrentDragPointValue.CopyValues(FStartDragPointValue);
        FCurrentDragPoint.X := FStartDragPoint.X;
        FCurrentDragPoint.Y := FStartDragPoint.Y;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

end.
