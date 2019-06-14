//
//
//  UNIT      : Contains TYRCContainerAbstractFunctionSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerAbstractFunctionSeries;

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
  UAbstractObject,
  UYRCContainerAbstractSeries;

type


  TLoadCaseSeries = class(TObject)
  protected
    FReadOnlyLineSeries: TLineSeries;
    FEditablePointSeries: TPointSeries;
    FReadOnlyPointSeries: TPointSeries;
  public
    constructor Create;
    property ReadOnlyLineSeries  : TLineSeries   read FReadOnlyLineSeries  write FReadOnlyLineSeries;
    property EditablePointSeries : TPointSeries  read FEditablePointSeries write FEditablePointSeries;
    property ReadOnlyPointSeries : TPointSeries  read FReadOnlyPointSeries write FReadOnlyPointSeries;
 end;

  TYRCContainerAbstractFunctionSeries = class(TYRCContainerAbstractSeries)
  protected
    FEnabled: boolean;
    FInDragMode: boolean;
    FDragValuesIndex: integer;
    FStartDragPoint,
    FCurrentDragPoint: TPoint;
    FStartDragPointValue,
    FCurrentDragPointValue: TXYZValue;

    FYRCFunctionSeriesNodeEditor: TYRCDeterministicFunctionSeriesNodeEditor;
    FFunctionPointSeries: TObjectList;
    FFunctionLineSeries : TObjectList;

    FCurrentTargetDraftPointSeries: TPointSeries;
    FCurrentTargetDraftLineSeries: TLineSeries;
    FCurrentEditPointIndex: integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure UpdateEnabledState(AEnabled: Boolean);
    procedure UpdateDragMode(ADragging: Boolean);
    function  FunctionSeriesIndex(ASeriesContainer:TObjectList; ASeries: TChartSeries): integer;
    function  ChartPointToValues(AChart: TChart; APoint: TPoint; var AValues:TXYZValue): boolean;
    function  ChartValuesToPoint(AChart: TChart; var APoint: TPoint;  AValues:TXYZValue): boolean;
    function  PopulateLineSeriesFromPointSeries(APointSeries: TPointSeries; ALineSeries: TLineSeries): boolean; virtual; abstract;
    procedure EditValidate(APointValue: TXYZValue; var AValueWithError: integer); virtual;
    procedure EditPoint(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure StartDragingPoint(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);virtual;
  public
    //procedure OnTargetDraftSelectionChange(ASelectionIndeces: array of integer); virtual; abstract;
    procedure ProcessPointSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
              Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ProcessMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure ProcessMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);virtual;
    property Enabled : boolean  read FEnabled write UpdateEnabledState;
    property InDragMode: boolean read FInDragMode write UpdateDragMode;
  end;

implementation

uses
  Math,
  SysUtils,
  Messages,
  UErrorHandlingOperations;

{ TLoadCaseSeries }

constructor TLoadCaseSeries.Create;
const OPNAME = 'TLoadCaseSeries.Create';
begin
  inherited;
  try
    FReadOnlyLineSeries  := nil;
    FEditablePointSeries := nil;
    FReadOnlyPointSeries := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TYRCContainerAbstractFunctionSeries }

procedure TYRCContainerAbstractFunctionSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerAbstractFunctionSeries.CreateMemberObjects';
begin
  inherited;
  try
    FFunctionPointSeries   := TObjectList.Create(False);
    FFunctionLineSeries    := TObjectList.Create(False);
    FInDragMode            := False;
    FDragValuesIndex       := -1;
    FStartDragPointValue   := TXYZValue.Create;
    FCurrentDragPointValue := TXYZValue.Create;
    FStartDragPoint.X      := 0;
    FStartDragPoint.Y      := 0;
    FCurrentDragPoint.X    := 0;
    FCurrentDragPoint.Y    := 0;
    FCurrentTargetDraftLineSeries  := nil;
    FCurrentTargetDraftPointSeries := nil;
    FCurrentEditPointIndex         := -1;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerAbstractFunctionSeries.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FStartDragPointValue);
    FreeAndNil(FCurrentDragPointValue);
    FreeAndNil(FFunctionPointSeries);
    FreeAndNil(FFunctionLineSeries);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractFunctionSeries.FunctionSeriesIndex(ASeriesContainer: TObjectList; ASeries: TChartSeries): integer;
const OPNAME = 'TYRCContainerAbstractFunctionSeries.FunctionSeriesIndex';
var
  LCount: integer;
begin
  Result := -1;
  try
    if Assigned(ASeriesContainer) and Assigned(ASeries) then
    begin
      for LCount := 0 to ASeriesContainer.Count - 1 do
      begin
        if (TChartSeries(ASeriesContainer.Items[LCount]) = ASeries) then
        begin
          Result := LCount;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.UpdateDragMode(ADragging: boolean);
const OPNAME = 'TYRCContainerAbstractFunctionSeries.UpdateDragMode';
begin
  try
    FInDragMode := ADragging;
    FStartDragPointValue.Reset;
    FCurrentDragPointValue.Reset;
    FStartDragPoint.X      := 0;
    FStartDragPoint.Y      := 0;
    FCurrentDragPoint.X    := 0;
    FCurrentDragPoint.Y    := 0;
    FDragValuesIndex       := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.UpdateEnabledState(AEnabled: Boolean);
const OPNAME = 'TYRCContainerAbstractFunctionSeries.UpdateEnabledState';
var
  LCount: integer;
begin
  try
    UpdateDragMode(False);
    for LCount := 0 to FFunctionPointSeries.Count -1 do
       TPointSeries(FFunctionPointSeries.Items[LCount]).Active := AEnabled;
    for LCount := 0 to FFunctionLineSeries.Count -1 do
       TLineSeries(FFunctionPointSeries.Items[LCount]).Active := AEnabled;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractFunctionSeries.ChartPointToValues(AChart: TChart; APoint: TPoint; var AValues: TXYZValue): boolean;
const OPNAME = 'TYRCContainerAbstractFunctionSeries.ChartPointToValues';
begin
  Result := False;
  try
    AValues.Reset;
    if Assigned(AChart) and Assigned(AValues) then
    begin
      if Assigned(AChart.BottomAxis) and Assigned(AChart.LeftAxis) then
      begin
        AValues.XValue := AChart.BottomAxis.CalcPosPoint(APoint.X);
        AValues.YValue := AChart.LeftAxis.CalcPosPoint(APoint.Y);
        Result := True;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractFunctionSeries.ChartValuesToPoint(AChart: TChart; var APoint: TPoint; AValues: TXYZValue): boolean;
const OPNAME = 'TYRCContainerAbstractFunctionSeries.ChartValuesToPoint';

begin
  Result := False;
  try
    if Assigned(AChart) and Assigned(AValues) then
    begin
      if Assigned(AChart.BottomAxis) and Assigned(AChart.LeftAxis) then
      begin
        APoint.X := AChart.BottomAxis.CalcPosValue(AValues.XValue);
        APoint.Y := AChart.LeftAxis.CalcPosValue(AValues.YValue);
        Result := True;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.ProcessPointSeriesClick(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerAbstractFunctionSeries.ProcessPointSeriesClick';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.StartDragingPoint(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerAbstractFunctionSeries.StartDragingPoint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.ProcessMouseUp( Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X,Y: Integer);
const OPNAME = 'TYRCContainerAbstractFunctionSeries.ProcessMouseUp';
begin
  try
    if (Button = mbLeft) and FInDragMode then
      UpdateDragMode(False);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.EditValidate(APointValue: TXYZValue; var AValueWithError: integer);
const OPNAME = 'TYRCContainerAbstractFunctionSeries.EditValidate';
begin
  AValueWithError := 0;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractFunctionSeries.ProcessMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerAbstractFunctionSeries.ProcessMouseMove';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;

end;

end.
