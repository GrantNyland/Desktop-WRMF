//
//
//  UNIT      : Contains TYRCContainerFunctionSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerFunctionSeries;

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
  UAbstractYRCData,
  UAbstractObject,
  UYRCContainerAbstractSeries,
  UYRCContainerAbstractFunctionSeries,
  UYRCContainerDeterministicFunctionSeries,
  UYRCContainerRegressionFunctionSeries;

type

  TYRCContainerFunctionSeries = class(TYRCContainerAbstractSeries)
  protected
    FDeterministicFunctionSeries: TYRCContainerDeterministicFunctionSeries;
    FRegressionFunctionSeries: TYRCContainerRegressionFunctionSeries;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure UpdateGraphMode(AGraphMode:TChartMode);
    function InitialiseSeries: boolean; override;

    procedure OnSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
              Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
    procedure OnTargetDraftSelectionChange(ASelectionIndeces: array of integer); override;
    procedure OnChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    property GraphMode : TChartMode  read FChartMode write UpdateGraphMode;
  end;

implementation

uses
  Math,
  SysUtils,
  Messages,
  UErrorHandlingOperations;

{ TYRCContainerFunctionSeries }

procedure TYRCContainerFunctionSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerFunctionSeries.CreateMemberObjects';
begin
  inherited;
  try
    FChartMode := cmPlane;
    FDeterministicFunctionSeries := TYRCContainerDeterministicFunctionSeries.Create(FAppModules);
    FRegressionFunctionSeries    := TYRCContainerRegressionFunctionSeries.Create(FAppModules);
    ProcessSeriesClickMessage    := OnSeriesClick;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerFunctionSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerFunctionSeries.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FDeterministicFunctionSeries);
  FreeAndNil(FRegressionFunctionSeries);
end;

procedure TYRCContainerFunctionSeries.UpdateGraphMode(AGraphMode: TChartMode);
const OPNAME = 'TYRCContainerFunctionSeries.UpdateGraphMode';
begin
  try
    case AGraphMode of
      cmPlane:
        begin
          FRegressionFunctionSeries.Enabled := False;
          FDeterministicFunctionSeries.Enabled := False;
        end;
      cmDeterministic:
        begin
          FRegressionFunctionSeries.Enabled := False;
          FDeterministicFunctionSeries.Enabled := True;
        end;
      cmRegretion:
        begin
          FRegressionFunctionSeries.Enabled := False;
          FDeterministicFunctionSeries.Enabled := True;
        end;
    end;//case
    FChartMode := AGraphMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerFunctionSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerFunctionSeries.ShowSeries';
begin
  Result := inherited ShowSeries(AOwner,AYRCDataObject);;
  try
    Result := Result and
              FRegressionFunctionSeries.ShowSeries(AOwner,AYRCDataObject) and
              FDeterministicFunctionSeries.ShowSeries(AOwner,AYRCDataObject);
    UpdateGraphMode(FChartMode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerFunctionSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerFunctionSeries.InitialiseSeries';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerFunctionSeries.OnSeriesClick(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer;
          Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerFunctionSeries.OnSeriesClick';
begin
  try
    case FChartMode of
      cmDeterministic:
        FDeterministicFunctionSeries.ProcessPointSeriesClick(Sender,Series,ValueIndex,Button,Shift,X, Y);
      cmRegretion    :
       FRegressionFunctionSeries.ProcessPointSeriesClick(Sender,Series,ValueIndex,Button,Shift,X, Y);
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCContainerFunctionSeries.OnTargetDraftSelectionChange(ASelectionIndeces: array of integer);
const OPNAME = 'TYRCContainerFunctionSeries.OnTargetDraftSelectionChange';
begin
  inherited OnTargetDraftSelectionChange(ASelectionIndeces);
  try
    case FChartMode of
      cmDeterministic:
        FDeterministicFunctionSeries.OnTargetDraftSelectionChange(ASelectionIndeces);
      cmRegretion    :
       FRegressionFunctionSeries.OnTargetDraftSelectionChange(ASelectionIndeces);
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerFunctionSeries.OnChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerFunctionSeries.OnChartMouseUp';
begin
  try
    case FChartMode of
      cmDeterministic:
        FDeterministicFunctionSeries.ProcessMouseUp(Sender,Button, Shift, X, Y);
      cmRegretion    :
       FRegressionFunctionSeries.ProcessMouseUp(Sender,Button, Shift, X, Y);
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerFunctionSeries.OnChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYRCContainerFunctionSeries.OnChartMouseMove';
begin
  try
    case FChartMode of
      cmDeterministic:
        FDeterministicFunctionSeries.ProcessMouseMove(Sender, Shift, X, Y);
      cmRegretion    :
       FRegressionFunctionSeries.ProcessMouseMove(Sender, Shift, X, Y);
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
