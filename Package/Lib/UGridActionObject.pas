//
//
//  UNIT      : Contains TGridActionObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridActionObject;

interface

uses
  Classes;

type
  //TGridAction = (gaAdd, gaDelete, gaEdit, gaView);
  //TTimeSeriesComparitorActions = (taAddSeries, taRemoveSeries);
  //TYRCActions = (raResetChartData, raTogglePlaneMode, raToggleChartMode, raSaveChart);

  TGridAction = (gaAdd, gaDelete, gaEdit, gaView); //General
  TGridActionObject = class(TObject)
  protected
    FAction: TGridAction;
    FDatasetID: string;
    FDatasetProperties: TStringList;
  public
    constructor Create(AAction:TGridAction);
    destructor Destroy; override;
    property Action: TGridAction read FAction write FAction;
    property DatasetID: string read FDatasetID write FDatasetID;
    property DatasetProperties: TStringList read FDatasetProperties write FDatasetProperties;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TGridActionObject }

constructor TGridActionObject.Create(AAction:TGridAction);
const OPNAME = 'TGridActionObject.Create';
begin
  inherited Create;
  try
    FAction := AAction;
    FDatasetProperties := TStringList.Create
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TGridActionObject.Destroy;
const OPNAME = 'TGridActionObject.Destroy';
begin
  inherited;
  try
    FreeAndNil(FDatasetProperties);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
