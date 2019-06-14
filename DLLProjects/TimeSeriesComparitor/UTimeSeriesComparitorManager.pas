//
//
//  UNIT      : Contains TTimeSeriesComparitorViewManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorManager;

interface

uses
  UGenericModelLinkClasses,
  UDataViewerManager;

type
  TTimeSeriesComparitorManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

  public
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
  end;

implementation

uses
  SysUtils,
  UGridActionObject,
  UTimeSeriesComparitorSheet,
  UErrorHandlingOperations, UAbstractObject, UTabSheetManager;

procedure TTimeSeriesComparitorManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TTimeSeriesComparitorSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
procedure TTimeSeriesComparitorManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FTabSheet);
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TTimeSeriesComparitorManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if Assigned(ACustomModelEvent) then
    begin
      Result := True;
      case ACustomModelEvent.Action of
        meCreateChart               : TTimeSeriesComparitorSheet(FTabSheet).CreateChart;
        meRenameChart               : TTimeSeriesComparitorSheet(FTabSheet).RenameChart;
        meDeleteChart               : TTimeSeriesComparitorSheet(FTabSheet).DeleteChart;
        meCreateView                : TTimeSeriesComparitorSheet(FTabSheet).CreateView;
        meRenameView                : TTimeSeriesComparitorSheet(FTabSheet).RenameView;
        meDeleteView                : TTimeSeriesComparitorSheet(FTabSheet).DeleteView;
        meAddChart                  : TTimeSeriesComparitorSheet(FTabSheet).AddChartToView;
        meRemoveChart               : TTimeSeriesComparitorSheet(FTabSheet).RemoveChartFromView;
        meChartName                 : TTimeSeriesComparitorSheet(FTabSheet).SetChartName;
        meAddSeries                 : TTimeSeriesComparitorSheet(FTabSheet).AddSeriesToChart;
        meRemoveSeries              : TTimeSeriesComparitorSheet(FTabSheet).RemoveSeriesFromChart;
        meSeriesColor               : TTimeSeriesComparitorSheet(FTabSheet).SetSeriesColor;
        meSaveView                  : TTimeSeriesComparitorSheet(FTabSheet).SaveView;
        meTSCToggleIndividualSeries : TTimeSeriesComparitorSheet(FTabSheet).TSCToggleIndividualSeries;
        meShowChartLegendDialog     : TTimeSeriesComparitorSheet(FTabSheet).ShowChartLegendDialog;
      else
        Result := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TTimeSeriesComparitorManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := TTimeSeriesComparitorSheet(FTabSheet).ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
