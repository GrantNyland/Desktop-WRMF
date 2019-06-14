//
//
//  UNIT      : Contains TOutputComparisonManager Class
//  AUTHOR    : Sam Dhlamini (ARIVIA)
//  DATE      : 2007/06/11
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UOutputComparisonManager;

interface

uses
  VCL.ComCtrls,
  UDataViewerManager,
  UGenericModelLinkClasses,
  UAbstractModelObjects;

type
  TOutputComparisonManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
  public
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
  end;

implementation

uses
  SysUtils,
  UOutputComparisonSheet,
  UErrorHandlingOperations;

procedure TOutputComparisonManager.CreateMemberObjects;
const OPNAME = 'TOutputComparisonManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TOutputComparisonSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComparisonManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TOutputComparisonManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if Assigned(ACustomModelEvent) then
    begin
      Result := True;
      case ACustomModelEvent.Action of
        meOutputComparitorShowChartLegendDialog : TOutputComparisonSheet(FTabSheet).ShowChartLegendDialog;
      else
        Result := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
