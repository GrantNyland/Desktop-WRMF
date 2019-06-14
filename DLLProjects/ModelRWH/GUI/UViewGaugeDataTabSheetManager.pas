//
//
//  UNIT      : Contains TViewGaugeDataTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UViewGaugeDataTabSheetManager;

interface

uses
  Classes,
  UTabSheetManager;

type
  TViewGaugeDataTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  UViewGaugeDataTabSheet,
  UErrorHandlingOperations;

procedure TViewGaugeDataTabSheetManager.CreateMemberObjects;
const OPNAME = 'TViewGaugeDataTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TViewGaugeDataTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
