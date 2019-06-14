unit URWHGaugeSelectionTabSheetManager;
//
//
//  UNIT      : Contains TRWHGaugeSelectionTabSheetManager Class
//  AUTHOR    : Sam Dhlamini
//  DATE      :
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

interface

uses
  Classes,
  UTabSheetManager;

type
  TRWHGaugeSelectionTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  URWHGaugeSelectionTabSheet,
  UErrorHandlingOperations;
  {TRWHGaugeSelectionTabSheetManager}
procedure TRWHGaugeSelectionTabSheetManager.CreateMemberObjects;
const OPNAME = 'TRWHGaugeSelectionTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TRWHGaugeSelectionTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
