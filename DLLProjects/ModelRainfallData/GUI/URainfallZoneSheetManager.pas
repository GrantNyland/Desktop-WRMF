//
//
//  UNIT      : Contains TRainFallDataTabSheet Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 04/06/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//  comments  : Not used any where
//

unit URainfallZoneSheetManager;

interface
uses
  UTabsheetManager;

type
  TRainfallZoneSheetManager = class( TTabSheetManager )
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations,
  URainfallZoneSheet;

{ TRainFallDataTabSheetManager }

procedure TRainfallZoneSheetManager.DestroyMemberObjects;
const OPNAME = 'TRainfallZoneSheetManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallZoneSheetManager.CreateMemberObjects;
const OPNAME = 'TRainfallZoneSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TRainfallZoneSheet.Create(nil, fAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

