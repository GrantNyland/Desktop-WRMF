//
//
//  UNIT      : Contains TRainFallDataTabSheet Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 04/06/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//  comments  : Not used any where
//

unit URainfallPatchAdminSheetManager;

interface

uses
  UTabsheetManager;

type
  TRainfallPatchAdminSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
  end;

implementation

uses

   SysUtils,
   UErrorHandlingOperations,
   URainfallPatchAdminSheet;

{ TRainFallDataTabSheetManager }

procedure TRainfallPatchAdminSheetManager.DestroyMemberObjects;
const OPNAME = 'TRainfallPatchAdminSheetManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallPatchAdminSheetManager.CreateMemberObjects;
const OPNAME = 'TRainfallPatchAdminSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TRainfallPatchAdminSheet.Create(nil, fAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

