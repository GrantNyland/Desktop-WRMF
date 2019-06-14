//
//
//  UNIT      : Contains TViewContoursTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UViewContoursTabSheetManager;

interface

uses
  Classes,
  UTabSheetManager;

type
  TViewContoursTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  UViewContoursTabSheet,
  UErrorHandlingOperations;

procedure TViewContoursTabSheetManager.CreateMemberObjects;
const OPNAME = 'TViewContoursTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TViewContoursTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
