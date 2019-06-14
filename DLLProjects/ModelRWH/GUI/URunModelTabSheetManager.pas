//
//
//  UNIT      : Contains TRunModelTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit URunModelTabSheetManager;

interface

uses
  Classes,
  UTabSheetManager;

type
  TRunModelTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  URunModelTabSheet,
  UErrorHandlingOperations;

procedure TRunModelTabSheetManager.CreateMemberObjects;
const OPNAME = 'TRunModelTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TRunModelTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
