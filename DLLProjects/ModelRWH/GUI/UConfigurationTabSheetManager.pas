//
//
//  UNIT      : Contains TRunModelTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UConfigurationTabSheetManager;

interface

uses
  Classes,
  UTabSheetManager;

type
  TConfigurationTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  UConfigurationTabSheet,
  UErrorHandlingOperations;

procedure TConfigurationTabSheetManager.CreateMemberObjects;
const OPNAME = 'TRunModelTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TConfigurationTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
