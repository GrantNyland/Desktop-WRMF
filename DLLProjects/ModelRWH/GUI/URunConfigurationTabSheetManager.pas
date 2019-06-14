//
//
//  UNIT      : Contains TRunModelTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit URunConfigurationTabSheetManager;

interface

uses
  Classes,
  UTabSheetManager;

type
  TRunConfigurationTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  URunConfigurationTabSheet,
  UErrorHandlingOperations;

procedure TRunConfigurationTabSheetManager.CreateMemberObjects;
const OPNAME = 'TRunModelTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TRunConfigurationTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
