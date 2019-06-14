//
//
//  UNIT      : Contains TViewOutputTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UViewOutputTabSheetManager;

interface

uses
  Classes,
  UTabSheetManager;

type
  TViewOutputTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
  SysUtils,
  UViewOutputTabSheet,
  UErrorHandlingOperations;

procedure TViewOutputTabSheetManager.CreateMemberObjects;
const OPNAME = 'TViewOutputTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TViewOutputTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
