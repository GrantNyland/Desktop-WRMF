//
//
//  UNIT      : Contains TViewOutputGraphManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewOutputGraphManager;

interface

uses
  UDataViewerManager;

type
  TViewOutputGraphManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
  end;

implementation

uses
  SysUtils,
  UOutputGraphSheet,
  UErrorHandlingOperations;

procedure TViewOutputGraphManager.CreateMemberObjects;
const OPNAME = 'TViewOutputGraphManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TOutputGraphSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
