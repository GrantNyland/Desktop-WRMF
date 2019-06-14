//
//
//  UNIT      : Contains TViewDataGraphManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewDataGraphManager;

interface

uses
  UDataViewerManager;

type
  TViewDataGraphManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
  end;

implementation

uses
  SysUtils,
  UGraphSheet,
  UErrorHandlingOperations;

procedure TViewDataGraphManager.CreateMemberObjects;
const OPNAME = 'TViewDataGraphManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TGraphSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
