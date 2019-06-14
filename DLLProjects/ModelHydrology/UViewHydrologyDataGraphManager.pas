//
//
//  UNIT      : Contains TViewDataGraphManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewHydrologyDataGraphManager;

interface

uses
  UDataViewerManager;

type
  TViewHydrologyDataGraphManager = class( TDataViewerManager )
  protected
    procedure CreateMemberObjects; override;
  end;

implementation

uses
  SysUtils,
  UHydrologyGraphSheet,
  UErrorHandlingOperations;

procedure TViewHydrologyDataGraphManager.CreateMemberObjects;
const OPNAME = 'TViewHydrologyDataGraphManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := THydrologyGraphSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
