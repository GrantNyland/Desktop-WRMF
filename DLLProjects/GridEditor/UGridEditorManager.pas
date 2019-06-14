//
//
//  UNIT      : Contains TGridEditorManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/11/13
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorManager;

interface

uses
  vcl.ComCtrls,
  UDataViewerManager,
  UAbstractModelObjects;

type
  TGridEditorManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
  end;

implementation

uses
  SysUtils,
  UGridEditorSheet,
  UErrorHandlingOperations;

procedure TGridEditorManager.CreateMemberObjects;
const OPNAME = 'TGridEditorManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TGridEditorSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
