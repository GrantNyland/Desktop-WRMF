//
//
//  UNIT      : Contains TGridOutputEditorManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/11/13
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridOutputEditorManager;

interface

uses
  VCL.ComCtrls,
  UDataViewerManager,
  UAbstractModelObjects;

type
  TGridOutputEditorManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
  end;

implementation

uses
  SysUtils,
  UGridOutputEditorSheet,
  UErrorHandlingOperations;

procedure TGridOutputEditorManager.CreateMemberObjects;
const OPNAME = 'TGridOutputEditorManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TGridOutputEditorSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
