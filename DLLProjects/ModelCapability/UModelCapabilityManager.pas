//
//
//  UNIT      : Contains TModelCapabilityManager Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2004/12/22
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UModelCapabilityManager;

interface

uses
  VCL.ComCtrls,
  UTabSheetManager,
  UAbstractModelObjects;

type
  TModelCapabilityManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  end;

implementation

uses
  SysUtils,
  UModelCapabilityTabSheet,
  UErrorHandlingOperations;

procedure TModelCapabilityManager.CreateMemberObjects;
const OPNAME = 'TModelCapabilityManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TModelCapabilityTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
