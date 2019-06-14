//
//
//  UNIT      : Contains TYieldReliabilityCurveManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/08/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldReliabilityCurveManager;

interface

uses
  VCL.Menus,
  UAbstractObject,
  UGenericModelLinkClasses,
  UTabSheetManager;

type
  TYieldReliabilityCurveManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
  end;

implementation

uses
  SysUtils,
  UYRCMainSheet,
  UErrorHandlingOperations;

procedure TYieldReliabilityCurveManager.CreateMemberObjects;
const OPNAME = 'TYieldReliabilityCurveManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TYRCMainSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldReliabilityCurveManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TYieldReliabilityCurveManager.DoCustomTabSheetEvent';
begin
  Result := FALSE;
  try
    Result := TYRCMainSheet(FTabSheet).DoCustomTabSheetEvent(ACustomModelEvent);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldReliabilityCurveManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TYieldReliabilityCurveManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := TYRCMainSheet(FTabSheet).ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
