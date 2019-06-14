{******************************************************************************}
{*  UNIT      : Contains TPatchRTabSheetManager Class                         *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 12/10/2004                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UPatchRTabSheetManager;

interface
uses
  UTabsheetManager,
  UAbstractObject;

type
  TPatchRTabSheetManager = class(TTabSheetManager)
  protected
    procedure CreateMemberObjects; override;
  public
  end;

implementation

uses
   SysUtils,
   UErrorHandlingOperations,
   UPatchRTabSheet;

{ TPatchRTabSheetManager }

procedure TPatchRTabSheetManager.CreateMemberObjects;
const OPNAME = 'TPatchRTabSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TPatchRTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
