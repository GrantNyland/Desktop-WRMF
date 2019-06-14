unit UModelYRCTabSheetManager;

interface
uses
  UTabsheetManager,
  UModelYRCTabSheet;
type
  TModelYRCTabSheetManager = class ( TTabSheetManager )
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
end;

implementation
uses
  // Delphi
  SysUtils,
  // Arivia.kom
  UErrorHandlingOperations;


{ TModelYRCTabSheetManager }

procedure TModelYRCTabSheetManager.CreateMemberObjects;
const OPNAME = 'TModelYRCTabSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TModelYRCTabSheet.Create ( nil, AppModules );
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelYRCTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TModelYRCTabSheetManager.DestroyMemberObjects';
begin
 inherited DestroyMemberObjects;
  try
    if Assigned(FTabSheet) then
      FreeAndNil(FTabSheet);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelYRCTabSheetManager.Initialise: boolean;
const OPNAME = 'TModelYRCTabSheetManager.Initialise';
begin
  Result := True;
  try
  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
 