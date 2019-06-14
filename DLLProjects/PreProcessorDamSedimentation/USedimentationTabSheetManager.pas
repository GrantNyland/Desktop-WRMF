unit USedimentationTabSheetManager;

interface

uses
  VCL.ComCtrls,
  VCL.Controls,
  VCL.ExtCtrls,
  UDataViewerManager,
  UTabSheetManager,
  UGenericModelLinkClasses,
  UAbstractModelObjects;

type
  TSedimentationTabSheetManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: Boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;override;

  end;

implementation

uses
  SysUtils,
  USedimentationTabSheet,
  UErrorHandlingOperations, UAbstractObject;

procedure TSedimentationTabSheetManager.CreateMemberObjects;
const OPNAME = 'TSedimentationTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TSedimentationTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSedimentationTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TSedimentationTabSheetManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TSedimentationTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationTabSheetManager.Initialise: Boolean;
const OPNAME = 'TSedimentationTabSheetManager.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
