{******************************************************************************}
{* UNIT : Contains the class UHydrologyModelToolbar.                  *}
{******************************************************************************}

unit UHydrologyModelToolbar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  THydrologyModelToolbar = class(TChildToolBar)
  protected
    FGridTabSelected             : Boolean;
    FValidateFiles               : TAbstractSpeedButton;

    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnValidateFilesClick(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetValidateFilesState(AState: boolean);

    procedure TabHasChanged(AGridTabSelected: boolean);
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure THydrologyModelToolbar.CreateMemberObjects;
const OPNAME = 'THydrologyModelToolbar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGridTabSelected := False;
    FValidateFiles := CreateButton('ValidateFiles');
    FValidateFiles.OnClick := OnValidateFilesClick;
    FValidateFiles.Enabled := False;

    SetHorizontalPositions;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelToolbar.AssignHelpContext;
const OPNAME = 'THydrologyModelToolbar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FValidateFiles,HC_ValidateButton);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyModelToolbar.SetHorizontalPositions;
const OPNAME = 'THydrologyModelToolbar.SetHorizontalPositions';
var
 LButtonCount,
 LGaps :integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;

    SetButtonHorizontalPosition(FValidateFiles,           True, False, LButtonCount, LGaps);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModelToolbar.LanguageHasChanged: boolean;
const OPNAME = 'THydrologyModelToolbar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FValidateFiles.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelToolbar.SetValidateFilesState(AState: boolean);
const OPNAME = 'THydrologyModelToolbar.SetValidateFilesState';
begin
  try
    SetButtonEnabled(FValidateFiles, AState, 'ValidateFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelToolbar.OnValidateFilesClick(Sender: TObject);
const OPNAME = 'THydrologyModelToolbar.OnValidateFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeValidateFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelToolbar.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'THydrologyModelToolbar.TabHasChanged';
begin
  try
    FGridTabSelected := AGridTabSelected;
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
