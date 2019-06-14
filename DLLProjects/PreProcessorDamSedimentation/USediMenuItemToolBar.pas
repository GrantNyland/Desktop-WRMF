unit USediMenuItemToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TSediMenuItemToolBar = class(TChildToolBar)
  protected
    FCreateDamSedimentationBtn : TAbstractSpeedButton;
    FDeleteDamSedimentationBtn : TAbstractSpeedButton;
    FSaveDamSedimentationBtn   : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnClickCreateDamSedimentation(Sender: TObject);
    procedure OnClickDeleteDamSedimentation(Sender: TObject);
    procedure OnClickSaveDamSedimentation(Sender: TObject);
  public
    function LanguageHasChanged: boolean; override;
    procedure SetCreateDamSedimentation(AEnabled: boolean);
    procedure SetDeleteDamSedimentation(AEnabled: boolean);
    procedure SetSaveDamSedimentation(AEnabled: boolean);

    property CreateDamSedimentationBtn : TAbstractSpeedButton read FCreateDamSedimentationBtn;
    property DeleteDamSedimentationBtn : TAbstractSpeedButton read FDeleteDamSedimentationBtn;
    property SaveDamSedimentationBtn   : TAbstractSpeedButton read FSaveDamSedimentationBtn;
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TSediMenuItemToolBar.CreateMemberObjects;
const OPNAME = 'TSediMenuItemToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCreateDamSedimentationBtn   := CreateButton('CreateDamSedimentation');
    FDeleteDamSedimentationBtn   := CreateButton('DeleteDamSedimentation');
    FSaveDamSedimentationBtn     := CreateButton('SaveDamSedimentation');

    FCreateDamSedimentationBtn.OnClick  := OnClickCreateDamSedimentation;
    FDeleteDamSedimentationBtn.OnClick  := OnClickDeleteDamSedimentation;
    FSaveDamSedimentationBtn.OnClick  := OnClickSaveDamSedimentation;
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSediMenuItemToolBar.SetHorizontalPositions;
const OPNAME = 'TSediMenuItemToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FCreateDamSedimentationBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteDamSedimentationBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FSaveDamSedimentationBtn, True,  False, LButtonCount, LGaps);
    Width := FSaveDamSedimentationBtn.Left + FSaveDamSedimentationBtn.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSediMenuItemToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TSediMenuItemToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FCreateDamSedimentationBtn.LanguageHasChanged;
    FDeleteDamSedimentationBtn.LanguageHasChanged;
    FSaveDamSedimentationBtn.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSediMenuItemToolBar.OnClickCreateDamSedimentation(Sender: TObject);
const OPNAME = 'TSediMenuItemToolBar.OnClickCreateDamSedimentation';
begin
  try
    FAppModules.Model.ProcessEvent(cmeCreateDamSedimentation, nil);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSediMenuItemToolBar.OnClickDeleteDamSedimentation(Sender: TObject);
const OPNAME = 'TSediMenuItemToolBar.OnClickDeleteDamSedimentation';
begin
  try
    FAppModules.Model.ProcessEvent(cmeDeleteDamSedimentation, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSediMenuItemToolBar.OnClickSaveDamSedimentation(Sender: TObject);
const OPNAME = 'TSediMenuItemToolBar.OnClickSaveDamSedimentation';
begin
  try
    FAppModules.Model.ProcessEvent(cmeSaveDamSedimentation, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TSediMenuItemToolBar.SetCreateDamSedimentation(AEnabled: boolean);
const OPNAME = 'TSediMenuItemToolBar.SetCreateDamSedimentation';
begin
  try
    SetButtonEnabled(FCreateDamSedimentationBtn, AEnabled, 'CreateDamSedimentationDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSediMenuItemToolBar.SetDeleteDamSedimentation(AEnabled: boolean);
const OPNAME = 'TSediMenuItemToolBar.SetDeleteDamSedimentation';
begin
  try
    SetButtonEnabled(FDeleteDamSedimentationBtn, AEnabled, 'DeleteDamSedimentationDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSediMenuItemToolBar.SetSaveDamSedimentation(AEnabled: boolean);
const OPNAME = 'TSediMenuItemToolBar.SetSaveDamSedimentation';
begin
  try
    SetButtonEnabled(FSaveDamSedimentationBtn, AEnabled, 'SaveDamSedimentationDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
