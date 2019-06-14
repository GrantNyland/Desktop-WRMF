{******************************************************************************}
{*  UNIT      : Contains the class TSelectChannelDialog.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/05/14                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USelectChannelDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Forms,
  Types,
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UDataComponent;

type

  TSelectChannelDialog = class(TAbstractScrollablePanel)
  private
    { Private declarations }
  protected
    FHeadingLabel    : TLabel;
    FChannelsListBox : TFieldListBox;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    property ChannelsListBox : TFieldListBox read FChannelsListBox;
  end;

  implementation

uses
  SysUtils,
  Windows,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TSelectChannelDialog                                                       *}
{******************************************************************************}

procedure TSelectChannelDialog.CreateMemberObjects;
const OPNAME = 'TSelectChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FHeadingLabel := CreateFieldLabel(lOwner, lParent,  10,   5, 200,  21);
    FHeadingLabel.Alignment := taCenter;
    FChannelsListBox := TFieldListBox.Create(lOwner, FAppModules);
    with FChannelsListBox do
    begin
      Parent     := lParent;
      Top        := 25;
      Left       := 10;
      Width      := 250;
      Height     := 300;
      TabStop    := TRUE;
      TabOrder   := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelDialog.DestroyMemberObjects;
const OPNAME = 'TSelectChannelDialog.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited DestroyMemberObjects;
end;

function TSelectChannelDialog.Initialise: boolean;
const OPNAME = 'TSelectChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelDialog.Resize;
const OPNAME = 'TSelectChannelDialog.Resize';
begin
  inherited Resize;
  try
    FChannelsListBox.Height  := Self.Height - 60;
    FChannelsListBox.Width   := Self.Width  - 20;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSelectChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FHeadingLabel.Caption := FAppModules.Language.GetString('TField.Description');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelDialog.SaveState: boolean;
const OPNAME = 'TSelectChannelDialog.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelDialog.RestoreColourState;
const OPNAME = 'TSelectChannelDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelDialog.AssignHelpContext;
const OPNAME = 'TSelectChannelDialog.AssignHelpContext';
begin
  try
//    SetControlHelpContext(FChannelsListBox,         HC_SelectChannelNames);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

