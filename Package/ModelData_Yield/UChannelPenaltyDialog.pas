{******************************************************************************}
{*  UNIT      : Contains the class TChannelPenaltyDialog.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UChannelPenaltyDialog;

interface

uses
  Classes,
  System.Contnrs,
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
  UDataEditComponent,
  UDataComponent,
  UWRMFThemes;

type
  TChannelPenaltyDialog = class(TAbstractScrollablePanel)
  private
    { Private declarations }
    FPenaltyPanels : TObjectList;
    FActivePanel   : TChannelPenaltyPanel;
    procedure SetActivePanel (AActivePanel : TChannelPenaltyPanel);
    procedure ScrollToActivePanel;
  protected
    FHeadingPenaltiesLabel   : TLabel;
    FHeadingChannelsLabel    : TLabel;
    FPenaltyIDPanel          : TPanel;
    FDescriptionPanel        : TPanel;
    FArc1Panel               : TPanel;
    FArc2Panel               : TPanel;
    FArc3Panel               : TPanel;
    FArc4Panel               : TPanel;
    FArc5Panel               : TPanel;
    FPenaltyScrollBox        : TScrollBox;
    FChannelsListBox         : TFieldListBox;
    FNewPenaltyButton        : TFieldButton;
    FDeletePenaltyButton     : TFieldButton;
    FHighlightSelectedPanel  : boolean;
    FInflowPenaltyNolbl      : TLabel;
    FInflowPenaltyNocmbox    : TFieldComboBox;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure OnUpDownKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function AddChannelPenaltyPanel : TChannelPenaltyPanel;
    procedure MoveUp (Sender : TObject);
    procedure MoveDown (Sender : TObject);
    property PenaltyPanels : TObjectList read FPenaltyPanels;
    property ActivePanel   : TChannelPenaltyPanel read FActivePanel write SetActivePanel;
    property PenaltyScrollBox : TScrollBox read FPenaltyScrollBox;
    property HeadingPenaltiesLabel : TLabel read FHeadingPenaltiesLabel;
    property HeadingChannelsLabel  : TLabel read FHeadingChannelsLabel;
    property ChannelsListBox : TFieldListBox read FChannelsListBox;
    property NewPenaltyButton : TFieldButton read FNewPenaltyButton;
    property DeletePenaltyButton : TFieldButton read FDeletePenaltyButton;
    property InflowPenaltyNolbl      : TLabel read FInflowPenaltyNolbl;
    property InflowPenaltyNocmbox    : TFieldComboBox read FInflowPenaltyNocmbox;
    property HighlightSelectedPanel   : boolean read FHighlightSelectedPanel write FHighlightSelectedPanel;
  end;

  implementation

uses
  SysUtils,
  Windows,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TChannelPenaltyDialog                                                      *}
{******************************************************************************}

procedure TChannelPenaltyDialog.CreateMemberObjects;
const OPNAME = 'TChannelPenaltyDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    FHighlightSelectedPanel := False;
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FHeadingPenaltiesLabel := CreateFieldLabel(lOwner, lParent,  10,   5, 512,  21);
    FHeadingPenaltiesLabel.Alignment := taCenter;
    FHeadingChannelsLabel  := CreateFieldLabel(lOwner, lParent, 525,   5, 175,  21);
    FHeadingChannelsLabel.AutoSize := TRUE;
    FPenaltyIDPanel        := CreatePanel     (lOwner, lParent,  10,  25,  47,  22, 0);
    FDescriptionPanel      := CreatePanel     (lOwner, lParent,  55,  25, 189,  22, 1);
    FArc1Panel             := CreatePanel     (lOwner, lParent, 242,  25,  53,  22, 2);
    FArc2Panel             := CreatePanel     (lOwner, lParent, 293,  25,  53,  22, 2);
    FArc3Panel             := CreatePanel     (lOwner, lParent, 344,  25,  53,  22, 2);
    FArc4Panel             := CreatePanel     (lOwner, lParent, 395,  25,  53,  22, 2);
    FArc5Panel             := CreatePanel     (lOwner, lParent, 446,  25,  53,  22, 2);
    with FPenaltyIDPanel do
    begin
      BevelInner  := bvRaised;
      BevelOuter  := bvNone;
      BorderStyle := bsSingle;
    end;
    with FDescriptionPanel do
    begin
      BevelInner  := bvRaised;
      BevelOuter  := bvNone;
      BorderStyle := bsSingle;
    end;
    with FArc1Panel do
    begin
      BevelInner  := bvRaised;
      BevelOuter  := bvNone;
      BorderStyle := bsSingle;
    end;
    with FArc2Panel do
    begin
      BevelInner  := bvRaised;
      BevelOuter  := bvNone;
      BorderStyle := bsSingle;
    end;
    with FArc3Panel do
    begin
      BevelInner  := bvRaised;
      BevelOuter  := bvNone;
      BorderStyle := bsSingle;
    end;
    with FArc4Panel do
    begin
      BevelInner  := bvRaised;
      BevelOuter  := bvNone;
      BorderStyle := bsSingle;
    end;
    with FArc5Panel do
    begin
      BevelInner  := bvRaised;
      BevelOuter  := bvNone;
      BorderStyle := bsSingle;
    end;
    FPenaltyScrollBox        := TScrollBox.Create(lOwner);
    with FPenaltyScrollBox do
    begin
      Parent     := lParent;
      Top        := 45;
      Left       := 10;
      Width      := 520;
      Height     := 335;
      Parentfont := FALSE;
      TabStop    := TRUE;
      TabOrder   := 0;
    end;
    FNewPenaltyButton    := CreateFieldButton  (FAppModules, lOwner, lParent, 10,  10,  75, 25, 3, FALSE, 'New');
    FDeletePenaltyButton := CreateFieldButton  (FAppModules, lOwner, lParent, 95,  10,  75, 25, 3, FALSE, 'Delete');

    FInflowPenaltyNolbl      := CreateFieldLabel   (lOwner, lParent,  115,  10, 50, 21);
    FInflowPenaltyNocmbox    := CreateFieldComboBox(FAppModules, lOwner, lParent, 155,  10, 165, 21, 3,   True, csDropDownList);

    FChannelsListBox := TFieldListBox.Create(lOwner, FAppModules);
    with FChannelsListBox do
    begin
      Parent     := lParent;
      Top        := 25;
      Left       := 535;
      Width      := 155;
      Height     := 355;
      TabStop    := FALSE;
      TabOrder   := 10;
    end;
    FPenaltyPanels := TObjectList.Create(True);
    FActivePanel   := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyDialog.DestroyMemberObjects;
const OPNAME = 'TChannelPenaltyDialog.DestroyMemberObjects';
begin
  try
    FPenaltyPanels.Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited DestroyMemberObjects;
end;

procedure TChannelPenaltyDialog.MoveUp (Sender : TObject);
const OPNAME = 'TChannelPenaltyDialog.MoveUp';
var
  lPanel  : TChannelPenaltyPanel;
  lIndexA : integer;
begin
  try
    lIndexA := FPenaltyPanels.IndexOf(FActivePanel);
    if (lIndexA > 0) then
    begin
      lPanel := TChannelPenaltyPanel(FPenaltyPanels.Items[lIndexA - 1]);
      if (Sender.ClassName = 'TFieldEdit') then
        lPanel.DescriptionEdit.SetFocus
      else
        lPanel.ArcPenaltiesGrid.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyDialog.MoveDown (Sender : TObject);
const OPNAME = 'TChannelPenaltyDialog.MoveDown';
var
  lPanel  : TChannelPenaltyPanel;
  lIndexA : integer;
begin
  try
    lIndexA := FPenaltyPanels.IndexOf(FActivePanel);
    if (lIndexA < (FPenaltyPanels.Count - 1)) then
    begin
      lPanel := TChannelPenaltyPanel(FPenaltyPanels.Items[lIndexA + 1]);
      if (Sender.ClassName = 'TFieldEdit') then
        lPanel.DescriptionEdit.SetFocus
      else
        lPanel.ArcPenaltiesGrid.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyDialog.Initialise: boolean;
const OPNAME = 'TChannelPenaltyDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FInflowPenaltyNolbl.Visible      := (FAppModules.StudyArea.ModelCode = CPlanning);
    FInflowPenaltyNocmbox.Visible    := (FAppModules.StudyArea.ModelCode = CPlanning);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyDialog.Resize;
const OPNAME = 'TChannelPenaltyDialog.Resize';
var
  lCenter : integer;
begin
  inherited Resize;
  try
    FPenaltyScrollBox.Height := Self.Height - (80 + 25);
    FChannelsListBox.Height  := Self.Height - (60 + 25);
    FNewPenaltyButton.Top    := Self.Height - 55;
    FDeletePenaltyButton.Top := Self.Height - 55;
    if (FInflowPenaltyNolbl <> nil) and (FInflowPenaltyNocmbox <> nil) then
    begin
      FInflowPenaltyNolbl.AutoSize := True;
      FInflowPenaltyNolbl.Top      := Self.Height - 45;
      FInflowPenaltyNolbl.Left     := FDeletePenaltyButton.Left + FDeletePenaltyButton.Width +C_ControlOffset;
      FInflowPenaltyNolbl.Caption  := 'Penalty structure type for nature inflows:';

      FInflowPenaltyNocmbox.Top    := Self.Height - 50;
      FInflowPenaltyNocmbox.Left   := FInflowPenaltyNolbl.Left + FInflowPenaltyNolbl.Width +C_LabelOffset;
    end;
    FChannelsListBox.Width   := Self.Width  - 545;
    if (FChannelsListBox.Width >= FHeadingChannelsLabel.Width) then
    begin
      lCenter := FChannelsListBox.Left +  (FChannelsListBox.Width div 2);
      FHeadingChannelsLabel.Left := lCenter - (FHeadingChannelsLabel.Width div 2);
    end
    else
    begin
      FHeadingChannelsLabel.Left := FChannelsListBox.Left  +
                                    FChannelsListBox.Width -
                                    FHeadingChannelsLabel.Width;
    end;
    ScrollToActivePanel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyDialog.LanguageHasChanged: boolean;
const OPNAME = 'TChannelPenaltyDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FPenaltyIDPanel.Caption         := FAppModules.Language.GetString('TField.Id');
    FDescriptionPanel.Caption       := FAppModules.Language.GetString('TField.Description');
    FArc1Panel.Caption              := FAppModules.Language.GetString('Channel.Arc') + ' 1';
    FArc2Panel.Caption              := FAppModules.Language.GetString('Channel.Arc') + ' 2';
    FArc3Panel.Caption              := FAppModules.Language.GetString('Channel.Arc') + ' 3';
    FArc4Panel.Caption              := FAppModules.Language.GetString('Channel.Arc') + ' 4';
    FArc5Panel.Caption              := FAppModules.Language.GetString('Channel.Arc') + ' 5';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyDialog.SaveState: boolean;
const OPNAME = 'TChannelPenaltyDialog.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyDialog.RestoreColourState;
const OPNAME = 'TChannelPenaltyDialog.RestoreColourState';
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

function TChannelPenaltyDialog.AddChannelPenaltyPanel : TChannelPenaltyPanel;
const OPNAME = 'TChannelPenaltyDialog.AddChannelPenaltyPanel';
var
  nCount : integer;
  pPanel : TChannelPenaltyPanel;
begin
  nCount := FPenaltyPanels.Count;
  pPanel := TChannelPenaltyPanel.Create(Self, FAppModules);
  pPanel.DescriptionEdit.OnKeyDown := OnUpDownKeyDown;
  pPanel.ArcPenaltiesGrid.OnKeyDown := OnUpDownKeyDown;

  with pPanel do
  begin
    Parent   := FPenaltyScrollBox;
    Left     := 0;
    Top      := nCount * 35;
    Width    := 500;
    Height   := 35;
    TabOrder := nCount;
    IDlabel.Caption := IntToStr(nCount + 1);
  end;
  FPenaltyPanels.Add(pPanel);
  Result := pPanel;
end;


procedure TChannelPenaltyDialog.SetActivePanel (AActivePanel : TChannelPenaltyPanel);
const OPNAME = 'TChannelPenaltyDialog.SetActivePanel';
begin
  try
    if HighlightSelectedPanel  and (FActivePanel <> nil) and (FPenaltyPanels.IndexOf(FActivePanel) >= 0) then
    begin
      TWRMFThemesManager.SetChannelPenaltyPanelDefaultColor(FActivePanel)
    end;

    {if HighlightSelectedPanel and (FActivePanel <> nil) then
    begin
      FActivePanel.BevelShadowColor := clRed;
      FActivePanel.BevelHighlightColor := clRed;
      //FActivePanel.FArcPenaltiesGrid.Col := 0;
      //FActivePanel.Color := clBtnFace;
      //FActivePanel.FIDLabel.Font.Color := clBlack;
    end;}
    FActivePanel := AActivePanel;
    if HighlightSelectedPanel and (FActivePanel <> nil) then
    begin
      FActivePanel.BevelShadowColor := clRed;
      FActivePanel.BevelHighlightColor := clRed;
      //FActivePanel.Color := clNavy;
      //FActivePanel.FIDLabel.Font.Color := clWhite;
    end;
    ScrollToActivePanel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyDialog.ScrollToActivePanel;
const OPNAME = 'TChannelPenaltyDialog.ScrollToActivePanel';
var
  lOver : integer;
begin
  try
    if Assigned(FActivePanel) then
    begin
      if (FActivePanel.Top < 0) then
        FPenaltyScrollBox.VertScrollBar.Position :=
          FPenaltyScrollBox.VertScrollBar.Position + FActivePanel.Top
      else
      begin
        lOver := FActivePanel.Top + FActivePanel.Height - FPenaltyScrollBox.Height;
        if (lOver > 0) then
          FPenaltyScrollBox.VertScrollBar.Position :=
            FPenaltyScrollBox.VertScrollBar.Position + lOver + 4;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyDialog.AssignHelpContext;
const OPNAME = 'TChannelPenaltyDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                                      HC_ChannelPenaltyStructures);
    SetControlHelpContext(FChannelsListBox,                          HC_ChannelPenaltyStructures);
    SetControlHelpContext(FNewPenaltyButton,                         HC_ChannelPenaltyStructures);
    SetControlHelpContext(FDeletePenaltyButton,                      HC_ChannelPenaltyStructures);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TChannelPenaltyDialog.OnUpDownKeyDown(Sender: TObject;var Key: Word; Shift: TShiftState);
const OPNAME = 'TChannelPenaltyDialog.OnUpDownKeyDown';
begin
  try
    if (Key = VK_UP) then
      MoveUp(Sender)
    else
    if (Key = VK_DOWN) then
      MoveDown(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

