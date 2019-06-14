{******************************************************************************}
{*  UNIT      : Contains the class TChannelSwitchControlDialog.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/03/09                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UChannelSwitchControlDialog;

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
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TChannelSwitchControlDialog = class(TAbstractScrollablePanel)
  private
  protected
    FDateActiveGBox         : TScrollBox;

    FPnlTop                    : TPanel;
    FPnlBottom                 : TPanel;
    FBtnAddSwitchControl       : TFieldBitBtn;
    FBtnDeleteSwitchControl    : TFieldBitBtn;
    FGrdSwitchControls         : TStringGrid;
    FPnlClient                 : TPanel;
    FLblSwitchDefinition       : TLabel;
    FCbxSwitchDefinition       : TFieldComboBox;
    FLblAssociatedNode         : TLabel;
    FCbxAssociatedNode         : TFieldComboBox;
    FLblWaterLevel             : TLabel;
    FEdtWaterLevel             : TFieldEdit;
    FLblSwitchType             : TLabel;
    FRgpSwitchType             : TFieldRadioGroup;
    FImgSwitch                 : TImage;
    FLblInitialStatus          : TLabel;
    FRgpInitialStatus          : TFieldRadioGroup;

    FTimeRelatedGroup          : TPanel;
    FLevelRelatedGroup         : TPanel;


    FCbxYearActive             : TFieldComboBox;
    FCbxMonthActive            : TFieldComboBox;
    FCbxYearObsolete           : TFieldComboBox;
    FCbxMonthObsolete          : TFieldComboBox;

    FYearActiveLabel           : TLabel;
    FMonthActiveLabel          : TLabel;
    FYearObsoleteLabel         : TLabel;
    FMonthObsoleteLabel        : TLabel;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure RestoreColourState; override;

    property BtnAddSwitchControl       : TFieldBitBtn     read FBtnAddSwitchControl;
    property BtnDeleteSwitchControl    : TFieldBitBtn     read FBtnDeleteSwitchControl;
    property GrdSwitchControls         : TStringGrid      read FGrdSwitchControls;
    property PnlClient                 : TPanel           read FPnlClient;
    property CbxSwitchDefinition       : TFieldComboBox   read FCbxSwitchDefinition;
    property CbxAssociatedNode         : TFieldComboBox   read FCbxAssociatedNode;
    property EdtWaterLevel             : TFieldEdit       read FEdtWaterLevel;
    property RgpSwitchType             : TFieldRadioGroup read FRgpSwitchType;
    property RgpInitialStatus          : TFieldRadioGroup read FRgpInitialStatus;

    property TimeRelatedGroup          : TPanel           read FTimeRelatedGroup;
    property LevelRelatedGroup         : TPanel           read FLevelRelatedGroup;

    property CbxYearActive            : TFieldComboBox    read FCbxYearActive;
    property CbxMonthActive           : TFieldComboBox    read FCbxMonthActive;
    property CbxYearObsolete          : TFieldComboBox    read FCbxYearObsolete;
    property CbxMonthObsolete         : TFieldComboBox    read FCbxMonthObsolete;

  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{******************************************************************************}
{* TChannelSwitchControlDialog                                                *}
{******************************************************************************}

procedure TChannelSwitchControlDialog.CreateMemberObjects;
const OPNAME = 'TChannelSwitchControlDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FPnlTop := TPanel.Create(lOwner);
    with FPnlTop do
    begin
      Parent     := lParent;
      Left       := 0;
      Top        := 0;
      Width      := 753;
      Height     := 30;
      Align      := alTop;
      BevelOuter := bvNone;
    end;
    FPnlBottom := TPanel.Create(lOwner);
    with FPnlBottom do
    begin
      Parent     := lParent;
      Left       := 0;
      Top        := 30;
      Width      := 753;
      Height     := 440;
      Align      := alTop;
      BevelOuter := bvNone;
    end;

    FBtnAddSwitchControl       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddSwitchControl.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    with FBtnAddSwitchControl do
    begin
      Parent  := FPnlTop;
      Left    := 0;
      Top     := 0;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FBtnDeleteSwitchControl       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteSwitchControl.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    with FBtnDeleteSwitchControl do
    begin
      Parent  := FPnlTop;
      Left    := 75;
      Top     := 0;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;

    FGrdSwitchControls     := TStringGrid.Create(lOwner);
    with FGrdSwitchControls do
    begin
      Parent           := FPnlBottom;
      Left             := 0;
      Top              := 0;
      Width            := 176;
      Height           := 440;
      Align            := alLeft;
      ColCount         := 1;
      DefaultColWidth  := 170;
      DefaultRowHeight := 20;
      ScrollBars       := ssVertical;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect];
    end;


    FTimeRelatedGroup            := TPanel.Create(lOwner);
    FTimeRelatedGroup.Parent      := LParent;
    FTimeRelatedGroup.Left        := FGrdSwitchControls.Width + 5;
    FTimeRelatedGroup.Top         := FPnlBottom.Top;
    FTimeRelatedGroup.Width       := 500;
    //FTimeRelatedGroup.Align       := alTop;
    FTimeRelatedGroup.Height      := 450;
    FTimeRelatedGroup.BevelOuter  := bvNone;

    FLevelRelatedGroup            := TPanel.Create(lOwner);
    FLevelRelatedGroup.Parent     := LParent;
    FLevelRelatedGroup.Left       := FGrdSwitchControls.Width + 5;
    FLevelRelatedGroup.Top        := FPnlBottom.Top;
    FLevelRelatedGroup.Width      := 500;
    FLevelRelatedGroup.Height     := 450;
    FLevelRelatedGroup.BevelOuter := bvNone;


    FYearActiveLabel      := CreateFieldLabel     (lOwner, FTimeRelatedGroup,  30, 10, 140,  21);
    FMonthActiveLabel     := CreateFieldLabel     (lOwner, FTimeRelatedGroup,  30, 40, 140,  21);
    FYearObsoleteLabel    := CreateFieldLabel     (lOwner, FTimeRelatedGroup,  30, 70, 140,  21);
    FMonthObsoleteLabel   := CreateFieldLabel     (lOwner, FTimeRelatedGroup,  30, 100, 140,  21);

    FCbxYearActive      := CreateFieldComboBox  (FAppModules, lOwner, FTimeRelatedGroup, 160, 10,  80,  21, 0, TRUE,csDropDownList);
    FCbxMonthActive     := CreateFieldComboBox  (FAppModules, lOwner, FTimeRelatedGroup, 160, 40,  80,  21, 0, TRUE,csDropDownList);
    FCbxYearObsolete    := CreateFieldComboBox  (FAppModules, lOwner, FTimeRelatedGroup, 160, 70,  80,  21, 0, TRUE,csDropDownList);
    FCbxMonthObsolete   := CreateFieldComboBox  (FAppModules, lOwner, FTimeRelatedGroup, 160, 100, 80,  21, 0, TRUE,csDropDownList);

    FCbxYearActive.DropDownCount   := 25;
    FCbxYearObsolete.DropDownCount := 25;

    FPnlClient := TPanel.Create(lOwner);
    with FPnlClient do
    begin
      Parent     := FPnlBottom;
      Left       := 0;
      Top        := 0;
      Width      := 753;
      Height     := 30;
      Align      := alClient;
      BevelOuter := bvNone;
    end;


    FLblSwitchDefinition := TLabel.Create(lOwner);
    with FLblSwitchDefinition do
    begin
      Parent    := FLevelRelatedGroup; //FPnlClient;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 10;
      Width     := 170;
      Height    := 21;
    end;
    FCbxSwitchDefinition  := CreateFieldComboBox(FAppModules, lOwner, FLevelRelatedGroup{FPnlClient}, 190,  10, 200, 21, 3, TRUE, csDropDownList);

    FLblAssociatedNode := TLabel.Create(lOwner);
    with FLblAssociatedNode do
    begin
      Parent    := FLevelRelatedGroup; //FPnlClient;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 35;
      Width     := 170;
      Height    := 21;
    end;
    FCbxAssociatedNode  := CreateFieldComboBox(FAppModules, lOwner, FLevelRelatedGroup{FPnlClient}, 190,  35, 200, 21, 3, TRUE, csDropDownList);

    FLblWaterLevel := TLabel.Create(lOwner);
    with FLblWaterLevel do
    begin
      Parent    := FLevelRelatedGroup; //FPnlClient;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 60;
      Width     := 170;
      Height    := 21;
    end;
    FEdtWaterLevel := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtWaterLevel do
    begin
      Parent    := FLevelRelatedGroup; //FPnlClient;
      Left      := 190;
      Top       := 60;
      Width     := 70;
      Height    := 21;
    end;

    FLblSwitchType := TLabel.Create(lOwner);
    with FLblSwitchType do
    begin
      Parent    := FLevelRelatedGroup; //FPnlClient;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 85;
      Width     := 170;
      Height    := 21;
    end;
    FRgpSwitchType := CreateFieldRadioGroup(FAppModules, lOwner, FLevelRelatedGroup{FPnlClient}, 190, 85, 280, 90, 0, TRUE);
    FRgpSwitchType.Columns := 1;

    FImgSwitch := TImage.Create(lOwner);
    with FImgSwitch do
    begin
      Parent := FLevelRelatedGroup; //FPnlClient;
      Left   := 190;
      Top    := 180;
      Width  := 144;
      Height := 67;
    end;
    FImgSwitch.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'CHANNELSWITCH');

    FLblInitialStatus := TLabel.Create(lOwner);
    with FLblInitialStatus do
    begin
      Parent    := FLevelRelatedGroup; //FPnlClient;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 255;
      Width     := 170;
      Height    := 21;
    end;
    FRgpInitialStatus := CreateFieldRadioGroup(FAppModules, lOwner, FLevelRelatedGroup{FPnlClient}, 190, 255, 90, 60, 0, TRUE);
    FRgpInitialStatus.Columns := 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlDialog.Initialise: boolean;
const OPNAME = 'TChannelSwitchControlDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    with FRgpSwitchType do
    begin
      Items.Clear;
      Items.Add('Open if in A and Closed if in B');
      Items.Add('Closed if in A and Open if in B');
      Items.Add('Change status once, when switch level is crossed');
    end;
    with FRgpInitialStatus do
    begin
      Items.Clear;
      Items.Add('Closed');
      Items.Add('Open');
    end;  
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlDialog.Resize;
const OPNAME = 'TChannelSwitchControlDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlDialog.LanguageHasChanged: boolean;
const OPNAME = 'TChannelSwitchControlDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblSwitchDefinition.Caption := FAppModules.Language.GetString('LabelCaption.SwitchDefinition');
    FLblAssociatedNode.Caption   := FAppModules.Language.GetString('LabelCaption.AssociatedNode');
    FLblWaterLevel.Caption       := FAppModules.Language.GetString('LabelCaption.WaterLevel');
    FLblSwitchType.Caption       := FAppModules.Language.GetString('LabelCaption.SwitchType');
    FLblInitialStatus.Caption    := FAppModules.Language.GetString('LabelCaption.InitialStatus');
//    FNodenumberLabel.Caption      := FAppModules.Language.GetString('TField.NodeCount');

    FYearActiveLabel.Caption     := 'Active Hydrological Year';
    FMonthActiveLabel.Caption    := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.MonthActiveLabel');
    FYearObsoleteLabel.Caption   := 'Inactive Hydrological Year';
    FMonthObsoleteLabel.Caption  := 'Inactive Month';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlDialog.RestoreColourState;
const OPNAME = 'TChannelSwitchControlDialog.RestoreColourState';
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

procedure TChannelSwitchControlDialog.AssignHelpContext;
const OPNAME = 'TChannelSwitchControlDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                   HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FSelectPenaltyStruct,   HC_ReservoirPenaltyStructures);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

