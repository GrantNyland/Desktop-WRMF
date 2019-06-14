{******************************************************************************}
{*  UNIT      : Contains the class TFMSupportChannelsDialog.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMSupportChannelsDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Grids,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TFMSupportChannelsDialog = class(TAbstractScrollablePanel)
  private
  protected
    FPnlTop                    : TPanel;
    FPnlBottom                 : TPanel;
    FPnlSupportChannel         : TPanel;
    FBtnAddSupportChannel      : TFieldBitBtn;
    FBtnDeleteSupportChannel   : TFieldBitBtn;
    FLblDescription            : TLabel;
    FGrdSupportChannels        : TFieldStringGrid;

    FCbxSupportChannel         : TFieldComboBox;
    FLblControllingSubSystems  : TLabel;
    FBtnAddCntrlSubSystem      : TFieldBitBtn;
    FBtnDeleteCntrlSubSystem   : TFieldBitBtn;
    FGrdCntrlSubSystems        : TFieldStringGrid;
    FCbxSubSystem              : TFieldComboBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property BtnAddSupportChannel          : TFieldBitBtn       read FBtnAddSupportChannel;
    property BtnDeleteSupportChannel       : TFieldBitBtn       read FBtnDeleteSupportChannel;
    property GrdSupportChannels            : TFieldStringGrid   read FGrdSupportChannels;
    property CbxSupportChannel             : TFieldComboBox     read FCbxSupportChannel;
    property BtnAddCntrlSubSystem          : TFieldBitBtn       read FBtnAddCntrlSubSystem;
    property BtnDeleteCntrlSubSystem       : TFieldBitBtn       read FBtnDeleteCntrlSubSystem;
    property GrdCntrlSubSystems            : TFieldStringGrid   read FGrdCntrlSubSystems;
    property CbxSubSystem                  : TFieldComboBox     read FCbxSubSystem;
    property PnlSupportChannel             : TPanel             read FPnlSupportChannel;
  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  UConstants,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TFMSupportChannelsDialog                                                   *}
{******************************************************************************}

procedure TFMSupportChannelsDialog.CreateMemberObjects;
const OPNAME = 'TFMSupportChannelsDialog.CreateMemberObjects';
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

    FBtnAddSupportChannel       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddSupportChannel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    with FBtnAddSupportChannel do
    begin
      Parent  := FPnlTop;
      Left    := 0;
      Top     := 0;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FBtnDeleteSupportChannel       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteSupportChannel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    with FBtnDeleteSupportChannel do
    begin
      Parent  := FPnlTop;
      Left    := 75;
      Top     := 0;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FLblDescription := TLabel.Create(lOwner);
    with FLblDescription do
    begin
      Parent    := FPnlTop;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 190;
      Top       := 2;
      Width     := 400;
      Height    := 26;
      Font.Style := [fsBold];
      WordWrap  := TRUE;
    end;

    FGrdSupportChannels := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdSupportChannels do
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

    FCbxSupportChannel := CreateFieldComboBox(FAppModules, lOwner, FPnlBottom, 0, 0, 172, 21, 3, TRUE, csDropDownList);
    FCbxSupportChannel.Visible := FALSE;
    FPnlSupportChannel := TPanel.Create(lOwner);
    with FPnlSupportChannel do
    begin
      Parent     := FPnlBottom;
      Left       := 174;
      Top        := 0;
      Align      := alClient;
      BevelOuter := bvNone;
    end;

    FLblControllingSubSystems := TLabel.Create(lOwner);
    with FLblControllingSubSystems do
    begin
      Parent    := FPnlSupportChannel;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 20;
      Top       := 10;
      Width     := 120;
      Height    := 21;
    end;

    FBtnAddCntrlSubSystem     := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddCntrlSubSystem.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATECHART');
    with FBtnAddCntrlSubSystem do
    begin
      Parent  := FPnlSupportChannel;
      Left    := 152;
      Top     := 4;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FBtnDeleteCntrlSubSystem  := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteCntrlSubSystem.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETECHART');
    with FBtnDeleteCntrlSubSystem do
    begin
      Parent  := FPnlSupportChannel;
      Left    := 231;
      Top     := 4;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;

    FGrdCntrlSubSystems := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdCntrlSubSystems do
    begin
      Parent           := FPnlSupportChannel;
      Left             := 20;
      Top              := 35;
      Width            := 322;
      Height           := 120;
      ColCount         := 3;
      RowCount         := 1;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 100;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    end;
    FCbxSubSystem := CreateFieldComboBox(FAppModules, lOwner, FPnlSupportChannel, 53, 132, 172, 21, 3, TRUE, csDropDownList);
    FCbxSubSystem.Visible := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsDialog.Resize;
const OPNAME = 'TFMSupportChannelsDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsDialog.Initialise: boolean;
const OPNAME = 'TFMSupportChannelsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    BtnAddSupportChannel.Enabled    := (FAppModules.User.UserRights in CUR_EditData) AND
                                       (NOT FAppModules.StudyArea.ScenarioLocked);
    BtnDeleteSupportChannel.Enabled := (FAppModules.User.UserRights in CUR_EditData) AND
                                       (NOT FAppModules.StudyArea.ScenarioLocked);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TFMSupportChannelsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblControllingSubSystems.Caption := FAppModules.Language.GetString('PlanningGUI.ControllingSubSystems');
    FLblDescription.Caption           := FAppModules.Language.GetString('PlanningGUI.SupportChannelDescr');
    FBtnAddSupportChannel.Caption     := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnAddSupportChannel.Hint        := FAppModules.Language.GetString('ButtonHint.AddSupportChannel');
    FBtnDeleteSupportChannel.Caption  := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnDeleteSupportChannel.Hint     := FAppModules.Language.GetString('ButtonHint.DeleteSupportChannel');
    FBtnAddCntrlSubSystem.Caption     := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnAddCntrlSubSystem.Hint        := FAppModules.Language.GetString('ButtonHint.AddCntrlSubSystem');
    FBtnDeleteCntrlSubSystem.Caption  := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnDeleteCntrlSubSystem.Hint     := FAppModules.Language.GetString('ButtonHint.DeleteCntrlSubSystem');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsDialog.RestoreColourState;
const OPNAME = 'TFMSupportChannelsDialog.RestoreColourState';
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

procedure TFMSupportChannelsDialog.AssignHelpContext;
const OPNAME = 'TFMSupportChannelsDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
