{******************************************************************************}
{*  UNIT      : Contains the class TFMSupportStrategyDialog.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMSupportStrategyDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TFMSupportStrategyDialog = class(TAbstractScrollablePanel)
  private
  protected
    FLblSupportStrategy        : TLabel;
    FCbxSupportStrategy        : TFieldComboBox;
    FLblBalancingOption        : TLabel;
    FRgpBalancingOption        : TFieldRadioGroup;
    FStrategyGroupBox          : TGroupBox;

    FLblSubSystemOrder         : TLabel;
    FLblSubSystemOrderExpl     : TLabel;
    FBtnMoveUp                 : TFieldBitBtn;
    FBtnMoveDown               : TFieldBitBtn;
    FGrdSubSystemOrder         : TFieldStringGrid;

    FLblFixedPosition          : TLabel;
    FLblFixedPositionExpl      : TLabel;
    FBtnAddFixedPosition       : TFieldBitBtn;
    FBtnDeleteFixedPosition    : TFieldBitBtn;
    FGrdFixedPosition          : TFieldStringGrid;

    FLblSpecificOrder          : TLabel;
    FLblSpecificOrderExpl      : TLabel;
    FBtnAddSpecificOrder       : TFieldBitBtn;
    FBtnDeleteSpecificOrder    : TFieldBitBtn;
    FGrdSpecificOrder          : TFieldStringGrid;

    FCbxPosition               : TFieldComboBox;
    FCbxSubSystem              : TFieldComboBox;
    FCbxBeforeSubSystem        : TFieldComboBox;
    FCbxAfterSubSystem         : TFieldComboBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure ResetButtonState(ANrOfSubSystems, ANrInFixedPosition, ANrInSpecificOrder : integer);
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property CbxSupportStrategy        : TFieldComboBox   read FCbxSupportStrategy;
    property RgpBalancingOption        : TFieldRadioGroup read FRgpBalancingOption;
    property BtnMoveUp                 : TFieldBitBtn     read FBtnMoveUp;
    property BtnMoveDown               : TFieldBitBtn     read FBtnMoveDown;
    property GrdSubSystemOrder         : TFieldStringGrid read FGrdSubSystemOrder;
    property GrdFixedPosition          : TFieldStringGrid read FGrdFixedPosition;
    property GrdSpecificOrder          : TFieldStringGrid read FGrdSpecificOrder;
    property BtnAddFixedPosition       : TFieldBitBtn     read FBtnAddFixedPosition;
    property BtnDeleteFixedPosition    : TFieldBitBtn     read FBtnDeleteFixedPosition;
    property BtnAddSpecificOrder       : TFieldBitBtn     read FBtnAddSpecificOrder;
    property BtnDeleteSpecificOrder    : TFieldBitBtn     read FBtnDeleteSpecificOrder;
    property CbxPosition               : TFieldComboBox   read FCbxPosition;
    property CbxSubSystem              : TFieldComboBox   read FCbxSubSystem;
    property CbxBeforeSubSystem        : TFieldComboBox   read FCbxBeforeSubSystem;
    property CbxAfterSubSystem         : TFieldComboBox   read FCbxAfterSubSystem;
    property StrategyGroupBox          : TGroupBox        read FStrategyGroupBox;
  end;

implementation

uses
  SysUtils,
  VCL.Grids,
  UConstants,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TFMSupportStrategyDialog                                                   *}
{******************************************************************************}

procedure TFMSupportStrategyDialog.CreateMemberObjects;
const OPNAME = 'TFMSupportStrategyDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FLblSupportStrategy   := TLabel.Create(lOwner);
    FStrategyGroupBox := CreateFieldGroupBox(lOwner, lParent, 10, 5, 360, 70, 0, FALSE);

    with FLblSupportStrategy do
    begin
      Parent    := FStrategyGroupBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 10;
      Width     := 210;
      Height    := 21;
    end;
    FCbxSupportStrategy  := CreateFieldComboBox(FAppModules, lOwner, FStrategyGroupBox, 140,  10,  40, 21, 3, TRUE, csDropDownList);

    FLblBalancingOption   := TLabel.Create(lOwner);
    with FLblBalancingOption do
    begin
      Parent    := FStrategyGroupBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 40;
      Width     := 240;
      Height    := 21;
    end;
    FRgpBalancingOption   := CreateFieldRadioGroup(FAppModules, lOwner, FStrategyGroupBox, 255, 31, 100, 34, 0, TRUE);
    FRgpBalancingOption.Columns := 2;

    FLblSubSystemOrder   := TLabel.Create(lOwner);
    with FLblSubSystemOrder do
    begin
      Parent    := lParent;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 78;
      Width     := 220;
      Height    := 16;
    end;
    FLblSubSystemOrderExpl  := TLabel.Create(lOwner);
    with FLblSubSystemOrderExpl do
    begin
      Parent     := lParent;
      AutoSize   := FALSE;
      Layout     := tlCenter;
      Left       := 10;
      Top        := 95;
      Width      := 220;
      WordWrap   := TRUE;
      Font.Color := clRed;
      Height     := 16;
    end;
    FBtnMoveUp       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnMoveUp.Glyph.LoadFromResourceName(HImagesInstance, 'ARROWUP');
    FBtnMoveUp.Parent := lParent;
    with FBtnMoveUp do
    begin
      Left    := 10;
      Top     := 115;
      Width   := 95;
      Height  := 25;
      ShowHint:= True;
    end;
    FBtnMoveDown       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnMoveDown.Glyph.LoadFromResourceName(HImagesInstance, 'ARROWDOWN');
    FBtnMoveDown.Parent := lParent;
    with FBtnMoveDown do
    begin
      Left    := 115;
      Top     := 115;
      Width   := 95;
      Height  := 25;
      ShowHint:= True;
    end;
    FGrdSubSystemOrder := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdSubSystemOrder do
    begin
      Parent           := lParent;
      Left             := 10;
      Top              := 145;
      Width            := 222;
      Height           := 320;
      ColCount         := 2;
      RowCount         := 2;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 100;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    end;

    FLblFixedPosition   := TLabel.Create(lOwner);
    with FLblFixedPosition do
    begin
      Parent    := lParent;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 280;
      Top       := 78;
      Width     := 210;
      Height    := 16;
    end;
    FLblFixedPositionExpl  := TLabel.Create(lOwner);
    with FLblFixedPositionExpl do
    begin
      Parent     := lParent;
      AutoSize   := FALSE;
      Layout     := tlCenter;
      Left       := 280;
      Top        := 95;
      Width      := 220;
      WordWrap   := TRUE;
      Font.Color := clRed;
      Height     := 16;
    end;
    FBtnAddFixedPosition       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddFixedPosition.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    FBtnAddFixedPosition.Parent := lParent;
    with FBtnAddFixedPosition do
    begin
      Left    := 280;
      Top     := 115;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FBtnDeleteFixedPosition       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteFixedPosition.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    FBtnDeleteFixedPosition.Parent := lParent;
    with FBtnDeleteFixedPosition do
    begin
      Left    := 385;
      Top     := 115;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FGrdFixedPosition := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdFixedPosition do
    begin
      Parent           := lParent;
      Left             := 280;
      Top              := 145;
      Width            := 222;
      Height           := 108;
      ColCount         := 2;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 100;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goAlwaysShowEditor];
    end;

    FLblSpecificOrder   := TLabel.Create(lOwner);
    with FLblSpecificOrder do
    begin
      Parent    := lParent;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 280;
      Top       := 288;
      Width     := 310;
      Height    := 16;
    end;
    FLblSpecificOrderExpl  := TLabel.Create(lOwner);
    with FLblSpecificOrderExpl do
    begin
      Parent     := lParent;
      AutoSize   := FALSE;
      Layout     := tlCenter;
      Left       := 280;
      Top        := 305;
      Width      := 220;
      WordWrap   := TRUE;
      Font.Color := clRed;
      Height     := 16;
    end;
    FBtnAddSpecificOrder     := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddSpecificOrder.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATECHART');
    FBtnAddSpecificOrder.Parent := lParent;
    with FBtnAddSpecificOrder do
    begin
      Left    := 280;
      Top     := 325;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FBtnDeleteSpecificOrder  := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteSpecificOrder.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETECHART');
    FBtnDeleteSpecificOrder.Parent := lParent;
    with FBtnDeleteSpecificOrder do
    begin
      Left    := 385;
      Top     := 325;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FGrdSpecificOrder := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdSpecificOrder do
    begin
      Parent           := lParent;
      Left             := 280;
      Top              := 355;
      Width            := 302{393};
      Height           := 108;
      ColCount         := 2;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 170;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goAlwaysShowEditor];
    end;

    FCbxPosition        := CreateFieldComboBox(FAppModules, lOwner, lParent,  12, 133,  62, 21, 3, TRUE, csDropDownList);
    FCbxSubSystem       := CreateFieldComboBox(FAppModules, lOwner, lParent,  73, 133, 142, 21, 3, TRUE, csDropDownList);
    FCbxBeforeSubSystem := CreateFieldComboBox(FAppModules, lOwner, lParent,  12, 303, 142, 21, 3, TRUE, csDropDownList);
    FCbxAfterSubSystem  := CreateFieldComboBox(FAppModules, lOwner, lParent, 183, 303, 142, 21, 3, TRUE, csDropDownList);

    FCbxPosition.Visible        := FALSE;
    FCbxSubSystem.Visible       := FALSE;
    FCbxBeforeSubSystem.Visible := FALSE;
    FCbxAfterSubSystem.Visible  := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyDialog.Resize;
const OPNAME = 'TFMSupportStrategyDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyDialog.ResetButtonState(ANrOfSubSystems, ANrInFixedPosition,
                                                    ANrInSpecificOrder : integer);
const OPNAME = 'TFMSupportStrategyDialog.ResetButtonState';
begin
  try
    BtnMoveUp.Enabled              := (FAppModules.User.UserRights in CUR_EditData) AND
                                      (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                      (GrdSubSystemOrder.Row > 1);
    BtnMoveDown.Enabled            := (FAppModules.User.UserRights in CUR_EditData) AND
                                      (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                      (GrdSubSystemOrder.Row < GrdSubSystemOrder.RowCount-1);
    BtnAddFixedPosition.Enabled    := (FAppModules.User.UserRights in CUR_EditData) AND
                                      (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                      (ANrOfSubSystems > 0);
    BtnDeleteFixedPosition.Enabled := (FAppModules.User.UserRights in CUR_EditData) AND
                                      (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                      (ANrInFixedPosition > 0);
    BtnAddSpecificOrder.Enabled    := (FAppModules.User.UserRights in CUR_EditData) AND
                                      (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                      (ANrOfSubSystems > 0);
    BtnDeleteSpecificOrder.Enabled := (FAppModules.User.UserRights in CUR_EditData) AND
                                      (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                      (ANrInSpecificOrder > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFMSupportStrategyDialog.Initialise: boolean;
const OPNAME = 'TFMSupportStrategyDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FGrdSubSystemOrder.ColWidths[0] := 60;
    FGrdSubSystemOrder.ColWidths[1] := 140;

    FGrdFixedPosition.ColWidths[0] := 60;
    FGrdFixedPosition.ColWidths[1] := 140;

    FGrdSpecificOrder.ColWidths[0] := 140;
    FGrdSpecificOrder.ColWidths[1] := 140;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportStrategyDialog.LanguageHasChanged: boolean;
const OPNAME = 'TFMSupportStrategyDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblSubSystemOrder.Caption     := FAppModules.Language.GetString('PlanningGUI.OrderInWhichSubSystemsAreSolved');
    FLblSubSystemOrderExpl.Caption := FAppModules.Language.GetString('PlanningGUI.DefaultOrderA');
    FLblFixedPositionExpl.Caption  := FAppModules.Language.GetString('PlanningGUI.OrderBOverridesA');
    FLblSpecificOrderExpl.Caption  := FAppModules.Language.GetString('PlanningGUI.OrderCOverridesAB');
    FBtnMoveUp.Caption             := FAppModules.Language.GetString('ButtonCaption.BtnMoveUp');
    FBtnMoveDown.Caption           := FAppModules.Language.GetString('ButtonCaption.BtnMoveDown');
    FBtnMoveUp.Hint                := FAppModules.Language.GetString('ButtonHint.BtnMoveUpSubSystem');
    FBtnMoveDown.Hint              := FAppModules.Language.GetString('ButtonHint.BtnMoveDownSubSystem');

    FLblSupportStrategy.Caption     := FAppModules.Language.GetString('PlanningGUI.SupportStrategy');
    FLblBalancingOption.Caption     := FAppModules.Language.GetString('PlanningGUI.BalancingOption');
    FLblFixedPosition.Caption       := FAppModules.Language.GetString('PlanningGUI.FixedPosition');
    FBtnAddFixedPosition.Caption    := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnDeleteFixedPosition.Caption := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnAddFixedPosition.Hint       := FAppModules.Language.GetString('ButtonHint.AddFixedPosition');
    FBtnDeleteFixedPosition.Hint    := FAppModules.Language.GetString('ButtonHint.DeleteFixedPosition');
    FLblSpecificOrder.Caption       := FAppModules.Language.GetString('PlanningGUI.SpecificOrder');
    FBtnAddSpecificOrder.Caption    := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnDeleteSpecificOrder.Caption := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnAddSpecificOrder.Hint       := FAppModules.Language.GetString('ButtonHint.AddSpecificOrder');
    FBtnDeleteSpecificOrder.Hint    := FAppModules.Language.GetString('ButtonHint.DeleteSpecificOrder');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyDialog.RestoreColourState;
const OPNAME = 'TFMSupportStrategyDialog.RestoreColourState';
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

procedure TFMSupportStrategyDialog.AssignHelpContext;
const OPNAME = 'TFMSupportStrategyDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
