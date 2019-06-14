{******************************************************************************}
{*  UNIT      : Contains the class TFMDemandDefinitionDialog.                 *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/26                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMDemandDefinitionDialog;

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

  TFMDemandDefinitionDialog = class(TAbstractScrollablePanel)
  private
  protected
    FPnlTop                    : TPanel;
    FPnlBottom                 : TPanel;
    FBtnAddDemandDef           : TFieldBitBtn;
    FBtnDeleteDemandDef        : TFieldBitBtn;
    FBtnMoveUp                 : TFieldBitBtn;
    FBtnMoveDown               : TFieldBitBtn;
    FTrvDemandDefs             : TAbstractTreeView;
    FPnlDemandDef              : TPanel;

    FLblDemandCentreID         : TLabel;
    FCbxDemandCentreID         : TFieldComboBox;
    FLblSubSystem              : TLabel;
    FRgpGrowthType             : TFieldRadioGroup;
    FLblTargetDemand           : TLabel;
    FEdtTargetDemand           : TFieldEdit;
    FLblTargetDemandUnits      : TLabel;
    FLblUserCategory           : TLabel;
    FCbxUserCategory           : TFieldComboBox;
    FLblSupportArc1            : TLabel;
    FCbxSupportArc1            : TFieldComboBox;
    FLblSupportArc2            : TLabel;
    FCbxSupportArc2            : TFieldComboBox;
    FGrpGeneralSupport         : TGroupBox;
    FGrpSpecificSupport        : TGroupBox;

    FBtnAddSupportSubSystem    : TFieldBitBtn;
    FBtnDeleteSupportSubSystem : TFieldBitBtn;
    FLblSubSystemGridHeader    : TLabel;
    FGrdSupportSubSystems      : TFieldStringGrid;
    FCbxSupportSubSystem       : TFieldComboBox;
    FCbxChannel                : TFieldComboBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property BtnAddDemandDef           : TFieldBitBtn      read FBtnAddDemandDef;
    property BtnDeleteDemandDef        : TFieldBitBtn      read FBtnDeleteDemandDef;
    property BtnMoveUp                 : TFieldBitBtn      read FBtnMoveUp;
    property BtnMoveDown               : TFieldBitBtn      read FBtnMoveDown;
    property TrvDemandDefs             : TAbstractTreeView read FTrvDemandDefs;
    property CbxDemandCentreID         : TFieldComboBox    read FCbxDemandCentreID;
    property RgpGrowthType             : TFieldRadioGroup  read FRgpGrowthType;
    property EdtTargetDemand           : TFieldEdit        read FEdtTargetDemand;
    property CbxUserCategory           : TFieldComboBox    read FCbxUserCategory;
    property CbxSupportArc1            : TFieldComboBox    read FCbxSupportArc1;
    property CbxSupportArc2            : TFieldComboBox    read FCbxSupportArc2;
    property BtnAddSupportSubSystem    : TFieldBitBtn      read FBtnAddSupportSubSystem;
    property BtnDeleteSupportSubSystem : TFieldBitBtn      read FBtnDeleteSupportSubSystem;
    property GrdSupportSubSystems      : TFieldStringGrid  read FGrdSupportSubSystems;
    property CbxSupportSubSystem       : TFieldComboBox    read FCbxSupportSubSystem;
    property CbxChannel                : TFieldComboBox    read FCbxChannel;
    property PnlDemandDef              : TPanel            read FPnlDemandDef;
    property LblSubSystem              : TLabel            read FLblSubSystem;
  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  UConstants,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TFMDemandDefinitionDialog                                                  *}
{******************************************************************************}

procedure TFMDemandDefinitionDialog.CreateMemberObjects;
const OPNAME = 'TFMDemandDefinitionDialog.CreateMemberObjects';
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

    FBtnAddDemandDef := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddDemandDef.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    with FBtnAddDemandDef do
    begin
      Parent  := FPnlTop;
      Left    := 0;
      Top     := 0;
      Width   := 95;
      Height  := 25;
      ShowHint := True;
    end;
    FBtnDeleteDemandDef := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteDemandDef.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    with FBtnDeleteDemandDef do
    begin
      Parent  := FPnlTop;
      Left    := 95;
      Top     := 0;
      Width   := 95;
      Height  := 25;
      ShowHint := True;
    end;
    FBtnMoveUp := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnMoveUp.Glyph.LoadFromResourceName(HImagesInstance, 'ARROWUP');
    with FBtnMoveUp do
    begin
      Parent  := FPnlTop;
      Left    := 190;
      Top     := 0;
      Width   := 95;
      Height  := 25;
      ShowHint := True;
    end;
    FBtnMoveDown := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnMoveDown.Glyph.LoadFromResourceName(HImagesInstance, 'ARROWDOWN');
    with FBtnMoveDown do
    begin
      Parent  := FPnlTop;
      Left    := 285;
      Top     := 0;
      Width   := 95;
      Height  := 25;
      ShowHint := True;
    end;

    FTrvDemandDefs     := TAbstractTreeView.Create(lOwner, FAppModules);
    with FTrvDemandDefs do
    begin
      Parent           := FPnlBottom;
      Left             := 0;
      Top              := 0;
      Width            := 150;
      Height           := 440;
      Align            := alLeft;
      ReadOnly         := True;
      HideSelection    := False;
      MultiSelect      := FALSE;
      DragMode         := dmAutomatic;
    end;

    FPnlDemandDef := TPanel.Create(lOwner);
    with FPnlDemandDef do
    begin
      Parent           := FPnlBottom;
      Left             := 174;
      Top              := 0;
      Width            := 563;
      Height           := 440;
      Align            := alClient;
      BevelOuter       := bvNone;
    end;

    FLblDemandCentreID := TLabel.Create(lOwner);
    with FLblDemandCentreID do
    begin
      Parent    := FPnlDemandDef;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 30;
      Width     := 130;
      Height    := 21;
    end;
    FCbxDemandCentreID := CreateFieldComboBox(FAppModules, lOwner, FPnlDemandDef,  150, 30, 145, 21, 0, TRUE, csDropDownList);

    FLblSubSystem := TLabel.Create(lOwner);
    with FLblSubSystem do
    begin
      Parent    := FPnlDemandDef;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 5;//55
      Width     := 130;
      Height    := 21;
    end;
    FLblUserCategory := TLabel.Create(lOwner);
    with FLblUserCategory do
    begin
      Parent    := FPnlDemandDef;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 55;
      Width     := 130;
      Height    := 21;
    end;
    FCbxUserCategory := CreateFieldComboBox(FAppModules, lOwner, FPnlDemandDef, 150, 55, 145, 21, 3, TRUE, csDropDownList);

    FLblTargetDemand := TLabel.Create(lOwner);
    with FLblTargetDemand do
    begin
      Parent    := FPnlDemandDef;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 80;
      Width     := 130;
      Height    := 21;
    end;
    FLblTargetDemandUnits := TLabel.Create(lOwner);
    with FLblTargetDemandUnits do
    begin
      Parent    := FPnlDemandDef;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 300;
      Top       := 80;
      Width     := 145;
      Height    := 21;
    end;

    FEdtTargetDemand := CreateFieldEdit(FAppModules,lOwner,FPnlDemandDef, 150, 80, 145, 21, 3, TRUE);

    FRgpGrowthType := CreateFieldRadioGroup(FAppModules, lOwner, FPnlDemandDef, 10, 105, 555, 45, 0, TRUE);
    FRgpGrowthType.Columns := 3;

    FGrpGeneralSupport := TGroupBox.Create(lOwner);
    with FGrpGeneralSupport do
    begin
      Parent := FPnlDemandDef;
      Left   := 10;
      Top    := 160;
      Width  := 555;
      Height := 50;
    end;
    FLblSupportArc1 := TLabel.Create(lOwner);
    with FLblSupportArc1 do
    begin
      Parent    := FGrpGeneralSupport;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 20;
      Width     := 80;
      Height    := 21;
    end;
    FCbxSupportArc1  := CreateFieldComboBox(FAppModules, lOwner, FGrpGeneralSupport, 100, 20, 120, 21, 0, TRUE, csDropDownList);

    FLblSupportArc2 := TLabel.Create(lOwner);
    with FLblSupportArc2 do
    begin
      Parent    := FGrpGeneralSupport;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 250;
      Top       := 20;
      Width     := 80;
      Height    := 21;
    end;
    FCbxSupportArc2  := CreateFieldComboBox(FAppModules, lOwner, FGrpGeneralSupport, 340, 20, 120, 21, 0, TRUE, csDropDownList);

    FGrpSpecificSupport := TGroupBox.Create(lOwner);
    with FGrpSpecificSupport do
    begin
      Parent := FPnlDemandDef;
      Left   := 10;
      Top    := 220;
      Width  := 555;
      Height := 220;
    end;
    FBtnAddSupportSubSystem := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddSupportSubSystem.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATECHART');
    FBtnAddSupportSubSystem.Parent := FGrpSpecificSupport;
    with FBtnAddSupportSubSystem do
    begin
      Left    := 10;
      Top     := 20;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;
    FBtnDeleteSupportSubSystem  := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteSupportSubSystem.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETECHART');
    FBtnDeleteSupportSubSystem.Parent := FGrpSpecificSupport;
    with FBtnDeleteSupportSubSystem do
    begin
      Left    := 90;
      Top     := 20;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;

    FLblSubSystemGridHeader := TLabel.Create(lOwner);
    with FLblSubSystemGridHeader do
    begin
      Parent    := FGrpSpecificSupport;
      Left      := 10;
      Top       := 50;
      Width     := 250;
      Height    := 21;
    end;
    FGrdSupportSubSystems := CreateFieldStringGrid(FAppModules, lOwner, FGrpSpecificSupport,
                                                   10, 70, 530, 130, 0, TRUE);
    with FGrdSupportSubSystems do
    begin
      ColCount         := 6;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 85;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    end;

    FCbxSupportSubSystem := CreateFieldComboBox(FAppModules, lOwner, FGrpSpecificSupport,  12,  81,  85, 21, 3, TRUE, csDropDownList);
    FCbxChannel          := CreateFieldComboBox(FAppModules, lOwner, FGrpSpecificSupport, 168,  81,  85, 21, 3, TRUE, csDropDownList);
    FCbxSupportSubSystem.Visible := FALSE;
    FCbxChannel.Visible := FALSE;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionDialog.Resize;
const OPNAME = 'TFMDemandDefinitionDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionDialog.Initialise: boolean;
const OPNAME = 'TFMDemandDefinitionDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    BtnAddDemandDef.Enabled := (FAppModules.User.UserRights in CUR_EditData) AND
                               (NOT FAppModules.StudyArea.ScenarioLocked);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TFMDemandDefinitionDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FBtnAddDemandDef.Caption        := FAppModules.Language.GetString('ButtonCaption.btnAdd');
    FBtnAddDemandDef.Hint           := FAppModules.Language.GetString('ButtonHint.AddDemandDef');
    FBtnDeleteDemandDef.Caption     := FAppModules.Language.GetString('ButtonCaption.btnDelete');
    FBtnDeleteDemandDef.Hint        := FAppModules.Language.GetString('ButtonHint.DeleteDemandDef');
    FBtnMoveUp.Caption              := FAppModules.Language.GetString('ButtonCaption.BtnMoveUp');
    FBtnMoveDown.Caption            := FAppModules.Language.GetString('ButtonCaption.BtnMoveDown');
    FBtnMoveUp.Hint                 := FAppModules.Language.GetString('ButtonHint.BtnMoveUpDemandDef');
    FBtnMoveDown.Hint               := FAppModules.Language.GetString('ButtonHint.BtnMoveDownDemandDef');

    FBtnAddSupportSubSystem.Caption    := FAppModules.Language.GetString('ButtonCaption.btnAdd');
    FBtnAddSupportSubSystem.Hint       := FAppModules.Language.GetString('ButtonHint.AddSupportSubSystem');
    FBtnDeleteSupportSubSystem.Caption := FAppModules.Language.GetString('ButtonCaption.btnDelete');
    FBtnDeleteSupportSubSystem.Hint    := FAppModules.Language.GetString('ButtonHint.DeleteSupportSubSystem');

    FRgpGrowthType.Caption             := FAppModules.Language.GetString('PlanningGUI.GrowthType');
    FLblSubSystem.Caption              := FAppModules.Language.GetString('PlanningGUI.SubSystem') + ' :';
    FLblDemandCentreID.Caption         := FAppModules.Language.GetString('PlanningGUI.DemandDefName');
    FLblTargetDemand.Caption           := FAppModules.Language.GetString('PlanningGUI.TargetDemand');
    FLblUserCategory.Caption           := FAppModules.Language.GetString('PlanningGUI.Usercategory');
    FLblSupportArc1.Caption            := FAppModules.Language.GetString('PlanningGUI.SupportArc1');
    FLblSupportArc2.Caption            := FAppModules.Language.GetString('PlanningGUI.SupportArc2');
    FLblTargetDemandUnits.Caption      := FAppModules.Language.GetString('PlanningGUI.TargetBaseYearUnit');
    FLblSubSystemGridHeader.Caption    := FAppModules.Language.GetString('PlanningGUI.InterSubSystem');
    FGrpGeneralSupport.Caption         := FAppModules.Language.GetString('PlanningGUI.GeneralSubSystemSupport');
    FGrpSpecificSupport.Caption        := FAppModules.Language.GetString('PlanningGUI.SpecificSubSystemSupport');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionDialog.RestoreColourState;
const OPNAME = 'TFMDemandDefinitionDialog.RestoreColourState';
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

procedure TFMDemandDefinitionDialog.AssignHelpContext;
const OPNAME = 'TFMDemandDefinitionDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
