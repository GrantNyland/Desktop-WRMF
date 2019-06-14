{******************************************************************************}
{*  UNIT      : Contains the class TFMUserPriorityClassificationDialog.       *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMUserPriorityClassificationDialog;

interface

uses
  VCL.Forms,
  Classes,
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

  TFMUserPriorityClassificationDialog = class(TAbstractScrollablePanel)
  private
  protected
    FLblNrOfRIs                : TLabel;
    FEdtNrOfRIs                : TFieldEdit;
    FBtnAddRI                  : TFieldBitBtn;
    FBtnDeleteRI               : TFieldBitBtn;
    FLblGrdRIHeading           : TLabel;
    FGrdRIValue                : TFieldStringGrid;
    FGrdRILabel                : TFieldStringGrid;
    FLblUserCategories         : TLabel;
    FBtnAddCategory            : TFieldBitBtn;
    FBtnDeleteCategory         : TFieldBitBtn;
    FLblGrdDistributionHeading : TLabel;
    FGrdDistribution           : TFieldStringGrid;
    FLblAllocationLevels       : TLabel;
    FBtnAddAllocLevel          : TFieldBitBtn;
    FBtnDeleteAllocLevel       : TFieldBitBtn;
    FLblGrdCurtailmentHeading  : TLabel;
    FGrdCurtailment            : TFieldStringGrid;
    FLblAllocLevelWarning      : TLabel;
    FHighLightShape            : TShape;
    FlblHighLight              : TLabel;
    FPanelBottom               : TPanel;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure ResetButtonState(ANrOfReliabilityClasses, ANrOfUserCategories,
                               ANrOfAllocationLevels : integer);
    property EdtNrOfRIs          : TFieldEdit       read FEdtNrOfRIs;
    property BtnAddRI            : TFieldBitBtn     read FBtnAddRI;
    property BtnDeleteRI         : TFieldBitBtn     read FBtnDeleteRI;
    property GrdRIValue          : TFieldStringGrid read FGrdRIValue;
    property GrdRILabel          : TFieldStringGrid read FGrdRILabel;
    property BtnAddCategory      : TFieldBitBtn     read FBtnAddCategory;
    property BtnDeleteCategory   : TFieldBitBtn     read FBtnDeleteCategory;
    property GrdDistribution     : TFieldStringGrid read FGrdDistribution;
    property BtnAddAllocLevel    : TFieldBitBtn     read FBtnAddAllocLevel;
    property BtnDeleteAllocLevel : TFieldBitBtn     read FBtnDeleteAllocLevel;
    property GrdCurtailment      : TFieldStringGrid read FGrdCurtailment;
    property LblAllocLevelWarning : TLabel          read FLblAllocLevelWarning;
  end;

implementation

uses
  VCL.Controls,
  SysUtils,
  VCL.Grids,
  UConstants,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TFMUserPriorityClassificationDialog                                        *}
{******************************************************************************}

procedure TFMUserPriorityClassificationDialog.CreateMemberObjects;
const OPNAME = 'TFMUserPriorityClassificationDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                     Left  Top Width Height
    FLblNrOfRIs      := CreateFieldLabel                  (lOwner, lParent,  10,  10, 130, 13);
    FEdtNrOfRIs      := CreateFieldEdit      (FAppModules, lOwner, lParent, 150,  10,  30, 21, 0, TRUE);

    FBtnAddRI       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddRI.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORADDSERIES');
    FBtnAddRI.Parent := lParent;
    with FBtnAddRI do
    begin
      Left    := 200;
      Top     := 10;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;
    FBtnDeleteRI       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteRI.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORREMOVESERIES');
    FBtnDeleteRI.Parent := lParent;
    with FBtnDeleteRI do
    begin
      Left    := 280;
      Top     := 10;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;

    FLblGrdRIHeading := CreateFieldLabel                  (lOwner, lParent,  10,  35, 215, 13);
    FGrdRIValue      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10,  50, 308, 23, 0, TRUE);
    with FGrdRIValue do
    begin
      ScrollBars       := ssNone;
      ColCount         := 6;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 60;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    FGrdRILabel      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10,  74, 308, 23, 0, TRUE);
    with FGrdRILabel do
    begin
      ScrollBars       := ssNone;
      ColCount         := 6;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 60;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    FLblUserCategories    := CreateFieldLabel(lOwner, lParent,  10, 110,  80, 13);
    FBtnAddCategory       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddCategory.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    FBtnAddCategory.Parent := lParent;
    with FBtnAddCategory do
    begin
      Left    := 200;
      Top     := 103;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;
    FBtnDeleteCategory       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteCategory.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    FBtnDeleteCategory.Parent := lParent;
    with FBtnDeleteCategory do
    begin
      Left    := 280;
      Top     := 103;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;
    FLblGrdDistributionHeading := CreateFieldLabel             (lOwner, lParent,  10, 130, 200, 13);
    FGrdDistribution      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10,  45, 308, 45, 0, TRUE);
    with FGrdDistribution do
    begin
      Left             := 10;
      Top              := 145;
      Width            := 495;
      Height           := 135;
      ColCount         := 7;
      DefaultColWidth  := 60;
      DefaultRowHeight := 20;
      RowCount         := 7;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    FLblAllocationLevels  := CreateFieldLabel(lOwner, lParent,  10, 295, 140, 13);
    FBtnAddAllocLevel     := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddAllocLevel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATECHART');
    FBtnAddAllocLevel.Parent := lParent;
    with FBtnAddAllocLevel do
    begin
      Left    := 200;
      Top     := 288;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;
    FBtnDeleteAllocLevel  := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteAllocLevel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETECHART');
    FBtnDeleteAllocLevel.Parent := lParent;
    with FBtnDeleteAllocLevel do
    begin
      Left    := 280;
      Top     := 288;
      Width   := 75;
      Height  := 25;
      ShowHint := True;
    end;
    FLblGrdCurtailmentHeading := CreateFieldLabel             (lOwner, lParent,  10, 315, 220, 13);
    FGrdCurtailment      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10,  45, 308, 45, 0, TRUE);
    with FGrdCurtailment do
    begin
      Left             := 10;
      Top              := 330;
      Width            := 498;
      Height           := 135;
      ColCount         := 7;
      DefaultColWidth  := 60;
      DefaultRowHeight := 20;
      RowCount         := 7;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FLblAllocLevelWarning := CreateFieldLabel(lOwner, lParent, 520, 330, 200, 13);
    with FLblAllocLevelWarning do
    begin
      AutoSize   := FALSE;
      WordWrap   := TRUE;
      Layout     := tlTop;
      Height     := 55;
      Font.Color := clRed;
      Font.Style := [fsBold];
      Visible    := FALSE;
    end;
    FPanelBottom             := TPanel.Create(LOwner);
    FPanelBottom.Parent      := LParent;
    FPanelBottom.Align       := alBottom;
    FPanelBottom.Height      := 50;
    FPanelBottom.BorderStyle := bsNone;
    FPanelBottom.BevelInner  := bvNone;
    FPanelBottom.BevelOuter  := bvNone;

    FHighLightShape        := TShape.Create(LOwner);
    FHighLightShape.Parent := FPanelBottom;
    FHighLightShape.Left   := 20;
    FHighLightShape.Top    := 10;
    FHighLightShape.Width  := 20;
    FHighLightShape.Height := 15;
    FHighLightShape.Brush.Color := clYellow;

    FlblHighLight          := TLabel.Create(LOwner);
    FlblHighLight.Parent   := FPanelBottom;
    FlblHighLight.Left     := 50;
    FlblHighLight.Top      := 10;
    FlblHighLight.AutoSize := True;
    //FlblHighLight.Width    := 80;
    //FlblHighLight.Height   := 13;
    FlblHighLight.Layout   := tlCenter;
    FlblHighLight.Caption  := FAppModules.Language.GetString('LabelText.DummyValues');


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationDialog.Resize;
const OPNAME = 'TFMUserPriorityClassificationDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMUserPriorityClassificationDialog.Initialise: boolean;
const OPNAME = 'TFMUserPriorityClassificationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationDialog.ResetButtonState(ANrOfReliabilityClasses,
                                                               ANrOfUserCategories,
                                                               ANrOfAllocationLevels : integer);
const OPNAME = 'TFMUserPriorityClassificationDialog.ResetButtonState';
begin
  try
    BtnAddRI.Enabled            := (FAppModules.User.UserRights in CUR_EditData) AND
                                   (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                   (ANrOfReliabilityClasses < 5);
    BtnDeleteRI.Enabled         := (FAppModules.User.UserRights in CUR_EditData) AND
                                   (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                   (ANrOfReliabilityClasses > 0);
    BtnAddCategory.Enabled      := (FAppModules.User.UserRights in CUR_EditData) AND
                                   (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                   (ANrOfUserCategories < 10);
    BtnDeleteCategory.Enabled   := (FAppModules.User.UserRights in CUR_EditData) AND
                                   (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                   (ANrOfUserCategories > 0);
    BtnAddAllocLevel.Enabled    := (FAppModules.User.UserRights in CUR_EditData) AND
                                   (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                   (ANrOfAllocationLevels < 5);
    BtnDeleteAllocLevel.Enabled := (FAppModules.User.UserRights in CUR_EditData) AND
                                   (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                   (ANrOfAllocationLevels > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFMUserPriorityClassificationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TFMUserPriorityClassificationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblNrOfRIs.Caption                := FAppModules.Language.GetString('PlanningGUI.NrOfReliabilityClasses');
    FLblGrdRIHeading.Caption           := FAppModules.Language.GetString('PlanningGUI.RILabel');
    FLblUserCategories.Caption         := FAppModules.Language.GetString('PlanningGUI.UserCategoryName');
    FLblGrdDistributionHeading.Caption := FAppModules.Language.GetString('PlanningGUI.Distribution');
    FLblAllocationLevels.Caption       := FAppModules.Language.GetString('PlanningGUI.AllocCurtailment');
    FLblGrdCurtailmentHeading.Caption  := FAppModules.Language.GetString('PlanningGUI.DemandCurtailment');
    FBtnAddRI.Caption                  := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnDeleteRI.Caption               := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnAddCategory.Caption            := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnDeleteCategory.Caption         := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnAddAllocLevel.Caption          := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnDeleteAllocLevel.Caption       := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnAddRI.Hint                     := FAppModules.Language.GetString('ButtonHint.AddRI');
    FBtnDeleteRI.Hint                  := FAppModules.Language.GetString('ButtonHint.DeleteRI');
    FBtnAddCategory.Hint               := FAppModules.Language.GetString('ButtonHint.AddCategory');
    FBtnDeleteCategory.Hint            := FAppModules.Language.GetString('ButtonHint.DeleteCategory');
    FBtnAddAllocLevel.Hint             := FAppModules.Language.GetString('ButtonHint.AddAllocLevel');
    FBtnDeleteAllocLevel.Hint          := FAppModules.Language.GetString('ButtonHint.DeleteAllocLevel');
    FLblAllocLevelWarning.Caption      := FAppModules.Language.GetString('PlanningGUI.AllocLevelWarning');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationDialog.RestoreColourState;
const OPNAME = 'TFMUserPriorityClassificationDialog.RestoreColourState';
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

procedure TFMUserPriorityClassificationDialog.AssignHelpContext;
const OPNAME = 'TFMUserPriorityClassificationDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
