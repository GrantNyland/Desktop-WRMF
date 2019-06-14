{******************************************************************************}
{*  UNIT      : Contains the class TFMSubSystemsDialog.                       *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMSubSystemsDialog;

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

  TFMSubSystemsDialog = class(TAbstractScrollablePanel)
  private
  protected
    FPnlTop                    : TPanel;
    FPnlBottom                 : TPanel;
    FBtnAddSubSystem           : TFieldBitBtn;
    FBtnDeleteSubSystem        : TFieldBitBtn;
    FTrvSubSystems             : TAbstractTreeView;
    FPgcSubSystems             : TPageControl;
    FTbsProperties             : TTabSheet;
    FTbsCoefficients           : TTabSheet;
    FTbsDemandSupport          : TTabSheet;

    FLblSubSystemName          : TLabel;
    FLblStartDate              : Tlabel;
    FLblEndDate                : TLabel;
    FLblSupporting             : TLabel;
    FLblSubtracted             : TLabel;
    FLblSupportChannel         : TLabel;
    FLblYieldShort             : TLabel;
    FLblLowestStreamFlow       : TLabel;
    FLblYieldLong              : TLabel;
    FLblFirmYield              : TLabel;
    FLblReservoirs             : TLabel;
    FEdtSubSystemName          : TFieldEdit;
    FCbxStartYear              : TFieldComboBox;
    FCbxStartMonth             : TFieldComboBox;
    FCbxEndYear                : TFieldComboBox;
    FCbxEndMonth               : TFieldComboBox;
    FCbxSubtracted             : TFieldComboBox;
    FCbxSupporting             : TFieldComboBox;
    FCbxSupportChannel         : TFieldComboBox;
    FEdtYieldShort             : TFieldEdit;
    FEdtLowestStreamFlow       : TFieldEdit;
    FEdtYieldLong              : TFieldEdit;
    FRgpFirmYield              : TFieldRadioGroup;
    FClbReservoirsInSubSystem  : TFieldCheckListBox;

    FLblStartStoragePerc       : TLabel;
    FLblDecisionCurveSet       : TLabel;
    FGrdStartStoragePerc       : TFieldStringGrid;
    FGrdDecisionCurveSet       : TFieldStringGrid;
    FLblCoefficients           : TLabel;
    FGrdCoefficients           : TFieldStringGrid;

    FGrdRoutingChannels        : TFieldStringGrid;
    FCbxChannel                : TFieldComboBox;
    FRgpSupportCalcType        : TFieldRadioGroup;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property BtnAddSubSystem          : TFieldBitBtn       read FBtnAddSubSystem;
    property BtnDeleteSubSystem       : TFieldBitBtn       read FBtnDeleteSubSystem;
    property TrvSubSystems            : TAbstractTreeView  read FTrvSubSystems;
    property EdtSubSystemName         : TFieldEdit         read FEdtSubSystemName;
    property CbxStartYear             : TFieldComboBox     read FCbxStartYear;
    property CbxStartMonth            : TFieldComboBox     read FCbxStartMonth;
    property CbxEndYear               : TFieldComboBox     read FCbxEndYear;
    property CbxEndMonth              : TFieldComboBox     read FCbxEndMonth;
    property CbxSubtracted            : TFieldComboBox     read FCbxSubtracted;
    property CbxSupporting            : TFieldComboBox     read FCbxSupporting;
    property CbxSupportChannel        : TFieldComboBox     read FCbxSupportChannel;
    property EdtYieldShort            : TFieldEdit         read FEdtYieldShort;
    property EdtLowestStreamFlow      : TFieldEdit         read FEdtLowestStreamFlow;
    property EdtYieldLong             : TFieldEdit         read FEdtYieldLong;
    property RgpFirmYield             : TFieldRadioGroup   read FRgpFirmYield;
    property ClbReservoirsInSubSystem : TFieldCheckListBox read FClbReservoirsInSubSystem;
    property GrdStartStoragePerc      : TFieldStringGrid   read FGrdStartStoragePerc;
    property GrdDecisionCurveSet      : TFieldStringGrid   read FGrdDecisionCurveSet;
    property GrdCoefficients          : TFieldStringGrid   read FGrdCoefficients;
    property GrdRoutingChannels       : TFieldStringGrid   read FGrdRoutingChannels;
    property CbxChannel               : TFieldComboBox     read FCbxChannel;
    property RgpSupportCalcType       : TFieldRadioGroup   read FRgpSupportCalcType;
    property PgcSubSystems            : TPageControl       read FPgcSubSystems;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TFMSubSystemsDialog                                                        *}
{******************************************************************************}

procedure TFMSubSystemsDialog.CreateMemberObjects;
const OPNAME = 'TFMSubSystemsDialog.CreateMemberObjects';
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

    FBtnAddSubSystem       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddSubSystem.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    with FBtnAddSubSystem do
    begin
      Parent  := FPnlTop;
      Left    := 0;
      Top     := 0;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;
    FBtnDeleteSubSystem       := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteSubSystem.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    with FBtnDeleteSubSystem do
    begin
      Parent  := FPnlTop;
      Left    := 75;
      Top     := 0;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;

    FTrvSubSystems     := TAbstractTreeView.Create(lOwner, FAppModules);
    with FTrvSubSystems do
    begin
      Parent           := FPnlBottom;
      Left             := 0;
      Top              := 0;
      Width            := 176;
      Height           := 440;
      Align            := alLeft;
      ReadOnly         := True;
      HideSelection    := False;
      MultiSelect      := FALSE;
    end;
    FPgcSubSystems := TPageControl.Create(lOwner);
    with FPgcSubSystems do
    begin
      Parent           := FPnlBottom;
      Left             := 174;
      Top              := 0;
      Width            := 563;
      Height           := 440;
      Align            := alClient;
    end;
    FTbsProperties           := TTabSheet.Create(lOwner);
    FTbsProperties.Parent    := FPnlBottom;
    FTbsCoefficients         := TTabSheet.Create(lOwner);
    FTbsCoefficients.Parent  := FPnlBottom;
    FTbsDemandSupport        := TTabSheet.Create(lOwner);
    FTbsDemandSupport.Parent := FPnlBottom;

    FLblSubSystemName := TLabel.Create(lOwner);
    with FLblSubSystemName do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 10;
      Width     := 190;
      Height    := 21;
    end;
    FLblStartDate := TLabel.Create(lOwner);
    with FLblStartDate do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 35;
      Width     := 190;
      Height    := 21;
    end;
    FLblEndDate := TLabel.Create(lOwner);
    with FLblEndDate do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 60;
      Width     := 190;
      Height    := 21;
    end;
    FLblSubtracted := TLabel.Create(lOwner);
    with FLblSubtracted do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 85;
      Width     := 190;
      Height    := 21;
    end;
    FLblSupporting := TLabel.Create(lOwner);
    with FLblSupporting do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 110;
      Width     := 190;
      Height    := 21;
    end;
    FLblSupportChannel := TLabel.Create(lOwner);
    with FLblSupportChannel do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 135;
      Width     := 190;
      Height    := 21;
    end;
    FLblYieldShort := TLabel.Create(lOwner);
    with FLblYieldShort do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 160;
      Width     := 190;
      Height    := 21;
    end;
    FLblLowestStreamFlow := TLabel.Create(lOwner);
    with FLblLowestStreamFlow do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 185;
      Width     := 190;
      Height    := 21;
    end;
    FLblYieldLong := TLabel.Create(lOwner);
    with FLblYieldLong do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 210;
      Width     := 190;
      Height    := 21;
    end;
    FLblFirmYield := TLabel.Create(lOwner);
    with FLblFirmYield do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 240;
      Width     := 190;
      Height    := 21;
    end;
    FLblReservoirs := TLabel.Create(lOwner);
    with FLblReservoirs do
    begin
      Parent    := FTbsProperties;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 270;
      Width     := 190;
      Height    := 21;
    end;

    FEdtSubSystemName  := CreateFieldEdit    (FAppModules, lOwner, FTbsProperties, 210,  10, 175, 21, 0, FALSE);
    FCbxStartYear      := CreateFieldComboBox(FAppModules, lOwner, FTbsProperties, 210,  35,  50, 21, 3, TRUE, csDropDownList);
    FCbxStartMonth     := CreateFieldComboBox(FAppModules, lOwner, FTbsProperties, 270,  35,  50, 21, 3, TRUE, csDropDownList);
    FCbxEndYear        := CreateFieldComboBox(FAppModules, lOwner, FTbsProperties, 210,  60,  50, 21, 3, TRUE, csDropDownList);
    FCbxEndMonth       := CreateFieldComboBox(FAppModules, lOwner, FTbsProperties, 270,  60,  50, 21, 3, TRUE, csDropDownList);
    FCbxSubtracted     := CreateFieldComboBox(FAppModules, lOwner, FTbsProperties, 210,  85, 175, 21, 3, TRUE, csDropDownList);
    FCbxSupporting     := CreateFieldComboBox(FAppModules, lOwner, FTbsProperties, 210, 110, 175, 21, 3, TRUE, csDropDownList);
    FCbxSupportChannel := CreateFieldComboBox(FAppModules, lOwner, FTbsProperties, 210, 135, 175, 21, 3, TRUE, csDropDownList);
    FEdtYieldShort     := CreateFieldEdit    (FAppModules, lOwner, FTbsProperties, 210, 160, 120, 21, 0, FALSE);
    FEdtLowestStreamFlow := CreateFieldEdit  (FAppModules, lOwner, FTbsProperties, 210, 185, 120, 21, 0, FALSE);
    FEdtYieldLong      := CreateFieldEdit    (FAppModules, lOwner, FTbsProperties, 210, 210, 120, 21, 0, FALSE);
    FRgpFirmYield      := CreateFieldRadioGroup(FAppModules, lOwner, FTbsProperties, 210, 232, 160, 35, 0, TRUE);
    FRgpFirmYield.Columns := 2;
    FClbReservoirsInSubSystem := CreateFieldCheckListBox(FAppModules, lOwner, FTbsProperties, 210, 275, 200, 123, 0, TRUE);

    FLblStartStoragePerc := TLabel.Create(lOwner);
    with FLblStartStoragePerc do
    begin
      Parent    := FTbsCoefficients;
      Left      := 10;
      Top       := 90;
    end;
    FLblDecisionCurveSet := TLabel.Create(lOwner);
    with FLblDecisionCurveSet do
    begin
      Parent    := FTbsCoefficients;
      Left      := 10;
      Top       := 5;
    end;

    FGrdStartStoragePerc := CreateFieldStringGrid(FAppModules, lOwner, FTbsCoefficients,
                                                 210, 90, 201, 24, 0, TRUE);
    with FGrdStartStoragePerc do
    begin
      ColCount         := 4;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
      Color            := clSilver;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected	];
    end;
    
    FGrdDecisionCurveSet := CreateFieldStringGrid(FAppModules, lOwner, FTbsCoefficients,
                                                 10, 25, 492, 45, 0, TRUE);
    with FGrdDecisionCurveSet do
    begin
      ColCount         := 13;
      RowCount         := 2;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 35;
      Color            := clSilver;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected];
    end;

    FLblCoefficients := TLabel.Create(lOwner);
    with FLblCoefficients do
    begin
      Parent    := FTbsCoefficients;
      Left      := 10;
      Top       := 120;
      Width     := 250;
      Height    := 21;
    end;
    
    FGrdCoefficients := CreateFieldStringGrid(FAppModules, lOwner, FTbsCoefficients,
                                              10, 140, 490, 245, 0, TRUE);
    with FGrdCoefficients do
    begin
      ColCount         := 6;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 80;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    FRgpSupportCalcType := CreateFieldRadioGroup(FAppModules, lOwner, FTbsDemandSupport, 10, 10, 230, 65, 0, TRUE);

    FGrdRoutingChannels := CreateFieldStringGrid(FAppModules, lOwner, FTbsDemandSupport,
                                                 10, 80, 284, 109, 0, TRUE);
    with FGrdRoutingChannels do
    begin
      ColCount         := 2;
      RowCount         := 6;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 200;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
      ScrollBars       := ssNone;
    end;
    FCbxChannel := CreateFieldComboBox(FAppModules, lOwner, FTbsDemandSupport, 93,  24,  200, 21, 3, TRUE, csDropDownList);
    FCbxChannel.Visible := FALSE;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsDialog.Resize;
const OPNAME = 'TFMSubSystemsDialog.Resize';
begin
  inherited Resize;
  try
    FTbsProperties.PageControl := FPgcSubSystems;
    FTbsCoefficients.PageControl := FPgcSubSystems;
    FTbsDemandSupport.PageControl := FPgcSubSystems;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsDialog.Initialise: boolean;
const OPNAME = 'TFMSubSystemsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FGrdDecisionCurveSet.ColWidths[0] := 55;
    FBtnAddSubSystem.Enabled    := (FAppModules.User.UserRights in CUR_EditData) and
                                   (not FAppModules.StudyArea.ScenarioLocked);
    FBtnDeleteSubSystem.Enabled := (FAppModules.User.UserRights in CUR_EditData) and
                                   (not FAppModules.StudyArea.ScenarioLocked);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TFMSubSystemsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FTbsProperties.Caption      := FAppModules.Language.GetString('Tabsheet.Properties');
    FTbsCoefficients.Caption    := FAppModules.Language.GetString('Tabsheet.Coefficients');
    FTbsDemandSupport.Caption   := FAppModules.Language.GetString('Tabsheet.DemandSupport');
    FBtnAddSubSystem.Caption    := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnAddSubSystem.Hint       := FAppModules.Language.GetString('ButtonHint.AddSubSystem');
    FBtnDeleteSubSystem.Caption := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnDeleteSubSystem.Hint    := FAppModules.Language.GetString('ButtonHint.DeleteSubSystem');

    FLblSubSystemName.Caption    := FAppModules.Language.GetString('PlanningGUI.SubSystemName');
    FLblStartDate.Caption        := FAppModules.Language.GetString('PlanningGUI.StartDate');
    FLblEndDate.Caption          := FAppModules.Language.GetString('PlanningGUI.EndDate');
    FLblSubtracted.Caption       := FAppModules.Language.GetString('PlanningGUI.Subtracted');
    FLblSupporting.Caption       := FAppModules.Language.GetString('PlanningGUI.Supporting');
    FLblSupportChannel.Caption   := FAppModules.Language.GetString('PlanningGUI.SupportChannel') + ' :';
    FLblYieldShort.Caption       := FAppModules.Language.GetString('PlanningGUI.YieldShort');
    FLblLowestStreamFlow.Caption := FAppModules.Language.GetString('PlanningGUI.LowestStreamFlow');
    FLblYieldLong.Caption        := FAppModules.Language.GetString('PlanningGUI.YieldLong');
    FLblFirmYield.Caption        := FAppModules.Language.GetString('PlanningGUI.FirmYield');
    FLblReservoirs.Caption       := FAppModules.Language.GetString('PlanningGUI.Reservoirs');
    FLblCoefficients.Caption     := FAppModules.Language.GetString('PlanningGUI.CoefficientData');

    FLblStartStoragePerc.Caption := FAppModules.Language.GetString('PlanningGUI.StartStoragePerc');
    FLblDecisionCurveSet.Caption := FAppModules.Language.GetString('PlanningGUI.DecisionCurveSet');

    FRgpSupportCalcType.Caption  := FAppModules.Language.GetString('PlanningGUI.SupportCalcType');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsDialog.RestoreColourState;
const OPNAME = 'TFMSubSystemsDialog.RestoreColourState';
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

procedure TFMSubSystemsDialog.AssignHelpContext;
const OPNAME = 'TFMSubSystemsDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
