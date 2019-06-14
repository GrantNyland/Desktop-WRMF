{******************************************************************************}
{*  UNIT      : Contains the class TYieldRunYRCDialog.                        *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/07/22                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UYieldRunYRCDialog;

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

  TFieldChkBoxArray = array[1..10] of TFieldChkBox;

  TYieldRunYRCDialog = class(TAbstractScrollablePanel)
  private
  protected
    FPageControl                  : TPageControl;
    FTabSheet1                    : TTabSheet;
    FTabSheet2                    : TTabSheet;
    FTabSheet3                    : TTabSheet;
    FRunTypeRadioGroup            : TFieldRadioGroup;
    FSummaryLevelRadioGroup       : TFieldRadioGroup;
    FSummaryOutputWarningLabel    : TLabel;
    FGroupBox1                    : TGroupBox;
    FGroupBox2                    : TGroupBox;
    FGroupBox3                    : TGroupBox;
    FFirmYieldChkBox              : TFieldChkBox;
    FFirmYieldWarningLabel        : TLabel;
    FMultiplePeriodsChkBox        : TFieldChkBox;
    FMultiplePeriodsWarningLabel  : TLabel;
    FNumberOfYearsLabel           : TLabel;
    FNumberOfYearsEdit            : TFieldEdit;
    FNumberOfYearsWarningLabel    : TLabel;

    FTargetYieldLabel             : TLabel;
    FMaxYieldLabel                : TLabel;
    FPowerDemandLabel             : TLabel;
    FWaterLoadCasesGrid           : TFieldStringGrid;
    FPowerLoadCasesGrid           : TFieldStringGrid;
    FIncludeInAnalysis            : TLabel;
    FLoadCaseSelectedChkBox       : TFieldChkBoxArray;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property PageControl               : TPageControl     read FPageControl;
    property TabSheet1                 : TTabSheet        read FTabSheet1;
    property TabSheet2                 : TTabSheet        read FTabSheet2;
    property TabSheet3                 : TTabSheet        read FTabSheet3;
    property RunTypeRadioGroup         : TFieldRadioGroup read FRunTypeRadioGroup;
    property SummaryLevelRadioGroup    : TFieldRadioGroup read FSummaryLevelRadioGroup;
    property FirmYieldChkBox           : TFieldChkBox     read FFirmYieldChkBox;
    property NumberOfYearsEdit         : TFieldEdit       read FNumberOfYearsEdit;
    property MultiplePeriodsChkBox     : TFieldChkBox     read FMultiplePeriodsChkBox;
    property MultiplePeriodsWarningLabel : TLabel         read FMultiplePeriodsWarningLabel;
    property WaterLoadCasesGrid        : TFieldStringGrid read FWaterLoadCasesGrid;
    property PowerLoadCasesGrid        : TFieldStringGrid read FPowerLoadCasesGrid;
    property SummaryOutputWarningLabel : TLabel           read FSummaryOutputWarningLabel;
    property FirmYieldWarningLabel     : TLabel           read FFirmYieldWarningLabel;
    property NumberOfYearsWarningLabel : TLabel           read FNumberOfYearsWarningLabel;
    function LoadCaseSelectedChkBox(AIndex : integer) : TFieldChkBox;
    function IndexOfLoadCaseCheckBox (AChkBox : TFieldChkBox) : integer;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities,
  VCL.ImgList, VCL.Grids;

{******************************************************************************}
{* TYieldRunYRCDialog                                                         *}
{******************************************************************************}

procedure TYieldRunYRCDialog.CreateMemberObjects;
const OPNAME = 'TYieldRunYRCDialog.CreateMemberObjects';
var
  lIndex  : integer;
  lChkBox : TFieldChkBox;
  lOwner  : TComponent;
begin
  inherited;
  try
    lOwner  := ControlsOwner;

    FPageControl := TPageControl.Create(lOwner);
    with FPageControl do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 0;
      Width      := 688;
      Height     := 451;
      Align      := alClient;
      Style      := tsFlatButtons;
      TabOrder   := 0;
    end;

    FTabSheet1 := TTabSheet.Create(lOwner);
    FTabSheet1.Parent := FPageControl;
    FTabSheet1.TabVisible := FALSE;

    FTabSheet2 := TTabSheet.Create(lOwner);
    FTabSheet2.Parent := FPageControl;
    FTabSheet2.TabVisible := FALSE;

    FTabSheet3 := TTabSheet.Create(lOwner);
    FTabSheet3.Parent := FPageControl;
    FTabSheet3.TabVisible := FALSE;

    FRunTypeRadioGroup      := CreateFieldRadioGroup (FAppModules, lOwner, FTabSheet1,  10,   5, 200,  80, 0, FALSE);
    FSummaryLevelRadioGroup := CreateFieldRadioGroup (FAppModules, lOwner, FTabSheet1,  10, 100, 200, 100, 1, TRUE);
    FSummaryOutputWarningLabel := CreateFieldLabel                (lOwner, FTabSheet1, 220, 100, 200, 100);
    with FSummaryOutputWarningLabel do
    begin
      Anchors    := [akLeft, akTop, akRight];
      AutoSize   := FALSE;
      Font.Color := clRed;
      Font.Style := [fsBold];
      Layout     := tlCenter;
      WordWrap   := TRUE;
    end;

    FGroupBox1 := TGroupBox.Create(lOwner);
    with FGroupBox1 do
    begin
      Parent   := FTabSheet2;
      Left     := 0;
      Top      := 0;
      Width    := 680;
      Height   := 80;
      Align    := alTop;
      TabOrder := 0;
    end;
    FFirmYieldChkBox       := CreateFieldChkBox (FAppModules, lOwner, FGroupBox1,   8, 32, 165, 17, 0, TRUE, taLeftJustify);
    FFirmYieldWarningLabel := CreateFieldLabel               (lOwner, FGroupBox1, 220, 20, 450, 40);
    with FFirmYieldWarningLabel do
    begin
      Anchors    := [akLeft, akTop, akRight];
      AutoSize   := FALSE;
      Font.Color := clRed;
      Font.Style := [fsBold];
      Layout     := tlCenter;
      WordWrap   := TRUE;
    end;

    FGroupBox2 := TGroupBox.Create(lOwner);
    with FGroupBox2 do
    begin
      Parent   := FTabSheet2;
      Left     := 0;
      Top      := 80;
      Width    := 680;
      Height   := 80;
      Align    := alTop;
      TabOrder := 1;
    end;
    FMultiplePeriodsChkBox := CreateFieldChkBox (FAppModules, lOwner, FGroupBox2,   8, 22, 165, 17, 0, TRUE, taLeftJustify);
    FMultiplePeriodsWarningLabel := CreateFieldLabel         (lOwner, FGroupBox2, 220,  8, 450, 40);
    with FMultiplePeriodsWarningLabel do
    begin
      Anchors    := [akLeft, akTop, akRight];
      AutoSize   := FALSE;
      Font.Color := clRed;
      Font.Style := [fsBold];
      Layout     := tlCenter;
      WordWrap   := TRUE;
    end;

    FGroupBox3 := TGroupBox.Create(lOwner);
    with FGroupBox3 do
    begin
      Parent   := FTabSheet2;
      Left     := 0;
      Top      := 160;
      Width    := 680;
      Height   := 80;
      Align    := alTop;
      TabOrder := 2;
    end;

    FNumberOfYearsEdit         := CreateFieldEdit   (FAppModules, lOwner, FGroupBox3, 160, 30,  35, 21, 0, TRUE);
    FNumberOfYearsLabel        := CreateFieldLabel               (lOwner, FGroupBox3,  10, 34, 143, 13);
    FNumberOfYearsWarningLabel := CreateFieldLabel               (lOwner, FGroupBox3, 220, 20, 450, 40);
    with FNumberOfYearsWarningLabel do
    begin
      Anchors    := [akLeft, akTop, akRight];
      AutoSize   := FALSE;
      Font.Color := clRed;
      Font.Style := [fsBold];
      Layout     := tlCenter;
      WordWrap   := TRUE;
    end;

    FTargetYieldLabel    := CreateFieldLabel (lOwner, FTabSheet3,  10, 10, 80, 50);
    FMaxYieldLabel       := CreateFieldLabel (lOwner, FTabSheet3,  90, 10, 80, 50);
    FPowerDemandLabel    := CreateFieldLabel (lOwner, FTabSheet3, 180, 10, 80, 50);
    FIncludeInAnalysis   := CreateFieldLabel (lOwner, FTabSheet3, 268, 10, 40, 50);
    FTargetYieldLabel.Alignment  := taCenter;
    FTargetYieldLabel.WordWrap   := TRUE;
    FMaxYieldLabel.Alignment     := taCenter;
    FMaxYieldLabel.WordWrap      := TRUE;
    FPowerDemandLabel.Alignment  := taCenter;
    FPowerDemandLabel.WordWrap   := TRUE;
    FIncludeInAnalysis.Alignment := taCenter;
    FIncludeInAnalysis.WordWrap  := TRUE;
    FWaterLoadCasesGrid  := CreateFieldStringGrid(FAppModules, lOwner, FTabSheet3,  10, 55, 165, 213, 12, TRUE);
    with WaterLoadCasesGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 10;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 80;
    end;
    FPowerLoadCasesGrid  := CreateFieldStringGrid(FAppModules, lOwner, FTabSheet3, 180, 55,  83, 213, 13, TRUE);
    with PowerLoadCasesGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 1;
      RowCount         := 10;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 80;
    end;
    for lIndex := 1 to 10 do
    begin
      lChkBox := CreateFieldChkBox(FAppModules, lOwner, FTabSheet3, 280, 55+(lIndex-1)*21, 25, 20, 1+lIndex, TRUE, taRightJustify);
      lChkBox.Caption := '  ';
      FLoadCaseSelectedChkBox[lIndex] := lChkBox;
    end;

    FPageControl.ActivePage := FTabSheet1
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCDialog.Resize;
const OPNAME = 'TYieldRunYRCDialog.Resize';
begin
  inherited Resize;
  try
    FTabSheet1.PageControl := FPageControl;
    FTabSheet2.PageControl := FPageControl;
    FTabSheet3.PageControl := FPageControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCDialog.Initialise: boolean;
const OPNAME = 'TYieldRunYRCDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCDialog.LanguageHasChanged: boolean;
const OPNAME = 'TYieldRunYRCDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FRunTypeRadioGroup.Caption       := ' ' + FAppModules.Language.GetString('TField.RunType') + ' ';
    FRunTypeRadioGroup.Hint          := FAppModules.Language.GetString('TRunConfigurationDialog.RunTypeRadioGroup');
    FSummaryLevelRadioGroup.Caption  := ' ' + FAppModules.Language.GetString('TField.SummaryLevel') + ' ';

    FFirmYieldChkBox.Caption         := FAppModules.Language.GetString('TField.CalcHistoryOpt') + ' :';
    FMultiplePeriodsChkBox.Caption   := FAppModules.Language.GetString('TField.MultPeriodOpt') + ' :';
    FNumberOfYearsLabel.Caption      := FAppModules.Language.GetString('TField.YearsCount')  + ' :';

    FTargetYieldLabel.Caption        := FAppModules.Language.GetString('MasterControl.TargetSystemYield');
    FMaxYieldLabel.Caption           := FAppModules.Language.GetString('MasterControl.MaximumSystemYield');
    FPowerDemandLabel.Caption        := FAppModules.Language.GetString('MasterControl.TargetPowerDemand');
    FIncludeInAnalysis.Caption       := FAppModules.language.GetString('MasterControl.IncludeInAnalysis');

    FMultiplePeriodsWarningLabel.Caption := FAppModules.Language.GetString('LabelCaption.MultiplePeriodsWarning');
    FFirmYieldWarningLabel.Caption       := FAppModules.Language.GetString('LabelCaption.FirmYieldWarning');
    FNumberOfYearsWarningLabel.Caption   := FAppModules.Language.GetString('LabelCaption.NumberOfYearsWarning');
    FSummaryOutputWarningLabel.Caption   := FAppModules.Language.GetString('LabelCaption.SummaryOutputWarning');

    FSummaryLevelRadioGroup.Hints.Clear;
    FSummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescBrief'));
    FSummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescAdditional'));
    FSummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescFull'));

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCDialog.AssignHelpContext;
const OPNAME = 'TYieldRunYRCDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(FRunTypeRadioGroup,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FSummaryLevelRadioGroup, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FFirmYieldChkBox,        HC_WaterResourcesYieldModel);
    SetControlHelpContext(FMultiplePeriodsChkBox,  HC_WaterResourcesYieldModel);
    SetControlHelpContext(FNumberOfYearsEdit,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FWaterLoadCasesGrid,     HC_WaterResourcesYieldModel);
    SetControlHelpContext(FPowerLoadCasesGrid,     HC_WaterResourcesYieldModel);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunYRCDialog.RestoreColourState;
const OPNAME = 'TYieldRunYRCDialog.RestoreColourState';
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

function TYieldRunYRCDialog.LoadCaseSelectedChkBox(AIndex : integer) : TFieldChkBox;
const OPNAME = 'TYieldRunYRCDialog.LoadCaseSelectedChkBox';
begin
  result := nil;
  try
    result := FLoadCaseSelectedChkBox[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCDialog.IndexOfLoadCaseCheckBox (AChkBox : TFieldChkBox) : integer;
const OPNAME = 'TYieldRunYRCDialog.IndexOfLoadCaseCheckBox';
var
  lIndexA : integer;
begin
  Result := -1;
  try
    lIndexA := 1;
    while ((result = -1) AND (lIndexA <= 10)) do
    begin
      if (AChkBox = FLoadCaseSelectedChkBox[lIndexA]) then
        Result := lIndexA
      else
        lIndexA := lIndexA + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
