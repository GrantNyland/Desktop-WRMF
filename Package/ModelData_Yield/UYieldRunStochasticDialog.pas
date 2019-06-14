{******************************************************************************}
{*  UNIT      : Contains the class TYieldRunStochasticDialog.                 *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/07/22                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UYieldRunStochasticDialog;

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

  TYieldRunStochasticDialog = class(TAbstractScrollablePanel)
  private
  protected
    FPageControl                  : TPageControl;
    FTabSheet1                    : TTabSheet;
    FTabSheet2                    : TTabSheet;
    FTabSheet3                    : TTabSheet;
    FTabSheet4                    : TTabSheet;
    FRunTypeRadioGroup            : TFieldRadioGroup;
    FSummaryLevelRadioGroup       : TFieldRadioGroup;
    FSummaryOutputWarningLabel    : TLabel;
    FGroupBox1                    : TGroupBox;
    FGroupBox2                    : TGroupBox;
    FGroupBox3                    : TGroupBox;
    FGroupBox4                    : TGroupBox;
    FGroupBox5                    : TGroupBox;
    FGroupBox6                    : TGroupBox;
    FMultiplePeriodsChkBox        : TFieldChkBox;
    FMultiplePeriodsWarningLabel  : TLabel;
    FReduceSequencesChkBox        : TFieldChkBox;
    FReduceSeqWarningLabel        : TLabel;
    FStartTypeRadioGroup          : TFieldRadioGroup;
    FStartTypeWarningLabel        : TLabel;
    FNrOfSequencesLabel           : TLabel;
    FNrOfSequencesEdit            : TFieldEdit;
    FStartSequenceLabel           : TLabel;
    FStartSequenceEdit            : TFieldEdit;
    FSequenceOrderLabel           : TLabel;
    FSequenceOrderGrid            : TFieldStringGrid;

    FNumberOfYearsLabel           : TLabel;
    FNumberOfYearsEdit            : TFieldEdit;

    FTargetYieldLabel             : TLabel;
    FMaxYieldLabel                : TLabel;
    FPowerDemandLabel             : TLabel;
    FWaterLoadCasesGrid           : TFieldStringGrid;
    FPowerLoadCasesGrid           : TFieldStringGrid;
    FIncludeInAnalysis            : TLabel;
    FLoadCaseSelectedChkBox       : TFieldChkBoxArray;

    FDemandChannelGrid            : TFieldStringGrid;
    FSpecifiedDemandWarningLabel  : TLabel;
    FSetHistoric                  : TButton;
    FSetStochastic                : TButton;

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
    property TabSheet4                 : TTabSheet        read FTabSheet4;
    property RunTypeRadioGroup         : TFieldRadioGroup read FRunTypeRadioGroup;
    property SummaryLevelRadioGroup    : TFieldRadioGroup read FSummaryLevelRadioGroup;
    property MultiplePeriodsChkBox     : TFieldChkBox     read FMultiplePeriodsChkBox;
    property ReduceSequencesChkBox     : TFieldChkBox     read FReduceSequencesChkBox;
    property NumberOfYearsEdit         : TFieldEdit       read FNumberOfYearsEdit;
    property WaterLoadCasesGrid        : TFieldStringGrid read FWaterLoadCasesGrid;
    property PowerLoadCasesGrid        : TFieldStringGrid read FPowerLoadCasesGrid;
    property DemandChannelGrid         : TFieldStringGrid read FDemandChannelGrid;
    property SummaryOutputWarningLabel    : TLabel        read FSummaryOutputWarningLabel;
    property MultiplePeriodsWarningLabel  : TLabel        read FMultiplePeriodsWarningLabel;
    property ReduceSequencesWarningLabel  : TLabel        read FReduceSeqWarningLabel;
    property StartTypeRadioGroup          : TFieldRadioGroup     read FStartTypeRadioGroup;
    property StartTypeWarningLabel        : TLabel               read FStartTypeWarningLabel;
    property NrOfSequencesLabel           : TLabel               read FNrOfSequencesLabel;
    property NrOfSequencesEdit            : TFieldEdit           read FNrOfSequencesEdit;
    property StartSequenceLabel           : TLabel               read FStartSequenceLabel;
    property StartSequenceEdit            : TFieldEdit           read FStartSequenceEdit;
    property SequenceOrderLabel           : TLabel               read FSequenceOrderLabel;
    property SequenceOrderGrid            : TFieldStringGrid     read FSequenceOrderGrid;
    property SpecifiedDemandWarningLabel  : TLabel        read FSpecifiedDemandWarningLabel;
    property SetHistoricButton            : TButton       read FSetHistoric;
    property SetStochasticButton          : TButton       read FSetStochastic;
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
{* TYieldRunStochasticDialog                                                  *}
{******************************************************************************}

procedure TYieldRunStochasticDialog.CreateMemberObjects;
const OPNAME = 'TYieldRunStochasticDialog.CreateMemberObjects';
var
  lIndex  : integer;
  lChkBox : TFieldChkBox;
  lOwner  : TComponent;
//  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
//    lParent := ControlsParent;

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

    FTabSheet4 := TTabSheet.Create(lOwner);
    FTabSheet4.Parent := FPageControl;
    FTabSheet4.TabVisible := FALSE;

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
      Height   := 50;
      Align    := alTop;
      TabOrder := 0;
    end;
    FMultiplePeriodsChkBox := CreateFieldChkBox (FAppModules, lOwner, FGroupBox1,   8, 22, 187, 17, 0, TRUE, taLeftJustify);
    FMultiplePeriodsWarningLabel := CreateFieldLabel         (lOwner, FGroupBox1, 220,  8, 450, 40);
    with FMultiplePeriodsWarningLabel do
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
      Top      := 50;
      Width    := 680;
      Height   := 50;
      Align    := alTop;
      TabOrder := 1;
    end;
    FReduceSequencesChkBox := CreateFieldChkBox (FAppModules, lOwner, FGroupBox2,   8, 22, 187, 17, 0, TRUE, taLeftJustify);
    FReduceSeqWarningLabel := CreateFieldLabel               (lOwner, FGroupBox2, 220,  8, 450, 40);
    with FReduceSeqWarningLabel do
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
      Top      := 100;
      Width    := 680;
      Height   := 100;
      Align    := alTop;
      TabOrder := 2;
    end;
    FStartTypeRadioGroup
      := CreateFieldRadioGroup(FAppModules, lOwner, FGroupBox3, 10, 8, 190,  85, 2, FALSE);
    FStartTypeWarningLabel := CreateFieldLabel(lOwner, FGroupBox3, 220,  8, 450, 40);
    with FStartTypeWarningLabel do
    begin
      Anchors    := [akLeft, akTop, akRight];
      AutoSize   := FALSE;
      Font.Color := clRed;
      Font.Style := [fsBold];
      Layout     := tlCenter;
      WordWrap   := TRUE;
    end;

    FGroupBox4 := TGroupBox.Create(lOwner);
    with FGroupBox4 do
    begin
      Parent   := FTabSheet2;
      Left     := 0;
      Top      := 200;
      Width    := 680;
      Height   := 50;
      Align    := alTop;
      TabOrder := 2;
    end;
    FNrOfSequencesLabel := CreateFieldLabel             (lOwner, FGroupBox4,  10, 15, 175,  21);
    FNrOfSequencesEdit  := CreateFieldEdit (FAppModules, lOwner, FGroupBox4, 190, 15,  40,  21, 3, TRUE);

    FGroupBox5 := TGroupBox.Create(lOwner);
    with FGroupBox5 do
    begin
      Parent   := FTabSheet2;
      Left     := 0;
      Top      := 250;
      Width    := 680;
      Height   := 65;
      Align    := alTop;
      TabOrder := 2;
    end;
    FStartSequenceLabel := CreateFieldLabel                  (lOwner, FGroupBox5, 10, 10, 175,  21);
    FStartSequenceEdit  := CreateFieldEdit      (FAppModules, lOwner, FGroupBox5, 10, 35,  40,  21, 4, TRUE);
    FSequenceOrderLabel := CreateFieldLabel                  (lOwner, FGroupBox5, 10, 10, 175,  21);
    FSequenceOrderGrid  := CreateFieldStringGrid(FAppModules, lOwner, FGroupBox5, 10, 35, 413,  22, 5, TRUE);
    with FSequenceOrderGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 10;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
    end;

    FGroupBox6 := TGroupBox.Create(lOwner);
    with FGroupBox6 do
    begin
      Parent   := FTabSheet2;
      Left     := 0;
      Top      := 315;
      Width    := 680;
      Height   := 50;
      Align    := alTop;
      TabOrder := 5;
    end;
    FNumberOfYearsEdit         := CreateFieldEdit   (FAppModules, lOwner, FGroupBox6, 160, 15,  35, 21, 0, TRUE);
    FNumberOfYearsLabel        := CreateFieldLabel               (lOwner, FGroupBox6,  10, 15, 143, 21);

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

    FDemandChannelGrid  := CreateFieldStringGrid(FAppModules, lOwner, FTabSheet4,  10, 10, 390, 300, 12, TRUE);
    with FDemandChannelGrid do
    begin
      ScrollBars       := ssBoth;
      ColCount         := 3;
      RowCount         := 2;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 30;
      ColWidths[1]     := 300;
    end;
    FSpecifiedDemandWarningLabel := CreateFieldLabel(lOwner, FTabSheet4, 10, 315, 450, 30);
    with FSpecifiedDemandWarningLabel do
    begin
      Anchors    := [akLeft, akTop, akRight];
      AutoSize   := FALSE;
      Font.Color := clRed;
      Font.Style := [fsBold];
      Layout     := tlCenter;
      TabStop    := FALSE;
    end;

    FSetHistoric := TButton.Create(lOwner);
    with FSetHistoric do
    begin
      Parent   := FTabSheet4;
      Left     := 10;
      Top      := 350;
      Width    := 390;
      Height   := 25;
      TabOrder := 0;
    end;

    FSetStochastic := TButton.Create(lOwner);
    with FSetStochastic do
    begin
      Parent   := FTabSheet4;
      Left     := 10;
      Top      := 380;
      Width    := 390;
      Height   := 25;
      TabOrder := 0;
    end;

    FPageControl.ActivePage := FTabSheet1
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticDialog.Resize;
const OPNAME = 'TYieldRunStochasticDialog.Resize';
begin
  inherited Resize;
  try
    FTabSheet1.PageControl := FPageControl;
    FTabSheet2.PageControl := FPageControl;
    FTabSheet3.PageControl := FPageControl;
    FTabSheet4.PageControl := FPageControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticDialog.Initialise: boolean;
const OPNAME = 'TYieldRunStochasticDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticDialog.LanguageHasChanged: boolean;
const OPNAME = 'TYieldRunStochasticDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FRunTypeRadioGroup.Caption           := ' ' + FAppModules.Language.GetString('TField.RunType') + ' ';
    FRunTypeRadioGroup.Hint              := FAppModules.Language.GetString('TRunConfigurationDialog.RunTypeRadioGroup');
    FSummaryLevelRadioGroup.Caption      := ' ' + FAppModules.Language.GetString('TField.SummaryLevel') + ' ';

    FMultiplePeriodsChkBox.Caption       := FAppModules.Language.GetString('TField.MultPeriodOpt') + ' :';
    FReduceSequencesChkBox.Caption       := FAppModules.Language.GetString('TField.ReduceSeqOpt') + ' :';
    FStartTypeRadioGroup.Caption         := ' ' + FAppModules.Language.GetString('TRunConfigurationDialog.StartTypeRadioGroup') + ' ';
    FNrOfSequencesLabel.Caption          := FAppModules.Language.GetString('TField.HydroSeqCount') + ' :';
    FStartSequenceLabel.Caption          := FAppModules.Language.GetString('RunParameters.SequenceStartNumber') + ' :';
    FSequenceOrderLabel.Caption          := FAppModules.Language.GetString('RunParameters.SequenceOrder') + ' :';

    FNumberOfYearsLabel.Caption          := FAppModules.Language.GetString('TField.YearsCount')  + ' :';

    FTargetYieldLabel.Caption            := FAppModules.Language.GetString('MasterControl.TargetSystemYield');
    FMaxYieldLabel.Caption               := FAppModules.Language.GetString('MasterControl.MaximumSystemYield');
    FPowerDemandLabel.Caption            := FAppModules.Language.GetString('MasterControl.TargetPowerDemand');
    FIncludeInAnalysis.Caption           := FAppModules.language.GetString('MasterControl.IncludeInAnalysis');

    FDemandChannelGrid.Cells[1,0]        := FAppModules.Language.GetString('ViewData.ChannelDetails11');
    FDemandChannelGrid.Cells[2,0]        := 'H/S';

    FMultiplePeriodsWarningLabel.Caption := FAppModules.language.GetString('MasterControl.MultiplePeriodWarning');
    FReduceSeqWarningLabel.Caption       := FAppModules.language.GetString('MasterControl.ReduceSeqWarning');
    FStartTypeWarningLabel.Caption       := FAppModules.language.GetString('MasterControl.StartTypeWarning');
    FSummaryOutputWarningLabel.Caption   := FAppModules.language.GetString('MasterControl.SummaryOutputWarning');
    FSpecifiedDemandWarningLabel.Caption := FAppModules.language.GetString('MasterControl.StochasticDemandWarning');
    FSetHistoric.Caption                 := 'MasterControl.SetHistoric';
    FSetStochastic.Caption               := 'MasterControl.SetStochastic';

    FSummaryLevelRadioGroup.Hints.Clear;
    FSummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescBrief'));
    FSummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescAdditional'));
    FSummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescFull'));
    FStartTypeRadioGroup.Hint       := FAppModules.Language.GetString('TFiled.StartTypeDescr');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticDialog.AssignHelpContext;
const OPNAME = 'TYieldRunStochasticDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(FRunTypeRadioGroup,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FSummaryLevelRadioGroup, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FMultiplePeriodsChkBox,  HC_WaterResourcesYieldModel);
    SetControlHelpContext(FReduceSequencesChkBox,  HC_WaterResourcesYieldModel);
    SetControlHelpContext(FStartTypeRadioGroup,    HC_WaterResourcesYieldModel);
    SetControlHelpContext(FNrOfSequencesEdit,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FStartSequenceEdit,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FNumberOfYearsEdit,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FWaterLoadCasesGrid,     HC_WaterResourcesYieldModel);
    SetControlHelpContext(FPowerLoadCasesGrid,     HC_WaterResourcesYieldModel);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunStochasticDialog.RestoreColourState;
const OPNAME = 'TYieldRunStochasticDialog.RestoreColourState';
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

function TYieldRunStochasticDialog.LoadCaseSelectedChkBox(AIndex : integer) : TFieldChkBox;
const OPNAME = 'TYieldRunStochasticDialog.LoadCaseSelectedChkBox';
begin
  result := nil;
  try
    result := FLoadCaseSelectedChkBox[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticDialog.IndexOfLoadCaseCheckBox (AChkBox : TFieldChkBox) : integer;
const OPNAME = 'TYieldRunStochasticDialog.IndexOfLoadCaseCheckBox';
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
