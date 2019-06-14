{******************************************************************************}
{*  UNIT      : Contains the class TMasterControlChannelDialog.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMasterControlChannelDialog;

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

  TMasterControlChannelDialog = class(TAbstractScrollablePanel)
  private
  protected
    FFeatureNameLabel             : TLabel;
    FFeatureNameEdit              : TFieldEdit;
    FChannelTypeLabel             : TLabel;
    FChannelTypeCbx               : TFieldComboBox;
    FWaterFactorLabel             : TLabel;
    FWaterTargetLabel             : TLabel;
    FPowerFactorLabel             : TLabel;
    FPowerTargetLabel             : TLabel;
    FWaterDistributionGrid        : TFieldStringGrid;
    FPowerDistributionGrid        : TFieldStringGrid;
    FLoadCasesGroupBox            : TGroupBox;
    FDemandGroupBox               : TGroupBox;
    FTargetYieldLabel             : TLabel;
    FMaxYieldLabel                : TLabel;
    FPowerDemandLabel             : TLabel;
    FWaterLoadCasesGrid           : TFieldStringGrid;
    FPowerLoadCasesGrid           : TFieldStringGrid;
    FIncludeInAnalysis            : TLabel;
    FLoadCaseSelectedChkBox       : TFieldChkBoxArray;
    FFactorTotalEdit              : TFieldEdit;
    FYearlyTotalEdit              : TFieldEdit;
    FWaterAndTargetTotalLabel     : TLabel;
    FMillionM3RadioButton         : TRadioButton;
    FM3RadioButton                : TRadioButton;
    FTotalUnitsLabel              : TLabel;
    FHistoricTargetYieldLabel     : TLabel;
    FStochasticTargetYieldLabel   : TLabel;

    FDemandCentreGroupBox         : TGroupBox;
    FIncludeInOutputChkBox        : TFieldChkBox;
    FAnnualDemandLabel            : TLabel;
    FAnnualDemandEdit             : TFieldEdit;
    FMinimumDemandLabel           : TLabel;
    FMinimumDemandEdit            : TFieldEdit;
    FDemandCentreType             : TFieldRadioGroup;
  procedure CreateMemberObjects; override;
  procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function LoadCaseSelectedChkBox(AIndex : integer) : TFieldChkBox;
    function IndexOfLoadCaseCheckBox (AChkBox : TFieldChkBox) : integer;

    property FeatureNameLabel           : TLabel           read FFeatureNameLabel;
    property FeatureNameEdit            : TFieldEdit       read FFeatureNameEdit;
    property ChannelTypeCbx             : TFieldComboBox   read FChannelTypeCbx;
    property LoadCasesGroupBox          : TGroupBox        read FLoadCasesGroupBox;
    property WaterDistributionGrid      : TFieldStringGrid read FWaterDistributionGrid;
    property PowerDistributionGrid      : TFieldStringGrid read FPowerDistributionGrid;
    property FactorTotalEdit            : TFieldEdit       read FFactorTotalEdit;
    property YearlyTotalEdit            : TFieldEdit       read FYearlyTotalEdit;
    property WaterAndTargetTotalLabel   : TLabel           read FWaterAndTargetTotalLabel;
    property WaterLoadCasesGrid         : TFieldStringGrid read FWaterLoadCasesGrid;
    property PowerLoadCasesGrid         : TFieldStringGrid read FPowerLoadCasesGrid;
    property WaterFactorLabel           : TLabel           read FWaterFactorLabel;
    property WaterTargetLabel           : TLabel           read FWaterTargetLabel;
    property HistoricTargetYieldLabel   : TLabel           read FHistoricTargetYieldLabel;
    property StochasticTargetYieldLabel : TLabel           read FStochasticTargetYieldLabel;
    property MillionM3RadioButton       : TRadioButton     read FMillionM3RadioButton;
    property M3RadioButton              : TRadioButton     read FM3RadioButton;
    property PowerFactorLabel           : TLabel           read FPowerFactorLabel;
    property PowerTargetLabel           : TLabel           read FPowerTargetLabel;
    property DemandCentreGroupBox       : TGroupBox        read FDemandCentreGroupBox;
    property DemandGroupBox             : TGroupBox        read FDemandGroupBox;
    property IncludeInOutputChkBox      : TFieldChkBox     read FIncludeInOutputChkBox;
    property AnnualDemandLabel          : TLabel           read FAnnualDemandLabel;
    property AnnualDemandEdit           : TFieldEdit       read FAnnualDemandEdit;
    property MinimumDemandLabel         : TLabel           read FMinimumDemandLabel;
    property MinimumDemandEdit          : TFieldEdit       read FMinimumDemandEdit;
    property DemandCentreTypeRadioGrp   : TFieldRadioGroup read FDemandCentreType;
    property TotalUnitsLabel            : TLabel           read FTotalUnitsLabel;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TMasterControlChannelDialog                                                *}
{******************************************************************************}

procedure TMasterControlChannelDialog.CreateMemberObjects;
const OPNAME = 'TMasterControlChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
  lIndexA : integer;
  lChkBox : TFieldChkBox;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FDemandGroupBox            := CreateFieldGroupBox(lOwner, lParent, 350, 60, 340, 380,12,FALSE);
    FFeatureNameLabel          := CreateFieldLabel                  (lOwner, lParent,  10,  10, 130,  21);
    FFeatureNameEdit           := CreateFieldEdit      (FAppModules, lOwner, lParent, 140,  10, 160,  21, 0, TRUE);
    FChannelTypeLabel          := CreateFieldLabel                  (lOwner, lParent,  10,  35, 130,  21);
    FChannelTypeCbx            := CreateFieldComboBox  (FAppModules, lOwner, lParent, 140,  35, 160,  21, 7, TRUE, csDropDownList);
    FWaterFactorLabel          := CreateFieldLabel                  (lOwner, FDemandGroupBox, 50,  15,  90,  40);
    FWaterTargetLabel          := CreateFieldLabel                  (lOwner, FDemandGroupBox, 140, 15,  90,  26);
    FTotalUnitsLabel           := CreateFieldLabel                  (lOwner, FDemandGroupBox, 240, 345, 90,  21);

    FMillionM3RadioButton      := TRadioButton.Create(lOwner);
    with FMillionM3RadioButton do
    begin
      Parent := FDemandGroupBox;
      Left   := 150;
      Top    := 40;
      Width  := 80;
      Height := 17;
      Alignment := taRightJustify;
    end;
    FM3RadioButton             := TRadioButton.Create(lOwner);
    with FM3RadioButton do
    begin
      Parent := FDemandGroupBox;
      Left   := 150;
      Top    := 55;
      Width  := 80;
      Height := 17;
      Alignment := taRightJustify;
    end;

    FPowerFactorLabel          := CreateFieldLabel                  (lOwner, FDemandGroupBox, 50,  15,  90,  40);
    FPowerTargetLabel          := CreateFieldLabel                  (lOwner, FDemandGroupBox, 140, 15,  90,  40);
    FWaterAndTargetTotalLabel  := CreateFieldLabel                  (lOwner, FDemandGroupBox, 10,  345, 50,  15);
    FWaterDistributionGrid     := CreateFieldStringGrid(FAppModules, lOwner, FDemandGroupBox, 10, 75, 226, 267, 8, TRUE);
    FPowerDistributionGrid     := CreateFieldStringGrid(FAppModules, lOwner, FDemandGroupBox, 10, 75, 226, 267, 9, TRUE);
    FFactorTotalEdit           := CreateFieldEdit      (FAppModules, lOwner, FDemandGroupBox, 70, 345,  82,  21, 9, FALSE);
    FYearlyTotalEdit           := CreateFieldEdit      (FAppModules, lOwner, FDemandGroupBox, 155, 345,  82,  21,10, FALSE);
    FWaterFactorLabel.WordWrap  := TRUE;
    FWaterFactorLabel.Alignment := taCenter;
    FWaterTargetLabel.WordWrap  := TRUE;
    FWaterTargetLabel.Alignment := taCenter;
    FPowerFactorLabel.WordWrap  := TRUE;
    FPowerFactorLabel.Alignment := taCenter;
    FPowerTargetLabel.WordWrap  := TRUE;
    FPowerTargetLabel.Alignment := taCenter;
    with FWaterDistributionGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 12;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 21;
      DefaultColWidth  := 80;
      ColWidths[0]     := 60;
    end;
    with FPowerDistributionGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 12;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 21;
      DefaultColWidth  := 80;
      ColWidths[0]     := 60;
    end;

    FLoadCasesGroupBox           := CreateFieldGroupBox(lOwner, lParent, 10,  60, 320, 380, 11, FALSE);
    FTargetYieldLabel            := CreateFieldLabel (lOwner, FLoadCasesGroupBox,  10, 15, 80, 50);
    FMaxYieldLabel               := CreateFieldLabel (lOwner, FLoadCasesGroupBox,  90, 15, 80, 50);
    FPowerDemandLabel            := CreateFieldLabel (lOwner, FLoadCasesGroupBox, 180, 15, 80, 50);
    FIncludeInAnalysis           := CreateFieldLabel (lOwner, FLoadCasesGroupBox, 260, 10, 50, 50);
    FHistoricTargetYieldLabel    := CreateFieldLabel (lOwner, FLoadCasesGroupBox, 5, 280, 250, 50);
    FStochasticTargetYieldLabel  := CreateFieldLabel (lOwner, FLoadCasesGroupBox, 5, 280, 250, 50);
    FHistoricTargetYieldLabel.Alignment  := taCenter;
    FHistoricTargetYieldLabel.WordWrap   := TRUE;
    FHistoricTargetYieldLabel.Font.Color := clRed;
    FHistoricTargetYieldLabel.Visible    := FALSE;
    FStochasticTargetYieldLabel.Alignment  := taCenter;
    FStochasticTargetYieldLabel.WordWrap   := TRUE;
    FStochasticTargetYieldLabel.Font.Color := clRed;
    FStochasticTargetYieldLabel.Visible    := FALSE;

    FTargetYieldLabel.Alignment  := taCenter;
    FTargetYieldLabel.WordWrap   := TRUE;
    FMaxYieldLabel.Alignment     := taCenter;
    FMaxYieldLabel.WordWrap      := TRUE;
    FPowerDemandLabel.Alignment  := taCenter;
    FPowerDemandLabel.WordWrap   := TRUE;
    FIncludeInAnalysis.Alignment := taCenter;
    FIncludeInAnalysis.WordWrap  := True;
    FWaterLoadCasesGrid  := CreateFieldStringGrid(FAppModules, lOwner, FLoadCasesGroupBox,  10, 70, 165, 213, 12, TRUE);
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
    FPowerLoadCasesGrid  := CreateFieldStringGrid(FAppModules, lOwner, FLoadCasesGroupBox, 180, 70,  83, 213, 13, TRUE);
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
    for lIndexA := 1 to 10 do
    begin
      lChkBox := CreateFieldChkBox(FAppModules, lOwner, FLoadCasesGroupBox, 280, 70+(lIndexA-1)*21, 25, 20, 1+lIndexA, TRUE, taRightJustify);
      lChkBox.Caption := '  ';
      FLoadCaseSelectedChkBox[lIndexA] := lChkBox;
    end;

    FDemandCentreType     := CreateFieldRadioGroup(FAppModules, lOwner, lParent,   10,  60, 320,  50, 2, FALSE);
    FDemandCentreGroupBox  := CreateFieldGroupBox(lOwner, lParent, 10,  130, 320, 310, 11, FALSE);
    FIncludeInOutputChkBox := CreateFieldChkBox(FAppModules, lOwner, FDemandCentreGroupBox, 10, 25, 172, 21,
                                                0, TRUE, taLeftJustify);
    FAnnualDemandLabel     := CreateFieldLabel(lOwner, FDemandCentreGroupBox, 10, 55, 175,  21);
    FAnnualDemandEdit      := CreateFieldEdit (FAppModules, lOwner, FDemandCentreGroupBox, 168, 55,  80,  21, 0, TRUE);
    FMinimumDemandLabel    := CreateFieldLabel(lOwner, FDemandCentreGroupBox, 10, 85, 175,  21);
    FMinimumDemandEdit     := CreateFieldEdit (FAppModules, lOwner, FDemandCentreGroupBox, 168, 85,  80,  21, 0, TRUE);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelDialog.Resize;
const OPNAME = 'TMasterControlChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelDialog.Initialise: boolean;
const OPNAME = 'TMasterControlChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMasterControlChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption           := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    FChannelTypeLabel.Caption           := FAppModules.Language.GetString('TField.ChannelSubType')  + ' :';
    FWaterFactorLabel.Caption           := FAppModules.Language.GetString('TField.WaterSupplyDistribution');
    FWaterTargetLabel.Caption           := FAppModules.Language.GetString('MasterControl.MonthlyTargetDraft');
    FPowerFactorLabel.Caption           := FAppModules.Language.GetString('TField.MinEnergyDemand');
    FPowerTargetLabel.Caption           := FAppModules.Language.GetString('MasterControl.MonthlyEnergyDemand');
    FLoadCasesGroupBox.Caption          := ' ' + FAppModules.Language.GetString('MasterControl.LoadCases') + ' ';
    FTargetYieldLabel.Caption           := FAppModules.Language.GetString('MasterControl.TargetSystemYield');
    FHistoricTargetYieldLabel.Caption   := FAppModules.Language.GetString('MasterControl.HistoricTargetYield');
    FStochasticTargetYieldLabel.Caption := FAppModules.Language.GetString('MasterControl.StochasticTargetYield');
    FWaterAndTargetTotalLabel.Caption   := FAppModules.Language.GetString('LabelHeading.Total'); 
    FMaxYieldLabel.Caption              := FAppModules.Language.GetString('MasterControl.MaximumSystemYield');
    FPowerDemandLabel.Caption           := FAppModules.Language.GetString('MasterControl.TargetPowerDemand');
    FIncludeInAnalysis.Caption          := FAppModules.language.GetString('MasterControl.IncludeInAnalysis');
    FMillionM3RadioButton.Caption       := FAppModules.Language.GetString('MasterControl.MillionM3perAnnum');
    FM3RadioButton.Caption              := FAppModules.Language.GetString('MasterControl.M3perSecond');
    FFactorTotalEdit.Hint               := FAppModules.Language.GetString('TMasterControlChannelDialog.FactorTotalEdit');
    FYearlyTotalEdit.Hint               := FAppModules.Language.GetString('TMasterControlChannelDialog.YearlyTotalEdit');

    FDemandCentreGroupBox.Caption  := FAppModules.Language.GetString('MasterControl.DemandCentre');
    FDemandCentreType.Caption     := FAppModules.Language.GetString('MasterControl.DemandCentreType');
    FDemandCentreType.Hints.Clear;
    FDemandCentreType.Hints.Add(FAppModules.Language.GetString('MasterControl.ChannelTypeD'));
    FDemandCentreType.Hints.Add(FAppModules.Language.GetString('MasterControl.ChannelTypeR'));
    FIncludeInOutputChkBox.Caption := FAppModules.Language.GetString('TField.IncludeInOutput')+ ':';
    FAnnualDemandLabel.Caption     := FAppModules.Language.GetString('MasterControl.AnnualDemand')+ ':';
    FMinimumDemandLabel.Caption    := FAppModules.Language.GetString('MasterControl.MinimumDemand')+ ':';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelDialog.RestoreColourState;
const OPNAME = 'TMasterControlChannelDialog.RestoreColourState';
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

function TMasterControlChannelDialog.LoadCaseSelectedChkBox(AIndex : integer) : TFieldChkBox;
const OPNAME = 'TMasterControlChannelDialog.LoadCaseSelectedChkBox';
begin
  result := nil;
  try
    result := FLoadCaseSelectedChkBox[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelDialog.IndexOfLoadCaseCheckBox (AChkBox : TFieldChkBox) : integer;
const OPNAME = 'TMasterControlChannelDialog.IndexOfLoadCaseCheckBox';
var
  lIndexA : integer;
begin
  result := -1;
  try
    lIndexA := 1;
    while ((result = -1) AND (lIndexA <= 10)) do
    begin
      if (AChkBox = FLoadCaseSelectedChkBox[lIndexA]) then
        result := lIndexA
      else
        lIndexA := lIndexA + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelDialog.AssignHelpContext;
const OPNAME = 'TMasterControlChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                   HC_MasterControlChannels);
    SetControlHelpContext(FFeatureNameEdit,       HC_MasterControlChannels);
    SetControlHelpContext(FChannelTypeCbx,        HC_MasterControlChannels);

    SetControlHelpContext(FLoadCasesGroupBox,     HC_MasterControlChannels);
    SetControlHelpContext(FWaterLoadCasesGrid,    HC_MasterControlChannels);
    SetControlHelpContext(FPowerLoadCasesGrid,    HC_MasterControlChannels);
    SetControlHelpContext(FWaterDistributionGrid, HC_MasterControlChannels);
    SetControlHelpContext(FPowerDistributionGrid, HC_MasterControlChannels);

    SetControlHelpContext(FFactorTotalEdit,       HC_MasterControlChannels);
    SetControlHelpContext(FYearlyTotalEdit,       HC_MasterControlChannels);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

