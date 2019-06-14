{******************************************************************************}
{*  UNIT      : Contains the class TPlanningRunConfigurationDialog.           *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/04/26                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UPlanningRunConfigurationDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Grids,
  VCL.Graphics,
  UAbstractObject,
  VoaimsCom_TLB,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TPlanningRunConfigurationDialog = class(TAbstractScrollablePanel)
  private
    FConfigurationGroupBox          : TGroupBox;
    FDecisionDatesGroupBox          : TGroupBox;
    FShortTermOptionGroupBox        : TGroupBox;
    FWaterQualityOptionChkBox       : TFieldChkBox;
    FHydroPowerOptionLabel          : TLabel;
    FAllocationControlOptionlbl     : TLabel;
    FHydroPowerOptionComboBox       : TFieldComboBox;
    FAllocationControlOptionComboBox  : TFieldComboBox;
    FNrOfPeriodsPerYearLabel        : TLabel;
    FNrOfPeriodsPerYearEdit         : TFieldEdit;
    FShortTermOptionRadioGroup      : TFieldRadioGroup;
    FNrOfDecisionDatesLabel         : TLabel;
    FNrOfDecisionDatesEdit          : TFieldEdit;
    FBtnAddDecisionDate             : TFieldBitBtn;
    FBtnDeleteDecisionDate          : TFieldBitBtn;
    FGrdDecisionDates               : TFieldStringGrid;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure ResetButtonState;

    property ConfigurationGroupBox          : TGroupBox        read FConfigurationGroupBox;
    property DecisionDatesGroupBox          : TGroupBox        read FDecisionDatesGroupBox;
    property ShortTermOptionGroupBox        : TGroupBox        read FShortTermOptionGroupBox;
    property WaterQualityOptionChkBox       : TFieldChkBox     read FWaterQualityOptionChkBox;
    property HydroPowerOptionComboBox       : TFieldComboBox   read FHydroPowerOptionComboBox;
    property AllocationControlOptionComboBox: TFieldComboBox   read FAllocationControlOptionComboBox;
    property NrOfPeriodsPerYearLabel        : TLabel           read FNrOfPeriodsPerYearLabel;
    property NrOfPeriodsPerYearEdit         : TFieldEdit       read FNrOfPeriodsPerYearEdit;
    property ShortTermOptionRadioGroup      : TFieldRadioGroup read FShortTermOptionRadioGroup;
    property NrOfDecisionDatesLabel         : TLabel           read FNrOfDecisionDatesLabel;
    property NrOfDecisionDatesEdit          : TFieldEdit       read FNrOfDecisionDatesEdit;
    property BtnAddDecisionDate             : TFieldBitBtn     read FBtnAddDecisionDate;
    property BtnDeleteDecisionDate          : TFieldBitBtn     read FBtnDeleteDecisionDate;
    property GrdDecisionDates               : TFieldStringGrid read FGrdDecisionDates;
  end;

  implementation

uses
  SysUtils,
  UConstants,
  UHelpContexts,
  UErrorHandlingOperations,
  UYieldModelDataObject,
  UControlCreationUtilities;

{******************************************************************************}
{* TPlanningRunConfigurationDialog                                                    *}
{******************************************************************************}

procedure TPlanningRunConfigurationDialog.CreateMemberObjects;
const OPNAME = 'TPlanningRunConfigurationDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FConfigurationGroupBox         := CreateFieldGroupBox  (lOwner, lParent,   5,   5, 320, 160, 0, FALSE);
    FWaterQualityOptionChkBox      := CreateFieldChkBox    (FAppModules, lOwner, FConfigurationGroupBox, 10, 10, 160,  21, 1, TRUE, taLeftJustify);
    FHydroPowerOptionLabel         := CreateFieldLabel     (lOwner, FConfigurationGroupBox,  10,  35, 175,  21);
    FHydroPowerOptionComboBox      := CreateFieldComboBox  (FAppModules, lOwner, FConfigurationGroupBox, 157, 35, 50,  21, 1, TRUE,csDropDown);

    FAllocationControlOptionlbl    := CreateFieldLabel     (lOwner, FConfigurationGroupBox,  10,  60, 175,  21);

    FAllocationControlOptionComboBox := CreateFieldComboBox(FAppModules, lOwner, FConfigurationGroupBox, 157, 60, 50,  21, 1, TRUE, csDropDown);
    FNrOfPeriodsPerYearLabel       := CreateFieldLabel     (lOwner, FConfigurationGroupBox,  10,  85, 175,  21);
    FNrOfPeriodsPerYearEdit        := CreateFieldEdit      (FAppModules, lOwner, FConfigurationGroupBox, 157, 85,  40,  21, 2, TRUE);
    FShortTermOptionRadioGroup     := CreateFieldRadioGroup(FAppModules, lOwner, lParent,   5,  180, 320,  90, 2, FALSE);
    FDecisionDatesGroupBox         := CreateFieldGroupBox  (lOwner, lParent,   340,   5, 440, 265, 0, FALSE);
    FNrOfDecisionDatesLabel        := CreateFieldLabel     (lOwner, FDecisionDatesGroupBox,  10,  10, 175,  21);
    FNrOfDecisionDatesEdit         := CreateFieldEdit      (FAppModules, lOwner, FDecisionDatesGroupBox, 145, 10,  30,  21, 2, TRUE);

    FBtnAddDecisionDate            :=  TFieldBitBtn.Create(lOwner,FAppModules);
    FBtnAddDecisionDate.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    with FBtnAddDecisionDate do
    begin
      Parent  := FDecisionDatesGroupBox;
      Left    := 10;
      Top     := 50;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;

    FBtnDeleteDecisionDate            :=  TFieldBitBtn.Create(lOwner,FAppModules);
    FBtnDeleteDecisionDate.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    with FBtnDeleteDecisionDate do
    begin
      Parent  := FDecisionDatesGroupBox;
      Left    := 260;
      Top     := 10;
      Width   := 75;
      Height  := 25;
      ShowHint:= True;
    end;

    FGrdDecisionDates := CreateFieldStringGrid(FAppModules, lOwner, FDecisionDatesGroupBox,
                                              10, 40, 425, 100, 0, TRUE);
    with FGrdDecisionDates do
    begin
      ColCount         := 4;
      RowCount         := 2;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 130;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end; 

procedure TPlanningRunConfigurationDialog.Resize;
const OPNAME = 'TPlanningRunConfigurationDialog.Resize';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningRunConfigurationDialog.Initialise: boolean;
const OPNAME = 'TPlanningRunConfigurationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FBtnAddDecisionDate.Enabled    := False;
    FBtnDeleteDecisionDate.Enabled := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationDialog.ResetButtonState;
const OPNAME = 'TPlanningRunConfigurationDialog.ResetButtonState';
var
  lNrOfDecisionDates : integer;
  lConfigData        : IRunConfigurationData;
  lFieldProperty     : TAbstractFieldProperty;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      lNrOfDecisionDates := lConfigData.NrOfDecisionMonths;
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfDecisionDates');
      if Assigned(lFieldProperty) then
      begin
        FBtnAddDecisionDate.IsEnabled    := (lNrOfDecisionDates <  StrToInt(lFieldProperty.FieldMaximumValue)) AND
                                            (FAppModules.User.UserRights in CUR_EditData);
        FBtnDeleteDecisionDate.IsEnabled := (lNrOfDecisionDates >  StrToInt(lFieldProperty.FieldMinimumValue)) AND
                                            (FAppModules.User.UserRights in CUR_EditData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningRunConfigurationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPlanningRunConfigurationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FWaterQualityOptionChkBox.Caption      := FAppModules.Language.GetString('TField.WaterQualityOption') + ' :';
    FHydroPowerOptionLabel.Caption         := FAppModules.Language.GetString('TField.HydroPowerOption') + ' :';
    FAllocationControlOptionlbl.Caption := FAppModules.Language.GetString('TField.AllocationControlOption') + ' :';
    FNrOfPeriodsPerYearLabel.Caption       := FAppModules.Language.GetString('TField.PeriodsPerYear')   + ' :';
    FNrOfDecisionDatesLabel.Caption        := FAppModules.Language.GetString('TField.NrOfDecisionDates')   + ' :';
    FShortTermOptionGroupBox.Caption       := ' ' + FAppModules.Language.GetString('TField.ShortTermOption') + ' ';
    FShortTermOptionRadioGroup.Caption     := ' ' + FAppModules.Language.GetString('TField.ShortTermOption') + ' ';
    FShortTermOptionRadioGroup.Hints.Clear;
    FShortTermOptionRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.ShortTermPlanningNDescr'));
    FShortTermOptionRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.ShortTermPlanningPDescr'));
    FShortTermOptionRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.ShortTermPlanningMDescr'));
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationDialog.RestoreColourState;
const OPNAME = 'TPlanningRunConfigurationDialog.RestoreColourState';
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

procedure TPlanningRunConfigurationDialog.AssignHelpContext;
const OPNAME = 'TPlanningRunConfigurationDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                          HC_RunControl);
    SetControlHelpContext(FConfigurationGroupBox,        HC_AnalysisPeriod);
    SetControlHelpContext(FWaterQualityOptionChkBox,     HC_AnalysisPeriod);
    SetControlHelpContext(FNrOfPeriodsPerYearEdit,       HC_AnalysisPeriod);

   except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
