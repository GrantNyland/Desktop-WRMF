
unit UAnnualIFRFeatureDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  TIFRLineSeriesArray = array [1..12] of TLineSeries;
  TAnnualIFRFeatureDialog = class(TAbstractScrollablePanel)
  protected
//    FSiteLabel              : TLabel;
//    FSitesCbx               : TFieldComboBox;
    FFeatureNameLabel       : TLabel;
    FFeatureNameEdit        : TFieldEdit;
    FAnnualGrid             : TFieldStringGrid;
    FNrOfPointsLabel        : TLabel;
    FNrOfPointsEdit         : TFieldEdit;
    FCalcOptionLabel        : TLabel;
    FCalcOptionCbx          : TFieldComboBox;
    FedtCalcOption          : TFieldEdit;
    FReferenceNodesLabel    : TLabel;
    FReferenceNodesCheckLbx : TFieldCheckListBox;

    FUnitsOptionLabel       : TLabel;
    FUnitsOptionRadioGroup  : TFieldRadioGroup;
    FrdgAnnualMonthlyOption : TFieldRadioGroup;
    FlblAnnualMonthlyOption : TLabel;

    FIFRFeatureExists       : TFieldChkBox;
    FTotalMARLabel          : TLabel;
    FTotalMAREdit           : TFieldEdit;
    FIFRLoss                : TFieldChkBox;

    FMonthlyIFRLossGrid     : TFieldStringGrid;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property FeatureNameEdit        : TFieldEdit         read FFeatureNameEdit;
//    property SitesCbx               : TFieldComboBox     read FSitesCbx;
    property NrOfPointsEdit         : TFieldEdit         read FNrOfPointsEdit;
    property CalcOptionCbx          : TFieldComboBox     read FCalcOptionCbx;
    property edtCalcOption          : TFieldEdit         read FedtCalcOption;
    property ReferenceNodesCheckLbx : TFieldCheckListBox read FReferenceNodesCheckLbx;
    property AnnualGrid             : TFieldStringGrid   read FAnnualGrid;
    property UnitsOptionRadioGroup  : TFieldRadioGroup   read FUnitsOptionRadioGroup;
    property rdgAnnualMonthlyOption : TFieldRadioGroup   read FrdgAnnualMonthlyOption;
    property ChkboxFIFRFeatureExists : TFieldChkBox       read FIFRFeatureExists;
    property TotalMAREdit            : TFieldEdit         read FTotalMAREdit;
    property IFRLoss                 : TFieldChkBox       read FIFRLoss;
    property MonthlyIFRLossGrid      : TFieldStringGrid   read FMonthlyIFRLossGrid;



  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Forms,
  VCL.Grids,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TAnnualIFRFeatureDialog                                                          *}
{******************************************************************************}

procedure TAnnualIFRFeatureDialog.CreateMemberObjects;
const OPNAME = 'TAnnualIFRFeatureDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    //                                                                          Left  Top  Width Height
//    FSiteLabel              := CreateFieldLabel       (lOwner, lParent,      10 , 10,    130,  20);
//    FSitesCbx               := CreateFieldComboBox    (FAppModules, lOwner, lParent, 120 , 10, 190, 20, 0 , True, csDropDownList);
    FFeatureNameLabel       := CreateFieldLabel       (lOwner,          lParent,  10,  10, 130,  20);
    FFeatureNameEdit        := CreateFieldEdit        (FAppModules, lOwner,          lParent, 120,  10, 190,  20, 1, TRUE);
    FNrOfPointsLabel        := CreateFieldLabel       (lOwner,          lParent,  10,  35, 130,  20);
    FNrOfPointsEdit         := CreateFieldEdit        (FAppModules, lOwner,          lParent, 120, 35,  60,  20, 3, TRUE);
    FCalcOptionLabel        := CreateFieldLabel       (lOwner, lParent, 10,  60, 130, 21);
    FCalcOptionCbx          := CreateFieldComboBox    (FAppModules, lOwner,          lParent, 120, 60,80,20, 4, TRUE, csDropDownList);
    FedtCalcOption          := CreateFieldEdit        (FAppModules, lOwner,          lParent, 205, 60,  60,  20, 3, False);

    FReferenceNodesLabel    := CreateFieldLabel       (lOwner,          lParent, 325,  10, 105,  20);
    FReferenceNodesCheckLbx := CreateFieldCheckListBox(FAppModules, lOwner, lParent, 420,  10, 250, 120, 7, TRUE);
    FUnitsOptionLabel       := CreateFieldLabel       (lOwner,          lParent,  10,  85, 130,  20);
    FUnitsOptionRadioGroup  := CreateFieldRadioGroup  (FAppModules, lOwner, lParent,  120,  80, 190,  30, 5, True);
    FlblAnnualMonthlyOption := CreateFieldLabel       (lOwner,          lParent,  10, 115 , 130,  20);

    FrdgAnnualMonthlyOption := CreateFieldRadioGroup  (FAppModules, lOwner, lParent,  120,  110, 190,  30, 5, True);
    FAnnualGrid             := TFieldStringGrid.Create(LOwner,FAppModules);
    FAnnualGrid.Parent      := ControlsParent;

    FIFRFeatureExists        := CreateFieldChkBox     (FAppModules, lOwner, lParent,   10,  140, 123, 30, 20, TRUE, taLeftJustify);
    FIFRFeatureExists.Parent := lParent;

//    FIFRFeatureExists.Top    := FUpdateIFRFromReferenceInflows.Top + FUpdateIFRFromReferenceInflows.Height + 5;
//    FIFRFeatureExists.Width  := FUpdateIFRFromReferenceInflows.Width;

    FTotalMARLabel          := CreateFieldLabel       (lOwner,      lParent,  180,  144, 240,  20);
    FTotalMAREdit           := CreateFieldEdit        (FAppModules, lOwner,          lParent, 420,  142, 190,  20, 1, TRUE);
    FTotalMAREdit.Enabled   := False;

    FFeatureNameEdit.IsEnabled := False;
    FNrOfPointsLabel.WordWrap := TRUE;

  //  FIFRLoss        := TFieldChkBox.Create(ControlsOwner, FAppModules);
  //  FIFRLoss.Parent := lParent;
  //  FIFRLoss.Left   := 10;
   // FIFRLoss.Top    := FIFRFeatureExists.Top + FIFRFeatureExists.Height + 5;
   // FIFRLoss.Width  := FIFRFeatureExists.Width;
   // FIFRLoss.Alignment := taLeftJustify;

  { FMonthlyIFRLossGrid     := CreateFieldStringGrid(FAppModules, lOwner, lParent, 10, FIFRLoss.Top +FIFRLoss.Height+5,  450, 45, 0, TRUE);

    with FMonthlyIFRLossGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 12;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 17;
      DefaultColWidth  := 50;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
      Width            := (DefaultColWidth*ColCount)+5;
      Visible          := False;

    end;
       }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureDialog.Resize;
const OPNAME = 'TAnnualIFRFeatureDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureDialog.Initialise: boolean;
const OPNAME = 'TAnnualIFRFeatureDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    FUnitsOptionRadioGroup.Columns := 2;
    FUnitsOptionRadioGroup.Items.Clear;
    FUnitsOptionRadioGroup.Items.Add('');
    FUnitsOptionRadioGroup.Items.Add('');
    FrdgAnnualMonthlyOption.Columns := 2;
    FrdgAnnualMonthlyOption.Items.Clear;
    FrdgAnnualMonthlyOption.Items.Add('');
    FrdgAnnualMonthlyOption.Items.Add('');
    FrdgAnnualMonthlyOption.ItemIndex := 1;

    FAnnualGrid.ColCount := 13;
    FAnnualGrid.RowCount := 2;
    FAnnualGrid.FixedCols := 0;
    FAnnualGrid.FixedRows := 1;
    FAnnualGrid.ColWidths[0] := 100;
    FAnnualGrid.RowHeights[0] := 30;
    FAnnualGrid.WrapHeaderText := True;

    FAnnualGrid.Left     := 10;
    FAnnualGrid.Top      := 200;
    FCalcOptionCbx.Items.Add('Option 1');
    FCalcOptionCbx.Items.Add('Option 2');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureDialog.LanguageHasChanged: boolean;
const OPNAME = 'TAnnualIFRFeatureDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
//    FSiteLabel.Caption           := FAppModules.Language.GetString('TField.IFRSiteID')+ ' :';
    FFeatureNameLabel.Caption    := FAppModules.Language.GetString('NetworkFeatures.IFRFeatureName') + ' :';
    FCalcOptionLabel.Caption     := FAppModules.Language.GetString('TField.IFRCalculationOption') + ' :';
    FNrOfPointsLabel.Caption     := FAppModules.Language.GetString('TField.IFRPointsCount') + ' :';
    FReferenceNodesLabel.Caption := FAppModules.Language.GetString('NetworkFeatures.ReferenceNodes') + ' :';
    FReferenceNodesCheckLbx.Hint := FAppModules.Language.GetString('TIFRFeatureDialog.ReferenceNodes');
    FlblAnnualMonthlyOption.Caption := FAppModules.Language.GetString('TField.IFRType') + ' :';
    
    FAnnualGrid.Cells[0,0]                  := 'Annual Reference Flow(Million m3/a)';
    FUnitsOptionRadioGroup.Items[0]  := FAppModules.Language.GetString('TField.IFRUnitOptionM3PerSec');
    FUnitsOptionRadioGroup.Items[1]  := FAppModules.Language.GetString('TField.IFRUnitOptionM3PerMonth');
    FUnitsOptionLabel.Caption        := FAppModules.Language.GetString('TField.IFRUnits') + ' :';
    FrdgAnnualMonthlyOption.Items[0] := FAppModules.Language.GetString('TIFRFeatureDialog.MonthlyRefenceFlow');
    FrdgAnnualMonthlyOption.Items[1] := FAppModules.Language.GetString('TIFRFeatureDialog.AnnualRefenceFlow');

    FIFRFeatureExists.Caption        := FAppModules.Language.GetString('TIFRFeatureDialog.IFRFeatureExists');
    FTotalMARLabel.Caption           := FAppModules.Language.GetString('TIFRFeatureDialog.TotalMAR');
    FIFRLoss.Caption                 := 'IFR Loss';

//    FUnitsOptionRadioGroup.ItemIndex := 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureDialog.RestoreColourState;
const OPNAME = 'TAnnualIFRFeatureDialog.RestoreColourState';
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

procedure TAnnualIFRFeatureDialog.AssignHelpContext;
const OPNAME = 'TAnnualIFRFeatureDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                    HC_EcologicalRequirementsIFRs);
//    SetControlHelpContext(FSitesCbx,               HC_EcologicalRequirementsIFRs);
    SetControlHelpContext(FFeatureNameEdit,        HC_EcologicalRequirementsIFRs);
    SetControlHelpContext(FNrOfPointsEdit,         HC_EcologicalRequirementsIFRs);
    SetControlHelpContext(FCalcOptionCbx,          HC_EcologicalRequirementsIFRs);
    SetControlHelpContext(FReferenceNodesCheckLbx, HC_EcologicalRequirementsIFRs);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.


