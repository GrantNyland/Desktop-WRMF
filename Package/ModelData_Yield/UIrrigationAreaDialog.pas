{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationAreaDialog.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UIrrigationAreaDialog;

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

  TIrrigationAreaDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel     : TLabel;
    FFeatureNameEdit      : TFieldEdit;
    FUpstreamNodeLabel    : TLabel;
    FDownstreamNodeLabel  : TLabel;
    FUpStreamNodeCbx      : TFieldComboBox;
    FDownStreamNodeCbx    : TFieldComboBox;
    FRelaxationRadioGroup : TFieldRadioGroup;
    FMonthlyFlowsLabel    : TLabel;
    FMonthlyFlowsGrid     : TFieldStringGrid;

    FIrrigationAreaXCoordLabel : TLabel;
    FIrrigationAreaXCoordEdit  : TFieldEdit;
    FIrrigationAreaYCoordLabel : TLabel;
    FIrrigationAreaYCoordEdit  : TFieldEdit;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property FeatureNameEdit      : TFieldEdit       read FFeatureNameEdit;
    property UpStreamNodeCbx      : TFieldComboBox   read FUpStreamNodeCbx;
    property DownStreamNodeCbx    : TFieldComboBox   read FDownStreamNodeCbx;
    property RelaxationRadioGroup : TFieldRadioGroup read FRelaxationRadioGroup;
    property MonthlyFlowsGrid     : TFieldStringGrid read FMonthlyFlowsGrid;
    property IrrigationAreaXCoordEdit : TFieldEdit   read FIrrigationAreaXCoordEdit;
    property IrrigationAreaYCoordEdit : TFieldEdit   read FIrrigationAreaYCoordEdit;
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
{* TIrrigationAreaDialog                                                      *}
{******************************************************************************}

procedure TIrrigationAreaDialog.CreateMemberObjects;
const OPNAME = 'TIrrigationAreaDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                              Left  Top  Width Height
    FFeatureNameLabel     := CreateFieldLabel                  (lOwner, lParent,  10,  10, 130,  21);
    FFeatureNameEdit      := CreateFieldEdit      (FAppModules, lOwner, lParent, 140,  10, 200,  21, 0, TRUE);
    FUpStreamNodeLabel    := CreateFieldLabel                  (lOwner, lParent,  10,  38, 130,  26);
    FUpStreamNodeLabel.WordWrap := TRUE;
    FUpStreamNodeCbx      := CreateFieldComboBox  (FAppModules, lOwner, lParent, 140,  40, 200,  21, 1, TRUE, csDropDownList);
    FDownstreamNodeLabel  := CreateFieldLabel                  (lOwner, lParent,  10,  68, 130,  26);
    FDownstreamNodeLabel.WordWrap := TRUE;
    FDownStreamNodeCbx    := CreateFieldComboBox  (FAppModules, lOwner, lParent, 140,  70, 200,  21, 2, TRUE, csDropDownList);

    FIrrigationAreaXCoordLabel := CreateFieldLabel     (lOwner, lParent,  10,  100, 200,  21);
    FIrrigationAreaYCoordLabel := CreateFieldLabel     (lOwner, lParent,  10,  130, 200,  21);
    FIrrigationAreaXCoordEdit  := CreateFieldEdit      (FAppModules, lOwner, lParent, 140,  100, 50,  21, 0, TRUE);
    FIrrigationAreaYCoordEdit  := CreateFieldEdit      (FAppModules, lOwner, lParent, 140,  130, 50,  21, 0, TRUE);

    FRelaxationRadioGroup := CreateFieldRadioGroup(FAppModules, lOwner, lParent,  360, 5, 311,  87, 1, FALSE);
    FMonthlyFlowsLabel    := CreateFieldLabel                  (lOwner, lParent,  10, 160, 120,  21);
    FMonthlyFlowsGrid     := CreateFieldStringGrid(FAppModules, lOwner, lParent, 134, 160, 257, 276, 2, TRUE);
    with FMonthlyFlowsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 4;
      RowCount         := 13;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 60;
      ColWidths[3]     := 70;
      DefaultDrawing   := FALSE;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDialog.Resize;
const OPNAME = 'TIrrigationAreaDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaDialog.Initialise: boolean;
const OPNAME = 'TIrrigationAreaDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrigationAreaDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption     := FAppModules.Language.GetString('TField.AreaName') + ' :';
    FUpstreamNodeLabel.Caption    := FAppModules.Language.GetString('NetworkFeatures.DiversionUpstreamNode') + ' :';
    FDownstreamNodeLabel.Caption  := FAppModules.Language.GetString('NetworkFeatures.ReturnFlowDownstreamNode') + ' :';
    FRelaxationRadioGroup.Caption := ' ' + FAppModules.Language.GetString('TField.RelaxationDemand') + ' ';
    FMonthlyFlowsLabel.Caption    := FAppModules.Language.GetString('NetworkFeatures.MonthlyIrrigationFlows') + ' :';
    FMonthlyFlowsGrid.Cells[1, 0] := FAppModules.Language.GetString('NetworkFeatures.Diversion');
    FMonthlyFlowsGrid.Cells[2, 0] := FAppModules.Language.GetString('NetworkFeatures.Return');
    FMonthlyFlowsGrid.Cells[3, 0] := FAppModules.Language.GetString('NetworkFeatures.Consumption');
    FRelaxationRadioGroup.Hints.Clear;
    FRelaxationRadioGroup.Hints.Add(FAppModules.Language.GetString('NetworkFeatures.IrrigationSuppy'));
    FRelaxationRadioGroup.Hints.Add(FAppModules.Language.GetString('NetworkFeatures.ReturnFlow'));
    FRelaxationRadioGroup.Hints.Add(FAppModules.Language.GetString('NetworkFeatures.IrrigationAndReturnFlow'));
    FIrrigationAreaXCoordLabel.Caption     := FAppModules.Language.GetString('TField.XCoord');
    FIrrigationAreaYCoordLabel.Caption     := FAppModules.Language.GetString('TField.YCoord');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDialog.RestoreColourState;
const OPNAME = 'TIrrigationAreaDialog.RestoreColourState';
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

procedure TIrrigationAreaDialog.AssignHelpContext;
const OPNAME = 'TIrrigationAreaDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                    HC_IrrigationAreas);
    SetControlHelpContext(FFeatureNameEdit,        HC_IrrigationAreas);
    SetControlHelpContext(FUpStreamNodeCbx,        HC_IrrigationAreas);
    SetControlHelpContext(FDownStreamNodeCbx,      HC_IrrigationAreas);
    SetControlHelpContext(FRelaxationRadioGroup,   HC_IrrigationAreas);
    SetControlHelpContext(FMonthlyFlowsGrid,       HC_IrrigationAreas);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
