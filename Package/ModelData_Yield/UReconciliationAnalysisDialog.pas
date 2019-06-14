{******************************************************************************}
{*  UNIT      : Contains the class TReconciliationAnalysisDialog.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UReconciliationAnalysisDialog;

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

  TReconciliationAnalysisDialog = class(TAbstractScrollablePanel)
  private
  protected
    FReconciliationAnalysisCheck : TFieldChkBox;
    FNrOfCategoriesLabel         : TLabel;
    FNrOfAssurancesLabel         : TLabel;
    FAssurancesLabel             : TLabel;
    FCategoriesLabel             : TLabel;
    FChannelsLabel               : TLabel;
    FNrOfCategoriesEdit          : TFieldEdit;
    FNrOfAssurancesEdit          : TFieldEdit;
    FAssurancesGrid              : TFieldStringGrid;
    FReconciliationGrid          : TFieldStringGrid;
    FChannelsListBox             : TFieldListBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property ReconciliationAnalysisCheck : TFieldChkBox     read FReconciliationAnalysisCheck;
    property NrOfCategoriesEdit          : TFieldEdit       read FNrOfCategoriesEdit;
    property NrOfAssurancesEdit          : TFieldEdit       read FNrOfAssurancesEdit;
    property AssurancesGrid              : TFieldStringGrid read FAssurancesGrid;
    property ReconciliationGrid          : TFieldStringGrid read FReconciliationGrid;
    property ChannelsListBox             : TFieldListBox    read FChannelsListBox;
  end;

  implementation

uses
  VCL.Grids,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TReconciliationAnalysisDialog                                              *}
{******************************************************************************}

procedure TReconciliationAnalysisDialog.CreateMemberObjects;
const OPNAME = 'TReconciliationAnalysisDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FReconciliationAnalysisCheck := TFieldChkBox.Create(lOwner, FAppModules);
    FReconciliationAnalysisCheck.Parent := lParent;
    FReconciliationAnalysisCheck.Left   := 10;
    FReconciliationAnalysisCheck.Top    := 10;
    FReconciliationAnalysisCheck.Width  := 215;
    FReconciliationAnalysisCheck.Alignment := taLeftJustify;

    //                                                                Left  Top Width Height
    FNrOfCategoriesLabel      := CreateFieldLabel   (lOwner, lParent,  10,  40, 190, 21);
    FNrOfAssurancesLabel      := CreateFieldLabel   (lOwner, lParent,  10,  70, 190, 21);
    FAssurancesLabel          := CreateFieldLabel   (lOwner, lParent,  10,  100, 190, 21);
    FCategoriesLabel          := CreateFieldLabel   (lOwner, lParent,  10, 130, 200, 21);
    FChannelsLabel            := CreateFieldLabel   (lOwner, lParent, 452, 123,  96, 26);
    with FChannelsLabel do
    begin
      Alignment := taCenter;
      AutoSize  := FALSE;
      WordWrap  := TRUE;
    end;
    FNrOfCategoriesEdit     := CreateFieldEdit      (FAppModules, lOwner, lParent, 210,  40,  30,  21, 0, TRUE);
    FNrOfAssurancesEdit     := CreateFieldEdit      (FAppModules, lOwner, lParent, 210,  68,  30,  21, 1, TRUE);
    FAssurancesGrid         := CreateFieldStringGrid(FAppModules, lOwner, lParent, 210,  100, 167,  24, 2, TRUE);
    FReconciliationGrid     := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, 150, 410, 176, 3, TRUE);
    with FAssurancesGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      DefaultColWidth  := 40;
      DefaultRowHeight := 20;
      FixedCols        := 0;
      RowCount         := 1;
      FixedRows        := 0;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
    with FReconciliationGrid do
    begin
      ColCount         := 6;
      DefaultColWidth  := 40;
      DefaultRowHeight := 20;
      RowCount         := 4;
      ColWidths[0]     := 50;
      ColWidths[1]     := 150;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
    FChannelsListBox := TFieldListBox.Create(lOwner, FAppModules);
    with  FChannelsListBox do
    begin
      Parent     := lParent;
      Top        := 150;
      Left       := 425;
      Width      := 155;
      Height     := 180;
      TabStop    := FALSE;
      TabOrder   := 4;
    end;
    {
    Caption = 'Number of water use categories :'
    Caption = 'Number of risk criteria / assurances :'
    Caption = 'Channels in water use category'
    Caption = 'Assurances :'
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisDialog.Resize;
const OPNAME = 'TReconciliationAnalysisDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisDialog.Initialise: boolean;
const OPNAME = 'TReconciliationAnalysisDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReconciliationAnalysisDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FReconciliationAnalysisCheck.Caption := FAppModules.Language.GetString('NetworkFeatures.ReconciliationAnalysis')    + ' :';
    FNrOfCategoriesLabel.Caption := FAppModules.Language.GetString('TField.WaterDemandCategoryCount')    + ' :';
    FNrOfAssurancesLabel.Caption := FAppModules.Language.GetString('TField.WaterDemandRiskCriteriaCount')    + ' :';
    FAssurancesLabel.Caption     := FAppModules.Language.GetString('TField.RecurrenceInterval')    + ' :';
    FCategoriesLabel.Caption     := FAppModules.Language.GetString('TField.WaterDemandCategory')    + ' :';
    FChannelsLabel.Caption       := FAppModules.Language.GetString('NetworkFeatures.ChannelsInWaterDemandCategory')    + ' :';
    FReconciliationGrid.Cells[0,0] := FAppModules.Language.GetString('TField.TheNumber');
    FReconciliationGrid.Cells[1,0] := FAppModules.Language.GetString('NetworkFeatures.WaterUseCategoryDescr');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisDialog.RestoreColourState;
const OPNAME = 'TReconciliationAnalysisDialog.RestoreColourState';
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

procedure TReconciliationAnalysisDialog.AssignHelpContext;
const OPNAME = 'TReconciliationAnalysisDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FNrOfCategoriesEdit, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FNrOfAssurancesEdit, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FAssurancesGrid,     HC_WaterResourcesYieldModel);
    SetControlHelpContext(FReconciliationGrid, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FChannelsListBox,    HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
