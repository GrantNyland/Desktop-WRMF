{******************************************************************************}
{*  UNIT      : Contains the class TWaterDemandFeatureDialog.                 *}
{*  AUTHOR    : Dziedzi Ramulondi                                             *}
{*  DATE      : 2004/10/20                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UWaterDemandFeatureDialog;

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

  TWaterDemandFeatureDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel         : TLabel;
    FFeatureNameEdit          : TFieldEdit;
    FWaterUseCategoryLabel    : TLabel;
    FWaterUseCategoryComboBox : TFieldComboBox;
    FProportionWaterUseLabel  : TLabel;
    FProportionWaterUseGrid   : TFieldStringGrid;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetSenarioCount(ACount: integer);
    property FeatureNameEdit          : TFieldEdit       read FFeatureNameEdit;
    property WaterUseCategoryComboBox : TFieldComboBox   read FWaterUseCategoryComboBox;
    property ProportionWaterUseGrid   : TFieldStringGrid read FProportionWaterUseGrid;
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
{* TWaterDemandFeatureDialog                                                    *}
{******************************************************************************}

procedure TWaterDemandFeatureDialog.CreateMemberObjects;
const OPNAME = 'TWaterDemandFeatureDialog.CreateMemberObjects';
begin
  inherited;
  try
    //                                                                                            Left  Top  Width Height
    FFeatureNameLabel         := CreateFieldLabel                  (ControlsOwner, ControlsParent,  10,  10, 140, 21);
    FFeatureNameEdit          := CreateFieldEdit      (FAppModules, ControlsOwner, ControlsParent, 160,  10, 180, 21, 0, TRUE);
    FWaterUseCategoryLabel    := CreateFieldLabel                  (ControlsOwner, ControlsParent,  10,  35, 140, 21);
    FWaterUseCategoryComboBox := CreateFieldComboBox  (FAppModules, ControlsOwner, ControlsParent, 160,  35, 300, 21, 1, TRUE, csDropDownList);
    FProportionWaterUseLabel  := CreateFieldLabel                  (ControlsOwner, ControlsParent,  10,  60, 140, 21);
    FProportionWaterUseGrid   := CreateFieldStringGrid(FAppModules, ControlsOwner, ControlsParent, 160,  60, 300, 200, 2, TRUE);
    with FProportionWaterUseGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 2;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 18;
      DefaultColWidth  := 100;
      ColWidths[0]     := 80;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureDialog.Resize;
const OPNAME = 'TWaterDemandFeatureDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureDialog.Initialise: boolean;
const OPNAME = 'TWaterDemandFeatureDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FFeatureNameEdit.Text := '';
    FWaterUseCategoryComboBox.Items.Clear;
    FProportionWaterUseGrid.RowCount := 2;
    FProportionWaterUseGrid.ColCount := 2;
    SetSenarioCount(0);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureDialog.SetSenarioCount(ACount: integer);
const OPNAME = 'TWaterDemandFeatureDialog.SetSenarioCount';
var
  LIndex: integer;
begin
  try

    if(ACount > 0) then
    begin
      FProportionWaterUseGrid.RowCount := ACount;
      FProportionWaterUseGrid.Options  := FProportionWaterUseGrid.Options + [goEditing];
      FProportionWaterUseGrid.Enabled  := True;
    end
    else
    begin
      FProportionWaterUseGrid.RowCount := 1;
      FProportionWaterUseGrid.Options  := FProportionWaterUseGrid.Options - [goEditing];
      FProportionWaterUseGrid.Enabled  := False;
    end;
    //FProportionWaterUseGrid.Height     := (FProportionWaterUseGrid.DefaultRowHeight *
    //                                      FProportionWaterUseGrid.RowCount)+ 5;

    for LIndex := 1 to FProportionWaterUseGrid.ColCount -1 do
      FProportionWaterUseGrid.Cols[LIndex].Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterDemandFeatureDialog.LanguageHasChanged: boolean;
const OPNAME = 'TWaterDemandFeatureDialog.LanguageHasChanged';
var
  lLanguage : TAbstractLanguage;
  LIndex: integer;
begin
  Result := inherited LanguageHasChanged;
  try
    lLanguage := FAppModules.Language;
    FFeatureNameLabel.Caption         := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    FWaterUseCategoryLabel.Caption    := lLanguage.GetString('TField.WaterDemandCategory') + ' :';
    FProportionWaterUseLabel.Caption  := lLanguage.GetString('TField.ScenarioPortion') + ' :';
    for LIndex := 0 to FProportionWaterUseGrid.RowCount -1 do
      FProportionWaterUseGrid.Cells[0,LIndex] := lLanguage.GetString('TField.Scenario') +' ' + IntToStr(LIndex+1);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureDialog.AssignHelpContext;
const OPNAME = 'TWaterDemandFeatureDialog.AssignHelpContext';
begin
  try
    {SetControlHelpContext(FDivFeatureNameEdit,   HC_WaterDemandFeatureName);
    SetControlHelpContext(FLossFeatureNameEdit,  HC_WaterDemandFeatureName);
    SetControlHelpContext(FDivTypeRadioGroup,    HC_DiversionChannelTypes);
    SetControlHelpContext(FLossTypeRadioGroup,   HC_LossChannelTypes);
    SetControlHelpContext(FLossType1GroupBox,    HC_LossChannelTypes);
    SetControlHelpContext(FType1HeadPanel1,      HC_MonthlyDivertedDemands);
    SetControlHelpContext(FType1HeadPanel2,      HC_MonthlyDivertedDemands);
    SetControlHelpContext(FType1Grid,            HC_MonthlyDivertedDemands);

    SetControlHelpContext(FLossType1HeadPanel1,  HC_FlowRangeGrid);
    SetControlHelpContext(FLossType1HeadPanel2,  HC_FlowRangeGrid);
    SetControlHelpContext(FType2Grid,            HC_FlowRangeGrid);

    SetControlHelpContext(FType2HeadPanel1,      HC_FlowRangeGrid);
    SetControlHelpContext(FType1HeadPanel2,      HC_MonthlyDivertedDemands);

    SetControlHelpContext(FType2GroupBox,        HC_LossChannelTypes);
    SetControlHelpContext(FType3HeadPanel1,      HC_LossChannelTypes);
    SetControlHelpContext(FType1HeadPanel2,      HC_LossChannelTypes);

    SetControlHelpContext(FType3GroupBox,        HC_LossChannelTypes);
    SetControlHelpContext(FType3FlowsGrid,       HC_ReferenceFlowValues);
    SetControlHelpContext(FType3LevelsGrid,      HC_ReservoirWaterStorageLevels);
    SetControlHelpContext(FType3ProportionsGrid, HC_ProportionOfFlowDiverted);
    SetControlHelpContext(FType3Panel,           HC_ProportionOfFlowDiverted);

    SetControlHelpContext(FReservoirCbx,         HC_ControllingReservoirs);
    SetControlHelpContext(FNrOfFlowsEdit,        HC_NumberOfReferenceFlows);
    SetControlHelpContext(FNrOfLevelsEdit,       HC_NumberOfWateStorageLevels);
    SetControlHelpContext(FMinimumEdit,          HC_ReservoirMinimumLevels);
    SetControlHelpContext(FMaximumEdit,          HC_ReservoirMaximumLevels);
    }
   except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
