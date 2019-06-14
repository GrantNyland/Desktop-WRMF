{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentreDialog                      *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/11/10                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentreDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Grids,
  Windows,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TYMDemandCentreDialog = class(TAbstractScrollablePanel)
  protected
    FNodeNumberLabel            : TLabel;
    FDescriptionLabel           : TLabel;
    FNameLabel                  : TLabel;
    FAveReturnFlowFactorLabel   : TLabel;
    FAveMonthlyEvaporationLabel : TLabel;
    FRoutingConstantLabel       : TLabel;
    FRainScalingFactorLabel     : TLabel;
    FTotalFlowLostLabel         : TLabel;
    FStdDeviationFactorLabel    : TLabel;
    FNodeNumberRefCbxLabel      : TLabel;
    FConsumptiveChannelLabel    : TLabel;
    FReclaimationChannelChkLabel: TLabel;
    FDemandCentreXCoordLabel    : TLabel;
    FDemandCentreYCoordLabel    : TLabel;

    FNodeNumberEdit             : TFieldEdit;
    FDescriptionEdit            : TFieldEdit;
    FNameEdit                   : TFieldEdit;
    FAveReturnFlowFactorEdit    : TFieldEdit;
    FAveMonthlyEvaporationEdit  : TFieldEdit;
    FRoutingConstantEdit        : TFieldEdit;
    FRainScalingFactorEdit      : TFieldEdit;
    FTotalFlowLost              : TFieldEdit;
    FStdDeviationFactor         : TFieldEdit;
    FNodeNumberRefCbx           : TFieldComboBox;
    FDemandCentreXCoordEdit     : TFieldEdit;
    FDemandCentreYCoordEdit     : TFieldEdit;
    FConsumptiveChannelCbx      : TFieldComboBox;
    FReclaimationChannelChkBox  : TFieldChkBox;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    property DemandCentreNameEdit      : TFieldEdit      read FNameEdit;
    property DescriptionEdit           : TFieldEdit      read FDescriptionEdit;
    property NodeNumberEdit            : TFieldEdit      read FNodeNumberEdit;
    property AveReturnFlowFactorEdit   : TFieldEdit      read FAveReturnFlowFactorEdit;
    property AveMonthlyEvaporationEdit : TFieldEdit      read FAveMonthlyEvaporationEdit;
    property RoutingConstantEdit       : TFieldEdit      read FRoutingConstantEdit;
    property RainScalingFactorEdit     : TFieldEdit      read FRainScalingFactorEdit;
    property TotalFlowLostEdit         : TFieldEdit      read FTotalFlowLost;
    property StdDeviationFactorEdit    : TFieldEdit      read FStdDeviationFactor;
    property NodeNumberRefCbx          : TFieldComboBox  read FNodeNumberRefCbx;
    property ConsumptiveChannelCbx     : TFieldComboBox  read FConsumptiveChannelCbx;
    property ReclaimationChannelCbx    : TFieldChkBox    read FReclaimationChannelChkBox;
    property DemandCentreXCoordEdit    : TFieldEdit      read FDemandCentreXCoordEdit;
    property DemandCentreYCoordEdit    : TFieldEdit      read FDemandCentreYCoordEdit;

  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TYMDemandCentreDialog                                                      *}
{******************************************************************************}

procedure TYMDemandCentreDialog.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

                                                           //Left  Top Width Height
    FNodeNumberLabel        := CreateFieldLabel            (lOwner, lParent, 10,   5,  200, 21);
    FNodeNumberEdit         := CreateFieldEdit(FAppModules, lOwner, lParent, 350,  5,  100, 20, 8, TRUE);

    FNameLabel              := CreateFieldLabel            (lOwner, lParent, 10,  35,  420, 21);
    FNameEdit               := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 35,  300, 20, 8, TRUE);

    FDescriptionLabel       := CreateFieldLabel            (lOwner, lParent, 10,  65,  420, 21);
    FDescriptionEdit        := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 65,  300, 20, 8, TRUE);

    FNodeNumberRefCbxLabel  := CreateFieldLabel                (lOwner, lParent, 10,  95,  340, 21);
    FNodeNumberRefCbx       := CreateFieldComboBox(FAppModules, lOwner, lParent, 350,  95, 300,  20, 8, TRUE, csDropDownList);

    FAveReturnFlowFactorLabel     := CreateFieldLabel            (lOwner, lParent, 10,  125,  420, 21);
    FAveReturnFlowFactorEdit      := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 125,  100, 20, 8, TRUE);

    FAveMonthlyEvaporationLabel  := CreateFieldLabel            (lOwner, lParent, 10,  155,  300, 30);
    FAveMonthlyEvaporationEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 155,  100, 20, 8, TRUE);

    FStdDeviationFactorLabel  := CreateFieldLabel            (lOwner, lParent, 10,  185,  300, 30);
    FStdDeviationFactor       := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 185,  100, 20, 8, TRUE);

    FRoutingConstantLabel   := CreateFieldLabel            (lOwner, lParent, 10,  215,  300, 30);
    FRoutingConstantEdit    := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 215,  100, 20, 8, TRUE);

    FRainScalingFactorLabel := CreateFieldLabel            (lOwner, lParent, 10,  245,  300, 30);
    FRainScalingFactorEdit  := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 245,  100, 20, 8, TRUE);
                                                             //Left  Top Width Height
    FTotalFlowLostLabel     := CreateFieldLabel            (lOwner, lParent, 10,  275,  300, 30);
    FTotalFlowLost          := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 275,  100, 20, 8, TRUE);

    FDemandCentreXCoordLabel  := CreateFieldLabel            (lOwner, lParent, 10,  305,  300, 30);
    FDemandCentreXCoordEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 305,  100, 20, 8, TRUE);
    FDemandCentreYCoordLabel  := CreateFieldLabel            (lOwner, lParent, 10,  335,  300, 30);
    FDemandCentreYCoordEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 350, 335,  100, 20, 8, TRUE);


    FConsumptiveChannelLabel := CreateFieldLabel            (lOwner, lParent, 10,  365,  300, 30);
    FConsumptiveChannelCbx   := CreateFieldComboBox(FAppModules, lOwner, lParent, 350,  365, 300,  20, 8, TRUE, csDropDownList);

    FReclaimationChannelChkLabel    := CreateFieldLabel            (lOwner, lParent, 10,  395,  300, 30);
    FReclaimationChannelChkBox           := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FReclaimationChannelChkBox.Parent    := lParent;
    FReclaimationChannelChkBox.Left      := 350;
    FReclaimationChannelChkBox.Top       := 395;
    FReclaimationChannelChkBox.Alignment := taRightJustify;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreDialog.Resize;
const OPNAME = 'TYMDemandCentreDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreDialog.Initialise: boolean;
const OPNAME = 'TYMDemandCentreDialog.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreDialog.LanguageHasChanged: boolean;
const OPNAME = 'TYMDemandCentreDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNodeNumberLabel.Caption            := FAppModules.Language.GetString('TField.YMDemandCentreNodeNumber');
    FNameLabel.Caption                  := FAppModules.Language.GetString('TField.YMDemandCentreName');
    FDescriptionLabel.Caption           := FAppModules.Language.GetString('TField.YMDemandCentreDescription');
    FAveReturnFlowFactorLabel.Caption   := FAppModules.Language.GetString('TField.AveReturnFlowFactor');
    FAveMonthlyEvaporationLabel.Caption := FAppModules.Language.GetString('TField.AveEvaporation');
    FRoutingConstantLabel.Caption       := FAppModules.Language.GetString('TField.YMDemandCentreRoutingConstant');
    FRainScalingFactorLabel.Caption     := FAppModules.Language.GetString('TField.RainfallScalingFactor');
    FNodeNumberRefCbxLabel.Caption      := FAppModules.Language.GetString('TField.NodeRefNr');
    FTotalFlowLostLabel.Caption         := FAppModules.Language.GetString('TField.TotalFlowLost');
    FStdDeviationFactorLabel.Caption    := FAppModules.Language.GetString('TField.StdDeviationFactor');
    FDemandCentreXCoordLabel.Caption    := FAppModules.Language.GetString('TField.XCoord');
    FDemandCentreYCoordLabel.Caption    := FAppModules.Language.GetString('TField.YCoord');

    FConsumptiveChannelLabel.Caption            := FAppModules.Language.GetString('TField.ConsumptiveChannel');
    FReclaimationChannelChkLabel.Caption        := FAppModules.Language.GetString('TField.ReclaimtionChannel');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreDialog.RestoreColourState;
const OPNAME = 'TYMDemandCentreDialog.RestoreColourState';
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

procedure TYMDemandCentreDialog.AssignHelpContext;
const OPNAME = 'TYMDemandCentreDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                        HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FNodeNumberEdit,             HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FDescriptionEdit,            HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FNameEdit,                   HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FAveReturnFlowFactorEdit,    HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FAveMonthlyEvaporationEdit,  HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FRoutingConstantEdit,        HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FRainScalingFactorEdit,      HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FTotalFlowLost,              HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FStdDeviationFactor,         HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FNodeNumberRefCbx,           HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FDemandCentreXCoordEdit,     HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FDemandCentreYCoordEdit,     HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FConsumptiveChannelCbx,      HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FReclaimationChannelChkBox,  HC_ReturnFlowsFromLargeUrbanCentres);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
