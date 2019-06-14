{******************************************************************************}
{*  UNIT      : Contains the class TWetlandDialog                             *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/08/11                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UWetlandDialog;

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

  TWetlandDialog = class(TAbstractScrollablePanel)
  protected
    FPenaltyStructureLabel   : TLabel;
    FPenaltyStructLabel      : TLabel;
    FPenaltyStructEdit       : TFieldEdit;
    FSelectPenaltyStruct     : TFieldBitBtn;
    FPriorityLabel           : TLabel;
    FPriorityEdit            : TFieldEdit;
    FRainCoeffLabel          : TLabel;
    FRainCoeffEdit           : TFieldEdit;
    FSummaryIncludeLabel     : TLabel;
    FSummaryIncludeChkBox    : TFieldChkBox;
    FReservoirExistsLabel    : TLabel;
    FReservoirExistsChkBox   : TFieldChkBox;
    FDamLevelsFileNameLabel     : TLabel;
    FDamLevelsFileNameCbx       : TFieldComboBox;
    FDamLevelsFileNameSelectBtn : TFieldBitBtn;
    FDamLevelsFileNameGridBtn   : TFieldBitBtn;
    FDamLevelsFileNameGraphBtn  : TFieldBitBtn;

    FNodeNumberLabel         : TLabel;
    FNameLabel               : TLabel;
    FStorageVolumeLabel      : TLabel;
    FInflowProportionLabel   : TLabel;
    FOutflowProportionLabel  : TLabel;
    FUpstreamThresholdLabel  : TLabel;
    FInflowLabel             : TLabel;
    FOutflowLabel            : TLabel;

    FNodeNumberEdit          : TFieldEdit;
    FNameEdit                : TFieldEdit;
    FStorageVolumeEdit       : TFieldEdit;
    FInflowProportionEdit    : TFieldEdit;
    FOutflowProportionEdit   : TFieldEdit;
    FUpstreamThresholdEdit   : TFieldEdit;
    FInflowCbx               : TFieldComboBox;
    FOutflowCbx              : TFieldComboBox;

    FWetlandXCoordLabel      : TLabel;
    FWetlandXCoordEdit       : TFieldEdit;
    FWetlandYCoordLabel      : TLabel;
    FWetlandYCoordEdit       : TFieldEdit;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    property NodeNumberEdit         : TFieldEdit      read FNodeNumberEdit;
    property NameEdit               : TFieldEdit      read FNameEdit;
    property StorageVolumeEdit      : TFieldEdit      read FStorageVolumeEdit;
    property InflowProportionEdit   : TFieldEdit      read FInflowProportionEdit;
    property OutflowProportionEdit  : TFieldEdit      read FOutflowProportionEdit;
    property UpstreamThresholdEdit  : TFieldEdit      read FUpstreamThresholdEdit;
    property InflowCbx              : TFieldComboBox  read FInflowCbx;
    property OutflowCbx             : TFieldComboBox  read FOutflowCbx;

    property PenaltyStructureEdit       : TFieldEdit       read FPenaltyStructEdit;
    property PriorityEdit               : TFieldEdit       read FPriorityEdit;
    property RainCoeffEdit              : TFieldEdit       read FRainCoeffEdit;
    property SummaryIncludeChkBox       : TFieldChkBox     read FSummaryIncludeChkBox;
    property ReservoirExistsChkBox      : TFieldChkBox     read FReservoirExistsChkBox;
    property SelectPenaltyStruct        : TFieldBitBtn     read FSelectPenaltyStruct;

    property DamLevelsFileNameCbx       : TFieldComboBox   read FDamLevelsFileNameCbx;
    property DamLevelsFileNameSelectBtn : TFieldBitBtn     read FDamLevelsFileNameSelectBtn;
    property DamLevelsFileNameGridBtn   : TFieldBitBtn     read FDamLevelsFileNameGridBtn;
    property DamLevelsFileNameGraphBtn  : TFieldBitBtn     read FDamLevelsFileNameGraphBtn;

    property WetlandXCoordEdit  : TFieldEdit read FWetlandXCoordEdit;
    property WetlandYCoordEdit  : TFieldEdit read FWetlandYCoordEdit;

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
{* TWetlandDialog                                                           *}
{******************************************************************************}

procedure TWetlandDialog.CreateMemberObjects;
const OPNAME = 'TWetlandDialog.CreateMemberObjects';
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
    FNodeNumberEdit         := CreateFieldEdit(FAppModules, lOwner, lParent, 320,  5,  100, 20, 8, TRUE);

    FNameLabel              := CreateFieldLabel            (lOwner, lParent, 10,  35,  420, 21);
    FNameEdit               := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 35,  300, 20, 8, TRUE);

    FStorageVolumeLabel     := CreateFieldLabel            (lOwner, lParent, 10,  65,  420, 21);
    FStorageVolumeEdit      := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 65,  100, 20, 8, TRUE);

    FInflowProportionLabel  := CreateFieldLabel            (lOwner, lParent, 10,  90,  300, 30);
    FInflowProportionLabel.WordWrap := True;
    FInflowProportionEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 95,  100, 20, 8, TRUE);

    FOutflowProportionLabel := CreateFieldLabel            (lOwner, lParent, 10,  120,  300, 30);
    FOutflowProportionLabel.WordWrap := True;
    FOutflowProportionEdit  := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 125,  100, 20, 8, TRUE);

    FUpstreamThresholdLabel := CreateFieldLabel            (lOwner, lParent, 10,  150,  300, 30);
    FUpstreamThresholdLabel.WordWrap := True;
    FUpstreamThresholdEdit  := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 155,  100, 20, 8, TRUE);

    FInflowLabel            := CreateFieldLabel                (lOwner, lParent, 10,  185, 300, 21);
    FInflowCbx              := CreateFieldComboBox(FAppModules, lOwner, lParent, 320, 185, 200,  20, 1, TRUE, csDropDownList);

    FOutflowLabel           := CreateFieldLabel                (lOwner, lParent, 10,  215, 300, 21);
    FOutflowCbx             := CreateFieldComboBox(FAppModules, lOwner, lParent, 320, 215, 200,  20, 1, TRUE, csDropDownList);

    FPenaltyStructLabel     := CreateFieldLabel                (lOwner, lParent, 10,  245, 300, 21);
    FPenaltyStructEdit      := CreateFieldEdit    (FAppModules, lOwner, lParent, 320, 245, 100, 20, 8, TRUE);

    // Create the Penalty Structure Select Button.
    FSelectPenaltyStruct            := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FSelectPenaltyStruct.Parent     := lParent;
    FSelectPenaltyStruct.Left       := 425;
    FSelectPenaltyStruct.Top        := 245;
    FSelectPenaltyStruct.Height     := 21;
    FSelectPenaltyStruct.Width      := 38;
    FSelectPenaltyStruct.Glyph.LoadFromResourceName(HImagesInstance, 'IMPORTFILES');
    FSelectPenaltyStruct.NumGlyphs  := 2;

    FPriorityLabel  := CreateFieldLabel            (lOwner, lParent, 10,  275, 300, 21);
    FPriorityEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 275, 100, 20, 8, TRUE);

    FRainCoeffLabel  := CreateFieldLabel            (lOwner, lParent, 10,  305, 300, 21);
    FRainCoeffEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 305, 100, 20, 8, TRUE);


    FWetlandXCoordLabel  := CreateFieldLabel            (lOwner, lParent, 10,  335, 300, 21);
    FWetlandXCoordEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 335, 100, 20, 8, TRUE);

    FWetlandYCoordLabel  := CreateFieldLabel            (lOwner, lParent, 10,  365, 300, 21);
    FWetlandYCoordEdit   := CreateFieldEdit(FAppModules, lOwner, lParent, 320, 365, 100, 20, 8, TRUE);

    //create the historic Water level label/comboBox
    FDamLevelsFileNameLabel  := CreateFieldLabel(lOwner, lParent, 10,  395, 300, 21);
    FDamLevelsFileNameCbx    := CreateFieldComboBox(FAppModules, lOwner, lParent, 320, 395, 200,  20, 1, TRUE, csDropDownList);

    FDamLevelsFileNameSelectBtn            := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FDamLevelsFileNameSelectBtn.Parent     := lParent;
    FDamLevelsFileNameSelectBtn.Top        := FDamLevelsFileNameCbx.Top;
    FDamLevelsFileNameSelectBtn.Height     := FDamLevelsFileNameCbx.Height;
    FDamLevelsFileNameSelectBtn.Width      := 30;
    FDamLevelsFileNameSelectBtn.Left       := FDamLevelsFileNameCbx.Left + FDamLevelsFileNameCbx.Width + 2;
    FDamLevelsFileNameSelectBtn.Caption    := FAppModules.language.GetString('ButtonCaption.SelectDemandFile');
    FDamLevelsFileNameSelectBtn.Font.Name  := 'Arial';
    FDamLevelsFileNameSelectBtn.Font.Style := [fsBold];

    FDamLevelsFileNameGridBtn              := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FDamLevelsFileNameGridBtn.Parent       := lParent;
    FDamLevelsFileNameGridBtn.Top          := FDamLevelsFileNameCbx.Top;
    FDamLevelsFileNameGridBtn.Height       := FDamLevelsFileNameCbx.Height;
    FDamLevelsFileNameGridBtn.Width        := 30;
    FDamLevelsFileNameGridBtn.Left         := FDamLevelsFileNameSelectBtn.Left + FDamLevelsFileNameSelectBtn.Width + 2;
    FDamLevelsFileNameGridBtn.Glyph.LoadFromResourceName(HImagesInstance, 'VIEWDATAGRID');
    FDamLevelsFileNameGridBtn.NumGlyphs := 2;

    FDamLevelsFileNameGraphBtn              := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FDamLevelsFileNameGraphBtn.Parent       := lParent;
    FDamLevelsFileNameGraphBtn.Top          := FDamLevelsFileNameCbx.Top;
    FDamLevelsFileNameGraphBtn.Height       := FDamLevelsFileNameCbx.Height;
    FDamLevelsFileNameGraphBtn.Width        := 30;
    FDamLevelsFileNameGraphBtn.Left         := FDamLevelsFileNameGridBtn.Left + FDamLevelsFileNameGridBtn.Width + 2;
    FDamLevelsFileNameGraphBtn.Glyph.LoadFromResourceName(HImagesInstance, 'VIEWDATAGRAPH');
    FDamLevelsFileNameGraphBtn.NumGlyphs := 2;

        // Create the Include summary  label.
    FSummaryIncludeLabel            := CreateFieldLabel(lOwner, lParent, 10,  425, 300, 21);
    // Create the Include summary check box.
    FSummaryIncludeChkBox           := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FSummaryIncludeChkBox.Parent    := lParent;
    FSummaryIncludeChkBox.Left      := 320;
    FSummaryIncludeChkBox.Top       := 425;
    FSummaryIncludeChkBox.Alignment := taRightJustify;

    // Create the Reservoir Exists  label/check box.
    FReservoirExistsLabel             := CreateFieldLabel(lOwner, lParent, 10,  455, 300, 21);
    FReservoirExistsChkBox            := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FReservoirExistsChkBox.Parent     := lParent;
    FReservoirExistsChkBox.Left       := 320;
    FReservoirExistsChkBox.Top        := 455;
    FReservoirExistsChkBox.Alignment  := taRightJustify;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandDialog.Resize;
const OPNAME = 'TWetlandDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandDialog.Initialise: boolean;
const OPNAME = 'TWetlandDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandDialog.LanguageHasChanged: boolean;
const OPNAME = 'TWetlandDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNodeNumberLabel.Caption        := FAppModules.Language.GetString('TField.WetlandNodeNumber');
    FNameLabel.Caption              := FAppModules.Language.GetString('TField.WetlandName');
    FStorageVolumeLabel.Caption     := FAppModules.Language.GetString('TField.StorageVolume');
    FInflowProportionLabel.Caption  := FAppModules.Language.GetString('TField.InflowProportion');
    FOutflowProportionLabel.Caption := FAppModules.Language.GetString('TField.OutflowProportion');
    FUpstreamThresholdLabel.Caption := FAppModules.Language.GetString('TField.UpstreamThreshold');
    FInflowLabel.Caption            := FAppModules.Language.GetString('TField.WetlandUpStreamNode');
    FOutflowLabel.Caption           := FAppModules.Language.GetString('TField.WetlandDownStreamNode');

    FPenaltyStructLabel.Caption     := FAppModules.Language.GetString('TField.PenaltyStruct');
    FSummaryIncludeLabel.Caption    := FAppModules.Language.GetString('TField.IncludeSummary');
    FReservoirExistsLabel.Caption   := FAppModules.Language.GetString('TField.StatusIndicatorWetland');
    FSelectPenaltyStruct.Hint       := FAppModules.Language.GetString('ButtonHint.Select');
    FPriorityLabel.Caption          := FAppModules.Language.GetString('TField.ReservoirPriority');
    FRainCoeffLabel.Caption         := FAppModules.Language.GetString('TField.RainCoef');
    FWetlandXCoordLabel.Caption     := FAppModules.Language.GetString('TField.XCoord');
    FWetlandYCoordLabel.Caption     := FAppModules.Language.GetString('TField.YCoord');
    FDamLevelsFileNameLabel.Caption := FAppModules.Language.GetString('TField.WetlandLevelsFileName');
    
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandDialog.RestoreColourState;
const OPNAME = 'TWetlandDialog.RestoreColourState';
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

procedure TWetlandDialog.AssignHelpContext;
const OPNAME = 'TWetlandDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                   HC_Wetlands);
    SetControlHelpContext(FNodeNumberEdit,        HC_Wetlands);
    SetControlHelpContext(FNameEdit,              HC_Wetlands);
    SetControlHelpContext(FStorageVolumeEdit,     HC_Wetlands);
    SetControlHelpContext(FUpstreamThresholdEdit, HC_Wetlands);
    SetControlHelpContext(FInflowProportionEdit,  HC_Wetlands);
    SetControlHelpContext(FInflowCbx,             HC_Wetlands);
    SetControlHelpContext(FOutflowCbx,            HC_Wetlands);
    SetControlHelpContext(FPenaltyStructEdit,     HC_Wetlands);
    SetControlHelpContext(FPriorityEdit,          HC_Wetlands);
    SetControlHelpContext(FRainCoeffEdit,         HC_Wetlands);
    SetControlHelpContext(FWetlandXCoordEdit,     HC_Wetlands);
    SetControlHelpContext(FWetlandYCoordEdit,     HC_Wetlands);
    SetControlHelpContext(FDamLevelsFileNameCbx,  HC_Wetlands);
    SetControlHelpContext(FSummaryIncludeChkBox,  HC_Wetlands);
    SetControlHelpContext(FReservoirExistsChkBox, HC_Wetlands);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
