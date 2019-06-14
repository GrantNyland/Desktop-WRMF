//
//
//  UNIT      : Contains the class TReservoirPropertiesDialog.
//  AUTHOR    : Titi Ngubane (ARIVIA)
//  DATE      : 2003/06/30
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UReservoirPropertiesDialog;

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

  TReservoirPropertiesDialog = class(TAbstractScrollablePanel)
  private
  protected
    FReservoirNameLabel      : TLabel;
    FReservoirNameEdit       : TFieldEdit;
    FReservoirNumberLabel    : TLabel;
    FReservoirNumberEdit     : TFieldEdit;
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
    FDamLevelsFileNameLabel  : TLabel;
    FDamLevelsFileNameCbx    : TFieldComboBox;
    FDamLevelsFileNameSelectBtn : TFieldBitBtn;
    FDamLevelsFileNameGridBtn   : TFieldBitBtn;
    FDamLevelsFileNameGraphBtn  : TFieldBitBtn;

    FReservoirXCoordLabel      : TLabel;
    FReservoirXCoordEdit       : TFieldEdit;
    FReservoirYCoordLabel      : TLabel;
    FReservoirYCoordEdit       : TFieldEdit;
    FReservoirAreaGroupLabel   : TLabel;
    FReservoirAreaGroupCbx     : TFieldComboBox;


    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure RestoreColourState; override;

    property ReservoirNameEdit          : TFieldEdit       read FReservoirNameEdit;
    property ReservoirNumberEdit        : TFieldEdit       read FReservoirNumberEdit;
    property PenaltyStructureEdit       : TFieldEdit       read FPenaltyStructEdit;
    property PriorityEdit               : TFieldEdit       read FPriorityEdit;
    property RainCoeffEdit              : TFieldEdit       read FRainCoeffEdit;
    property ReservoirXCoordEdit        : TFieldEdit       read FReservoirXCoordEdit;
    property ReservoirYCoordEdit        : TFieldEdit       read FReservoirYCoordEdit;
    property ReservoirAreaGroupCbx      : TFieldComboBox   read FReservoirAreaGroupCbx;
    property SummaryIncludeChkBox       : TFieldChkBox     read FSummaryIncludeChkBox;
    property ReservoirExistsChkBox      : TFieldChkBox     read FReservoirExistsChkBox;
    property SelectPenaltyStruct        : TFieldBitBtn     read FSelectPenaltyStruct;
    property DamLevelsFileNameCbx       : TFieldComboBox   read FDamLevelsFileNameCbx;
    property DamLevelsFileNameSelectBtn : TFieldBitBtn     read FDamLevelsFileNameSelectBtn;
    property DamLevelsFileNameGridBtn   : TFieldBitBtn     read FDamLevelsFileNameGridBtn;
    property DamLevelsFileNameGraphBtn  : TFieldBitBtn     read FDamLevelsFileNameGraphBtn;
  end;

  implementation
uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TReservoirPropertiesDialog }

procedure TReservoirPropertiesDialog.CreateMemberObjects;
const OPNAME = 'TReservoirPropertiesDialog.CreateMemberObjects';
begin
  inherited;
  try
    // Create the node number edit and label.
    FReservoirNumberEdit            := TFieldEdit.Create(ControlsOwner, FAppModules);
    FReservoirNumberEdit.Parent     := ControlsParent;
    FReservoirNumberEdit.Top        := C_ControlBorder;

    FReservoirNumberLabel           := TLabel.Create(ControlsOwner);
    FReservoirNumberLabel.Parent    := ControlsParent;
    FReservoirNumberLabel.Top       := FReservoirNumberEdit.Top + C_LabelOffset;
    FReservoirNumberLabel.Left      := C_LabelOffset;

    // Create the reservoir name edit and label.
    FReservoirNameEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FReservoirNameEdit.Parent  := ControlsParent;
    FReservoirNameEdit.Top     := FReservoirNumberEdit.Top + FReservoirNumberEdit.Height + C_ControlBorder;

    FReservoirNameLabel        := TLabel.Create(ControlsOwner);
    FReservoirNameLabel.Parent := ControlsParent;
    FReservoirNameLabel.Top    := FReservoirNameEdit.Top + C_LabelOffset;
    FReservoirNameLabel.Left   := C_LabelOffset;

    // Create the Penalty Structure Edit box and label and button.
    FPenaltyStructEdit            := TFieldEdit.Create(ControlsOwner, FAppModules);
    FPenaltyStructEdit.Parent     := ControlsParent;
    FPenaltyStructEdit.Top        := FReservoirNameEdit.Top + FReservoirNameEdit.Height + C_ControlBorder;

    FPenaltyStructLabel        := TLabel.Create(ControlsOwner);
    FPenaltyStructLabel.Parent := ControlsParent;
    FPenaltyStructLabel.Top    := FPenaltyStructEdit.Top + C_LabelOffset;
    FPenaltyStructLabel.Left   := C_LabelOffset;

    FSelectPenaltyStruct         := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FSelectPenaltyStruct.Parent  := ControlsParent;
    FSelectPenaltyStruct.Top     := FPenaltyStructEdit.Top ;
    FSelectPenaltyStruct.Height  := FPenaltyStructEdit.Height + C_ControlBorder;
    FSelectPenaltyStruct.Width   := 38;
    FSelectPenaltyStruct.Glyph.LoadFromResourceName(HImagesInstance, 'IMPORTFILES');
    FSelectPenaltyStruct.NumGlyphs := 2;

    // Create the Reservoir Priority Edit box and label.
    FPriorityEdit            := TFieldEdit.Create(ControlsOwner, FAppModules);
    FPriorityEdit.Parent     := ControlsParent;
    FPriorityEdit.Top        := FPenaltyStructEdit.Top + FPenaltyStructEdit.Height + C_ControlBorder;

    FPriorityLabel        := TLabel.Create(ControlsOwner);
    FPriorityLabel.Parent := ControlsParent;
    FPriorityLabel.Top    := FPriorityEdit.Top + C_LabelOffset;
    FPriorityLabel.Left   := C_LabelOffset;

    //Create the Rainfall/Runoff Coefficient label and editbox
    FRainCoeffEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FRainCoeffEdit.Parent  := ControlsParent;
    FRainCoeffEdit.Top     := FPriorityEdit.Top + FPriorityEdit.Height + C_ControlBorder;

    FRainCoeffLabel        := TLabel.Create(ControlsOwner);
    FRainCoeffLabel.Parent := ControlsParent;
    FRainCoeffLabel.Top    := FRainCoeffEdit.Top + C_LabelOffset;
    FRainCoeffLabel.Left   := C_LabelOffset;

    //Create the XCoord label and editbox
    FReservoirXCoordEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FReservoirXCoordEdit.Parent  := ControlsParent;
    FReservoirXCoordEdit.Top     := FRainCoeffEdit.Top + FRainCoeffEdit.Height + C_ControlBorder;

    FReservoirXCoordLabel        := TLabel.Create(ControlsOwner);
    FReservoirXCoordLabel.Parent := ControlsParent;
    FReservoirXCoordLabel.Top    := FReservoirXCoordEdit.Top + C_LabelOffset;
    FReservoirXCoordLabel.Left   := C_LabelOffset;

    //Create the YCoord label and editbox
    FReservoirYCoordEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FReservoirYCoordEdit.Parent  := ControlsParent;
    FReservoirYCoordEdit.Top     := FReservoirXCoordEdit.Top + FReservoirXCoordEdit.Height + C_ControlBorder;

    FReservoirYCoordLabel        := TLabel.Create(ControlsOwner);
    FReservoirYCoordLabel.Parent := ControlsParent;
    FReservoirYCoordLabel.Top    := FReservoirYCoordEdit.Top + C_LabelOffset;
    FReservoirYCoordLabel.Left   := C_LabelOffset;

    //Create the Reservoir Area Group label and ComboBox
    FReservoirAreaGroupCbx         := TFieldComboBox.Create(ControlsOwner, FAppModules);
    FReservoirAreaGroupCbx.Parent  := ControlsParent;
    FReservoirAreaGroupCbx.Top     := FReservoirYCoordEdit.Top + FReservoirYCoordEdit.Height + C_ControlBorder;

    FReservoirAreaGroupLabel     := TLabel.Create(ControlsOwner);
    FReservoirAreaGroupLabel.Parent := ControlsParent;
    FReservoirAreaGroupLabel.Top    := FReservoirAreaGroupCbx.Top + C_LabelOffset;
    FReservoirAreaGroupLabel.Left   := C_LabelOffset;


    // create the historic Water level comboBox  and label
    FDamLevelsFileNameCbx               := TFieldComboBox.Create(ControlsOwner, FAppModules);
    FDamLevelsFileNameCbx.Parent        := ControlsParent;
    FDamLevelsFileNameCbx.Top           := FReservoirAreaGroupCbx.Top + FReservoirAreaGroupCbx.Height + C_ControlBorder ;
    FDamLevelsFileNameCbx.Style         := csDropDownList;

    FDamLevelsFileNameLabel             := TLabel.Create(ControlsOwner);
    FDamLevelsFileNameLabel.Parent      := ControlsParent;
    FDamLevelsFileNameLabel.Top         := FDamLevelsFileNameCbx.Top + C_LabelOffset;
    FDamLevelsFileNameLabel.Left        := C_LabelOffset;

    // Create the Include summary check box.
    FSummaryIncludeChkBox             := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FSummaryIncludeChkBox.Parent      := ControlsParent;
    FSummaryIncludeChkBox.Top         := FDamLevelsFileNameCbx.Top  + FDamLevelsFileNameCbx.Height + C_ControlBorder;
    FSummaryIncludeChkBox.Alignment   := taRightJustify;

    FSummaryIncludeLabel        := TLabel.Create(ControlsOwner);
    FSummaryIncludeLabel.Parent := ControlsParent;
    FSummaryIncludeLabel.Top    := FSummaryIncludeChkBox.Top + C_LabelOffset ;
    FSummaryIncludeLabel.Left   := C_LabelOffset;

    // Create the Reservoir Exists check box.
    FReservoirExistsChkBox             := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FReservoirExistsChkBox.Parent      := ControlsParent;
    FReservoirExistsChkBox.Top         := FSummaryIncludeChkBox.Top + FSummaryIncludeChkBox.Height + C_ControlBorder;
    FReservoirExistsChkBox.Alignment   := taRightJustify;

    FReservoirExistsLabel        := TLabel.Create(ControlsOwner);
    FReservoirExistsLabel.Parent := ControlsParent;
    FReservoirExistsLabel.Top    := FReservoirExistsChkBox.Top + C_LabelOffset;
    FReservoirExistsLabel.Left   := C_LabelOffset;

    FDamLevelsFileNameSelectBtn            := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FDamLevelsFileNameSelectBtn.Parent     := ControlsParent;
    FDamLevelsFileNameSelectBtn.Top        := FDamLevelsFileNameCbx.Top;
    FDamLevelsFileNameSelectBtn.Height     := FDamLevelsFileNameCbx.Height - 4;
    FDamLevelsFileNameSelectBtn.Width      := 30;
    FDamLevelsFileNameSelectBtn.Caption    := FAppModules.language.GetString('ButtonCaption.SelectDemandFile');
    FDamLevelsFileNameSelectBtn.Font.Name  := 'Arial';
    FDamLevelsFileNameSelectBtn.Font.Style := [fsBold];

    FDamLevelsFileNameGridBtn              := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FDamLevelsFileNameGridBtn.Parent       := ControlsParent;
    FDamLevelsFileNameGridBtn.Top          := FDamLevelsFileNameCbx.Top;
    FDamLevelsFileNameGridBtn.Height       := FDamLevelsFileNameCbx.Height - 4;
    FDamLevelsFileNameGridBtn.Width        := 30;
    FDamLevelsFileNameGridBtn.Glyph.LoadFromResourceName(HImagesInstance, 'VIEWDATAGRID');
    FDamLevelsFileNameGridBtn.NumGlyphs := 2;

    FDamLevelsFileNameGraphBtn              := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FDamLevelsFileNameGraphBtn.Parent       := ControlsParent;
    FDamLevelsFileNameGraphBtn.Top          := FDamLevelsFileNameCbx.Top;
    FDamLevelsFileNameGraphBtn.Height       := FDamLevelsFileNameCbx.Height - 4;
    FDamLevelsFileNameGraphBtn.Width        := 30;
    FDamLevelsFileNameGraphBtn.Glyph.LoadFromResourceName(HImagesInstance, 'VIEWDATAGRAPH');
    FDamLevelsFileNameGraphBtn.NumGlyphs := 2;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirPropertiesDialog.Initialise: boolean;
const OPNAME = 'TReservoirPropertiesDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    Result := True;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDialog.Resize;
const OPNAME = 'TReservoirPropertiesDialog.Resize';
begin
  // Call the ancestor.
  inherited Resize;
  try
    // Set out the controls.
    FReservoirNumberEdit.Width         := Self.ClientWidth div 2 - C_ControlBorder;
    FReservoirNumberEdit.Left          := Self.ClientWidth div 2;

    FReservoirNameEdit.Left            := Self.ClientWidth div 2;
    FReservoirNameEdit.Width           := Self.ClientWidth div 2 - C_ControlBorder;

    FSummaryIncludeChkBox.Left         := Self.ClientWidth div 2;
    FSummaryIncludeChkBox.Width        := Self.ClientWidth div 2 - C_ControlBorder;

    FReservoirExistsChkBox.Left        := Self.ClientWidth div 2;
    FReservoirExistsChkBox.Width       := Self.ClientWidth div 2 - C_ControlBorder;

    //FNewPenaltyStruct.Left           := Self.ClientWidth - FNewPenaltyStruct.Width - C_ControlBorder;
    //FSelectPenaltyStruct.Left        := FNewPenaltyStruct.Left - FSelectPenaltyStruct.Width;
    FSelectPenaltyStruct.Left          := Self.ClientWidth - FSelectPenaltyStruct.Width - C_ControlBorder;

    FPenaltyStructEdit.Left            := Self.ClientWidth div 2;
    FPenaltyStructEdit.Width           := FSelectPenaltyStruct.Left - ( Self.ClientWidth div 2) - 2;

    FPriorityEdit.Left                 := Self.ClientWidth div 2;
    FPriorityEdit.Width                := FPenaltyStructEdit.Width;

    FRainCoeffEdit.Left                := Self.ClientWidth div 2;
    FRainCoeffEdit.Width               := FPenaltyStructEdit.Width;

    ReservoirXCoordEdit.Left           := Self.ClientWidth div 2;
    ReservoirXCoordEdit.Width          := FPenaltyStructEdit.Width;

    ReservoirYCoordEdit.Left           := Self.ClientWidth div 2;
    ReservoirYCoordEdit.Width          := FPenaltyStructEdit.Width;

    FReservoirAreaGroupCbx.Width       := ReservoirXCoordEdit.Width;
    FReservoirAreaGroupCbx.Left        := Self.ClientWidth div 2; 

    FDamLevelsFileNameCbx.Left         := Self.ClientWidth div 2;
    FDamLevelsFileNameCbx.Width        := Self.ClientWidth div 2 - (C_ControlBorder + 120);

    FDamLevelsFileNameSelectBtn.Left   := Self.ClientWidth - FSelectPenaltyStruct.Width - (C_ControlBorder + 80) ;

    FDamLevelsFileNameGridBtn.Left     := Self.ClientWidth - FSelectPenaltyStruct.Width - (C_ControlBorder + 45) ;

    FDamLevelsFileNameGraphBtn.Left    := Self.ClientWidth - FSelectPenaltyStruct.Width - (C_ControlBorder + 10) ;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirPropertiesDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FReservoirNumberLabel.Caption     := FAppModules.Language.GetString('TField.NodeCount');
    FReservoirNameLabel.Caption       := FAppModules.Language.GetString('TField.ReservoirName');
    FPenaltyStructLabel.Caption       := FAppModules.Language.GetString('TField.PenaltyStruct');
    FSummaryIncludeLabel.Caption      := FAppModules.Language.GetString('TField.IncludeSummary');
    FReservoirExistsLabel.Caption     := FAppModules.Language.GetString('TField.StatusIndicator');
    FSelectPenaltyStruct.Hint         := FAppModules.Language.GetString('ButtonHint.Select');
    FPriorityLabel.Caption            := FAppModules.Language.GetString('TField.ReservoirPriority');
    FRainCoeffLabel.Caption           := FAppModules.Language.GetString('TField.RainCoef');
    FDamLevelsFileNameLabel.Caption   := FAppModules.Language.GetString('TField.DamLevelsFileName');
    FReservoirXCoordLabel.Caption     := FAppModules.Language.GetString('TField.XCoord');
    FReservoirYCoordLabel.Caption     := FAppModules.Language.GetString('TField.YCoord');
    FReservoirAreaGroupLabel.Caption  := FAppModules.Language.GetString('TField.ReservoirAreaGroupName');
    FDamLevelsFileNameSelectBtn.Caption    := FAppModules.language.GetString('ButtonCaption.SelectDemandFile');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDialog.RestoreColourState;
const OPNAME = 'TReservoirPropertiesDialog.RestoreColourState';
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

procedure TReservoirPropertiesDialog.AssignHelpContext;
const OPNAME = 'TReservoirPropertiesDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                   HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FSelectPenaltyStruct,   HC_ReservoirPenaltyStructures);
    SetControlHelpContext(FRainCoeffEdit,         HC_DefiningCatchmentHydrologyInflows);
    SetControlHelpContext(FSummaryIncludeChkBox,  HC_ReservoirOutputOptions);

    SetControlHelpContext(FReservoirNameEdit,     HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FReservoirNumberEdit,   HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FPenaltyStructEdit,     HC_ReservoirPenaltyStructures);
    SetControlHelpContext(FPriorityEdit,          HC_ReservoirPenaltyStructures);
    SetControlHelpContext(FReservoirExistsChkBox, HC_PhysicalReservoirCharacteristics);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

