{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationBlockDialog.                    *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrigationBlockDialog;

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

  TIrrigationBlockDialog = class(TAbstractScrollablePanel)
  protected
    FNumberLabel                 : TLabel;
    FNumberEdit                  : TFieldEdit;
    FNameLabel                   : TLabel;
    FNameEdit                    : TFieldEdit;
    FDescriptionLabel            : TLabel;
    FDescriptionEdit             : TFieldEdit;
    FUpStreamNodeLabel           : TLabel;
    FUpStreamNodeCbx             : TFieldComboBox;
    FDownStreamNodeLabel         : TLabel;
    FDownStreamNodeCbx           : TFieldComboBox;
    FMaxWaterAllocationLabel     : TLabel;
    FMaxWaterAllocationEdit      : TFieldEdit;
    FSpecifiedDemandLabel        : TLabel;
    FFileNameCbx                 : TFieldComboBox;
    FFileNameSelectBtn           : TFieldButton;
    FFileNameGridBtn             : TFieldBitBtn;
    FFileNameGraphBtn            : TFieldBitBtn;
    FNodeNumberLabel             : TLabel;
    FNodeNumberCbx               : TFieldComboBox;
    FCanalTransportLossLabel     : TLabel;
    FCanalTransportLossEdit      : TFieldEdit;
    FEfficiencyFactorLabel       : TLabel;
    FEfficiencyFactorEdit        : TFieldEdit;
    FAllocatedIrrigationAreaLabel: TLabel;
    FAllocatedIrrigationAreaEdit : TFieldEdit;
    FDroughtApplicableChkBox     : TFieldChkBox;

    FIrrigationXCoordLabel      : TLabel;
    FIrrigationXCoordEdit       : TFieldEdit;
    FIrrigationYCoordLabel      : TLabel;
    FIrrigationYCoordEdit       : TFieldEdit;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    property NumberEdit                   : TFieldEdit read FNumberEdit;
    property NameEdit                     : TFieldEdit read FNameEdit;
    property DescriptionEdit              : TFieldEdit read FDescriptionEdit;
    property UpStreamNodeCbx              : TFieldComboBox   read FUpStreamNodeCbx;
    property DownStreamNodeCbx            : TFieldComboBox   read FDownStreamNodeCbx;
    property MaxWaterAllocationEdit       : TFieldEdit       read FMaxWaterAllocationEdit;
    property AllocatedIrrigationAreaEdit  : TFieldEdit       read FAllocatedIrrigationAreaEdit;
    property FileNameCbx                  : TFieldComboBox   read FFileNameCbx;
    property FileNameSelectBtn            : TFieldButton     read FFileNameSelectBtn;
    property FileNameGridBtn              : TFieldBitBtn     read FFileNameGridBtn;
    property FileNameGraphBtn             : TFieldBitBtn     read FFileNameGraphBtn;
    property CanalTransportLossEdit       : TFieldEdit read FCanalTransportLossEdit;
    property EfficiencyFactorEdit         : TFieldEdit read FEfficiencyFactorEdit;
    property NodeNumberCbx                : TFieldComboBox read FNodeNumberCbx;
    property DroughtApplicableChkBox      : TFieldChkBox read FDroughtApplicableChkBox;
    property IrrigationXCoordEdit         : TFieldEdit read FIrrigationXCoordEdit;
    property IrrigationYCoordEdit         : TFieldEdit read FIrrigationYCoordEdit;
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
{* TIrrigationBlockDialog                                                     *}
{******************************************************************************}

procedure TIrrigationBlockDialog.CreateMemberObjects;
const OPNAME = 'TIrrigationBlockDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
//                                                                                Left  Top Width Height
    FNumberLabel                        := CreateFieldLabel(lOwner, lParent, 10,  5,    350, 21);
    FNumberEdit                         := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  5,    100,  20, 8, TRUE);

    FNameLabel                          := CreateFieldLabel(lOwner, lParent, 10,  30,   350, 21);
    FNameEdit                           := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  30,   300,  20, 8, TRUE);

    FDescriptionLabel                   := CreateFieldLabel(lOwner, lParent, 10,  55,   350, 21);
    FDescriptionEdit                    := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  55,   300,  20, 8, TRUE);

    FUpStreamNodeLabel                  := CreateFieldLabel(lOwner, lParent, 10,  80,   350, 21);
    FUpStreamNodeCbx                    := CreateFieldComboBox(FAppModules, lOwner, lParent, 370,  80, 200,  21, 1, TRUE, csDropDownList);;

    FDownStreamNodeLabel                := CreateFieldLabel(lOwner, lParent, 10,  105,   340, 21);
    FDownStreamNodeCbx                  := CreateFieldComboBox(FAppModules, lOwner, lParent, 370,  105, 200,  21, 1, TRUE, csDropDownList);
    //5
    FAllocatedIrrigationAreaLabel       := CreateFieldLabel(lOwner, lParent, 10,  130,  340, 21);
    FAllocatedIrrigationAreaEdit        := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  130,  100,  20, 8, TRUE);

    FEfficiencyFactorLabel              := CreateFieldLabel(lOwner, lParent, 10,  155,  340, 21);
    FEfficiencyFactorEdit               := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  155,  100,  20, 8, TRUE);

    FMaxWaterAllocationLabel            := CreateFieldLabel(lOwner, lParent, 10,  180,   340, 21);
    FMaxWaterAllocationEdit             := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  180,   100,  20, 8, TRUE);

    FSpecifiedDemandLabel               := CreateFieldLabel(lOwner, lParent, 10,  205,   340, 21);
    FFileNameCbx                        := CreateFieldComboBox(FAppModules, lOwner, lParent, 370, 205, 260, 21,11, TRUE, csDropDownList);
    FFileNameSelectBtn                  := CreateFieldButton(FAppModules, lOwner, lParent, 635, 205,  25, 21, 8, TRUE, '...');
    with FFileNameSelectBtn do
    begin
      Font.Name  := 'Arial';
      Font.Size  := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FFileNameGridBtn                    := CreateFieldBitButton(FAppModules, lOwner, lParent, 665, 205,  25, 21, 8, TRUE, 'VIEWDATAGRID');
    FFileNameGraphBtn                   := CreateFieldBitButton(FAppModules, lOwner, lParent, 695, 205,  25, 21, 8, TRUE, 'VIEWDATAGRAPH');

    FCanalTransportLossLabel            := CreateFieldLabel(lOwner, lParent, 10,  230,  340, 21);
    FCanalTransportLossEdit             := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  230,  100,  20, 8, TRUE);

    FNodeNumberLabel                    := CreateFieldLabel(lOwner, lParent, 10,  255,  340, 21);
    FNodeNumberCbx                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 370,  255, 200,  20, 8, TRUE, csDropDownList);
    FNodeNumberCbx.Sorted               := True;

    FIrrigationXCoordLabel              := CreateFieldLabel(lOwner, lParent, 10,  280,  340, 21);
    FIrrigationXCoordEdit               := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  280,   100,  20, 8, TRUE);
    FIrrigationYCoordLabel              := CreateFieldLabel(lOwner, lParent, 10,  305,  340, 21);
    FIrrigationYCoordEdit               := CreateFieldEdit(FAppModules, lOwner, lParent, 370,  305,   100,  20, 8, TRUE);

    FDroughtApplicableChkBox            := CreateFieldChkBox(FAppModules, lOwner, lParent,   10,  330, 373, 21, 9,  TRUE, taLeftJustify);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockDialog.Resize;
const OPNAME = 'TIrrigationBlockDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockDialog.Initialise: boolean;
const OPNAME = 'TIrrigationBlockDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrigationBlockDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNumberLabel.Caption                         := FAppModules.Language.GetString('TField.IrrigationBlockBlockNumber');
    FNameLabel.Caption                           := FAppModules.Language.GetString('TField.IrrigationBlockName');
    FDescriptionLabel.Caption                    := FAppModules.Language.GetString('TField.IrrigationBlockDescription');
    FUpStreamNodeLabel.Caption                   := FAppModules.Language.GetString('TField.IrrigationBlockUpStreamNode');
    FDownStreamNodeLabel.Caption                 := FAppModules.Language.GetString('TField.IrrigationBlockDownStreamNode');
    FMaxWaterAllocationLabel.Caption             := FAppModules.Language.GetString('TField.IrrigationBlockMaxWaterAllocation');
    FAllocatedIrrigationAreaLabel.Caption        := FAppModules.Language.GetString('TField.IrrigationBlockAllocatedIrrigationArea');
    FSpecifiedDemandLabel.Caption                := FAppModules.Language.GetString('TField.IrrigationBlockDemandFileName');
    FCanalTransportLossLabel.Caption             := FAppModules.Language.GetString('TField.IrrigationBlockCanalTransportLoss');
    FEfficiencyFactorLabel.Caption               := FAppModules.Language.GetString('TField.IrrigationBlockEfficiencyFactor');
    FNodeNumberLabel.Caption                     := FAppModules.Language.GetString('TField.IrrigationBlockNodeNumber');
    FDroughtApplicableChkBox.Caption             := FAppModules.Language.GetString('TField.IrrigationBlockDroughtApplicable');
    FIrrigationXCoordLabel.Caption               := FAppModules.Language.GetString('TField.XCoord');
    FIrrigationYCoordLabel.Caption               := FAppModules.Language.GetString('TField.YCoord');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockDialog.RestoreColourState;
const OPNAME = 'TIrrigationBlockDialog.RestoreColourState';
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

procedure TIrrigationBlockDialog.AssignHelpContext;
const OPNAME = 'TIrrigationBlockDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                         HC_Irrigation);
    SetControlHelpContext(FNumberEdit,                  HC_Irrigation);
    SetControlHelpContext(FNameEdit,                    HC_Irrigation);
    SetControlHelpContext(FDescriptionEdit,             HC_Irrigation);
    SetControlHelpContext(FUpStreamNodeCbx,             HC_Irrigation);
    SetControlHelpContext(FDownStreamNodeCbx,           HC_Irrigation);
    SetControlHelpContext(FFileNameCbx,                 HC_Irrigation);
    SetControlHelpContext(FNodeNumberCbx,               HC_Irrigation);
    SetControlHelpContext(FCanalTransportLossEdit,      HC_Irrigation);
    SetControlHelpContext(FEfficiencyFactorEdit,        HC_Irrigation);
    SetControlHelpContext(FAllocatedIrrigationAreaEdit, HC_Irrigation);
    SetControlHelpContext(FDroughtApplicableChkBox,     HC_Irrigation);
    SetControlHelpContext(FIrrigationXCoordEdit,        HC_Irrigation);
    SetControlHelpContext(FIrrigationYCoordEdit,        HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
