{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockPropertiesDialog.                    *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockPropertiesDialog;

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

  TIrrBlockPropertiesDialog = class(TAbstractScrollablePanel)
  protected
    FNumberLabel                 : TLabel;
    FNumberEdit                  : TFieldEdit;
    FTypeLabel                   : TLabel;
    FTypeEdit                    : TFieldEdit;
    FNameLabel                   : TLabel;
    FNameEdit                    : TFieldEdit;
    FDescriptionLabel            : TLabel;
    FDescriptionEdit             : TFieldEdit;
    FUpStreamNodeLabel           : TLabel;
    FUpStreamNodeCbx             : TFieldComboBox;
    FDownStreamNodeLabel         : TLabel;
    FDownStreamNodeCbx           : TFieldComboBox;
    FSpecifiedDemandLabel        : TLabel;
    FFileNameCbx                 : TFieldComboBox;
    FFileNameSelectBtn           : TFieldButton;
    FFileNameGridBtn             : TFieldBitBtn;
    FFileNameGraphBtn            : TFieldBitBtn;
    FNodeNumberLabel             : TLabel;
    FNodeNumberCbx               : TFieldComboBox;

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
    property TypeEdit                     : TFieldEdit read FTypeEdit;
    property NameEdit                     : TFieldEdit read FNameEdit;
    property DescriptionEdit              : TFieldEdit read FDescriptionEdit;
    property UpStreamNodeCbx              : TFieldComboBox   read FUpStreamNodeCbx;
    property DownStreamNodeCbx            : TFieldComboBox   read FDownStreamNodeCbx;
    property FileNameCbx                  : TFieldComboBox   read FFileNameCbx;
    property FileNameSelectBtn            : TFieldButton     read FFileNameSelectBtn;
    property FileNameGridBtn              : TFieldBitBtn     read FFileNameGridBtn;
    property FileNameGraphBtn             : TFieldBitBtn     read FFileNameGraphBtn;
    property NodeNumberCbx                : TFieldComboBox read FNodeNumberCbx;
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
{* TIrrBlockPropertiesDialog                                                     *}
{******************************************************************************}

procedure TIrrBlockPropertiesDialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockPropertiesDialog.CreateMemberObjects';
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
    FNumberEdit                         := CreateFieldEdit(FAppModules, lOwner, lParent, 300,  5,    100,  20, 8, TRUE);

    FTypeLabel                          := CreateFieldLabel(lOwner, lParent, 10,  30,   350, 21);
    FTypeEdit                           := CreateFieldEdit(FAppModules, lOwner, lParent, 300,  30,   100,  20, 8, TRUE);

    FNameLabel                          := CreateFieldLabel(lOwner, lParent, 10,  55,   350, 21);
    FNameEdit                           := CreateFieldEdit(FAppModules, lOwner, lParent, 300,  55,   300,  20, 8, TRUE);

    FDescriptionLabel                   := CreateFieldLabel(lOwner, lParent, 10,  80,   350, 21);
    FDescriptionEdit                    := CreateFieldEdit(FAppModules, lOwner, lParent, 300,  80,   300,  20, 8, TRUE);

    FUpStreamNodeLabel                  := CreateFieldLabel(lOwner, lParent, 10,  105,  350, 21);
    FUpStreamNodeCbx                    := CreateFieldComboBox(FAppModules, lOwner, lParent, 300, 105, 200,  21, 1, TRUE, csDropDownList);;

    FDownStreamNodeLabel                := CreateFieldLabel(lOwner, lParent, 10,  130,   340, 21);
    FDownStreamNodeCbx                  := CreateFieldComboBox(FAppModules, lOwner, lParent, 300,  130, 200,  21, 1, TRUE, csDropDownList);

    FSpecifiedDemandLabel               := CreateFieldLabel(lOwner, lParent, 10,  155,   340, 21);
    FFileNameCbx                        := CreateFieldComboBox(FAppModules, lOwner, lParent, 300, 155, 430, 21,11, TRUE, csDropDownList);
    FFileNameSelectBtn                  := CreateFieldButton(FAppModules, lOwner, lParent, 735, 155,  25, 21, 8, TRUE, '...');
    FFileNameGridBtn                    := CreateFieldBitButton(FAppModules, lOwner, lParent, 765, 155,  25, 21, 8, TRUE, 'VIEWDATAGRID');
    FFileNameGraphBtn                   := CreateFieldBitButton(FAppModules, lOwner, lParent, 795, 155,  25, 21, 8, TRUE, 'VIEWDATAGRAPH');

    FNodeNumberLabel                    := CreateFieldLabel(lOwner, lParent, 10,  180,  340, 21);
    FNodeNumberCbx                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 300,  180, 200,  20, 8, TRUE, csDropDownList);
    FNodeNumberCbx.Sorted               := True;

    FIrrigationXCoordLabel              := CreateFieldLabel(lOwner, lParent, 10,  205,  340, 21);
    FIrrigationXCoordEdit               := CreateFieldEdit(FAppModules, lOwner, lParent, 300,  205,   100,  20, 8, TRUE);
    FIrrigationYCoordLabel              := CreateFieldLabel(lOwner, lParent, 10,  230,  340, 21);
    FIrrigationYCoordEdit               := CreateFieldEdit(FAppModules, lOwner, lParent, 300,  230,   100,  20, 8, TRUE);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockPropertiesDialog.Resize;
const OPNAME = 'TIrrBlockPropertiesDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockPropertiesDialog.Initialise: boolean;
const OPNAME = 'TIrrBlockPropertiesDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    with FFileNameSelectBtn do
    begin
      Font.Name  := 'Arial';
      Font.Size  := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockPropertiesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockPropertiesDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNumberLabel.Caption                         := FAppModules.Language.GetString('TField.IrrigationBlockBlockNumber')+':';
    FTypeLabel.Caption                           := FAppModules.Language.GetString('TField.IrrigationBlockType')+':';
    FNameLabel.Caption                           := FAppModules.Language.GetString('TField.IrrigationBlockName')+':';
    FDescriptionLabel.Caption                    := FAppModules.Language.GetString('TField.IrrigationBlockDescription')+':';
    FUpStreamNodeLabel.Caption                   := FAppModules.Language.GetString('TField.IrrigationBlockUpStreamNode')+':';
    FDownStreamNodeLabel.Caption                 := FAppModules.Language.GetString('TField.IrrigationBlockDownStreamNode')+':';
    FSpecifiedDemandLabel.Caption                := FAppModules.Language.GetString('TField.IrrigationBlockDemandFileName')+':';
    FNodeNumberLabel.Caption                     := FAppModules.Language.GetString('TField.IrrigationBlockNodeNumber')+':';
    FIrrigationXCoordLabel.Caption               := FAppModules.Language.GetString('TField.XCoord')+':';
    FIrrigationYCoordLabel.Caption               := FAppModules.Language.GetString('TField.YCoord')+':';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockPropertiesDialog.RestoreColourState;
const OPNAME = 'TIrrBlockPropertiesDialog.RestoreColourState';
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

procedure TIrrBlockPropertiesDialog.AssignHelpContext;
const OPNAME = 'TIrrBlockPropertiesDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                         HC_Irrigation);
    SetControlHelpContext(FNumberEdit,                  HC_Irrigation);
    SetControlHelpContext(FTypeEdit,                    HC_Irrigation);
    SetControlHelpContext(FNameEdit,                    HC_Irrigation);
    SetControlHelpContext(FDescriptionEdit,             HC_Irrigation);
    SetControlHelpContext(FUpStreamNodeCbx,             HC_Irrigation);
    SetControlHelpContext(FDownStreamNodeCbx,           HC_Irrigation);
    SetControlHelpContext(FFileNameCbx,                 HC_Irrigation);
    SetControlHelpContext(FNodeNumberCbx,               HC_Irrigation);
    SetControlHelpContext(FIrrigationXCoordEdit,        HC_Irrigation);
    SetControlHelpContext(FIrrigationYCoordEdit,        HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
