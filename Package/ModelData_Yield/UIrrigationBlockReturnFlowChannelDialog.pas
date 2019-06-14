{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationBlockReturnFlowChannelDialog.pas*}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/07/31                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrigationBlockReturnFlowChannelDialog;

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

  TIrrigationBlockReturnFlowChannelDialog = class(TAbstractScrollablePanel)
  private
  protected
    FChannelNameLabel             : TLabel;
    FUpstreamNodeLabel            : TLabel;
    FDownstreamNodeLabel          : TLabel;
    FPenaltyStructureLabel        : TLabel;
    FChannelAreaLabel             : TLabel;
    FChannelNameEdit              : TFieldEdit;
    FChannelNumberEdit            : TFieldEdit;
    FUpStreamNodeCbx              : TFieldComboBox;
    FDownStreamNodeCbx            : TFieldComboBox;
    FChannelAreaCbx               : TFieldComboBox;
    FUpStreamNodeEdit             : TFieldEdit;
    FDownStreamNodeEdit           : TFieldEdit;
    FPenaltyStructureEdit         : TFieldEdit;
    FSelectPenaltyStructureBtn    : TFieldButton;
    FSummaryOutputChkBox          : TFieldChkBox;
    FFirmYieldAnalysisChkBox      : TFieldChkBox;
    FSelectChannelBtn             : TFieldButton;

    FIrrigationBlockReturnFlowLossLabel   : TLabel;
    FIrrigationBlockReturnFlowLossEdit    : TFieldEdit;
    FIrrigationBlockReturnFlowFactorLabel : TLabel;
    FIrrigationBlockReturnFlowFactorEdit  : TFieldEdit;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property ChannelNameEdit           : TFieldEdit     read FChannelNameEdit;
    property ChannelNumberEdit         : TFieldEdit     read FChannelNumberEdit;
    property UpStreamNodeCbx           : TFieldComboBox read FUpStreamNodeCbx;
    property DownStreamNodeCbx         : TFieldComboBox read FDownStreamNodeCbx;
    property ChannelAreaCbx            : TFieldComboBox read FChannelAreaCbx;
    property UpStreamNodeEdit          : TFieldEdit     read FUpStreamNodeEdit;
    property DownStreamNodeEdit        : TFieldEdit     read FDownStreamNodeEdit;
    property PenaltyStructureEdit      : TFieldEdit     read FPenaltyStructureEdit;
    property SelectPenaltyStructureBtn : TFieldButton   read FSelectPenaltyStructureBtn;
    property SummaryOutputChkBox       : TFieldChkBox   read FSummaryOutputChkBox;
    property FirmYieldAnalysisChkBox   : TFieldChkBox   read FFirmYieldAnalysisChkBox;
    property SelectChannelBtn          : TFieldButton   read FSelectChannelBtn;
    property ReturnFlowLossEdit        : TFieldEdit     read FIrrigationBlockReturnFlowLossEdit;
    property ReturnFlowFactorEdit      : TFieldEdit     read FIrrigationBlockReturnFlowFactorEdit;
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
{* TIrrigationBlockReturnFlowChannelDialog                                                  *}
{******************************************************************************}

procedure TIrrigationBlockReturnFlowChannelDialog.CreateMemberObjects;
const OPNAME = 'TIrrigationBlockReturnFlowChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                Left  Top Width Height
    FChannelNameLabel                      := CreateFieldLabel   (lOwner, lParent,  10,  10, 140, 21);
    FUpStreamNodeLabel                     := CreateFieldLabel   (lOwner, lParent,  10,  35, 140, 21);
    FDownstreamNodeLabel                   := CreateFieldLabel   (lOwner, lParent,  10,  60, 140, 21);
    FChannelAreaLabel                      := CreateFieldLabel   (lOwner, lParent,  10,  85, 140, 21);
    FPenaltyStructureLabel                 := CreateFieldLabel   (lOwner, lParent,  10,  110, 140, 21);
    FIrrigationBlockReturnFlowLossLabel    := CreateFieldLabel   (lOwner, lParent,  10,  160, 420, 21);
    FIrrigationBlockReturnFlowFactorLabel  := CreateFieldLabel   (lOwner, lParent,  10,  185, 420, 21);
                                           
    FChannelNumberEdit                     := CreateFieldEdit    (FAppModules, lOwner, lParent, 300,  10,  35, 21, 0,   FALSE);
    FChannelNameEdit                       := CreateFieldEdit    (FAppModules, lOwner, lParent, 340,  10, 270, 21, 1,   TRUE);
    FSelectChannelBtn                      := CreateFieldButton  (FAppModules, lOwner, lParent, 595,  10,  25, 21, 2,   TRUE, '...');
    FUpStreamNodeCbx                       := CreateFieldComboBox(FAppModules, lOwner, lParent, 300,  35, 280, 21, 3,   TRUE, csDropDownList);
    FUpStreamNodeEdit                      := CreateFieldEdit    (FAppModules, lOwner, lParent, 300,  35, 280, 21, 4,   FALSE);
    FDownStreamNodeCbx                     := CreateFieldComboBox(FAppModules, lOwner, lParent, 300,  60, 280, 21, 5,   TRUE, csDropDownList);
    FDownStreamNodeEdit                    := CreateFieldEdit    (FAppModules, lOwner, lParent, 300,  60, 280, 21, 6,   FALSE);
    FChannelAreaCbx                        := CreateFieldComboBox(FAppModules, lOwner, lParent, 300,  85, 280, 21, 7,   TRUE, csDropDownList);
    FPenaltyStructureEdit                  := CreateFieldEdit    (FAppModules, lOwner, lParent, 300,  110,  50, 21, 7,  TRUE);
    FSelectPenaltyStructureBtn             := CreateFieldButton  (FAppModules, lOwner, lParent, 355,  110,  25, 21, 8,  TRUE, '...');
    FSummaryOutputChkBox                   := CreateFieldChkBox  (FAppModules, lOwner, lParent,   8,  135, 305, 21, 9,  TRUE, taLeftJustify);
    FIrrigationBlockReturnFlowLossEdit     := CreateFieldEdit    (FAppModules, lOwner, lParent, 300,  160, 100, 21, 6,   TRUE);
    FIrrigationBlockReturnFlowFactorEdit   := CreateFieldEdit    (FAppModules, lOwner, lParent, 300,  185, 100, 21, 6,   TRUE);
    FFirmYieldAnalysisChkBox               := CreateFieldChkBox  (FAppModules, lOwner, lParent,   8,  210, 305, 21, 10, TRUE, taLeftJustify);
    with FUpStreamNodeEdit do
    begin
      Color    := clBtnFace;
      ReadOnly := True;
    end;
    with FDownStreamNodeEdit do
    begin
      Color    := clBtnFace;
      ReadOnly := True;
    end;
    with FSelectPenaltyStructureBtn do
    begin
      Font.Name  := 'Arial';
      Font.Size  := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    with FSelectChannelBtn do
    begin
      Font.Name  := 'Arial';
      Font.Size  := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelDialog.Resize;
const OPNAME = 'TIrrigationBlockReturnFlowChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelDialog.Initialise: boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FChannelNameLabel.Caption        := FAppModules.Language.GetString('TField.ChannelName')    + ' :';
    FUpStreamNodeLabel.Caption       := FAppModules.Language.GetString('TField.UpNodeNumber')   + ' :';
    FDownstreamNodeLabel.Caption     := FAppModules.Language.GetString('TField.DownNodeNumber') + ' :';
    FPenaltyStructureLabel.Caption   := FAppModules.Language.GetString('TField.PenaltyStruct')  + ' :';
    FSummaryOutputChkBox.Caption     := FAppModules.Language.GetString('TField.IncludeSummary') + ' ?';
    FFirmYieldAnalysisChkBox.Caption := FAppModules.Language.GetString('TField.PlanningPrintOut');
    FChannelAreaLabel.Caption        := FAppModules.Language.GetString('TField.ChannelArea')    + ' :';
    FSelectPenaltyStructureBtn.Hint  := FAppModules.Language.GetString('ButtonHint.Select');
    FSelectChannelBtn.Hint           := FAppModules.Language.GetString('ButtonHint.Select');
    FIrrigationBlockReturnFlowLossLabel.Caption   := FAppModules.Language.GetString('TField.IrrigationBlockReturnFlowLoss');
    FIrrigationBlockReturnFlowFactorLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockReturnFlowFactor');    
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelDialog.RestoreColourState;
const OPNAME = 'TIrrigationBlockReturnFlowChannelDialog.RestoreColourState';
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

procedure TIrrigationBlockReturnFlowChannelDialog.AssignHelpContext;
const OPNAME = 'TIrrigationBlockReturnFlowChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                                 HC_CreatingChannels);
    SetControlHelpContext(FChannelNameEdit,                     HC_CreatingChannels);
    SetControlHelpContext(FChannelNumberEdit,                   HC_CreatingChannels);
    SetControlHelpContext(FPenaltyStructureEdit,                HC_ChannelPenaltyStructures);
    SetControlHelpContext(FUpStreamNodeCbx,                     HC_CreatingChannels);
    SetControlHelpContext(FDownStreamNodeCbx,                   HC_CreatingChannels);
    SetControlHelpContext(FSelectPenaltyStructureBtn,           HC_ChannelPenaltyStructures);
    SetControlHelpContext(FSummaryOutputChkBox,                 HC_ChannelOutputOptions);
    SetControlHelpContext(FIrrigationBlockReturnFlowLossEdit,   HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockReturnFlowFactorEdit, HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
