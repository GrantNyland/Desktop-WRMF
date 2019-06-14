{******************************************************************************}
{*  UNIT      : Contains the class TGeneralFlowChannelDialog.                 *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UGeneralFlowChannelDialog;

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

  TGeneralFlowChannelDialog = class(TAbstractScrollablePanel)
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
    FFlowOutputChkBox             : TFieldChkBox;
    FGrowthChkBox                 : TFieldChkBox;

    FSelectChannelBtn             : TFieldButton;
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
    property FlowOutputChkBox          : TFieldChkBox   read FFlowOutputChkBox;
    property SelectChannelBtn          : TFieldButton   read FSelectChannelBtn;
    property GrowthChkBox              : TFieldChkBox   read FGrowthChkBox;

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
{* TGeneralFlowChannelDialog                                                  *}
{******************************************************************************}

procedure TGeneralFlowChannelDialog.CreateMemberObjects;
const OPNAME = 'TGeneralFlowChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                Left  Top Width Height
    FChannelNameLabel          := CreateFieldLabel   (lOwner, lParent,  10,  10, 140, 21);
    FUpStreamNodeLabel         := CreateFieldLabel   (lOwner, lParent,  10,  35, 140, 21);
    FDownstreamNodeLabel       := CreateFieldLabel   (lOwner, lParent,  10,  60, 140, 21);
    FChannelAreaLabel          := CreateFieldLabel   (lOwner, lParent,  10,  85, 140, 21);
    FPenaltyStructureLabel     := CreateFieldLabel   (lOwner, lParent,  10,  110, 140, 21);

    FChannelNumberEdit         := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  10,  35, 21, 0,   FALSE);
    FChannelNameEdit           := CreateFieldEdit    (FAppModules, lOwner, lParent, 200,  10, 240, 21, 1,   TRUE);
    FSelectChannelBtn          := CreateFieldButton  (FAppModules, lOwner, lParent, 455,  10,  25, 21, 2,   TRUE, '...');
    FUpStreamNodeCbx           := CreateFieldComboBox(FAppModules, lOwner, lParent, 160,  35, 280, 21, 3,   TRUE, csDropDownList);
    FUpStreamNodeEdit          := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  35, 280, 21, 4,   FALSE);
    FDownStreamNodeCbx         := CreateFieldComboBox(FAppModules, lOwner, lParent, 160,  60, 280, 21, 5,   TRUE, csDropDownList);
    FDownStreamNodeEdit        := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  60, 280, 21, 6,   FALSE);
    FChannelAreaCbx            := CreateFieldComboBox(FAppModules, lOwner, lParent, 160,  85, 280, 21, 7,   TRUE, csDropDownList);
    FPenaltyStructureEdit      := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  110,  50, 21, 7,  TRUE);
    FSelectPenaltyStructureBtn := CreateFieldButton  (FAppModules, lOwner, lParent, 215,  110,  25, 21, 8,  TRUE, '...');
    FSummaryOutputChkBox       := CreateFieldChkBox  (FAppModules, lOwner, lParent,   8,  135, 165, 21, 9,  TRUE, taLeftJustify);
    FFirmYieldAnalysisChkBox   := CreateFieldChkBox  (FAppModules, lOwner, lParent,   8,  160, 165, 21, 10, TRUE, taLeftJustify);

    if (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      FFlowOutputChkBox   := CreateFieldChkBox  (FAppModules, lOwner, lParent,   8,  185, 165, 21, 10, TRUE, taLeftJustify);
      FGrowthChkBox := CreateFieldChkBox(FAppModules, lOwner, lParent,   8,  215, 165, 21, 10, TRUE, taLeftJustify);



    end;
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

procedure TGeneralFlowChannelDialog.Resize;
const OPNAME = 'TGeneralFlowChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelDialog.Initialise: boolean;
const OPNAME = 'TGeneralFlowChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TGeneralFlowChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FChannelNameLabel.Caption        := FAppModules.Language.GetString('TField.ChannelName')    + ' :';
    FUpStreamNodeLabel.Caption       := FAppModules.Language.GetString('TField.UpNodeNumber')   + ' :';
    FDownstreamNodeLabel.Caption     := FAppModules.Language.GetString('TField.DownNodeNumber') + ' :';
    FPenaltyStructureLabel.Caption   := FAppModules.Language.GetString('TField.PenaltyStruct')  + ' :';
    FSummaryOutputChkBox.Caption     := FAppModules.Language.GetString('TField.IncludeSummary') + ' ?';
    if (FAppModules.Model.ModelName = CYield) then
      FFirmYieldAnalysisChkBox.Caption := FAppModules.Language.GetString('TField.FirmYieldCalc') + ' ?';
    if (FAppModules.Model.ModelName = CPlanning) then
      FFirmYieldAnalysisChkBox.Caption := FAppModules.Language.GetString('TField.PlanningPrintOut');
    if FGrowthChkBox <> nil then
    begin
      FFlowOutputChkBox.Caption := 'Flow Output';
      FGrowthChkBox.Caption := FAppModules.Language.GetString('channel.Growth');

    end;
    FChannelAreaLabel.Caption        := FAppModules.Language.GetString('TField.ChannelArea')    + ' :';
    FSelectPenaltyStructureBtn.Hint  := FAppModules.Language.GetString('ButtonHint.Select');
    FSelectChannelBtn.Hint           := FAppModules.Language.GetString('ButtonHint.Select');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelDialog.RestoreColourState;
const OPNAME = 'TGeneralFlowChannelDialog.RestoreColourState';
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

procedure TGeneralFlowChannelDialog.AssignHelpContext;
const OPNAME = 'TGeneralFlowChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                       HC_CreatingChannels);
    SetControlHelpContext(FChannelNameEdit,           HC_CreatingChannels);
    SetControlHelpContext(FChannelNumberEdit,         HC_CreatingChannels);
    SetControlHelpContext(FPenaltyStructureEdit,      HC_CreatingChannels);
    SetControlHelpContext(FUpStreamNodeCbx,           HC_CreatingChannels);
    SetControlHelpContext(FDownStreamNodeCbx,         HC_CreatingChannels);
    SetControlHelpContext(FSelectPenaltyStructureBtn, HC_ChannelPenaltyStructures);
    SetControlHelpContext(FSummaryOutputChkBox,       HC_ChannelOutputOptions);
    SetControlHelpContext(FFirmYieldAnalysisChkBox,   HC_CreatingChannels);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
