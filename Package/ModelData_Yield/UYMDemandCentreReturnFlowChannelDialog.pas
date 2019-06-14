{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentreReturnFlowChannelDialog     *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/12/08                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentreReturnFlowChannelDialog;

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
  UDataComponent,
  UConstants;

type

  TYMDemandCentreReturnFlowChannelDialog = class(TAbstractScrollablePanel)
  protected
    FNoOfReturnFlowChannelsLabel  : TLabel;
    FUpstreamNodeLabel            : TLabel;
    FDownstreamNodeLabel          : TLabel;
    FPenaltyStructureLabel        : TLabel;

    FUpstreamNodeEdit             : TFieldEdit;
    FReturnFlowChannelsGrid       : TFieldStringGrid;

    FBtnInsertRow                 : TFieldButton;
    FBtnDeleteRow                 : TFieldButton;

    FDownStreamNodeCbx            : TFieldComboBox;
    FPenaltyStructureEdit         : TFieldEdit;
    FSelectPenaltyStructureBtn    : TFieldButton;
    FSummaryOutputChkBox          : TFieldChkBox;
    FFirmYieldAnalysisChkBox      : TFieldChkBox;

    FTotalReturnFlowLabel : TLabel;
    FTotalReturnFlowEdit  : TFieldEdit;
    FFlowDiversionLabel   : TLabel;
    FFlowDiversionEdit    : TFieldEdit;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    procedure ResetButtonState;
        
    property ReturnFlowChannelsGrid     : TFieldStringGrid  read FReturnFlowChannelsGrid;
    property UpstreamNodeEdit           : TFieldEdit        read FUpstreamNodeEdit;
    property BtnInsertRow               : TFieldButton      read FBtnInsertRow;
    property BtnDeleteRow               : TFieldButton      read FBtnDeleteRow;

    property DownStreamNodeCbx          : TFieldComboBox  read FDownStreamNodeCbx;
    property SummaryOutputChkBox        : TFieldChkBox    read FSummaryOutputChkBox;
    property FirmYieldAnalysisChkBox    : TFieldChkBox    read FFirmYieldAnalysisChkBox;
    property PenaltyStructureEdit       : TFieldEdit      read FPenaltyStructureEdit;
    property SelectPenaltyStructureBtn  : TFieldButton    read FSelectPenaltyStructureBtn;
    property TotalReturnFlowEdit        : TFieldEdit      read FTotalReturnFlowEdit;
    property FlowDiversionEdit          : TFieldEdit      read FFlowDiversionEdit;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities, UCropWaterDialog;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TYMDemandCentreReturnFlowChannelDialog                                     *}
{******************************************************************************}

procedure TYMDemandCentreReturnFlowChannelDialog.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
  lWidth,
  lHeight,
  lGridTop    : Integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
                                            //Left  Top Width Height
    FUpstreamNodeLabel          := CreateFieldLabel            (lOwner, lParent, 10,  5, 100, 21);
    FUpstreamNodeEdit           := CreateFieldEdit(FAppModules, lOwner, lParent, 400, 5, 200, 20, 1, True);
    FUpstreamNodeEdit.Enabled   := False;
    FUpstreamNodeEdit.Color     := clBtnFace;

    FDownstreamNodeLabel        := CreateFieldLabel   (lOwner, lParent,  10,  35, 100, 21);
    FDownStreamNodeCbx          := CreateFieldComboBox(FAppModules, lOwner, lParent, 400,  35, 200, 21, 5,   TRUE, csDropDownList);

    FPenaltyStructureLabel      := CreateFieldLabel                (lOwner, lParent,  10,  65, 140, 21);
    FPenaltyStructureEdit       := CreateFieldEdit    (FAppModules, lOwner, lParent, 400,  65,  50, 21, 7,  TRUE);
    FSelectPenaltyStructureBtn  := CreateFieldButton  (FAppModules, lOwner, lParent, 455,  65,  25, 21, 8,  TRUE, '...');

    FSummaryOutputChkBox        := CreateFieldChkBox  (FAppModules, lOwner, lParent,   8,  95, 408, 21, 10, TRUE, taLeftJustify);
    FFirmYieldAnalysisChkBox    := CreateFieldChkBox  (FAppModules, lOwner, lParent,   8,  125, 408, 21, 10, TRUE, taLeftJustify);

    FFlowDiversionLabel         := CreateFieldLabel             (lOwner, lParent,  10,  155, 390,  21);
    FFlowDiversionEdit          := CreateFieldEdit (FAppModules, lOwner, lParent, 400,  155, 200,  21,  0, TRUE);

    FTotalReturnFlowLabel       := CreateFieldLabel             (lOwner, lParent,  10,  185, 280,  21);
    FTotalReturnFlowEdit        := CreateFieldEdit (FAppModules, lOwner, lParent, 400,  185, 200,  21,  0, TRUE);

    FBtnInsertRow               := CreateFieldButton(FAppModules, lOwner, lParent, 10,  220,  150, 25, 2,   TRUE, '');
    FBtnDeleteRow               := CreateFieldButton(FAppModules, lOwner, lParent, 170,  220,  150, 25, 2,   TRUE, '');

    lGridTop                    := 255;
    lWidth                      := lParent.Width - 20;
    lHeight                     := lParent.Height - lGridTop - 10;
    FReturnFlowChannelsGrid     := CreateFieldStringGrid(FAppModules, lOwner, lParent, 10, lGridTop, lWidth, lHeight, 3, TRUE);

    FReturnFlowChannelsGrid.Constraints.MinHeight := 100;
    FReturnFlowChannelsGrid.Constraints.MinWidth  := 200;
    FReturnFlowChannelsGrid.FixedCols             := 1;              
    FReturnFlowChannelsGrid.ColCount              := 3;
    FReturnFlowChannelsGrid.DefaultColWidth       := 40;
    FReturnFlowChannelsGrid.DefaultRowHeight      := 20;
    FReturnFlowChannelsGrid.RowCount              := 2;
    FReturnFlowChannelsGrid.ColWidths[0]          := 50;
    FReturnFlowChannelsGrid.ColWidths[1]          := 100;
    FReturnFlowChannelsGrid.ColWidths[2]          := 300;
    FReturnFlowChannelsGrid.Options               := [goColSizing,  goFixedVertLine,
                                                      goFixedHorzLine,  goVertLine,
                                                      goHorzLine,       goRangeSelect,
                                                      goEditing, goRowSelect];
    FReturnFlowChannelsGrid.Anchors               := [akLeft, akRight, akTop, akBottom];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelDialog.Resize;
const OPNAME = 'TYMDemandCentreReturnFlowChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelDialog.Initialise: boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FBtnInsertRow.Enabled := False;
    FBtnDeleteRow.Enabled := False;  
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNoOfReturnFlowChannelsLabel.Caption  := FAppModules.Language.GetString('TField.NoOfReturnFlowChannels');
    FUpstreamNodeLabel.Caption            := FAppModules.Language.GetString('TField.UpNodeNumber');

    FBtnInsertRow.Caption                 := FAppModules.Language.GetString('ButtonCaption.InsertReturnFlow');
    FBtnDeleteRow.Caption                 := FAppModules.Language.GetString('ButtonCaption.DeleteReturnFlow');

    FDownstreamNodeLabel.Caption          := FAppModules.Language.GetString('TField.DownNodeNumber');
    FPenaltyStructureLabel.Caption        := FAppModules.Language.GetString('TField.PenaltyStruct');
    FSummaryOutputChkBox.Caption          := FAppModules.Language.GetString('TField.IncludeSummary');
    FFirmYieldAnalysisChkBox.Caption      := FAppModules.Language.GetString('TField.FirmYieldCalc');
    FSelectPenaltyStructureBtn.Hint       := FAppModules.Language.GetString('ButtonHint.Select');

    FTotalReturnFlowLabel.Caption         := FAppModules.Language.GetString('TField.TotalReturnFlow');
    FFlowDiversionLabel.Caption           := FAppModules.Language.GetString('TField.FlowDiversion');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelDialog.RestoreColourState;
const OPNAME = 'TYMDemandCentreReturnFlowChannelDialog.RestoreColourState';
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

procedure TYMDemandCentreReturnFlowChannelDialog.AssignHelpContext;
const OPNAME = 'TYMDemandCentreReturnFlowChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                       HC_CreatingChannels);
    SetControlHelpContext(FPenaltyStructureEdit,      HC_CreatingChannels);
    SetControlHelpContext(FDownStreamNodeCbx,         HC_CreatingChannels);
    SetControlHelpContext(FSelectPenaltyStructureBtn, HC_ChannelPenaltyStructures);
    SetControlHelpContext(FSummaryOutputChkBox,       HC_ChannelOutputOptions);
    SetControlHelpContext(FFirmYieldAnalysisChkBox,   HC_FirmYIeldLine);
    SetControlHelpContext(FTotalReturnFlowEdit,       HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FFlowDiversionEdit,         HC_ReturnFlowsFromLargeUrbanCentres);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreReturnFlowChannelDialog.ResetButtonState;
const OPNAME = 'TYMDemandCentreReturnFlowChannelDialog.ResetButtonState';
begin
  try
    BtnInsertRow.Enabled := False;
    BtnDeleteRow.Enabled := False;
    if(FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      BtnInsertRow.Enabled := (ReturnFlowChannelsGrid.RowCount < 21);

      BtnDeleteRow.Enabled := (goEditing in FReturnFlowChannelsGrid.Options) and
                              (FReturnFlowChannelsGrid.Col >= 0) and
                              (FReturnFlowChannelsGrid.Row >= 0);

      BtnDeleteRow.Enabled := (FReturnFlowChannelsGrid.RowCount >= 2);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
