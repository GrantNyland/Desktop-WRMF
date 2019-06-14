{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentreSupplyChannelDialog         *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/12/12                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentreSupplyChannelDialog;

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

  TYMDemandCentreSupplyChannelDialog = class(TAbstractScrollablePanel)
  protected
    FGeneralChannelsLabel : TLabel;
    FDownstreamNodeLabel  : TLabel;

    FDownStreamNodeEdit   : TFieldEdit;
    FSupplyChannelsGrid   : TFieldStringGrid;
    FGeneralChannelsCbx   : TFieldComboBox;
    FBtnInsertRow         : TFieldButton;
    FBtnDeleteRow         : TFieldButton;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    procedure ResetButtonState;

    property SupplyChannelsGrid   : TFieldStringGrid  read FSupplyChannelsGrid;
    property DownStreamNodeEdit   : TFieldEdit        read FDownStreamNodeEdit;
    property GeneralChannelsCbx   : TFieldComboBox    read FGeneralChannelsCbx;
    property BtnInsertRow         : TFieldButton      read FBtnInsertRow;
    property BtnDeleteRow         : TFieldButton      read FBtnDeleteRow;
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
{* TYMDemandCentreSupplyChannelDialog                                     *}
{******************************************************************************}

procedure TYMDemandCentreSupplyChannelDialog.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreSupplyChannelDialog.CreateMemberObjects';
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
    FDownstreamNodeLabel          := CreateFieldLabel            (lOwner, lParent, 10,  5, 100, 21);
    FDownstreamNodeEdit           := CreateFieldEdit(FAppModules, lOwner, lParent, 120, 5, 300, 20, 1, True);
    FDownstreamNodeEdit.Enabled   := False;
    FDownstreamNodeEdit.Color     := clBtnFace;

    FGeneralChannelsLabel         := CreateFieldLabel                (lOwner, lParent,  10,  35, 100, 21);
    FGeneralChannelsCbx           := CreateFieldComboBox(FAppModules, lOwner, lParent, 120,  35, 300, 21, 5,   TRUE, csDropDownList);

    FBtnInsertRow               := CreateFieldButton(FAppModules, lOwner, lParent, 10,  65,  150, 25, 2,   TRUE, '');
    FBtnDeleteRow               := CreateFieldButton(FAppModules, lOwner, lParent, 170,  65,  150, 25, 2,   TRUE, '');

    lGridTop                    := 95;
    lWidth                      := lParent.Width - 20;
    lHeight                     := lParent.Height - lGridTop - 10;
    FSupplyChannelsGrid         := CreateFieldStringGrid(FAppModules, lOwner, lParent, 10, lGridTop, lWidth, lHeight, 3, TRUE);

    FSupplyChannelsGrid.Constraints.MinHeight := 100;
    FSupplyChannelsGrid.Constraints.MinWidth  := 200;
    FSupplyChannelsGrid.FixedCols             := 1;
    FSupplyChannelsGrid.ColCount              := 6;
    FSupplyChannelsGrid.DefaultColWidth       := 50;
    FSupplyChannelsGrid.DefaultRowHeight      := 20;
    FSupplyChannelsGrid.RowCount              := 2;
    FSupplyChannelsGrid.ColWidths[0]          := 50;
    FSupplyChannelsGrid.ColWidths[1]          := 100;
    FSupplyChannelsGrid.ColWidths[2]          := 200;
    FSupplyChannelsGrid.ColWidths[3]          := 200;
    FSupplyChannelsGrid.ColWidths[4]          := 100;
    FSupplyChannelsGrid.ColWidths[5]          := 100;
    FSupplyChannelsGrid.Options               := [goColSizing,  goFixedVertLine,
                                                      goFixedHorzLine,  goVertLine,
                                                      goHorzLine,       goRangeSelect,
                                                      goEditing, goRowSelect];
    FSupplyChannelsGrid.Anchors               := [akLeft, akRight, akTop, akBottom];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelDialog.Resize;
const OPNAME = 'TYMDemandCentreSupplyChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelDialog.Initialise: boolean;
const OPNAME = 'TYMDemandCentreSupplyChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FBtnInsertRow.Enabled := False;
    FBtnDeleteRow.Enabled := False;  
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TYMDemandCentreSupplyChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FGeneralChannelsLabel.Caption         := FAppModules.Language.GetString('TField.GeneralChannels');
    FDownstreamNodeLabel.Caption          := FAppModules.Language.GetString('TField.DownNodeNumber');
    FBtnInsertRow.Caption                 := FAppModules.Language.GetString('ButtonCaption.AddSupplyChannel');
    FBtnDeleteRow.Caption                 := FAppModules.Language.GetString('ButtonCaption.DeleteSupplyChannel');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelDialog.RestoreColourState;
const OPNAME = 'TYMDemandCentreSupplyChannelDialog.RestoreColourState';
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

procedure TYMDemandCentreSupplyChannelDialog.AssignHelpContext;
const OPNAME = 'TYMDemandCentreSupplyChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                 HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FDownStreamNodeEdit,  HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FSupplyChannelsGrid,  HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FGeneralChannelsCbx,  HC_ReturnFlowsFromLargeUrbanCentres);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreSupplyChannelDialog.ResetButtonState;
const OPNAME = 'TYMDemandCentreSupplyChannelDialog.ResetButtonState';
begin
  try
    BtnInsertRow.Enabled := False;
    BtnDeleteRow.Enabled := False;
    if(FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      BtnInsertRow.Enabled := True;

      BtnDeleteRow.Enabled := (goEditing in FSupplyChannelsGrid.Options) and
                              (FSupplyChannelsGrid.Col >= 0) and
                              (FSupplyChannelsGrid.Row >= 0);

      BtnDeleteRow.Enabled := (FSupplyChannelsGrid.RowCount >= 2);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
