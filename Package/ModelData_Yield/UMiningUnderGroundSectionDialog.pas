{******************************************************************************}
{*  UNIT      : Contains the class TMiningUnderGroundSectionDialog.           *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/03/14                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMiningUnderGroundSectionDialog;

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

  TMiningUnderGroundSectionDialog = class(TAbstractScrollablePanel)
  private
  protected
    FNrOfUnderGroundMiningLabel : TLabel;
    FNrOfUnderGroundMiningEdt   : TFieldEdit;
    FInsertUndergroundBtn       : TFieldButton;
    FDeleteUndergroundSlurryBtn : TFieldButton;
    FUnderGroundMiningGrid      : TFieldButtonStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure ResetButtonState(ACount : integer);

    property NrOfUnderGroundMiningEdt   : TFieldEdit              read FNrOfUnderGroundMiningEdt;
    property UnderGroundMiningGrid      : TFieldButtonStringGrid  read FUnderGroundMiningGrid;
    property InsertUndergroundBtn       : TFieldButton            read FInsertUndergroundBtn;
    property DeleteUndergroundSlurryBtn : TFieldButton            read FDeleteUndergroundSlurryBtn;
  end;

  implementation

uses
  VCL.Grids,
  SysUtils,
  UConstants,
  VCL.Forms,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TMiningUnderGroundSectionDialog                                              *}
{******************************************************************************}

procedure TMiningUnderGroundSectionDialog.CreateMemberObjects;
const OPNAME = 'TMiningUnderGroundSectionDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FNrOfUnderGroundMiningLabel := CreateFieldLabel(lOwner, lParent,                10,  10,   180, 21);
    FNrOfUnderGroundMiningEdt   := CreateFieldEdit(FAppModules, lOwner, lParent,    210, 10,    40 ,  20 , 8, TRUE);
    FInsertUndergroundBtn       := CreateFieldButton(FAppModules, lOwner, lParent,  10,  50, 125 , 25 , 8, TRUE,'');
    FDeleteUndergroundSlurryBtn := CreateFieldButton(FAppModules, lOwner, lParent,  150, 50, 125 , 25 , 8, TRUE,'');
    FUnderGroundMiningGrid      := CreateFieldButtonStringGrid(FAppModules, ControlsOwner, ControlsParent,  10, 80, 825, 166, 1, TRUE);
    with FUnderGroundMiningGrid do
    begin
      BorderStyle      := bsSingle;
      ColCount         := 10;
      FixedCols        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 81;
      RowHeights[0]    := 40;
      WrapHeaderText   := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionDialog.Resize;
const OPNAME = 'TMiningUnderGroundSectionDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionDialog.Initialise: boolean;
const OPNAME = 'TMiningUnderGroundSectionDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningUnderGroundSectionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMiningUnderGroundSectionDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNrOfUnderGroundMiningLabel.Caption := FAppModules.Language.GetString('TField.NrOfUnderGroundMining');

    UnderGroundMiningGrid.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.UndergroundMineID');
    UnderGroundMiningGrid.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.ChannelNumberToUGDam');
    UnderGroundMiningGrid.Cells[2,0]   := FAppModules.Language.GetString('GridHeading.UndergroundSectionName');
    UnderGroundMiningGrid.Cells[3,0]   := FAppModules.Language.GetString('GridHeading.UpstreamCatchmentArea');
    UnderGroundMiningGrid.Cells[4,0]   := FAppModules.Language.GetString('GridHeading.BoardPillarCatchmentArea');
    UnderGroundMiningGrid.Cells[5,0]   := FAppModules.Language.GetString('GridHeading.HighExtractionCatchmentArea');
    UnderGroundMiningGrid.Cells[6,0]   := FAppModules.Language.GetString('GridHeading.HighExtractionAreaRunoffFactor');
    UnderGroundMiningGrid.Cells[7,0]   := FAppModules.Language.GetString('GridHeading.MineUGUpstreamRunoff');
    UnderGroundMiningGrid.Cells[8,0]   := FAppModules.Language.GetString('GridHeading.UGBoardPillarRechargeFactors');
    UnderGroundMiningGrid.Cells[9,0]   := FAppModules.Language.GetString('GridHeading.UGHighExtractionRechargeFactors');

    FInsertUndergroundBtn.Caption       := FAppModules.Language.GetString('ButtonCaption.InsertUnderground');
    FDeleteUndergroundSlurryBtn.Caption := FAppModules.Language.GetString('ButtonCaption.DeleteUnderground');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningUnderGroundSectionDialog.RestoreColourState;
const OPNAME = 'TMiningUnderGroundSectionDialog.RestoreColourState';
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

procedure TMiningUnderGroundSectionDialog.AssignHelpContext;
const OPNAME = 'TMiningUnderGroundSectionDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                       HC_CoalMines);
    SetControlHelpContext(FNrOfUnderGroundMiningEdt,  HC_CoalMines);
    SetControlHelpContext(FUnderGroundMiningGrid,     HC_CoalMines);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningUnderGroundSectionDialog.ResetButtonState(ACount: integer);
const OPNAME = 'TMiningUnderGroundSectionDialog.ResetButtonState';
begin
  try
    InsertUndergroundBtn.Enabled := False;
    DeleteUndergroundSlurryBtn.Enabled := False;
    if(FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      InsertUndergroundBtn.Enabled := (ACount < 10);

      DeleteUndergroundSlurryBtn.Enabled := (goEditing in UnderGroundMiningGrid.Options) and
                                 (ACount > 0);

      DeleteUndergroundSlurryBtn.Enabled := (ACount > 0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
