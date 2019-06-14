{******************************************************************************}
{*  UNIT      : Contains the class TMiningOpenCastPitDialog.                  *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/03/12                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMiningOpenCastPitDialog;

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

  TMiningOpenCastPitDialog = class(TAbstractScrollablePanel)
  private
  protected
    FNumberOfOpenCastPitsLabel : TLabel;
    FNumberOfOpenCastPitsEdt   : TFieldEdit;
    FInsertOpenCastBtn         : TFieldButton;
    FDeleteOpenCastBtn         : TFieldButton;
    FOpenCastPitsGrid          : TFieldButtonStringGrid;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure ResetButtonState(ACount : integer);

    property NumberOfOpenCastPitsEdt : TFieldEdit              read FNumberOfOpenCastPitsEdt;
    property OpenCastPitsGrid        : TFieldButtonStringGrid  read FOpenCastPitsGrid;
    property InsertOpenCastBtn       : TFieldButton            read FInsertOpenCastBtn;
    property DeleteOpenCastBtn       : TFieldButton            read FDeleteOpenCastBtn;
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
{* TMiningOpenCastPitDialog                                              *}
{******************************************************************************}

procedure TMiningOpenCastPitDialog.CreateMemberObjects;
const OPNAME = 'TMiningOpenCastPitDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
//                                                                               Left  Top Width Height
    FNumberOfOpenCastPitsLabel := CreateFieldLabel(lOwner, lParent,                10,  10,   180, 21);
    FNumberOfOpenCastPitsEdt   := CreateFieldEdit(FAppModules, lOwner, lParent,    210, 10,    40 ,  20 , 8, TRUE);
    FInsertOpenCastBtn         := CreateFieldButton(FAppModules, lOwner, lParent,  10,  50, 125 , 25 , 8, TRUE,'');
    FDeleteOpenCastBtn         := CreateFieldButton(FAppModules, lOwner, lParent,  150, 50, 125 , 25 , 8, TRUE,'');

    FOpenCastPitsGrid          := CreateFieldButtonStringGrid(FAppModules, ControlsOwner, ControlsParent,  10, 80, 800, 176, 1, TRUE);
    with FOpenCastPitsGrid do
    begin
      BorderStyle      := bsSingle;
      ColCount         := 19;
      FixedCols        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 66;
      RowHeights[0]    := 50;
      WrapHeaderText   := True;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitDialog.Resize;
const OPNAME = 'TMiningOpenCastPitDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitDialog.Initialise: boolean;
const OPNAME = 'TMiningOpenCastPitDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningOpenCastPitDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMiningOpenCastPitDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNumberOfOpenCastPitsLabel.Caption := FAppModules.Language.GetString('TField.NrOfOpenCastPits');

      OpenCastPitsGrid.Cells[0,0]  := FAppModules.Language.GetString('GridHeading.PitID ');
      OpenCastPitsGrid.Cells[1,0]  := FAppModules.Language.GetString('GridHeading.PitName');
      OpenCastPitsGrid.Cells[2,0]  := FAppModules.Language.GetString('GridHeading.CoalReserveArea');
      OpenCastPitsGrid.Cells[3,0]  := FAppModules.Language.GetString('GridHeading.WorkingsArea');
      OpenCastPitsGrid.Cells[4,0]  := FAppModules.Language.GetString('GridHeading.DisturbedWorkingsArea');
      OpenCastPitsGrid.Cells[5,0]  := FAppModules.Language.GetString('GridHeading.DisturbedArea');
      OpenCastPitsGrid.Cells[6,0]  := FAppModules.Language.GetString('GridHeading.WaterSurfaceEvapArea');
      OpenCastPitsGrid.Cells[7,0]  := FAppModules.Language.GetString('GridHeading.DisturbedAreaRunOff');
      OpenCastPitsGrid.Cells[8,0]  := FAppModules.Language.GetString('GridHeading.DisturbedWorkingsAreaRunOff');
      OpenCastPitsGrid.Cells[9,0]  := FAppModules.Language.GetString('GridHeading.DecantVolume');
      OpenCastPitsGrid.Cells[10,0] := FAppModules.Language.GetString('GridHeading.SeepageVolume');
      OpenCastPitsGrid.Cells[11,0] := FAppModules.Language.GetString('GridHeading.AnalysisStartVolume');
      OpenCastPitsGrid.Cells[12,0] := FAppModules.Language.GetString('GridHeading.MaximumSeepageRate');
      OpenCastPitsGrid.Cells[13,0] := FAppModules.Language.GetString('GridHeading.SeepageExponent');
      OpenCastPitsGrid.Cells[14,0] := FAppModules.Language.GetString('GridHeading.PCDSurfaceArea');
      OpenCastPitsGrid.Cells[15,0] := FAppModules.Language.GetString('GridHeading.PCDStorageCapacity');
      OpenCastPitsGrid.Cells[16,0] := FAppModules.Language.GetString('GridHeading.PCDAnalysisStartVolume');
      OpenCastPitsGrid.Cells[17,0] := FAppModules.Language.GetString('GridHeading.DisturbedMonthlyRecharge');
      OpenCastPitsGrid.Cells[18,0] := FAppModules.Language.GetString('GridHeading.DisturbedWorkingsMonthlyRecharge');

      FInsertOpenCastBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.InsertOpenCast');
      FDeleteOpenCastBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.DeleteOpenCast');


    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningOpenCastPitDialog.RestoreColourState;
const OPNAME = 'TMiningOpenCastPitDialog.RestoreColourState';
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

procedure TMiningOpenCastPitDialog.AssignHelpContext;
const OPNAME = 'TMiningOpenCastPitDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                      HC_CoalMines);
    SetControlHelpContext(FNumberOfOpenCastPitsEdt,  HC_CoalMines);
    SetControlHelpContext(FOpenCastPitsGrid,         HC_CoalMines);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningOpenCastPitDialog.ResetButtonState(ACount: integer);
const OPNAME = 'TMiningOpenCastPitDialog.ResetButtonState';
begin
  try
    InsertOpenCastBtn.Enabled := False;
    DeleteOpenCastBtn.Enabled := False;
    if(FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      InsertOpenCastBtn.Enabled := (ACount < 10);

      DeleteOpenCastBtn.Enabled := (goEditing in OpenCastPitsGrid.Options) and
                                   (ACount > 0);

      DeleteOpenCastBtn.Enabled := (ACount > 0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
