{******************************************************************************}
{*  UNIT      : Contains the class TMineSubcatcmentDialog.                    *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/11/09                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMineSubcatchmentDialog;

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

  TMineSubcatchmentDialog = class(TAbstractScrollablePanel)
  private
  protected
    FCatchmentRefNrLabel               : TLabel;
    FCatchmentRefNrEdit                : TFieldEdit;
    FMinimunGroundwaterFlowVolumeGrid  : TFieldButtonStringGrid;
    FReservoirListBox                  : TFieldListBox;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property CatchmentRefNrEdit               : TFieldEdit             read FCatchmentRefNrEdit;
    property MinimunGroundwaterFlowVolumeGrid : TFieldButtonStringGrid read FMinimunGroundwaterFlowVolumeGrid;
    property ReservoirListBox                 : TFieldListBox          read FReservoirListBox;
 end;

  implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Forms,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TMineSubcatchmentDialog                                                    *}
{******************************************************************************}

procedure TMineSubcatchmentDialog.CreateMemberObjects;
const OPNAME = 'TMineSubcatchmentDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
//                                                                        Left  Top Width Height
    FCatchmentRefNrLabel              := CreateFieldLabel(lOwner, lParent, 10,  10,  190, 21);
    FCatchmentRefNrEdit               := CreateFieldEdit(FAppModules, lOwner, lParent,210,10, 40, 20 , 1, TRUE);
    FMinimunGroundwaterFlowVolumeGrid := CreateFieldButtonStringGrid(FAppModules, ControlsOwner, ControlsParent,  10, 40, 530, 200, 1, TRUE);
    with FMinimunGroundwaterFlowVolumeGrid do
    begin
      BorderStyle      := bsSingle;
      ColCount         := 5;
      FixedCols        := 0;
      DefaultColWidth  := 100;
      DefaultRowHeight := 25;
      RowHeights[0]    := 30;
      ColWidths[0]     := 110;
      RowCount         := 2;
      WrapHeaderText   := True;
    end;

    FReservoirListBox := TFieldListBox.Create(lOwner, FAppModules);
    with FReservoirListBox do
    begin
      Parent     := lParent;
      Top        := FMinimunGroundwaterFlowVolumeGrid.Top;
      Left       := 550;
      Width      := 160;
      Height     := FMinimunGroundwaterFlowVolumeGrid.Height;
      TabStop    := FALSE;
      TabOrder   := 4;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentDialog.Resize;
const OPNAME = 'TMineSubcatchmentDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentDialog.Initialise: boolean;
const OPNAME = 'TMineSubcatchmentDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubcatchmentDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMineSubcatchmentDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FCatchmentRefNrLabel.Caption := FAppModules.Language.GetString('TField.NrOfGroundwaterSubCatchment');
    FMinimunGroundwaterFlowVolumeGrid.Cells[0,0] := 'Catchment ref name';
    FMinimunGroundwaterFlowVolumeGrid.Cells[1,0] := 'Proportion antecedent flow';
    FMinimunGroundwaterFlowVolumeGrid.Cells[2,0] := 'Groundwater flow volume(million m3)';
    FMinimunGroundwaterFlowVolumeGrid.Cells[3,0] := 'Antecedent runoff decay factor';
    FMinimunGroundwaterFlowVolumeGrid.Cells[4,0] := 'Monthly flow volume(million m3)';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubcatchmentDialog.RestoreColourState;
const OPNAME = 'TMineSubcatchmentDialog.RestoreColourState';
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

procedure TMineSubcatchmentDialog.AssignHelpContext;
const OPNAME = 'TMineSubcatchmentDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                            HC_WaterResourcesYieldModel);
    SetControlHelpContext(FCatchmentRefNrEdit,             HC_WaterResourcesYieldModel);
    SetControlHelpContext(FReservoirListBox,               HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
