{******************************************************************************}
{*  UNIT      : Contains the class TChannelAreaDialog.                        *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2005/07/18                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UReservoirAreaGroupDialog;

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

  TReservoirAreaGroupDialog = class(TAbstractScrollablePanel)
  private
  protected
    FReservoirAreaGroupCountEdt : TFieldEdit;
    FReservoirAreaGroupGrid     : TFieldStringGrid;
    FReservoirAreaGroupListBox  : TFieldListBox;
    FReservoirAreaGroupLabel    : TLabel;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property ReservoirAreaGroupCountEdt  : TFieldEdit       read FReservoirAreaGroupCountEdt;
    property ReservoirAreaGroupGrid      : TFieldStringGrid read FReservoirAreaGroupGrid;
    property ReservoirAreaGroupListBox   : TFieldListBox    read FReservoirAreaGroupListBox;

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
{* TReservoirAreaGroupDialog                                              *}
{******************************************************************************}

procedure TReservoirAreaGroupDialog.CreateMemberObjects;
const OPNAME = 'TReservoirAreaGroupDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
//                                                                              Left  Top Width Height
    FReservoirAreaGroupLabel     := CreateFieldLabel   (lOwner, lParent,                10,  5,  180, 21);
    FReservoirAreaGroupCountEdt  := CreateFieldEdit(FAppModules, lOwner, lParent,        180,   5,  40,  20 , 8, TRUE);
    FReservoirAreaGroupGrid      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  5,   35, 410, 176, 3, TRUE);

    with FReservoirAreaGroupGrid do
    begin
      ColCount         := 2;
      DefaultColWidth  := 40;
      DefaultRowHeight := 20;
      RowCount         := 4;
      ColWidths[0]     := 50;
      ColWidths[1]     := 350;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
    FReservoirAreaGroupListBox := TFieldListBox.Create(lOwner, FAppModules);
    with  FReservoirAreaGroupListBox do
    begin
      Parent     := lParent;
      Top        := 35;
      Left       := 420;
      Width      := 230;
      Height     := 176;
      TabStop    := FALSE;
      TabOrder   := 4;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupDialog.Resize;
const OPNAME = 'TReservoirAreaGroupDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupDialog.Initialise: boolean;
const OPNAME = 'TReservoirAreaGroupDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreaGroupDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirAreaGroupDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FReservoirAreaGroupGrid.Cells[0,0] := FAppModules.Language.GetString('TField.TheNumber');
    FReservoirAreaGroupGrid.Cells[1,0] := FAppModules.Language.GetString('TField.ReservoirAreaGroupName');
    FReservoirAreaGroupLabel.Caption   := FAppModules.Language.GetString('TField.ReservoirAreaGroupCount');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreaGroupDialog.RestoreColourState;
const OPNAME = 'TReservoirAreaGroupDialog.RestoreColourState';
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

procedure TReservoirAreaGroupDialog.AssignHelpContext;
const OPNAME = 'TReservoirAreaGroupDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                  HC_WaterResourcesYieldModel);
    SetControlHelpContext(FReservoirAreaGroupCountEdt,  HC_WaterResourcesYieldModel);
    SetControlHelpContext(FReservoirAreaGroupGrid,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FReservoirAreaGroupListBox,   HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
