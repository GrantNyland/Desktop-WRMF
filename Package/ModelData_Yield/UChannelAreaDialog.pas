{******************************************************************************}
{*  UNIT      : Contains the class TChannelAreaDialog.                        *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2005/07/18                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChannelAreaDialog;

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

  TChannelAreaDialog = class(TAbstractScrollablePanel)
  private
  protected
    FChannelAreaCountEdt : TFieldEdit;
    FChannelAreaGrid     : TFieldStringGrid;
    FChannelAreaListBox  : TFieldListBox;
    FChannelAreaLabel    : TLabel;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property ChannelAreaCountEdt  : TFieldEdit       read FChannelAreaCountEdt;
    property ChannelAreaGrid      : TFieldStringGrid read FChannelAreaGrid;
    property ChannelAreaListBox   : TFieldListBox    read FChannelAreaListBox;

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
{* TChannelAreaDialog                                              *}
{******************************************************************************}

procedure TChannelAreaDialog.CreateMemberObjects;
const OPNAME = 'TChannelAreaDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
//                                                                              Left  Top Width Height
    FChannelAreaLabel     := CreateFieldLabel   (lOwner, lParent,                10,  5,  140, 21);
    FChannelAreaCountEdt  := CreateFieldEdit(FAppModules, lOwner, lParent,        150,   5,  40,  20 , 8, TRUE);
    FChannelAreaGrid      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  5,   35, 410, 176, 3, TRUE);

    with FChannelAreaGrid do
    begin
      ColCount         := 2;
      DefaultColWidth  := 40;
      DefaultRowHeight := 20;
      RowCount         := 4;
      ColWidths[0]     := 50;
      ColWidths[1]     := 350;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
    FChannelAreaListBox := TFieldListBox.Create(lOwner, FAppModules);
    with  FChannelAreaListBox do
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

procedure TChannelAreaDialog.Resize;
const OPNAME = 'TChannelAreaDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaDialog.Initialise: boolean;
const OPNAME = 'TChannelAreaDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelAreaDialog.LanguageHasChanged: boolean;
const OPNAME = 'TChannelAreaDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FChannelAreaGrid.Cells[0,0] := FAppModules.Language.GetString('TField.TheNumber');
    FChannelAreaGrid.Cells[1,0] := FAppModules.Language.GetString('TField.ChannelArea');
    FChannelAreaLabel.Caption   := FAppModules.Language.GetString('TField.ChannelAreaNumbers');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelAreaDialog.RestoreColourState;
const OPNAME = 'TChannelAreaDialog.RestoreColourState';
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

procedure TChannelAreaDialog.AssignHelpContext;
const OPNAME = 'TChannelAreaDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                  HC_WaterResourcesYieldModel);
    SetControlHelpContext(FChannelAreaCountEdt,  HC_WaterResourcesYieldModel);
    SetControlHelpContext(FChannelAreaGrid,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FChannelAreaListBox,   HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
