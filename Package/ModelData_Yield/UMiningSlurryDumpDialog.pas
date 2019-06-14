{******************************************************************************}
{*  UNIT      : Contains the class TMiningSlurryDumpDialog.                   *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/03/09                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMiningSlurryDumpDialog;

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

  TMiningSlurryDumpDialog = class(TAbstractScrollablePanel)
  private
  protected
    FNumberOfSlurryDumpLabel : TLabel;
    FNumberOfSlurryDumpEdt   : TFieldEdit;
    FInsertSlurryBtn         : TFieldButton;
    FDeleteSlurryBtn         : TFieldButton;
    FSlurryDumpGrid          : TFieldButtonStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure ResetButtonState(ACount : integer);

    property NumberOfSlurryDumpEdt : TFieldEdit              read FNumberOfSlurryDumpEdt;
    property InsertSlurryBtn       : TFieldButton            read FInsertSlurryBtn;
    property DeleteSlurryBtn       : TFieldButton            read FDeleteSlurryBtn;
    property SlurryDumpGrid        : TFieldButtonStringGrid  read FSlurryDumpGrid;
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
{* TMiningSlurryDumpDialog                                              *}
{******************************************************************************}

procedure TMiningSlurryDumpDialog.CreateMemberObjects;
const OPNAME = 'TMiningSlurryDumpDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FNumberOfSlurryDumpLabel  := CreateFieldLabel(lOwner, lParent,                10,  10, 180, 21);
    FNumberOfSlurryDumpEdt    := CreateFieldEdit(FAppModules, lOwner, lParent,   210,  10, 40 , 20 , 8, TRUE);
    FInsertSlurryBtn          := CreateFieldButton(FAppModules, lOwner, lParent,  10,  50, 125 , 25 , 8, TRUE,'');
    FDeleteSlurryBtn          := CreateFieldButton(FAppModules, lOwner, lParent,  150, 50, 125 , 25 , 8, TRUE,'');

    FSlurryDumpGrid           := CreateFieldButtonStringGrid(FAppModules, ControlsOwner, ControlsParent,  10, 80, 825, 176, 1, TRUE);
    with FSlurryDumpGrid do
    begin
      BorderStyle      := bsSingle;
      ColCount         := 9;
      FixedCols        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 90;
      RowHeights[0]    := 35;
      WrapHeaderText   := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpDialog.Resize;
const OPNAME = 'TMiningSlurryDumpDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningSlurryDumpDialog.Initialise: boolean;
const OPNAME = 'TMiningSlurryDumpDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningSlurryDumpDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMiningSlurryDumpDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNumberOfSlurryDumpLabel.Caption := FAppModules.Language.GetString('TField.NrOfSlurryDump');
    FInsertSlurryBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.InsertSlurry');
    FDeleteSlurryBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.DeleteSlurry');

    FSlurryDumpGrid.Cells[0,0]       := FAppModules.Language.GetString('GridHeading.DumpID');
    FSlurryDumpGrid.Cells[1,0]       := FAppModules.Language.GetString('GridHeading.DumpName');
    FSlurryDumpGrid.Cells[2,0]       := FAppModules.Language.GetString('GridHeading.DumpSurfaceArea');
    FSlurryDumpGrid.Cells[3,0]       := FAppModules.Language.GetString('GridHeading.RunoffFactorToPCD');
    FSlurryDumpGrid.Cells[4,0]       := FAppModules.Language.GetString('GridHeading.SeepageSplitFactor');
    FSlurryDumpGrid.Cells[5,0]       := FAppModules.Language.GetString('GridHeading.PCDStorageCapacity');
    FSlurryDumpGrid.Cells[6,0]       := FAppModules.Language.GetString('GridHeading.PCDSurfaceArea');
    FSlurryDumpGrid.Cells[7,0]       := FAppModules.Language.GetString('GridHeading.PCDAnalysisStartVolume');
    FSlurryDumpGrid.Cells[8,0]       := FAppModules.Language.GetString('GridHeading.MonthlyRecharge');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningSlurryDumpDialog.RestoreColourState;
const OPNAME = 'TMiningSlurryDumpDialog.RestoreColourState';
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

procedure TMiningSlurryDumpDialog.AssignHelpContext;
const OPNAME = 'TMiningSlurryDumpDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                    HC_CoalMines);
    SetControlHelpContext(FNumberOfSlurryDumpEdt,  HC_CoalMines);
    SetControlHelpContext(FSlurryDumpGrid,         HC_CoalMines);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningSlurryDumpDialog.ResetButtonState(ACount : integer);
const OPNAME = 'TMiningSlurryDumpDialog.ResetButtonState';
begin
  try
    InsertSlurryBtn.Enabled := False;
    DeleteSlurryBtn.Enabled := False;
    if(FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      InsertSlurryBtn.Enabled := (ACount < 10);

      DeleteSlurryBtn.Enabled := (goEditing in SlurryDumpGrid.Options) and
                                 (ACount > 0);

      DeleteSlurryBtn.Enabled := (ACount > 0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
