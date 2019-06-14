{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantTailwaterDialog.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantTailwaterDialog;

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

  TPowerPlantTailwaterDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel        : TLabel;
    FTailwaterTypeRadioGroup : TFieldRadioGroup;
    FTailwaterPanel          : TPanel;
    FTailwater1Label         : TLabel;
    FTailwater2Label         : TLabel;
    FTailwater3Label         : TLabel;
    FElevationsPanel         : TPanel;
    FElevationsLabel         : TLabel;
    FTailwaterGrid           : TFieldStringGrid;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property FeatureNameLabel        : TLabel            read FFeatureNameLabel;
    property TailwaterTypeRadioGroup : TFieldRadioGroup  read FTailwaterTypeRadioGroup;
    property TailwaterPanel          : TPanel            read FTailwaterPanel;
    property Tailwater1Label         : TLabel            read FTailwater1Label;
    property Tailwater2Label         : TLabel            read FTailwater2Label;
    property Tailwater3Label         : TLabel            read FTailwater3Label;
    property ElevationsPanel         : TPanel            read FElevationsPanel;
    property ElevationsLabel         : TLabel            read FElevationsLabel;
    property TailwaterGrid           : TFieldStringGrid  read FTailwaterGrid;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Forms,
  VCL.Grids,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TPowerPlantTailwaterDialog                                                 *}
{******************************************************************************}

procedure TPowerPlantTailwaterDialog.CreateMemberObjects;
const OPNAME = 'TPowerPlantTailwaterDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                                Left  Top  Width Height
    FFeatureNameLabel        := CreateFieldLabel      (lOwner,                lParent,  10,  10, 240,  21);
    FTailwaterTypeRadioGroup := CreateFieldRadioGroup (FAppModules, lOwner,                lParent,  10,  35, 235,  70, 0, FALSE);
    FTailwaterGrid           := CreateFieldStringGrid (FAppModules, lOwner,                lParent,  10, 110, 196, 259, 1, TRUE);
    with FTailwaterGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 11;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 85;
      ColWidths[0]     := 20;
      RowHeights[0]    := 45;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FTailwaterPanel          := CreatePanel           (lOwner, lParent,  36, 113,  80,  41, 2);
    FElevationsPanel         := CreatePanel           (lOwner, lParent, 120, 113,  80,  41, 3);
    with FTailwaterPanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FElevationsPanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    FTailwater1Label         := CreateFieldLabel      (lOwner,  FTailwaterPanel,   0,   0,  80,  41);
    FTailwater2Label         := CreateFieldLabel      (lOwner,  FTailwaterPanel,   0,   0,  80,  41);
    FTailwater3Label         := CreateFieldLabel      (lOwner,  FTailwaterPanel,   0,   0,  80,  41);
    FElevationsLabel         := CreateFieldLabel      (lOwner, FElevationsPanel,   0,   0,  80,  41);
    with FTailwater1Label do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
      Visible   := False;
    end;
    with FTailwater2Label do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
      Visible   := False;
    end;
    with FTailwater2Label do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
      Visible   := False;
    end;
    with FElevationsLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;

{

}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterDialog.Resize;
const OPNAME = 'TPowerPlantTailwaterDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterDialog.Initialise: boolean;
const OPNAME = 'TPowerPlantTailwaterDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantTailwaterDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FTailwaterTypeRadioGroup.Caption := ' ' + FAppModules.Language.GetString('TField.TailWaterTypeCode') + ' ';
    FTailwater1Label.Caption       := FAppModules.Language.GetString('NetworkFeatures.TailwaterDischarge');
    FTailwater2Label.Caption       := FAppModules.Language.GetString('NetworkFeatures.DownstreamReservoirElevations');
    FTailwater3Label.Caption       := FAppModules.Language.GetString('NetworkFeatures.UpstreamReservoirElevations');
    FElevationsLabel.Caption       := FAppModules.Language.GetString('TField.TailWaterElevation');
    FTailwaterTypeRadioGroup.Hint  := FAppModules.Language.GetString('TPowerPlantTailwaterDialog.TailWaterType');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterDialog.RestoreColourState;
const OPNAME = 'TPowerPlantTailwaterDialog.RestoreColourState';
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

procedure TPowerPlantTailwaterDialog.AssignHelpContext;
const OPNAME = 'TPowerPlantTailwaterDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                     HC_HydropowerPlants);
    SetControlHelpContext(FTailwaterGrid,           HC_HydropowerPlants);
    SetControlHelpContext(FTailwaterTypeRadioGroup, HC_HydropowerPlants);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.
