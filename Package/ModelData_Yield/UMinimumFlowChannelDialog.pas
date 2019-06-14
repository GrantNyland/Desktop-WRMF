{******************************************************************************}
{*  UNIT      : Contains the class TMinimumFlowChannelDialog.                 *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/03                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMinimumFlowChannelDialog;

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

  TMinimumFlowChannelDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel        : TLabel;
    FFeatureNameEdit         : TFieldEdit;
    //FFirmYieldAnalysisChkBox : TFieldChkBox;
    //FFirmYieldAnalysisLabel  : TLabel;
    FMinFlowDemandLabel      : TLabel;
    FMinFlowDemandGrid       : TFieldStringGrid;
    FTotalsGrid              : TFieldStringGrid;
    FTotalsLabel             : TLabel;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property FeatureNameEdit         : TFieldEdit       read FFeatureNameEdit;
    //property FirmYieldAnalysisChkBox : TFieldChkBox     read FFirmYieldAnalysisChkBox;
    property MinFlowDemandLabel      : TLabel           read FMinFlowDemandLabel;
    property MinFlowDemandGrid       : TFieldStringGrid read FMinFlowDemandGrid;
    property TotalsGrid              : TFieldStringGrid read FTotalsGrid;
    property TotalsLabel             : TLabel           read FTotalsLabel;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Grids,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TMinimumFlowChannelDialog                                                  *}
{******************************************************************************}

procedure TMinimumFlowChannelDialog.CreateMemberObjects;
const OPNAME = 'TMinimumFlowChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                Left  Top Width Height
    FFeatureNameLabel        := CreateFieldLabel                  (lOwner, lParent,  10,  30, 140,  21);
    FFeatureNameEdit         := CreateFieldEdit      (FAppModules, lOwner, lParent, 160,  30, 180,  21, 0, TRUE);
    {FFeatureNameLabel        := CreateFieldLabel                  (lOwner, lParent,  10,  10, 140,  21);
    FFeatureNameEdit         := CreateFieldEdit      (FAppModules, lOwner, lParent, 160,  10, 180,  21, 0, TRUE);
    FFirmYieldAnalysisChkBox := CreateFieldChkBox    (FAppModules, lOwner, lParent,   8,  35, 165,  21, 1, TRUE, taLeftJustify);
    FFirmYieldAnalysisLabel  := CreateFieldLabel                  (lOwner, lParent, 180,  35, 163,  21);
    }
    FMinFlowDemandLabel      := CreateFieldLabel                  (lOwner, lParent,  10,  60, 140,  26);
    FMinFlowDemandGrid       := CreateFieldStringGrid(FAppModules, lOwner, lParent, 160,  60, 160, 255, 2, TRUE);
    FTotalsGrid              := CreateFieldStringGrid(FAppModules, lOwner, lParent, 160, 319, 160,  24,  0, TRUE);
    FTotalsLabel             := CreateFieldLabel                  (lOwner, lParent, 330, 319, 100,  21);
//    FFeatureNameEdit.IsEnabled := FALSE;
//    FFeatureNameEdit.Color   := clBtnFace;
    FMinFlowDemandLabel.WordWrap  := TRUE;
    with FMinFlowDemandGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 12;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 95;
      ColWidths[0]     := 60;
    end;
    with FTotalsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 1;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 95;
      ColWidths[0]     := 60;
      Options          := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelDialog.Resize;
const OPNAME = 'TMinimumFlowChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelDialog.Initialise: boolean;
const OPNAME = 'TMinimumFlowChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMinimumFlowChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption        := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    //FFirmYieldAnalysisChkBox.Caption := FAppModules.Language.GetString('TField.FirmYieldCalc') + ' ?';
    //FFirmYieldAnalysisLabel.Caption  := FAppModules.Language.GetString('Channel.InAdditionToMasterControl');
    FMinFlowDemandLabel.Caption      := FAppModules.Language.GetString('TField.MinFlowDemandDescr') + ' :';
    FTotalsLabel.Caption             := FAppModules.Language.GetString('Channel.MillionM3PerYear');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelDialog.RestoreColourState;
const OPNAME = 'TMinimumFlowChannelDialog.RestoreColourState';
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


procedure TMinimumFlowChannelDialog.AssignHelpContext;
const OPNAME = 'TMinimumFlowChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                     HC_MinimumFlowSpecifications);
    SetControlHelpContext(FFeatureNameEdit,         HC_MinimumFlowSpecifications);
    //SetControlHelpContext(FFirmYieldAnalysisChkBox, HC_MinimumFlowSpecifications);
    SetControlHelpContext(FMinFlowDemandGrid,       HC_MinimumFlowSpecifications);
    SetControlHelpContext(FTotalsGrid,              HC_MinimumFlowSpecifications);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.




