{******************************************************************************}
{*  UNIT      : Contains the class TFMYieldCharacteristicsDialog.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMYieldCharacteristicsDialog;

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

  TFMYieldCharacteristicsDialog = class(TAbstractScrollablePanel)
  private
  protected
    FLblNrOfLoadCases          : TLabel;
    FEdtNrOfLoadCases          : TFieldEdit;
    FLblPeriodLength           : TLabel;
    FEdtPeriodLength           : TFieldEdit;
    FLblPeriodLengthDescr      : TLabel;
    FLblNrOfStartStoragePercs  : TLabel;
    FEdtNrOfStartStoragePercs  : TFieldEdit;
    FGrdStartStoragePercs      : TFieldStringGrid;
    FLblNrOfCurveSets          : TLabel;
    FEdtNrOfCurveSets          : TFieldEdit;
    FNrOfCurveSetsDescr        : TLabel;
    FGrdCurveSets              : TFieldStringGrid;
    FLblStartVolume            : TLabel;
    FLblCurveSetIndicator      : TLabel;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property EdtNrOfLoadCases          : TFieldEdit       read FEdtNrOfLoadCases;
    property EdtPeriodLength           : TFieldEdit       read FEdtPeriodLength;
    property LblNrOfStartStoragePercs  : TLabel           read FLblNrOfStartStoragePercs;
    property EdtNrOfStartStoragePercs  : TFieldEdit       read FEdtNrOfStartStoragePercs;
    property GrdStartStoragePercs      : TFieldStringGrid read FGrdStartStoragePercs;
    property EdtNrOfCurveSets          : TFieldEdit       read FEdtNrOfCurveSets;
    property GrdCurveSets              : TFieldStringGrid read FGrdCurveSets;
  end;

implementation

uses
  SysUtils,
  VCL.Grids,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TFMYieldCharacteristicsDialog                                              *}
{******************************************************************************}

procedure TFMYieldCharacteristicsDialog.CreateMemberObjects;
const OPNAME = 'TFMYieldCharacteristicsDialog.CreateMemberObjects';
var
  lOwner       : TComponent;
  lParent      : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //  Left  Top Width Height
    FLblNrOfLoadCases         := TLabel.Create(lOwner);
    with FLblNrOfLoadCases do
    begin
      Parent    := lParent;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 10;
      Width     := 230;
      Height    := 21;
    end;
    FEdtNrOfLoadCases         := CreateFieldEdit (FAppModules, lOwner, lParent, 245,  10,  30, 21, 0, TRUE);

    FLblPeriodLength          := TLabel.Create(lOwner);
    with FLblPeriodLength do
    begin
      Parent    := lParent;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 40;
      Width     := 230;
      Height    := 21;
    end;
    FEdtPeriodLength          := CreateFieldEdit (FAppModules, lOwner, lParent, 245,  40,  30, 21, 1, TRUE);
    FLblPeriodLengthDescr     := CreateFieldLabel             (lOwner, lParent, 280,  40, 310, 21);

    FLblNrOfCurveSets         := TLabel.Create(lOwner);
    with FLblNrOfCurveSets do
    begin
      Parent    := lParent;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 70;
      Width     := 230;
      Height    := 21;
    end;
    FEdtNrOfCurveSets   := CreateFieldEdit(FAppModules, lOwner, lParent, 245, 70,  30, 21, 4, TRUE);
    FNrOfCurveSetsDescr := CreateFieldLabel(lOwner, lParent, 280,  70, 310, 21);


    FLblCurveSetIndicator := TLabel.Create(lOwner);
    with FLblCurveSetIndicator do
    begin
      Parent    := lParent;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 120;
      Width     := 230;
      Height    := 21;
    end;

    FGrdCurveSets      := CreateFieldStringGrid(FAppModules, lOwner, lParent, 245, 100, 431, 277, 5, TRUE);
    with FGrdCurveSets do
    begin
      ColCount         := 13;
      RowCount         := 11;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 30;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    end;

    FLblNrOfStartStoragePercs := TLabel.Create(lOwner);
    with FLblNrOfStartStoragePercs do
    begin
      Parent    := lParent;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 180;
      Width     := 230;
      Height    := 21;
    end;
    FEdtNrOfStartStoragePercs := CreateFieldEdit (FAppModules, lOwner, lParent, 245, 180, 30, 21, 2, TRUE);

    FGrdStartStoragePercs     := CreateFieldStringGrid(FAppModules, lOwner, lParent, 245, 210, 238, 24, 3, TRUE);
    with FGrdStartStoragePercs do
    begin
      ColCount         := 6;
      RowCount         := 1;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 38;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    
    FLblStartVolume := TLabel.Create(lOwner);
    with FLblStartVolume do
    begin
      Parent    := lParent;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 270;
      Width     := 230;
      Height    := 21;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsDialog.Resize;
const OPNAME = 'TFMYieldCharacteristicsDialog.Resize';
begin
  inherited Resize;
  try
    EdtNrOfStartStoragePercs.Top := GrdCurveSets.Height + 15 + 95;
    GrdStartStoragePercs.Top     := EdtNrOfStartStoragePercs.Top + 30;
    LblNrOfStartStoragePercs.Top := EdtNrOfStartStoragePercs.Top;
    FLblStartVolume.Top          := EdtNrOfStartStoragePercs.Top + 30
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsDialog.Initialise: boolean;
const OPNAME = 'TFMYieldCharacteristicsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TFMYieldCharacteristicsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblNrOfLoadCases.Caption         := FAppModules.Language.GetString('PlanningGUI.NrOfLoadCases');
    FLblPeriodLength.Caption          := FAppModules.Language.GetString('PlanningGUI.PeriodLength');
    FLblPeriodLengthDescr.Caption     := FAppModules.Language.GetString('PlanningGUI.PeriodLengthDescr');
    FLblNrOfStartStoragePercs.Caption := FAppModules.Language.GetString('PlanningGUI.NrOfStartStoragePercs');
    FLblNrOfCurveSets.Caption         := FAppModules.Language.GetString('PlanningGUI.NrOfCurveSets');
    FNrOfCurveSetsDescr.Caption       := FAppModules.Language.GetString('PlanningGUI.NrOfCurveSetsDescr');
    FLblStartVolume.Caption           := FAppModules.Language.GetString('PlanningGUI.StartVolumes');
    FLblCurveSetIndicator.Caption     := FAppModules.Language.GetString('PlanningGUI.MonthCurveSet');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsDialog.RestoreColourState;
const OPNAME = 'TFMYieldCharacteristicsDialog.RestoreColourState';
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

procedure TFMYieldCharacteristicsDialog.AssignHelpContext;
const OPNAME = 'TFMYieldCharacteristicsDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
