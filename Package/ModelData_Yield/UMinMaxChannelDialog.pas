{******************************************************************************}
{*  UNIT      : Contains the class TMinMaxChannelDialog.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/03                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMinMaxChannelDialog;


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

  TMinMaxChannelDialog = class(TAbstractScrollablePanel)
  private
    FFlowContraintsGroup     : TGroupBox;
    FDistributionGroup       : TGroupBox;
    FFeatureNameLabel        : TLabel;
    FFeatureNameEdit         : TFieldEdit;
    //FFirmYieldAnalysisChkBox : TFieldChkBox;
    //FFirmYieldAnalysisLabel  : TLabel;
    FFlowConstraintLabel     : TLabel;
    FFlowConstraintGrid      : TFieldStringGrid;

    FDistributionGrid        : TFieldStringGrid;
    FDistributionTotalsGrid  : TFieldStringGrid;
    FDistributionGridHeader  : TFieldStringGrid;
    FTotalsGrid              : TFieldStringGrid;
    FTotalsLabel             : TLabel;
    FChannelGrowthLabel      : TLabel;

    FIncludeInWQConChkBox         : TFieldChkBox;
    FIncludeInBoundChkBox         : TFieldChkBox;
//    FDistributionTotalsLabel : TLabel;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetFlowConstraintGridCols(ACount : integer);
    property FeatureNameEdit         : TFieldEdit       read FFeatureNameEdit;
    //property FirmYieldAnalysisChkBox : TFieldChkBox     read FFirmYieldAnalysisChkBox;
    property FlowConstraintLabel     : TLabel           read FFlowConstraintLabel;
    property FlowConstraintGrid      : TFieldStringGrid read FFlowConstraintGrid;
    property DistributionGrid        : TFieldStringGrid read FDistributionGrid;
    property DistributionTotalsGrid  : TFieldStringGrid read FDistributionTotalsGrid;
    property DistributionGridHeader  : TFieldStringGrid read FDistributionGridHeader;
    property DistributionGroup       : TGroupBox        read FDistributionGroup;
    property TotalsGrid              : TFieldStringGrid read FTotalsGrid;
    property TotalsLabel             : TLabel           read FTotalsLabel;
    property GrowthWarningLabel      : TLabel           read FChannelGrowthLabel;
    property IncludeInWQConChkBox    : TFieldChkBox     read FIncludeInWQConChkBox;
    property IncludeInBoundChkBox    : TFieldChkBox     read FIncludeInBoundChkBox;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities, VCL.Grids, UStringGridWithCellChange;

{******************************************************************************}
{* TMinMaxChannelDialog                                                       *}
{******************************************************************************}

procedure TMinMaxChannelDialog.CreateMemberObjects;
const OPNAME = 'TMinMaxChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    FFlowContraintsGroup     := TGroupBox.Create(ControlsOwner);
    FFlowContraintsGroup.Parent     := ControlsParent;
    FDistributionGroup       := TGroupBox.Create(ControlsOwner);
    FDistributionGroup.Parent       := ControlsParent;

    //                                                                             Left  Top Width Height
    FFeatureNameLabel        := CreateFieldLabel                  (lOwner, FFlowContraintsGroup,  10,  30, 140,  21);
    FFeatureNameEdit         := CreateFieldEdit      (FAppModules, lOwner, FFlowContraintsGroup, 160,  30, 180,  21, 0, TRUE);

    {FFeatureNameLabel        := CreateFieldLabel                  (lOwner, FFlowContraintsGroup,  10,  10, 140,  21);
    FFeatureNameEdit         := CreateFieldEdit      (FAppModules, lOwner, FFlowContraintsGroup, 160,  10, 180,  21, 0, TRUE);
    FFirmYieldAnalysisChkBox := CreateFieldChkBox    (FAppModules, lOwner, FFlowContraintsGroup,   8,  35, 165,  21,  7, TRUE, taLeftJustify);
    FFirmYieldAnalysisLabel  := CreateFieldLabel                  (lOwner, FFlowContraintsGroup, 180,  35, 163,  21);
    }
    FFlowConstraintLabel     := CreateFieldLabel                  (lOwner, FFlowContraintsGroup,  10,  60, 140,  26);
    FFlowConstraintGrid      := CreateFieldStringGrid(FAppModules, lOwner, FFlowContraintsGroup, 160,  60, 369, 276,  0, TRUE);

    FDistributionGrid        := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FDistributionGrid.Parent := FDistributionGroup;
    FDistributionTotalsGrid  := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FDistributionTotalsGrid.Parent := FDistributionGroup;

    FDistributionGridHeader  := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FDistributionGridHeader.Parent := FDistributionGroup;

    FTotalsGrid              := CreateFieldStringGrid(FAppModules, lOwner, FFlowContraintsGroup, 160, 340, 369,  24,  0, TRUE);
    FTotalsLabel             := CreateFieldLabel                  (lOwner, FFlowContraintsGroup, 540, 340, 100,  21);
    FChannelGrowthLabel := TLabel.Create(ControlsOwner);
    FChannelGrowthLabel.parent := FDistributionGroup;
    FChannelGrowthLabel.WordWrap := True;

    FFlowConstraintLabel.WordWrap  := TRUE;
    with FFlowConstraintGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 6;
      RowCount         := 13;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 60;
    end;
    with FTotalsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 6;
      RowCount         := 1;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 60;
      Options          := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
      if (FAppModules.StudyArea.ModelCode = CPlanning) then
      begin
        FIncludeInWQConChkBox := CreateFieldChkBox(FAppModules, lOwner, FFlowContraintsGroup,  10,  FTotalsGrid.Top + FTotalsGrid.Height+5, 165, 21, 10, TRUE, taLeftJustify);
        FIncludeInBoundChkBox := CreateFieldChkBox(FAppModules, lOwner, FFlowContraintsGroup,  10,  FIncludeInWQConChkBox.Top + FIncludeInWQConChkBox.Height+5, 165, 21, 10, TRUE, taLeftJustify);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelDialog.Resize;
const OPNAME = 'TMinMaxChannelDialog.Resize';
begin
  inherited Resize;
  try
    FFlowContraintsGroup.Align := alLeft;
    if (FAppModules.Model.ModelName = CPlanning) then
    begin
      FFlowContraintsGroup.Width := self.ClientWidth div 2;
      FTotalsLabel.Caption       := '';         
    end
    else
      FFlowContraintsGroup.Width := self.ClientWidth - C_ControlOffset;

    FDistributionGroup.Align := alLeft;
    FDistributionGroup.Width := FFlowContraintsGroup.Width - (C_ControlOffset*2);

    FDistributionGrid.Top  :=  FeatureNameEdit.Top + FeatureNameEdit.Height;
    FDistributionGrid.Left := C_ControlOffset;
    FDistributionGrid.Height := (FDistributionGrid.DefaultRowHeight*FDistributionGrid.RowCount) + FDistributionGrid.DefaultRowHeight+(C_ControlOffset*2);

    FDistributionTotalsGrid.Top := FDistributionGrid.Top + FDistributionGrid.Height + C_LabelOffset;
    FDistributionTotalsGrid.Left := FDistributionGrid.Left;
    FDistributionTotalsGrid.Height := FDistributionTotalsGrid.DefaultRowHeight + C_ControlBorder;

    FDistributionGridHeader.Top := FDistributionGrid.Top - FDistributionGrid.DefaultRowHeight;
    FDistributionGridHeader.Left := FDistributionGrid.Left;
    FDistributionGridHeader.Height := FDistributionGridHeader.DefaultRowHeight;
    FDistributionGridHeader.ColAutoSizeIgnoreHeading := False;
    FChannelGrowthLabel.Top := FDistributionTotalsGrid.Top + FDistributionTotalsGrid.RowHeights[0] + C_ControlOffset;
    FChannelGrowthLabel.Left := FDistributionTotalsGrid.Left;
    FChannelGrowthLabel.Width := FDistributionTotalsGrid.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelDialog.Initialise: boolean;
const OPNAME = 'TMinMaxChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    FDistributionGridHeader.ScrollBars       := ssNone;
    FDistributionGridHeader.RowCount         := 2;
    FDistributionGridHeader.ColCount         := 2;
    FDistributionGridHeader.FixedCols        := 1;
    FDistributionGridHeader.FixedRows        := 1;
    FDistributionGridHeader.DefaultRowHeight := 20;
    FDistributionGridHeader.WrapHeaderText   := True;
    FDistributionGridHeader.IsRowEnabled[0]  := False;


    FDistributionGrid.ColCount         := 6;
    FDistributionGrid.RowCount         := 13;
    FDistributionGrid.FixedCols        := 1;
    FDistributionGrid.FixedRows        := 1;
    FDistributionGrid.DefaultRowHeight := 20;
    FDistributionGrid.DefaultColWidth  := 90;
    FDistributionGrid.WrapHeaderText   := True;

    FDistributionTotalsGrid.ScrollBars       := ssNone;
    FDistributionTotalsGrid.ColCount         := 6;
    FDistributionTotalsGrid.RowCount         := 1;
    FDistributionTotalsGrid.FixedCols        := 1;
    FDistributionTotalsGrid.FixedRows        := 0;
    FDistributionTotalsGrid.DefaultRowHeight := 20;
    FDistributionTotalsGrid.DefaultColWidth  := 90;
    FDistributionTotalsGrid.Options          := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
    
    FChannelGrowthLabel.Font.Color := clRed;
    FChannelGrowthLabel.Font.Size  := 12;
    FChannelGrowthLabel.AutoSize := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMinMaxChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption        := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    //FFirmYieldAnalysisChkBox.Caption := FAppModules.Language.GetString('TField.FirmYieldCalc') + ' ?';
    //FFirmYieldAnalysisLabel.Caption  := FAppModules.Language.GetString('Channel.InAdditionToMasterControl');
    FFlowConstraintLabel.Caption     := FAppModules.Language.GetString('TField.FlowConstraints') + ' :';
    FTotalsLabel.Caption             := FAppModules.Language.GetString('Channel.MillionM3PerYear');
    FChannelGrowthLabel.Caption      := FAppModules.Language.GetString('Channel.DistributionWarning');
    if (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      FIncludeInWQConChkBox.Caption := 'Include Water Quality ?';
      FIncludeInBoundChkBox.Caption := 'Include Bound Channel ?';
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelDialog.SetFlowConstraintGridCols(ACount : integer);
const OPNAME = 'TMinMaxChannelDialog.SetFlowConstraintGridCols';
var
  LIndex : integer;
begin
  try
    DistributionGridHeader.ColCount  := ACount;
    for LIndex := 0 to DistributionGridHeader.ColCount-1 do
      if (LIndex = 0) and (ACount > 1) then
        DistributionGridHeader.ColWidths[LIndex] := (DistributionGridHeader.Width div ACount)+ 17
      else
        DistributionGridHeader.ColWidths[LIndex] := (DistributionGridHeader.Width div ACount);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TMinMaxChannelDialog.RestoreColourState;
const OPNAME = 'TMinMaxChannelDialog.RestoreColourState';
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

procedure TMinMaxChannelDialog.AssignHelpContext;
const OPNAME = 'TMinMaxChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                     HC_MinimumAndMaximumFlowConstraints);
    SetControlHelpContext(FFeatureNameEdit,         HC_MinimumAndMaximumFlowConstraints);
    //SetControlHelpContext(FFirmYieldAnalysisChkBox, HC_MinimumAndMaximumFlowConstraints);
    SetControlHelpContext(FFlowConstraintGrid,      HC_MinimumAndMaximumFlowConstraints);
    SetControlHelpContext(FTotalsGrid,              HC_MinimumAndMaximumFlowConstraints);
  except on E: Exception do HandleError(E, OPNAME); end;
end;



end.
