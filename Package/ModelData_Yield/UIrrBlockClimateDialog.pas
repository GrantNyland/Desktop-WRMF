{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockClimateDialog                         *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockClimateDialog;

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

  TIrrBlockClimateDialog = class(TAbstractScrollablePanel)
  protected
    FWidthPanel                                     : TPanel;
    FgboxProperties                                 : TGroupBox;
    FIrrigationBlockCatchmentFileNameLabel          : TLabel;
    FIrrigationBlockCatchmentMAPLabel               : TLabel;
    FIrrigationBlockCatchmentMAPEdit                : TFieldEdit;
    FIrrigationBlockCatchmentFileNameEdit           : TFieldEdit;

    FIrrigationBlockRainCatchmentScalingFactorLabel : TLabel;
    FIrrigationBlockRainCatchmentScalingFactorEdit  : TFieldEdit;

    FgboxRainfallFactor                             : TGroupBox;
    FIrrigationBlockRainfallFactorGrid              : TFieldStringGrid;

    //FgboxMaxMeanRainfallFactor                      : TGroupBox;
    //FgrdMaxMeanRainfallFactor                       : TFieldStringGrid;

    FgboxPanEvaporation                             : TGroupBox;
    FIrrigationBlockPanEvaporationGrid              : TFieldStringGrid;

    FgboxAPanConvFactor                             : TGroupBox;
    FIrrigationBlockAPanConvFactorGrid              : TFieldStringGrid;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    property IrrigationBlockCatchmentMAPEdit                  : TFieldEdit      read FIrrigationBlockCatchmentMAPEdit;
    property IrrigationBlockCatchmentFileNameEdit             : TFieldEdit      read FIrrigationBlockCatchmentFileNameEdit;
    property IrrigationBlockRainCatchmentScalingFactorEdit    : TFieldEdit      read FIrrigationBlockRainCatchmentScalingFactorEdit;

    property IrrigationBlockRainfallFactorGrid : TFieldStringGrid read FIrrigationBlockRainfallFactorGrid;
    property IrrigationBlockPanEvaporationGrid : TFieldStringGrid read FIrrigationBlockPanEvaporationGrid;
    property IrrigationBlockAPanConvFactorGrid : TFieldStringGrid read FIrrigationBlockAPanConvFactorGrid;
    //property grdMaxMeanRainfallFactor          : TFieldStringGrid read FgrdMaxMeanRainfallFactor;
    property gboxRainfallFactor                : TGroupBox        read FgboxRainfallFactor;
    property gboxPanEvaporation                : TGroupBox        read FgboxPanEvaporation;
    property gboxAPanConvFactor                : TGroupBox        read FgboxAPanConvFactor;
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
{* TIrrBlockClimateDialog                                                         *}
{******************************************************************************}

procedure TIrrBlockClimateDialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockClimateDialog.CreateMemberObjects';
var
  lOwner    : TComponent;
  lParent   : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FWidthPanel                                       := TPanel.Create(lOwner);
    FWidthPanel.Parent                                := lParent;
    FWidthPanel.Align                                 := alLeft;
    FWidthPanel.Width                                 := 800;
    //FWidthPanel.BevelOuter                            := bvNone;
    FWidthPanel.BevelInner                            := bvLowered;

    FgboxProperties                                   := TGroupBox.Create(lOwner);
    FgboxProperties.Parent                            := FWidthPanel;
    FgboxProperties.Align                             := alTop;
    FgboxProperties.Height                            := 100;
    lParent                                           := FgboxProperties;

    FIrrigationBlockCatchmentFileNameLabel            := CreateFieldLabel(lOwner, lParent, 10,  15,  420, 21);
    FIrrigationBlockCatchmentFileNameEdit             := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 15, 200, 21,11, TRUE);

    FIrrigationBlockCatchmentMAPLabel                 := CreateFieldLabel(lOwner, lParent, 10, 40, 200, 21);
    FIrrigationBlockCatchmentMAPEdit                  := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 40, 100, 20, 8, TRUE);

    FIrrigationBlockRainCatchmentScalingFactorLabel   := CreateFieldLabel(lOwner, lParent, 10, 65,  420, 21);
    FIrrigationBlockRainCatchmentScalingFactorEdit    := CreateFieldEdit(FAppModules, lOwner, lParent, 230,  65,  100,  20, 8, TRUE);

    //rainfall factor
    FgboxRainfallFactor                                     := TGroupBox.Create(lOwner);
    FgboxRainfallFactor.Parent                              := FWidthPanel;
    FgboxRainfallFactor.Align                               := alTop;
    FgboxRainfallFactor.Height                              := 100;
    lParent                                                 := FgboxRainfallFactor;

    FIrrigationBlockRainfallFactorGrid                      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, 1, 100, 100, 3, TRUE);
    FIrrigationBlockRainfallFactorGrid.Align                := alClient;
    FIrrigationBlockRainfallFactorGrid.ScrollBars           := ssNone;
    FIrrigationBlockRainfallFactorGrid.FixedCols            := 0;
    FIrrigationBlockRainfallFactorGrid.ColCount             := 12;
    FIrrigationBlockRainfallFactorGrid.DefaultColWidth      := 60;
    FIrrigationBlockRainfallFactorGrid.DefaultRowHeight     := 20;
    FIrrigationBlockRainfallFactorGrid.RowCount             := 2;
    FIrrigationBlockRainfallFactorGrid.Options              := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    {//Max Mean Rainfall Factor
    //if IrrigationBlockType = 4 then
    //begin
    FgboxMaxMeanRainfallFactor                              := TGroupBox.Create(lOwner);
    FgboxMaxMeanRainfallFactor.Parent                       := FWidthPanel;
    FgboxMaxMeanRainfallFactor.Align                        := alTop;
    FgboxMaxMeanRainfallFactor.Height                       := 100;
    FgboxMaxMeanRainfallFactor.Width                        := 110;
    lParent                                                 := FgboxMaxMeanRainfallFactor;

    FgrdMaxMeanRainfallFactor                               := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, 1, 100, 100, 3, TRUE);
    FgrdMaxMeanRainfallFactor.Align                         := alClient;
    FgrdMaxMeanRainfallFactor.ScrollBars                    := ssNone;
    FgrdMaxMeanRainfallFactor.FixedCols                     := 0;
    FgrdMaxMeanRainfallFactor.ColCount                      := 12;
    FgrdMaxMeanRainfallFactor.DefaultColWidth               := 60;
    FgrdMaxMeanRainfallFactor.DefaultRowHeight              := 20;
    FgrdMaxMeanRainfallFactor.RowCount                      := 2;
    FgrdMaxMeanRainfallFactor.Options                       := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    //end;}

    //Pan evaporation
    FgboxPanEvaporation                                     := TGroupBox.Create(lOwner);
    FgboxPanEvaporation.Parent                              := FWidthPanel;
    FgboxPanEvaporation.Align                               := alTop;
    FgboxPanEvaporation.Height                              := 100;
    FgboxPanEvaporation.Width                               := 110;
    lParent                                                 := FgboxPanEvaporation;

    FIrrigationBlockPanEvaporationGrid  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, 1, 100, 100, 3, TRUE);
    FIrrigationBlockPanEvaporationGrid.Align                 := alClient;
    FIrrigationBlockPanEvaporationGrid.ScrollBars            := ssNone;
    FIrrigationBlockPanEvaporationGrid.FixedCols             := 0;
    FIrrigationBlockPanEvaporationGrid.ColCount              := 12;
    FIrrigationBlockPanEvaporationGrid.DefaultColWidth       := 60;
    FIrrigationBlockPanEvaporationGrid.DefaultRowHeight      := 20;
    FIrrigationBlockPanEvaporationGrid.RowCount              := 2;
    FIrrigationBlockPanEvaporationGrid.Options               := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    //APan conversion factor
    FgboxAPanConvFactor                                     := TGroupBox.Create(lOwner);
    FgboxAPanConvFactor.Parent                              := FWidthPanel;
    FgboxAPanConvFactor.Align                               := alTop;
    FgboxAPanConvFactor.Height                              := 100;
    FgboxAPanConvFactor.Width                               := 110;
    lParent                                                 := FgboxAPanConvFactor;

    FIrrigationBlockAPanConvFactorGrid                      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, 1, 100, 100, 3, TRUE);
    FIrrigationBlockAPanConvFactorGrid.Align                := alClient;
    FIrrigationBlockAPanConvFactorGrid.ScrollBars           := ssNone;
    FIrrigationBlockAPanConvFactorGrid.FixedCols            := 0;
    FIrrigationBlockAPanConvFactorGrid.ColCount             := 12;
    FIrrigationBlockAPanConvFactorGrid.DefaultColWidth      := 60;
    FIrrigationBlockAPanConvFactorGrid.DefaultRowHeight     := 20;
    FIrrigationBlockAPanConvFactorGrid.RowCount             := 2;
    FIrrigationBlockAPanConvFactorGrid.Options              := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockClimateDialog.Resize;
const OPNAME = 'TIrrBlockClimateDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockClimateDialog.Initialise: boolean;
const OPNAME = 'TIrrBlockClimateDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockClimateDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockClimateDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FIrrigationBlockCatchmentFileNameLabel.Caption          := FAppModules.Language.GetString('TField.IrrigationBlockCatchmentFileName');
    FIrrigationBlockCatchmentMAPLabel.Caption               := FAppModules.Language.GetString('TField.IrrigationBlockMAP');
    FIrrigationBlockRainCatchmentScalingFactorLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockRainCatchmentScalingFactor');

    {FgboxProperties.Caption                                 := 'Properties';
    FgboxRainfallFactor.Caption                             := FAppModules.Language.GetString('TField.IrrigationBlockRainfallFactor');
    //FgboxMaxMeanRainfallFactor.Caption                      := 'Max Mean Rainfall Factors';
    FgboxPanEvaporation.Caption                             := FAppModules.Language.GetString('TField.IrrigationBlockPanEvaporation');
    FgboxAPanConvFactor.Caption                             := FAppModules.Language.GetString('TField.IrrigationBlockAPanConvFactor'); }
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockClimateDialog.RestoreColourState;
const OPNAME = 'TIrrBlockClimateDialog.RestoreColourState';
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

procedure TIrrBlockClimateDialog.AssignHelpContext;
const OPNAME = 'TIrrBlockClimateDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                                           HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockCatchmentMAPEdit,               HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockCatchmentFileNameEdit,          HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockRainCatchmentScalingFactorEdit, HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockRainfallFactorGrid,             HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockPanEvaporationGrid,             HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockAPanConvFactorGrid,             HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
