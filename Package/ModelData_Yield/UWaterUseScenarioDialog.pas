{******************************************************************************}
{*  UNIT      : Contains the class TWaterUseScenarioDialog.                   *}
{*  AUTHOR    : Dziedzi Ramulondi                                             *}
{*  DATE      : 2004/10/20                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UWaterUseScenarioDialog;

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

  TWaterUseScenarioDialog = class(TAbstractScrollablePanel)
  protected
    FScenarioCountLabel      : TLabel;
    FScenarioCountEdit       : TFieldEdit;
    FProportionWaterUseLabel : TLabel;
    FHeadingGrid             : TFieldStringGrid;
    FProportionWaterUseGrid  : TFieldStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure OnColumnResize(ASender: TObject; ACol: integer);
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetScenarioCount(AChannelCount,AScenarioCount: integer);
    property ScenarioCountEdit : TFieldEdit   read FScenarioCountEdit;
    property ProportionWaterUseGrid   : TFieldStringGrid read FProportionWaterUseGrid;
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
{* TWaterUseScenarioDialog                                                    *}
{******************************************************************************}

procedure TWaterUseScenarioDialog.CreateMemberObjects;
const OPNAME = 'TWaterUseScenarioDialog.CreateMemberObjects';
begin
  inherited;
  try
    //                                                                 Left  Top  Width Height
    FScenarioCountLabel       := CreateFieldLabel   (ControlsOwner, ControlsParent,  10,  10, 110, 21);;
    FScenarioCountEdit        := CreateFieldEdit    (FAppModules, ControlsOwner, ControlsParent, 130,  10,  30,  21, 0, TRUE);
    FProportionWaterUseLabel  := CreateFieldLabel   (ControlsOwner, ControlsParent,  10,  40, 190, 40);
    FHeadingGrid              := CreateFieldStringGrid(FAppModules, ControlsOwner, ControlsParent,  10, 62, 500, 20, 0, False);
    FProportionWaterUseGrid   := CreateFieldStringGrid(FAppModules, ControlsOwner, ControlsParent,  10, 80, 500, 176, 1, TRUE);
    FProportionWaterUseLabel.Alignment := taCenter;
    with FHeadingGrid do
    begin
      Alignment        := taCenter;
      ScrollBars       := ssNone;
      BorderStyle      := bsNone;
      ColCount         := 3;
      RowCount         := 2;
      FixedRows        := 1;
      FixedCols        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 45;
      ColWidths[0]     := 241;
      ColWidths[1]     := 120;
      ColWidths[2]     := 140;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    end;
    with FProportionWaterUseGrid do
    begin
      BorderStyle      := bsNone;
      ColCount         := 13;
      RowCount         := 2;
      FixedRows        := 1;
      FixedCols        := 3;
      DefaultRowHeight := 20;
      DefaultColWidth  := 32;
      ColWidths[0]     := 40;
      ColWidths[1]     := 200;
      ColWidths[2]     := 120;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FProportionWaterUseGrid.AutoSizeFixedCols := True;
    FProportionWaterUseGrid.OnColumnResize := Self.OnColumnResize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioDialog.Resize;
const OPNAME = 'TWaterUseScenarioDialog.Resize';
var
  LFixedWidth,
  LIndex,
  LWidth: integer;
begin
  inherited Resize;
  try
    FHeadingGrid.Align  := alNone;
    FProportionWaterUseGrid.Align  := alNone;
    FProportionWaterUseLabel.Align  := alNone;

    FProportionWaterUseGrid.Height := ControlsParent.ClientHeight -
                                      FHeadingGrid.Height -
                                      FProportionWaterUseLabel.Height -
                                      (FScenarioCountEdit.Top + FScenarioCountEdit.Height + 10);
    FProportionWaterUseGrid.Align  := alBottom;
    FHeadingGrid.Align  := alBottom;
    FProportionWaterUseLabel.Align  := alBottom;
    LFixedWidth := FHeadingGrid.ColWidths[0] +  FHeadingGrid.ColWidths[1];
    if((FHeadingGrid.ClientWidth - LFixedWidth) > 0) then
      FHeadingGrid.ColWidths[2]       := FHeadingGrid.ClientWidth - LFixedWidth - 2;
    if((FProportionWaterUseGrid.ClientWidth - LFixedWidth) > 0) then
    begin
      LWidth := (FProportionWaterUseGrid.ClientWidth - LFixedWidth) div
                (FProportionWaterUseGrid.ColCount -3);
      LWidth := LWidth -5;
      for LIndex := 3 to FProportionWaterUseGrid.ColCount -1 do
        FProportionWaterUseGrid.ColWidths[LIndex] := LWidth;

    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioDialog.Initialise: boolean;
const OPNAME = 'TWaterUseScenarioDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FScenarioCountEdit.Text := '';
    SetScenarioCount(0,0);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioDialog.SetScenarioCount(AChannelCount,AScenarioCount: integer);
const OPNAME = 'TWaterUseScenarioDialog.SetScenarioCount';
var
  LIndex: integer;
begin
  try
    if(AChannelCount > 0) and (AScenarioCount > 0) then
    begin
      if(FProportionWaterUseGrid.RowCount <> (AChannelCount + 1)) then
        FProportionWaterUseGrid.RowCount := AChannelCount + 1;
      if (FProportionWaterUseGrid.ColCount <> (AScenarioCount + 3)) then
        FProportionWaterUseGrid.ColCount := AScenarioCount + 3;
      FProportionWaterUseGrid.Options  := FProportionWaterUseGrid.Options + [goEditing];
      FProportionWaterUseGrid.Enabled  := True;
    end
    else
    begin
      if(FProportionWaterUseGrid.RowCount <> 2) then
        FProportionWaterUseGrid.RowCount := 2;
      if(FProportionWaterUseGrid.ColCount <> 4) then
        FProportionWaterUseGrid.ColCount := 4;
      FProportionWaterUseGrid.Options  := FProportionWaterUseGrid.Options - [goEditing];
      FProportionWaterUseGrid.Enabled  := False;
    end;

    for LIndex := 0 to FProportionWaterUseGrid.RowCount -1 do
      FProportionWaterUseGrid.Rows[LIndex].Clear;

    LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUseScenarioDialog.LanguageHasChanged: boolean;
const OPNAME = 'TWaterUseScenarioDialog.LanguageHasChanged';
var
  lLanguage : TAbstractLanguage;
  LIndex: integer;
begin
  Result := inherited LanguageHasChanged;
  try
    lLanguage := FAppModules.Language;
    FScenarioCountLabel.Caption    := lLanguage.GetString('TField.WaterDemandScenarioCount') + ' :';
    FProportionWaterUseLabel.Caption  := lLanguage.GetString('WaterUse.WaterUse');
    FHeadingGrid.Cells[0,0] := lLanguage.GetString('WaterUse.Channel');
    FHeadingGrid.Cells[1,0] := lLanguage.GetString('WaterUse.Category');
    FHeadingGrid.Cells[2,0] := lLanguage.GetString('WaterUse.Scenario');

    FProportionWaterUseGrid.Cells[0,0] := lLanguage.GetString('WaterUse.No');
    FProportionWaterUseGrid.Cells[1,0] := lLanguage.GetString('WaterUse.Name');
    FProportionWaterUseGrid.Cells[2,0] := lLanguage.GetString('WaterUse.Name');
    for LIndex := 3 to FProportionWaterUseGrid.ColCount -1 do
      FProportionWaterUseGrid.Cells[LIndex,0] := IntToStr(LIndex-2);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioDialog.AssignHelpContext;
const OPNAME = 'TWaterUseScenarioDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                    HC_WaterResourcesYieldModel);
    SetControlHelpContext(FScenarioCountEdit,      HC_WaterResourcesYieldModel);
    SetControlHelpContext(FHeadingGrid,            HC_WaterResourcesYieldModel);
    SetControlHelpContext(FProportionWaterUseGrid, HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUseScenarioDialog.OnColumnResize(ASender: TObject; ACol: integer);
const OPNAME = 'TWaterUseScenarioDialog.OnColumnResize';
var
  LWidth : integer;
begin
  try
     if(ACol in [0,1,2]) then
     begin
       if(ACol in [0,1]) then
       begin
        LWidth := FHeadingGrid.ColWidths[0] - (FProportionWaterUseGrid.ColWidths[0] + FProportionWaterUseGrid.ColWidths[1]);
        FHeadingGrid.ColWidths[0]  := FProportionWaterUseGrid.ColWidths[0] + FProportionWaterUseGrid.ColWidths[1]+1;
        FHeadingGrid.ColWidths[2]  := FHeadingGrid.ColWidths[2] + LWidth - 1;
       end
       else
       begin
        LWidth := FHeadingGrid.ColWidths[1] - FProportionWaterUseGrid.ColWidths[2];
        FHeadingGrid.ColWidths[1]  := FProportionWaterUseGrid.ColWidths[2];
        FHeadingGrid.ColWidths[2]  := FHeadingGrid.ColWidths[2] + LWidth;
       end;

     end;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
