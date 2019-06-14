//
//
//  UNIT      : Contains the class TReservoirPhysicalCharacteristicsDialog.
//  AUTHOR    : Valentino Naicker (arivia.kom)
//  DATE      : 2003/06/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UReservoirPhysicalCharacteristicsDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Grids,
  VCLTee.Chart,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TReservoirPhysicalCharacteristicsDialog = class(TAbstractScrollablePanel)
  protected
    FGbxReservoirData : TGroupBox;

    // Labels
    FLblAreaWhenFull : TLabel;
    FLblStartingStorage : TLabel;
    FLblFullStorageLevel : TLabel;
    FLblDeadStorageLevel : TLabel;
    FLblBottomOfReservoir : TLabel;

    // Field Edit boxes
    FEdtAreaWhenFull : TFieldEdit;
    FEdtStartingStorage : TFieldEdit;
    FEdtFullStorageLevel : TFieldEdit;
    FEdtDeadStorageLevel : TFieldEdit;
    FEdtBottomOfReservoir : TFieldEdit;

    FGrdElevation : TFieldStringGrid;
    FChtElevation : TAbstractChart;

    FGbxButtons   : TGroupBox;
    FBtnInsertRow : TFieldBitBtn;
    FBtnDeleteRow : TFieldBitBtn;
    FDamBasinSurveyBtn : TFieldButton;
    FStartingStorageBtn : TFieldBitBtn;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;

  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function SetRowCount(ARowCount: integer): boolean;
    procedure RestoreColourState; override;
    procedure ResetButtonState;
    property EdtAreaWhenFull : TFieldEdit read FEdtAreaWhenFull;
    property EdtStartingStorage : TFieldEdit read FEdtStartingStorage;
    property EdtFullStorageLevel : TFieldEdit read FEdtFullStorageLevel;
    property EdtDeadStorageLevel : TFieldEdit read FEdtDeadStorageLevel;
    property EdtBottomOfReservoir : TFieldEdit read FEdtBottomOfReservoir;

    property GrdElevation : TFieldStringGrid read FGrdElevation;
    property ChtElevation : TAbstractChart read FChtElevation;

    property BtnInsertRow :TFieldBitBtn read FBtnInsertRow;
    property BtnDeleteRow :TFieldBitBtn read FBtnDeleteRow;
    property DamBasinSurveyBtn : TFieldButton read FDamBasinSurveyBtn;
    property StartingStorageBtn : TFieldBitBtn read FStartingStorageBtn;

  end;

implementation

uses
  VCL.Forms,
  math,
  VCL.Buttons,
  UConstants,
  SysUtils,
  VCL.Graphics,
  UHelpContexts,
  //UDBConstants,
  UErrorHandlingOperations;

{ TReservoirPhysicalCharacteristicsDialog }

procedure TReservoirPhysicalCharacteristicsDialog.CreateMemberObjects;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    // Create
    FGbxReservoirData := TGroupBox.Create(ControlsOwner);

    FLblAreaWhenFull := TLabel.Create(ControlsOwner);
    FLblStartingStorage := TLabel.Create(ControlsOwner);
    FLblFullStorageLevel := TLabel.Create(ControlsOwner);
    FLblDeadStorageLevel := TLabel.Create(ControlsOwner);
    FLblBottomOfReservoir := TLabel.Create(ControlsOwner);

    FEdtAreaWhenFull := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEdtStartingStorage := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEdtFullStorageLevel := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEdtDeadStorageLevel := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEdtBottomOfReservoir := TFieldEdit.Create(ControlsOwner, FAppModules);

    FGrdElevation := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FChtElevation := TAbstractChart.Create(ControlsOwner, FAppModules);

    FGbxButtons   := TGroupBox.Create(ControlsOwner);
    FBtnInsertRow := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnDeleteRow := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FDamBasinSurveyBtn := TFieldButton.Create(ControlsOwner, FAppModules, 'DamBasinSurvey');
    FStartingStorageBtn := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    // Set Parent
    FGbxReservoirData.Parent := ControlsParent;

    FLblAreaWhenFull.Parent := FGbxReservoirData;
    FLblStartingStorage.Parent := FGbxReservoirData;
    FLblFullStorageLevel.Parent := FGbxReservoirData;
    FLblDeadStorageLevel.Parent := FGbxReservoirData;
    FLblBottomOfReservoir.Parent := FGbxReservoirData;
    FStartingStorageBtn.Parent := FGbxReservoirData;

    FEdtAreaWhenFull.Parent := FGbxReservoirData;
    FEdtStartingStorage.Parent := FGbxReservoirData;
    FEdtFullStorageLevel.Parent := FGbxReservoirData;
    FEdtDeadStorageLevel.Parent := FGbxReservoirData;
    FEdtBottomOfReservoir.Parent := FGbxReservoirData;

    FGrdElevation.Parent := ControlsParent;
    FGrdElevation.Visible := True;
    FChtElevation.Parent := ControlsParent;

    FGbxButtons.Parent   := ControlsParent;
    FBtnInsertRow.Parent := FGbxButtons;
    FBtnDeleteRow.Parent := FGbxButtons;
    FDamBasinSurveyBtn.Parent := FGbxButtons;
    
    // Set Label Height
    FLblAreaWhenFull.Height := C_LabelHeight;
    FLblStartingStorage.Height := C_LabelHeight;
    FLblFullStorageLevel.Height := C_LabelHeight;
    FLblDeadStorageLevel.Height := C_LabelHeight;
    FLblBottomOfReservoir.Height := C_LabelHeight;

    // Set Field Edit Box Height
    FEdtAreaWhenFull.Height := C_EditBoxHeight;
    FEdtStartingStorage.Height := C_EditBoxHeight;
    FEdtFullStorageLevel.Height := C_EditBoxHeight;
    FEdtDeadStorageLevel.Height := C_EditBoxHeight;
    FEdtBottomOfReservoir.Height := C_EditBoxHeight;

    // Set default Grid properties
    FGrdElevation.RowCount := 16;
    FGrdElevation.ColCount := 3;
    FGrdElevation.FixedRows := 1;
    FGrdElevation.FixedCols := 0;
    FGrdElevation.DefaultRowHeight := 18;
    FGrdElevation.ScrollBars := ssBoth;
    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsDialog.DestroyMemberObjects;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsDialog.Initialise: boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.Initialise';
begin
  Result := inherited Initialise;

  try
    FEdtAreaWhenFull.Text := '';
    FEdtStartingStorage.Text := '';
    FEdtFullStorageLevel.Text := '';
    FEdtDeadStorageLevel.Text := '';
    FEdtBottomOfReservoir.Text := '';

    FChtElevation.LeftAxis.AxisValuesFormat   := '###0.###';
    FChtElevation.RightAxis.AxisValuesFormat  := '###0.###';
    FChtElevation.BottomAxis.AxisValuesFormat := '###0.###';
    FChtElevation.TopAxis.AxisValuesFormat    := '###0.###';

    FBtnInsertRow.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORADDSERIES');
    FBtnDeleteRow.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORREMOVESERIES');
    FBtnInsertRow.Enabled := False;
    FBtnDeleteRow.Enabled := False;

    FStartingStorageBtn.Glyph.LoadFromResourceName(HImagesInstance, 'CALCULATOR');
    FStartingStorageBtn.Layout := blGlyphRight;
    FStartingStorageBtn.NumGlyphs := 2;
    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsDialog.Resize;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.Resize';
var
  LClientWidth : integer;
  LEditBoxWidth : integer;
begin
  // Call the ancestor.
  inherited Resize;
  try
    // Check that construction is complete before laying out controls.

    if Assigned(FChtElevation) then
    begin
      LClientWidth := Self.ClientWidth;

      FGbxReservoirData.Left := C_ControlBorder;
      FGbxReservoirData.Top := C_ControlBorder;
      FGbxReservoirData.Width := Trunc(LClientWidth * 0.4) - 2 * C_ControlBorder;
      FGbxReservoirData.Height := 5 * (FEdtAreaWhenFull.Height + C_ControlBorder) + 3 * C_ControlBorder;

      // Set Label left
      FLblAreaWhenFull.Left := C_ControlBorder;
      FLblStartingStorage.Left := C_ControlBorder;
      FLblFullStorageLevel.Left := C_ControlBorder;
      FLblDeadStorageLevel.Left := C_ControlBorder;
      FLblBottomOfReservoir.Left := C_ControlBorder;

      // Set Edit Box Left
      FEdtAreaWhenFull.Left := Trunc(0.65 * FGbxReservoirData.Width) - 2 * C_ControlBorder;
      FEdtStartingStorage.Left := Trunc(0.65 * FGbxReservoirData.Width) - 2 * C_ControlBorder;
      FEdtFullStorageLevel.Left := Trunc(0.65 * FGbxReservoirData.Width) - 2 * C_ControlBorder;
      FEdtDeadStorageLevel.Left := Trunc(0.65 * FGbxReservoirData.Width) - 2 * C_ControlBorder;
      FEdtBottomOfReservoir.Left := Trunc(0.65 * FGbxReservoirData.Width)- 2 * C_ControlBorder;


      // Set Edit Box width
      LEditBoxWidth := Trunc(0.3 * FGbxReservoirData.Width) - 2 * C_ControlBorder;
      FEdtAreaWhenFull.Width := LEditBoxWidth;
      FEdtStartingStorage.Width := LEditBoxWidth;
      FEdtFullStorageLevel.Width := LEditBoxWidth;
      FEdtDeadStorageLevel.Width := LEditBoxWidth;
      FEdtBottomOfReservoir.Width := LEditBoxWidth;

      // Set Edit Box top
      FEdtAreaWhenFull.Top := FGbxReservoirData.Top + C_ControlBorder;
      FEdtStartingStorage.Top := FEdtAreaWhenFull.Top + C_EditBoxHeight + C_ControlBorder;
      FEdtFullStorageLevel.Top := FEdtStartingStorage.Top  + C_EditBoxHeight + C_ControlBorder;
      FEdtDeadStorageLevel.Top := FEdtFullStorageLevel.Top  + C_EditBoxHeight + C_ControlBorder;
      FEdtBottomOfReservoir.Top := FEdtDeadStorageLevel.Top  + C_EditBoxHeight + C_ControlBorder;

      // Set Label Top
      FLblAreaWhenFull.Top := FEdtAreaWhenFull.Top + (C_EditBoxHeight - C_LabelHeight) div 2;
      FLblStartingStorage.Top := FEdtStartingStorage.Top + (C_EditBoxHeight - C_LabelHeight) div 2;
      FLblFullStorageLevel.Top := FEdtFullStorageLevel.Top + (C_EditBoxHeight - C_LabelHeight) div 2;
      FLblDeadStorageLevel.Top := FEdtDeadStorageLevel.Top + (C_EditBoxHeight - C_LabelHeight) div 2;
      FLblBottomOfReservoir.Top := FEdtBottomOfReservoir.Top + (C_EditBoxHeight - C_LabelHeight) div 2;

      // Position the Chart
      VCL.Controls.TControl(FChtElevation).Left := C_ControlBorder + Trunc(0.4 * LClientWidth) + 2 * C_ControlBorder;
      VCL.Controls.TControl(FChtElevation).Top := C_ControlBorder;

      // Resize the Chart
      FChtElevation.Width := LClientWidth - VCL.Controls.TControl(FChtElevation).Left - 2 * C_ControlBorder;
      FChtElevation.Height := Self.ClientHeight - 6 * C_ControlBorder;

      if FScrollBox.VertScrollBar.IsScrollBarVisible then
        FChtElevation.Width := FChtElevation.Width - 3 * C_ControlBorder;

      FGbxButtons.Left   := FGbxReservoirData.Left;
      FGbxButtons.Width  := FGbxReservoirData.Width;
      FGbxButtons.Top    := FGbxReservoirData.Top + FGbxReservoirData.Height;
      FGbxButtons.Height := FGbxReservoirData.Height div 4;

      FBtnInsertRow.Left   :=  C_ControlBorder;
      FBtnInsertRow.Width  := (FGbxButtons.Width - (4 * C_ControlBorder)) div 3;
      FBtnInsertRow.Height := FGbxButtons.Height - (3 * C_ControlBorder);
      FBtnInsertRow.Top    := 2 * C_ControlBorder;

      FBtnDeleteRow.Width  := FBtnInsertRow.Width;
      FBtnDeleteRow.Top    := FBtnInsertRow.Top;
      FBtnDeleteRow.Height := FBtnInsertRow.Height;
      FBtnDeleteRow.Left   := FBtnInsertRow.Left + FBtnInsertRow.Width + C_ControlBorder;

      FDamBasinSurveyBtn.Width  := FBtnInsertRow.Width;
      FDamBasinSurveyBtn.Top    := FBtnInsertRow.Top;
      FDamBasinSurveyBtn.Height := FBtnInsertRow.Height;
      FDamBasinSurveyBtn.Left   := FBtnDeleteRow.Left + FBtnDeleteRow.Width + C_ControlBorder;
{
      FBtnInsertRow.Left   :=  C_ControlBorder*2;
      FBtnInsertRow.Width  := (FGbxButtons.Width div 2)- (4 * C_ControlBorder);
      FBtnInsertRow.Height := FGbxButtons.Height - (3 * C_ControlBorder);
      FBtnInsertRow.Top    := 2* C_ControlBorder;

      FBtnDeleteRow.Width  := FBtnInsertRow.Width;
      FBtnDeleteRow.Top    := FBtnInsertRow.Top;
      FBtnDeleteRow.Height := FBtnInsertRow.Height;
      FBtnDeleteRow.Left   := FGbxButtons.Width - (FBtnDeleteRow.Width + (2 * C_ControlBorder));
}
      // Position the string Grid
      FGrdElevation.Left := C_ControlBorder;
      FGrdElevation.Top := FGbxButtons.Top + FGbxButtons.Height;

      // Resize the Grid
      FGrdElevation.Width := FGbxButtons.Width;
      FGrdElevation.Height := VCL.Controls.TControl(FChtElevation).Height - (FGbxButtons.Top + FGbxButtons.Height);
      //FGrdElevation.Height := (FGrdElevation.DefaultRowHeight + 2) * 16;

      FGrdElevation.ColWidths[0] := Trunc((0.4 * LClientWidth) / 3) - 2 * C_ControlBorder;
      FGrdElevation.ColWidths[1] := Trunc((0.4 * LClientWidth) / 3) - 2 * C_ControlBorder;
      FGrdElevation.ColWidths[2] := Trunc((0.4 * LClientWidth) / 3) - 2 * C_ControlBorder;

      FStartingStorageBtn.Width  := 30;
      FStartingStorageBtn.Left   := FEdtStartingStorage.Left + FEdtStartingStorage.Width + 2;
      FStartingStorageBtn.Top    := FEdtStartingStorage.Top;
      FStartingStorageBtn.Height := FEdtStartingStorage.Height;

    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    FLblAreaWhenFull.Caption := FAppModules.Language.GetString('LabelText.AreaWhenFull');
    FLblStartingStorage.Caption := FAppModules.Language.GetString('LabelText.StartingStorage');
    FLblFullStorageLevel.Caption := FAppModules.Language.GetString('LabelText.FullStorageLevel');
    FLblDeadStorageLevel.Caption := FAppModules.Language.GetString('LabelText.DeadStorageLevel');
    FLblBottomOfReservoir.Caption := FAppModules.Language.GetString('LabelText.BottomOfReservoir');

    FGrdElevation.Cells[0,0] := FAppModules.Language.GetString('GridHeading.Elevation');
    FGrdElevation.Cells[1,0] := FAppModules.Language.GetString('GridHeading.Volume');
    FGrdElevation.Cells[2,0] := FAppModules.Language.GetString('GridHeading.SurfaceArea');

    FChtElevation.BottomAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.Volume');
    FChtElevation.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.Elevation');
    FChtElevation.TopAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.SurfaceArea');
    FChtElevation.RightAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.Depth');

    FBtnInsertRow.Caption := FAppModules.Language.GetString('ButtonCaption.InsertRow');
    FBtnDeleteRow.Caption := FAppModules.Language.GetString('ButtonCaption.DeleteRow');
    FDamBasinSurveyBtn.Caption := FAppModules.Language.GetString('ButtonCaption.DamBasinSurvey');
    FStartingStorageBtn.Caption := '%';
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsDialog.SetRowCount(ARowCount: integer): boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.SetRowCount';
var
 LRows: integer;
begin
  Result := False;
  try
    LRows := Max(2,ARowCount);
    FGrdElevation.RowCount := LRows;
    FGrdElevation.ColCount := 3;
    FGrdElevation.FixedRows := 1;
    FGrdElevation.FixedCols := 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsDialog.RestoreColourState;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.RestoreColourState';
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
procedure TReservoirPhysicalCharacteristicsDialog.ResetButtonState;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.ResetButtonState';
begin
  try
    BtnInsertRow.Enabled := False;
    BtnDeleteRow.Enabled := False;
    if(FAppModules.User.UserRights in CUR_EditData) and
      (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      BtnInsertRow.Enabled := (GrdElevation.RowCount < 16);
      BtnDeleteRow.Enabled := (goEditing in GrdElevation.Options) and (GrdElevation.Col >= 0) and (GrdElevation.Row >= 0);
      BtnDeleteRow.Enabled := (GrdElevation.RowCount >= 5);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsDialog.AssignHelpContext;
const OPNAME = 'TReservoirPhysicalCharacteristicsDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                  HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FGbxReservoirData,     HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FEdtFullStorageLevel,  HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FEdtDeadStorageLevel,  HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FEdtBottomOfReservoir, HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FGrdElevation,         HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(VCL.Controls.TControl(FChtElevation),         HC_PhysicalReservoirCharacteristics);

    SetControlHelpContext(FEdtAreaWhenFull,      HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FEdtStartingStorage,   HC_PhysicalReservoirCharacteristics);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.







