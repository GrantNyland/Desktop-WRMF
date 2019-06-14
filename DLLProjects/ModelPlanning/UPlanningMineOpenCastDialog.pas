unit UPlanningMineOpenCastDialog;

interface
uses
  Classes,
  Vcl.Controls,
  VCL.stdCtrls,
  UDataEditComponent,
  ULoadGenerationControl,
  UGrowthFactorControl,
  UAbstractionControl,
  UMiningOpenCastPitDialog;
type
  TPlanningMineOpenCastDialog = class(TMiningOpenCastPitDialog)
    protected
      FLoadGenerationControl : TGroupBox;
      FLoadGenerationTypeLabel : TLabel;
      FLoadGenerationTypeCombo : TFieldComboBox;
      FStdDeviationLabel : TLabel;
      FStdDeviationEdit : TFieldEdit;
      FFactorAndMeanGrid : TFieldStringGrid;



      FBtnAddGrowthFactors : TFieldBitBtn;
      FBtnDeleteGrowthFactors : TFieldBitBtn;
      FBtnAddLoadGeneration : TFieldBitBtn;
      FBtnDeleteLoadGeneration : TFieldBitBtn;
      FGrowthFactorControl : TGroupBox;

      FGrowthFactorGrid : TFieldStringGrid;

      FInterpolationCmBox : TFieldComboBox;

      FYearAndFactorGrid: TFieldStringGrid;

      FAbstractionControl : TGroupBox;
      FAbstractionCheckBox : TFieldChkBox;
      FAbstractionGrid: TFieldStringGrid;
      procedure CreateMemberObjects; override;
      //procedure DestroyMemberObjects; override;
    public
      procedure Resize; override;
      function LanguageHasChanged: boolean; override;
      function Initialise: boolean; override;
      //property GrowthFactorControl : TGrowthFactorControl read FGrowthFactorControl;
      property GrowthFactorControl : TGroupBox read FGrowthFactorControl;
      property LoadGenerationControl : TGroupBox read FLoadGenerationControl;
      property LoadGenerationCombo : TFieldComboBox read FLoadGenerationTypeCombo;
      property StdDeviationEdit  : TFieldEdit read FStdDeviationEdit;
      property FactorAndMeanGrid : TFieldStringGrid read FFactorAndMeanGrid;

      //property AbstractionControl :  TAbstractionControl read FAbstractionControl;

      property BtnAddGrowthFactors    : TFieldBitBtn         read FBtnAddGrowthFactors;
      property BtnAddLoadGeneration : TFieldBitBtn           read FBtnAddLoadGeneration;
      property BtnDeleteGrowthFactors : TFieldBitBtn         read FBtnDeleteGrowthFactors;
      property BtnDeleteLoadGeneration: TFieldBitBtn         read FBtnDeleteLoadGeneration;
      property GrowthFactorGrid       : TFieldStringGrid     read FGrowthFactorGrid;
      property InterpolationCmBox     : TFieldComboBox       read FInterpolationCmBox;
      property YearAndFactorGrid      : TFieldStringGrid     read FYearAndFactorGrid;
      property AbstractionControl     : TGroupBox            read FAbstractionControl;
      property AbstractionCheckBox    : TFieldChkBox         read FAbstractionCheckBox;
      property AbstractionGrid        : TFieldStringGrid     read FAbstractionGrid;


  end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations,
  VCL.Forms,
  VCL.Grids,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,

  UControlCreationUtilities,
  Math,
  UDataComponent;

procedure TPlanningMineOpenCastDialog.CreateMemberObjects;
const OPNAME = 'TPlanningMineOpenCastDialog.CreateMemberObjects';
begin
  inherited;
  try

    FAbstractionControl            := TGroupBox.Create(ControlsOwner);
    FAbstractionControl.Parent     := ControlsParent;


    FAbstractionCheckBox              := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FAbstractionCheckBox.Parent       := FAbstractionControl;

    FAbstractionGrid                  := TFieldStringGrid.Create(ControlsOwner, FAppModules);
    FAbstractionGrid.Parent           := FAbstractionControl;


    FGrowthFactorControl            := TGroupBox.Create(ControlsOwner);
    FGrowthFactorControl.Parent     := ControlsParent;
    FGrowthFactorControl.Visible    := False;

    FBtnAddGrowthFactors                := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnAddGrowthFactors.Parent         := FGrowthFactorControl;
    FBtnAddGrowthFactors.Left           := 10;
    FBtnAddGrowthFactors.Top            := 20;
    FBtnAddGrowthFactors.Width          := 75;
    FBtnAddGrowthFactors.Height         := 25;
    FBtnAddGrowthFactors.ShowHint       := True;
    FBtnAddGrowthFactors.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');




    FBtnDeleteGrowthFactors                := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnDeleteGrowthFactors.Parent         := FGrowthFactorControl;
    FBtnDeleteGrowthFactors.Left           := 95;
    FBtnDeleteGrowthFactors.Top            := 20;
    FBtnDeleteGrowthFactors.Width          := 75;
    FBtnDeleteGrowthFactors.Height         := 25;
    FBtnDeleteGrowthFactors.ShowHint       := True;
    FBtnDeleteGrowthFactors.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');




    FGrowthFactorGrid           := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FGrowthFactorGrid.Parent    := FGrowthFactorControl;
    FGrowthFactorGrid.Top       := FBtnAddGrowthFactors.Height + FBtnAddGrowthFactors.Top + 10;
    FGrowthFactorGrid.Left      := 10;

    FYearAndFactorGrid                  := TFieldStringGrid.Create(ControlsOwner, FAppModules);
    FYearAndFactorGrid.Parent           := FGrowthFactorControl;

    FInterpolationCmBox                := TFieldComboBox.Create(ControlsOwner, FAppModules);
    FInterpolationCmBox.Parent         := FGrowthFactorControl;
    
    //FLoadGenerationControl := TLoadGenerationControl.Create(ControlsOwner,FAppModules);

    FLoadGenerationControl        := TGroupBox.Create(ControlsOwner);
    FLoadGenerationControl.Parent := ControlsParent;
    FLoadGenerationControl.Visible := False;

    FBtnAddLoadGeneration                := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnAddLoadGeneration.Parent         := FLoadGenerationControl;
    FBtnAddLoadGeneration.Left           := 10;
    FBtnAddLoadGeneration.Top            := 20;
    FBtnAddLoadGeneration.Width          := 75;
    FBtnAddLoadGeneration.Height         := 25;
    FBtnAddLoadGeneration.ShowHint       := True;
    FBtnAddLoadGeneration.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');

    FBtnDeleteLoadGeneration                := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnDeleteLoadGeneration.Parent         := FLoadGenerationControl;
    FBtnDeleteLoadGeneration.Left           := 130;
    FBtnDeleteLoadGeneration.Top            := 20;
    FBtnDeleteLoadGeneration.Width          := 75;
    FBtnDeleteLoadGeneration.Height         := 25;
    FBtnDeleteLoadGeneration.ShowHint       := True;
    FBtnDeleteLoadGeneration.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');

    FLoadGenerationTypeLabel := TLabel.Create(ControlsOwner);
    FLoadGenerationTypeLabel.Parent := FLoadGenerationControl;
    FLoadGenerationTypeLabel.Top := FBtnDeleteLoadGeneration.Top + FBtnDeleteLoadGeneration.Height + 10;
    FLoadGenerationTypeLabel.Left := 10;
    FLoadGenerationTypeLabel.Width := 70;

    FLoadGenerationTypeCombo := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FLoadGenerationTypeCombo.Parent := FLoadGenerationControl;
     FLoadGenerationTypeCombo.Top := FLoadGenerationTypeLabel.Top;
    FLoadGenerationTypeCombo.Left := 130;
    FLoadGenerationTypeCombo.Width := 150;
    FLoadGenerationTypeCombo.Style := TComboBoxStyle.csDropDownList;


    FStdDeviationLabel := TLabel.Create(ControlsOwner);
    FStdDeviationLabel.Parent := FLoadGenerationControl;
    FStdDeviationLabel.Top := FLoadGenerationTypeLabel.Top + FLoadGenerationTypeLabel.Height + 10;
    FStdDeviationLabel.Left := 10;

    FStdDeviationEdit := TFieldEdit.Create(ControlsOwner,FAppModules);
    FStdDeviationEdit.Parent := FLoadGenerationControl;
    FStdDeviationEdit.Top := FStdDeviationLabel.Top;
    FStdDeviationEdit.Left := FLoadGenerationTypeCombo.Left;
    FStdDeviationEdit.Width := 50;

    FFactorAndMeanGrid := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FFactorAndMeanGrid.Parent :=  FLoadGenerationControl;
    FFactorAndMeanGrid.Top := FStdDeviationLabel.Top + FStdDeviationLabel.Height + 10;
    FFactorAndMeanGrid.Left := FStdDeviationEdit.Left;
    FFactorAndMeanGrid.ColAutoSizeIgnoreHeading := true;
   // FFactorAndMeanGrid.ColCount := 2;
   Resize;

  except on E: Exception do HandleError(E,OPNAME); end;
end;
(*procedure TPlanningMineOpenCastDialog.DestroyMemberObjects;
const OPNAME = 'TPlanningMineOpenCastDialog.DestroyMemberObjects';
begin
  //inherited;
  try
    //FreeAndNil(FGrowthFactorControl);
    //FreeAndNil(FLoadGenerationControl);

  except on E: Exception do HandleError(e,opname); end;
end;*)

function TPlanningMineOpenCastDialog.Initialise;
const OPNAME = 'TPlanningMineOpenCastDialog.Initialise';
begin
  Result := false;
  try
    Result := inherited Initialise;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineOpenCastDialog.LanguageHasChanged;
const OPNAME = 'TPlanningMineOpenCastDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    //FGrowthFactorControl.FactorTypeLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.FactorType');
    FGrowthFactorControl.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.Caption');
    //NoofPointsLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.NoOfPoints');
    //InterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.InterpolationMethod');
    //LinearInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.LinearInterpolation');
    //ExponentialInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.ExponentialInterpolation');


    FLoadGenerationControl.Caption :=  FAppModules.Language.GetString('TLoadGenerationControl.Caption');
    FLoadGenerationTypeLabel.Caption :=  FAppModules.Language.GetString('TLoadGenerationControl.LoadGenerationType');
    FStdDeviationLabel.Caption := FAppModules.Language.GetString('TLoadGenerationControl.StandardDeviation');

    FOpenCastPitsGrid.Cells[19,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.PCDIniConcentrationCol');
    FOpenCastPitsGrid.Cells[20,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.WorkingCommYearCol');
    FOpenCastPitsGrid.Cells[21,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.WorkingCommMonthCol');
    FOpenCastPitsGrid.Cells[22,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.WorkingDecommYearCol');
    FOpenCastPitsGrid.Cells[23,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.WorkingDecommMonthCol');
    FOpenCastPitsGrid.Cells[24,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.RunoffSaltWashoffEfficiencyFactorCol');
    FOpenCastPitsGrid.Cells[25,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.IniSaltStoreCol');
    FOpenCastPitsGrid.Cells[26,0]:= FAppModules.Language.GetString('PlanningMineOpenCastDialog.ReChargeRateCol');

    FAbstractionControl.Caption :=   FAppModules.Language.GetString('AbstractionControl.Caption');

    FAbstractionCheckBox.Caption := FAppModules.Language.GetString('AbstractionControl.AbstractionLabel');
    FAbstractionGrid.Cells[0,0] := FAppModules.Language.GetString('AbstractionControl.EvapCol');
    FAbstractionGrid.Cells[1,0] := FAppModules.Language.GetString('AbstractionControl.RiverCol');
    FAbstractionGrid.Cells[2,0] := FAppModules.Language.GetString('AbstractionControl.PCDCol');
    FAbstractionGrid.Cells[3,0] := FAppModules.Language.GetString('AbstractionControl.TotalCol');
    FAbstractionGrid.Cells[4,0] := FAppModules.Language.GetString('AbstractionControl.TimeSeriesLabel');

    //FAbstractionGrid.Cells[1,0] :=   FAppModules.Language.GetString('AbstractionControl.PortionCol');
    //FAbstractionControl.TimeSeriesLabel.Caption :=   FAppModules.Language.GetString('AbstractionControl.TimeSeriesLabel');
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastDialog.Resize;
const OPNAME = 'TPlanningMineOpenCastDialog.Resize';
begin
  inherited Resize;
  try

   with FOpenCastPitsGrid do
    begin
      BorderStyle      := bsSingle;
      ColCount         := 27;
      FixedCols        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 66;
      RowHeights[0]    := 50;
      WrapHeaderText   := True;
      Height           := DefaultRowHeight + RowHeights[0] + 25;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;

    FAbstractionControl.Top        := FOpenCastPitsGrid.Height + FOpenCastPitsGrid.Top + 10;
    FAbstractionControl.Width      := 480;
    FAbstractionControl.Left       := 10;

    FAbstractionCheckBox.Top          := 20;
    FAbstractionCheckBox.Left         := 10;

    FAbstractionGrid.Top              := FAbstractionCheckBox.Top + FAbstractionCheckBox.Height + 10;
    FAbstractionGrid.Left             := 10;
    FAbstractionGrid.ColCount         := 5;
    FAbstractionGrid.RowCount         := 2;
    FAbstractionGrid.FixedCols        := 0;
    FAbstractionGrid.FixedRows        := 1;
    FAbstractionGrid.DefaultRowHeight := 20;
    FAbstractionGrid.DefaultColWidth  := 85;
    FAbstractionGrid.Width            := (FAbstractionGrid.DefaultColWidth * FAbstractionGrid.ColCount) + 15;
    FAbstractionGrid.Height           := 48;
    FAbstractionGrid.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];

    FGrowthFactorControl.Top        := FAbstractionControl.Top;
    FLoadGenerationControl.Top      := FAbstractionControl.Top + FAbstractionControl.Height + 10;



    FGrowthFactorControl.Width      := 480;



    FInterpolationCmBox.Top            := FGrowthFactorGrid.Top + 27;
    FInterpolationCmBox.Left           := FGrowthFactorGrid.ColWidths[0];
    FInterpolationCmBox.Width          := 70;
    FInterpolationCmBox.Height         := 28;
    FInterpolationCmBox.Style          := csDropDownList;
    FInterpolationCmBox.Visible        := False;



    FYearAndFactorGrid.Top              := FGrowthFactorGrid.Top + FGrowthFactorGrid.Height + 10;
    FYearAndFactorGrid.Left             := 10;
    FYearAndFactorGrid.ColCount         := 3;
    FYearAndFactorGrid.RowCount         := 2;
    FYearAndFactorGrid.FixedCols        := 0;
    FYearAndFactorGrid.FixedRows        := 1;
  //  FYearAndFactorGrid.DefaultRowHeight := 40;
   // FYearAndFactorGrid.DefaultColWidth  := 85;
    FYearAndFactorGrid.Width            := (FYearAndFactorGrid.DefaultColWidth * FYearAndFactorGrid.ColCount) + 15;
    FYearAndFactorGrid.Height           := (FYearAndFactorGrid.DefaultRowHeight * FYearAndFactorGrid.RowCount) + 15;
    FYearAndFactorGrid.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];

    FYearAndFactorGrid.DefaultRowHeight   := 20;
    FYearAndFactorGrid.DefaultColWidth    := 40;
    FYearAndFactorGrid.Height             := 48;




    FLoadGenerationControl.Top := FAbstractionControl.Top + FAbstractionControl.Height + 10;
    FLoadGenerationControl.Left := 10;

    //FactorAndMeanGrid.ColCount := 2;
    FLoadGenerationControl.Width      := 350;
    FLoadGenerationControl.Height     := 400;
    FGrowthFactorControl.Left       := FAbstractionControl.Left + FAbstractionControl.Width + 10;
    FGrowthFactorControl.Height   := FYearAndFactorGrid.Top + FYearAndFactorGrid.Height + 10;
    FactorAndMeanGrid.DefaultRowHeight := 20;
    FactorAndMeanGrid.DefaultColWidth  := 85;
    FloadGenerationControl.Width    := FAbstractionControl.Width;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
