unit UPlanningSlurryDumpDialog;

interface
uses
  Classes,
  VCL.Forms,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Grids,
  UDataEditComponent,
  UGrowthFactorControl,
  ULoadGenerationControl,
  UMiningSlurryDumpDialog;
type
  TPlanningMineSlurryDumpDialog = class(TMiningSlurryDumpDialog)
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
      FFactorTypeLabel : TLabel;
      FFactorTypeCombo : TFieldComboBox;
      FNoofPointsLabel : TLabel;
      FNoOfPointsEdit : TFieldEdit;
      FInterpolationLabel : Tlabel;
      FInterpolationCheckBox : TFieldComboBox;
      FYearAndFactorGrid: TFieldStringGrid;

      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;

    public
      procedure Resize; override;
      function LanguageHasChanged: boolean; override;
      property LoadGenerationControl : TGroupBox       read FLoadGenerationControl;
      property GrowthFactorControl :  TGroupBox        read FGrowthFactorControl;
      property FactorTypeCombo : TFieldComboBox        read FFactorTypeCombo;
      property NoOfPointsEdit : TFieldEdit             read FNoOfPointsEdit;
      property InterpolationCheckBox : TFieldComboBox  read FInterpolationCheckBox;
      property YearAndFactorGrid: TFieldStringGrid     read FYearAndFactorGrid;

      property LoadGenerationCombo : TFieldComboBox read FLoadGenerationTypeCombo;
      property StdDeviationEdit  : TFieldEdit read FStdDeviationEdit;
      property FactorAndMeanGrid : TFieldStringGrid read FFactorAndMeanGrid;

      property BtnAddGrowthFactors    : TFieldBitBtn         read FBtnAddGrowthFactors;
      property BtnAddLoadGeneration : TFieldBitBtn           read FBtnAddLoadGeneration;
      property BtnDeleteGrowthFactors : TFieldBitBtn         read FBtnDeleteGrowthFactors;
      property BtnDeleteLoadGeneration: TFieldBitBtn         read FBtnDeleteLoadGeneration;

end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations,
  UAbstractObject,
  UAbstractComponent,

  UControlCreationUtilities,
  Math,
  UDataComponent;
{ TPlanningMineSlurryDumpDialog }

procedure TPlanningMineSlurryDumpDialog.CreateMemberObjects;
const OPNAME = 'TPlanningMineSlurryDumpDialog.CreateMemeberObjects';
begin
  inherited;
  try
    with FSlurryDumpGrid do
    begin
      BorderStyle      := bsSingle;
      ColCount         := 10;
      FixedCols        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 90;
      RowHeights[0]    := 35;
      WrapHeaderText   := True;
    end;

    FGrowthFactorControl := TGroupBox.Create(ControlsOwner);
    FGrowthFactorControl.Parent := ControlsParent;
    FGrowthFactorControl.Visible := False;

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

    FFactorTypeLabel                  := TLabel.Create(ControlsOwner);
    FFactorTypeLabel.Parent           := FGrowthFactorControl;

    FFactorTypeCombo                 := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FFactorTypeCombo.Parent          := FGrowthFactorControl;


    FNoofPointsLabel                 := TLabel.Create(ControlsOwner);
    FNoofPointsLabel.Parent          := FGrowthFactorControl;

    FNoOfPointsEdit                  := TFieldEdit.Create(ControlsOwner,FAppModules);
    FNoOfPointsEdit.Parent           := FGrowthFactorControl;

    FInterpolationLabel              := Tlabel.Create(ControlsOwner);
    FInterpolationLabel.Parent       := FGrowthFactorControl;

    FInterpolationCheckBox           := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FInterpolationCheckBox.Parent    := FGrowthFactorControl;

    FYearAndFactorGrid               := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FYearAndFactorGrid.Parent        := ControlsParent;
    FYearAndFactorGrid.Visible       := False;

    FLoadGenerationControl        := TGroupBox.Create(ControlsOwner);
    FLoadGenerationControl.Parent := ControlsParent;
    FLoadGenerationControl.Visible    := False;

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
    FLoadGenerationTypeLabel.Top := 60;
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

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSlurryDumpDialog.DestroyMemberObjects;
const OPNAME = 'TPlanningMineSlurryDumpDialog.DestroyMemberObjects';
begin
  try


  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSlurryDumpDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPlanningMineSlurryDumpDialog.LanguageHasChanged';
begin
  Result:= inherited LanguageHasChanged;
  try
    FSlurryDumpGrid.Cells[9,0]       := FAppModules.Language.GetString('PlanningSlurryDumpDialog.SaltConcentrationCol');
    FFactorTypeLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.FactorType');
    FGrowthFactorControl.Caption := 'Growth Factor';
    FNoofPointsLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.NoOfPoints');
    FInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.InterpolationMethod');
    //FLinearInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.LinearInterpolation');
    //ExponentialInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.ExponentialInterpolation');
    FLoadGenerationControl.Caption :=  FAppModules.Language.GetString('TLoadGenerationControl.Caption');
    FLoadGenerationTypeLabel.Caption :=  FAppModules.Language.GetString('TLoadGenerationControl.LoadGenerationType');
    FStdDeviationLabel.Caption := FAppModules.Language.GetString('TLoadGenerationControl.StandardDeviation');
    Result := true;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSlurryDumpDialog.Resize;
const OPNAME = 'TPlanningMineSlurryDumpDialog.Resize';
begin
  inherited;
  try
    FLoadGenerationControl.Top            := FSlurryDumpGrid.Top + FSlurryDumpGrid.Height + 10;
    FLoadGenerationControl.Left           := 10;//   FGrowthFactorControl.Left + FGrowthFactorControl.Width + 10;
    FLoadGenerationControl.Width      := 350;
    FLoadGenerationControl.Height     := 360;

    FGrowthFactorControl.Top := FLoadGenerationControl.Top ;
    FGrowthFactorControl.Left := FLoadGenerationControl.Left + FLoadGenerationControl.Width + 10; //10;


    FGrowthFactorControl.Width := 450;
    FGrowthFactorControl.Height := 180;

    FFactorTypeLabel.Left                := 10;
    FFactorTypeLabel.Top                 := 65;

    FFactorTypeCombo.Top                  := FFactorTypeLabel.Top;
    FFactorTypeCombo.Left                 := 210;
    FFactorTypeCombo.Width                := 150;


    FNoofPointsLabel.Top                  := FFactorTypeLabel.Top + 25;
    FNoofPointsLabel.Left                 := FFactorTypeLabel.Left;

    FNoOfPointsEdit.Top                   := FNoofPointsLabel.Top;
    FNoOfPointsEdit.Left                  := FFactorTypeCombo.Left;
    FNoOfPointsEdit.Width                 := 50;

    FInterpolationLabel.Top                := FNoofPointsLabel.Top + 25;
    FInterpolationLabel.Left               := FFactorTypeLabel.Left;

    FInterpolationCheckBox.Left           := FFactorTypeCombo.Left;
    FInterpolationCheckBox.Top            := FInterpolationLabel.Top;
    FInterpolationCheckBox.Width          := 150;

    FYearAndFactorGrid.Left               := FGrowthFactorControl.Left;
    FYearAndFactorGrid.Top                := FGrowthFactorControl.Top + FGrowthFactorControl.Height + 10;
    FYearAndFactorGrid.DefaultRowHeight   := 20;
    FYearAndFactorGrid.DefaultColWidth    := 40;
    FYearAndFactorGrid.Height             := 48;

   {* FYearAndFactorGrid.Top              := FGrowthFactorGrid.Top + FGrowthFactorGrid.Height + 10;
    FYearAndFactorGrid.Left             := 10;
    FYearAndFactorGrid.ColCount         := 3;
    FYearAndFactorGrid.RowCount         := 2;
    FYearAndFactorGrid.FixedCols        := 0;
    FYearAndFactorGrid.FixedRows        := 1;
  //  FYearAndFactorGrid.DefaultRowHeight := 40;
   // FYearAndFactorGrid.DefaultColWidth  := 85;
    FYearAndFactorGrid.Width            := (FYearAndFactorGrid.DefaultColWidth * FYearAndFactorGrid.ColCount) + 15;
    FYearAndFactorGrid.Height           := (FYearAndFactorGrid.DefaultRowHeight * FYearAndFactorGrid.RowCount) + 15;
    FYearAndFactorGrid.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];*}




    FactorAndMeanGrid.DefaultRowHeight := 20;
    FactorAndMeanGrid.DefaultColWidth  := 85;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
