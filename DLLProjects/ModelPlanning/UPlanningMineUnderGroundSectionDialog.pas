unit UPlanningMineUnderGroundSectionDialog;

interface
uses
  Classes,

  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UDataEditComponent,
  UGrowthFactorControl,
  ULoadGenerationControl,
  UMiningUnderGroundSectionDialog;
type
  TPlanningMineUnderGroundSectionDialog = class(TMiningUnderGroundSectionDialog)
    protected
      FLoadGenerationControl : TGroupBox;
      FLoadGenerationTypeLabel : TLabel;
      FLoadGenerationTypeCombo : TFieldComboBox;
      FStdDeviationLabel : TLabel;
      FStdDeviationEdit : TFieldEdit;
      FFactorAndMeanGrid : TFieldStringGrid;

      FGrowthFactorControl : TGroupBox;
      FFactorTypeLabel : TLabel;
      FFactorTypeCombo : TFieldComboBox;
      FNoofPointsLabel : TLabel;
      FNoOfPointsEdit : TFieldEdit;
      FInterpolationLabel : Tlabel;
      FInterpolationCheckBox : TFieldComboBox;
      FYearAndFactorGrid: TFieldStringGrid;


      FBtnDeleteGrowthFactors : TFieldBitBtn;
      FBtnAddGrowthFactors : TFieldBitBtn;
      FBtnAddLoadGeneration : TFieldBitBtn;
      FBtnDeleteLoadGeneration : TFieldBitBtn;

      procedure CreateMemberObjects; override;

    public
      procedure Resize; override;
      function LanguageHasChanged: boolean; override;
      property GrowthFactorControl     : TGroupBox               read FGrowthFactorControl;
      property FactorTypeCombo         : TFieldComboBox          read FFactorTypeCombo;
      property NoOfPointsEdit          : TFieldEdit              read FNoOfPointsEdit;
      property InterpolationCheckBox   : TFieldComboBox          read FInterpolationCheckBox;
      property YearAndFactorGrid       : TFieldStringGrid        read FYearAndFactorGrid;

      property LoadGenerationControl : TGroupBox read FLoadGenerationControl;
      property LoadGenerationCombo : TFieldComboBox read FLoadGenerationTypeCombo;
      property StdDeviationEdit  : TFieldEdit read FStdDeviationEdit;
      property FactorAndMeanGrid : TFieldStringGrid read FFactorAndMeanGrid;

      property BtnAddGrowthFactors    : TFieldBitBtn         read FBtnAddGrowthFactors;
      property BtnDeleteGrowthFactors : TFieldBitBtn         read FBtnDeleteGrowthFactors;
      property BtnAddLoadGeneration : TFieldBitBtn           read FBtnAddLoadGeneration;
      property BtnDeleteLoadGeneration: TFieldBitBtn         read FBtnDeleteLoadGeneration;

  end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations,
  VCL.Forms,
  VCL.Grids,



  UAbstractObject,
  UAbstractComponent,

  UControlCreationUtilities,
  Math,
  UDataComponent;

{ TPlanningMineUnderGroundSectionDialog }

procedure TPlanningMineUnderGroundSectionDialog.CreateMemberObjects;
const OPNAME ='TPlanningMineUnderGroundSectionDialog.CreateMemberObjects';
begin
  inherited;
  try
    FGrowthFactorControl := TGroupBox.Create(ControlsOwner);
    FGrowthFactorControl.Parent := ControlsParent;
    FGrowthFactorControl.Visible := false;


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



    FGrowthFactorControl.Top := FUnderGroundMiningGrid.Top + FUnderGroundMiningGrid.Height + 10;
    FGrowthFactorControl.Left := 10;


    FFactorTypeLabel                  := TLabel.Create(ControlsOwner);
    FFactorTypeLabel.Parent           := FGrowthFactorControl;

    FFactorTypeCombo                 := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FFactorTypeCombo.Parent          := FGrowthFactorControl;
    FFactorTypeCombo.Style           := TComboBoxStyle.csDropDownList;


    FNoofPointsLabel                 := TLabel.Create(ControlsOwner);
    FNoofPointsLabel.Parent          := FGrowthFactorControl;

    FNoOfPointsEdit                  := TFieldEdit.Create(ControlsOwner,FAppModules);
    FNoOfPointsEdit.Parent           := FGrowthFactorControl;

    FInterpolationLabel              := Tlabel.Create(ControlsOwner);
    FInterpolationLabel.Parent       := FGrowthFactorControl;


    FInterpolationCheckBox           := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FInterpolationCheckBox.Parent    := FGrowthFactorControl;
    FInterpolationCheckBox.Style     := TComboBoxStyle.csDropDownList;

    FYearAndFactorGrid               := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FYearAndFactorGrid.Parent        := FGrowthFactorControl;


    FLoadGenerationControl        := TGroupBox.Create(ControlsOwner);
    FLoadGenerationControl.Parent := ControlsParent;
    FLoadGenerationControl.visible        := False;

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

  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TPlanningMineUnderGroundSectionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPlanningMineUnderGroundSectionDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFactorTypeLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.FactorType');
    FGrowthFactorControl.Caption := 'Growth Factor';
    FNoofPointsLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.NoOfPoints');
    FInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.InterpolationMethod');
    //FGrowthFactorControl.LinearInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.LinearInterpolation');
    //FGrowthFactorControl.ExponentialInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.ExponentialInterpolation');
    FLoadGenerationControl.Caption :=  FAppModules.Language.GetString('TLoadGenerationControl.Caption');
    FLoadGenerationTypeLabel.Caption := FAppModules.Language.GetString('TLoadGenerationControl.LoadGenerationType');
   // FLoadGenerationControl.LoadGenerationLabel.Caption :=  FAppModules.Language.GetString('TLoadGenerationControl.LoadGenerationType');

    FStdDeviationLabel.Caption := FAppModules.Language.GetString('TLoadGenerationControl.StandardDeviation');
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TPlanningMineUnderGroundSectionDialog.Resize;
const OPNAME = 'TPlanningMineUnderGroundSectionDialog.Resize';
var
LControlCount : integer;
LMaxLeftPos : integer;
LControl : TControl;
begin
  inherited;
  try
    FGrowthFactorControl.Top := FUnderGroundMiningGrid.Top + FUnderGroundMiningGrid.Height + 10;


    FGrowthFactorControl.Width := 450;
    FGrowthFactorControl.Height := 220;

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

    FYearAndFactorGrid.Left               := FInterpolationLabel.Left;
    FYearAndFactorGrid.Top                := FInterpolationLabel.Top + 25;
    FYearAndFactorGrid.DefaultRowHeight   := 20;
    FYearAndFactorGrid.DefaultColWidth    := 40;
    FYearAndFactorGrid.Height             := 48;
    FYearAndFactorGrid.RowCount           := 2;
    FYearAndFactorGrid.ColWidths[0] := YearAndFactorGrid.DefaultColWidth*5;

    FLoadGenerationControl.Top            := FGrowthFactorControl.Top;
    FLoadGenerationControl.Left           := 10;
    FLoadGenerationControl.Width          := 350;
    FLoadGenerationControl.Height         := 390;

    FGrowthFactorControl.Left := FLoadGenerationControl.Left +FLoadGenerationControl.Width + 10;
    LMaxLeftPos := 0;
    for LControlCount := 0 to FGrowthFactorControl.ControlCount -1 do
    begin
      LControl := FGrowthFactorControl.Controls[LControlCount];
      if LControl.Left + LControl.Width > LMaxLeftPos then
        LMaxLeftPos := LControl.Left + LControl.Width;
    end;

    FGrowthFactorControl.Width := LMaxLeftPos + 10;

    FactorAndMeanGrid.DefaultRowHeight    := 20;
    FactorAndMeanGrid.DefaultColWidth     := 85;

  except on E: Exception do HandleError(E,OPNAME) end;
end;

end.
