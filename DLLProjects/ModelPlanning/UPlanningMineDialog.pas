unit UPlanningMineDialog;

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
    UDataEditComponent,
    UGrowthFactorControl,
    UMiningDialog;

  type

    TPlanningMineDialog = class(TMiningDialog)
      protected
        FSaltAssociationLabel  : TLabel;
        FMineGrowthLabel :    TLabel;
        FSaltAssociationButton: TButton;
        FMineGrowthButton : TButton;
        FRainfallLabel : TLabel;
        FRainfallEdit : TFieldEdit;
        FMeanAnnualPrecipitationLabel : TLabel;
        FMeanAnnualPrecipitationEdit : TFieldEdit;

        FGrowthFactorControl : TGroupBox;

        FFactorTypeLabel : TLabel;
        FFactorTypeCombo : TFieldComboBox;
        FNoofPointsLabel : TLabel;
        FNoOfPointsEdit : TFieldEdit;
        FInterpolationLabel : Tlabel;
        FInterpolationCheckBox : TFieldComboBox;
        FYearAndFactorGrid: TFieldStringGrid;
        FOpenRainfallFileBtn : TFieldButton;

        FAssocSaltWashoffLabel : TLabel;
        FAssocSaltWashoffEdit : TFieldEdit;
        FSaltBuildUpRateLabel : TLabel;
        FSaltBuildUpRateEdit : TFieldEdit;
        FSaltWashOffEfficiencyFactorLabel : TLabel;
        FSaltWashOffEfficiencyFactorEdit : TFieldEdit;
        FInitialSaltStoreLabel : TLabel;
        FInitialSaltStoreEdit : TFieldEdit;

        FCreateGrowthFactorButton : TFieldBitBtn;

        FDeleteGrowthFactorButton : TFieldBitBtn;

        procedure CreateMemberObjects; override;
        procedure DestroyMemberObjects; override;
        procedure AssignHelpContext; override;
      public
        procedure Resize; override;
        procedure RestoreColourState; override;
        function LanguageHasChanged: boolean; override;
        function Initialise: boolean; override;
        property SaltAssociationButton: TButton read FSaltAssociationButton;
        property MineGrowthButton: TButton read FMineGrowthButton;
        property RainfallEdit: TFieldEdit read FRainfallEdit;
        property MeanAnnualPrecipitationEdit: TFieldEdit read FMeanAnnualPrecipitationEdit;

        property GrowthFactorControl: TGroupBox read FGrowthFactorControl;

        property AssocSaltWashoffEdit: TFieldEdit read FAssocSaltWashoffEdit;
        property SaltBuildUpRateEdit: TFieldEdit read FSaltBuildUpRateEdit;
        property SaltWashOffEfficiencyFactorEdit : TFieldEdit read FSaltWashOffEfficiencyFactorEdit;
        property InitialSaltStoreEdit: TFieldEdit read FInitialSaltStoreEdit;
        property CreateGrowthFactorButton: TFieldBitBtn read FCreateGrowthFactorButton;
        property DeleteGrowthFactorButton : TFieldBitBtn  read FDeleteGrowthFactorButton;
        property NoOfPointsEdit          : TFieldEdit       read FNoOfPointsEdit;
        property InterpolationCheckBox   : TFieldComboBox   read FInterpolationCheckBox;
        property YearAndFactorGrid       : TFieldStringGrid read FYearAndFactorGrid;
        property FactorTypeCombo         : TFieldComboBox   read FFactorTypeCombo;
        property OpenRainfallFileBtn     : TFieldButton read FOpenRainfallFileBtn;

    end;

implementation
uses
  SysUtils,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TPlanningMineDialog }

procedure TPlanningMineDialog.AssignHelpContext;
begin
  inherited;

end;

procedure TPlanningMineDialog.CreateMemberObjects;
const OPNAME = 'TPlanningMineDialog.CreateMemberObjects';
begin
  inherited;
  try
    FAssocSaltWashoffLabel := TLabel.Create(ControlsOwner);
    FAssocSaltWashoffLabel.Parent := ControlsParent;
    FAssocSaltWashoffLabel.Top :=FPCDExistsLabel.Top + FPCDExistsLabel.Height + 10;
    FAssocSaltWashoffLabel.Left := 10;

    FAssocSaltWashoffEdit := TFieldEdit.Create(ControlsOwner,FAppModules);
    FAssocSaltWashoffEdit.Parent := ControlsParent;
    FAssocSaltWashoffEdit.Top := FAssocSaltWashoffLabel.Top;
    FAssocSaltWashoffEdit.Left := 210;

    FRainfallLabel := TLabel.Create(ControlsOwner);
    FRainfallLabel.Parent := ControlsParent;
    FRainfallLabel.Left := 10;
    FRainfallLabel.Top := FAssocSaltWashoffLabel.Top + FAssocSaltWashoffLabel.Height + 10;

    FRainfallEdit := TFieldEdit.Create(ControlsOwner,FAppModules);
    FRainfallEdit.Parent := ControlsParent;
    FRainfallEdit.Left := 210;
    FRainfallEdit.Top := FRainfallLabel.Top;
    FOpenRainfallFileBtn          :=TFieldButton.Create(ControlsOwner,FAppModules,FAppModules.Language.GetString('LabelText.ThreeDots'));
    FOpenRainfallFileBtn.Parent := ControlsParent;
    FOpenRainfallFileBtn.Caption := FAppModules.Language.GetString('LabelText.ThreeDots');

    FOpenRainfallFileBtn.Top := FRainfallEdit.Top;
    FOpenRainfallFileBtn.Left := FRainfallEdit.Left + FRainfallEdit.Width + 2;
    FOpenRainfallFileBtn.Width := 25;
    FOpenRainfallFileBtn.Height := 20;

    FMeanAnnualPrecipitationLabel := TLabel.Create(ControlsOwner);
    FMeanAnnualPrecipitationLabel.Parent := ControlsParent;
    FMeanAnnualPrecipitationLabel.Top := FRainfallLabel.Top + FRainfallLabel.Height + 10;
    FMeanAnnualPrecipitationLabel.Left := FRainfallLabel.Left;

    FMeanAnnualPrecipitationEdit := TFieldEdit.Create(ControlsOwner,FAppModules);
    FMeanAnnualPrecipitationEdit.Parent := ControlsParent;
    FMeanAnnualPrecipitationEdit.Top := FMeanAnnualPrecipitationLabel.Top;
    FMeanAnnualPrecipitationEdit.Left := FRainfallEdit.Left;




    FSaltBuildUpRateLabel := TLabel.Create(ControlsOwner);
    FSaltBuildUpRateLabel.Parent := ControlsParent;
    FSaltBuildUpRateLabel.Top := FMeanAnnualPrecipitationLabel.Top + FMeanAnnualPrecipitationLabel.Height + 10;
    FSaltBuildUpRateLabel.Left := FRainfallLabel.Left;

    FSaltBuildUpRateEdit := TFieldEdit.Create(ControlsOwner,FAppModules);
    FSaltBuildUpRateEdit.Parent := ControlsParent;
    FSaltBuildUpRateEdit.Left := FRainfallEdit.Left;
    FSaltBuildUpRateEdit.Top := FSaltBuildUpRateLabel.Top;

    FSaltWashOffEfficiencyFactorLabel := TLabel.Create(ControlsOwner);
    FSaltWashOffEfficiencyFactorLabel.Parent := ControlsParent;
    FSaltWashOffEfficiencyFactorLabel.Top := FSaltBuildUpRateLabel.Top + FSaltBuildUpRateLabel.Height + 10;
    FSaltWashOffEfficiencyFactorLabel.Left := FRainfallLabel.Left;

    FSaltWashOffEfficiencyFactorEdit := TFieldEdit.Create(ControlsOwner,FAppModules);
    FSaltWashOffEfficiencyFactorEdit.Parent := ControlsParent;
    FSaltWashOffEfficiencyFactorEdit.Top := FSaltWashOffEfficiencyFactorLabel.Top;
    FSaltWashOffEfficiencyFactorEdit.Left := FRainfallEdit.Left;

    FInitialSaltStoreLabel := TLabel.Create(ControlsOwner);
    FInitialSaltStoreLabel.Parent := ControlsParent;
    FInitialSaltStoreLabel.Top :=  FSaltWashOffEfficiencyFactorLabel.Top + FSaltWashOffEfficiencyFactorLabel.Height + 10;
    FInitialSaltStoreLabel.Left := FRainfallLabel.Left;
    FInitialSaltStoreEdit := TFieldEdit.Create(ControlsOwner,FAppModules);
    FInitialSaltStoreEdit.Parent := ControlsParent;
    FInitialSaltStoreEdit.Top := FInitialSaltStoreLabel.Top;
    FInitialSaltStoreEdit.Left := FRainfallEdit.Left;

    FGrowthFactorControl              := TGroupBox.Create(ControlsOwner);
    FGrowthFactorControl.Parent       := ControlsParent;

    //FCreateGrowthFactorButton         := TFieldBitBtn.Create(ControlsOwner);
    //FCreateGrowthFactorButton.Parent  := FGrowthFactorControl;


    FCreateGrowthFactorButton                := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FCreateGrowthFactorButton.Parent         := FGrowthFactorControl;
   
    FCreateGrowthFactorButton.ShowHint       := True;
    FCreateGrowthFactorButton.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');


    FDeleteGrowthFactorButton                := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FDeleteGrowthFactorButton.Parent         := FGrowthFactorControl;

    FDeleteGrowthFactorButton.ShowHint       := True;
    FDeleteGrowthFactorButton.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');


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


  except on E: Exception do HandleError(E,OPNAME); end;

end;

procedure TPlanningMineDialog.DestroyMemberObjects;
const OPNAME = 'TPlanningMineDialog.DestroyMemberObjects';
begin
  inherited;
  try

  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TPlanningMineDialog.Initialise: boolean;
const OPNAME = 'TPlanningMineDialog.Initialise';
begin
  Result := false;
  try
    Result := inherited Initialise;

  except on E:Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPlanningMineDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FRainfallLabel.Caption := FAppModules.Language.GetString('TPlanningMineDialog.RainfallLabel');
    FMeanAnnualPrecipitationLabel.Caption := FAppModules.Language.GetString('TPlanningMineDialog.MeanAnnualPrecipitationLabel');
    FFactorTypeLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.FactorType');
    FGrowthFactorControl.Caption := 'Growth Factor';
    FNoofPointsLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.NoOfPoints');
    FInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.InterpolationMethod');
   // FLinearInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.LinearInterpolation');
    //FGrowthFactorControl.ExponentialInterpolationLabel.Caption := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.ExponentialInterpolation');
    FAssocSaltWashoffLabel.Caption := FAppModules.Language.GetString('TPlanningMineDialog.SaltWashoffNoLabel');
    FSaltBuildUpRateLabel.Caption := FAppModules.Language.GetString('TPlanningMineDialog.BuildUpRateLabel');
    FSaltWashOffEfficiencyFactorLabel.Caption := FAppModules.Language.GetString('TPlanningMineDialog.EfficiencyFactorLabel');
    FInitialSaltStoreLabel.Caption := FAppModules.Language.GetString('TPlanningMineDialog.InitialSaltStoreLabel');
    FCreateGrowthFactorButton.Caption := 'Add'; //FAppModules.Language.GetString('PlanningMineValidator.CreateGrowthFactorButton');
    FDeleteGrowthFactorButton.Caption := 'Delete';
    Result := True;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineDialog.Resize;
const OPNAME = 'TPlanningMineDialog.Resize';
begin
  inherited;
  try
     HydrologyNodeNumberCbx.Visible := False;
     FHydrologyNodeNumberLabel.Visible := False;

     FMonthlyMeanPanEvapLabel.Top := FHydrologyNodeNumberLabel.Top;
     FMonthlyMeanPanEvapBtn.Top := FMonthlyMeanPanEvapLabel.Top;
     FMonthlyLakeEvapFactorsLabel.Top :=  FMonthlyMeanPanEvapBtn.Top + 25;
     FMonthlyLakeEvapFactorsBtn.Top := FMonthlyLakeEvapFactorsLabel.Top;
     FBeneficiationPlantAreaLabel.Top := FMonthlyLakeEvapFactorsBtn.Top+25;
     FBeneficiationPlantAreaEdt.Top := FMonthlyLakeEvapFactorsBtn.Top + 25;
     FBeneficiationRunOffFactorLabel.Top := FBeneficiationPlantAreaEdt.Top + 25;
     FBeneficiationRunOffFactorEdt.Top := FBeneficiationPlantAreaEdt.Top + 25;
     FMineXCoordLabel.Top := FBeneficiationRunOffFactorEdt.Top + 25;
     FMineXCoordEdit.Top := FBeneficiationRunOffFactorEdt.Top + 25;
     FMineYCoordLabel.Top := FMineXCoordEdit.Top + 25;
     FMineYCoordEdit.Top := FMineXCoordEdit.Top + 25;

     FPCDExistsLabel.Top := FMineYCoordEdit.Top + 25;
     FPCDExistsChkBox.Top := FPCDExistsLabel.Top ;

     FAssocSaltWashoffLabel.Top := FPCDExistsChkBox.Top + 25;
     FAssocSaltWashoffEdit.Top := FAssocSaltWashoffLabel.Top;
     FAssocSaltWashoffEdit.Width := 80;
     FRainfallLabel.Top := FAssocSaltWashoffLabel.Top + 25;
     FRainfallEdit.Top := FRainfallLabel.Top;
     FOpenRainfallFileBtn.Top :=FRainfallEdit.Top;
     FMeanAnnualPrecipitationLabel.Top := FRainfallLabel.Top + 25;
     FMeanAnnualPrecipitationEdit.Top := FMeanAnnualPrecipitationLabel.Top;
     FMeanAnnualPrecipitationEdit.Width := 80;

     FSaltBuildUpRateLabel.Top := FMeanAnnualPrecipitationLabel.Top + 25;
     FSaltBuildUpRateEdit.Top := FSaltBuildUpRateLabel.Top;
     FSaltBuildUpRateEdit.Width := 80;
     FSaltWashOffEfficiencyFactorLabel.Top := FSaltBuildUpRateLabel.Top + 25;
     FSaltWashOffEfficiencyFactorEdit.Top := FSaltWashOffEfficiencyFactorLabel.Top;
     FSaltWashOffEfficiencyFactorEdit.Width := 80;
     FInitialSaltStoreLabel.Top :=  FSaltWashOffEfficiencyFactorLabel.Top + 25;
     FInitialSaltStoreEdit.Top := FInitialSaltStoreLabel.Top;
     FInitialSaltStoreEdit.Width := 80;

     FCreateGrowthFactorButton.Top        := FMineNumberEdt.Top;
     FCreateGrowthFactorButton.Left       := FHydrologyNodeNumberCbx.Left + FHydrologyNodeNumberCbx.Width + 15;
     FCreateGrowthFactorButton.Width      := 100;


     FGrowthFactorControl.Top   := FMineNumberEdt.Top;


     FGrowthFactorControl.Width := 450;
     FGrowthFactorControl.Height := 180;


     FGrowthFactorControl.Left                := FHydrologyNodeNumberCbx.Left + FHydrologyNodeNumberCbx.Width + 10;
     FCreateGrowthFactorButton.Left           := 10;
     FCreateGrowthFactorButton.Top            := 20;
     FCreateGrowthFactorButton.Width          := 100;
     FCreateGrowthFactorButton.Height         := 25;

     FDeleteGrowthFactorButton.Left           := FCreateGrowthFactorButton.Width + FCreateGrowthFactorButton.Left + 10;
     FDeleteGrowthFactorButton.Top            := 20;
     FDeleteGrowthFactorButton.Width          := 100;
     FDeleteGrowthFactorButton.Height         := 25;

  //   FCreateGrowthFactorButton.Left        := 10;
  //   FCreateGrowthFactorButton.Top         := 15;
  //   FCreateGrowthFactorButton.Width       := 100;

     FFactorTypeLabel.Left                := FCreateGrowthFactorButton.Left;
     FFactorTypeLabel.Top                 := FCreateGrowthFactorButton.Top + 55;

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

     FYearAndFactorGrid.Left               := FGrowthFactorControl.Left + 5;
     FYearAndFactorGrid.Top                := FGrowthFactorControl.Top + FGrowthFactorControl.Height + 10;
     FYearAndFactorGrid.DefaultRowHeight   := 20;
     FYearAndFactorGrid.DefaultColWidth    := 40;
     FYearAndFactorGrid.Height             := 48;


  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineDialog.RestoreColourState;
begin
  inherited;

end;

end.
