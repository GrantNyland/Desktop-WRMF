unit UPlanningMineValidator;

interface
uses
 Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UPlanningMineDialog,
  UPlanningMineGrowthFactor,
  UMiningValidator,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

 type
  TPlanningMineValidator = class(TMiningValidator)
    protected
      procedure CreateMemberObjects; override;
      procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
      procedure OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);

      procedure onSaltAssocitionButtonClick(Sender: TObject);
      procedure onMineGrowthButtonClick(Sender: TObject);
      procedure OnDeleteGrowthFactorButtonClick(Sender: TObject);
      procedure OnEditControlEnter(Sender: TObject); override;
      procedure OnEditControltExit(Sender: TObject); override;
      procedure onOpenFileClick(Sender: TObject);
      procedure RePopulateDataViewer; reintroduce;

      procedure UpdateSaltWashOffNo;
      procedure UpdateRainfallFileName;
      procedure UpdateMeanAnnualPrecipitation;
      procedure UpdateSaltBuildUpRate;
      procedure UpdateSaltWashOffEfficiencyFactor;
      procedure UpdateIniSaltStore;

      procedure UpdateIntepolationMethod;
      procedure UpdateYearDataPoints;
      procedure UpDateNoOfYears(ACol, ARow: integer);
      procedure UpDateGrowthFactor(ACol, ARow: integer);
      procedure UpdateYearAndFactorGrid(ARow : integer; AValue : string);


      procedure ValidateSaltWashOffNo(AMine: IPlanningMine);
      procedure ValidateRainfallFileName(AMine: IPlanningMine);
      procedure ValidateMeanAnnualPrecipitation(AMine: IPlanningMine);
      procedure ValidateSaltBuildUpRate(AMine: IPlanningMine);
      procedure ValidateSaltWashOffEfficiencyFactor(AMine: IPlanningMine);
      procedure ValidateIniSaltStore(AMine: IPlanningMine);

      procedure ValidateNoOfPoints(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateInterpolationMedthod(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateNoOfYears(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateGrowthFactor(AGrowthFactor : TPlanningMineGrowthFactor);

    public
      function MiningDialog: TPlanningMineDialog;
      procedure ClearDataViewer; override;
      procedure PopulateDataViewer; override;
      procedure DoContextValidation(AValidationType: TDialogValidationType);override;
  end;


implementation
uses
  SysUtils,
  VCL.Graphics,
  UFileNames,
  UConstants,
  UAbstractFileNamesObject,
  UMiningEvaporationValidator,
  UYieldModelDataGUIForm,
  UMineMonthlyDataDialog,
  UPlanningModelDataObject,
  UErrorHandlingOperations, Math, UNetworkFeaturesData, UPlanningMineData,
  UUtilities,
  UNetworkElementData;


{ TPlannningMineValidator }

procedure TPlanningMineValidator.CreateMemberObjects;
const OPNAME = 'TPlannningMineValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TPlanningMineDialog.Create(FPanelOwner,FAppModules);
    with MiningDialog do
    begin

      MineNumberEdt.FieldProperty                := FAppModules.FieldProperties.FieldProperty('MineNumber');
      MineNumberEdt.OnEnter                      := OnEditControlEnter;

      MineNameEdt.FieldProperty                  := FAppModules.FieldProperties.FieldProperty('MineName');
      MineNameEdt.OnEnter                        := OnEditControlEnter;
      MineNameEdt.OnExit                         := OnEditControltExit;

      NrOfMineCastPitsEdt.FieldProperty          := FAppModules.FieldProperties.FieldProperty('NrOfMineCastPits');
      NrOfMineCastPitsEdt.OnEnter                := OnEditControlEnter;
      NrOfMineCastPitsEdt.OnExit                 := OnEditControltExit;

      NrOfSlurryDumpEdt.FieldProperty            := FAppModules.FieldProperties.FieldProperty('NrOfSlurryDump');
      NrOfSlurryDumpEdt.OnEnter                  := OnEditControlEnter;
      NrOfSlurryDumpEdt.OnExit                   := OnEditControltExit;

      NrOfUnderGroundMiningEdt.FieldProperty     := FAppModules.FieldProperties.FieldProperty('NrOfUnderGroundMining');
      NrOfUnderGroundMiningEdt.OnEnter           := OnEditControlEnter;
      NrOfUnderGroundMiningEdt.OnExit            := OnEditControltExit;

      HydrologyNodeNumberCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('HydrologyNodeNumber');
      HydrologyNodeNumberCbx.OnEnter             := OnEditControlEnter;
      HydrologyNodeNumberCbx.OnExit              := OnEditControltExit;


      MonthlyMeanPanEvapBtn.FieldProperty        := FAppModules.fieldProperties.FieldProperty('RechargeFactors');
      MonthlyMeanPanEvapBtn.OnEnter              := OnEditControlEnter;
      MonthlyMeanPanEvapBtn.OnExit               := OnEditControltExit;
      MonthlyMeanPanEvapBtn.OnClick              := OnMonthlyMeanPanEvaporationClick;
      MonthlyMeanPanEvapBtn.Enabled              := (FAppModules.User.UserRights in CUR_EditData) and
                                                    (not FAppModules.StudyArea.ScenarioLocked);

      MonthlyLakeEvapFactorsBtn.FieldProperty    := FAppModules.fieldProperties.FieldProperty('RechargeFactors');
      MonthlyLakeEvapFactorsBtn.OnEnter          := OnEditControlEnter;
      MonthlyLakeEvapFactorsBtn.OnExit           := OnEditControltExit;
      MonthlyLakeEvapFactorsBtn.OnClick          := OnMonthlyLakeEvaporationClick;
      MonthlyLakeEvapFactorsBtn.Enabled          := (FAppModules.User.UserRights in CUR_EditData) and
                                                    (not FAppModules.StudyArea.ScenarioLocked);

      BeneficiationPlantAreaEdt.FieldProperty    := FAppModules.FieldProperties.FieldProperty('BeneficiationPlantArea');
      BeneficiationPlantAreaEdt.OnEnter          := OnEditControlEnter;
      BeneficiationPlantAreaEdt.OnExit           := OnEditControltExit;

      BeneficiationRunOffFactorEdt.FieldProperty := FAppModules.FieldProperties.FieldProperty('BeneficiationRunOffFactor');
      BeneficiationRunOffFactorEdt.OnEnter       := OnEditControlEnter;
      BeneficiationRunOffFactorEdt.OnExit        := OnEditControltExit;

      MineXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
      MineXCoordEdit.OnEnter       := OnEditControlEnter;
      MineXCoordEdit.OnExit        := OnEditControltExit;

      MineYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
      MineYCoordEdit.OnEnter       := OnEditControlEnter;
      MineYCoordEdit.OnExit        := OnEditControltExit;

      PCDExistsChkBox.FieldProperty       := FAppModules.FieldProperties.FieldProperty('PCDChannelNumber');
      PCDExistsChkBox.OnClick             := PCDExistsChkBoxOnClick;

      NoOfPointsEdit.FieldProperty        := FAppModules.FieldProperties.FieldProperty('NoOfPoints');
      NoOfPointsEdit.OnEnter              := OnEditControlEnter;
      NoOfPointsEdit.OnExit               := OnEditControltExit;

      InterpolationCheckBox.FieldProperty        := FAppModules.FieldProperties.FieldProperty('InterpolationMethod');
      InterpolationCheckBox.OnEnter              := OnEditControlEnter;
      InterpolationCheckBox.OnExit               := OnEditControltExit;


      YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NYR'));
      YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GrowthFactors'));
      YearAndFactorGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      YearAndFactorGrid.OnSelectCell       := OnGridSelectCell;
      YearAndFactorGrid.OnEnter            := OnEditControlEnter;

      GrowthFactorControl.Visible := false;
      CreateGrowthFactorButton.OnClick := onMineGrowthButtonClick;
      DeleteGrowthFactorButton.OnClick := OnDeleteGrowthFactorButtonClick;

      //SaltAssociationButton.OnClick      := onSaltAssocitionButtonClick;
      //MineGrowthButton.OnClick           := onMineGrowthButtonClick;
      AssocSaltWashoffEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('SaltWashOffNo');
      AssocSaltWashoffEdit.OnEnter       := OnEditControlEnter;
      AssocSaltWashoffEdit.OnExit        := OnEditControltExit;

      RainfallEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PlanningMineRainfallFile');
      RainfallEdit.OnEnter       := OnEditControlEnter;
      RainfallEdit.OnExit        := OnEditControltExit;
      OpenRainfallFileBtn.OnClick :=   onOpenFileClick;

      MeanAnnualPrecipitationEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MeanAnnualPrecipitation');
      MeanAnnualPrecipitationEdit.OnEnter       := OnEditControlEnter;
      MeanAnnualPrecipitationEdit.OnExit        := OnEditControltExit;

      SaltBuildUpRateEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('SaltBuildUpRate');
      SaltBuildUpRateEdit.OnEnter       := OnEditControlEnter;
      SaltBuildUpRateEdit.OnExit        := OnEditControltExit;

      SaltWashOffEfficiencyFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('SaltWashOffEfficiencyFactor');
      SaltWashOffEfficiencyFactorEdit.OnEnter       := OnEditControlEnter;
      SaltWashOffEfficiencyFactorEdit.OnExit        := OnEditControltExit;

      InitialSaltStoreEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IniSaltStore');
      InitialSaltStoreEdit.OnEnter       := OnEditControlEnter;
      InitialSaltStoreEdit.OnExit        := OnEditControltExit;

    end
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMiningValidator.DoContextValidation';
var
  LMine      : TPlanningMine;
  LGrowth    : TPlanningMineGrowthFactor;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
    if (LMine <> nil) then
    begin
      LGrowth := LMine.GrowthFactor;
      case AValidationType of
        dvtMineAll :
        begin
          ValidateSaltWashOffNo(LMine);
          ValidateRainfallFileName(LMine);
          ValidateMeanAnnualPrecipitation(LMine);
          ValidateSaltBuildUpRate(LMine);
          ValidateSaltWashOffEfficiencyFactor(LMine);
          ValidateIniSaltStore(LMine);

          ValidateNoOfPoints(LGrowth);
          ValidateInterpolationMedthod(LGrowth);
          ValidateNoOfYears(LGrowth);
          ValidateGrowthFactor(LGrowth);
        end;
        dvtAssocSaltWashoff                  : ValidateSaltWashOffNo(LMine);
        dvtRainfallFileName                  : ValidateRainfallFileName(LMine);
        dvtMAP                               : ValidateMeanAnnualPrecipitation(LMine);
        dvtSaltBuildUpRate                   : ValidateSaltBuildUpRate(LMine);
        dvtSaltWashOffEfficiencyFactor       : ValidateSaltWashOffEfficiencyFactor(LMine);
        dvtIniSaltStore                      : ValidateIniSaltStore(LMine);
        dvtGrowthFactor                      : ValidateGrowthFactor(LGrowth);
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningMineValidator.MiningDialog: TPlanningMineDialog;
const OPNAME = 'TPlannningMineValidator.MiningDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TPlanningMineDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPlanningMineValidator.OnEditControlEnter';
begin
  try
    inherited OnEditControlEnter(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMiningValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender =  MiningDialog.AssocSaltWashoffEdit) ) then
       UpdateSaltWashOffNo
    else
    if ((Sender =  MiningDialog.RainfallEdit)) then
       UpdateRainfallFileName
    else
    if ((Sender =  MiningDialog.MeanAnnualPrecipitationEdit)) then
       UpdateMeanAnnualPrecipitation
    else
    if ((sender = MiningDialog.SaltBuildUpRateEdit)) then
      UpdateSaltBuildUpRate
    else
    if ((sender = MiningDialog.SaltWashOffEfficiencyFactorEdit)) then
      UpdateSaltWashOffEfficiencyFactor
    else
    if ((sender = MiningDialog.InitialSaltStoreEdit)) then
      UpdateIniSaltStore
    else
    if ((sender = MiningDialog.InterpolationCheckBox)) then
      UpdateIntepolationMethod
    else
    if ((sender = MiningDialog.NoOfPointsEdit)) then
      UpdateYearDataPoints;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TMiningValidator.OnGridSelectCell';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineValidator.UpdateIniSaltStore;
const OPNAME = 'TMiningValidator.UpdateSaltWashOffEfficiencyFactor';
var
  LErrorMessage    : string;
  LMine            : TPlanningMine;
begin
  try
    if MiningDialog.InitialSaltStoreEdit.HasValueChanged then
    begin
      LMine :=TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if (LMine <> nil) then
      begin

        with MiningDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('IniSaltStore',InitialSaltStoreEdit.Text, LErrorMessage) then
          begin
            LMine.IniSaltStore := StrToFloat(InitialSaltStoreEdit.Text);
            InitialSaltStoreEdit.SetFieldValue(LMine.IniSaltStore);
            DoContextValidation(dvtIniSaltStore);
          end
          else
            InitialSaltStoreEdit.FieldValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineValidator.UpdateIntepolationMethod;
const OPNAME = 'TPlanningMineValidator.UpdateIntepolationMethod';
var
  LMine : TPlanningMine;
  LGrowth : TPlanningMineGrowthFactor;
  LMessage: string;
  LValue : integer;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                CastMineList.CastMinebyNodeNumber[FIdentifier]);
    if LMine <> nil then
    begin
      with MiningDialog do
      begin
        LValue := integer(MiningDialog.InterpolationCheckBox.Items.Objects[MiningDialog.InterpolationCheckBox.ItemIndex]);
        LGrowth := LMine.GrowthFactor;
        if LGrowth <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
                'InterpolationMethod', IntToStr(LValue), LMessage)) then
          begin
            LGrowth.InterpolationMethod := LValue;
            PopulateDataViewer;
            DoContextValidation(dvtInterpolationMethod);
          end
            else
              InterpolationCheckBox.ValidationError := LMessage;


        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.UpdateYearAndFactorGrid(ARow: integer; AValue: string);
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateYearAndFactorGrid';
var
  LMine : TPlanningMine;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LFieldName : string;
begin
  try
    with MiningDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LFieldName := 'GrowthFactors';
        if ARow = 0 then
        LFieldName := 'NYR';
        if trim(AValue) = '' then
          AValue := '0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            LFieldName,AValue, LMessage)) then
        begin
          LGrowthFactor := LMine.GrowthFactor;
          if LGrowthFactor <> nil then
          begin
            if ARow = 1 then
              LGrowthFactor.GrowthFactorByIndex[YearAndFactorGrid.Col-1]  := StrToInt(AValue)
            else
              LGrowthFactor.NoOfYearsByIndex[YearAndFactorGrid.Col-1]  := StrToInt(AValue);
          end;
          PopulateDataViewer;
          DoContextValidation(dvtGrowthFactor);
        end
        else
          YearAndFactorGrid.ValidationError[YearAndFactorGrid.Col, YearAndFactorGrid.Row,gveCellField ] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.UPdateYearDataPoints;
const OPNAME = 'TPlanningMineValidator.UPdateYearDataPoints';
var
  LMine : TPlanningMine;
  LGrowth : TPlanningMineGrowthFactor;
  LMessage : string;
  LValue : integer;
begin
  try
    with MiningDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LGrowth := LMine.GrowthFactor;
        if LGrowth <> nil then
        begin
          LValue := StrToInt(NoOfPointsEdit.Text);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'NoOfPoints', IntToStr(LValue), LMessage)) then
          begin
            LGrowth.NoOfPoints := LValue;
            PopulateDataViewer;
            DoContextValidation(dvtNoOfPoints);
          end
          else
            NoOfPointsEdit.ContextValidationError := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.UpDateNoOfYears(ACol, ARow: integer);
const OPNAME = 'TPlanningMineValidator.UpDateNoOfYears';
var
  LMine : TPlanningMine;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with MiningDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LCellVal := YearAndFactorGrid.Cells[ACol,ARow];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'NYR',LCellVal, LMessage)) then
        begin
          LGrowthFactor := LMine.GrowthFactor;
          if LGrowthFactor <> nil then
          begin
            LGrowthFactor.NoOfYearsByIndex[ACol -1]  := StrToInt(YearAndFactorGrid.Cells[ACol,ARow]);
            PopulateDataViewer;
          end;
        end
        else
          YearAndFactorGrid.ValidationError[ACol,ARow,gveCellField ] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineValidator.UpDateGrowthFactor(ACol, ARow: integer);
const OPNAME = 'TPlanningMineValidator.UpDateGrowthFactor';
var
  LMine : TPlanningMine;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with MiningDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LCellVal := YearAndFactorGrid.Cells[ACol,ARow];
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'GrowthFactors',LCellVal, LMessage)) then
        begin
          LGrowthFactor := LMine.GrowthFactor;
          if LGrowthFactor <> nil then
          begin
            LGrowthFactor.GrowthFactorByIndex[ACol -1]  := StrToInt(LCellVal);
            PopulateDataViewer;
            DoContextValidation(dvtGrowthFactor);
          end;
        end
        else
          YearAndFactorGrid.ValidationError[ACol,ARow,gveCellField ] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;



procedure TPlanningMineValidator.UpdateMeanAnnualPrecipitation;
const OPNAME = 'TMiningValidator.UpdateMeanAnnualPrecipitation';
var
  LErrorMessage    : string;
  LMine            : TPlanningMine;
begin
  try
    if MiningDialog.MeanAnnualPrecipitationEdit.HasValueChanged then
    begin
      LMine :=TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if (LMine <> nil) then
      begin

        with MiningDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('MeanAnnualPrecipitation',MeanAnnualPrecipitationEdit.Text, LErrorMessage) then
          begin
            LMine.MeanAnnualPrecipitation := StrToFloat(MeanAnnualPrecipitationEdit.Text);
            MeanAnnualPrecipitationEdit.SetFieldValue(LMine.MeanAnnualPrecipitation);
            DoContextValidation(dvtMAP);
          end
          else
            MeanAnnualPrecipitationEdit.FieldValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineValidator.UpdateRainfallFileName;
const OPNAME = 'TMiningValidator.UpdateRainfallFileName';
var
  LErrorMessage    : string;
  LMine            : TPlanningMine;
begin
  try
    if MiningDialog.RainfallEdit.HasValueChanged then
    begin
      LMine :=TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if (LMine <> nil) then
      begin

        with MiningDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('PlanningMineRainfallFile',RainfallEdit.Text, LErrorMessage) then
          begin
            LMine.RainfallFileName := RainfallEdit.Text;
            RainfallEdit.SetFieldValue(LMine.RainfallFileName);
            DoContextValidation(dvtRainfallFileName);
          end
          else
            RainfallEdit.FieldValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineValidator.UpdateSaltBuildUpRate;
const OPNAME = 'TMiningValidator.UpdateSaltBuildUpRate';
var
  LErrorMessage    : string;
  LMine            : TPlanningMine;
begin
  try
    if MiningDialog.SaltBuildUpRateEdit.HasValueChanged then
    begin
      LMine :=TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if (LMine <> nil) then
      begin

        with MiningDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('SaltBuildUpRate',SaltBuildUpRateEdit.Text, LErrorMessage) then
          begin
            LMine.SaltBuildUpRate := StrToFloat(SaltBuildUpRateEdit.Text);
            SaltBuildUpRateEdit.SetFieldValue(LMine.SaltBuildUpRate);
            DoContextValidation(dvtSaltBuildUpRate);
          end
          else
            SaltBuildUpRateEdit.FieldValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineValidator.UpdateSaltWashOffEfficiencyFactor;
const OPNAME = 'TMiningValidator.UpdateSaltWashOffEfficiencyFactor';
var
  LErrorMessage    : string;
  LMine            : TPlanningMine;
begin
  try
    if MiningDialog.SaltWashOffEfficiencyFactorEdit.HasValueChanged then
    begin
      LMine :=TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if (LMine <> nil) then
      begin

        with MiningDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('SaltWashOffEfficiencyFactor',SaltWashOffEfficiencyFactorEdit.Text, LErrorMessage) then
          begin
            LMine.SaltWashOffEfficiencyFactor := StrToFloat(SaltWashOffEfficiencyFactorEdit.Text);
            SaltWashOffEfficiencyFactorEdit.SetFieldValue(LMine.SaltWashOffEfficiencyFactor);
            DoContextValidation(dvtSaltWashOffEfficiencyFactor);
          end
          else
            SaltWashOffEfficiencyFactorEdit.FieldValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineValidator.UpdateSaltWashOffNo;
const OPNAME = 'TMiningValidator.UpdateSaltWashOffNo';
var
  lErrorMessage    : string;
  LMine            : TPlanningMine;
begin
  try
    if MiningDialog.AssocSaltWashoffEdit.HasValueChanged then
    begin
      LMine :=TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if (LMine <> nil) then
      begin

        with MiningDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('SaltWashOffNo',AssocSaltWashoffEdit.Text, lErrorMessage) then
          begin
            LMine.AssocSaltWashoff := StrToInt(AssocSaltWashoffEdit.Text);
            AssocSaltWashoffEdit.SetFieldValue(LMine.AssocSaltWashoff);
            DoContextValidation(dvtAssocSaltWashoff);
          end
          else
            AssocSaltWashoffEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineValidator.ValidateIniSaltStore(AMine: IPlanningMine);
const OPNAME = 'TMiningValidator.ValidateIniSaltStore';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'IniSaltStore');
      InitialSaltStoreEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineValidator.ValidateNoOfPoints(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineValidator.ValidateNoOfPoints';
begin
  try
  with MiningDialog do
  begin
    if AGrowthFactor <> nil then
    begin
      FErrorMessage := '';
      AGrowthFactor.Validate(FErrorMessage,'NoOfPoints');
      NoOfPointsEdit.ContextValidationError := FErrorMessage;
    end;
  end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.ValidateInterpolationMedthod(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineValidator.ValidateInterpolationMedthod';
begin
  try
    with MiningDialog do
    begin
    if AGrowthFactor <> nil then
    begin
      FErrorMessage := '';
      AGrowthFactor.Validate(FErrorMessage,'InterpolationMethod');
      InterpolationCheckBox.ValidationError := FErrorMessage;
    end;
  end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineValidator.ValidateNoOfYears(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineValidator.ValidateNoOfYears';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  LIndex,
  LCol : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    try
      if AGrowthFactor <> nil then
      begin
        if AGrowthFactor.Validate(FErrorMessage,'NYR') then
          for LCol := 1 to AGrowthFactor.NoOfPoints do
            MiningDialog.YearAndFactorGrid.ValidationError[LCol, 0, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LCol := 1 to AGrowthFactor.NoOfPoints do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LCol));
            if (LIndex >= 0) then
              MiningDialog.YearAndFactorGrid.ValidationError[LCol, 0, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              MiningDialog.YearAndFactorGrid.ValidationError[LCol, 0, gveCellContext] := ''
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineValidator.ValidateGrowthFactor(AGrowthFactor: TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineValidator.ValidateGrowthFactor';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  LIndex,
  LCol : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    try
      if AGrowthFactor <> nil then
      begin
        if AGrowthFactor.Validate(FErrorMessage,'GrowthFactors') then
          for LCol := 1 to AGrowthFactor.NoOfPoints do
            MiningDialog.YearAndFactorGrid.ValidationError[LCol, 1, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LCol := 1 to AGrowthFactor.NoOfPoints do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LCol));
            if (LIndex >= 0) then
              MiningDialog.YearAndFactorGrid.ValidationError[LCol, 1, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              MiningDialog.YearAndFactorGrid.ValidationError[LCol, 1, gveCellContext] := ''
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineValidator.ValidateMeanAnnualPrecipitation(AMine: IPlanningMine);
const OPNAME = 'TMiningValidator.ValidateMeanAnnualPrecipitation';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'MeanAnnualPrecipitation');
      MeanAnnualPrecipitationEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;



procedure TPlanningMineValidator.ValidateRainfallFileName(AMine: IPlanningMine);
const OPNAME = 'TMiningValidator.ValidateRainfallFileName';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'PlanningMineRainfallFile');
      RainfallEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;


procedure TPlanningMineValidator.ValidateSaltBuildUpRate(AMine: IPlanningMine);
const OPNAME = 'TMiningValidator.ValidateSaltBuildUpRate';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'SaltBuildUpRate');
      SaltBuildUpRateEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineValidator.ValidateSaltWashOffEfficiencyFactor(AMine: IPlanningMine);
const OPNAME = 'TMiningValidator.ValidateSaltWashOffEfficiencyFactor';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'SaltWashOffEfficiencyFactor');
      SaltWashOffEfficiencyFactorEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;


procedure TPlanningMineValidator.ValidateSaltWashOffNo(AMine: IPlanningMine);
const OPNAME = 'TMiningValidator.ValidateSaltWashOffNo';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'SaltWashOffNo');
      AssocSaltWashoffEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineValidator.onMineGrowthButtonClick;
const OPNAME = 'TPlanningMineValidator.onMineGrowthButtonClick';
var
  LMine : TPlanningMine;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
    LMine.CreatGrowthFactor;
    if LMine.GrowthFactor <> nil then
    PopulateDataViewer;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineValidator.onOpenFileClick(Sender: TObject);
const OPNAME = 'TPlanningMineValidator.onOpenFileClick';
var
  LOpenDialog : TOpenDialog;    // Open dialog variable
  LFileNamesObject: TModelFileNames;
  LPath : string;
  LFileName : string;
begin
  try
    LOpenDialog := nil;
    with MiningDialog do
    begin
      try

            LOpenDialog := TOpenDialog.Create(nil);
            LFileNamesObject :=  TPlanningModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject;
            if LFileNamesObject <> nil then
              LPath := LFileNamesObject.InputFilesPath + CWQT;
            LOpenDialog.InitialDir := LPath;
            LOpenDialog.Options := [ofFileMustExist];
            LOpenDialog.Filter := 'Rain fall file|*.ran';
            LOpenDialog.FilterIndex := 1;
            if LOpenDialog.Execute then
            begin
              LFileName := StringReplace(LOpenDialog.FileName,LPath + '\','',[rfReplaceAll,rfIgnoreCase]);
              RainfallEdit.Text := LFileName;
              UpdateRainfallFileName;
            end;
      finally
        if (Assigned(LOpenDialog)) then
          LOpenDialog.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineValidator.OnDeleteGrowthFactorButtonClick(Sender: TObject);
const OPNAME = 'TPlanningMineValidator.onMineGrowthButtonClick';
var
  LMine : TPlanningMine;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
    if LMine.GrowthFactor <> nil then
      LMine.DeleteGrowthFactor;
    PopulateDataViewer;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.onSaltAssocitionButtonClick;
const OPNAME = 'TPlanningMineValidator.onSaltAssocitionButtonClick';
//var
//  LForm : TYieldModelDataGUIForm;
//  LDialogValidator : TSaltWashOffValidator;
begin
  try
(*     LForm := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
     try
        LForm.Initialise;
        LForm.BtnCancel.Visible := False;
        LForm.LanguageHasChanged;
        LDialogValidator := TSaltWashOffValidator.Create(LForm, FAppModules);
        try

          LDialogValidator.MineIdentifer := FIdentifier;
          LForm.AddModelDataPanel(LDialogValidator.Panel);
          LDialogValidator.Initialise;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
          LForm.ShowModal;
        finally
          LDialogValidator.Free;
        end;
     finally
        LForm.Free;
     end;*)
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.ClearDataViewer;
const OPNAME = 'TPlanningMineValidator.ClearDataViewer';
var
  LRow, LCol : integer;
begin
  try
    with MiningDialog do
    begin
      AssocSaltWashoffEdit.Text := '';
      SaltBuildUpRateEdit.Text := '';
      SaltWashOffEfficiencyFactorEdit.Text := '';
      InitialSaltStoreEdit.Text := '';
      RainfallEdit.Text := '';
      MeanAnnualPrecipitationEdit.Text := '';
      InterpolationCheckBox.Items.Clear;
      InterpolationCheckBox.Items.AddObject('1 - Linear Interpolation', TObject(1));
      InterpolationCheckBox.Items.AddObject('2 - Exponential interpolation', TObject(2));
      InterpolationCheckBox.Text := '';
      YearAndFactorGrid.FixedRows := 0;
      YearAndFactorGrid.FixedCols := 1;
      YearAndFactorGrid.ColCount := 2;
      YearAndFactorGrid.RowCount := 2;
      FactorTypeCombo.Clear;
      NoOfPointsEdit.Text := '';
      YearAndFactorGrid.ColWidths[0] := YearAndFactorGrid.DefaultColWidth*5;
      YearAndFactorGrid.Cells[0,0] := FAppModules.Language.GetString('FYearAndFactorGrid.YearCol');
      YearAndFactorGrid.Cells[0,1] := FAppModules.Language.GetString('FYearAndFactorGrid.GrowthFactorMine');
      for LRow := 0 to 1 do
        for LCol := 1 to 2 do
          YearAndFactorGrid.Cells[LCol, LRow] := '';
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TPlanningMineValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with MiningDialog do
    begin
      if (YearAndFactorGrid = ASender) then
      begin
        if ARow = 0 then
          UpDateNoOfYears(ACol, ARow)
        else
          UpDateGrowthFactor(ACol, ARow);
      end;
        //UpdateYearAndFactorGrid(ARow, Trim(YearAndFactorGrid.Cells[ACol,ARow]));

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineValidator.PopulateDataViewer;
const OPNAME = 'TPlanningMineValidator.PopulateDataViewer';
begin
  inherited;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineValidator.RePopulateDataViewer;
const OPNAME = 'TPlanningMineValidator.RePopulateDataViewer';
var
  LMine : TPlanningMine;
  LIndex : integer;
begin
  inherited;
  try

    with MiningDialog do
    begin
      LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);

      AssocSaltWashoffEdit.SetFieldValue(LMine.AssocSaltWashoff);
      SaltBuildUpRateEdit.SetFieldValue(LMine.SaltBuildUpRate);
      SaltWashOffEfficiencyFactorEdit.SetFieldValue(LMine.SaltWashOffEfficiencyFactor);
      InitialSaltStoreEdit.SetFieldValue(LMine.IniSaltStore);
      RainfallEdit.SetFieldValue(LMine.RainfallFileName);
      MeanAnnualPrecipitationEdit.SetFieldValue(LMine.MeanAnnualPrecipitation);
      GrowthFactorControl.Visible := true;
      CreateGrowthFactorButton.Enabled := True;
      DeleteGrowthFactorButton.Enabled := False;
      if LMine.GrowthFactor <> nil then
      begin
        CreateGrowthFactorButton.Enabled := false;
        DeleteGrowthFactorButton.Enabled := True;

        FactorTypeCombo.Clear;
        FactorTypeCombo.Items.AddObject(LMine.GrowthFactor.Description,TObject(LMine.GrowthFactor.FactorType));
        FactorTypeCombo.SetFieldIndex(FactorTypeCombo.Items.IndexOfObject(TObject(LMine.GrowthFactor.FactorType)));
        NoOfPointsEdit.SetFieldValue(LMine.GrowthFactor.NoOfPoints);
        InterpolationCheckBox.SetFieldIndex(InterpolationCheckBox.items.IndexOfObject(TObject(LMine.GrowthFactor.InterpolationMethod)));

        YearAndFactorGrid.ColCount := LMine.GrowthFactor.NoOfPoints + 1;
        YearAndFactorGrid.Width := (10 + (1 + YearAndFactorGrid.DefaultColWidth) * (YearAndFactorGrid.ColCount-1)) + YearAndFactorGrid.ColWidths[0];

        for LIndex := 1 to LMine.GrowthFactor.NoOfPoints do
        begin


          //YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NYR'));

          YearAndFactorGrid.Cells[LIndex,0] := IntToStr(LMine.GrowthFactor.NoOfYearsByIndex[LIndex - 1]);

          YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GrowthFactors'));
          YearAndFactorGrid.Cells[LIndex,1] := Format(FAppModules.FieldProperties.FieldProperty('GrowthFactors').FormatStringGrid,[LMine.GrowthFactor.GrowthFactorByIndex[LIndex-1]]);
        end;

      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
