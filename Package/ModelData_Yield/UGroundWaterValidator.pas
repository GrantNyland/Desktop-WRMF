unit UGroundWaterValidator;

interface
uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UGroundWaterDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TGroundWaterValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnMonthlyGroundWaterEvaporationBtnClick(Sender: TObject);
    procedure OnMonthlyGroundWaterUsageFactorBtnClick(Sender: TObject);

    procedure UpdateGroundWaterName;
    procedure UpdateGroundWaterDescription;
    procedure UpdateAquiferStorativity;
    procedure UpdateAquiferStaticWaterLevel;
    procedure UpdateUnsaturatedStorageCapacity;
    procedure UpdateInitialUnsaturatedStorage;
    procedure UpdateMaximumDischargeRate;
    procedure UpdateMovingAverageRecharge;
    procedure UpdateMaximumRateOfGroundwaterBaseFlow;
    procedure UpdatePowerHeadDifferenceBaseFlowEquation;
    procedure UpdateMaximumHydrologicalGradient;
    procedure UpdateAquiferTransmissivity;
    procedure UpdateBoreHoleDistanceToRiver;
    procedure UpdateMaximumGroundwaterAbstraction;
    procedure UpdateParameterK2;
    procedure UpdateParameterK3;
    procedure UpdateGroundWaterEvaporationArea;
    procedure UpdateRefNodeNumberCbx;

    procedure ValidateGroundWaterDescription(AGroundWater : IGroundWater);
    procedure ValidateGroundWaterName(AGroundWater : IGroundWater);
    procedure ValidateAquiferStorativity(AGroundWater : IGroundWater);
    procedure ValidateAquiferStaticWaterLevel(AGroundWater : IGroundWater);
    procedure ValidateUnsaturatedStorageCapacity(AGroundWater : IGroundWater);
    procedure ValidateInitialUnsaturatedStorage(AGroundWater : IGroundWater);
    procedure ValidateMaximumDischargeRate(AGroundWater : IGroundWater);
    procedure ValidateMovingAverageRecharge(AGroundWater : IGroundWater);
    procedure ValidateMaximumRateOfGroundwaterBaseFlow(AGroundWater : IGroundWater);
    procedure ValidatePowerHeadDifferenceBaseFlowEquation(AGroundWater : IGroundWater);
    procedure ValidateMaximumHydrologicalGradient(AGroundWater : IGroundWater);
    procedure ValidateAquiferTransmissivity(AGroundWater : IGroundWater);
    procedure ValidateBoreHoleDistanceToRiver(AGroundWater : IGroundWater);
    procedure ValidateMaximumGroundwaterAbstraction(AGroundWater : IGroundWater);
    procedure ValidateParameterK2(AGroundWater : IGroundWater);
    procedure ValidateParameterK3(AGroundWater : IGroundWater);
    procedure ValidateGroundWaterEvaporationArea(AGroundWater : IGroundWater);
    procedure RePopulateDataViewer;
    procedure PopulateRefNodeNumber;

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function GroundWaterDialog: TGroundWaterDialog;
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
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  Math,
  Contnrs,
  UGroundWaterEvaporationValidator,
  UNetworkFeaturesData,
  UNetworkElementData,
  UGroundWater,
  UParameterData;

{ TGroundWaterValidator }

procedure TGroundWaterValidator.ClearDataViewer;
const OPNAME = 'TGroundWaterValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with GroundWaterDialog do
    begin
      GroundWaterNodeNumberEdt.SetFieldValue('-1');
      GroundWaterNameEdt.SetFieldValue('');
      GroundWaterDescriptionEdt.SetFieldValue('');
      AquiferStorativityEdt.SetFieldValue('-1');
      AquiferStaticWaterLevelEdt.SetFieldValue('-1');
      UnsaturatedStorageCapacityEdt.SetFieldValue('-1');
      InitialUnsaturatedStorageEdt.SetFieldValue('-1');
      MaximumAquiferRechargeEdt.SetFieldValue('-1');
      MovingAverageRechargeEdt.SetFieldValue('-1');
      MaximumBaseFlowRateEdt.SetFieldValue('-1');
      HeadBaseFlowPowerEdt.SetFieldValue('-1');
      MaximumHydrologicalGradientEdt.SetFieldValue('-1');
      AquiferTransmissivityEdt.SetFieldValue('-1');
      BoreholeDistanceToRiverEdt.SetFieldValue('-1');
      MaximumWaterAbstractionEdt.SetFieldValue('-1');
      Parameterk2Edt.SetFieldValue('-1');
      Parameterk3Edt.SetFieldValue('-1');
      WaterEvaporationAreaEdt.SetFieldValue('-1');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.CreateMemberObjects;
const OPNAME = 'TGroundWaterValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TGroundWaterDialog.Create(FPanelOwner,FAppModules);
    with GroundWaterDialog do
    begin
      GroundWaterNodeNumberEdt.FieldProperty       := FAppModules.FieldProperties.FieldProperty('GroundWaterNodeNumber');
      GroundWaterNodeNumberEdt.OnEnter             := OnEditControlEnter;
      GroundWaterNodeNumberEdt.OnExit              := OnEditControltExit;

      GroundWaterNameEdt.FieldProperty             := FAppModules.FieldProperties.FieldProperty('GroundWaterName');
      GroundWaterNameEdt.OnEnter                   := OnEditControlEnter;
      GroundWaterNameEdt.OnExit                    := OnEditControltExit;

      GroundWaterDescriptionEdt.FieldProperty      := FAppModules.FieldProperties.FieldProperty('GroundWaterDescription');
      GroundWaterDescriptionEdt.OnEnter            := OnEditControlEnter;
      GroundWaterDescriptionEdt.OnExit             := OnEditControltExit;

      AquiferStorativityEdt.FieldProperty          := FAppModules.FieldProperties.FieldProperty('AquiferStorativity');
      AquiferStorativityEdt.OnEnter                := OnEditControlEnter;
      AquiferStorativityEdt.OnExit                 := OnEditControltExit;

      AquiferStaticWaterLevelEdt.FieldProperty     := FAppModules.FieldProperties.FieldProperty('AquiferStaticWaterlevel');
      AquiferStaticWaterLevelEdt.OnEnter           := OnEditControlEnter;
      AquiferStaticWaterLevelEdt.OnExit            := OnEditControltExit;

      UnsaturatedStorageCapacityEdt.FieldProperty  := FAppModules.FieldProperties.FieldProperty('UnsaturatedStorageCapacity');
      UnsaturatedStorageCapacityEdt.OnEnter        := OnEditControlEnter;
      UnsaturatedStorageCapacityEdt.OnExit         := OnEditControltExit;

      InitialUnsaturatedStorageEdt.FieldProperty   := FAppModules.FieldProperties.FieldProperty('InitialUnsaturatedStorage');
      InitialUnsaturatedStorageEdt.OnEnter         := OnEditControlEnter;
      InitialUnsaturatedStorageEdt.OnExit          := OnEditControltExit;

      MaximumAquiferRechargeEdt.FieldProperty      := FAppModules.FieldProperties.FieldProperty('MaximumDischargeRate');
      MaximumAquiferRechargeEdt.OnEnter            := OnEditControlEnter;
      MaximumAquiferRechargeEdt.OnExit             := OnEditControltExit;

      MovingAverageRechargeEdt.FieldProperty       := FAppModules.FieldProperties.FieldProperty('MovingAverageRecharge');
      MovingAverageRechargeEdt.OnEnter             := OnEditControlEnter;
      MovingAverageRechargeEdt.OnExit              := OnEditControltExit;

      MaximumBaseFlowRateEdt.FieldProperty         := FAppModules.FieldProperties.FieldProperty('MaximumRateOfGroundwaterBaseFlow');
      MaximumBaseFlowRateEdt.OnEnter               := OnEditControlEnter;
      MaximumBaseFlowRateEdt.OnExit                := OnEditControltExit;

      HeadBaseFlowPowerEdt.FieldProperty           := FAppModules.FieldProperties.FieldProperty('PowerHeadDifferenceBaseFlowEquation');
      HeadBaseFlowPowerEdt.OnEnter                 := OnEditControlEnter;
      HeadBaseFlowPowerEdt.OnExit                  := OnEditControltExit;

      MaximumHydrologicalGradientEdt.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaximumHydrologicalGradient');
      MaximumHydrologicalGradientEdt.OnEnter       := OnEditControlEnter;
      MaximumHydrologicalGradientEdt.OnExit        := OnEditControltExit;

      AquiferTransmissivityEdt.FieldProperty       := FAppModules.FieldProperties.FieldProperty('AquiferTransmissivity');
      AquiferTransmissivityEdt.OnEnter             := OnEditControlEnter;
      AquiferTransmissivityEdt.OnExit              := OnEditControltExit;

      BoreholeDistanceToRiverEdt.FieldProperty     := FAppModules.FieldProperties.FieldProperty('BoreHoleDistanceToRiver');
      BoreholeDistanceToRiverEdt.OnEnter           := OnEditControlEnter;
      BoreholeDistanceToRiverEdt.OnExit            := OnEditControltExit;

      MaximumWaterAbstractionEdt.FieldProperty     := FAppModules.FieldProperties.FieldProperty('MaximumGroundwaterAbstraction');
      MaximumWaterAbstractionEdt.OnEnter           := OnEditControlEnter;
      MaximumWaterAbstractionEdt.OnExit            := OnEditControltExit;

      Parameterk2Edt.FieldProperty                 := FAppModules.FieldProperties.FieldProperty('ParameterK2');
      Parameterk2Edt.OnEnter                       := OnEditControlEnter;
      Parameterk2Edt.OnExit                        := OnEditControltExit;

      Parameterk3Edt.FieldProperty                 := FAppModules.FieldProperties.FieldProperty('ParameterK3');
      Parameterk3Edt.OnEnter                       := OnEditControlEnter;
      Parameterk3Edt.OnExit                        := OnEditControltExit;

      MonthlyWaterEvaporationBtn.FieldProperty     := FAppModules.fieldProperties.FieldProperty('MonthlyWaterEvaporation');
      MonthlyWaterEvaporationBtn.OnEnter           := OnEditControlEnter;
      MonthlyWaterEvaporationBtn.OnExit            := OnEditControltExit;
      MonthlyWaterEvaporationBtn.OnClick           := OnMonthlyGroundWaterEvaporationBtnClick;
      MonthlyWaterEvaporationBtn.Enabled           := (FAppModules.User.UserRights in CUR_EditData) and
                                                    (not FAppModules.StudyArea.ScenarioLocked);

      MonthlyUsageFactorBtn.FieldProperty          := FAppModules.fieldProperties.FieldProperty('MonthlyWaterUsageFactors');
      MonthlyUsageFactorBtn.OnEnter                := OnEditControlEnter;
      MonthlyUsageFactorBtn.OnExit                 := OnEditControltExit;
      MonthlyUsageFactorBtn.OnClick                := OnMonthlyGroundWaterUsageFactorBtnClick;
      MonthlyUsageFactorBtn.Enabled                := (FAppModules.User.UserRights in CUR_EditData) and
                                                    (not FAppModules.StudyArea.ScenarioLocked);

      WaterEvaporationAreaEdt.FieldProperty        := FAppModules.FieldProperties.FieldProperty('GroundWaterEvaporationArea');
      WaterEvaporationAreaEdt.OnEnter              := OnEditControlEnter;
      WaterEvaporationAreaEdt.OnExit               := OnEditControltExit;

      RefNodeNumberCbx.OnEnter                  := OnEditControlEnter;
      RefNodeNumberCbx.OnExit                   := OnEditControltExit;
      RefNodeNumberCbx.FieldProperty            := FAppModules.FieldProperties.FieldProperty('GroundWaterRefNodeNumber');
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.DestroyMemberObjects;
const OPNAME = 'TGroundWaterValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterValidator.GroundWaterDialog: TGroundWaterDialog;
const OPNAME = 'TGroundWaterValidator.GroundWaterDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TGroundWaterDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterValidator.Initialise: boolean;
const OPNAME = 'TGroundWaterValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterValidator.LanguageHasChanged: boolean;
const OPNAME = 'TGroundWaterValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.language.GetString('TabCaption.Groundwater');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterValidator.SaveState: boolean;
const OPNAME = 'TGroundWaterValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterValidator.StudyHasChanged: boolean;
const OPNAME = 'TGroundWaterValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateGroundWaterDescription;
const OPNAME = 'TGroundWaterValidator.UpdateGroundWaterDescription';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterDescription',
            GroundWaterDescriptionEdt.Text,LMessage)) then
        begin
          GroundWaterDescriptionEdt.FieldValidationError := LMessage;
          LGroundWater.Description := GroundWaterDescriptionEdt.Text;
          GroundWaterDescriptionEdt.SetFieldValue(LGroundWater.Description);
          DoContextValidation(dvtGroundWaterDescription);
        end
        else
          GroundWaterDescriptionEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateGroundWaterName;
const OPNAME = 'TGroundWaterValidator.UpdateGroundWaterName';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterName',
            GroundWaterNameEdt.Text,LMessage)) then
        begin
          GroundWaterNameEdt.FieldValidationError := LMessage;
          LGroundWater.Name := GroundWaterNameEdt.Text;
          GroundWaterNameEdt.SetFieldValue(LGroundWater.Name);
          DoContextValidation(dvtGroundWaterName);
        end
        else
          GroundWaterNameEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateAquiferStorativity;
const OPNAME = 'TGroundWaterValidator.UpdateAquiferStorativity';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('AquiferStorativity',
            AquiferStorativityEdt.Text,LMessage)) then
        begin
          AquiferStorativityEdt.FieldValidationError := LMessage;
          LGroundWater.AquiferStorativity := StrToFloat(AquiferStorativityEdt.Text);
          AquiferStorativityEdt.SetFieldValue(LGroundWater.AquiferStorativity);
          DoContextValidation(dvtAquiferStorativity);
        end
        else
          AquiferStorativityEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateAquiferStaticWaterLevel;
const OPNAME = 'TGroundWaterValidator.UpdateAquiferStaticWaterLevel';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('AquiferStaticWaterlevel',
            AquiferStaticWaterLevelEdt.Text,LMessage)) then
        begin
          AquiferStaticWaterLevelEdt.FieldValidationError := LMessage;
          LGroundWater.AquiferStaticWaterLevel := StrToFloat(AquiferStaticWaterLevelEdt.Text);
          AquiferStaticWaterLevelEdt.SetFieldValue(LGroundWater.AquiferStaticWaterLevel);
          DoContextValidation(dvtAquiferStaticWaterLevel);
        end
        else
          AquiferStaticWaterLevelEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateUnsaturatedStorageCapacity;
const OPNAME = 'TGroundWaterValidator.UpdateUnsaturatedStorageCapacity';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('UnsaturatedStorageCapacity',
            UnsaturatedStorageCapacityEdt.Text,LMessage)) then
        begin
          UnsaturatedStorageCapacityEdt.FieldValidationError := LMessage;
          LGroundWater.UnsaturatedStorageCapacity := StrToFloat(UnsaturatedStorageCapacityEdt.Text);
          UnsaturatedStorageCapacityEdt.SetFieldValue(LGroundWater.UnsaturatedStorageCapacity);
          DoContextValidation(dvtUnsaturatedStorageCapacity);
        end
        else
          UnsaturatedStorageCapacityEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateInitialUnsaturatedStorage;
const OPNAME = 'TGroundWaterValidator.UpdateInitialUnsaturatedStorage';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('InitialUnsaturatedStorage',
            InitialUnsaturatedStorageEdt.Text,LMessage)) then
        begin
          InitialUnsaturatedStorageEdt.FieldValidationError := LMessage;
          LGroundWater.InitialUnsaturatedStorage := StrToFloat(InitialUnsaturatedStorageEdt.Text);
          InitialUnsaturatedStorageEdt.SetFieldValue(LGroundWater.InitialUnsaturatedStorage);
          DoContextValidation(dvtInitialUnsaturatedStorage);
        end
        else
          InitialUnsaturatedStorageEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateMaximumDischargeRate;
const OPNAME = 'TGroundWaterValidator.UpdateMaximumDischargeRate';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('MaximumDischargeRate',
            MaximumAquiferRechargeEdt.Text,LMessage)) then
        begin
          MaximumAquiferRechargeEdt.FieldValidationError := LMessage;
          LGroundWater.MaximumDischargeRate := StrToFloat(MaximumAquiferRechargeEdt.Text);
          MaximumAquiferRechargeEdt.SetFieldValue(LGroundWater.MaximumDischargeRate);
          DoContextValidation(dvtMaximumDischargeRate);
        end
        else
          MaximumAquiferRechargeEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateMovingAverageRecharge;
const OPNAME = 'TGroundWaterValidator.UpdateMovingAverageRecharge';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('MovingAverageRecharge',
            MovingAverageRechargeEdt.Text,LMessage)) then
        begin
          MovingAverageRechargeEdt.FieldValidationError := LMessage;
          LGroundWater.MovingAverageRecharge := StrToFloat(MovingAverageRechargeEdt.Text);
          MovingAverageRechargeEdt.SetFieldValue(LGroundWater.MovingAverageRecharge);
          DoContextValidation(dvtMovingAverageRecharge);
        end
        else
          MovingAverageRechargeEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateMaximumRateOfGroundwaterBaseFlow;
const OPNAME = 'TGroundWaterValidator.UpdateMaximumRateOfGroundwaterBaseFlow';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('MaximumRateOfGroundwaterBaseFlow',
            MaximumBaseFlowRateEdt.Text,LMessage)) then
        begin
          MaximumBaseFlowRateEdt.FieldValidationError := LMessage;
          LGroundWater.MaximumRateOfGroundwaterBaseFlow := StrToFloat(MaximumBaseFlowRateEdt.Text);
          MaximumBaseFlowRateEdt.SetFieldValue(LGroundWater.MaximumRateOfGroundwaterBaseFlow);
          DoContextValidation(dvtMaximumRateOfGroundwaterBaseFlow);
        end
        else
          MaximumBaseFlowRateEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdatePowerHeadDifferenceBaseFlowEquation;
const OPNAME = 'TGroundWaterValidator.UpdatePowerHeadDifferenceBaseFlowEquation';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PowerHeadDifferenceBaseFlowEquation',
            HeadBaseFlowPowerEdt.Text,LMessage)) then
        begin
          HeadBaseFlowPowerEdt.FieldValidationError := LMessage;
          LGroundWater.PowerHeadDifferenceBaseFlowEquation := StrToFloat(HeadBaseFlowPowerEdt.Text);
          HeadBaseFlowPowerEdt.SetFieldValue(LGroundWater.PowerHeadDifferenceBaseFlowEquation);
          DoContextValidation(dvtPowerHeadDifferenceBaseFlowEquation);
        end
        else
          HeadBaseFlowPowerEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateMaximumHydrologicalGradient;
const OPNAME = 'TGroundWaterValidator.UpdateMaximumHydrologicalGradient';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('MaximumHydrologicalGradient',
            MaximumHydrologicalGradientEdt.Text,LMessage)) then
        begin
          MaximumHydrologicalGradientEdt.FieldValidationError := LMessage;
          LGroundWater.MaximumHydrologicalGradient := StrToFloat(MaximumHydrologicalGradientEdt.Text);
          MaximumHydrologicalGradientEdt.SetFieldValue(LGroundWater.MaximumHydrologicalGradient);
          DoContextValidation(dvtMaximumHydrologicalGradient);
        end
        else
          MaximumHydrologicalGradientEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateAquiferTransmissivity;
const OPNAME = 'TGroundWaterValidator.UpdateAquiferTransmissivity';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('AquiferTransmissivity',
            AquiferTransmissivityEdt.Text,LMessage)) then
        begin
          AquiferTransmissivityEdt.FieldValidationError := LMessage;
          LGroundWater.AquiferTransmissivity := StrToFloat(AquiferTransmissivityEdt.Text);
          AquiferTransmissivityEdt.SetFieldValue(LGroundWater.AquiferTransmissivity);
          DoContextValidation(dvtAquiferTransmissivity);
        end
        else
          AquiferTransmissivityEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateBoreHoleDistanceToRiver;
const OPNAME = 'TGroundWaterValidator.UpdateBoreHoleDistanceToRiver';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('BoreHoleDistanceToRiver',
            BoreholeDistanceToRiverEdt.Text,LMessage)) then
        begin
          BoreholeDistanceToRiverEdt.FieldValidationError := LMessage;
          LGroundWater.BoreHoleDistanceToRiver := StrToFloat(BoreholeDistanceToRiverEdt.Text);
          BoreholeDistanceToRiverEdt.SetFieldValue(LGroundWater.BoreHoleDistanceToRiver);
          DoContextValidation(dvtBoreHoleDistanceToRiver);
        end
        else
          BoreholeDistanceToRiverEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateMaximumGroundwaterAbstraction;
const OPNAME = 'TGroundWaterValidator.UpdateMaximumGroundwaterAbstraction';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('MaximumGroundwaterAbstraction',
            MaximumWaterAbstractionEdt.Text,LMessage)) then
        begin
          MaximumWaterAbstractionEdt.FieldValidationError := LMessage;
          LGroundWater.MaximumGroundwaterAbstraction := StrToFloat(MaximumWaterAbstractionEdt.Text);
          MaximumWaterAbstractionEdt.SetFieldValue(LGroundWater.MaximumGroundwaterAbstraction);
          DoContextValidation(dvtMaximumGroundwaterAbstraction);
        end
        else
          MaximumWaterAbstractionEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateParameterK2;
const OPNAME = 'TGroundWaterValidator.UpdateParameterK2';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('ParameterK2',
            Parameterk2Edt.Text,LMessage)) then
        begin
          Parameterk2Edt.FieldValidationError := LMessage;
          LGroundWater.ParameterK2 := StrToFloat(Parameterk2Edt.Text);
          Parameterk2Edt.SetFieldValue(LGroundWater.ParameterK2);
          DoContextValidation(dvtParameterK2);
        end
        else
          Parameterk2Edt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateParameterK3;
const OPNAME = 'TGroundWaterValidator.UpdateParameterK3';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('ParameterK3',
            Parameterk3Edt.Text,LMessage)) then
        begin
          Parameterk3Edt.FieldValidationError := LMessage;
          LGroundWater.ParameterK3 := StrToFloat(Parameterk3Edt.Text);
          Parameterk3Edt.SetFieldValue(LGroundWater.ParameterK3);
          DoContextValidation(dvtParameterK3);
        end
        else
          Parameterk3Edt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateGroundWaterEvaporationArea;
const OPNAME = 'TGroundWaterValidator.UpdateGroundWaterEvaporationArea';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterEvaporationArea',
            WaterEvaporationAreaEdt.Text,LMessage)) then
        begin
          WaterEvaporationAreaEdt.FieldValidationError := LMessage;
          LGroundWater.GroundWaterEvaporationArea := StrToFloat(WaterEvaporationAreaEdt.Text);
          WaterEvaporationAreaEdt.SetFieldValue(LGroundWater.GroundWaterEvaporationArea);
          DoContextValidation(dvtGroundWaterEvaporationArea);
        end
        else
          WaterEvaporationAreaEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.ValidateGroundWaterDescription(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateGroundWaterDescription';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'GroundWaterDescription');
      GroundWaterDescriptionEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateGroundWaterName(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateGroundWaterName';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'GroundWaterName');
      GroundWaterNameEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateAquiferStorativity(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateAquiferStorativity';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'AquiferStorativity');
      AquiferStorativityEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateAquiferStaticWaterLevel(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateAquiferStaticWaterLevel';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'AquiferStaticWaterlevel');
      AquiferStaticWaterLevelEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateUnsaturatedStorageCapacity(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateUnsaturatedStorageCapacity';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'UnsaturatedStorageCapacity');
      UnsaturatedStorageCapacityEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateInitialUnsaturatedStorage(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateInitialUnsaturatedStorage';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'InitialUnsaturatedStorage');
      InitialUnsaturatedStorageEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateMaximumDischargeRate(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateMaximumDischargeRate';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'MaximumDischargeRate');
      MaximumAquiferRechargeEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateMovingAverageRecharge(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateMovingAverageRecharge';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'MovingAverageRecharge');
      MovingAverageRechargeEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateMaximumRateOfGroundwaterBaseFlow(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateMaximumRateOfGroundwaterBaseFlow';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'MaximumRateOfGroundwaterBaseFlow');
      MaximumBaseFlowRateEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidatePowerHeadDifferenceBaseFlowEquation(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidatePowerHeadDifferenceBaseFlowEquation';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PowerHeadDifferenceBaseFlowEquation');
      HeadBaseFlowPowerEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateMaximumHydrologicalGradient(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateMaximumHydrologicalGradient';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'MaximumHydrologicalGradient');
      MaximumHydrologicalGradientEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateAquiferTransmissivity(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateAquiferTransmissivity';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'AquiferTransmissivity');
      AquiferTransmissivityEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateBoreHoleDistanceToRiver(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateBoreHoleDistanceToRiver';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'BoreHoleDistanceToRiver');
      BoreholeDistanceToRiverEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateMaximumGroundwaterAbstraction(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateMaximumGroundwaterAbstraction';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'MaximumGroundwaterAbstraction');
      MaximumWaterAbstractionEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateParameterK2(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateParameterK2';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'ParameterK2');
      Parameterk2Edt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateParameterK3(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateParameterK3';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'ParameterK3');
      Parameterk3Edt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.ValidateGroundWaterEvaporationArea(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterValidator.ValidateGroundWaterEvaporationArea';
begin
  try
    with GroundWaterDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'GroundWaterEvaporationArea');
      WaterEvaporationAreaEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TGroundWaterValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TGroundWaterValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender = GroundWaterDialog.GroundWaterDescriptionEdt) AND
       (GroundWaterDialog.GroundWaterDescriptionEdt.HasValueChanged)) then
       UpdateGroundWaterDescription
    else
    if ((Sender =  GroundWaterDialog.GroundWaterNameEdt) AND
       (GroundWaterDialog.GroundWaterNameEdt.HasValueChanged)) then
       UpdateGroundWaterName
    else
    if ((Sender =  GroundWaterDialog.AquiferStorativityEdt) AND
       (GroundWaterDialog.AquiferStorativityEdt.HasValueChanged)) then
       UpdateAquiferStorativity
    else
    if ((Sender =  GroundWaterDialog.AquiferStaticWaterLevelEdt) AND
       (GroundWaterDialog.AquiferStaticWaterLevelEdt.HasValueChanged)) then
       UpdateAquiferStaticWaterLevel
    else
    if ((Sender =  GroundWaterDialog.UnsaturatedStorageCapacityEdt) AND
       (GroundWaterDialog.UnsaturatedStorageCapacityEdt.HasValueChanged)) then
       UpdateUnsaturatedStorageCapacity
    else
    if ((Sender =  GroundWaterDialog.InitialUnsaturatedStorageEdt) AND
       (GroundWaterDialog.InitialUnsaturatedStorageEdt.HasValueChanged)) then
       UpdateInitialUnsaturatedStorage
    else
    if ((Sender =  GroundWaterDialog.MaximumAquiferRechargeEdt) AND
       (GroundWaterDialog.MaximumAquiferRechargeEdt.HasValueChanged)) then
       UpdateMaximumDischargeRate
    else
    if ((Sender = GroundWaterDialog.MovingAverageRechargeEdt) AND
       (GroundWaterDialog.MovingAverageRechargeEdt.HasValueChanged)) then
       UpdateMovingAverageRecharge
    else
    if ((Sender =  GroundWaterDialog.MaximumBaseFlowRateEdt) AND
       (GroundWaterDialog.MaximumBaseFlowRateEdt.HasValueChanged)) then
       UpdateMaximumRateOfGroundwaterBaseFlow
    else
    if ((Sender =  GroundWaterDialog.HeadBaseFlowPowerEdt) AND
       (GroundWaterDialog.HeadBaseFlowPowerEdt.HasValueChanged)) then
       UpdatePowerHeadDifferenceBaseFlowEquation
    else
    if ((Sender =  GroundWaterDialog.MaximumHydrologicalGradientEdt) AND
       (GroundWaterDialog.MaximumHydrologicalGradientEdt.HasValueChanged)) then
       UpdateMaximumHydrologicalGradient
    else
    if ((Sender =  GroundWaterDialog.AquiferTransmissivityEdt) AND
       (GroundWaterDialog.AquiferTransmissivityEdt.HasValueChanged)) then
       UpdateAquiferTransmissivity
    else
    if ((Sender =  GroundWaterDialog.BoreholeDistanceToRiverEdt) AND
       (GroundWaterDialog.BoreholeDistanceToRiverEdt.HasValueChanged)) then
       UpdateBoreHoleDistanceToRiver
    else
    if ((Sender =  GroundWaterDialog.MaximumWaterAbstractionEdt) AND
       (GroundWaterDialog.MaximumWaterAbstractionEdt.HasValueChanged)) then
       UpdateMaximumGroundwaterAbstraction
    else
    if ((Sender =  GroundWaterDialog.Parameterk2Edt) AND
       (GroundWaterDialog.Parameterk2Edt.HasValueChanged)) then
       UpdateParameterK2
    else
    if ((Sender =  GroundWaterDialog.Parameterk3Edt) AND
       (GroundWaterDialog.Parameterk3Edt.HasValueChanged)) then
       UpdateParameterK3
    else
    if ((Sender =  GroundWaterDialog.WaterEvaporationAreaEdt) AND
       (GroundWaterDialog.WaterEvaporationAreaEdt.HasValueChanged)) then
       UpdateGroundWaterEvaporationArea
    else
    if((Sender = GroundWaterDialog.RefNodeNumberCbx) AND
       (GroundWaterDialog.RefNodeNumberCbx.HasValueChanged ))then
        UpdateRefNodeNumberCbx;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TGroundWaterValidator.DoContextValidation';
var
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[Fidentifier];
    if (LGroundWater <> nil) then
    begin
      case AValidationType of
        dvtGroundWaterAll :
        begin
          ValidateGroundWaterDescription(LGroundWater);
          ValidateGroundWaterName(LGroundWater);
          ValidateAquiferStorativity(LGroundWater);
          ValidateAquiferStaticWaterLevel(LGroundWater);
          ValidateUnsaturatedStorageCapacity(LGroundWater);
          ValidateInitialUnsaturatedStorage(LGroundWater);
          ValidateMaximumDischargeRate(LGroundWater);
          ValidateMovingAverageRecharge(LGroundWater);
          ValidateMaximumRateOfGroundwaterBaseFlow(LGroundWater);
          ValidatePowerHeadDifferenceBaseFlowEquation(LGroundWater);
          ValidateMaximumHydrologicalGradient(LGroundWater);
          ValidateAquiferTransmissivity(LGroundWater);
          ValidateBoreHoleDistanceToRiver(LGroundWater);
          ValidateMaximumGroundwaterAbstraction(LGroundWater);
          ValidateParameterK2(LGroundWater);
          ValidateParameterK3(LGroundWater);
          ValidateGroundWaterEvaporationArea(LGroundWater);
        end;
        dvtGroundWaterDescription              : ValidateGroundWaterDescription(LGroundWater);
        dvtGroundWaterName                     : ValidateGroundWaterName(LGroundWater);
        dvtAquiferStorativity                  : ValidateAquiferStorativity(LGroundWater);
        dvtAquiferStaticWaterLevel             : ValidateAquiferStaticWaterLevel(LGroundWater);
        dvtUnsaturatedStorageCapacity          : ValidateUnsaturatedStorageCapacity(LGroundWater);
        dvtInitialUnsaturatedStorage           : ValidateInitialUnsaturatedStorage(LGroundWater);
        dvtMaximumDischargeRate                : ValidateMaximumDischargeRate(LGroundWater);
        dvtMovingAverageRecharge               : ValidateMovingAverageRecharge(LGroundWater);
        dvtMaximumRateOfGroundwaterBaseFlow    : ValidateMaximumRateOfGroundwaterBaseFlow(LGroundWater);
        dvtPowerHeadDifferenceBaseFlowEquation : ValidatePowerHeadDifferenceBaseFlowEquation(LGroundWater);
        dvtMaximumHydrologicalGradient         : ValidateMaximumHydrologicalGradient(LGroundWater);
        dvtAquiferTransmissivity               : ValidateAquiferTransmissivity(LGroundWater);
        dvtBoreHoleDistanceToRiver             : ValidateBoreHoleDistanceToRiver(LGroundWater);
        dvtMaximumGroundwaterAbstraction       : ValidateMaximumGroundwaterAbstraction(LGroundWater);
        dvtParameterK2                         : ValidateParameterK2(LGroundWater);
        dvtParameterK3                         : ValidateParameterK3(LGroundWater);
        dvtGroundWaterEvaporationArea          : ValidateGroundWaterEvaporationArea(LGroundWater);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.PopulateDataViewer;
const OPNAME = 'TGroundWaterValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtGroundWaterAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.RePopulateDataViewer;
const OPNAME = 'TGroundWaterValidator.RePopulateDataViewer';
var
  LGroundWater : IGroundWater;
begin
  try
    if (FIdentifier > 0) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                   CastGroundWaterList.GroundWaterByID[FIdentifier];
      if (LGroundWater <> nil) then
      begin
        with GroundWaterDialog do
        begin
          GroundWaterNodeNumberEdt.SetFieldValue(LGroundWater.AquiferNodeNr);
          GroundWaterNameEdt.SetFieldValue(LGroundWater.Name);
          GroundWaterDescriptionEdt.SetFieldValue(LGroundWater.Description);
          AquiferStorativityEdt.SetFieldValue(LGroundWater.AquiferStorativity);
          AquiferStaticWaterLevelEdt.SetFieldValue(LGroundWater.AquiferStaticWaterLevel);
          UnsaturatedStorageCapacityEdt.SetFieldValue(LGroundWater.UnsaturatedStorageCapacity);
          InitialUnsaturatedStorageEdt.SetFieldValue(LGroundWater.InitialUnsaturatedStorage);
          MaximumAquiferRechargeEdt.SetFieldValue(LGroundWater.MaximumDischargeRate);
          MovingAverageRechargeEdt.SetFieldValue(LGroundWater.MovingAverageRecharge);
          MaximumBaseFlowRateEdt.SetFieldValue(LGroundWater.MaximumRateOfGroundwaterBaseFlow);
          HeadBaseFlowPowerEdt.SetFieldValue(LGroundWater.PowerHeadDifferenceBaseFlowEquation);
          MaximumHydrologicalGradientEdt.SetFieldValue(LGroundWater.MaximumHydrologicalGradient);
          AquiferTransmissivityEdt.SetFieldValue(LGroundWater.AquiferTransmissivity);
          BoreholeDistanceToRiverEdt.SetFieldValue(LGroundWater.BoreHoleDistanceToRiver);
          MaximumWaterAbstractionEdt.SetFieldValue(LGroundWater.MaximumGroundwaterAbstraction);
          Parameterk2Edt.SetFieldValue(LGroundWater.ParameterK2);
          Parameterk3Edt.SetFieldValue(LGroundWater.ParameterK3);
          WaterEvaporationAreaEdt.SetFieldValue(LGroundWater.GroundWaterEvaporationArea);

          PopulateRefNodeNumber;

        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.OnMonthlyGroundWaterEvaporationBtnClick(Sender: TObject);
const OPNAME = 'TGroundWaterValidator.OnMonthlyGroundWaterEvaporationBtnClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TGroundWaterEvaporationValidator;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.BtnCancel.Visible := False;
      LForm.LanguageHasChanged;
      LDialogValidator := TGroundWaterEvaporationValidator.Create(LForm,FAppModules);
      try
        LDialogValidator.Identifier := FIdentifier;
        LDialogValidator.RiverineMonthlyType := 'Evaporation';
        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LForm.ShowModal;
        if (LForm.ModalResult = mrOk) then
        begin
        end;
      finally
        LDialogValidator.Free;
      end;
    finally
      LForm.Free;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.OnMonthlyGroundWaterUsageFactorBtnClick(Sender: TObject);
const OPNAME = 'TGroundWaterValidator.OnMonthlyGroundWaterUsageFactorBtnClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TGroundWaterEvaporationValidator;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.BtnCancel.Visible := False;
      LForm.LanguageHasChanged;
      LDialogValidator := TGroundWaterEvaporationValidator.Create(LForm,FAppModules);
      try
        LDialogValidator.Identifier := FIdentifier;
        LDialogValidator.RiverineMonthlyType := 'Factors';
        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LForm.ShowModal;
        if (LForm.ModalResult = mrOk) then
        begin
        end;
      finally
        LDialogValidator.Free;
      end;
    finally
      LForm.Free;
    end;
  // Handle exceptions.}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                   ANewValue: string): boolean;
const OPNAME = 'TGroundWaterValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.PopulateRefNodeNumber;
const OPNAME = 'TGroundWaterValidator.PopulateRefNodeNumber';
var
  LGroundWater   : IGroundWater;
  LReservoirList : IReservoirDataList;
  LReservoirData : IReservoirData;
  LIndex         : integer;
  LFound         : Boolean;
  LIndexA        : integer;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                    GroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        RefNodeNumberCbx.Items.Clear;
        LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.ReservoirList;
        if (LReservoirList <> nil) then
        begin
          for LIndex := 0 to LReservoirList.ReservoirAndNodesCount - 1 do
          begin
            LReservoirData  := LReservoirList.ReservoirOrNodeByIndex[LIndex];
            if(lReservoirData.ReservoirConfigurationData.CatchmentRef <> 0) then
            begin
              RefNodeNumberCbx.Items.AddObject(lReservoirData.ReservoirConfigurationData.ReservoirName,
                                          TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
            end;
          end;
        end;

        LFound := FALSE;
        LIndexA := 0;
        while ((NOT LFound) AND (LIndexA < RefNodeNumberCbx.Items.Count)) do
        begin
          if (Integer(RefNodeNumberCbx.Items.Objects[LIndexA]) = LGroundWater.RefNodeNumber) then
            LFound := TRUE
          else
            LIndexA := LIndexA + 1;
        end;
        if (LFound) then
          RefNodeNumberCbx.ItemIndex := LIndexA
        else
          RefNodeNumberCbx.ItemIndex := -1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterValidator.UpdateRefNodeNumberCbx;
const OPNAME = 'TGroundWaterValidator.UpdateRefNodeNumberCbx';
var
  LGroundWater : IGroundWater;
  LMessage          : string;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.GroundWaterList.
                    GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterDialog do
      begin
        RefNodeNumberCbx.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterRefNodeNumber',
                                       IntToStr(Integer(RefNodeNumberCbx.Items.Objects[RefNodeNumberCbx.ItemIndex])),
                                       LMessage) then
        begin
          if RefNodeNumberCbx.ItemIndex <> -1 then
            LGroundWater.RefNodeNumber := Integer(RefNodeNumberCbx.Items.Objects[RefNodeNumberCbx.ItemIndex]);
          RePopulateDataViewer;
          DoContextValidation (dvtGroundWaterRefNodeNumber);
        end
        else
          RefNodeNumberCbx.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
