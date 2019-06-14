unit UGroundWaterPitmanValidator;

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
  UGroundWaterPitmanDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TGroundWaterPitmanValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    FIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;

    procedure UpdatePitmanSoilMoistureCapacity;
    procedure UpdatePitmanSoilMoistureStorageCapacity;
    procedure UpdatePitmansoilMoistureFlowState;
    procedure UpdatePitmanSoilMoistureFlowEquation;
    procedure UpdatePitmanMaximumGroundwaterFlow;
    procedure UpdatePitmanSoilMoistureRechargeEquation;
    procedure UpdatePitmanGroundwaterFlow;

    procedure ValidatePitmanSoilMoistureCapacity(AGroundWater : IGroundWater);
    procedure ValidatePitmanSoilMoistureStorageCapacity(AGroundWater : IGroundWater);
    procedure ValidatePitmansoilMoistureFlowState(AGroundWater : IGroundWater);
    procedure ValidatePitmanSoilMoistureFlowEquation(AGroundWater : IGroundWater);
    procedure ValidatePitmanMaximumGroundwaterFlow(AGroundWater : IGroundWater);
    procedure ValidatePitmanSoilMoistureRechargeEquation(AGroundWater : IGroundWater);
    procedure ValidatePitmanGroundwaterFlow(AGroundWater : IGroundWater);
    procedure RePopulateDataViewer;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function GroundWaterPitmanDialog: TGroundWaterPitmanDialog;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationType: TDialogValidationType);override;

    property Identifier : integer read FIdentifier write FIdentifier;
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
  UNetworkFeaturesData,
  UNetworkElementData, UGroundWater;

{ TGroundWaterPitmanValidator }

procedure TGroundWaterPitmanValidator.ClearDataViewer;
const OPNAME = 'TGroundWaterPitmanValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with GroundWaterPitmanDialog do
    begin
      SoilMoistureCapacityEdt.SetFieldValue('-1');
      SoilMoistureStorageCapacityEdt.SetFieldValue('-1');
      SoilMoistureFlowStateEdt.SetFieldValue('-1');
      SoilMoistureFlowEquationEdt.SetFieldValue('-1');
      MaximumGroundWaterFlowEdt.SetFieldValue('-1');
      SoilMoistureRechargeEquationEdt.SetFieldValue('-1');
      GroundWaterFlowEdt.SetFieldValue('-1');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.CreateMemberObjects;
const OPNAME = 'TGroundWaterPitmanValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TGroundWaterPitmanDialog.Create(FPanelOwner,FAppModules);
    with GroundWaterPitmanDialog do
    begin
      SoilMoistureCapacityEdt.FieldProperty         := FAppModules.FieldProperties.FieldProperty('PitmanSoilMoistureCapacity');
      SoilMoistureCapacityEdt.OnEnter               := OnEditControlEnter;
      SoilMoistureCapacityEdt.OnExit                := OnEditControltExit;

      SoilMoistureStorageCapacityEdt.FieldProperty  := FAppModules.FieldProperties.FieldProperty('PitmanSoilMoistureStorageCapacity');
      SoilMoistureStorageCapacityEdt.OnEnter        := OnEditControlEnter;
      SoilMoistureStorageCapacityEdt.OnExit         := OnEditControltExit;

      SoilMoistureFlowStateEdt.FieldProperty        := FAppModules.FieldProperties.FieldProperty('PitmansoilMoistureFlowState');
      SoilMoistureFlowStateEdt.OnEnter              := OnEditControlEnter;
      SoilMoistureFlowStateEdt.OnExit               := OnEditControltExit;

      SoilMoistureFlowEquationEdt.FieldProperty     := FAppModules.FieldProperties.FieldProperty('PitmanSoilMoistureFlowEquation');
      SoilMoistureFlowEquationEdt.OnEnter           := OnEditControlEnter;
      SoilMoistureFlowEquationEdt.OnExit            := OnEditControltExit;

      MaximumGroundWaterFlowEdt.FieldProperty       := FAppModules.FieldProperties.FieldProperty('PitmanMaximumGroundwaterFlow');
      MaximumGroundWaterFlowEdt.OnEnter             := OnEditControlEnter;
      MaximumGroundWaterFlowEdt.OnExit              := OnEditControltExit;

      SoilMoistureRechargeEquationEdt.FieldProperty := FAppModules.FieldProperties.FieldProperty('PitmanSoilMoistureRechargeEquation');
      SoilMoistureRechargeEquationEdt.OnEnter       := OnEditControlEnter;
      SoilMoistureRechargeEquationEdt.OnExit        := OnEditControltExit;

      GroundWaterFlowEdt.FieldProperty              := FAppModules.FieldProperties.FieldProperty('PitmanGroundwaterFlow');
      GroundWaterFlowEdt.OnEnter                    := OnEditControlEnter;
      GroundWaterFlowEdt.OnExit                     := OnEditControltExit;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.DestroyMemberObjects;
const OPNAME = 'TGroundWaterPitmanValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanValidator.GroundWaterPitmanDialog: TGroundWaterPitmanDialog;
const OPNAME = 'TGroundWaterPitmanValidator.GroundWaterPitmanDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TGroundWaterPitmanDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanValidator.Initialise: boolean;
const OPNAME = 'TGroundWaterPitmanValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanValidator.LanguageHasChanged: boolean;
const OPNAME = 'TGroundWaterPitmanValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.language.GetString('TabCaption.GroundwaterPitman');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanValidator.SaveState: boolean;
const OPNAME = 'TGroundWaterPitmanValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanValidator.StudyHasChanged: boolean;
const OPNAME = 'TGroundWaterPitmanValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureCapacity;
const OPNAME = 'TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureCapacity';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterPitmanDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureCapacity',
            SoilMoistureCapacityEdt.Text,LMessage)) then
        begin
          SoilMoistureCapacityEdt.FieldValidationError := LMessage;
          LGroundWater.PitmanSoilMoistureCapacity := StrToFloat(SoilMoistureCapacityEdt.Text);
          SoilMoistureCapacityEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureCapacity);
          DoContextValidation(dvtPitmanSoilMoistureCapacity);
        end
        else
          SoilMoistureCapacityEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureStorageCapacity;
const OPNAME = 'TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureStorageCapacity';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterPitmanDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureStorageCapacity',
            SoilMoistureStorageCapacityEdt.Text,LMessage)) then
        begin
          SoilMoistureStorageCapacityEdt.FieldValidationError := LMessage;
          LGroundWater.PitmanSoilMoistureStorageCapacity := StrToFloat(SoilMoistureStorageCapacityEdt.Text);
          SoilMoistureStorageCapacityEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureStorageCapacity);
          DoContextValidation(dvtPitmanSoilMoistureStorageCapacity);
        end
        else
          SoilMoistureStorageCapacityEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.UpdatePitmansoilMoistureFlowState;
const OPNAME = 'TGroundWaterPitmanValidator.UpdatePitmansoilMoistureFlowState';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterPitmanDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureFlowState',
            SoilMoistureFlowStateEdt.Text,LMessage)) then
        begin
          SoilMoistureFlowStateEdt.FieldValidationError := LMessage;
          LGroundWater.PitmansoilMoistureFlowState := StrToFloat(SoilMoistureFlowStateEdt.Text);
          SoilMoistureFlowStateEdt.SetFieldValue(LGroundWater.PitmansoilMoistureFlowState);
          DoContextValidation(dvtPitmansoilMoistureFlowState);
        end
        else
          SoilMoistureFlowStateEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureFlowEquation;
const OPNAME = 'TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureFlowEquation';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterPitmanDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureFlowEquation',
            SoilMoistureFlowEquationEdt.Text,LMessage)) then
        begin
          SoilMoistureFlowEquationEdt.FieldValidationError := LMessage;
          LGroundWater.PitmanSoilMoistureFlowEquation := StrToFloat(SoilMoistureFlowEquationEdt.Text);
          SoilMoistureFlowEquationEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureFlowEquation);
          DoContextValidation(dvtPitmanSoilMoistureFlowEquation);
        end
        else
          SoilMoistureFlowEquationEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.UpdatePitmanMaximumGroundwaterFlow;
const OPNAME = 'TGroundWaterPitmanValidator.UpdatePitmanMaximumGroundwaterFlow';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterPitmanDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PitmanMaximumGroundwaterFlow',
            MaximumGroundWaterFlowEdt.Text,LMessage)) then
        begin
          MaximumGroundWaterFlowEdt.FieldValidationError := LMessage;
          LGroundWater.PitmanMaximumGroundwaterFlow := StrToFloat(MaximumGroundWaterFlowEdt.Text);
          MaximumGroundWaterFlowEdt.SetFieldValue(LGroundWater.PitmanMaximumGroundwaterFlow);
          DoContextValidation(dvtPitmanMaximumGroundwaterFlow);
        end
        else
          MaximumGroundWaterFlowEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureRechargeEquation;
const OPNAME = 'TGroundWaterPitmanValidator.UpdatePitmanSoilMoistureRechargeEquation';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterPitmanDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureRechargeEquation',
            SoilMoistureRechargeEquationEdt.Text,LMessage)) then
        begin
          SoilMoistureRechargeEquationEdt.FieldValidationError := LMessage;
          LGroundWater.PitmanSoilMoistureRechargeEquation := StrToFloat(SoilMoistureRechargeEquationEdt.Text);
          SoilMoistureRechargeEquationEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureRechargeEquation);
          DoContextValidation(dvtPitmanSoilMoistureRechargeEquation);
        end
        else
          SoilMoistureRechargeEquationEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.UpdatePitmanGroundwaterFlow;
const OPNAME = 'TGroundWaterPitmanValidator.UpdatePitmanGroundwaterFlow';
var
  LMessage     : string;
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterPitmanDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('PitmanGroundwaterFlow',
            GroundWaterFlowEdt.Text,LMessage)) then
        begin
          GroundWaterFlowEdt.FieldValidationError := LMessage;
          LGroundWater.PitmanGroundwaterFlow := StrToFloat(GroundWaterFlowEdt.Text);
          GroundWaterFlowEdt.SetFieldValue(LGroundWater.PitmanGroundwaterFlow);
          DoContextValidation(dvtPitmanGroundwaterFlow);
        end
        else
          GroundWaterFlowEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureCapacity(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureCapacity';
begin
  try
    with GroundWaterPitmanDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PitmanSoilMoistureCapacity');
      SoilMoistureCapacityEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureStorageCapacity(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureStorageCapacity';
begin
  try
    with GroundWaterPitmanDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PitmanSoilMoistureStorageCapacity');
      SoilMoistureStorageCapacityEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanValidator.ValidatePitmansoilMoistureFlowState(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterPitmanValidator.ValidatePitmansoilMoistureFlowState';
begin
  try
    with GroundWaterPitmanDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PitmansoilMoistureFlowState');
      SoilMoistureFlowStateEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureFlowEquation(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureFlowEquation';
begin
  try
    with GroundWaterPitmanDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PitmanSoilMoistureFlowEquation');
      SoilMoistureFlowEquationEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanValidator.ValidatePitmanMaximumGroundwaterFlow(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterPitmanValidator.ValidatePitmanMaximumGroundwaterFlow';
begin
  try
    with GroundWaterPitmanDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PitmanMaximumGroundwaterFlow');
      MaximumGroundWaterFlowEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureRechargeEquation(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterPitmanValidator.ValidatePitmanSoilMoistureRechargeEquation';
begin
  try
    with GroundWaterPitmanDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PitmanSoilMoistureRechargeEquation');
      SoilMoistureRechargeEquationEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanValidator.ValidatePitmanGroundwaterFlow(AGroundWater: IGroundWater);
const OPNAME = 'TGroundWaterPitmanValidator.ValidatePitmanGroundwaterFlow';
begin
  try
    with GroundWaterPitmanDialog do
    begin
      FErrorMessage := '';
      AGroundWater.Validate(FErrorMessage,'PitmanGroundwaterFlow');
      GroundWaterFlowEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TGroundWaterPitmanValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TGroundWaterPitmanValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender =  GroundWaterPitmanDialog.SoilMoistureCapacityEdt) AND
       (GroundWaterPitmanDialog.SoilMoistureCapacityEdt.HasValueChanged)) then
       UpdatePitmanSoilMoistureCapacity
    else
    if ((Sender =  GroundWaterPitmanDialog.SoilMoistureStorageCapacityEdt) AND
       (GroundWaterPitmanDialog.SoilMoistureStorageCapacityEdt.HasValueChanged)) then
       UpdatePitmanSoilMoistureStorageCapacity
    else
    if ((Sender =  GroundWaterPitmanDialog.SoilMoistureFlowStateEdt) AND
       (GroundWaterPitmanDialog.SoilMoistureFlowStateEdt.HasValueChanged)) then
       UpdatePitmansoilMoistureFlowState
    else
    if ((Sender =  GroundWaterPitmanDialog.SoilMoistureFlowEquationEdt) AND
       (GroundWaterPitmanDialog.SoilMoistureFlowEquationEdt.HasValueChanged)) then
       UpdatePitmanSoilMoistureFlowEquation
    else
    if ((Sender =  GroundWaterPitmanDialog.MaximumGroundWaterFlowEdt) AND
       (GroundWaterPitmanDialog.MaximumGroundWaterFlowEdt.HasValueChanged)) then
       UpdatePitmanMaximumGroundwaterFlow
    else
    if ((Sender =  GroundWaterPitmanDialog.SoilMoistureRechargeEquationEdt) AND
       (GroundWaterPitmanDialog.SoilMoistureRechargeEquationEdt.HasValueChanged)) then
       UpdatePitmanSoilMoistureRechargeEquation
    else
    if ((Sender =  GroundWaterPitmanDialog.GroundWaterFlowEdt) AND
       (GroundWaterPitmanDialog.GroundWaterFlowEdt.HasValueChanged)) then
       UpdatePitmanGroundwaterFlow
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.RePopulateDataViewer;
const OPNAME = 'TGroundWaterPitmanValidator.RePopulateDataViewer';
var
  LGroundWater : IGroundWater;
begin
  try
    if (FIdentifier > 0) then
    begin
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
      if (LGroundWater <> nil) then
      begin
        with GroundWaterPitmanDialog do
        begin
          SoilMoistureCapacityEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureCapacity);
          SoilMoistureStorageCapacityEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureStorageCapacity);
          SoilMoistureFlowStateEdt.SetFieldValue(LGroundWater.PitmansoilMoistureFlowState);
          SoilMoistureFlowEquationEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureFlowEquation);
          MaximumGroundWaterFlowEdt.SetFieldValue(LGroundWater.PitmanMaximumGroundwaterFlow);
          SoilMoistureRechargeEquationEdt.SetFieldValue(LGroundWater.PitmanSoilMoistureRechargeEquation);
          GroundWaterFlowEdt.SetFieldValue(LGroundWater.PitmanGroundwaterFlow);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.PopulateDataViewer;
const OPNAME = 'TGroundWaterPitmanValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtGroundWaterAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TGroundWaterPitmanValidator.DoContextValidation';
var
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByNodeNumber[Fidentifier];
    if (LGroundWater <> nil) then
    begin
      case AValidationType of
        dvtGroundWaterAll :
        begin
          ValidatePitmanSoilMoistureCapacity(LGroundWater);
          ValidatePitmanSoilMoistureStorageCapacity(LGroundWater);
          ValidatePitmansoilMoistureFlowState(LGroundWater);
          ValidatePitmanSoilMoistureFlowEquation(LGroundWater);
          ValidatePitmanMaximumGroundwaterFlow(LGroundWater);
          ValidatePitmanSoilMoistureRechargeEquation(LGroundWater);
          ValidatePitmanGroundwaterFlow(LGroundWater);
        end;
        dvtPitmanSoilMoistureCapacity          : ValidatePitmanSoilMoistureCapacity(LGroundWater);
        dvtPitmanSoilMoistureStorageCapacity   : ValidatePitmanSoilMoistureStorageCapacity(LGroundWater);
        dvtPitmansoilMoistureFlowState         : ValidatePitmansoilMoistureFlowState(LGroundWater);
        dvtPitmanSoilMoistureFlowEquation      : ValidatePitmanSoilMoistureFlowEquation(LGroundWater);
        dvtPitmanMaximumGroundwaterFlow        : ValidatePitmanMaximumGroundwaterFlow(LGroundWater);
        dvtPitmanSoilMoistureRechargeEquation  : ValidatePitmanSoilMoistureRechargeEquation(LGroundWater);
        dvtPitmanGroundwaterFlow               : ValidatePitmanGroundwaterFlow(LGroundWater);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                         ANewValue: string): boolean;
const OPNAME = 'TGroundWaterPitmanValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'GroundWaterName') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
