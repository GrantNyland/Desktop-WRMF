//
//
//  UNIT      : Contains the class TMiningValidator.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/09
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UMiningValidator;

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
  UMiningDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TMiningValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    FIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnMonthlyMeanPanEvaporationClick(Sender: TObject);
    procedure OnMonthlyLakeEvaporationClick(Sender: TObject);
    procedure UpdateMineName;
    procedure UpdateBeneficiationPlantArea;
    procedure UpdateBeneficiationRunOffFactor;
    procedure UpdateHydrologyNodeNumberCbx;
    procedure UpdateXCoord;
    procedure UpdateYCoord;
    procedure ValidateMineName(AMine : IMine);
    procedure ValidateBeneficiationPlantArea(AMine : IMine);
    procedure ValidateBeneficiationRunOffFactor(AMine : IMine);
    procedure ValidateRiverChannelNumber(AMine : IMine);
    procedure ValidateHydrologyNodeNumber(AMine : IMine);
    procedure ValidateXCoord(AReservoirData: IReservoirData);
    procedure ValidateYCoord(AReservoirData: IReservoirData);
    procedure PCDExistsChkBoxOnClick(Sender : TObject);
    procedure RePopulateDataViewer;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function MiningDialog: TMiningDialog;
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
  UMiningEvaporationValidator,
  UYieldModelDataGUIForm,
  UMineMonthlyDataDialog,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, UNetworkFeaturesData, UMiningData,
  UNetworkElementData;

{ TMiningValidator }

procedure TMiningValidator.CreateMemberObjects;
const OPNAME = 'TMiningValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TMiningDialog.Create(FPanelOwner,FAppModules);
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

      PCDExistsChkBox.FieldProperty      := FAppModules.FieldProperties.FieldProperty('PCDChannelNumber');
      PCDExistsChkBox.OnClick            := PCDExistsChkBoxOnClick;

    end
       // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.DestroyMemberObjects;
const OPNAME = 'TMiningValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningValidator.Initialise: boolean;
const OPNAME = 'TMiningValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMiningValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Mine';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.ClearDataViewer;
const OPNAME = 'TMiningValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with MiningDialog do
    begin
      MineNameEdt.SetFieldValue('');
      MineNumberEdt.SetFieldValue('-1');
      NrOfMineCastPitsEdt.SetFieldValue('-1');
      NrOfSlurryDumpEdt.SetFieldValue('-1');
      NrOfUnderGroundMiningEdt.SetFieldValue('-1');
      HydrologyNodeNumberCbx.ItemIndex := -1;
      BeneficiationPlantAreaEdt.SetFieldValue('-1');
      BeneficiationRunOffFactorEdt.SetFieldValue('-1');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningValidator.SaveState: boolean;
const OPNAME = 'TMiningValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMiningValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMiningValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender =  MiningDialog.MineNameEdt) AND
       (MiningDialog.MineNameEdt.HasValueChanged)) then
       UpdateMineName
    else
    if ((Sender =  MiningDialog.BeneficiationPlantAreaEdt) AND
       (MiningDialog.BeneficiationPlantAreaEdt.HasValueChanged)) then
       UpdateBeneficiationPlantArea
    else
    if ((Sender =  MiningDialog.BeneficiationRunOffFactorEdt) AND
       (MiningDialog.BeneficiationRunOffFactorEdt.HasValueChanged)) then
       UpdateBeneficiationRunOffFactor
    else
    if ((sender = MiningDialog.HydrologyNodeNumberCbx) AND
       (MiningDialog.HydrologyNodeNumberCbx.HasValueChanged)) then
    begin
      UpdateHydrologyNodeNumberCbx;
    end;
    if ((sender = MiningDialog.MineXCoordEdit) AND
       (MiningDialog.MineXCoordEdit.HasValueChanged)) then
       UpdateXCoord;
    if ((sender = MiningDialog.MineYCoordEdit) AND
       (MiningDialog.MineYCoordEdit.HasValueChanged)) then
       UpdateYCoord;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningValidator.MiningDialog :TMiningDialog;
const OPNAME = 'TMiningValidator.MiningDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TMiningDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMiningValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   if (AFieldName = 'OpenCast') or
      (AFieldName = 'UndegroundDam')  or
      (AFieldName = 'Slurry') then
         RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningValidator.StudyHasChanged: boolean;
const OPNAME = 'TMiningValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.PopulateDataViewer;
const OPNAME = 'TMiningValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtMineAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.RePopulateDataViewer;
const OPNAME = 'TMiningValidator.RePopulateDataViewer';
var
  LIndex          : integer;
  LMine           : IMine;
  lReservoirData  : IReservoirData;
  lReservoirList  : IReservoirDataList;
  lReservoirDataA : IReservoirData;
begin
  try
    if (FIdentifier > 0) then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                   CastMineList.MineByNodeNumber[FIdentifier];
      if (LMine <> nil) then
      begin
        with MiningDialog do
        begin
          MineNumberEdt.SetFieldValue(LMine.NodeNumber);
          MineNameEdt.SetFieldValue(LMine.MineName);
          NrOfMineCastPitsEdt.SetFieldValue(LMine.OpenCastCount);
          NrOfSlurryDumpEdt.SetFieldValue(LMine.SlurryDumpCount);
          NrOfUnderGroundMiningEdt.SetFieldValue(LMine.UndergroundCount);
          BeneficiationPlantAreaEdt.SetFieldValue(LMine.BeneficiationPlantArea);
          BeneficiationRunOffFactorEdt.SetFieldValue(LMine.BeneficiationRunoffFactor);
          PCDExistsChkBox.Checked := LMine.PolutionControlDamExists;

          lReservoirDataA := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                              .ReservoirList.ReservoirOrNodeByIdentifier[LMine.NodeNumber];
          if (lReservoirDataA <> nil) then
          begin
            MineXCoordEdit.SetFieldValue(lReservoirDataA.ReservoirConfigurationData.XCoord);
            MineYCoordEdit.SetFieldValue(lReservoirDataA.ReservoirConfigurationData.YCoord);
          end;

          HydrologyNodeNumberCbx.Items.Clear;
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for LIndex := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoirData  := lReservoirList.ReservoirOrNodeByIndex[lIndex];
              if(lReservoirData.ReservoirConfigurationData.CatchmentRef > 0) then
              begin
                HydrologyNodeNumberCbx.Items.AddObject(lReservoirData.ReservoirConfigurationData.ReservoirName,
                                          TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
              end;
            end;

            LIndex := 0;
            while (LIndex < HydrologyNodeNumberCbx.Items.Count) do
            begin
              if (Integer(HydrologyNodeNumberCbx.Items.Objects[LIndex]) = LMine.HydrologyNodeNumber) then
              begin
                HydrologyNodeNumberCbx.ItemIndex := LIndex;
                Break;
              end
              else
              begin
                HydrologyNodeNumberCbx.ItemIndex := -1;
                LIndex := LIndex + 1
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.UpdateMineName;
const OPNAME = 'TMiningValidator.UpdateMineName';
var
  lMessage  : string;
  LMine : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(MineNameEdt.FieldProperty.FieldName,
          MineNameEdt.Text,lMessage)) then
        begin
          MineNameEdt.FieldValidationError := lMessage;
          LMine.MineName := trim(MineNameEdt.Text);
          MineNameEdt.SetFieldValue(LMine.MineName);
          DoContextValidation(dvtMineName);
        end
        else
          MineNameEdt.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.UpdateBeneficiationPlantArea;
const OPNAME = 'TMiningValidator.UpdateBeneficiationPlantArea';
var
  lMessage  : string;
  LMine : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(BeneficiationPlantAreaEdt.FieldProperty.FieldName,
          BeneficiationPlantAreaEdt.Text,lMessage)) then
        begin
          BeneficiationPlantAreaEdt.FieldValidationError := lMessage;
          LMine.BeneficiationPlantArea := StrToFloat(BeneficiationPlantAreaEdt.Text);
          BeneficiationPlantAreaEdt.SetFieldValue(LMine.BeneficiationPlantArea);
          DoContextValidation(dvtBeneficiationPlantArea);
        end
        else
          BeneficiationPlantAreaEdt.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.UpdateBeneficiationRunOffFactor;
const OPNAME = 'TMiningValidator.UpdateBeneficiationRunOffFactor';
var
  lMessage  : string;
  LMine : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(BeneficiationPlantAreaEdt.FieldProperty.FieldName,
          BeneficiationRunOffFactorEdt.Text,lMessage)) then
        begin
          BeneficiationRunOffFactorEdt.FieldValidationError := lMessage;
          LMine.BeneficiationRunoffFactor := StrToFloat(BeneficiationRunOffFactorEdt.Text);
          BeneficiationRunOffFactorEdt.SetFieldValue(LMine.BeneficiationRunoffFactor);
          DoContextValidation(dvtBeneficiationRunOffFactor);
        end
        else
          BeneficiationRunOffFactorEdt.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.UpdateHydrologyNodeNumberCbx;
const OPNAME = 'TMiningValidator.UpdateHydrologyNodeNumberCbx';
var
  LMine    : IMine;
  LMessage : string;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
             CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MiningDialog do
      begin
        HydrologyNodeNumberCbx.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                HydrologyNodeNumberCbx.FieldProperty.FieldName,
                IntToStr(Integer(HydrologyNodeNumberCbx.Items.Objects[HydrologyNodeNumberCbx.ItemIndex])),
                LMessage) then
        begin
          if HydrologyNodeNumberCbx.ItemIndex <> -1 then
            LMine.HydrologyNodeNumber := Integer(HydrologyNodeNumberCbx.Items.Objects[HydrologyNodeNumberCbx.ItemIndex]);
          RePopulateDataViewer;
          DoContextValidation (dvtHydrologyNodeNumber);
        end
        else
          HydrologyNodeNumberCbx.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMiningValidator.DoContextValidation';
var
  LMine      : IMine;
  LReservoir : IReservoirData;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastMineList.MineByNodeNumber[Fidentifier];
    if (LMine <> nil) then
    begin
      case AValidationType of
        dvtMineAll :
        begin
          ValidateMineName(LMine);
          ValidateBeneficiationPlantArea(LMine);
          ValidateBeneficiationRunOffFactor(LMine);
          ValidateRiverChannelNumber(LMine);
          ValidateHydrologyNodeNumber(LMine);
        end;
        dvtMineName                  : ValidateMineName(LMine);
        dvtBeneficiationPlantArea    : ValidateBeneficiationPlantArea(LMine);
        dvtBeneficiationRunOffFactor : ValidateBeneficiationRunOffFactor(LMine);
        dvtRiverChannelNumber        : ValidateRiverChannelNumber(LMine);
        dvtHydrologyNodeNumber       : ValidateHydrologyNodeNumber(LMine);
      end;
      LReservoir  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ReservoirList.ReservoirOrNodeByIdentifier[LMine.NodeNumber];
      if (LReservoir <> nil) then
      begin
        if (AValidationType = dvtMineAll) then
        begin
          ValidateXCoord(LReservoir);
          ValidateYCoord(LReservoir);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.ValidateMineName(AMine: IMine);
const OPNAME = 'TMiningValidator.ValidateMineName';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'MineName');
      MineNameEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningValidator.ValidateBeneficiationRunOffFactor(AMine: IMine);
const OPNAME = 'TMiningValidator.ValidateBeneficiationRunOffFactor';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'BeneficiationRunOffFactor');
      BeneficiationRunOffFactorEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningValidator.ValidateHydrologyNodeNumber(AMine: IMine);
const OPNAME = 'TMiningValidator.ValidateHydrologyNodeNumber';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'HydrologyNodeNumber');
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningValidator.ValidateBeneficiationPlantArea(AMine: IMine);
const OPNAME = 'TMiningValidator.ValidateBeneficiationPlantArea';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,'BeneficiationPlantArea');
      BeneficiationPlantAreaEdt.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningValidator.ValidateRiverChannelNumber(AMine: IMine);
const OPNAME = 'TMiningValidator.ValidateRiverChannelNumber';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      AMine.Validate(FErrorMessage,' RiverChannelNumber');
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TMiningValidator.OnMonthlyLakeEvaporationClick(Sender: TObject);
const OPNAME = 'TMiningValidator.OnMonthlyLakeEvaporationClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TMiningEvaporationValidator;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.BtnCancel.Visible := False;
      LForm.LanguageHasChanged;
      LDialogValidator := TMiningEvaporationValidator.Create(LForm,FAppModules);
      try
        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Identifier := FIdentifier;
        LDialogValidator.EvaporationType := 'Lake';
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

procedure TMiningValidator.OnMonthlyMeanPanEvaporationClick(Sender: TObject);
const OPNAME = 'TMiningValidator.OnMonthlyMeanPanEvaporationClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TMiningEvaporationValidator;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.BtnCancel.Visible := False;
      LForm.LanguageHasChanged;
      LDialogValidator := TMiningEvaporationValidator.Create(LForm,FAppModules);
      try

        LDialogValidator.Identifier := FIdentifier;
        LDialogValidator.EvaporationType := 'Pan';
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

procedure TMiningValidator.UpdateXCoord;
const OPNAME = 'TMiningValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LMine            : IMine;
begin
  try
    if MiningDialog. MineXCoordEdit.HasValueChanged then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                  MineList.MineByNodeNumber[FIdentifier];
      if (LMine <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[LMine.NodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with MiningDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',MineXCoordEdit.Text, lErrorMessage) then
            begin
              MineXCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(MineXCoordEdit.Text);
              MineXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
            end
            else
              MineXCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.UpdateYCoord;
const OPNAME = 'TMiningValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LMine            : IMine;
begin
  try
    if MiningDialog.MineYCoordEdit.HasValueChanged then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                  MineList.MineByNodeNumber[FIdentifier];
      if (LMine <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[LMine.NodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with MiningDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',MineYCoordEdit.Text, lErrorMessage) then
            begin
              MineYCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(MineYCoordEdit.Text);
              MineYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
            end
            else
              MineYCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.ValidateXCoord(AReservoirData: IReservoirData);
const OPNAME = 'TMiningValidator.ValidateXCoord';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      MineXCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'XCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      MineXCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.ValidateYCoord(AReservoirData: IReservoirData);
const OPNAME = 'TMiningValidator.ValidateYCoord';
begin
  try
    with MiningDialog do
    begin
      FErrorMessage := '';
      MineYCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'YCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      MineYCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningValidator.PCDExistsChkBoxOnClick(Sender: TObject);
const OPNAME = 'TMiningValidator.PCDExistsChkBoxOnClick';
var
  LMine           : IMine;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                 CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      LMine.PolutionControlDamExists := MiningDialog.PCDExistsChkBox.Checked;
      MiningDialog.PCDExistsChkBox.Checked := LMine.PolutionControlDamExists;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


