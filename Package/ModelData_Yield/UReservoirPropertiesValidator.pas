//
//
//  UNIT      : Contains the class TReservoirPropertiesValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirPropertiesValidator;

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
  UReservoirPropertiesDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TReservoirPropertiesValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnSelectPenaltyClick(Sender: TObject);
    procedure OnViewGridClick(Sender: TObject);
    procedure OnViewGraphClick(Sender: TObject);
    procedure OnNewPenaltyClick(Sender: TObject);
    procedure OnSelectHistoricWaterlevelClick(Sender : TObject);
    procedure OnReservoirExistsClick(Sender: TObject);
    procedure OnSummaryIncludeClick(Sender: TObject);
    procedure UpdateResevoirName;
    procedure UpdateResevoirPenaltyStructure;
    procedure UpdateResevoirPriority;
    procedure UpdateResevoirIncludeSummary;
    procedure UpdateDamLevelFileName;
    procedure UpdateReservoirExists;
    procedure UpdateRainCoef;
    procedure UpdateXCoord;
    procedure UpdateYCoord;
    procedure UpdateReservoirAreaGroup;
    procedure RePopulateDataViewer;
    procedure RepopulateHistoricWaterLevels;
    procedure RepopulateReservoirAreaGroup;
    function  CurrentReservoir:IReservoirData;
    function  ValidPenaltyStructureNumber: boolean;
    procedure ValidateReservoirNumber(AReservoir : IReservoirData);
    procedure ValidateReservoirName(AReservoir : IReservoirData);
    procedure ValidateReservoirPenaltyStructure(AReservoir : IReservoirData);
    procedure ValidateReservoirPriority(AReservoir: IReservoirData);
    procedure ValidateRainCoef(AReservoir: IReservoirConfigurationData);
    procedure ValidateDamLevelFileName (AReservoir : IReservoirData);
    procedure SetGridGraphBtnState(AReservoir : IReservoirData);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function ReservoirPropertiesDialog: TReservoirPropertiesDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UFileNames,
  UConstants,
  UAbstractFileNamesObject,
  UMonthlyDamLevelsObject,
  UReservoirPenaltyValidator,
  UYieldModelDataGUIForm,
  UReservoirPenaltyDialog,
  UReservoirPenaltyStructureData,
  UReservoirAreaGroup,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, UNetworkElementData;

{ TReservoirPropertiesValidator }

procedure TReservoirPropertiesValidator.CreateMemberObjects;
const OPNAME = 'TReservoirPropertiesValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TReservoirPropertiesDialog.Create(FPanelOwner,FAppModules);

    ReservoirPropertiesDialog.ReservoirNumberEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ReservoirNodeNumber');
    ReservoirPropertiesDialog.ReservoirNumberEdit.OnEnter             := OnEditControlEnter;

    ReservoirPropertiesDialog.ReservoirNameEdit.EditIdentifier   := giReservoirName;
    ReservoirPropertiesDialog.ReservoirNameEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ReservoirName');
    ReservoirPropertiesDialog.ReservoirNameEdit.OnEnter          := OnEditControlEnter;
    ReservoirPropertiesDialog.ReservoirNameEdit.OnExit           := OnEditControltExit;

    ReservoirPropertiesDialog.PenaltyStructureEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
    ReservoirPropertiesDialog.PenaltyStructureEdit.OnEnter       := OnEditControlEnter;
    ReservoirPropertiesDialog.PenaltyStructureEdit.OnExit        := OnEditControltExit;

    ReservoirPropertiesDialog.PriorityEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ReservoirPriority');
    ReservoirPropertiesDialog.PriorityEdit.OnEnter               := OnEditControlEnter;
    ReservoirPropertiesDialog.PriorityEdit.OnExit                := OnEditControltExit;

    ReservoirPropertiesDialog.SummaryIncludeChkBox.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IncludeSummary');
    ReservoirPropertiesDialog.SummaryIncludeChkBox.CheckBoxIdentifier := giIncludeSummary;
    ReservoirPropertiesDialog.SummaryIncludeChkBox.OnEnter            := OnEditControlEnter;
    ReservoirPropertiesDialog.SummaryIncludeChkBox.OnClick            := OnSummaryIncludeClick;

    ReservoirPropertiesDialog.ReservoirExistsChkBox.FieldProperty      := FAppModules.FieldProperties.FieldProperty('StatusIndicator');
    ReservoirPropertiesDialog.ReservoirExistsChkBox.CheckBoxIdentifier := giIncludeSummary;
    ReservoirPropertiesDialog.ReservoirExistsChkBox.OnEnter            := OnEditControlEnter;
    ReservoirPropertiesDialog.ReservoirExistsChkBox.OnClick            := OnReservoirExistsClick;

    ReservoirPropertiesDialog.SelectPenaltyStruct.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
    ReservoirPropertiesDialog.SelectPenaltyStruct.OnEnter     := OnEditControlEnter;
    ReservoirPropertiesDialog.SelectPenaltyStruct.OnExit      := OnEditControltExit;
    ReservoirPropertiesDialog.SelectPenaltyStruct.OnClick     := OnSelectPenaltyClick;

    ReservoirPropertiesDialog.RainCoeffEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('RainCoef');
    ReservoirPropertiesDialog.RainCoeffEdit.OnEnter       := OnEditControlEnter;
    ReservoirPropertiesDialog.RainCoeffEdit.OnExit        := OnEditControltExit;

    ReservoirPropertiesDialog.ReservoirXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
    ReservoirPropertiesDialog.ReservoirXCoordEdit.OnEnter       := OnEditControlEnter;
    ReservoirPropertiesDialog.ReservoirXCoordEdit.OnExit        := OnEditControltExit;

    ReservoirPropertiesDialog.ReservoirYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
    ReservoirPropertiesDialog.ReservoirYCoordEdit.OnEnter       := OnEditControlEnter;
    ReservoirPropertiesDialog.ReservoirYCoordEdit.OnExit        := OnEditControltExit;

    ReservoirPropertiesDialog.ReservoirAreaGroupCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirAreaGroupName');
    ReservoirPropertiesDialog.ReservoirAreaGroupCbx.OnEnter       := OnEditControlEnter;
    ReservoirPropertiesDialog.ReservoirAreaGroupCbx.OnExit        := OnEditControltExit;

    ReservoirPropertiesDialog.DamLevelsFileNameCbx.FieldProperty     := FAppModules.FieldProperties.FieldProperty('DamLevelsFileName');
    ReservoirPropertiesDialog.DamLevelsFileNameCbx.OnEnter           := OnEditControlEnter;
    ReservoirPropertiesDialog.DamLevelsFileNameCbx.OnExit            := OnEditControltExit;

    ReservoirPropertiesDialog.DamLevelsFileNameSelectBtn.FieldProperty := FAppModules.fieldProperties.FieldProperty('PenaltyStruct');
    ReservoirPropertiesDialog.DamLevelsFileNameSelectBtn.OnEnter       := OnEditControlEnter;
    ReservoirPropertiesDialog.DamLevelsFileNameSelectBtn.OnExit        := OnEditControltExit;
    ReservoirPropertiesDialog.DamLevelsFileNameSelectBtn.OnClick       := OnSelectHistoricWaterlevelClick;
    ReservoirPropertiesDialog.DamLevelsFileNameSelectBtn.Enabled  := (FAppModules.User.UserRights in CUR_EditData) and
                                          (not FAppModules.StudyArea.ScenarioLocked);

   ReservoirPropertiesDialog.DamLevelsFileNameGridBtn.OnEnter  := OnEditControlEnter;
   ReservoirPropertiesDialog.DamLevelsFileNameGridBtn.OnExit   := OnEditControltExit;
   ReservoirPropertiesDialog.DamLevelsFileNameGridBtn.OnClick  := OnViewGridClick;

  ReservoirPropertiesDialog.DamLevelsFileNameGraphBtn.OnEnter  := OnEditControlEnter;
  ReservoirPropertiesDialog.DamLevelsFileNameGraphBtn.OnExit   := OnEditControltExit;
  ReservoirPropertiesDialog.DamLevelsFileNameGraphBtn.OnClick  := OnViewGraphClick;


  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirPropertiesValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.Initialise: boolean;
const OPNAME = 'TReservoirPropertiesValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirPropertiesValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Properties';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.ClearDataViewer;
const OPNAME = 'TReservoirPropertiesValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ReservoirPropertiesDialog.ReservoirNameEdit.SetFieldValue('');
    ReservoirPropertiesDialog.ReservoirNumberEdit.SetFieldValue('-1');
    ReservoirPropertiesDialog.PenaltyStructureEdit.SetFieldValue('-1');
    ReservoirPropertiesDialog.PriorityEdit.SetFieldValue('');
    ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked := False;
    ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirPropertiesValidator.RePopulateDataViewer';
var
  lKeyValues       : string;
  lFieldIndex      : string;
  lFieldProperty   : TAbstractFieldProperty;
  LReservoirObject : IReservoirData;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          lFieldIndex := '';

          ReservoirNumberEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.ReservoirIdentifier);

          lFieldProperty := ReservoirNameEdit.FieldProperty;
          lKeyValues := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          ReservoirNameEdit.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          ReservoirNameEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.ReservoirName);

          lFieldProperty := PenaltyStructureEdit.FieldProperty;
          lKeyValues := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          PenaltyStructureEdit.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          PenaltyStructureEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.PenaltyStructIdentifier);

          lFieldProperty := PriorityEdit.FieldProperty;
          lKeyValues := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          PriorityEdit.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          PriorityEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.Priority);

          lFieldProperty := RainCoeffEdit.FieldProperty;
          lKeyValues := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          RainCoeffEdit.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          RainCoeffEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.RainCoef);

          ReservoirXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
          ReservoirYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);

          lFieldProperty := SummaryIncludeChkBox.FieldProperty;
          lKeyValues := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          SummaryIncludeChkBox.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
           SummaryIncludeChkBox.Checked := (UpperCase(Trim(LReservoirObject.ReservoirConfigurationData.IncludeSummary)) = 'Y');

          lFieldProperty := ReservoirExistsChkBox.FieldProperty;
          lKeyValues := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          ReservoirExistsChkBox.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          ReservoirExistsChkBox.Checked := (LReservoirObject.ReservoirConfigurationData.StatusIndicator = 1);
          RepopulateHistoricWaterLevels;
          RepopulateReservoirAreaGroup;
          SetGridGraphBtnState(LReservoirObject);
        end;
        DoContextValidation(dvtResPropAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.SaveState: boolean;
const OPNAME = 'TReservoirPropertiesValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender =  ReservoirPropertiesDialog.ReservoirNameEdit) AND
       (ReservoirPropertiesDialog.ReservoirNameEdit.HasValueChanged)) then
       UpdateResevoirName
    else
    if ((Sender =  ReservoirPropertiesDialog.PenaltyStructureEdit) AND
       (ReservoirPropertiesDialog.PenaltyStructureEdit.HasValueChanged))then
       UpdateResevoirPenaltyStructure
    else
    if ((Sender =  ReservoirPropertiesDialog.PriorityEdit) AND
       (ReservoirPropertiesDialog.PriorityEdit.HasValueChanged)) then
       UpdateResevoirPriority
    else
    if ((sender = ReservoirPropertiesDialog.RainCoeffEdit) AND
       (ReservoirPropertiesDialog.RainCoeffEdit.HasValueChanged)) then
       UpdateRainCoef
    else
    if ((sender = ReservoirPropertiesDialog.ReservoirXCoordEdit) AND
       (ReservoirPropertiesDialog.ReservoirXCoordEdit.HasValueChanged)) then
       UpdateXCoord
    else
    if ((sender = ReservoirPropertiesDialog.ReservoirYCoordEdit) AND
       (ReservoirPropertiesDialog.ReservoirYCoordEdit.HasValueChanged)) then
       UpdateYCoord
    else
    if ((Sender = ReservoirPropertiesDialog.ReservoirAreaGroupCbx) AND
        (ReservoirPropertiesDialog.ReservoirAreaGroupCbx.HasValueChanged)) then
        UpdateReservoirAreaGroup
    else
    if ((sender = ReservoirPropertiesDialog.DamLevelsFileNameCbx) AND
       (ReservoirPropertiesDialog.DamLevelsFileNameCbx.HasValueChanged)) then
    begin
      UpdateDamLevelFileName;
      RePopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.ReservoirPropertiesDialog:TReservoirPropertiesDialog;
const OPNAME = 'TReservoirPropertiesValidator.ReservoirPropertiesDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TReservoirPropertiesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirPropertiesValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   if (AFieldName = 'ReservoirName') and
      (AOldValue = ReservoirPropertiesDialog.ReservoirNameEdit.Text) then
      RePopulateDataViewer;
   if (AFieldName = 'PenaltyStruct')  and
      (AOldValue = ReservoirPropertiesDialog.PenaltyStructureEdit.Text) then
      RePopulateDataViewer;
   if (AFieldName = 'ReservoirNodeNumber') and
      (AOldValue = ReservoirPropertiesDialog.ReservoirNumberEdit.Text) then
      RePopulateDataViewer;
   if (AFieldName = 'ReservoirPriority')  and
      (AOldValue = ReservoirPropertiesDialog.PriorityEdit.Text) then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirPropertiesValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnNewPenaltyClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnNewPenaltyClick';
begin
  try

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnSelectPenaltyClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnSelectPenaltyClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator :TReservoirPenaltyValidator;
  FSelectedPenalty : integer;
  LReservoirObject : IReservoirData;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      LDialogValidator := TReservoirPenaltyValidator.Create(LForm,FAppModules);
      try

        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LReservoirObject :=
          TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (LReservoirObject <> nil) then
          LDialogValidator.PenaltyStructureNumber := LReservoirObject.ReservoirConfigurationData.PenaltyStructIdentifier;
        LDialogValidator.ViewMode := vmSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LForm.ShowModal;
        if (LForm.ModalResult = mrOk) then
        begin
          FSelectedPenalty := TReservoirPenaltyDialog(LDialogValidator.Panel).GrdPenalty.Col -3;
          if(ReservoirPropertiesDialog.PenaltyStructureEdit.Text <> IntToStr(FSelectedPenalty)) then
          begin
            ReservoirPropertiesDialog.PenaltyStructureEdit.Text := IntToStr(FSelectedPenalty);
            ReservoirPropertiesDialog.PenaltyStructureEdit.OnExit(ReservoirPropertiesDialog.PenaltyStructureEdit);
            DoContextValidation(dvtReservoirPenalty);
          end;
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

procedure TReservoirPropertiesValidator.UpdateResevoirName;
const OPNAME = 'TReservoirPropertiesValidator.UpdateResevoirName';
var
  LReservoirObject: IReservoirData;
  LErrorMessage: string;
begin
  try
    if ReservoirPropertiesDialog.ReservoirNameEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              ReservoirNameEdit.FieldProperty.FieldName,
              ReservoirNameEdit.Text,LErrorMessage)) then
          begin
            LReservoirObject.ReservoirConfigurationData.ReservoirName := Trim(ReservoirNameEdit.Text);
            ReservoirNameEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.ReservoirName);
            DoContextValidation(dvtResPropReservoirName);
          end;
          ReservoirNameEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateResevoirPenaltyStructure;
const OPNAME = 'TReservoirPropertiesValidator.UpdateResevoirPenaltyStructure';
var
  LReservoirObject      : IReservoirData;
  LPenaltyStructureList : IReservoirPenaltyList;
  LPenaltyCountsData    : IReservoirPenaltyCounts;
  LErrorMessage         : string;
begin
  try
    if(ReservoirPropertiesDialog.PenaltyStructureEdit.HasValueChanged) then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty(
           ReservoirPropertiesDialog.PenaltyStructureEdit.FieldProperty.FieldName,
           ReservoirPropertiesDialog.PenaltyStructureEdit.Text,LErrorMessage) then
        begin
          LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
          if (LPenaltyStructureList <> nil) then
          begin
            LPenaltyCountsData := LPenaltyStructureList.ReservoirPenaltyCounts;
            if (LPenaltyCountsData <> nil) then
            begin
              if  (StrToInt(ReservoirPropertiesDialog.PenaltyStructureEdit.Text) >  LPenaltyCountsData.PenaltyStructureCount) then
                ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError := FAppModules.Language.GetString('ErrorString.InvalidPenaltyStructure')
              else
              begin
                ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError :=  LErrorMessage;
                LReservoirObject.ReservoirConfigurationData.PenaltyStructIdentifier := StrToInt(ReservoirPropertiesDialog.PenaltyStructureEdit.Text);
                ReservoirPropertiesDialog.PenaltyStructureEdit.SetFieldValue(ReservoirPropertiesDialog.PenaltyStructureEdit.Text);
                DoContextValidation(dvtReservoirPenalty);
              end;
            end;
          end;
        end
        else
        begin
          ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError :=  LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateResevoirPriority;
const OPNAME = 'TReservoirPropertiesValidator.UpdateResevoirPriority';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.PriorityEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty(
           'ReservoirPriority',PriorityEdit.Text, lErrorMessage) then
          begin
            PriorityEdit.FieldValidationError := LErrorMessage;
            LReservoirObject.ReservoirConfigurationData.Priority := StrToFloat(PriorityEdit.Text);
            PriorityEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.Priority);
            DoContextValidation(dvtResPropPriority);
          end
          else
            PriorityEdit.FieldValidationError := lErrorMessage;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateReservoirExists;
const OPNAME = 'TReservoirPropertiesValidator.UpdateReservoirExists';
var
  LReservoirObject: IReservoirData;
  LReservoirExists : integer;
  LErrorMessage: string;

begin
  try
    LReservoirObject := CurrentReservoir;
    if (LReservoirObject <> nil) then
    begin
      LReservoirExists := LReservoirObject.ReservoirConfigurationData.StatusIndicator;

      if((LReservoirExists = 1) and ( not ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked) or
         (LReservoirExists <> 1) and (ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked)) then
      begin
        if ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked then
          LReservoirExists := 1
        else
          LReservoirExists := 0;

      if FAppModules.FieldProperties.ValidateFieldProperty(
         ReservoirPropertiesDialog.ReservoirExistsChkBox.FieldProperty.FieldName,
         IntToStr(LReservoirExists),LErrorMessage) then
        begin
          LReservoirObject.ReservoirConfigurationData.StatusIndicator := LReservoirExists;
          ReservoirPropertiesDialog.ReservoirExistsChkBox.Checked     :=
            (LReservoirObject.ReservoirConfigurationData.StatusIndicator = 1);
        end
        else
        begin
          ReservoirPropertiesDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateResevoirIncludeSummary;
const OPNAME = 'TReservoirPropertiesValidator.UpdateResevoirIncludeSummary';
var
  LReservoirObject: IReservoirData;
  LIncludeSummary : string;
  LErrorMessage: string;
begin
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoirObject <> nil) then
    begin
      LIncludeSummary := UpperCase(Trim(LReservoirObject.ReservoirConfigurationData.IncludeSummary));

      if((LIncludeSummary = 'Y') and ( not ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked) or
         (LIncludeSummary <> 'Y') and (ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked)) then
      begin
        if ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked then
          LIncludeSummary := 'Y'
        else
          LIncludeSummary := 'N';

        if FAppModules.FieldProperties.ValidateFieldProperty(
         ReservoirPropertiesDialog.SummaryIncludeChkBox.FieldProperty.FieldName,
         LIncludeSummary,LErrorMessage) then
        begin
          LReservoirObject.ReservoirConfigurationData.IncludeSummary := LIncludeSummary;
          ReservoirPropertiesDialog.SummaryIncludeChkBox.Checked     :=
            (LReservoirObject.ReservoirConfigurationData.IncludeSummary = 'Y');
        end
        else
        begin
          ReservoirPropertiesDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateRainCoef;
const OPNAME = 'TReservoirPropertiesValidator.UpdateRainCoef';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.RainCoeffEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('RainCoef',RainCoeffEdit.Text, lErrorMessage) then
          begin
            RainCoeffEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.RainCoef := StrToFloat(RainCoeffEdit.Text);
            RainCoeffEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.RainCoef);
            DoContextValidation(dvtResPropRainCoef);
          end
          else
            RainCoeffEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateXCoord;
const OPNAME = 'TReservoirPropertiesValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.ReservoirXCoordEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',ReservoirXCoordEdit.Text, lErrorMessage) then
          begin
            ReservoirXCoordEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(ReservoirXCoordEdit.Text);
            ReservoirXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
          end
          else
            ReservoirXCoordEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateYCoord;
const OPNAME = 'TReservoirPropertiesValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if ReservoirPropertiesDialog.ReservoirYCoordEdit.HasValueChanged then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',ReservoirYCoordEdit.Text, lErrorMessage) then
          begin
            ReservoirYCoordEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(ReservoirYCoordEdit.Text);
            ReservoirYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
          end
          else
            ReservoirYCoordEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirPropertiesValidator.PopulateDataViewer;
const OPNAME = 'TReservoirPropertiesValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.CurrentReservoir: IReservoirData;
const OPNAME = 'TReservoirPropertiesValidator.CurrentReservoir';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).
              NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TReservoirPropertiesValidator.DoContextValidation';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
           if (AValidationType in [dvtResPropAll, dvtResPropReservoirName]) then
              ValidateReservoirName(lReservoir);

           if (AValidationType in [dvtResPropAll, dvtReservoirPenalty]) then
              ValidateReservoirPenaltyStructure(lReservoir);

           if (AValidationType in [dvtResPropAll, dvtResPropReservoirNumber]) then
              ValidateReservoirNumber(lReservoir);

           if (AValidationType in [dvtResPropAll, dvtResPropPriority]) then
              ValidateReservoirPriority(lReservoir);
           
           if (AValidationType in [dvtResPropAll, dvtResPropRainCoef]) then
              ValidateRainCoef(lReservoir.ReservoirConfigurationData);

           if (AValidationType in [dvtResPropAll, dvtDamlevelFileName]) then
              ValidateDamLevelFileName(lReservoir);
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TReservoirPropertiesValidator.DetermineWizardStatus';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
          DoContextValidation(dvtResPropAll);
          if (lReservoir.ReservoirConfigurationData.PenaltyStructIdentifier <> 0) then
          begin
            Result := 1;
            if (FAllErrorMessages.Count = 0) then
              Result := 2;
          end;    
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.ValidateReservoirName(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPropertiesValidator.ValidateReservoirName';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      ReservoirNameEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage ,'ReservoirName')) then
        FAllErrorMessages.Add(FErrorMessage);
      ReservoirNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.ValidateReservoirPenaltyStructure(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPropertiesValidator.ValidateReservoirPenaltyStructure';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';

      if (ValidPenaltyStructureNumber) then
      begin
        PenaltyStructureEdit.ContextValidationError := FErrorMessage;
        if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'ReservoirPenaltyStructure')) then
          FAllErrorMessages.Add(FErrorMessage);
        PenaltyStructureEdit.FieldValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.ValidateReservoirNumber(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPropertiesValidator.ValidateReservoirNumber';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      ReservoirNumberEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'ReservoirNumber')) then
        FAllErrorMessages.Add(FErrorMessage);
      ReservoirNumberEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.ValidateReservoirPriority(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPropertiesValidator.ValidateReservoirPriority';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      PriorityEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage, 'ReservoirPriority')) then
        FAllErrorMessages.Add(FErrorMessage);
      PriorityEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.ValidateRainCoef(AReservoir: IReservoirConfigurationData);
const OPNAME = 'TReservoirPropertiesValidator.ValidateRainCoef';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      RainCoeffEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.Validate(FErrorMessage, 'RainCoef')) then
        FAllErrorMessages.Add(FErrorMessage);
      RainCoeffEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TReservoirPropertiesValidator.ProcessMetaDataEvent';
var
  lFieldIndex    : string;
  lKeyValues     : string;
  lReservoir     : IReservoirData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
      begin
        with ReservoirPropertiesDialog do
        begin
          lFieldIndex         := '';
          lFieldProperty      := nil;
          if (FActiveControl  = ReservoirNameEdit) then
            lFieldProperty    := ReservoirNameEdit.FieldProperty;
          if (FActiveControl  = PenaltyStructureEdit) then
            lFieldProperty    := PenaltyStructureEdit.FieldProperty;
          if (FActiveControl  = PriorityEdit) then
            lFieldProperty    := PriorityEdit.FieldProperty;
          if (FActiveControl  = RainCoeffEdit) then
            lFieldProperty    := RainCoeffEdit.FieldProperty;
          if (FActiveControl  = SummaryIncludeChkBox) then
            lFieldProperty    := SummaryIncludeChkBox.FieldProperty;
          if (FActiveControl  = ReservoirExistsChkBox) then
            lFieldProperty    := ReservoirExistsChkBox.FieldProperty;

          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lReservoir.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPropertiesValidator.OnSelectHistoricWaterlevelClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnSelectHistoricWaterlevelClick';
var
  LFileSelector : TOpenDialog;
  LFileName     : string;
  lReservoir    : IReservoirData;
  lMessage      : string;
  LPath         : string;
begin
  try
    LFileSelector := TOpenDialog.Create(nil);
    try
      //LFileSelector.Filter  := 'All Files|*.*| (*.TXT)| *.txt|(*.ABS)|*.abs|(*.CIR)|*.cir|(*.DEM)|*.dem|(*.IRR)|*.IRR|(*.IRD)|*.ird|(*.URB)|*.urb';
      LFileSelector.Filter  := '(*.csv)| *.csv|All Files|*.*|';
      LPath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DamLevelsPath;
      if not DirectoryExists(LPath) then
        ForceDirectories(LPath);
      LFileSelector.InitialDir := LPath;
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      if LFileSelector.Execute then
      begin
        LFileName := LFileSelector.FileName;
        if(UpperCase(LFileName) <> UpperCase( ReservoirPropertiesDialog.DamLevelsFileNameCbx.Text)) then
        begin
          lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
          if (lReservoir <> nil) then
          begin
            with ReservoirPropertiesDialog do
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                  DamLevelsFileNameCbx.FieldProperty.FieldName,
                  LFileName,lMessage)) then
              begin
                lReservoir.ReservoirConfigurationData.DamLevelsFileName := LFileName;
                RepopulateHistoricWaterLevels;
                DamLevelsFileNameCbx.ItemIndex := DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName);
                DoContextValidation(dvtDamlevelFileName);
                SetGridGraphBtnState(lReservoir);
              end
              else
                DamLevelsFileNameCbx.ValidationError := lMessage;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.RepopulateHistoricWaterLevels;
const OPNAME = 'TReservoirPropertiesValidator.RepopulateHistoricWaterLevels';
var
  LFileNamesList : TFileNamesList;
  LFileName      : string;
  LIndex         : integer;
  lReservoir    : IReservoirData;
begin
  try
    with ReservoirPropertiesDialog do
    begin
      DamLevelsFileNameCbx.Items.Clear;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        LFileName := UpperCase(Trim(ExtractFileName(LFileNamesList.FileNameObject[LIndex].FileName)));
        DamLevelsFileNameCbx.Items.Add(LFileName);
      end;
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil)  and (lReservoir.ReservoirConfigurationData.DamLevelsFileName <> '')then
      DamLevelsFileNameCbx.ItemIndex := DamLevelsFileNameCbx.Items.IndexOf(ExtractFileName(lReservoir.ReservoirConfigurationData.DamLevelsFileName));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPropertiesValidator.UpdateDamLevelFileName;
const OPNAME = 'TReservoirPropertiesValidator.UpdateDamLevelFileName';
var
  lReservoir : IReservoirData;
  lMessage   : string;
begin
  try
    lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (lReservoir <> nil) then
    begin
      with ReservoirPropertiesDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DamLevelsFileNameCbx.FieldProperty.FieldName,
            DamLevelsFileNameCbx.Text,lMessage)) then
        begin
          lReservoir.ReservoirConfigurationData.DamLevelsFileName := DamLevelsFileNameCbx.Text;
          DamLevelsFileNameCbx.SetFieldIndex(DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName));
          DoContextValidation(dvtDamlevelFileName);
        end
        else
          DamLevelsFileNameCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnViewGridClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnViewGridClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRID');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=DAMLEVELS');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnViewGraphClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnViewGraphClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRAPH');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=DAMLEVELS');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.SetGridGraphBtnState(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPropertiesValidator.SetGridGraphBtnState';
var
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
  LIndex         : integer;
begin
  try
    ReservoirPropertiesDialog.DamLevelsFileNameGridBtn.Enabled := False;
    ReservoirPropertiesDialog.DamLevelsFileNameGraphBtn.Enabled := False;
    if(AReservoir <> nil) and (AReservoir.ReservoirConfigurationData.DamLevelsFileName <> '') then
    begin
      LFileNameObject := nil;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        if(UpperCase(AReservoir.ReservoirConfigurationData.DamLevelsFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].ShortName)) then
        begin
          LFileNameObject := LFileNamesList.FileNameObject[LIndex];
          Break;
        end;
      end;
      if(LFileNameObject <> nil) then
      begin
        ReservoirPropertiesDialog.DamLevelsFileNameGridBtn.Enabled  := LFileNameObject.SavedInDB;
        ReservoirPropertiesDialog.DamLevelsFileNameGraphBtn.Enabled := LFileNameObject.SavedInDB;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.ValidateDamLevelFileName(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPropertiesValidator.ValidateDamLevelFileName';
begin
  try
    with ReservoirPropertiesDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage, 'DamLevelsFileName')) then
      begin
        DamLevelsFileNameCbx.InValidationError := FALSE;
        DamLevelsFileNameCbx.ValidationError := '';
        DamLevelsFileNameCbx.ShowErrorState(FALSE);
      end
      else
      begin
        DamLevelsFileNameCbx.InValidationError := TRUE;
        DamLevelsFileNameCbx.ValidationError := FErrorMessage;
        DamLevelsFileNameCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesValidator.ValidPenaltyStructureNumber: boolean;
const OPNAME = 'TReservoirPropertiesValidator.ValidPenaltyStructureNumber';
var
  LPenaltyStructureList : IReservoirPenaltyList;
  LPenaltyCountsData    : IReservoirPenaltyCounts;
  LResult               : boolean;
  LErrorMessage         : string;
begin
  Result := False;
  try
    LResult := False;
    LErrorMessage := '';
    LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
    if (LPenaltyStructureList <> nil) then
    begin
      LPenaltyCountsData := LPenaltyStructureList.ReservoirPenaltyCounts;
      if (LPenaltyCountsData <> nil) then
      begin
        if  (StrToInt(ReservoirPropertiesDialog.PenaltyStructureEdit.Text) >  LPenaltyCountsData.PenaltyStructureCount) then
        begin
          ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError := FAppModules.Language.GetString('ErrorString.InvalidPenaltyStructure');
          LResult := False;
        end
        else
        begin
          ReservoirPropertiesDialog.PenaltyStructureEdit.FieldValidationError :=  LErrorMessage;
          LResult := True;
        end;
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.UpdateReservoirAreaGroup;
const OPNAME = 'TReservoirPropertiesValidator.UpdateReservoirAreaGroup';
var
  LErrorMessage           : string;
  LReservoirData          : IReservoirData;
  LReservoirAreaGroup     : IReservoirAreaGroup;
  LReservoirAreaGroupList : IReservoirAreaGroupList;
begin
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if LReservoirData <> nil then
    begin
      with ReservoirPropertiesDialog do
      begin
        LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkElementData.ReservoirAreaGroupList;
        if (LReservoirAreaGroupList <> nil) then
        begin
          LReservoirAreaGroup := LReservoirAreaGroupList.ReservoirAreaGroupByName(ReservoirAreaGroupCbx.Text);
          if(LReservoirAreaGroup <> nil) AND
          (LReservoirAreaGroup.GroupID <> LReservoirData.ReservoirConfigurationData.GroupID) then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty(ReservoirAreaGroupCbx.FieldProperty.FieldName,
                IntToStr(LReservoirAreaGroup.GroupID),LErrorMessage)) then
            begin
              ReservoirAreaGroupCbx.ValidationError := LErrorMessage;
              LReservoirData.ReservoirConfigurationData.GroupID := LReservoirAreaGroup.GroupID;
              RePopulateDataViewer;
            end
            else
              ReservoirAreaGroupCbx.ValidationError := LErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.RepopulateReservoirAreaGroup;
const OPNAME = 'TReservoirPropertiesValidator.RepopulateReservoirAreaGroup';
var
  LReservoirAreas         : TStringList;
  LIndex                  : integer;
  LReservoirAreaGroupName : string;
  LReservoirAreaGroupList : IReservoirAreaGroupList;
  LReservoirAreaGroup     : IReservoirAreaGroup;
  LReservoirData          : IReservoirData;
begin
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if (LReservoirData <> nil) then
    begin
      LReservoirAreas := TStringList.Create;
      try
        LReservoirAreas.Sorted := True;
        LReservoirAreas.Duplicates := dupIgnore;
        for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkElementData.ReservoirAreaGroupList.GroupAreaCount -1 do
        begin
          LReservoirAreas.Add(TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkElementData.ReservoirAreaGroupList.
                     ReservoirAreaGroupByIndex(LIndex).GroupName)
        end;

        ReservoirPropertiesDialog.ReservoirAreaGroupCbx.Items.Assign(LReservoirAreas);
        if (LReservoirData.ReservoirConfigurationData.GroupID <> 0) then
        begin
          LReservoirAreaGroupName := '';
          LReservoirAreaGroupList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirAreaGroupList;
          LReservoirAreaGroup := LReservoirAreaGroupList.
                              ReservoirAreaGroupByID(LReservoirData.ReservoirConfigurationData.GroupID);

          if (LReservoirAreaGroup <> nil) then
          begin
            LReservoirAreaGroupName := LReservoirAreaGroup.GroupName;
            ReservoirPropertiesDialog.ReservoirAreaGroupCbx.ItemIndex :=
            ReservoirPropertiesDialog.ReservoirAreaGroupCbx.Items.IndexOf(LReservoirAreaGroupName);
          end;
        end;
      finally
        LReservoirAreas.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnReservoirExistsClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnReservoirExistsClick';
begin
  try
    if (ReservoirPropertiesDialog.ReservoirExistsChkBox.HasValueChanged) then
       UpdateReservoirExists;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesValidator.OnSummaryIncludeClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesValidator.OnSummaryIncludeClick';
begin
  try
    if (ReservoirPropertiesDialog.SummaryIncludeChkBox.HasValueChanged) then
       UpdateResevoirIncludeSummary;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


