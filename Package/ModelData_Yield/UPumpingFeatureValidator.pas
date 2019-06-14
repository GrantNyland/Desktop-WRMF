{******************************************************************************}
{*  UNIT      : Contains the class TPumpingFeatureValidator.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/12                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPumpingFeatureValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  VoaimsCom_TLB,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UPumpingFeatureDialog;

type
  TPumpingFeatureValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure RePopulateDataViewer;
    procedure UpdateFeatureName;
    procedure UpdateChannelPumpingHead;
    procedure UpdateChannelPumpingEfficiency;
    procedure ValidateFeatureName (AFeature : IPumpingFeature);
    procedure ValidatePumpingHead (AFeature : IPumpingFeature);
    procedure ValidatePumpingEfficiency (AFeature : IPumpingFeature);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function PumpingFeatureDialog: TPumpingFeatureDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TPumpingFeatureValidator                                                    *}
{******************************************************************************}

procedure TPumpingFeatureValidator.CreateMemberObjects;
const OPNAME = 'TPumpingFeatureValidator.CreateMemberObjects';
var
  lpPanel : TPumpingFeatureDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel  := TPumpingFeatureDialog.Create(FPanelOwner,FAppModules);
    lpPanel := PumpingFeatureDialog;
    with lpPanel do
    begin
      FeatureNameEdit.FieldProperty         := FAppmodules.FieldProperties.FieldProperty('PumpingFeatureName');
      FeatureNameEdit.OnEnter               := OnEditControlEnter;
      FeatureNameEdit.OnExit                := OnEditControltExit;

      PumpHeadEdit.FieldProperty            := FAppModules.FieldProperties.FieldProperty('PumpingHead');
      PumpHeadEdit.OnEnter                  := OnEditControlEnter;
      PumpHeadEdit.OnExit                   := OnEditControltExit;

      PumpEfficiencyEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('PumpEfficiency');
      PumpEfficiencyEdit.OnEnter            := OnEditControlEnter;
      PumpEfficiencyEdit.OnExit             := OnEditControltExit;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.DestroyMemberObjects;
const OPNAME = 'TPumpingFeatureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.Initialise: boolean;
const OPNAME = 'TPumpingFeatureValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.LanguageHasChanged: boolean;
const OPNAME = 'TPumpingFeatureValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.PumpingFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.ClearDataViewer;
const OPNAME = 'TPumpingFeatureValidator.ClearDataViewer';
var
  lPanel : TPumpingFeatureDialog;
begin
  inherited ClearDataViewer;
  try
    lPanel := PumpingFeatureDialog;
    with lPanel do
    begin
      PumpHeadEdit.Text       := '-1';
      PumpEfficiencyEdit.Text := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.PopulateDataViewer;
const OPNAME = 'TPumpingFeatureValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtPumpingFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.RePopulateDataViewer;
const OPNAME = 'TPumpingFeatureValidator.RePopulateDataViewer';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IPumpingFeature;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.PumpingFeatureList.PumpingFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with PumpingFeatureDialog do
        begin
          lFieldIndex := '';

          lFieldProperty := PumpHeadEdit.FieldProperty;
          lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          PumpHeadEdit.HasChanges  :=
             FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
          PumpHeadEdit.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

          lFieldProperty := PumpEfficiencyEdit.FieldProperty;
          lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          PumpEfficiencyEdit.HasChanges :=
             FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
          PumpEfficiencyEdit.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          FeatureNameEdit.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

          PumpHeadEdit.SetFieldValue(FloatToStr(lFeature.PumpingHead));
          PumpEfficiencyEdit.SetFieldValue(FloatToStr(lFeature.PumpingEfficiency));
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.SaveState: boolean;
const OPNAME = 'TPumpingFeatureValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.PumpingFeatureDialog : TPumpingFeatureDialog;
const OPNAME = 'TPumpingFeatureValidator.PumpingFeatureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TPumpingFeatureDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPumpingFeatureValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.StudyHasChanged: boolean;
const OPNAME = 'TPumpingFeatureValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPumpingFeatureValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPumpingFeatureValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with PumpingFeatureDialog do
    begin
      if ((Sender = PumpHeadEdit) AND (NOT PumpHeadEdit.HasChanges) AND (PumpHeadEdit.HasValueChanged)) then
        UpdateChannelPumpingHead
      else
      if ((Sender = PumpEfficiencyEdit) AND (NOT PumpEfficiencyEdit.HasChanges) AND (PumpEfficiencyEdit.HasValueChanged)) then
        UpdateChannelPumpingEfficiency
      else
      if ((sender = FeatureNameEdit) AND (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.UpdateChannelPumpingHead;
const OPNAME = 'TPumpingFeatureValidator.UpdateChannelPumpingHead';
var
  lFeature : IPumpingFeature;
  lMessage : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.PumpingFeatureList.PumpingFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with PumpingFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            PumpHeadEdit.FieldProperty.FieldName,
            PumpHeadEdit.Text,lMessage)) then
        begin
          PumpHeadEdit.FieldValidationError := lMessage;
          lFeature.PumpingHead := (StrToFloat(Trim(PumpHeadEdit.Text)));
          PumpHeadEdit.SetFieldValue(FloatToStr(lFeature.PumpingHead));
          DoContextValidation(dvtPumpingFeatureHead);
        end
        else
          PumpHeadEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.UpdateChannelPumpingEfficiency;
const OPNAME = 'TPumpingFeatureValidator.UpdateChannelPumpingEfficiency';
var
  lFeature : IPumpingFeature;
  lMessage : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.PumpingFeatureList.PumpingFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with PumpingFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            PumpEfficiencyEdit.FieldProperty.FieldName,
            PumpEfficiencyEdit.Text,lMessage)) then
        begin
          PumpEfficiencyEdit.FieldValidationError := lMessage;
          lFeature.PumpingEfficiency := (StrToFloat(Trim(PumpEfficiencyEdit.Text)));
          PumpEfficiencyEdit.SetFieldValue(FloatToStr(lFeature.PumpingEfficiency));
          DoContextValidation(dvtPumpingFeatureEfficiency);
        end
        else
          PumpEfficiencyEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.UpdateFeatureName;
const OPNAME = 'TPumpingFeatureValidator.UpdateFeatureName';
var
  lMessage        : string;
  lPumpingFeature : IPumpingFeature;
begin
  try
    lPumpingFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                       .PumpingFeatureList.PumpingFeatureByID[FFeatureID];
    if (lPumpingFeature <> nil) then
    begin
      with PumpingFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(FeatureNameEdit.FieldProperty.FieldName,
          FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lPumpingFeature.FeatureName := trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lPumpingFeature.FeatureName);
          DoContextValidation(dvtPumpingFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.ValidateFeatureName (AFeature : IPumpingFeature);
const OPNAME = 'TPumpingFeatureValidator.ValidateFeatureName';
begin
  try
    with PumpingFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'PumpingFeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.DoContextValidation (AValidationType : TDialogValidationType);
const OPNAME = 'TPumpingFeatureValidator.DoContextValidation';
var
  lFeature     : IPumpingFeature;
  lFeatureList : IPumpingFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PumpingFeatureList;
      lFeature     := lFeatureList.PumpingFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtPumpingFeature, dvtPumpingFeatureName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtPumpingFeature, dvtPumpingFeatureHead]) then
          ValidatePumpingHead(lFeature);
        if (AValidationType in [dvtPumpingFeature, dvtPumpingFeatureEfficiency]) then
          ValidatePumpingEfficiency(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TPumpingFeatureValidator.DetermineWizardStatus';
var
  lFeature     : IPumpingFeature;
  lFeatureList : IPumpingFeatureList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PumpingFeatureList;
      lFeature := lFeatureList.PumpingFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtPumpingFeature);
        if ((lFeature.PumpingHead > 0) OR (lFeature.PumpingEfficiency > 0)) then
        begin
          Result := 1;
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.ValidatePumpingHead (AFeature : IPumpingFeature);
const OPNAME = 'TPumpingFeatureValidator.ValidatePumpingHead';
begin
  try
    with PumpingFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'PumpingHead')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      PumpHeadEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureValidator.ValidatePumpingEfficiency (AFeature : IPumpingFeature);
const OPNAME = 'TPumpingFeatureValidator.ValidatePumpingEfficiency';
begin
  try
    with PumpingFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate( FErrorMessage, 'PumpEfficiency')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      PumpEfficiencyEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TPumpingFeatureValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFeature       : IPumpingFeature;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.PumpingFeatureList.PumpingFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (FActiveControl.ClassNameIs('TFieldEdit')) then
        begin
          lFieldProperty := TFieldEdit(FActiveControl).FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, '');
            FAppModules.Changes.ShowParameterChanges
              (lFieldProperty.FieldName, lKeyValues, '');
            RePopulateDataViewer;
            FAppModules.Changes.SetParameterChanges(TRUE);
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TPumpingFeatureValidator.ProcessMetaDataEvent';
var
  lKeyValues     : string;
  lFeature       : IPumpingFeature;
  lFieldIndex    : string;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible  AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.PumpingFeatureList.PumpingFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with PumpingFeatureDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = PumpHeadEdit) then
            lFieldProperty := PumpHeadEdit.FieldProperty
          else
          if (FActiveControl = PumpEfficiencyEdit) then
            lFieldProperty := PumpEfficiencyEdit.FieldProperty;

          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName,'');
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, '');
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

