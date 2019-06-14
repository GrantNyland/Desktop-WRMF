{******************************************************************************}
{*  UNIT      : Contains the class TSpecifiedInflowDataValidator.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/12/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USpecifiedInflowDataValidator;

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
  USpecifiedInflowDataDialog;

type
  TSpecifiedInflowDataValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnSelectInflowFileNameClick(Sender: TObject);
    procedure OnViewGridClick(Sender: TObject);
    procedure OnViewGraphClick(Sender: TObject);
    procedure PopulateInflowFileNameCbx;
    procedure SetGridGraphBtnState(AFeature : ISpecifiedInflowFeature);
    procedure RePopulateDataViewer;
    procedure UpdateFeatureName;
    procedure UpdateFileName;
    procedure ValidateFeatureName(AFeature: ISpecifiedInflowFeature);
    procedure ValidateInflowFileName(AFeature: ISpecifiedInflowFeature);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function SpecifiedInflowDataDialog : TSpecifiedInflowDataDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Graphics,
  Contnrs,
  UConstants,
  VCL.Dialogs,
  Windows,
  UFileNames,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations, UAbstractFileNamesObject;

{******************************************************************************}
{* TSpecifiedInflowDataValidator                                              *}
{******************************************************************************}

procedure TSpecifiedInflowDataValidator.CreateMemberObjects;
const OPNAME = 'TSpecifiedInflowDataValidator.CreateMemberObjects';
var
  lPanel: TSpecifiedInflowDataDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TSpecifiedInflowDataDialog.Create(FPanelOwner,FAppModules);
    lPanel := SpecifiedInflowDataDialog;
    with lPanel do
    begin
      FeatureNameEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('SpecifiedInflowFeatureName');
      FeatureNameEdit.OnEnter         := OnEditControlEnter;
      FeatureNameEdit.OnExit          := OnEditControltExit;

      FileNameComboBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('InflowFileName');
      FileNameComboBox.OnEnter         := OnEditControlEnter;
      FileNameComboBox.OnExit          := OnEditControltExit;

      InflowFileNameBtn.OnEnter  := OnEditControlEnter;
      InflowFileNameBtn.OnClick  := OnSelectInflowFileNameClick;
      InflowFileNameBtn.Enabled  := (FAppModules.User.UserRights in CUR_EditData) and
                                             (not FAppModules.StudyArea.ScenarioLocked);
      InflowFileNameGridBtn.OnEnter  := OnEditControlEnter;
      InflowFileNameGridBtn.OnExit   := OnEditControltExit;
      InflowFileNameGridBtn.OnClick  := OnViewGridClick;
      InflowFileNameGraphBtn.OnEnter := OnEditControlEnter;
      InflowFileNameGraphBtn.OnExit  := OnEditControltExit;
      InflowFileNameGraphBtn.OnClick := OnViewGraphClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.DestroyMemberObjects;
const OPNAME = 'TSpecifiedInflowDataValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.Initialise: boolean;
const OPNAME = 'TSpecifiedInflowDataValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSpecifiedInflowDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.SpecifiedInflow');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.ClearDataViewer;
const OPNAME = 'TSpecifiedInflowDataValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with SpecifiedInflowDataDialog do
    begin
      FeatureNameEdit.SetFieldValue('');
      FileNameComboBox.Items.Clear;
      FileNameComboBox.ItemIndex := -1;
      FileNameComboBox.Text := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.PopulateDataViewer;
const OPNAME = 'TSpecifiedInflowDataValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtSpecifiedInflowFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.RePopulateDataViewer;
const OPNAME = 'TSpecifiedInflowDataValidator.RePopulateDataViewer';
var
  lInflowFeature  : ISpecifiedInflowFeature;
  lFieldProperty  : TAbstractFieldProperty;
  LFileName,
  lKeyValues      : string;
  LFileNameObject : TAbstractModelFileName;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lInflowFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FFeatureID];
      if (lInflowFeature <> nil) then
      begin
        with SpecifiedInflowDataDialog do
        begin
          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues     := lInflowFeature.GetKeyValues(lFieldProperty.FieldName, '');
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          FeatureNameEdit.SetFieldValue(lInflowFeature.FeatureName);
          LFileName := lInflowFeature.InflowFileName;
          FileNameComboBox.Items.Clear;
          PopulateInflowFileNameCbx;
          FileNameComboBox.SetFieldIndex(FileNameComboBox.Items.IndexOf(LFileName));
          
          InflowFileImportedLabel.Caption := '';
          LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(lInflowFeature.InflowFileName);
          if(LFileNameObject <> nil) then
          begin
            InflowFileImportedLabel.Visible := SpecifiedInflowDataDialog.FileNameComboBox.Text <> ''; 
            if (LFileNameObject.SavedInDB) and (SpecifiedInflowDataDialog.FileNameComboBox.Text <> '') then
              InflowFileImportedLabel.Caption := FAppModules.Language.GetString('TSpecifiedInflowDataValidator.InflowFileSavedInDB')
            else
              InflowFileImportedLabel.Caption := FAppModules.Language.GetString('TSpecifiedInflowDataValidator.InflowFileNotSavedInDB');
          end;
          SetGridGraphBtnState(lInflowFeature);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.SaveState: boolean;
const OPNAME = 'TSpecifiedInflowDataValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.SpecifiedInflowDataDialog : TSpecifiedInflowDataDialog;
const OPNAME = 'TSpecifiedInflowDataValidator.SpecifiedInflowDataDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TSpecifiedInflowDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSpecifiedInflowDataValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'DownNodeNumber') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TSpecifiedInflowDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TSpecifiedInflowDataValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TSpecifiedInflowDataValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with SpecifiedInflowDataDialog do
    begin
      if ((Sender = FeatureNameEdit) AND
          (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName;
      if ((Sender = FileNameComboBox) AND
          (FileNameComboBox.HasValueChanged)) then
        UpdateFileName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.OnSelectInflowFileNameClick(Sender: TObject);
const OPNAME = 'TSpecifiedInflowDataValidator.OnSelectInflowFileNameClick';
var
  LFileSelector : TOpenDialog;
  LInflowFilesPath,
  LPath,
  LFileName,
  LFileNameStr  : string;
  LFeature      : ISpecifiedInflowFeature;
begin
  try
    LFileSelector := TOpenDialog.Create(nil);
    try
      LFileSelector.Title   := 'Select inflow file.';
      LFileSelector.Filter  := 'All Files|*.*|(*.INF)|*.inf';
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      LFileSelector.InitialDir := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
      if LFileSelector.Execute then
      begin
        LFileNameStr := LFileSelector.FileName;
        LFileName    := ExtractFileName(LFileNameStr);
        LPath        := ExtractFilePath(LFileNameStr);

        LInflowFilesPath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
        if(LInflowFilesPath <> LPath) then
        begin
          if(MessageDlg('The selected inf file will be copied to the scenario folder before being imported. Continue?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
            Exit;
          LFileName   :=  IncludeTrailingPathDelimiter(LInflowFilesPath ) + LFileName;
          if FileExists(LFileName) then
          begin
            if(MessageDlg('File already exist in the destination folder. Do you want to override it?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
              Exit;
          end;
          if not DirectoryExists(LInflowFilesPath) then
            ForceDirectories(LInflowFilesPath);
          CopyFile(PChar(LFileNameStr),PChar(LFileName),False);
          LFileNameStr := LFileName;
        end;

        if(UpperCase(LFileNameStr) <> UpperCase(SpecifiedInflowDataDialog.FileNameComboBox.Text)) then
        begin
          LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FFeatureID];
          if (LFeature <> nil) then
          begin
            LFeature.InflowFileName := LFileNameStr;
            RePopulateDataViewer;
            ValidateInflowFileName(lFeature)
          end;
        end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.UpdateFeatureName;
const OPNAME = 'TSpecifiedInflowDataValidator.UpdateFeatureName';
var
  lInflowFeature : ISpecifiedInflowFeature;
  lMessage       : string;
begin
  try
    lInflowFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedInflowFeatureList.
                          SpecifiedInflowFeatureByID[FFeatureID];
    if (lInflowFeature <> nil) then
    begin
      with SpecifiedInflowDataDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FeatureNameEdit.FieldProperty.FieldName,
            FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lInflowFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lInflowFeature.FeatureName);
          DoContextValidation(dvtSpecifiedInflowFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.UpdateFileName;
const OPNAME = 'TSpecifiedInflowDataValidator.UpdateFileName';
var
  lInflowFeature : ISpecifiedInflowFeature;
  lMessage       : string;
begin
  try
    lInflowFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedInflowFeatureList.
                          SpecifiedInflowFeatureByID[FFeatureID];
    if (lInflowFeature <> nil) then
    begin
      with SpecifiedInflowDataDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FileNameComboBox.FieldProperty.FieldName,
            FileNameComboBox.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lInflowFeature.InflowFileName := Trim(FileNameComboBox.Text);
          DoContextValidation(dvtSpecifiedInflowFileName);
          RePopulateDataViewer;
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.ValidateFeatureName(AFeature: ISpecifiedInflowFeature);
const OPNAME = 'TSpecifiedInflowDataValidator.ValidateFeatureName';
begin
  try
    with SpecifiedInflowDataDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'SpecifiedInflowFeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.ValidateInflowFileName(AFeature: ISpecifiedInflowFeature);
const OPNAME = 'TSpecifiedInflowDataValidator.ValidateInflowFileName';
begin
  try
    with SpecifiedInflowDataDialog do
    begin
      FErrorMessage := '';
      FileNameComboBox.InValidationError := False;
      FileNameComboBox.ShowErrorState(False);
      if (NOT AFeature.Validate(FErrorMessage, 'InflowFileName')) then
      begin
        FileNameComboBox.ValidationError   := FErrorMessage;
        FileNameComboBox.InValidationError := True;
        FileNameComboBox.ShowErrorState(True);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TSpecifiedInflowDataValidator.DoContextValidation';
var
  lFeature     : ISpecifiedInflowFeature;
  lFeatureList : ISpecifiedInflowFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedInflowFeatureList;
      lFeature     := lFeatureList.SpecifiedInflowFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtSpecifiedInflowFeature, dvtSpecifiedInflowFeatureName]) then
           ValidateFeatureName(lFeature);
        if (AValidationType = dvtSpecifiedInflowFeature) or (AValidationType =dvtSpecifiedInflowFileName) then
           ValidateInflowFileName(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TSpecifiedInflowDataValidator.DetermineWizardStatus';
var
  lFeature         : ISpecifiedInflowFeature;
  lFeatureList     : ISpecifiedInflowFeatureList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedInflowFeatureList;
      lFeature := lFeatureList.SpecifiedInflowFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtSpecifiedInflowFeature);
        if (lFeature.FeatureName <> '') then
        begin
          Result := 1;
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TSpecifiedInflowDataValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : ISpecifiedInflowFeature;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with SpecifiedInflowDataDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.PopulateInflowFileNameCbx;
const OPNAME = 'TSpecifiedInflowDataValidator.PopulateInflowFileNameCbx';
var
  LSpecifiedInflowFeatureList : ISpecifiedInflowFeatureList;
  LSpecifiedflowFeature       : ISpecifiedInflowFeature;
  LFileNamesList              : TFileNamesList;
  LFileName,
  LFileNamePrefix,
  LRunSequenceType            : string;
  LIndex                      : integer;
begin
  try
    with SpecifiedInflowDataDialog do
    begin
      FileNameComboBox.Items.Clear;
      FileNameComboBox.Sorted := True;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount - 1 do
      begin
        LFileName := LFileNamesList.FileNameObject[LIndex].FileName;
        if(UpperCase(ExtractFileExt(LFileName)) = '.INF') then
        begin
          LFileNamePrefix := ExtractFileName(LFileName);
          LFileNamePrefix := Copy(LFileNamePrefix,0,(Length(LFileNamePrefix) - 4));
          LRunSequenceType := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.RunSequenceType;
          if(Pos(LRunSequenceType,LFileNamePrefix) = Length(LFileNamePrefix)) then
            FileNameComboBox.Items.Add(LFileName);
        end;
      end;

      LSpecifiedInflowFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                                     NetworkFeaturesData.SpecifiedInflowFeatureList;
      for LIndex := 0 to LSpecifiedInflowFeatureList.SpecifiedInflowFeatureCount - 1 do
      begin
        LSpecifiedflowFeature := LSpecifiedInflowFeatureList.SpecifiedInflowFeatureByIndex[LIndex];
        if(LSpecifiedflowFeature.InflowFileName <> '') then
        begin
          if FileNameComboBox.Items.IndexOf(LSpecifiedflowFeature.InflowFileName) < 0 then
          begin
            LFileNamePrefix := ExtractFileName(LSpecifiedflowFeature.InflowFileName);
            LFileNamePrefix := Copy(LFileNamePrefix,0,(Length(LFileNamePrefix) - 4));
            LRunSequenceType := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.RunSequenceType;
            if(Pos(LRunSequenceType,LFileNamePrefix) = Length(LFileNamePrefix)) then
              FileNameComboBox.Items.Add(LSpecifiedflowFeature.InflowFileName);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.OnViewGraphClick(Sender: TObject);
const OPNAME = 'TSpecifiedInflowDataValidator.OnViewGraphClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRAPH');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FFeatureID]));
      LContextDataList.Add('MODELELEMENTTYPE=SPECIFIEDINFLOWFEATURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.OnViewGridClick(Sender: TObject);
const OPNAME = 'TSpecifiedInflowDataValidator.OnViewGridClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRID');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FFeatureID]));
      LContextDataList.Add('MODELELEMENTTYPE=SPECIFIEDINFLOWFEATURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataValidator.SetGridGraphBtnState(AFeature: ISpecifiedInflowFeature);
const OPNAME = 'TSpecifiedInflowDataValidator.SetGridGraphBtnState';
var
  LFileNamesList  : TFileNamesList;
  LFileNameObject : TAbstractModelFileName;
  LIndex          : integer;
begin
  try
    SpecifiedInflowDataDialog.InflowFileNameGridBtn.Enabled  := False;
    SpecifiedInflowDataDialog.InflowFileNameGraphBtn.Enabled := False;
    if(AFeature <> nil) and (AFeature.InflowFileName <> '') then
    begin
      LFileNameObject := nil;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount - 1 do
      begin
        if(UpperCase(AFeature.InflowFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].FileName)) then
        begin
          LFileNameObject := LFileNamesList.FileNameObject[LIndex];
          Break;
        end;
      end;
      if(LFileNameObject <> nil) then
      begin
        SpecifiedInflowDataDialog.InflowFileNameGridBtn.Enabled  := (LFileNameObject.SavedInDB) and
                                                                    (SpecifiedInflowDataDialog.FileNameComboBox.Text <> '');
        SpecifiedInflowDataDialog.InflowFileNameGraphBtn.Enabled := LFileNameObject.SavedInDB and
                                                                    (SpecifiedInflowDataDialog.FileNameComboBox.Text <> '');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


