{******************************************************************************}
{*  UNIT      : Contains the class TConfigurationFilesValidator.              *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/09/04                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UConfigurationFilesValidator;

interface
{$WARN UNIT_PLATFORM OFF}

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.FileCtrl,
  VCL.Dialogs,
  UAbstractObject,
  VoaimsCom_TLB,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UConfigurationFilesDialog;

type
  TConfigurationFilesValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
//    procedure UpdateParamFileName;


    procedure UpdateParamFile(AFileName: string);
    procedure UpdateInputPath(AInputFilePath: string);
    procedure UpdateOutputPath(AOutputFilePath: string);
    procedure UpdateHydrologyPath(AHydrologyFilePath: string);
    procedure UpdateSpecifiedDemandPath(ADemandFilePath: string);
    procedure RePopulateDataViewer;

    procedure OnInputPathClick(Sender: TObject);
    procedure OnOutputPathClick(Sender: TObject);
    procedure OnHydrologyPathClick(Sender: TObject);
    procedure OnSpecifiedDemandPathClick(Sender: TObject);
    procedure OnSelectParamFileClick(Sender: TObject);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function ConfigurationFilesDialog: TConfigurationFilesDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  URunConfigurationData;

{******************************************************************************}
{* TConfigurationFilesValidator                                                    *}
{******************************************************************************}

procedure TConfigurationFilesValidator.CreateMemberObjects;
const OPNAME = 'TConfigurationFilesValidator.CreateMemberObjects';
var
  lpPanel : TConfigurationFilesDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel  := TConfigurationFilesDialog.Create(FPanelOwner,FAppModules);
    lpPanel := ConfigurationFilesDialog;
    with lpPanel do
    begin

      DataPathPrefixEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('DataFilesPrefix');
      DataPathPrefixEdit.OnEnter             := OnEditControlEnter;
      DataPathPrefixEdit.IsEnabled           := FALSE;

      if (FAppModules.StudyArea.ModelVersion <> '7') then
      begin
      InputPathEdit.FieldProperty            := FAppModules.FieldProperties.FieldProperty('InputPath');
      InputPathButton.OnClick                := OnInputPathClick;
      InputPathEdit.OnEnter                  := OnEditControlEnter;
      InputPathEdit.IsEnabled                := FALSE;
      end;

      HydrologyPathEdit.FieldProperty        := FAppModules.FieldProperties.FieldProperty('HydrologyPath');
      HydrologyPathButton.OnClick            := OnHydrologyPathClick;
      HydrologyPathEdit.OnEnter              := OnEditControlEnter;
      HydrologyPathEdit.IsEnabled            := FALSE;

      SpecifiedDemandPathEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('SpecifiedDemandPath');
      SpecifiedDemandPathButton.OnClick      := OnSpecifiedDemandPathClick;
      SpecifiedDemandPathEdit.OnEnter        := OnEditControlEnter;
      SpecifiedDemandPathEdit.IsEnabled      := FALSE;

      OutputPathEdit.FieldProperty           := FAppModules.FieldProperties.FieldProperty('OutputPath');
      OutputPathButton.OnClick               := OnOutputPathClick;
      OutputPathEdit.OnEnter                 := OnEditControlEnter;
      OutputPathEdit.IsEnabled               := FALSE;

      ParamFileNameEdit.OnEnter               := OnEditControlEnter;
      ParamFileNameEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ParamFile');
      ParamFileNameEdit.IsEnabled             := FALSE;
      SelectParamFileButton.FieldProperty     := FAppModules.FieldProperties.FieldProperty('ParamFile');
      SelectParamFileButton.OnEnter           := OnEditControlEnter;
      SelectParamFileButton.OnClick           := OnSelectParamFileClick;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.DestroyMemberObjects;
const OPNAME = 'TConfigurationFilesValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesValidator.Initialise: boolean;
const OPNAME = 'TConfigurationFilesValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesValidator.LanguageHasChanged: boolean;
const OPNAME = 'TConfigurationFilesValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Configuration Files';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.ClearDataViewer;
const OPNAME = 'TConfigurationFilesValidator.ClearDataViewer';
var
  lPanel : TConfigurationFilesDialog;
begin
  inherited ClearDataViewer;
  try
    lPanel := ConfigurationFilesDialog;
    with lPanel do
    begin
      if (FAppModules.StudyArea.ModelVersion <> '7') then
        InputPathEdit.SetFieldValue('');

      OutputPathEdit.SetFieldValue('');
      HydrologyPathEdit.SetFieldValue('');
      SpecifiedDemandPathEdit.SetFieldValue('');
      ParamFileNameEdit.SetFieldValue('');
      DataPathPrefixEdit.SetFieldValue('');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.PopulateDataViewer;
const OPNAME = 'TConfigurationFilesValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtPumpingFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.RePopulateDataViewer;
const OPNAME = 'TConfigurationFilesValidator.RePopulateDataViewer';
begin
  try
    with ConfigurationFilesDialog do
    begin
      DataPathPrefixEdit.SetFieldValue(TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastDataFilePaths.DataFilePrefix);

      if (FAppModules.StudyArea.ModelVersion <> '7') then
        InputPathEdit.SetFieldValue(TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastFileNamesObject.InputFilesPath);
      OutputPathEdit.SetFieldValue(TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastFileNamesObject.OutputFilesPath);
      HydrologyPathEdit.SetFieldValue(TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastFileNamesObject.HydrologyFilesPath);
      SpecifiedDemandPathEdit.SetFieldValue(TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastFileNamesObject.DemandFilesPath);
      ParamFileNameEdit.SetFieldValue(TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastFileNamesObject.ParamFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesValidator.SaveState: boolean;
const OPNAME = 'TConfigurationFilesValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesValidator.ConfigurationFilesDialog : TConfigurationFilesDialog;
const OPNAME = 'TConfigurationFilesValidator.ConfigurationFilesDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TConfigurationFilesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TConfigurationFilesValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesValidator.StudyHasChanged: boolean;
const OPNAME = 'TConfigurationFilesValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TConfigurationFilesValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TConfigurationFilesValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
//    with ConfigurationFilesDialog do
//    begin
//      if ((Sender = ParamFileNameEdit)  AND (ParamFileNameEdit.HasvalueChanged)) then
//        UpdateParamFileName
//    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TConfigurationFilesValidator.UpdateParamFileName;
const OPNAME = 'TConfigurationFilesValidator.UpdateParamFileName';
var
  lMessage : string;
begin
  try
    with ConfigurationFilesDialog do
    begin
      if (FAppModules.FieldProperties.ValidateFieldProperty('ParamFile',
         ParamFileNameEdit.Text, lMessage )) then
      begin
        ParamFileNameEdit.FieldValidationError := lMessage;
        TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.ParamFileName := ParamFileNameEdit.Text;
        ParamFileNameEdit.SetFieldValue(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.ParamFileName);
        DoContextValidation(dvtStartYearO);
      end
      else
        ParamFileNameEdit.FieldValidationError := lMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end; }

procedure TConfigurationFilesValidator.UpdateParamFile(AFileName: string);
const OPNAME = 'TConfigurationFilesValidator.UpdateParamFile';
begin
  try
   TYieldModelDataObject(FAppModules.Model.ModelData).
     CastFileNamesObject.ParamFileName := AFileName;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TConfigurationFilesValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TConfigurationFilesValidator.DetermineWizardStatus';
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

procedure TConfigurationFilesValidator.OnHydrologyPathClick(Sender: TObject);
const OPNAME = 'TConfigurationFilesValidator.OnHydrologyPathClick';
var
  LPath: string;
  LOld_Path : string;
begin
  try
    with ConfigurationFilesDialog do
    begin
      LOld_Path := Trim(HydrologyPathEdit.Text);
      if SelectDirectory(LPath,[],0 ) then
      begin
        if LOld_Path <> LPath then
        begin
          HydrologyPathEdit.Text := ExcludeTrailingPathDelimiter(LPath);
          UpdateHydrologyPath(trim(HydrologyPathEdit.Text));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.OnInputPathClick(Sender: TObject);
const OPNAME = 'TConfigurationFilesValidator.OnInputPathClick';
var
  LPath: string;
  LOld_Path : string;
begin
  try
    with ConfigurationFilesDialog do
    begin
      LOld_Path := Trim(InputPathEdit.Text);
      if SelectDirectory(LPath,[],0 ) then
      begin
        if LOld_Path <> LPath then
        begin
          InputPathEdit.Text := ExcludeTrailingPathDelimiter(LPath);
          UpdateInputPath(trim(InputPathEdit.Text));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.OnOutputPathClick(Sender: TObject);
const OPNAME = 'TConfigurationFilesValidator.OnOutputPathClick';
var
  LPath: string;
  LOld_Path : string;
begin
  try
    with ConfigurationFilesDialog do
    begin
      LOld_Path := Trim(OutputPathEdit.Text);
      if SelectDirectory(LPath,[],0 ) then
      begin
        if LOld_Path <> LPath then
        begin
          OutputPathEdit.Text := ExcludeTrailingPathDelimiter(LPath);
          UpdateOutputPath(trim(OutputPathEdit.Text));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.OnSpecifiedDemandPathClick(Sender: TObject);
const OPNAME = 'TConfigurationFilesValidator.OnSpecifiedDemandPathClick';
var
  LPath: string;
  LOld_Path : string;
begin
  try
    with ConfigurationFilesDialog do
    begin
      LOld_Path := Trim(SpecifiedDemandPathEdit.Text);
      if SelectDirectory(LPath,[],0 ) then
      begin
        if LOld_Path <> LPath then
        begin
          SpecifiedDemandPathEdit.Text := ExcludeTrailingPathDelimiter(LPath);
          UpdateSpecifiedDemandPath(trim(SpecifiedDemandPathEdit.Text));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.OnSelectParamFileClick(Sender: TObject);
const OPNAME = 'TConfigurationFilesValidator.OnSelectParamFileClick';
var
  LFileName: string;
begin
  try
    if PromptForFileName(LFileName,'*.dat','','Select pramemter file','',False) then
       UpdateParamFile(LFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.UpdateHydrologyPath(AHydrologyFilePath: string);
const OPNAME = 'TConfigurationFilesValidator.UpdateHydrologyPath';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).
     CastFileNamesObject.HydrologyFilesPath := AHydrologyFilePath;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.UpdateInputPath(AInputFilePath: string);
const OPNAME = 'TConfigurationFilesValidator.UpdateInputPath';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).
     CastFileNamesObject.InputFilesPath := AInputFilePath;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.UpdateOutputPath(AOutputFilePath: string);
const OPNAME = 'TConfigurationFilesValidator.UpdateOutputPath';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).
     CastFileNamesObject.OutputFilesPath := AOutputFilePath;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TConfigurationFilesValidator.UpdateSpecifiedDemandPath(ADemandFilePath: string);
const OPNAME = 'TConfigurationFilesValidator.UpdateSpecifiedDemandPath';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).
     CastFileNamesObject.DemandFilesPath := ADemandFilePath;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

