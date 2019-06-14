{******************************************************************************}
{*  UNIT      : Contains the class TYieldRunTitleValidator.                   *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2004/11/18                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UYieldRunTitleValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UYieldRunTitleDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type

  TYieldRunTitleValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure UpdateRunTitle1;
    procedure UpdateRunTitle2;
    procedure UpdateRunTitle3;
    procedure ValidateRunTitle1(AConfiguration : IRunConfigurationData);
    procedure ValidateRunTitle2(AConfiguration : IRunConfigurationData);
    procedure ValidateRunTitle3(AConfiguration : IRunConfigurationData);

    procedure RePopulateDataViewer;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function YieldRunTitleDialog: TYieldRunTitleDialog;
    procedure DoContextValidation(AValidationType: TDialogValidationType);override;

  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UConstants,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog,
  UNetworkElementData,
  Math;

{******************************************************************************}
{* TYieldRunTitleValidator                                                    *}
{******************************************************************************}

procedure TYieldRunTitleValidator.CreateMemberObjects;
const OPNAME = 'TYieldRunTitleValidator.CreateMemberObjects';
var
  lPanel : TYieldRunTitleDialog;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TYieldRunTitleDialog.Create(FPanelOwner,FAppModules);
    lPanel := YieldRunTitleDialog;
    with lPanel do
    begin
      RunTitle1Edit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('Title1');
      RunTitle1Edit.OnEnter          := OnEditControlEnter;
      RunTitle1Edit.OnExit           := OnEditControltExit;

      RunTitle2Edit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('Title2');
      RunTitle2Edit.OnEnter          := OnEditControlEnter;
      RunTitle2Edit.OnExit           := OnEditControltExit;

      RunTitle3Edit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('Title3');
      RunTitle3Edit.OnEnter          := OnEditControlEnter;
      RunTitle3Edit.OnExit           := OnEditControltExit;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunTitleValidator.DestroyMemberObjects;
const OPNAME = 'TYieldRunTitleValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunTitleValidator.Initialise: boolean;
const OPNAME = 'TYieldRunTitleValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunTitleValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYieldRunTitleValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.YieldRunTitle');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunTitleValidator.ClearDataViewer;
const OPNAME = 'TYieldRunTitleValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with YieldRunTitleDialog do
    begin
      RunTitle1Edit.SetFieldValue('');
      RunTitle2Edit.SetFieldValue('');
      RunTitle3Edit.SetFieldValue('');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunTitleValidator.PopulateDataViewer;
const OPNAME = 'TYieldRunTitleValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtConfigurationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunTitleValidator.RePopulateDataViewer;
const OPNAME = 'TYieldRunTitleValidator.RePopulateDataViewer';
var
  lConfigData: IRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunTitleDialog do
      begin
        RunTitle1Edit.SetFieldValue(lConfigData.YieldRunTitle1);
        RunTitle2Edit.SetFieldValue(lConfigData.YieldRunTitle2);
        RunTitle3Edit.SetFieldValue(lConfigData.YieldRunTitle3);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunTitleValidator.YieldRunTitleDialog: TYieldRunTitleDialog;
const OPNAME = 'TYieldRunTitleValidator.YieldRunTitleDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TYieldRunTitleDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunTitleValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TYieldRunTitleValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  Except on E : Exception do HandleError(E, OPNAME);end;
end;

procedure TYieldRunTitleValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYieldRunTitleValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with YieldRunTitleDialog do
    begin
      if (Sender = RunTitle1Edit) AND (RunTitle1Edit.HasValueChanged) then
        UpdateRunTitle1;
      if (sender = RunTitle2Edit) AND (RunTitle2Edit.HasValueChanged) then
        UpdateRunTitle2;
      if (sender = RunTitle3Edit) AND (RunTitle3Edit.HasValueChanged) then
        UpdateRunTitle3;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TYieldRunTitleValidator.SaveState: boolean;
const OPNAME = 'TYieldRunTitleValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TYieldRunTitleValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                     ANewValue: string): boolean;
const OPNAME = 'TYieldRunTitleValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunTitleValidator.UpdateRunTitle1;
const OPNAME = 'TYieldRunTitleValidator.UpdateRunTitle1';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with YieldRunTitleDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('Title1',
                       RunTitle1Edit.Text, LMessage)) then
        begin
          RunTitle1Edit.ContextValidationError := LMessage;
          LConfigData.YieldRunTitle1 := Trim(RunTitle1Edit.Text);
          RunTitle1Edit.SetFieldValue(LConfigData.YieldRunTitle1);
          DoContextValidation(dvtConfigurationAll);
        end
        else
          RunTitle1Edit.ContextValidationError := LMessage;
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunTitleValidator.UpdateRunTitle2;
const OPNAME = 'TYieldRunTitleValidator.UpdateRunTitle2';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with YieldRunTitleDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty('Title2',
                                      RunTitle2Edit.Text, LMessage) then
        begin
          RunTitle2Edit.ContextValidationError := LMessage;
          LConfigData.YieldRunTitle2 := Trim(RunTitle2Edit.Text);
          RunTitle2Edit.SetFieldValue(LConfigData.YieldRunTitle2);
        end
        else
          RunTitle2Edit.ContextValidationError := LMessage;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunTitleValidator.UpdateRunTitle3;
const OPNAME = 'TYieldRunTitleValidator.UpdateRunTitle3';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with YieldRunTitleDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty('Title3',
                                      RunTitle3Edit.Text, LMessage) then
        begin
          RunTitle3Edit.ContextValidationError := LMessage;
          LConfigData.YieldRunTitle3 := Trim(RunTitle3Edit.Text);
          RunTitle3Edit.SetFieldValue(LConfigData.YieldRunTitle3);
        end
        else
          RunTitle3Edit.ContextValidationError := LMessage;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunTitleValidator.ValidateRunTitle1(AConfiguration: IRunConfigurationData);
const OPNAME = 'TYieldRunTitleValidator.ValidateRunTitle1';
begin
  try
  with YieldRunTitleDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'RunTitle1');
      RunTitle1Edit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunTitleValidator.ValidateRunTitle2(AConfiguration: IRunConfigurationData);
const OPNAME = 'TYieldRunTitleValidator.ValidateRunTitle2';
begin
  try
  with YieldRunTitleDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'RunTitle2');
      RunTitle2Edit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunTitleValidator.ValidateRunTitle3(AConfiguration: IRunConfigurationData);
const OPNAME = 'TYieldRunTitleValidator.ValidateRunTitle3';
begin
  try
  with YieldRunTitleDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'RunTitle3');
      RunTitle3Edit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TYieldRunTitleValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TYieldRunTitleValidator.DoContextValidation';
var
  LConfigData: IRunConfigurationData;
begin
  try
    LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (LConfigData <> nil) then
    begin
      if (AValidationType in [dvtConfigurationAll, dvtRunTitle1 ]) then
        ValidateRunTitle1(LConfigData);
      if (AValidationType in [dvtConfigurationAll, dvtRunTitle2 ]) then
        ValidateRunTitle2(LConfigData);
      if (AValidationType in [dvtConfigurationAll, dvtRunTitle3 ]) then
        ValidateRunTitle3(LConfigData);
    end;
  except on E: exception do HandleError(E, OPNAME); end;
end;

end.

