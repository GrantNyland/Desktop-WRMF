//
//
//  UNIT      : Contains the class TYieldMetaDataValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/04/04
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UYieldMetaDataValidator;

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
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UYieldMetaDataDialog;

type
  TYieldMetaDataValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    function GetKeyValues: string;
    procedure OnBtnClick ( Sender : TObject );
    procedure OnStudyMetaDataBtnClick(Sender : TObject);
  public
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure PopulateDataViewer; override;
    function ProcessMetaDataEvent : boolean; override;
    function YieldMetaDataDialog: TYieldMetaDataDialog;
  end;

implementation

uses
  VCL.Forms,
  UStudyMetaDataValidator,
  SysUtils,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UYieldModelDataGUIForm;

{ TYieldMetaDataValidator }

procedure TYieldMetaDataValidator.CreateMemberObjects;
const OPNAME = 'TYieldMetaDataValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TYieldMetaDataDialog.Create ( FPanelOwner, FAppModules );

    YieldMetaDataDialog.HydrologyFilesBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'HydrologyFiles' );
    YieldMetaDataDialog.HydrologyFilesBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.HydrologyFilesBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.StudyResultsBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'StudyResults' );
    YieldMetaDataDialog.StudyResultsBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.StudyResultsBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.StudyReportsBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'StudyReports' );
    YieldMetaDataDialog.StudyReportsBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.StudyReportsBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.SenarioStrategyBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'SenarioStrategy' );
    YieldMetaDataDialog.SenarioStrategyBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.SenarioStrategyBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.StudyStakeholdersBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'StudyStakeholders' );
    YieldMetaDataDialog.StudyStakeholdersBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.StudyStakeholdersBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.OverallOperatingRulestrategyBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'OverallOperatingRulestrategy' );
    YieldMetaDataDialog.OverallOperatingRulestrategyBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.OverallOperatingRulestrategyBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.StudyBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'Study' );
    YieldMetaDataDialog.StudyBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.StudyBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.ProposedInfrastructureBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'ProposedInfrastructure' );
    YieldMetaDataDialog.ProposedInfrastructureBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.ProposedInfrastructureBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.DemandProjectionBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'DemandProjection' );
    YieldMetaDataDialog.DemandProjectionBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.DemandProjectionBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.DevOptionSequencesBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'DevOptionSequences' );
    YieldMetaDataDialog.DevOptionSequencesBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.DevOptionSequencesBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.OperatingRuleStrategyBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'OperatingRuleStrategy' );
    YieldMetaDataDialog.OperatingRuleStrategyBtn.OnClick := OnBtnClick;
    YieldMetaDataDialog.OperatingRuleStrategyBtn.OnEnter := OnEditControlEnter;

    YieldMetaDataDialog.StudyErrorMetaDataBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty ( 'StudyErrorMetaData' );
    YieldMetaDataDialog.StudyErrorMetaDataBtn.OnClick := OnStudyMetaDataBtnClick;
    YieldMetaDataDialog.StudyErrorMetaDataBtn.OnEnter := OnEditControlEnter;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldMetaDataValidator.YieldMetaDataDialog : TYieldMetaDataDialog;
const OPNAME = 'TYieldMetaDataValidator.YieldMetaDataDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TYieldMetaDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldMetaDataValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYieldMetaDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.MetaData');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldMetaDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TYieldMetaDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
   Initialise;
   PopulateDataViewer;
   LanguageHasChanged;
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldMetaDataValidator.GetKeyValues : string;
const OPNAME = 'TYieldMetaDataValidator.GetKeyValues';
begin
  try
    Result := 'Model='           + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ',StudyAreaName='  + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ',SubArea='        + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ',Scenario='       + QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYieldMetaDataValidator.OnBtnClick ( Sender : TObject );
const OPNAME = 'TYieldMetaDataValidator.OnBtnClick';
var
  lFieldProperty : TAbstractFieldProperty;
  lFieldIndex    : string;
  lKeyValues     : string;
begin
  try
    lFieldIndex    := '';
    lKeyValues     := GetKeyValues;
    lFieldProperty := nil;
    if ( Sender = YieldMetaDataDialog.HydrologyFilesBtn ) then
      lFieldProperty := YieldMetaDataDialog.HydrologyFilesBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.StudyResultsBtn ) then
      lFieldProperty := YieldMetaDataDialog.StudyResultsBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.StudyReportsBtn ) then
      lFieldProperty := YieldMetaDataDialog.StudyReportsBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.SenarioStrategyBtn ) then
      lFieldProperty := YieldMetaDataDialog.SenarioStrategyBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.StudyStakeholdersBtn ) then
      lFieldProperty := YieldMetaDataDialog.StudyStakeholdersBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.OverallOperatingRulestrategyBtn ) then
      lFieldProperty := YieldMetaDataDialog.OverallOperatingRulestrategyBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.StudyBtn ) then
      lFieldProperty := YieldMetaDataDialog.StudyBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.ProposedInfrastructureBtn ) then
      lFieldProperty := YieldMetaDataDialog.ProposedInfrastructureBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.DemandProjectionBtn ) then
      lFieldProperty := YieldMetaDataDialog.DemandProjectionBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.DevOptionSequencesBtn ) then
      lFieldProperty := YieldMetaDataDialog.DevOptionSequencesBtn.FieldProperty
    else
    if ( Sender = YieldMetaDataDialog.OperatingRuleStrategyBtn ) then
      lFieldProperty := YieldMetaDataDialog.OperatingRuleStrategyBtn.FieldProperty
    else
    if(Sender = YieldMetaDataDialog.StudyErrorMetaDataBtn) then
    begin
    end;

    if (lFieldProperty <> nil) then
    begin
      FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
      PopulateDataViewer;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYieldMetaDataValidator.OnStudyMetaDataBtnClick(Sender: TObject);
const OPNAME = 'TYieldMetaDataValidator.OnStudyMetaDataBtnClick';
var
  LStudyMetaDataForm : TYieldModelDataGUIForm;
  LDialogValidator   : TStudyMetaDataValidator;
begin
  try
    LStudyMetaDataForm := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LStudyMetaDataForm.Initialise;
      LStudyMetaDataForm.LanguageHasChanged;

      LDialogValidator := TStudyMetaDataValidator.Create(LStudyMetaDataForm,FAppModules);
      try
        LStudyMetaDataForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LDialogValidator.ViewMode := vmEditableSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        LStudyMetaDataForm.ShowModal;
      finally
        LDialogValidator.Free;
      end;
    finally
      LStudyMetaDataForm.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYieldMetaDataValidator.PopulateDataViewer;
const OPNAME = 'TYieldMetaDataValidator.PopulateDataViewer';
var
  lFieldProperty : TAbstractFieldProperty;
  lFieldIndex    : string;
  lKeyValues     : string;
begin
  try
    lFieldIndex    := '';
    lKeyValues     := GetKeyValues;
    lFieldProperty := YieldMetaDataDialog.HydrologyFilesBtn.FieldProperty;
    YieldMetaDataDialog.HydrologyFilesBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.StudyResultsBtn.FieldProperty;
    YieldMetaDataDialog.StudyResultsBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.StudyReportsBtn.FieldProperty;
    YieldMetaDataDialog.StudyReportsBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.SenarioStrategyBtn.FieldProperty;
    YieldMetaDataDialog.SenarioStrategyBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.StudyStakeholdersBtn.FieldProperty;
    YieldMetaDataDialog.StudyStakeholdersBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.OverallOperatingRulestrategyBtn.FieldProperty;
    YieldMetaDataDialog.OverallOperatingRulestrategyBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.StudyBtn.FieldProperty;
    YieldMetaDataDialog.StudyBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.ProposedInfrastructureBtn.FieldProperty;
    YieldMetaDataDialog.ProposedInfrastructureBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.DemandProjectionBtn.FieldProperty;
    YieldMetaDataDialog.DemandProjectionBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.DevOptionSequencesBtn.FieldProperty;
    YieldMetaDataDialog.DevOptionSequencesBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;

    lFieldProperty := YieldMetaDataDialog.OperatingRuleStrategyBtn.FieldProperty;
    YieldMetaDataDialog.OperatingRuleStrategyBtn.HasMetaData := FAppModules.MetaData.FindMetaData ( lFieldProperty.FieldName, lKeyValues, '' ) <> nil;


  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldMetaDataValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TYieldMetaDataValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lFieldIndex    : string;
  lKeyValues     : string;
begin
  Result := inherited ProcessMetaDataEvent;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      lFieldIndex    := '';
      lKeyValues     := GetKeyValues;
      lFieldProperty := nil;
      if ( FActiveControl = YieldMetaDataDialog.HydrologyFilesBtn ) then
        lFieldProperty := YieldMetaDataDialog.HydrologyFilesBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.StudyResultsBtn ) then
        lFieldProperty := YieldMetaDataDialog.StudyResultsBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.StudyReportsBtn ) then
        lFieldProperty := YieldMetaDataDialog.StudyReportsBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.SenarioStrategyBtn ) then
        lFieldProperty := YieldMetaDataDialog.SenarioStrategyBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.StudyStakeholdersBtn ) then
        lFieldProperty := YieldMetaDataDialog.StudyStakeholdersBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.OverallOperatingRulestrategyBtn ) then
        lFieldProperty := YieldMetaDataDialog.OverallOperatingRulestrategyBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.StudyBtn ) then
        lFieldProperty := YieldMetaDataDialog.StudyBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.ProposedInfrastructureBtn ) then
        lFieldProperty := YieldMetaDataDialog.ProposedInfrastructureBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.DemandProjectionBtn ) then
        lFieldProperty := YieldMetaDataDialog.DemandProjectionBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.DevOptionSequencesBtn ) then
        lFieldProperty := YieldMetaDataDialog.DevOptionSequencesBtn.FieldProperty
      else
      if ( FActiveControl = YieldMetaDataDialog.OperatingRuleStrategyBtn ) then
        lFieldProperty := YieldMetaDataDialog.OperatingRuleStrategyBtn.FieldProperty;

      if (lFieldProperty <> nil) then
      begin
        FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        PopulateDataViewer;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

