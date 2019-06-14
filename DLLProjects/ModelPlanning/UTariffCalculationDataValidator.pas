{******************************************************************************}
{*  UNIT      : Contains the class TDisbenefitFunctionDefinitionValidator.    *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/06/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UTariffCalculationDataValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UTariffCalculationDataDialog;

type

  TTariffCalculationDataValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure RePopulateDataViewer;
    //procedure UpdateChannelNumber;
    procedure UpdateTariff;
    procedure UpdateEscalationFactors;
    //procedure ValidateChannelNumber(AChannelTariff      : IChannelTariff);
    procedure ValidateTariff(AChannelTariff      : IChannelTariff);
    procedure ValidateEscalationFactors(AChannelTariff      : IChannelTariff);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;

    function TariffCalculationDataDialog: TTariffCalculationDataDialog;
  end;

implementation

uses
  VCL.Dialogs,
  SysUtils,
  VCL.Graphics,
  Contnrs,
  UFileNames,
  UUtilities,
  UConstants,
  UAbstractFileNamesObject,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations,
  UYieldModelDataObject,
  UParameterData;

{******************************************************************************}
{* TTariffCalculationDataValidator                                 *}
{******************************************************************************}

function TTariffCalculationDataValidator.TariffCalculationDataDialog : TTariffCalculationDataDialog;
const OPNAME = 'TTariffCalculationDataValidator.TariffCalculationDataDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TTariffCalculationDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.CreateMemberObjects;
const OPNAME = 'TTariffCalculationDataValidator.CreateMemberObjects';
var
  lpPanel : TTariffCalculationDataDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    lpPanel     := TTariffCalculationDataDialog.Create(FPanelOwner,FAppModules);
    FPanel      := lpPanel;
    with lpPanel do
    begin
      {ChannelNumberEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('ChannelNumber');
      ChannelNumberEdit.OnEnter            := OnEditControlEnter;
      ChannelNumberEdit.OnExit             := OnEditControltExit;}

      TariffEdit.FieldProperty             := FAppModules.FieldProperties.FieldProperty('Tariff');
      TariffEdit.OnEnter                   := OnEditControlEnter;
      TariffEdit.OnExit                    := OnEditControltExit;

      EscalationFactorsEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('EscalationFactors');
      EscalationFactorsEdit.OnEnter        := OnEditControlEnter;
      EscalationFactorsEdit.OnExit         := OnEditControltExit;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.DestroyMemberObjects;
const OPNAME = 'TTariffCalculationDataValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationDataValidator.Initialise: boolean;
const OPNAME = 'TTariffCalculationDataValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with TariffCalculationDataDialog do
    begin
      //ChannelNumberEdit.SetFieldValue('');
      TariffEdit.SetFieldValue('');
      EscalationFactorsEdit.SetFieldValue('');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationDataValidator.LanguageHasChanged: boolean;
const OPNAME = 'TTariffCalculationDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TTariffCalculationDataDialog.DialogCaption');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.ClearDataViewer;
const OPNAME = 'TTariffCalculationDataValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.PopulateDataViewer;
const OPNAME = 'TTariffCalculationDataValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtTariffCalculationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.RePopulateDataViewer;
const OPNAME = 'TTariffCalculationDataValidator.RePopulateDataViewer';
var
  //lChannelNumber      : integer;
  lChannel            : IGeneralFlowChannel;
  lChannelTariff      : IChannelTariff;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) AND (lChannel.TariffCalculation <> nil) then
      begin
        lChannelTariff := lChannel.TariffCalculation;
        with TariffCalculationDataDialog do
        begin
          //lChannelNumber := lChannelTariff.ChannelNumber;
          //ChannelNumberEdit.SetFieldValue(lChannelNumber);
          TariffEdit.SetFieldValue(FloatToStr(lChannelTariff.Tariff));
          EscalationFactorsEdit.SetFieldValue(lChannelTariff.EscalationFactors);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationDataValidator.SaveState: boolean;
const OPNAME = 'TTariffCalculationDataValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationDataValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TTariffCalculationDataValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TTariffCalculationDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TTariffCalculationDataValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TTariffCalculationDataValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with TariffCalculationDataDialog do
    begin
      {if ((Sender = ChannelNumberEdit) AND (ChannelNumberEdit.HasValueChanged)) then
        UpdateChannelNumber
      else}
      if ((Sender = TariffEdit) AND (TariffEdit.HasValueChanged)) then
        UpdateTariff
      else if ((Sender = EscalationFactorsEdit) AND (EscalationFactorsEdit.HasValueChanged)) then
        UpdateEscalationFactors;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TTariffCalculationDataValidator.UpdateChannelNumber;
const OPNAME = 'TTariffCalculationDataValidator.UpdateChannelNumber';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                   .ChannelList;
    with TariffCalculationDataDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
           'NrOfEconomicVariableYears', NrOfEconomicVariablesEdit.Text, lMessage)) then
        begin
          NrOfEconomicVariablesEdit.FieldValidationError := lMessage;
          lDisbenefitFunction.ChannelNumber := StrToInt(NrOfEconomicVariablesEdit.Text);
          NrOfEconomicVariablesEdit.SetFieldValue(lDisbenefitFunction.ChannelNumber);
          DoContextValidation(dvtNrOfEconomonicYears);
          RePopulateDataViewer;
        end
        else
          NrOfEconomicVariablesEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TTariffCalculationDataValidator.UpdateTariff;
const OPNAME = 'TTariffCalculationDataValidator.UpdateTariff';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lChannelTariff      : IChannelTariff;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with TariffCalculationDataDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) AND (lChannel.TariffCalculation <> nil) then
      begin
        lChannelTariff := lChannel.TariffCalculation;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'Tariff', TariffEdit.Text, lMessage)) then
        begin
          TariffEdit.FieldValidationError := lMessage;
          lChannelTariff.Tariff := StrToFloat(TariffEdit.Text);
          DoContextValidation(dvtTariffCalculationTariff);
          RePopulateDataViewer;
        end
        else
          TariffEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.UpdateEscalationFactors;
const OPNAME = 'TTariffCalculationDataValidator.UpdateEscalationFactors';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lChannelTariff      : IChannelTariff;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with TariffCalculationDataDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) AND (lChannel.TariffCalculation <> nil) then
      begin
        lChannelTariff := lChannel.TariffCalculation;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
           'EscalationFactors', EscalationFactorsEdit.Text, lMessage)) then
        begin
          EscalationFactorsEdit.FieldValidationError := lMessage;
          lChannelTariff.EscalationFactors := Trim(EscalationFactorsEdit.Text);
          DoContextValidation(dvtTariffCalculationEscalation);
          RePopulateDataViewer;
        end
        else
          EscalationFactorsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TTariffCalculationDataValidator.DoContextValidation';
var
  lChannelList          : IChannelList;
  lChannel              : IGeneralFlowChannel;
  lChannelTariff        : IChannelTariff;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier > 0) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
          NetworkElementData.ChannelList;
      lChannel   := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) AND (lChannel.TariffCalculation <> nil) then
      begin
        lChannelTariff := lChannel.TariffCalculation;
        //if ((AValidationType = dvtTariffCalculationAll) OR (AValidationType = dvtTariffCalculationChannelNumber)) then
        //  ValidateChannelNumber(lDisbenefitDefinition);
        if ((AValidationType = dvtTariffCalculationAll) OR (AValidationType = dvtTariffCalculationTariff)) then
          ValidateTariff(lChannelTariff);
        if ((AValidationType = dvtTariffCalculationAll) OR (AValidationType = dvtTariffCalculationEscalation)) then
          ValidateEscalationFactors(lChannelTariff);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TTariffCalculationDataValidator.ValidateChannelNumber(ADisbenefit: IDisbenefitFunctionDefinition);
const OPNAME = 'TTariffCalculationDataValidator.ValidateChannelNumber';
begin
  try
    with TariffCalculationDataDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'ChannelNumber')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfEconomicVariablesEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}


procedure TTariffCalculationDataValidator.ValidateTariff(AChannelTariff      : IChannelTariff);
const OPNAME = 'TTariffCalculationDataValidator.ValidateTariff';
begin
  try
    with TariffCalculationDataDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannelTariff.Validate(FErrorMessage, 'Tariff')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
        TariffEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataValidator.ValidateEscalationFactors(AChannelTariff      : IChannelTariff);
const OPNAME = 'TTariffCalculationDataValidator.ValidateEscalationFactors';
begin
  try
    with TariffCalculationDataDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannelTariff.Validate(FErrorMessage, 'EscalationFactors')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       EscalationFactorsEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

