{******************************************************************************}
{*  UNIT      : Contains the class TMinimumFlowChannelValidator.              *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/11                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMinimumFlowChannelValidator;

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
  UMinimumFlowChannelDialog;

type
  TMinimumFlowChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    //procedure OnFirmYieldAnalysisClick(Sender: TObject);
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure DisableModelControls;
    procedure RePopulateMinimumFlowDemands;
    //procedure RePopulateFirmYieldAnalysis;
    procedure UpdateFeatureName;
    //procedure UpdateChannelFirmYieldAnalysis;
    procedure UpdateMinimumFlowDemand(AMonth : integer;
                                      AValue : string);
    procedure ValidateMinFlowFeatureName (AFeature : IMinimumFlowConstraint);
    procedure ValidateMinFlowDemand (AFeature : IMinimumFlowConstraint);
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
    function MinimumFlowChannelDialog : TMinimumFlowChannelDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations, URunConfigurationData;


{******************************************************************************}
{* TMinimumFlowChannelValidator                                               *}
{******************************************************************************}

procedure TMinimumFlowChannelValidator.CreateMemberObjects;
const OPNAME = 'TMinimumFlowChannelValidator.CreateMemberObjects';
var
  lpPanel : TMinimumFlowChannelDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel  := TMinimumFlowChannelDialog.Create(FPanelOwner,FAppModules);
    lpPanel := MinimumFlowChannelDialog;
    with lpPanel do
    begin
      FeatureNameEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('MinFlowChannelName');
      FeatureNameEdit.OnEnter        := OnEditControlEnter;
      FeatureNameEdit.OnExit         := OnEditControltExit;

      {FirmYieldAnalysisChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('FirmYieldCalc');
      FirmYieldAnalysisChkBox.OnEnter       := OnEditControlEnter;
      FirmYieldAnalysisChkBox.OnClick       := OnFirmYieldAnalysisClick;
      }
      MinFlowDemandGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      MinFlowDemandGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MinFlowDemand'));
      MinFlowDemandGrid.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      MinFlowDemandGrid.OnColEnter          := OnStringGridColEnter;
      MinFlowDemandGrid.OnEnter             := OnEditControlEnter;
      MinFlowDemandGrid.ShowGridPopupMenu   := True;
      MinFlowDemandGrid.AllowPasteFromExcel := True;
      MinFlowDemandGrid.OnPasteFromExcel    := Self.OnAfterPasteColumnData;
      MinFlowDemandGrid.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.DestroyMemberObjects;
const OPNAME = 'TMinimumFlowChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.Initialise: boolean;
const OPNAME = 'TMinimumFlowChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMinimumFlowChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.MinimumFlowFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.ClearDataViewer;
const OPNAME = 'TMinimumFlowChannelValidator.ClearDataViewer';
var
  lPanel : TMinimumFlowChannelDialog;
  lMonth : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := MinimumFlowChannelDialog;
    with lPanel do
    begin
      FeatureNameEdit.Text := '';
      //FirmYieldAnalysisChkBox.Checked := FALSE;
      for lMonth := 0 to 11 do
        MinFlowDemandGrid.Cells[0, lMonth] := '-1';
      TotalsGrid.Cells[0,0] := '';
      TotalsGrid.Cells[1,0] := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.PopulateDataViewer;
const OPNAME = 'TMinimumFlowChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtMinFlowConstraints);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.RePopulateDataViewer;
const OPNAME = 'TMinimumFlowChannelValidator.RePopulateDataViewer';
var
  lConstraint    : IMinimumFlowConstraint;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        with MinimumFlowChannelDialog do
        begin
          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues     := lConstraint.GetKeyValues(lFieldProperty.FieldName, '');
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          FeatureNameEdit.SetFieldValue(lConstraint.FeatureName);
        end;
        //RePopulateFirmYieldAnalysis;
        RePopulateMinimumFlowDemands;
        DisableModelControls;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.DisableModelControls;
const OPNAME = 'TMinimumFlowChannelValidator.DisableModelControls';
begin
  try
    with MinimumFlowChannelDialog do
    begin
      if (FAppModules.Model.ModelName = CPlanning) then
      begin
        //FirmYieldAnalysisChkBox.Enabled := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMinimumFlowChannelValidator.RePopulateFirmYieldAnalysis;
const OPNAME = 'TMinimumFlowChannelValidator.RePopulateFirmYieldAnalysis';
var
  lConstraint    : IMinimumFlowConstraint;
  lChannel       : IGeneralFlowChannel;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil ) then
      begin
        lChannel := lConstraint.Channel;
        if (lChannel <> nil ) then
        begin
          with MinimumFlowChannelDialog do
          begin
            lFieldProperty := FirmYieldAnalysisChkBox.FieldProperty;
            lKeyValues     := lChannel.GetKeyValues(lFieldProperty.FieldName, '');
            FirmYieldAnalysisChkBox.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
            FirmYieldAnalysisChkBox.Checked := (UpperCase(Trim(lChannel.RequiresFirmYieldAnalysis)) = 'Y');
            FirmYieldAnalysisChkBox.Enabled := (Uppercase(Trim(lChannel.SummaryOutputRequired)) = 'Y');
          end;
        end;    
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TMinimumFlowChannelValidator.RePopulateMinimumFlowDemands;
const OPNAME = 'TMinimumFlowChannelValidator.RePopulateMinimumFlowDemands';
var
  lConstraint    : IMinimumFlowConstraint;
  lRow           : integer;
  lMonths        : TMonthNamesArray;
  lValue         : double;
  lTotal         : double;
  lMonthDays     : TMonthDaysArray;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  lMonths := nil;
  lMonthDays := nil;
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      lMonthDays := TYieldModelDataObject(FAppModules.Model.ModelData).
                      CastRunConfigurationData.MonthsDaysArray;
      if (lConstraint <> nil) then
      begin
        with MinimumFlowChannelDialog do
        begin
          TotalsGrid.Cells[0,0] := FAppModules.Language.GetString('Channel.Totals');
          lTotal := 0;
          lFieldProperty := MinFlowDemandGrid.FieldProperty(1);
          for lRow := 0 to 11 do
          begin
            lFieldIndex := IntToStr(lRow+1);
            lKeyValues  := lConstraint.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            MinFlowDemandGrid.HasChanges[1, lRow] :=
              FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            MinFlowDemandGrid.HasMetaData[1, lRow] :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

            lValue := lConstraint.MinimumFlowDemandByMonth[lRow+1];
            lTotal := lTotal + (lValue * lMonthDays[lRow+1] * 86400.0);
            MinFlowDemandGrid.Cells[0, lRow] := lMonths[lRow+1];
            LFieldProperty    := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
            MinFlowDemandGrid.AddFieldProperty(LFieldProperty);
            MinFlowDemandGrid.Cells[1, lRow] := Format(LFieldProperty.FormatStringGrid, [lValue]);
          end;
          lTotal := lTotal / 1000000;
          TotalsGrid.Cells[1, 0] := Format(LFieldProperty.FormatStringGrid, [lTotal]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.SaveState: boolean;
const OPNAME = 'TMinimumFlowChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.MinimumFlowChannelDialog:TMinimumFlowChannelDialog;
const OPNAME = 'TMinimumFlowChannelValidator.MinimumFlowChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TMinimumFlowChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMinimumFlowChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'SummaryOutputRequired') then
    begin
      //RePopulateFirmYieldAnalysis;
      DisableModelControls;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TMinimumFlowChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMinimumFlowChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMinimumFlowChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with MinimumFlowChannelDialog do
    begin
      if ((Sender = FeatureNameEdit) AND
          (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMinimumFlowChannelValidator.OnFirmYieldAnalysisClick(Sender: TObject);
const OPNAME = 'TMinimumFlowChannelValidator.OnFirmYieldAnalysisClick';
begin
  try
    with MinimumFlowChannelDialog do
    begin
      if(FirmYieldAnalysisChkBox.HasValueChanged) then
        UpdateChannelFirmYieldAnalysis;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TMinimumFlowChannelValidator.UpdateFeatureName;
const OPNAME = 'TMinimumFlowChannelValidator.UpdateFeatureName';
var
  lFeature : IMinimumFlowConstraint;
  lMessage : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with MinimumFlowChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FeatureNameEdit.FieldProperty.FieldName,
            FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
          DoContextValidation(dvtMinFlowFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMinimumFlowChannelValidator.UpdateChannelFirmYieldAnalysis;
const OPNAME = 'TMinimumFlowChannelValidator.UpdateChannelFirmYieldAnalysis';
var
  lConstraint : IMinimumFlowConstraint;
  lChannel    : IGeneralFlowChannel;
  lOldValue   : string;
  lNewValue   : string;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      lChannel := lConstraint.Channel;
      with MinimumFlowChannelDialog do
      begin
        lOldValue := UpperCase(Trim(lChannel.RequiresFirmYieldAnalysis));
        if FirmYieldAnalysisChkBox.Checked then
          lNewValue := 'Y'
        else
          lNewValue := 'N';
        if (lOldValue <> lNewvalue) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              FirmYieldAnalysisChkBox.FieldProperty.FieldName,
              lNewValue,lMessage)) then
          begin
            lChannel.RequiresFirmYieldAnalysis := lNewValue;
            FirmYieldAnalysisChkBox.Checked := (lChannel.RequiresFirmYieldAnalysis = 'Y');
          end
          else
            FirmYieldAnalysisChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TMinimumFlowChannelValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMinimumFlowChannelValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with MinimumFlowChannelDialog do
    begin
      if ((MinFlowDemandGrid = ASender) AND (ACol = 1) AND
          (NOT MinFlowDemandGrid.HasChanges[ACol,ARow])) then
        UpdateMinimumFlowDemand(ARow+1, Trim(MinFlowDemandGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.UpdateMinimumFlowDemand(AMonth : integer;
                                                               AValue : string);
const OPNAME = 'TMinimumFlowChannelValidator.UpdateMinimumFlowDemand';
var
  lConstraint : IMinimumFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with MinimumFlowChannelDialog do
      begin
        MinFlowDemandGrid.ValidationError[1, AMonth-1, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            MinFlowDemandGrid.FieldProperty(1).FieldName,
            AValue,lMessage, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          lConstraint.MinimumFlowDemandByMonth[AMonth] := lValue;
          RePopulateMinimumFlowDemands;
          DoContextValidation(dvtMinFlowDemand);
        end
        else
          MinFlowDemandGrid.ValidationError[1, AMonth-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMinimumFlowChannelValidator.DoContextValidation';
var
  lFeature     : IMinimumFlowConstraint;
  lFeatureList : IMinimumFlowConstraintList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MinimumFlowConstraintList;
      lFeature := lFeatureList.MinimumFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtMinFlowConstraints, dvtMinFlowFeatureName]) then
           ValidateMinFlowFeatureName(lFeature);
        if (AValidationType in [dvtMinFlowConstraints, dvtMinFlowDemand]) then
           ValidateMinFlowDemand(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TMinimumFlowChannelValidator.DetermineWizardStatus';
var
  lFeature       : IMinimumFlowConstraint;
  lFeatureList   : IMinimumFlowConstraintList;
  lMinFlowDemand : TAbstractFieldProperty;
  lNotZero       : Boolean;
  lIndex         : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MinimumFlowConstraintList;
      lFeature := lFeatureList.MinimumFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtMinFlowConstraints);
        lMinFlowDemand := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
        lNotZero := FALSE;
        lIndex   := lMinFlowDemand.ArrayLow;
        while ((NOT lNotZero) AND (lIndex <= lMinFlowDemand.ArrayHigh)) do
        begin
          if (lFeature.MinimumFlowDemandByMonth[lIndex] > 0) then
            lNotZero := TRUE
          else
            lIndex := lIndex + 1;
        end;
        if (lNotZero) then
        begin
          Result := 1;
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.ValidateMinFlowFeatureName(AFeature : IMinimumFlowConstraint);
const OPNAME = 'TMinimumFlowChannelValidator.ValidateMinFlowFeatureName';
begin
  try
    with MinimumFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MinFlowChannelName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
 except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowChannelValidator.ValidateMinFlowDemand (AFeature: IMinimumFlowConstraint);
const OPNAME = 'TMinimumFlowChannelValidator.ValidateMinFlowDemand';
begin
  try
    if (AFeature <> nil) then
    begin
      with MinimumFlowChannelDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'MinFlowDemand')) then
          MinFlowDemandGrid.ValidationError[1, 0, gveColContext] := ''
        else
        begin
          MinFlowDemandGrid.ValidationError[1, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TMinimumFlowChannelValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IMinimumFlowConstraint;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with MinimumFlowChannelDialog do
        begin
          if (FActiveControl = MinFlowDemandGrid) then
          begin
            lFieldIndex    := IntToStr(MinFlowDemandGrid.Row+1);
            lFieldProperty := MinFlowDemandGrid.FieldProperty(1);
            if (lFieldProperty <> nil) then
            begin
              lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              FAppModules.Changes.ShowParameterChanges
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              Result := TRUE;
              RePopulateDataViewer;
              FAppModules.Changes.SetParameterChanges(TRUE);
              Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowChannelValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TMinimumFlowChannelValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IMinimumFlowConstraint;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with MinimumFlowChannelDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          {if (FActiveControl = FirmYieldAnalysisChkBox) then
            lFieldProperty := FirmYieldAnalysisChkBox.FieldProperty
          else}
          if (FActiveControl = MinFlowDemandGrid) then
          begin
            lFieldIndex    := IntToStr(MinFlowDemandGrid.Row+1);
            lFieldProperty := MinFlowDemandGrid.FieldProperty(1);
          end;

          if (lFieldProperty <> nil) then
          begin
            {if (FActiveControl = FirmYieldAnalysisChkBox) AND (lFeature.Channel <> nil) then
              lKeyValues := lFeature.Channel.GetKeyValues(lFieldProperty.FieldName, lFieldIndex)
            else}
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

procedure TMinimumFlowChannelValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TMinimumFlowChannelValidator.OnAfterPasteColumnData';
var
  LConstraint : IMinimumFlowConstraint;
  LValue      : double;
  LRow        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinimumFlowConstraintList.MinimumFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil) then
    begin
      for LRow := MinimumFlowChannelDialog.MinFlowDemandGrid.FixedRows to MinimumFlowChannelDialog.MinFlowDemandGrid.RowCount - 1 do
      begin
        LValue := StrToFloat(Trim(MinimumFlowChannelDialog.MinFlowDemandGrid.Cells[1,LRow]));
        LConstraint.MinimumFlowDemandByMonth[LRow + 1] := lValue;
      end;
      RePopulateMinimumFlowDemands;
      DoContextValidation(dvtMinFlowDemand);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

