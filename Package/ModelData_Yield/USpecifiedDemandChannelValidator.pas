{******************************************************************************}
{*  UNIT      : Contains the class TSpecifiedDemandChannelValidator.          *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/12                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USpecifiedDemandChannelValidator;

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
  USpecifiedDemandChannelDialog;

type

  TSpecifiedDemandChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnSelectDemandFileNameClick(Sender: TObject);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnIndicatorRadioGroupClick(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure UpdateFeatureName;
    procedure UpdateCatchmentReference;
    procedure UpdateDemandFileName;
    procedure UpdateStochasticIndicator;
    procedure OnViewGridClick(Sender: TObject);
    procedure OnViewGraphClick(Sender: TObject);
    procedure ValidateFeatureName (AFeature : ISpecifiedDemandFeature);
    procedure ValidateStochasticIndicator (AFeature : ISpecifiedDemandFeature);
    procedure ValidateCatchmentRefNumber (AFeature : ISpecifiedDemandFeature);
    procedure ValidateFileName (AFeature : ISpecifiedDemandFeature);
    procedure PopulateDemandFileCbx;
    procedure SetGridGraphBtnState(AFeature : ISpecifiedDemandFeature);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    
    function SpecifiedDemandChannelDialog: TSpecifiedDemandChannelDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  System.UITypes,
  Windows,
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
{* TSpecifiedDemandChannelValidator                                           *}
{******************************************************************************}

procedure TSpecifiedDemandChannelValidator.CreateMemberObjects;
const OPNAME = 'TSpecifiedDemandChannelValidator.CreateMemberObjects';
var
  lpPanel : TSpecifiedDemandChannelDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel  := TSpecifiedDemandChannelDialog.Create(FPanelOwner,FAppModules);
    lpPanel := SpecifiedDemandChannelDialog;
    with lpPanel do
    begin
      FeatureNameEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('SpecifiedDemandFeatureName');
      FeatureNameEdit.OnEnter             := OnEditControlEnter;
      FeatureNameEdit.OnExit              := OnEditControltExit;

      CatchmentReferenceCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('GaugeNumber');
      CatchmentReferenceCbx.OnEnter       := OnEditControlEnter;
      CatchmentReferenceCbx.OnExit        := OnEditControltExit;

      IndicatorRadioGroup.FieldProperty   := FAppModules.FieldProperties.FieldProperty('Stochastic');
      IndicatorRadioGroup.OnEnter         := OnEditControlEnter;
      IndicatorRadioGroup.OnClick         := OnIndicatorRadioGroupClick;

      DemandFileNameCbx.FieldProperty     := FAppModules.FieldProperties.FieldProperty('Fullname');
      DemandFileNameCbx.OnEnter           := OnEditControlEnter;
      DemandFileNameCbx.OnExit            := OnEditControltExit;

      DemandFileNameSelectBtn.OnEnter     := OnEditControlEnter;
      DemandFileNameSelectBtn.OnClick     := OnSelectDemandFileNameClick;
      DemandFileNameSelectBtn.Enabled     := (FAppModules.User.UserRights in CUR_EditData) and
                                             (not FAppModules.StudyArea.ScenarioLocked);
      DemandFileNameGridBtn.OnEnter         := OnEditControlEnter;
      DemandFileNameGridBtn.OnExit          := OnEditControltExit;
      DemandFileNameGridBtn.OnClick       := OnViewGridClick;
      DemandFileNameGraphBtn.OnEnter         := OnEditControlEnter;
      DemandFileNameGraphBtn.OnExit          := OnEditControltExit;
      DemandFileNameGraphBtn.OnClick      := OnViewGraphClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.DestroyMemberObjects;
const OPNAME = 'TSpecifiedDemandChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.Initialise: boolean;
const OPNAME = 'TSpecifiedDemandChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with SpecifiedDemandChannelDialog.IndicatorRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.StochasticSelectionOrder'));
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.HistoricalSelectionOrder'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSpecifiedDemandChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.SpecifiedDemand');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.ClearDataViewer;
const OPNAME = 'TSpecifiedDemandChannelValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    SpecifiedDemandChannelDialog.IndicatorRadioGroup.ItemIndex := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.PopulateDataViewer;
const OPNAME = 'TSpecifiedDemandChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtSpecifiedDemandFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.RePopulateDataViewer;
const OPNAME = 'TSpecifiedDemandChannelValidator.RePopulateDataViewer';
var
  lFeature       : ISpecifiedDemandFeature;
  lCatchRefList  : TObjectList;
  lCatchmentRef  : TParamReference;
  lIndicator     : string;
  lIndexA        : integer;
  lFound         : boolean;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  LFileName      : TAbstractModelFileName;
  LCount         : Integer;
begin
  try
    lCatchRefList := TYieldModelDataObject(FAppModules.Model.ModelData).CastCastParameterData.AllReferenceData;
    if (lCatchRefList <> nil) then
    begin
      with SpecifiedDemandChannelDialog do
      begin
        for lIndexA := 0 to lCatchRefList.Count - 1 do
        begin
          lCatchmentRef  := TParamReference(lCatchRefList.Items[lIndexA]);
          CatchmentReferenceCbx.Items.AddObject
            (ExtractFileName(lCatchmentRef.FileReference), TObject(lCatchmentRef.CatchReference));
        end;
      end;
    end;
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with SpecifiedDemandChannelDialog do
        begin
          lIndicator := lFeature.StochasticIndicator;
          lFieldProperty := IndicatorRadioGroup.FieldProperty;
          lKeyValues     := lFeature.GetKeyValues(lFieldProperty.FieldName, '');
          IndicatorRadioGroup.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          if (lIndicator = 'S') then
            IndicatorRadioGroup.ItemIndex := 0
          else if (lIndicator = 'H') then
            IndicatorRadioGroup.ItemIndex := 1;

          lFieldProperty := CatchmentReferenceCbx.FieldProperty;
          CatchmentReferenceCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          lFound := FALSE;
          lIndexA := 0;
          while ((NOT lFound) AND (lIndexA < CatchmentReferenceCbx.Items.Count)) do
          begin
            if (Integer(CatchmentReferenceCbx.Items.Objects[lIndexA]) = lFeature.CatchmentRefNumber) then
              lFound := TRUE
            else
              lIndexA := lIndexA + 1;
          end;
          if (lFound) then
            CatchmentReferenceCbx.ItemIndex := lIndexA
          else
            CatchmentReferenceCbx.ItemIndex := -1;

          PopulateDemandFileCbx;
          lFieldProperty := DemandFileNameCbx.FieldProperty;
          DemandFileNameCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          DemandFileNameCbx.ItemIndex  := DemandFileNameCbx.Items.IndexOf(lFeature.SpecifiedDemandFileName);

          for LCount := 0 to DemandFileNameCbx.Items.Count - 1 do
          begin
            if(UpperCase(lFeature.SpecifiedDemandFileName) = UpperCase(DemandFileNameCbx.Items[LCount])) then
            begin
              DemandFileNameCbx.ItemIndex := LCount;
              Break;
            end;
          end;

          DemandFileImportedLabel.Caption := '';
          LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.FindFile(lFeature.SpecifiedDemandFileName);
          if(LFileName <> nil) then
          begin
            if LFileName.SavedInDB then
              DemandFileImportedLabel.Caption := FAppModules.Language.GetString('TSpecifiedDemandChannelDialog.DemandFileSavedInDB')
            else
              DemandFileImportedLabel.Caption := FAppModules.Language.GetString('TSpecifiedDemandChannelDialog.DemandFileNotSavedInDB');
          end;

          SetGridGraphBtnState(lFeature);
          lFieldProperty := FeatureNameEdit.FieldProperty;
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.PopulateDemandFileCbx;
const OPNAME = 'TSpecifiedDemandChannelValidator.PopulateDemandFileCbx';
var
  LFileNamesList : TFileNamesList;
  LFileName      : string;
  LIndex         : integer;
begin
  try
    with SpecifiedDemandChannelDialog do
    begin
      DemandFileNameCbx.Items.Clear;
      DemandFileNameCbx.Sorted := True;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        LFileName := LFileNamesList.FileNameObject[LIndex].FileName;
        DemandFileNameCbx.Items.Add(LFileName);
      end;
    end;
    {
    sFilePath := ExtractFilePath(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.ParamFileNames.FileNameObject[0].FileName);
    lFileList := TStringList.Create;
    try
      sFileName := sFilePath + '*.*';
      lFileList.Clear;
      if (UUtilities.SearchFiles(sFileName, lFileList)) then
      begin
        for nIndex := 0 to lFileList.Count - 1 do
        begin
          sFileExt := ExtractFileExt(LFileList[nIndex]);
          if(UpperCase(sFileExt) = '.ABS') or
            (UpperCase(sFileExt) = '.CIR') or
            (UpperCase(sFileExt) = '.IRR') or
            (UpperCase(sFileExt) = '.URB') or
            (UpperCase(sFileExt) = '.IRD') or
            (UpperCase(sFileExt) = '.DEM') then
          begin
            DemandFileNameCbx.Items.Add(lFileList[nIndex]);
          end;
        end;
      finally
        lFileList.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.SaveState: boolean;
const OPNAME = 'TSpecifiedDemandChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.SpecifiedDemandChannelDialog : TSpecifiedDemandChannelDialog;
const OPNAME = 'TSpecifiedDemandChannelValidator.SpecifiedDemandChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TSpecifiedDemandChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSpecifiedDemandChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TSpecifiedDemandChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TSpecifiedDemandChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TSpecifiedDemandChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with SpecifiedDemandChannelDialog do
    begin
      if ((Sender = CatchmentReferenceCbx) AND (CatchmentReferenceCbx.HasValueChanged)) then
        UpdateCatchmentReference
      else if ((Sender = DemandFileNameCbx) AND (DemandFileNameCbx.HasValueChanged)) then
        UpdateDemandFileName
      else if ((sender = FeatureNameEdit) AND (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.UpdateStochasticIndicator;
const OPNAME = 'TSpecifiedDemandChannelValidator.UpdateStochasticIndicator';
var
  lFeature  : ISpecifiedDemandFeature;
  lOldValue : string;
  lNewValue : string;
  lMessage  : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with SpecifiedDemandChannelDialog do
      begin
        lOldValue := UpperCase(Trim(lFeature.StochasticIndicator));
        lNewValue := lOldValue;
        if (IndicatorRadioGroup.ItemIndex = 0) then
          lNewValue := 'S'
        else if (IndicatorRadioGroup.ItemIndex = 1) then
          lNewValue := 'H';
        if (lOldValue <> lNewValue) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              IndicatorRadioGroup.FieldProperty.FieldName,
              lNewValue,lMessage)) then
          begin
            lFeature.StochasticIndicator:= lNewValue;
            lNewValue := lFeature.StochasticIndicator;
            if (lNewValue = 'S') then
              IndicatorRadioGroup.ItemIndex := 0
            else if (lNewValue = 'H') then
              IndicatorRadioGroup.ItemIndex := 1
            else
              IndicatorRadioGroup.ItemIndex := -1;
            DoContextValidation(dvtSpecifiedDemandStochasticIndicator);
          end
          else
            IndicatorRadioGroup.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.UpdateDemandFileName;
const OPNAME = 'TSpecifiedDemandChannelValidator.UpdateDemandFileName';
var
  lFeature : ISpecifiedDemandFeature;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with SpecifiedDemandChannelDialog do
      begin
        lFeature.SpecifiedDemandFileName := DemandFileNameCbx.Text;
        RePopulateDataViewer;
        ValidateFileName(lFeature)
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.UpdateCatchmentReference;
const OPNAME = 'TSpecifiedDemandChannelValidator.UpdateCatchmentReference';
var
  lFeature  : ISpecifiedDemandFeature;
  lMessage  : string;
  lCatchRef : integer;
  lFound    : boolean;
  lIndexA   : integer;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with SpecifiedDemandChannelDialog do
      begin
        lCatchRef := Integer(CatchmentReferenceCbx.Items.Objects[CatchmentReferenceCbx.ItemIndex]);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CatchmentReferenceCbx.FieldProperty.FieldName,
            IntToStr(lCatchRef), lMessage)) then
        begin
          lFeature.CatchmentRefNumber := lCatchRef;
          lFound  := FALSE;
          lIndexA := 0;
          while (NOT lFound) do
          begin
            if (Integer(CatchmentReferenceCbx.Items.Objects[lIndexA]) = lFeature.CatchmentRefNumber) then
              lFound := TRUE
            else
              lIndexA := lIndexA + 1;
          end;
          if (lFound) then
            CatchmentReferenceCbx.SetFieldIndex(lIndexA)
          else
            CatchmentReferenceCbx.SetFieldIndex(-1);
          DoContextValidation(dvtSpecifiedDemandCatchmentRef);
        end
        else
          CatchmentReferenceCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.UpdateFeatureName;
const OPNAME = 'TSpecifiedDemandChannelValidator.UpdateFeatureName';
var
  lMessage : string;
  lDemandFeature : ISpecifiedDemandFeature;
begin
  try
    lDemandFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                   .SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
    if (lDemandFeature <> nil) then
    begin
      with SpecifiedDemandChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(FeatureNameEdit.FieldProperty.FieldName,
          FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lDemandFeature.FeatureName := trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lDemandFeature.FeatureName);
          DoContextValidation(dvtSpecifiedDemandFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TSpecifiedDemandChannelValidator.DoContextValidation';
var
  lFeature     : ISpecifiedDemandFeature;
  lFeatureList : ISpecifiedDemandFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedDemandFeatureList;
      lFeature     := lFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtSpecifiedDemandFeature, dvtSpecifiedDemandFeatureName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtSpecifiedDemandFeature, dvtSpecifiedDemandStochasticIndicator]) then
          ValidateStochasticIndicator(lFeature);
        if (AValidationType in [dvtSpecifiedDemandFeature, dvtSpecifiedDemandCatchmentRef]) then
          ValidateCatchmentRefNumber(lFeature);
        if (AValidationType in [dvtSpecifiedDemandFeature, dvtSpecifiedDemandFullName]) then
          ValidateFileName(lFeature);
      end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TSpecifiedDemandChannelValidator.DetermineWizardStatus';
var
  lFeature       : ISpecifiedDemandFeature;
  lFeatureList   : ISpecifiedDemandFeatureList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedDemandFeatureList;
      lFeature := lFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtSpecifiedDemandFeature);
        if ((lFeature.CatchmentRefNumber <> 0) OR (lFeature.SpecifiedDemandFileName <> '')) then
        begin
          Result := 1;
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.ValidateFeatureName (AFeature : ISpecifiedDemandFeature);
const OPNAME = 'TSpecifiedDemandChannelValidator.ValidateFeatureName';
begin
  try
    with SpecifiedDemandChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'FeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.ValidateStochasticIndicator (AFeature : ISpecifiedDemandFeature);
const OPNAME = 'TSpecifiedDemandChannelValidator.ValidateStochasticIndicator';
begin
  try
    with SpecifiedDemandChannelDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'StochasticIndicator')) then
      begin
        IndicatorRadioGroup.InValidationError := FALSE;
        IndicatorRadioGroup.ShowErrorState(FALSE);
      end
      else
      begin
        IndicatorRadioGroup.InValidationError := TRUE;
        IndicatorRadioGroup.ValidationError := FErrorMessage;
        IndicatorRadioGroup.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.ValidateCatchmentRefNumber (AFeature : ISpecifiedDemandFeature);
const OPNAME = 'TSpecifiedDemandChannelValidator.ValidateCatchmentRefNumber';
begin
  try
    with SpecifiedDemandChannelDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'CatchmentRefNumber')) then
      begin
        CatchmentReferenceCbx.InValidationError := FALSE;
        CatchmentReferenceCbx.ValidationError := '';
        CatchmentReferenceCbx.ShowErrorState(FALSE);
      end
      else
      begin
        CatchmentReferenceCbx.InValidationError := TRUE;
        CatchmentReferenceCbx.ValidationError := FErrorMessage;
        CatchmentReferenceCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.ValidateFileName (AFeature : ISpecifiedDemandFeature);
const OPNAME = 'TSpecifiedDemandChannelValidator.ValidateFileName';
begin
  try
    with SpecifiedDemandChannelDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'FileName')) then
      begin
        DemandFileNameCbx.InValidationError := FALSE;
        DemandFileNameCbx.ValidationError := '';
        DemandFileNameCbx.ShowErrorState(FALSE);
      end
      else
      begin
        DemandFileNameCbx.InValidationError := TRUE;
        DemandFileNameCbx.ValidationError := FErrorMessage;
        DemandFileNameCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.OnIndicatorRadioGroupClick(Sender: TObject);
const OPNAME = 'TSpecifiedDemandChannelValidator.OnIndicatorRadioGroupClick';
begin
  try
    if(SpecifiedDemandChannelDialog.IndicatorRadioGroup.HasValueChanged) then
      UpdateStochasticIndicator;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.OnSelectDemandFileNameClick(Sender: TObject);
const OPNAME = 'TSpecifiedDemandChannelValidator.OnSelectDemandFileNameClick';
var
  LFileSelector: TOpenDialog;
  LDemandFilesPath,
  LPath,
  LFileName,
  LFileNameStr: string;
  lFeature : ISpecifiedDemandFeature;
begin
  try
    if(Trim(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath) = '') then
    begin
       ShowMessage(FAppModules.Language.GetString('Message.SelectDemandFilePath'));
       Exit;
    end;

    LFileSelector := TOpenDialog.Create(nil);
    try
      LFileSelector.Title   := 'Select demand file.';
      LFileSelector.Filter  := 'All Files|*.*|(*.ABS)|*.abs|(*.CIR)|*.cir|(*.DEM)|*.dem|(*.IRR)|*.IRR|(*.IRD)|*.ird|(*.URB)|*.urb';
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      LFileSelector.InitialDir := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath;
      if LFileSelector.Execute then
      begin
        LFileNameStr := LFileSelector.FileName;
        LFileName    := ExtractFileName(LFileNameStr);
        LPath        := ExtractFilePath(LFileNameStr);
        LDemandFilesPath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath;
        if(LDemandFilesPath <> LPath) then
        begin
          if(MessageDlg('The selected demand file will be copied to the scenario folder before being imported. Continue?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
            Exit;
          LFileName   :=  IncludeTrailingPathDelimiter(LDemandFilesPath ) + LFileName;
          if FileExists(LFileName) then
          begin
            if(MessageDlg('File already exist in the destination folder. Do you want to override it?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
              Exit;
          end;
          if not DirectoryExists(LDemandFilesPath) then
            ForceDirectories(LDemandFilesPath);
          CopyFile(PChar(LFileNameStr),PChar(LFileName),False);
          LFileNameStr := LFileName;
        end;

        if(UpperCase(LFileNameStr) <> UpperCase(SpecifiedDemandChannelDialog.DemandFileNameCbx.Text)) then
        begin
          lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
          if (lFeature <> nil) then
          begin
            lFeature.SpecifiedDemandFileName := LFileNameStr;
            RePopulateDataViewer;
            ValidateFileName(lFeature)
          end;
        end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TSpecifiedDemandChannelValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : ISpecifiedDemandFeature;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with SpecifiedDemandChannelDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = CatchmentReferenceCbx) then
            lFieldProperty := CatchmentReferenceCbx.FieldProperty
          else
          if (FActiveControl = DemandFileNameCbx) then
            lFieldProperty := DemandFileNameCbx.FieldProperty
          else
          if (FActiveControl = IndicatorRadioGroup) then
            lFieldProperty := IndicatorRadioGroup.FieldProperty;
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

procedure TSpecifiedDemandChannelValidator.OnViewGridClick(Sender: TObject);
const OPNAME = 'TSpecifiedDemandChannelValidator.OnViewGridClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRID');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FFeatureID]));
      LContextDataList.Add('MODELELEMENTTYPE=SPECIFIEDDEMANDFEATURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.OnViewGraphClick(Sender: TObject);
const OPNAME = 'TSpecifiedDemandChannelValidator.OnViewGraphClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRAPH');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FFeatureID]));
      LContextDataList.Add('MODELELEMENTTYPE=SPECIFIEDDEMANDFEATURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelValidator.SetGridGraphBtnState(AFeature: ISpecifiedDemandFeature);
const OPNAME = 'TSpecifiedDemandChannelValidator.SetGridGraphBtnState';
var
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
  LIndex         : integer;
begin
  try
    SpecifiedDemandChannelDialog.DemandFileNameGridBtn.Enabled := False;
    SpecifiedDemandChannelDialog.DemandFileNameGraphBtn.Enabled := False;
    if(AFeature <> nil) and (AFeature.SpecifiedDemandFileName <> '') then
    begin
      LFileNameObject := nil;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        if(UpperCase(AFeature.SpecifiedDemandFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].FileName)) then
        begin
          LFileNameObject := LFileNamesList.FileNameObject[LIndex];
          Break;
        end;
      end;
      if(LFileNameObject <> nil) then
      begin
        SpecifiedDemandChannelDialog.DemandFileNameGridBtn.Enabled  := LFileNameObject.SavedInDB;
        SpecifiedDemandChannelDialog.DemandFileNameGraphBtn.Enabled := LFileNameObject.SavedInDB;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

