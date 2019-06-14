//
//
//  UNIT      : Contains the class TStreamFlowReductionValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UStreamFlowReductionValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Windows,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UStreamFlowReduction,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UStreamFlowReductionDialog;

type
  TStreamFlowReductionValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;

    procedure OnViewGridClick(Sender: TObject);
    procedure OnViewGraphClick(Sender: TObject);
    procedure OnSelectUnitRunoffFileNameClick(Sender: TObject);
    procedure OnSelectSoilMoistureFileNameClick(Sender: TObject);

    procedure UpdateSFRName;
    procedure UpdateInflowNodeNumber;
    procedure UpdateSFRDescr;
    procedure UpdateCoveredArea;
    procedure UpdateUnitRunoffFileName;
    procedure UpdateSoilMoistureFileName;
    procedure SetFileStatusLabelsText;
    procedure SetGridGraphBtnState;

    procedure ValidateSFRName(AStreamFlowReduction: TStreamFlowReduction);
    procedure ValidateInflowNodeNumber(AStreamFlowReduction: TStreamFlowReduction);
    procedure ValidateCoveredArea(AStreamFlowReduction: TStreamFlowReduction);
    procedure ValidateSFRDescr(AStreamFlowReduction: TStreamFlowReduction);
    procedure ValidateUnitRunoffFileName(AStreamFlowReduction: TStreamFlowReduction);
    procedure ValidateSoilMoistureFileName(AStreamFlowReduction: TStreamFlowReduction);
    function StreamFlowReductionDialog: TStreamFlowReductionDialog;

    procedure RePopulateDataViewer;
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Dialogs,
  UFileNames,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TStreamFlowReductionValidator }

procedure TStreamFlowReductionValidator.CreateMemberObjects;
const OPNAME = 'TStreamFlowReductionValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TStreamFlowReductionDialog.Create(FPanelOwner,FAppModules);

    with StreamFlowReductionDialog do
    begin
      SFRIdentifierEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('Identifier');
      SFRIdentifierEdit.OnEnter             := OnEditControlEnter;
      SFRIdentifierEdit.IsEnabled           := FALSE;

      SFRNameEdit.FieldProperty             := FAppModules.FieldProperties.FieldProperty('SFRName');
      SFRNameEdit.OnEnter                   := OnEditControlEnter;
      SFRNameEdit.OnExit                    := OnEditControltExit;

      InflowNodeNumberCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('InflowNodeNumber');
      InflowNodeNumberCbx.OnEnter            := OnEditControlEnter;
      InflowNodeNumberCbx.OnExit             := OnEditControltExit;

      CoveredAreaEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('CoveredArea');
      CoveredAreaEdit.OnEnter               := OnEditControlEnter;
      CoveredAreaEdit.OnExit                := OnEditControltExit;

      UnitRunoffFileNameCbx.FieldProperty   := FAppModules.FieldProperties.FieldProperty('UnitRunoffFileName');
      UnitRunoffFileNameCbx.OnEnter         := OnEditControlEnter;
      UnitRunoffFileNameCbx.OnExit          := OnEditControltExit;

      SoilMoistureFileNameCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('SoilMoistureFileName');
      SoilMoistureFileNameCbx.OnEnter       := OnEditControlEnter;
      SoilMoistureFileNameCbx.OnExit        := OnEditControltExit;

      UnitRunoffFileNameGridBtn.OnClick     := OnViewGridClick;
      UnitRunoffFileNameGraphBtn.OnClick    := OnViewGraphClick;
      UnitRunoffFileNameSelectBtn.OnClick   := OnSelectUnitRunoffFileNameClick;
      UnitRunoffFileNameSelectBtn.FieldProperty  := FAppModules.FieldProperties.FieldProperty('UnitRunoffFileName');

      SoilMoistureFileNameGridBtn.OnClick   := OnViewGridClick;
      SoilMoistureFileNameGraphBtn.OnClick  := OnViewGraphClick;
      SoilMoistureFileNameSelectBtn.OnClick := OnSelectSoilMoistureFileNameClick;
      SoilMoistureFileNameSelectBtn.FieldProperty  := FAppModules.FieldProperties.FieldProperty('SoilMoistureFileName');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.DestroyMemberObjects;
const OPNAME = 'TStreamFlowReductionValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionValidator.Initialise: boolean;
const OPNAME = 'TStreamFlowReductionValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    StreamFlowReductionDialog.SFRDescrEdit.FieldProperty            := FAppModules.FieldProperties.FieldProperty('SFRDescr');
    StreamFlowReductionDialog.SFRDescrEdit.OnEnter                  := OnEditControlEnter;
    StreamFlowReductionDialog.SFRDescrEdit.OnExit                   := OnEditControltExit;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TStreamFlowReductionValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.StreamFlowReduction');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.ClearDataViewer;
const OPNAME = 'TStreamFlowReductionValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    StreamFlowReductionDialog.SFRIdentifierEdit.SetFieldValue('');
    StreamFlowReductionDialog.SFRNameEdit.SetFieldValue('');
    StreamFlowReductionDialog.InflowNodeNumberCbx.ItemIndex := -1;
    StreamFlowReductionDialog.SFRDescrEdit.SetFieldValue('');
    StreamFlowReductionDialog.CoveredAreaEdit.SetFieldValue('');
    StreamFlowReductionDialog.UnitRunoffFileNameCbx.ItemIndex := -1;
    StreamFlowReductionDialog.SoilMoistureFileNameCbx.ItemIndex := -1;
    StreamFlowReductionDialog.UnitRunoffFileNameGridBtn.Enabled := False;
    StreamFlowReductionDialog.UnitRunoffFileNameGraphBtn.Enabled := False;
    StreamFlowReductionDialog.SoilMoistureFileNameGridBtn.Enabled := False;
    StreamFlowReductionDialog.SoilMoistureFileNameGraphBtn.Enabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.PopulateDataViewer;
const OPNAME = 'TStreamFlowReductionValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtSreamFlowReductionAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.RePopulateDataViewer;
const OPNAME = 'TStreamFlowReductionValidator.RePopulateDataViewer';
var
  lReservoir           : IReservoirData;
  LStreamFlowReduction : TStreamFlowReduction;
  LIndex               : integer;
  LFileNamesList       : TFileNamesList;
  LFileName            : string;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
      if (LStreamFlowReduction <> nil) then
      begin
        with StreamFlowReductionDialog do
        begin
          SFRIdentifierEdit.SetFieldValue(LStreamFlowReduction.Identifier);
          SFRNameEdit.SetFieldValue(LStreamFlowReduction.SFRName);

          SFRDescrEdit.SetFieldValue(LStreamFlowReduction.SFRDescription);
          CoveredAreaEdit.SetFieldValue(LStreamFlowReduction.CoveredArea);

          LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames;
          UnitRunoffFileNameCbx.Items.Clear;
          UnitRunoffFileNameCbx.Sorted := True;
          SoilMoistureFileNameCbx.Items.Clear;
          SoilMoistureFileNameCbx.Sorted := True;
          for LIndex:= 0 to LFileNamesList.FilesCount-1 do
          begin
            LFileName := ExtractFileName(LFileNamesList.FileNameObject[LIndex].FileName);
            UnitRunoffFileNameCbx.Items.Add(LFileName);
            SoilMoistureFileNameCbx.Items.Add(LFileName);
          end;
          LFileName :=  ExtractFileName(LStreamFlowReduction.UnitRunoffFileName);
          UnitRunoffFileNameCbx.SetFieldIndex(UnitRunoffFileNameCbx.Items.IndexOf(LFileName));
          LFileName :=  ExtractFileName(LStreamFlowReduction.SoilMoistureFileName);
          SoilMoistureFileNameCbx.SetFieldIndex(SoilMoistureFileNameCbx.Items.IndexOf(LFileName));

          InflowNodeNumberCbx.Items.Clear;
          for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
                        ReservoirAndNodesCount - 1 do
          begin
            lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
                          ReservoirOrNodeByIndex[LIndex];

            if(lReservoir.ReservoirConfigurationData.CatchmentRef <> 0) then
            begin
              InflowNodeNumberCbx.Items.AddObject(lReservoir.ReservoirConfigurationData.ReservoirName,
                                                  TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier));
            end;
          end;
          lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
                          ReservoirOrNodeByIdentifier[LStreamFlowReduction.InflowNodeNumber];
          if(lReservoir <> nil) then
          begin
            InflowNodeNumberCbx.SetFieldIndex(InflowNodeNumberCbx.Items.IndexOf(
                                              lReservoir.ReservoirConfigurationData.ReservoirName));
          end;

          SetFileStatusLabelsText;
          SetGridGraphBtnState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.SetFileStatusLabelsText;
const OPNAME = 'TStreamFlowReductionValidator.SetFileStatusLabelsText';
var
  LStreamFlowReduction : TStreamFlowReduction;
  LFileName     : string;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
      if (LStreamFlowReduction <> nil) then
      begin
        with StreamFlowReductionDialog do
        begin
          UnitRunoffFileImportedLabel.Caption     := '';
          LFileName       := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath+LStreamFlowReduction.UnitRunoffFileName;
          LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames.FindFile(LFileName);
          if(LFileNameObject <> nil) then
          begin
            if LFileNameObject.SavedInDB then
              UnitRunoffFileImportedLabel.Caption := FAppModules.Language.GetString('TStreamFlowReductionDialog.UnitRunoffFileSavedInDB')
            else
              UnitRunoffFileImportedLabel.Caption := FAppModules.Language.GetString('TStreamFlowReductionDialog.UnitRunoffFileNotSavedInDB');
          end;

          SoilMoistureFileImportedLabel.Caption   := '';
          LFileName       := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath+LStreamFlowReduction.SoilMoistureFileName;
          LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames.FindFile(LFileName);
          if(LFileNameObject <> nil) then
          begin
            if LFileNameObject.SavedInDB then
              SoilMoistureFileImportedLabel.Caption := FAppModules.Language.GetString('TStreamFlowReductionDialog.SoilMoistureFileSavedInDB')
            else
              SoilMoistureFileImportedLabel.Caption := FAppModules.Language.GetString('TStreamFlowReductionDialog.SoilMoistureFileNotSavedInDB');
          end;

        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionValidator.SaveState: boolean;
const OPNAME = 'TStreamFlowReductionValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionValidator.StreamFlowReductionDialog : TStreamFlowReductionDialog;
const OPNAME = 'TStreamFlowReductionValidator.StreamFlowReductionDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TStreamFlowReductionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TStreamFlowReductionValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionValidator.StudyHasChanged: boolean;
const OPNAME = 'TStreamFlowReductionValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TStreamFlowReductionValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TStreamFlowReductionValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with StreamFlowReductionDialog do
    begin
      if ((Sender = SFRNameEdit) AND (SFRNameEdit.HasValueChanged)) then
        UpdateSFRName
      else
      if ((Sender = InflowNodeNumberCbx) AND (InflowNodeNumberCbx.HasValueChanged)) then
        UpdateInflowNodeNumber
      else
      if ((Sender = SFRDescrEdit) AND (SFRDescrEdit.HasValueChanged)) then
        UpdateSFRDescr
      else
      if ((Sender = CoveredAreaEdit) AND (CoveredAreaEdit.HasValueChanged)) then
        UpdateCoveredArea
      else
      if (Sender = UnitRunoffFileNameCbx) and UnitRunoffFileNameCbx.HasValueChanged then
        UpdateUnitRunoffFileName
      else
      if (Sender = SoilMoistureFileNameCbx) and SoilMoistureFileNameCbx.HasValueChanged then
        UpdateSoilMoistureFileName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.UpdateSFRName;
const OPNAME = 'TStreamFlowReductionValidator.UpdateSFRName';
var
  LStreamFlowReduction : TStreamFlowReduction;
  LErrorMessage: string;
begin
  try
    LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
    if (LStreamFlowReduction <> nil) then
    begin
      with StreamFlowReductionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('SFRName', SFRNameEdit.Text,LErrorMessage)) then
        begin
          LStreamFlowReduction.SFRName := Trim(SFRNameEdit.Text);
          SFRNameEdit.SetFieldValue(LStreamFlowReduction.SFRName);
          DoContextValidation(dvtSFRName);
        end
        else
          SFRNameEdit.FieldValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.UpdateInflowNodeNumber;
const OPNAME = 'TStreamFlowReductionValidator.UpdateInflowNodeNumber';
var
  LReservoir     : IReservoirData;
  LReservoirID   : integer;
  LStreamFlowReduction : TStreamFlowReduction;
  LErrorMessage: string;
begin
  try
    LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
    if (LStreamFlowReduction <> nil) then
    begin
      with StreamFlowReductionDialog do
      begin
        if(InflowNodeNumberCbx.ItemIndex >= 0) then
        begin
          LReservoirID := Integer(InflowNodeNumberCbx.Items.Objects[InflowNodeNumberCbx.ItemIndex]);
          LReservoir    := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
                           ReservoirOrNodeByIdentifier[LReservoirID];
          if(LReservoir <> nil) then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty('InflowNodeNumber', IntToStr(LReservoir.ReservoirConfigurationData.ReservoirIdentifier),LErrorMessage)) then
            begin
              LStreamFlowReduction.InflowNodeNumber := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
              InflowNodeNumberCbx.SetFieldIndex(InflowNodeNumberCbx.Items.IndexOf(LReservoir.ReservoirConfigurationData.ReservoirName));
              DoContextValidation(dvtInflowNodeNumber);
            end
            else
              CoveredAreaEdit.FieldValidationError := LErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.UpdateCoveredArea;
const OPNAME = 'TStreamFlowReductionValidator.UpdateCoveredArea';
var
  LStreamFlowReduction : TStreamFlowReduction;
  LErrorMessage: string;
begin
  try
    LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
    if (LStreamFlowReduction <> nil) then
    begin
      with StreamFlowReductionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('CoveredArea', CoveredAreaEdit.Text,LErrorMessage)) then
        begin
          LStreamFlowReduction.CoveredArea := StrToFloat(Trim(CoveredAreaEdit.Text));
          CoveredAreaEdit.SetFieldValue(LStreamFlowReduction.CoveredArea);
          DoContextValidation(dvtCoveredArea);
        end
        else
          CoveredAreaEdit.FieldValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.UpdateSFRDescr;
const OPNAME = 'TStreamFlowReductionValidator.UpdateSFRDescr';
var
  LStreamFlowReduction : TStreamFlowReduction;
  LErrorMessage: string;
begin
  try
    LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
    if (LStreamFlowReduction <> nil) then
    begin
      with StreamFlowReductionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('SFRDescr', SFRDescrEdit.Lines.Text,LErrorMessage)) then
        begin
          LStreamFlowReduction.SFRDescription := Trim(SFRDescrEdit.Lines.Text);
          SFRDescrEdit.SetFieldValue(LStreamFlowReduction.SFRDescription);
          DoContextValidation(dvtSFRDescription);
        end
        else
          SFRDescrEdit.FieldValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.UpdateUnitRunoffFileName;
const OPNAME = 'TStreamFlowReductionValidator.UpdateUnitRunoffFileName';
var
  LStreamFlowReduction : TStreamFlowReduction;
  LErrorMessage: string;
begin
  try
    LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
    if (LStreamFlowReduction <> nil) then
    begin
      with StreamFlowReductionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('UnitRunoffFileName', UnitRunoffFileNameCbx.Text,LErrorMessage)) then
        begin
          LStreamFlowReduction.UnitRunoffFileName := Trim(UnitRunoffFileNameCbx.Text);
          DoContextValidation(dvtUnitRunoffFileName);
        end
        else
          UnitRunoffFileNameCbx.ValidationError := LErrorMessage;
      end;
    end;
    SetFileStatusLabelsText;
    SetGridGraphBtnState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.UpdateSoilMoistureFileName;
const OPNAME = 'TStreamFlowReductionValidator.UpdateSoilMoistureFileName';
var
  LStreamFlowReduction : TStreamFlowReduction;
  LErrorMessage: string;
begin
  try
    LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
    if (LStreamFlowReduction <> nil) then
    begin
      with StreamFlowReductionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('SoilMoistureFileName', SoilMoistureFileNameCbx.Text,LErrorMessage)) then
        begin
          LStreamFlowReduction.SoilMoistureFileName := Trim(SoilMoistureFileNameCbx.Text);
          DoContextValidation(dvtSoilMoistureFileName);
        end
        else
          UnitRunoffFileNameCbx.ValidationError := LErrorMessage;
      end;
    end;
    SetFileStatusLabelsText;
    SetGridGraphBtnState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TStreamFlowReductionValidator.DoContextValidation';
var
  LStreamFlowReduction : TStreamFlowReduction;
begin
  try
    LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
    if (LStreamFlowReduction <> nil) then
    begin
       if (AValidationType = dvtSreamFlowReductionAll) or (AValidationType = dvtSFRName) then
          ValidateSFRName(LStreamFlowReduction);
       if (AValidationType = dvtSreamFlowReductionAll) or (AValidationType = dvtInflowNodeNumber) then
          ValidateInflowNodeNumber(LStreamFlowReduction);
       if (AValidationType = dvtSreamFlowReductionAll) or (AValidationType = dvtCoveredArea) then
          ValidateCoveredArea(LStreamFlowReduction);
       if (AValidationType = dvtSreamFlowReductionAll) or (AValidationType = dvtUnitRunoffFileName) then
          ValidateUnitRunoffFileName(LStreamFlowReduction);
       if (AValidationType = dvtSreamFlowReductionAll) or (AValidationType = dvtSoilMoistureFileName) then
          ValidateSoilMoistureFileName(LStreamFlowReduction);
       if (AValidationType = dvtSreamFlowReductionAll) or (AValidationType = dvtSFRDescription) then
          ValidateSFRDescr(LStreamFlowReduction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.ValidateSFRName(AStreamFlowReduction: TStreamFlowReduction);
const OPNAME = 'TStreamFlowReductionValidator.ValidateSFRName';
begin
  try
    with StreamFlowReductionDialog do
    begin
      FErrorMessage := '';
      if (NOT AStreamFlowReduction.Validate(FErrorMessage,'SFRName')) then
        FAllErrorMessages.Add(FErrorMessage);
      SFRNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.ValidateInflowNodeNumber(AStreamFlowReduction: TStreamFlowReduction);
const OPNAME = 'TStreamFlowReductionValidator.ValidateInflowNodeNumber';
begin
  try
    with StreamFlowReductionDialog do
    begin
      FErrorMessage := '';
      if (NOT AStreamFlowReduction.Validate(FErrorMessage,'InflowNodeNumber')) then
        FAllErrorMessages.Add(FErrorMessage);
      InflowNodeNumberCbx.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.ValidateCoveredArea(AStreamFlowReduction: TStreamFlowReduction);
const OPNAME = 'TStreamFlowReductionValidator.ValidateCoveredArea';
begin
  try
    with StreamFlowReductionDialog do
    begin
      FErrorMessage := '';
      if (NOT AStreamFlowReduction.Validate(FErrorMessage,'CoveredArea')) then
        FAllErrorMessages.Add(FErrorMessage);
      CoveredAreaEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TStreamFlowReductionValidator.ValidateSFRDescr(AStreamFlowReduction: TStreamFlowReduction);
const OPNAME = 'TStreamFlowReductionValidator.ValidateSFRDescr';
begin
  try
    with StreamFlowReductionDialog do
    begin
      FErrorMessage := '';
      if (NOT AStreamFlowReduction.Validate(FErrorMessage,'SFRDescr')) then
        FAllErrorMessages.Add(FErrorMessage);
      SFRDescrEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.ValidateSoilMoistureFileName(AStreamFlowReduction: TStreamFlowReduction);
const OPNAME = 'TStreamFlowReductionValidator.ValidateSoilMoistureFileName';
begin
  try
    with StreamFlowReductionDialog do
    begin
      FErrorMessage := '';
      if (NOT AStreamFlowReduction.Validate(FErrorMessage,'SoilMoistureFileName')) then
        FAllErrorMessages.Add(FErrorMessage);
      SoilMoistureFileNameCbx.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.ValidateUnitRunoffFileName(AStreamFlowReduction: TStreamFlowReduction);
const OPNAME = 'TStreamFlowReductionValidator.ValidateUnitRunoffFileName';
begin
  try
    with StreamFlowReductionDialog do
    begin
      FErrorMessage := '';
      if (NOT AStreamFlowReduction.Validate(FErrorMessage,'UnitRunoffFileName')) then
        FAllErrorMessages.Add(FErrorMessage);
      UnitRunoffFileNameCbx.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.OnSelectUnitRunoffFileNameClick(Sender: TObject);
const OPNAME = 'TStreamFlowReductionValidator.OnSelectUnitRunoffFileNameClick';
var
  LFileSelector: TOpenDialog;
  LHydrologyFilesPath,
  LPath,
  LFileName,
  LFileNameStr: string;
  LStreamFlowReduction : TStreamFlowReduction;
begin
  try
    if (TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath = '') then
    begin
       ShowMessage('Please select the hydrology files path(in Run Configuration) before selecting unit runoff time-series data files.');
       Exit;
    end;

    LFileSelector := TOpenDialog.Create(nil);
    try
      LFileSelector.Title   := 'Select unit runoff time-series data file(*.R)';
      LFileSelector.Filter  := 'Unit Runoff Files|*.R|All files (*.*)|*.*';
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      LFileSelector.InitialDir := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
      if LFileSelector.Execute then
      begin
        LFileNameStr := LFileSelector.FileName;
        LFileName    := ExtractFileName(LFileNameStr);
        LPath        := ExtractFilePath(LFileNameStr);
        LHydrologyFilesPath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
        if(LHydrologyFilesPath <> LPath) then
        begin
          if(MessageDlg('The selected unit runoff time-series file will be copied to the scenario folder before being imported. Continue?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
            Exit;
          LFileName   :=  IncludeTrailingPathDelimiter(LHydrologyFilesPath ) + LFileName;
          if FileExists(LFileName) then
          begin
            if(MessageDlg('File already exist in the destination folder. Do you want to override it?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
              Exit;
          end;
          if not DirectoryExists(LHydrologyFilesPath) then
            ForceDirectories(LHydrologyFilesPath);
          CopyFile(PChar(LFileNameStr),PChar(LFileName),False);
          LFileNameStr := LFileName;
        end;

        if(UpperCase(ExtractFileName(LFileNameStr)) <> UpperCase(StreamFlowReductionDialog.UnitRunoffFileNameCbx.Text)) then
        begin
          LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
          if (LStreamFlowReduction <> nil) then
          begin
            LStreamFlowReduction.UnitRunoffFileName := LFileNameStr;
            RePopulateDataViewer;
            ValidateUnitRunoffFileName(LStreamFlowReduction)
          end;
        end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.OnSelectSoilMoistureFileNameClick(Sender: TObject);
const OPNAME = 'TStreamFlowReductionValidator.OnSelectSoilMoistureFileNameClick';
var
  LFileSelector: TOpenDialog;
  LHydrologyFilesPath,
  LPath,
  LFileName,
  LFileNameStr: string;
  LStreamFlowReduction : TStreamFlowReduction;
begin
  try
    if (TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath = '') then
    begin
       ShowMessage('Please select the hydrology files path(in Run Configuration) before selecting total soil moisture data files.');
       Exit;
    end;

    LFileSelector := TOpenDialog.Create(nil);
    try
      LFileSelector.Title   := 'Select total soil moisture data file(*.S))';
      LFileSelector.Filter  := 'Soil Moisture Files|*.S|All files (*.*)|*.*';
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      LFileSelector.InitialDir := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
      if LFileSelector.Execute then
      begin
        LFileNameStr := LFileSelector.FileName;
        LFileName    := ExtractFileName(LFileNameStr);
        LPath        := ExtractFilePath(LFileNameStr);
        LHydrologyFilesPath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
        if(LHydrologyFilesPath <> LPath) then
        begin
          if(MessageDlg('The selected total soil moisture file will be copied to the scenario folder before being imported. Continue?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
            Exit;
          LFileName   :=  IncludeTrailingPathDelimiter(LHydrologyFilesPath ) + LFileName;
          if FileExists(LFileName) then
          begin
            if(MessageDlg('File already exist in the destination folder. Do you want to override it?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
              Exit;
          end;
          if not DirectoryExists(LHydrologyFilesPath) then
            ForceDirectories(LHydrologyFilesPath);
          CopyFile(PChar(LFileNameStr),PChar(LFileName),False);
          LFileNameStr := LFileName;
        end;

        if(UpperCase(ExtractFileName(LFileNameStr)) <> UpperCase(StreamFlowReductionDialog.SoilMoistureFileNameCbx.Text)) then
        begin
          LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
          if (LStreamFlowReduction <> nil) then
          begin
            LStreamFlowReduction.SoilMoistureFileName := LFileNameStr;
            RePopulateDataViewer;
            ValidateSoilMoistureFileName(LStreamFlowReduction)
          end;
        end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.SetGridGraphBtnState;
const OPNAME = 'TStreamFlowReductionValidator.SetGridGraphBtnState';
var
  LStreamFlowReduction : TStreamFlowReduction;
  LFileName     : string;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    StreamFlowReductionDialog.UnitRunoffFileNameGridBtn.Enabled := False;
    StreamFlowReductionDialog.UnitRunoffFileNameGraphBtn.Enabled := False;
    StreamFlowReductionDialog.SoilMoistureFileNameGridBtn.Enabled := False;
    StreamFlowReductionDialog.SoilMoistureFileNameGraphBtn.Enabled := False;

    if (FIdentifier >= 0) then
    begin
      LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
      if (LStreamFlowReduction <> nil) then
      begin
        LFileName       := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath+LStreamFlowReduction.UnitRunoffFileName;
        LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames.FindFile(LFileName);
        if(LFileNameObject <> nil) then
        begin
          if LFileNameObject.SavedInDB then
          begin
            StreamFlowReductionDialog.UnitRunoffFileNameGridBtn.Enabled := True;
            StreamFlowReductionDialog.UnitRunoffFileNameGraphBtn.Enabled := True;
          end;
        end;
        LFileName       := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath+LStreamFlowReduction.SoilMoistureFileName;
        LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames.FindFile(LFileName);
        if(LFileNameObject <> nil) then
        begin
          if LFileNameObject.SavedInDB then
          begin
            StreamFlowReductionDialog.SoilMoistureFileNameGridBtn.Enabled := True;
            StreamFlowReductionDialog.SoilMoistureFileNameGraphBtn.Enabled := True;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.OnViewGraphClick(Sender: TObject);
const OPNAME = 'TStreamFlowReductionValidator.OnViewGraphClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRAPH');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      if(Sender = StreamFlowReductionDialog.UnitRunoffFileNameGraphBtn) then
        LContextDataList.Add('MODELELEMENTTYPE=SFRUNITRUNOFFFILE')
      else
        LContextDataList.Add('MODELELEMENTTYPE=SFRSOILMOISTURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStreamFlowReductionValidator.OnViewGridClick(Sender: TObject);
const OPNAME = 'TStreamFlowReductionValidator.OnViewGridClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRID');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      if(Sender = StreamFlowReductionDialog.UnitRunoffFileNameGridBtn) then
        LContextDataList.Add('MODELELEMENTTYPE=SFRUNITRUNOFFFILE')
      else
        LContextDataList.Add('MODELELEMENTTYPE=SFRSOILMOISTURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

