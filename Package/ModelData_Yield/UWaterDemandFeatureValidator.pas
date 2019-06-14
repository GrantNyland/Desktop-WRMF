{******************************************************************************}
{*  UNIT      : Contains the class TWaterDemandFeatureValidator.              *}
{*  AUTHOR    : Dziedzi Ramulondi                                             *}
{*  DATE      : 2004/10/20                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UWaterDemandFeatureValidator;

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
  UWaterDemandFeatureDialog;

type
  TWaterDemandFeatureValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure UpdateFeatureName;
    procedure UpdateWaterUseCategory;
    procedure UpdateProportionWaterUse (AIndex : integer;
                                        AValue : string);
    procedure ValidateFeatureName(AFeature   : IWaterDemandFeature);
    procedure ValidateFeatureType(AFeature   : IWaterDemandFeature);
    procedure ValidateWaterScenario(AFeature : IWaterDemandFeature);
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
    function WaterDemandFeatureDialog: TWaterDemandFeatureDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UConstants,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TWaterDemandFeatureValidator                                                 *}
{******************************************************************************}

procedure TWaterDemandFeatureValidator.CreateMemberObjects;
const OPNAME = 'TWaterDemandFeatureValidator.CreateMemberObjects';
var
  lPanel : TWaterDemandFeatureDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TWaterDemandFeatureDialog.Create(FPanelOwner,FAppModules);
    lPanel := WaterDemandFeatureDialog;
    with lPanel do
    begin
      FeatureNameEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('WaterDemandFeatureName');
      FeatureNameEdit.OnEnter       := OnEditControlEnter;
      FeatureNameEdit.OnExit        := OnEditControltExit;

      WaterUseCategoryComboBox.FieldProperty   := FAppModules.FieldProperties.FieldProperty('WaterDemandCategory');
      WaterUseCategoryComboBox.OnEnter         := OnEditControlEnter;
      WaterUseCategoryComboBox.OnExit          := OnEditControltExit;

      ProportionWaterUseGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ScenarioPortion'));
      ProportionWaterUseGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ScenarioPortion'));
      ProportionWaterUseGrid.OnBeforeCellChange       := OnStringGridCellDataHasChanged;
      ProportionWaterUseGrid.OnColEnter               := OnStringGridColEnter;
      ProportionWaterUseGrid.OnEnter                  := OnEditControlEnter;
      ProportionWaterUseGrid.OnExit                   := OnEditControltExit;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.DestroyMemberObjects;
const OPNAME = 'TWaterDemandFeatureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.Initialise: boolean;
const OPNAME = 'TWaterDemandFeatureValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.WaterDemandFeatureDialog : TWaterDemandFeatureDialog;
const OPNAME = 'TWaterDemandFeatureValidator.WaterDemandFeatureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TWaterDemandFeatureDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TWaterDemandFeatureValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.StudyHasChanged: boolean;
const OPNAME = 'TWaterDemandFeatureValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.LanguageHasChanged: boolean;
const OPNAME = 'TWaterDemandFeatureValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.WaterDemandFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.SaveState: boolean;
const OPNAME = 'TWaterDemandFeatureValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.ClearDataViewer;
const OPNAME = 'TWaterDemandFeatureValidator.ClearDataViewer';
var
  lPanel : TWaterDemandFeatureDialog;
  lRow   : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := WaterDemandFeatureDialog;
    with lPanel do
    begin
      FeatureNameEdit.Text := '';
      WaterUseCategoryComboBox.ItemIndex := -1;
      for lRow := 0 to ProportionWaterUseGrid.RowCount - 1 do
        ProportionWaterUseGrid.Cells[1, lRow] := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.PopulateDataViewer;
const OPNAME = 'TWaterDemandFeatureValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtWaterDemandWaterUseProportioning)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.RePopulateDataViewer;
const OPNAME = 'TWaterDemandFeatureValidator.RePopulateDataViewer';
var
  LCategory            : TStringList;
  LSenarioCount        : integer;
  LIndex               : integer;
  lWaterDemandFeature  : IWaterDemandFeature;
  LWaterDemandCategory : IWaterDemandCategory;
  LCategoryName        : string;
  lFieldProperty       : TAbstractFieldProperty;
  lKeyValues           : string;
  lFieldIndex          : string;
begin
  try
    lWaterDemandFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkFeaturesData.WaterDemandFeatureList.WaterDemandFeatureByID[FFeatureID];
    if (lWaterDemandFeature <> nil) then
    begin
      LCategory := TStringList.Create;
      try
        //LCategory.Sorted := True;
        LCategory.Duplicates := dupIgnore;
        for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkFeaturesData.WaterDemandConfiguration.DemandCategoryCount -1 do
        begin
          LCategory.Add(TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.WaterDemandConfiguration.DemandCategoryByIndex[LIndex].CategoryName)
        end;

        lFieldIndex    := '';
        lFieldProperty := WaterDemandFeatureDialog.FeatureNameEdit.FieldProperty;
        lKeyValues     := lWaterDemandFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        WaterDemandFeatureDialog.FeatureNameEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
        WaterDemandFeatureDialog.FeatureNameEdit.SetFieldValue(lWaterDemandFeature.FeatureName);

        WaterDemandFeatureDialog.WaterUseCategoryComboBox.Items.Assign(LCategory);
        if (lWaterDemandFeature.WaterDemandCategory <> 0) then
        begin
          LCategoryName := '';
          LWaterDemandCategory := TYieldModelDataObject(FAppModules.Model.ModelData).
                                  NetworkFeaturesData.WaterDemandConfiguration.
                                  DemandCategoryByID[lWaterDemandFeature.WaterDemandCategory];
          if (LWaterDemandCategory <> nil) then
            LCategoryName := LWaterDemandCategory.CategoryName;

          lFieldIndex    := '';
          lFieldProperty := WaterDemandFeatureDialog.WaterUseCategoryComboBox.FieldProperty;
          lKeyValues     := lWaterDemandFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          WaterDemandFeatureDialog.WaterUseCategoryComboBox.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          WaterDemandFeatureDialog.WaterUseCategoryComboBox.ItemIndex :=
            WaterDemandFeatureDialog.WaterUseCategoryComboBox.Items.IndexOf(LCategoryName);
        end;
        LSenarioCount := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.WaterDemandFeatureList.ScenarioCount;
        if (LSenarioCount = NullInteger) then
          WaterDemandFeatureDialog.SetSenarioCount(0)
        else
          WaterDemandFeatureDialog.SetSenarioCount(LSenarioCount);

        for LIndex := 0 to LSenarioCount -1 do
        begin
          lFieldIndex    := IntToStr(LIndex + 1);
          lFieldProperty := WaterDemandFeatureDialog.ProportionWaterUseGrid.FieldProperty(1);
          lKeyValues     := lWaterDemandFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          WaterDemandFeatureDialog.ProportionWaterUseGrid.HasMetaData[1, lIndex] :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          if(lWaterDemandFeature.ScenarioPortionByIndex[LIndex+1] = NullFloat) then
            WaterDemandFeatureDialog.ProportionWaterUseGrid.Cells[1,LIndex] := '0.0'
          else
            WaterDemandFeatureDialog.ProportionWaterUseGrid.Cells[1,LIndex] :=
              Trim(Format(lFieldProperty.FormatStringGrid,
                [lWaterDemandFeature.ScenarioPortionByIndex[LIndex+1]]));
        end;
      finally
        LCategory.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWaterDemandFeatureValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TWaterDemandFeatureValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TWaterDemandFeatureValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with WaterDemandFeatureDialog do
    begin
      if ((Sender = WaterUseCategoryComboBox) AND (WaterUseCategoryComboBox.HasValueChanged)) then
        UpdateWaterUseCategory
      else
      if ((Sender = FeatureNameEdit) AND (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.UpdateFeatureName;
const OPNAME = 'TWaterDemandFeatureValidator.UpdateFeatureName';
var
  lFeature : IWaterDemandFeature;
  lMessage : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.WaterDemandFeatureList.WaterDemandFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with WaterDemandFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FeatureNameEdit.FieldProperty.FieldName,
            FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
//          DoContextValidation(dvtLossFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.UpdateWaterUseCategory;
const OPNAME = 'TWaterDemandFeatureValidator.UpdateWaterUseCategory';
var
  LWaterDemandFeature  : IWaterDemandFeature;
  LWaterDemandCategory : IWaterDemandCategory;
  lMessage     : string;
begin
  try
    LWaterDemandFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.WaterDemandFeatureList.WaterDemandFeatureByID[FFeatureID];
    if (LWaterDemandFeature <> nil) then
    begin
      with WaterDemandFeatureDialog do
      begin
        LWaterDemandCategory := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkFeaturesData.WaterDemandConfiguration.
                                DemandCategoryByName[WaterUseCategoryComboBox.Text];
        if(LWaterDemandCategory <> nil)  and
          (LWaterDemandCategory.CategoryID <> LWaterDemandFeature.WaterDemandCategory)then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              WaterUseCategoryComboBox.FieldProperty.FieldName,
              IntToStr(LWaterDemandCategory.CategoryID),lMessage)) then
          begin
            WaterUseCategoryComboBox.ValidationError := lMessage;
            LWaterDemandFeature.WaterDemandCategory := LWaterDemandCategory.CategoryID;
            RePopulateDataViewer;
            //DoContextValidation(dvtDivFeatureName);
          end
          else
            WaterUseCategoryComboBox.ValidationError := lMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.UpdateProportionWaterUse(AIndex: integer; AValue: string);
const OPNAME = 'TWaterDemandFeatureValidator.UpdateProportionWaterUse';
var
  LWaterDemandFeature  : IWaterDemandFeature;
  lMessage     : string;
begin
  try
    LWaterDemandFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.WaterDemandFeatureList.WaterDemandFeatureByID[FFeatureID];
    if (LWaterDemandFeature <> nil) then
    begin
      if (FAppModules.FieldProperties.ValidateFieldProperty('ScenarioPortion',AValue,lMessage,AIndex)) then
      begin
        WaterDemandFeatureDialog.WaterUseCategoryComboBox.ValidationError := lMessage;
        LWaterDemandFeature.ScenarioPortionByIndex[AIndex] := StrToFloat(AValue);
        //RePopulateDataViewer;
        DoContextValidation(dvtWaterDemandScenarioPortion);
      end
      else
        WaterDemandFeatureDialog.WaterUseCategoryComboBox.ValidationError := lMessage;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TWaterDemandFeatureValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with WaterDemandFeatureDialog do
    begin
      if ((ProportionWaterUseGrid = ASender) AND (ACol = 1)) then
        UpdateProportionWaterUse(ARow+1, Trim(ProportionWaterUseGrid.Cells[ACol, ARow]))
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TWaterDemandFeatureValidator.DoContextValidation';
var
  lFeature     : IWaterDemandFeature;
  lFeatureList : IWaterDemandFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID > 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.WaterDemandFeatureList;
      lFeature := lFeatureList.WaterDemandFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtWaterDemandWaterUseProportioning,
                                dvtWaterDemandScenarioPortion]) then
          ValidateWaterScenario(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TWaterDemandFeatureValidator.DetermineWizardStatus';
{var
  lFeature         : IWaterDemandFeature;
  lFeatureList     : IWaterDemandFeatureList;
  lLossFeature     : ILossFeature;
  lLossFeatureList : ILossFeatureList;
  lFieldProperty   : TAbstractFieldProperty;
  lNotZero         : Boolean;
  lIndex           : integer;
  lLevel           : integer;
}begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    {if (FFeatureID >= 0) then
    begin
      if (FLossType1) then
      begin
        lLossFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkFeaturesData.LossFeatureList;
        lLossFeature     := lLossFeatureList.LossFeatureByID[FFeatureID];
        if (lLossFeature <> nil) then
        begin
          DoContextValidation(dvtLossFeature);
          lFieldProperty := FAppModules.FieldProperties.FieldProperty('WaterLoss');
          lNotZero := FALSE;
          lIndex   := lFieldProperty.ArrayLow;
          while ((NOT lNotZero) AND (lIndex <= lFieldProperty.ArrayHigh)) do
          begin
            if ((lLossFeature.WaterLossByMonth[lIndex] > 0) OR
                (lLossFeature.DivertedFlowByMonth[lIndex] > 0)) then
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
      end
      else
      begin
        lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkFeaturesData.WaterDemandFeatureList;
        lFeature     := lFeatureList.WaterDemandFeatureByID[FFeatureID];
        if (lFeature <> nil) then
        begin
          DoContextValidation(dvtDivFeature);
          lNotZero := FALSE;
          case lFeature.DiversionType of
          1, 2 :
            begin
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
              lIndex   := lFieldProperty.ArrayLow;
              while ((NOT lNotZero) AND (lIndex <= lFieldProperty.ArrayHigh)) do
              begin
                if ((lFeature.DiversionDemandByIndex[lIndex] > 0) OR
                    (lFeature.DivertedFlowByIndex[lIndex] > 0)) then
                  lNotZero := TRUE
                else
                  lIndex := lIndex + 1;
              end;
            end;
          3 :
            begin
              lIndex   := 1;
              while ((NOT lNotZero) AND (lIndex <= lFeature.ReferenceFlowsCount)) do
              begin
                lLevel := 1;
                while ((NOT lNotZero) AND (lLevel <= lFeature.ReservoirElevationsCount)) do
                begin
                  if (lFeature.DivertedFlowProportion[lIndex, lLevel] > 0) then
                    lNotZero := TRUE
                  else
                    lLevel := lLevel + 1;
                end;
                lIndex := lIndex + 1;
              end;
            end;
          else
          end;
          if (lNotZero) then
          begin
            Result := 1;
            if (FAllErrorMessages.Count = 0) then
              Result := 2;
          end;
        end;
      end;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.ValidateFeatureName(AFeature: IWaterDemandFeature);
const OPNAME = 'TWaterDemandFeatureValidator.ValidateFeatureName';
begin
  try
    {with WaterDemandFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'FeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      DivFeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.ValidateFeatureType(AFeature: IWaterDemandFeature);
const OPNAME = 'TWaterDemandFeatureValidator.ValidateFeatureType';
begin
  try
    {with WaterDemandFeatureDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'DiversionType')) then
        DivTypeRadioGroup.InValidationError := FALSE
      else
      begin
        DivTypeRadioGroup.InValidationError := TRUE;
        DivTypeRadioGroup.ValidationError := FErrorMessage;
        DivTypeRadioGroup.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterDemandFeatureValidator.ValidateWaterScenario(AFeature: IWaterDemandFeature);
const OPNAME = 'TWaterDemandFeatureValidator.ValidateWaterScenario';
begin
  try
    if (AFeature <> nil) then
    begin
      with WaterDemandFeatureDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'ScenarioPortion')) then
          ProportionWaterUseGrid.ValidationError[1, 0, gveColContext] := ''
        else
        begin
          ProportionWaterUseGrid.ValidationError[1, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterDemandFeatureValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TWaterDemandFeatureValidator.ProcessMetaDataEvent';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IWaterDemandFeature;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.WaterDemandFeatureList.WaterDemandFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with WaterDemandFeatureDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = WaterUseCategoryComboBox) then
            lFieldProperty := WaterUseCategoryComboBox.FieldProperty
          else
          if (FActiveControl = ProportionWaterUseGrid) then
          begin
            lFieldIndex := IntToStr(ProportionWaterUseGrid.Row+1);
            lFieldProperty := ProportionWaterUseGrid.FieldProperty(1);
          end;

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

end.

