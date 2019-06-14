{******************************************************************************}
{*  UNIT      : Contains the class TReconciliationAnalysisValidator.          *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/03                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UReconciliationAnalysisValidator;

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
  UReconciliationAnalysisDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,

  UAbstractModelData,
  UFilesActionAbstractManager,
  UFileNames,
  UAbstractFileNamesObject,
  UFilesActionYieldManager,
  UDataFileObjects;
type

  TReconciliationAnalysisValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FSelectedCategoryIndex : integer;
    FSelectedCategoryID    : integer;
    FValidateCategoryIndex : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnReconciliationAnalysisClick(Sender: TObject);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure OnReconciliationGridSelectCell(ASender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure UpdateReconciliationAnalysisCheck;
    procedure RePopulateDataViewer;
    procedure RepopulateGrids;
    procedure RepopulateChannelListBox;
    procedure UpdateNrOfRiskCriteria;
    procedure UpdateNrOfCategories;
    procedure UpdateRecurrenceInterval (AIndex : integer; AValue : string);
    procedure UpdateDemandPortion (AIndex : integer; ACol : integer; AValue : string);
    procedure UpdateCategoryName (AIndex : integer; AValue : string);
    procedure ValidateCategoryCount (AConfigData : IWaterDemandConfiguration);
    procedure ValidateRiskCriteriaCount (AConfigData : IWaterDemandConfiguration);
    procedure ValidateRecurrenceIntervals (AConfigData : IWaterDemandConfiguration);
    procedure ValidateDemandCategories (AConfigData : IWaterDemandConfiguration);
    procedure ValidateCategoryName (ACategory : IWaterDemandCategory);
    procedure ValidateDemandPortion (ACategory : IWaterDemandCategory);
    procedure ValidateDemandPortionTotal (ACategory : IWaterDemandCategory);
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
    function ReconciliationAnalysisDialog: TReconciliationAnalysisDialog;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog, UNetworkElementData, Math;

{******************************************************************************}
{* TReconciliationAnalysisValidator                                           *}
{******************************************************************************}

procedure TReconciliationAnalysisValidator.CreateMemberObjects;
const OPNAME = 'TReconciliationAnalysisValidator.CreateMemberObjects';
var
  lPanel     : TReconciliationAnalysisDialog;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TReconciliationAnalysisDialog.Create(FPanelOwner,FAppModules);
    lPanel := ReconciliationAnalysisDialog;
    with lPanel do
    begin
      ReconciliationAnalysisCheck.FieldProperty  := FAppModules.FieldProperties.FieldProperty('WaterDemandFileCreate');
      ReconciliationAnalysisCheck.OnEnter        := OnEditControlEnter;
      ReconciliationAnalysisCheck.OnClick        := OnReconciliationAnalysisClick;

      NrOfCategoriesEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('WaterDemandCategoryCount');
      NrOfCategoriesEdit.OnEnter          := OnEditControlEnter;
      NrOfCategoriesEdit.OnExit           := OnEditControltExit;

      NrOfAssurancesEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('WaterDemandRiskCriteriaCount');
      NrOfAssurancesEdit.OnEnter          := OnEditControlEnter;
      NrOfAssurancesEdit.OnExit           := OnEditControltExit;

      AssurancesGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      AssurancesGrid.OnColEnter           := OnStringGridColEnter;
      AssurancesGrid.OnExit               := OnEditControltExit;
      AssurancesGrid.OnEnter              := OnEditControlEnter;

      ReconciliationGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      ReconciliationGrid.OnSelectCell       := OnReconciliationGridSelectCell;
      ReconciliationGrid.OnColEnter         := OnStringGridColEnter;
      ReconciliationGrid.OnExit             := OnEditControltExit;
      ReconciliationGrid.OnEnter            := OnEditControlEnter;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.DestroyMemberObjects;
const OPNAME = 'TReconciliationAnalysisValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisValidator.Initialise: boolean;
const OPNAME = 'TReconciliationAnalysisValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FSelectedCategoryID    := 0;
    FSelectedCategoryIndex := 0;
    FValidateCategoryIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReconciliationAnalysisValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.WaterUserAssurance');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ClearDataViewer;
const OPNAME = 'TReconciliationAnalysisValidator.ClearDataViewer';
var
  lpPanel : TReconciliationAnalysisDialog;
  lRow    : integer;
  lCol    : integer;
begin
  inherited ClearDataViewer;
  try
    lpPanel := ReconciliationAnalysisDialog;
    with lpPanel do
    begin
      NrOfCategoriesEdit.Text     := '-1';
      NrOfAssurancesEdit.Text     := '-1';
      for lCol := 0 to AssurancesGrid.ColCount - 1 do
        AssurancesGrid.Cells[lCol, 0] := '-1';
      for lRow := 1 to ReconciliationGrid.RowCount-1 do
      begin
        ReconciliationGrid.Cells[1, lRow] := '';
        for lCol := 2 to ReconciliationGrid.ColCount - 1 do
          ReconciliationGrid.Cells[lCol, lRow] := '-1';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.PopulateDataViewer;
const OPNAME = 'TReconciliationAnalysisValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtWaterDemandReconciliationAnalysis);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.RePopulateDataViewer;
const OPNAME = 'TReconciliationAnalysisValidator.RePopulateDataViewer';
var
  lConfigData             : IWaterDemandConfiguration;
  lFieldProperty          : TAbstractFieldProperty;
  lFieldIndex             : string;
  lKeyValues              : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     WaterDemandConfiguration;
    if (lConfigData <> nil) then
    begin
      with ReconciliationAnalysisDialog do
      begin
        lFieldProperty := NrOfCategoriesEdit.FieldProperty;
        lFieldIndex    := '';
        lKeyValues     := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        NrOfCategoriesEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
        NrOfCategoriesEdit.SetFieldValue(IntToStr(lConfigData.DemandCategoryCount));

        lFieldProperty := NrOfAssurancesEdit.FieldProperty;
        NrOfAssurancesEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
        NrOfAssurancesEdit.SetFieldValue(IntToStr(lConfigData.RiskCriteriaCount));

        RepopulateGrids;

         ReconciliationAnalysisCheck.Enabled := lConfigData.DemandCategoryCount > 0;

        if lConfigData.ImplementReconciliation then
           ReconciliationAnalysisCheck.Checked := True
        else
          ReconciliationAnalysisCheck.Checked := False;
     end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.RepopulateGrids;
const OPNAME = 'TReconciliationAnalysisValidator.RepopulateGrids';
var
  lConfigData       : IWaterDemandConfiguration;
  lNrOfCategories   : integer;
  lNrOfRiskCriteria : integer;
  lIndex            : integer;
  lRow              : integer;
  lCategory         : IWaterDemandCategory;
  lFieldProperty    : TAbstractFieldProperty;
  lFieldIndex       : string;
  lKeyValues        : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     WaterDemandConfiguration;
    if (lConfigData <> nil) then
    begin
      with ReconciliationAnalysisDialog do
      begin
        lNrOfCategories   := lConfigData.DemandCategoryCount;
        lNrOfRiskCriteria := lConfigData.RiskCriteriaCount;

        AssurancesGrid.ColCount := lNrOfRiskCriteria;
        AssurancesGrid.Width    := AssurancesGrid.ColCount * (AssurancesGrid.DefaultColWidth + 1) + 3;
        AssurancesGrid.ClearFieldProperties;
        ReconciliationGrid.ClearFieldProperties;
        lFieldProperty := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
        ReconciliationGrid.ColCount := 6;
        ReconciliationGrid.ColCount := Max((ReconciliationGrid.FixedCols + 1),(3 + lNrOfRiskCriteria));
        ReconciliationGrid.RowCount := Max((ReconciliationGrid.FixedRows + 1),(1 + lNrOfCategories));


        for lIndex := 1 to lNrOfRiskCriteria do
        begin
          AssurancesGrid.AddFieldProperty(lFieldProperty);
          lFieldIndex := IntToStr(lIndex);
          lKeyValues  := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          AssurancesGrid.HasMetaData[lIndex-1, 0] :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          if (lConfigData.RecurrenceIntervalByIndex[lIndex] = NullFloat) then
            AssurancesGrid.Cells[lIndex-1, 0] := ''
          else
            AssurancesGrid.Cells[lIndex-1, 0] := FloatToStr(lConfigData.RecurrenceIntervalByIndex[lIndex]);
        end;

//========================
        if (lNrOfRiskCriteria > 0) then
          AssurancesGrid.Options := AssurancesGrid.Options + [goEditing	]
        else
          AssurancesGrid.Options := AssurancesGrid.Options - [goEditing	];
        if (lNrOfCategories > 0) and ( lNrOfCategories <> ReconciliationGrid.ColCount - 1 ) then
          ReconciliationGrid.Options := ReconciliationGrid.Options + [goEditing	]
        else
          ReconciliationGrid.Options := ReconciliationGrid.Options - [goEditing	];

        lFieldProperty := FAppModules.FieldProperties.FieldProperty('WaterDemandCategoryName');
        ReconciliationGrid.AddFieldProperty(lFieldProperty);
        ReconciliationGrid.AddFieldProperty(lFieldProperty);
        for lRow := 1 to lNrOfCategories do
        begin
          lCategory := lConfigData.DemandCategoryByIndex[lRow-1];
          ReconciliationGrid.Cells[0, lRow] := IntToStr(lRow);

          lFieldIndex := '';
          lKeyValues  := lCategory.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          ReconciliationGrid.HasMetaData[1, lRow] :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          ReconciliationGrid.Cells[1, lRow] := lCategory.CategoryName;
        end;

        lFieldProperty := FAppModules.FieldProperties.FieldProperty('DemandPortion');
        for lIndex := 2 to ReconciliationGrid.ColCount - 2 do
        begin
          ReconciliationGrid.AddFieldProperty(lFieldProperty);
          ReconciliationGrid.IsColumnEnabled [ LIndex ] := True;
          ReconciliationGrid.Cells[lIndex,0] := '1:' + FloatToStr(lConfigData.RecurrenceIntervalByIndex[lIndex-1]);
        end;

        for lRow := 1 to lNrOfCategories do
        begin
          lCategory  := lConfigData.DemandCategoryByIndex[lRow-1];
          for lIndex := 1 to lNrOfRiskCriteria do
          begin
            lFieldIndex := IntToStr(lIndex);
            lKeyValues  := lCategory.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            ReconciliationGrid.HasMetaData[lIndex+1, lRow] :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
            ReconciliationGrid.Cells[lIndex+1, lRow] :=
              Trim(Format(lFieldProperty.FormatStringGrid,[lCategory.DemandPortionByIndex[lIndex]]));
          end;

        end;
        //========================

        for lRow := 1 to lNrOfCategories do
          ReconciliationGrid.Cells [ ReconciliationGrid.ColCount - 1, lRow ] := FloatToStr ( lCategory.PortionTotal [ lRow ] );

       lFieldProperty := FAppModules.FieldProperties.FieldProperty('DemandPortionTotal');
       ReconciliationGrid.AddFieldProperty(lFieldProperty);
       ReconciliationGrid.IsColumnEnabled [ ReconciliationGrid.ColCount - 1 ] := False;


        if ((FSelectedCategoryIndex = 0) AND (lNrOfCategories > 0)) then
        begin
          FSelectedCategoryIndex := 0;
          FSelectedCategoryID    := lConfigData.DemandCategoryByIndex[0].CategoryID;
        end;
        ReconciliationGrid.Cells[ReconciliationGrid.ColCount - 1, 0 ] := FAppModules.Language.GetString('GridHeading.Total');
        RepopulateChannelListBox;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.OnReconciliationGridSelectCell(ASender: TObject;
                                                                          ACol, ARow: Longint;
                                                                          var CanSelect: Boolean);
const OPNAME = 'TReconciliationAnalysisValidator.OnReconciliationGridSelectCell';
var
  lConfigData : IWaterDemandConfiguration;
begin
  try
    CanSelect := TRUE;
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     WaterDemandConfiguration;
    with ReconciliationAnalysisDialog do
    begin
      if (ARow <> FSelectedCategoryIndex+1) then
      begin
        FSelectedCategoryIndex := ARow - 1;
        FSelectedCategoryID    := lConfigData.DemandCategoryByIndex[FSelectedCategoryIndex].CategoryID;
        RepopulateChannelListBox;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.RepopulateChannelListBox;
const OPNAME = 'TReconciliationAnalysisValidator.RepopulateChannelListBox';
var
  lChannelList : IChannelList;
  lIndex       : integer;
  lChannel     : IGeneralFlowChannel;
begin
  try
    with ReconciliationAnalysisDialog do
    begin
      ChannelsListBox.Items.Clear;
      if (FSelectedCategoryID > 0) then
      begin
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
        for lIndex := 0 to lChannelList.ChannelCount - 1 do
        begin
          lChannel := lChannelList.ChannelByIndex[lIndex];
          if ((lChannel.WaterDemandFeature <> nil) AND
              (lChannel.WaterDemandFeature.WaterDemandCategory = FSelectedCategoryID)) then
            ChannelsListBox.Items.Add('(' + IntToStr(lChannel.ChannelNumber) + ') ' +
                                      lChannel.ChannelName);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.UpdateNrOfRiskCriteria;
const OPNAME = 'TReconciliationAnalysisValidator.UpdateNrOfRiskCriteria';
var
  lConfigData : IWaterDemandConfiguration;
  lMessage    : string;
begin
  try
    with ReconciliationAnalysisDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.WaterDemandConfiguration;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('WaterDemandRiskCriteriaCount',
            NrOfAssurancesEdit.Text, lMessage)) then
        begin
          NrOfAssurancesEdit.FieldValidationError := '';
          lConfigData.RiskCriteriaCount := StrToInt(NrOfAssurancesEdit.Text);
          NrOfAssurancesEdit.SetFieldValue(IntToStr(lConfigData.RiskCriteriaCount));
          DoContextValidation(dvtWaterDemandRiskCriteriaCount);
          PopulateDataViewer
        end
        else
          NrOfAssurancesEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.UpdateNrOfCategories;
const OPNAME = 'TReconciliationAnalysisValidator.UpdateNrOfCategories';
var
  lConfigData : IWaterDemandConfiguration;
  lCategory   : IWaterDemandCategory;
  lMessage    : string;
  lOldCount   : integer;
  lNewCount   : integer;
  lIndex      : integer;
begin
  try
    with ReconciliationAnalysisDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.WaterDemandConfiguration;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('WaterDemandCategoryCount',
            NrOfCategoriesEdit.Text, lMessage)) then
        begin
          NrOfCategoriesEdit.FieldValidationError := '';
          lOldCount := lConfigData.DemandCategoryCount;
          lNewCount := StrToInt(Trim(NrOfCategoriesEdit.Text));
          if (lNewCount > lOldCount) then
          begin
            for lIndex := lOldCount + 1 to lNewCount do
              lCategory := lConfigData.CreateWaterDemandCategory;
          end
          else
          begin
            for lIndex := lNewCount + 1 to lOldCount do
              lConfigData.RemoveWaterDemandCategoryWithID(lIndex);
          end;
          NrOfCategoriesEdit.SetFieldValue(IntToStr(lConfigData.DemandCategoryCount));
          if (lNewCount > 0) then
          begin
            lConfigData.UpdateWaterUseOutputProportions(lNewCount);
            FAppModules.Model.StudyDataHasChanged ( sdccEdit, 'WaterDemandCategoryCount', InttoStr(lOldCount), IntToStr(lNewCount) );
          end;
          DoContextValidation(dvtWaterDemandCategoryCount);
          PopulateDataViewer;
        end
        else
          NrOfCategoriesEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.UpdateRecurrenceInterval
                                                           (AIndex : integer;
                                                            AValue : string);
const OPNAME = 'TReconciliationAnalysisValidator.UpdateRecurrenceInterval';
var
  lConfigData : IWaterDemandConfiguration;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.WaterDemandConfiguration;
    if (lConfigData <> nil) then
    begin
      with ReconciliationAnalysisDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('RecurrenceInterval', AValue,
            lMessage, AIndex)) then
        begin
          AssurancesGrid.ValidationError[AIndex-1, 0, gveCellField] := '';
          lValue := StrToFloat(Trim(AValue));
          lConfigData.RecurrenceIntervalByIndex[AIndex] := lValue;
          DoContextValidation(dvtWaterDemandRecurrenceInterval);
          PopulateDataViewer;
        end
        else
          AssurancesGrid.ValidationError[AIndex-1, 0, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.UpdateCategoryName
                                                           (AIndex : integer;
                                                            AValue : string);
const OPNAME = 'TReconciliationAnalysisValidator.UpdateCategoryName';
var
  lConfigData : IWaterDemandConfiguration;
  lCategory   : IWaterDemandCategory;
  lMessage    : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.WaterDemandConfiguration;
    if (lConfigData <> nil) then
    begin
      lCategory := lConfigData.DemandCategoryByIndex[AIndex];
      if (lCategory <> nil) then
      begin
        with ReconciliationAnalysisDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('WaterDemandCategoryName', AValue,
              lMessage)) then
          begin
            ReconciliationGrid.ValidationError[1, AIndex+1, gveCellField] := '';
            lCategory.CategoryName := AValue;
            FValidateCategoryIndex := AIndex;
            DoContextValidation(dvtWaterDemandCategoryName);
            FValidateCategoryIndex := 0;
            PopulateDataViewer;
          end
          else
            ReconciliationGrid.ValidationError[1, AIndex+1, gveCellField] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.UpdateDemandPortion
                                                           (AIndex : integer;
                                                            ACol   : integer;
                                                            AValue : string);
const OPNAME = 'TReconciliationAnalysisValidator.UpdateDemandPortion';
var
  lConfigData : IWaterDemandConfiguration;
  lCategory   : IWaterDemandCategory;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.WaterDemandConfiguration;
    if (lConfigData <> nil) then
    begin
      lCategory := lConfigData.DemandCategoryByIndex[AIndex];
      if (lCategory <> nil) then
      begin
        with ReconciliationAnalysisDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('DemandPortion', AValue,
              lMessage, ACol)) then
          begin
            ReconciliationGrid.ValidationError[ACol+1, AIndex+1, gveCellField] := '';
            lValue := StrToFloat(Trim(AValue));
            lCategory.DemandPortionByIndex[ACol] := lValue;
            FValidateCategoryIndex := AIndex;
            DoContextValidation(dvtWaterDemandCategoryPortion);
            FValidateCategoryIndex := 0;
            PopulateDataViewer;
          end
          else
            ReconciliationGrid.ValidationError[ACol+1, AIndex+1, gveCellField] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisValidator.SaveState: boolean;
const OPNAME = 'TReconciliationAnalysisValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisValidator.ReconciliationAnalysisDialog : TReconciliationAnalysisDialog;
const OPNAME = 'TReconciliationAnalysisValidator.ReconciliationAnalysisDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TReconciliationAnalysisDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReconciliationAnalysisValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisValidator.StudyHasChanged: boolean;
const OPNAME = 'TReconciliationAnalysisValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReconciliationAnalysisValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TReconciliationAnalysisValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ReconciliationAnalysisDialog do
    begin
      if ((Sender = NrOfAssurancesEdit) AND NrOfAssurancesEdit.HasValueChanged) then
        UpdateNrOfRiskCriteria
      else
      if ((Sender = NrOfCategoriesEdit) AND NrOfCategoriesEdit.HasValueChanged) then
        UpdateNrOfCategories;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.OnReconciliationAnalysisClick(Sender: TObject);
const OPNAME = 'TReconciliationAnalysisValidator.OnReconciliationAnalysisClick';
begin
  try
    if(ReconciliationAnalysisDialog.ReconciliationAnalysisCheck.HasValueChanged) then
      UpdateReconciliationAnalysisCheck;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReconciliationAnalysisValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with ReconciliationAnalysisDialog do
    begin
      if (ASender = AssurancesGrid) then
        UpdateRecurrenceInterval(ACol+1, Trim(AssurancesGrid.Cells[ACol, ARow]))
      else
      if ((ASender = ReconciliationGrid) AND (ACol = 1)) then
        UpdateCategoryName(ARow-1, Trim(ReconciliationGrid.Cells[ACol, ARow]))
      else
      if ((ASender = ReconciliationGrid) AND (ACol > 1) and ( ACol <> ( ReconciliationGrid.ColCount - 1 ) ) ) then
        UpdateDemandPortion(ARow-1, ACol-1, Trim(ReconciliationGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TReconciliationAnalysisValidator.DoContextValidation';
var
  lConfigData : IWaterDemandConfiguration;
  lCategory   : IWaterDemandCategory;
begin
  try
    FAllErrorMessages.Clear;
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     WaterDemandConfiguration;
    if (lConfigData <> nil) then
    begin
      if (FValidateCategoryIndex >= 0) then
        lCategory := lConfigData.DemandCategoryByIndex[FValidateCategoryIndex]
      else
        lCategory := nil;
      if (AValidationType in [dvtWaterDemandReconciliationAnalysis,
                              dvtWaterDemandCategoryCount]) then
        ValidateCategoryCount(lConfigData);
      if (AValidationType in [dvtWaterDemandReconciliationAnalysis,
                              dvtWaterDemandRiskCriteriaCount]) then
        ValidateRiskCriteriaCount(lConfigData);
      if (AValidationType in [dvtWaterDemandReconciliationAnalysis,
                              dvtWaterDemandRecurrenceInterval]) then
        ValidateRecurrenceIntervals(lConfigData);
      if (AValidationType in [dvtWaterDemandReconciliationAnalysis,
                              dvtWaterDemandCategories]) then
        ValidateDemandCategories(lConfigData);
      if (AValidationType in [dvtWaterDemandCategoryName]) then
        ValidateCategoryName(lCategory);
      if (AValidationType in [dvtWaterDemandReconciliationAnalysis,
                              dvtWaterDemandCategoryPortion]) then
      begin
        ValidateDemandPortion(lCategory);
        ValidateDemandPortionTotal(lCategory);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReconciliationAnalysisValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TReconciliationAnalysisValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ValidateCategoryCount
                                             (AConfigData : IWaterDemandConfiguration);
const OPNAME = 'TReconciliationAnalysisValidator.ValidateCategoryCount';
begin
  try
    with ReconciliationAnalysisDialog do
    begin
      FErrorMessage := '';
      if (NOT AConfigData.Validate(FErrorMessage, 'CategoryCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfCategoriesEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ValidateRiskCriteriaCount
                                             (AConfigData : IWaterDemandConfiguration);
const OPNAME = 'TReconciliationAnalysisValidator.ValidateRiskCriteriaCount';
begin
  try
    with ReconciliationAnalysisDialog do
    begin
      FErrorMessage := '';
      if (NOT AConfigData.Validate(FErrorMessage, 'RiskCriteriaCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfAssurancesEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ValidateRecurrenceIntervals
                                             (AConfigData : IWaterDemandConfiguration);
const OPNAME = 'TReconciliationAnalysisValidator.ValidateRecurrenceIntervals';
var
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
  lIndex     : integer;
  lCount     : integer;
begin
  try
    with ReconciliationAnalysisDialog do
    begin
      lErrorCols := TStringList.Create;
      lErrorMsgs := TStringList.Create;
      try
        lErrorCols.Clear;
        FErrorMessage := '';
        if (AConfigData.Validate(FErrorMessage, 'RecurrenceIntervals')) then
        begin
          for lCount := 1 to AConfigData.RiskCriteriaCount do
            AssurancesGrid.ValidationError[lCount-1, 0, gveColContext] := ''
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
          for lCount := 1 to AConfigData.RiskCriteriaCount do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCount));
            if (lIndex >= 0) then
              AssurancesGrid.ValidationError[lCount-1, 0, gveColContext] := lErrorMsgs.Strings[lIndex]
            else
              AssurancesGrid.ValidationError[lCount-1, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrorMsgs);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMsgs);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ValidateDemandCategories
                                             (AConfigData : IWaterDemandConfiguration);
const OPNAME = 'TReconciliationAnalysisValidator.ValidateDemandCategories';
var
  lIndex    : integer;
  lCategory : IWaterDemandCategory;
begin
  try
    with ReconciliationAnalysisDialog do
    begin
      FErrorMessage := '';
      for lIndex := 0 to AConfigData.DemandCategoryCount - 1 do
      begin
        lCategory := AConfigData.DemandCategoryByIndex[lIndex];
        FValidateCategoryIndex := lIndex;
        ValidateCategoryName(lCategory);
        ValidateDemandPortion(lCategory);
        ValidateDemandPortionTotal(lCategory);
        FValidateCategoryIndex := 0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ValidateCategoryName
                                             (ACategory : IWaterDemandCategory);
const OPNAME = 'TReconciliationAnalysisValidator.ValidateCategoryName';
begin
  try
    if (ACategory <> nil) then
    begin
      with ReconciliationAnalysisDialog do
      begin
        FErrorMessage := '';
        if (ACategory.Validate(FErrorMessage, 'CategoryName')) then
          ReconciliationGrid.ValidationError[1, FValidateCategoryIndex+1, gveCellContext] := ''
        else
        begin
          ReconciliationGrid.ValidationError[1, FValidateCategoryIndex+1, gveCellContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ValidateDemandPortion
                                             (ACategory : IWaterDemandCategory);
const OPNAME = 'TReconciliationAnalysisValidator.ValidateDemandPortion';
var
  lErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
  lConfigData : IWaterDemandConfiguration;
begin
  try
    if (ACategory <> nil) then
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                       WaterDemandConfiguration;
      with ReconciliationAnalysisDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (ACategory.Validate(FErrorMessage, 'DemandPortion')) then
          begin
            for lCol := 1 to lConfigData.RiskCriteriaCount do
              ReconciliationGrid.ValidationError[lCol+1, FValidateCategoryIndex+1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 1 to lConfigData.RiskCriteriaCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                ReconciliationGrid.ValidationError[lCol+1, FValidateCategoryIndex+1, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                ReconciliationGrid.ValidationError[lCol+1, FValidateCategoryIndex+1, gveCellContext] := ''
            end;
            FAllErrorMessages.AddStrings(lErrorMsgs);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReconciliationAnalysisValidator.ValidateDemandPortionTotal ( ACategory : IWaterDemandCategory );
const OPNAME = 'TReconciliationAnalysisValidator.ValidateDemandPortionTotal';
begin
  try
    if (ACategory <> nil) then
    begin
      with ReconciliationAnalysisDialog do
      begin
          FErrorMessage := '';
          if (ACategory.Validate(FErrorMessage, 'DemandPortionTotal')) then
              ReconciliationGrid.ValidationError[ ReconciliationGrid.ColCount -1, FValidateCategoryIndex+1, gveCellContext] := ''
          else
              ReconciliationGrid.ValidationError[ ReconciliationGrid.ColCount -1, FValidateCategoryIndex+1, gveCellContext] := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReconciliationAnalysisValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TReconciliationAnalysisValidator.ProcessMetaDataEvent';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lConfigData    : IWaterDemandConfiguration;
  lCategory      : IWaterDemandCategory;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                       WaterDemandConfiguration;
      if (lConfigData <> nil) then
      begin
        with ReconciliationAnalysisDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = NrOfCategoriesEdit) then
          begin
            lFieldProperty := NrOfCategoriesEdit.FieldProperty;
            lKeyValues     := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          end
          else
          if (FActiveControl = NrOfAssurancesEdit) then
          begin
            lFieldProperty := NrOfAssurancesEdit.FieldProperty;
            lKeyValues     := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          end
          else
          if (FActiveControl = AssurancesGrid) then
          begin
            lFieldIndex    := IntToStr(AssurancesGrid.Col+1);
            lFieldProperty := AssurancesGrid.FieldProperty(1);
            if lFieldProperty <> nil then
              lKeyValues     := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          end
          else
          if (FActiveControl = ReconciliationGrid) then
          begin
            lFieldProperty := ReconciliationGrid.FieldProperty(ReconciliationGrid.Col);
            if (ReconciliationGrid.Col <= 1) then
              lFieldIndex  := ''
            else
              lFieldIndex  := IntToStr(ReconciliationGrid.Col-1);
            lCategory      := lConfigData.DemandCategoryByIndex[ReconciliationGrid.Row-1];
            lKeyValues     := lCategory.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          end;

          if (lFieldProperty <> nil) then
          begin
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

procedure TReconciliationAnalysisValidator.UpdateReconciliationAnalysisCheck;
const OPNAME = 'TReconciliationAnalysisValidator.UpdateReconciliationAnalysisCheck';
var
  LWaterDemandConfiguration : IWaterDemandConfiguration;
begin
  try
    LWaterDemandConfiguration := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.WaterDemandConfiguration;
    if (LWaterDemandConfiguration <> nil) then
    with ReconciliationAnalysisDialog do
    begin
      if ReconciliationAnalysisCheck.Checked then
        LWaterDemandConfiguration.ImplementReconciliation := TRUE
      else
        LWaterDemandConfiguration.ImplementReconciliation := FALSE
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

