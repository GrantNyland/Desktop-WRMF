{******************************************************************************}
{*  UNIT      : Contains the class TMasterControlChannelValidator.            *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/17                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMasterControlChannelValidator;

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
  UChannelPenaltyValidator,
  UMasterControlChannelDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TMasterControlChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID               : integer;
    FFactorTotal             : double;
    FYearlyTotal             : double;
    FSystemAction            : Boolean;
    FChannelPenaltyValidator : TChannelPenaltyValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnStringGridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnAfterPastePowerColumnData(Sender: TObject);
    procedure OnAfterPastePowerLoadCaseColumnData(Sender: TObject);
    procedure OnAfterPasteWaterDistributionColumnData(Sender : TObject);
    procedure OnAfterPasteWaterLoadCaseColumnData(Sender : TObject);
    procedure OnAfterPasteWaterLoadCaseGridData(Sender : TObject);
    procedure RePopulateDataViewer;
    procedure RePopulateDistributionFactorGrids;
    procedure RePopulateLoadCasesGrids;
    procedure DisableModelControls;
    procedure OnReduceSequencesClick(Sender: TObject);
    procedure OnLoadCaseSelectedClick(Sender: TObject);
    procedure OnMonthlyDemandUnitsClick(Sender: TObject);
    procedure OnIncludeInOutputClick(Sender: TObject);
    procedure OnDemandCentreTypeClick(Sender: TObject);
    procedure RecalculateMonthlyDemand(ATargetRow : integer);
    procedure RecalculateMonthlyDemandWater(ATargetRow : integer);
    procedure RecalculateMonthlyDemandPower(ATargetRow : integer);
    procedure UpdateFeatureName;
    procedure UpdateMasterControlType;
    procedure UpdateCalculateFirmYield;
    procedure UpdateReduceSequences;
    procedure UpdateDistributionFactors(AMonth : integer; AValue : string);
    procedure UpdateMonthlyDemandWater(AMonth : integer; AValue : string);
    procedure UpdateMonthlyDemandPower(AMonth : integer; AValue : string);
    procedure UpdateTargetYield(AIndex : integer; AValue : string);
    procedure UpdateMaximumYield(AIndex : integer; AValue : string);
    procedure UpdatePowerDemand(AIndex : integer; AValue : string);
    procedure UpdateAnnualDemand;
    procedure UpdateMinimumDemand;
    procedure UpdateIncludeInOutput;
    procedure UpdateDemandCentreType;
    procedure ValidateFeatureName (AFeature : IMasterControlFeature);
    procedure ValidateMasterControlType (AFeature : IMasterControlFeature);
    procedure ValidateDistributionFactors (AFeature : IMasterControlFeature);
    procedure ValidateTargetYield (ARunData : IRunConfigurationData);
    procedure ValidateMaximumYield (ARunData : IRunConfigurationData);
    procedure ValidateTargetPower (ARunData : IRunConfigurationData);
    procedure ValidateNrOfLoadCases (ARunData : IRunConfigurationData);
    procedure ValidateAnnualDemand (AFeature : IMasterControlFeature);
    procedure ValidateMinimumDemand (AFeature : IMasterControlFeature);
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
    function MasterControlChannelDialog : TMasterControlChannelDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UConstants,
  UYieldModelDataGUIForm,
  URunConfigurationData,
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UErrorHandlingOperations, VCL.Grids;

{******************************************************************************}
{* TMasterControlChannelValidator                                             *}
{******************************************************************************}

procedure TMasterControlChannelValidator.CreateMemberObjects;
const OPNAME = 'TMasterControlChannelValidator.CreateMemberObjects';
var
  lpPanel     : TMasterControlChannelDialog;
  lIndexA     : integer;
begin
  try
    inherited CreateMemberObjects;
    FSystemAction            := FALSE;
    FFeatureID               := -1;
    FChannelPenaltyValidator := nil;
    FPanel  := TMasterControlChannelDialog.Create(FPanelOwner,FAppModules);
    lpPanel := MasterControlChannelDialog;
    with lpPanel do
    begin
      FeatureNameEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('MasterControlFeatureName');
      FeatureNameEdit.OnEnter         := OnEditControlEnter;
      FeatureNameEdit.OnExit          := OnEditControltExit;

      ChannelTypeCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('MasterChannelType');
      ChannelTypeCbx.OnEnter             := OnEditControlEnter;
      ChannelTypeCbx.OnChange            := OnEditControltExit;
      ChannelTypeCbx.OnExit              := OnEditControltExit;

      WaterDistributionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      WaterDistributionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WaterSupplyDistribution'));
      WaterDistributionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WaterSupplyDistribution'));
      WaterDistributionGrid.OnBeforeCellChange     := OnStringGridCellDataHasChanged;
      WaterDistributionGrid.OnSelectCell           := OnStringGridSelectCell;
      WaterDistributionGrid.OnColEnter             := OnStringGridColEnter;
      WaterDistributionGrid.OnEnter                := OnEditControlEnter;
      WaterDistributionGrid.ShowGridPopupMenu      := True;
      WaterDistributionGrid.AllowPasteFromExcel    := True;
      WaterDistributionGrid.OnAfterPasteColumnData := OnAfterPasteWaterDistributionColumnData;
      WaterDistributionGrid.OnPasteFromExcel       := OnAfterPasteWaterDistributionColumnData;

      MillionM3RadioButton.OnClick                := OnMonthlyDemandUnitsClick;
      M3RadioButton.OnClick                       := OnMonthlyDemandUnitsClick;

      PowerDistributionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      PowerDistributionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MinEnergyDemand'));
      PowerDistributionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MinEnergyDemand'));
      PowerDistributionGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      PowerDistributionGrid.OnSelectCell                   := OnStringGridSelectCell;
      PowerDistributionGrid.OnColEnter                     := OnStringGridColEnter;
      PowerDistributionGrid.OnEnter                        := OnEditControlEnter;
      PowerDistributionGrid.ShowGridPopupMenu              := True;
      PowerDistributionGrid.AllowPasteFromExcel            := True;
      PowerDistributionGrid.OnAfterPasteColumnData         := OnAfterPastePowerColumnData;
      PowerDistributionGrid.OnPasteFromExcel               := OnAfterPastePowerColumnData;

      FactorTotalEdit.OnEnter            := OnEditControlEnter;
      FactorTotalEdit.ReadOnly           := TRUE;
      FactorTotalEdit.IsEnabled          := False;

      YearlyTotalEdit.OnEnter            := OnEditControlEnter;
      YearlyTotalEdit.ReadOnly           := TRUE;
      YearlyTotalEdit.IsEnabled          := False;

      WaterLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TYield'));
      WaterLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MYield'));
      WaterLoadCasesGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      WaterLoadCasesGrid.OnSelectCell                   := OnStringGridSelectCell;
      WaterLoadCasesGrid.OnColEnter                     := OnStringGridColEnter;
      WaterLoadCasesGrid.OnEnter                        := OnEditControlEnter;
      WaterLoadCasesGrid.ShowGridPopupMenu              := True;
      WaterLoadCasesGrid.AllowPasteFromExcel            := True;
      WaterLoadCasesGrid.OnAfterPasteColumnData         := OnAfterPasteWaterLoadCaseColumnData;
      WaterLoadCasesGrid.OnAfterPasteColumnsAndRowsData := OnAfterPasteWaterLoadCaseGridData;
      WaterLoadCasesGrid.OnPasteFromExcel               := OnAfterPasteWaterLoadCaseGridData; 

      PowerLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TPower'));
      PowerLoadCasesGrid.OnBeforeCellChange     := OnStringGridCellDataHasChanged;
      PowerLoadCasesGrid.OnSelectCell           := OnStringGridSelectCell;
      PowerLoadCasesGrid.OnColEnter             := OnStringGridColEnter;
      PowerLoadCasesGrid.OnEnter                := OnEditControlEnter;
      PowerLoadCasesGrid.ShowGridPopupMenu      := True;
      PowerLoadCasesGrid.AllowPasteFromExcel    := True;
      PowerLoadCasesGrid.OnAfterPasteColumnData := OnAfterPastePowerLoadCaseColumnData;
      PowerLoadCasesGrid.OnPasteFromExcel       := OnAfterPastePowerLoadCaseColumnData; 

      for lIndexA := 1 to 10 do
      begin
        LoadCaseSelectedChkBox(lIndexA).FieldProperty := FAppModules.FieldProperties.FieldProperty('LoadCasesCount');
        LoadCaseSelectedChkBox(lIndexA).OnEnter := OnEditControlEnter;
        LoadCaseSelectedChkBox(lIndexA).OnClick := OnLoadCaseSelectedClick;
      end;

      AnnualDemandEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('AnnualDemand');
      AnnualDemandEdit.OnEnter       := OnEditControlEnter;
      AnnualDemandEdit.OnExit        := OnEditControltExit;

      MinimumDemandEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinimumDemand');
      MinimumDemandEdit.OnEnter       := OnEditControlEnter;
      MinimumDemandEdit.OnExit        := OnEditControltExit;

      IncludeInOutputChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('IncludeInOutput');
      IncludeInOutputChkBox.OnEnter       := OnEditControlEnter;
      IncludeInOutputChkBox.OnClick       := OnIncludeInOutputClick;

      DemandCentreTypeRadioGrp.FieldProperty := FAppModules.FieldProperties.FieldProperty('DemandCentreType');
      DemandCentreTypeRadioGrp.OnEnter       := OnEditControlEnter;
      DemandCentreTypeRadioGrp.OnClick       := OnDemandCentreTypeClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.DestroyMemberObjects;
const OPNAME = 'TMasterControlChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelValidator.Initialise: boolean;
const OPNAME = 'TMasterControlChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with MasterControlChannelDialog.DemandCentreTypeRadioGrp do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('MasterControl.ChannelTypeD'));
      Items.Add(FAppModules.Language.GetString('MasterControl.ChannelTypeR'));
      Columns := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMasterControlChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.MasterControlFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.ClearDataViewer;
const OPNAME = 'TMasterControlChannelValidator.ClearDataViewer';
var
  lnIndexA    : integer;
begin
  inherited ClearDataViewer;
  try
    with MasterControlChannelDialog do
    begin
      FSystemAction := TRUE;
      for lnIndexA := 0 to 11 do
      begin
        WaterDistributionGrid.Cells[1, lnIndexA] := '-1';
        WaterDistributionGrid.Cells[2, lnIndexA] := '-1';
        PowerDistributionGrid.Cells[1, lnIndexA] := '-1';
        PowerDistributionGrid.Cells[2, lnIndexA] := '-1';
      end;
      for lnIndexA := 0 to 9 do
      begin
        WaterLoadCasesGrid.Cells[0, lnIndexA] := '-1';
        WaterLoadCasesGrid.Cells[1, lnIndexA] := '-1';
        PowerLoadCasesGrid.Cells[0, lnIndexA] := '-1';
      end;
      ChannelTypeCbx.Items.Clear;
      FactorTotalEdit.SetFieldValue(-1);
      YearlyTotalEdit.SetFieldValue(-1);
      FSystemAction := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.PopulateDataViewer;
const OPNAME = 'TMasterControlChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtMasterControlFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.RePopulateDataViewer;
const OPNAME = 'TMasterControlChannelValidator.RePopulateDataViewer';
var
  sWater         : string;
  sPower         : string;
  sHydro         : string;
  lKeyValues     : string;
  lFieldIndex    : string;
  lType          : string;
  lItemIndex     : integer;
  lFeature       : IMasterControlFeature;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      if ((lFeature <> nil) AND (lFeature.Channel <> nil)) then
      begin
        with MasterControlChannelDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues     := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);


          lFieldProperty := ChannelTypeCbx.FieldProperty;
          lKeyValues     := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          ChannelTypeCbx.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

          ChannelTypeCbx.Items.Clear;
          sWater := FAppModules.Language.GetString('MasterControl.Water');
          sPower := FAppModules.Language.GetString('MasterControl.Power');
          sHydro := FAppModules.Language.GetString('MasterControl.Hydro');
          ChannelTypeCbx.Items.Add(sWater);
          if (FAppModules.Model.ModelName = CYield) then
            ChannelTypeCbx.Items.Add(sPower)
          else
            ChannelTypeCbx.Items.Add(sHydro);

          if (lFeature.MasterControlType = 'W') then
            ChannelTypeCbx.ItemIndex := ChannelTypeCbx.Items.IndexOf(sWater)
          else if (lFeature.MasterControlType = 'P') then
            ChannelTypeCbx.ItemIndex := ChannelTypeCbx.Items.IndexOf(sPower)
          else if (lFeature.MasterControlType = 'H') then
            ChannelTypeCbx.ItemIndex := ChannelTypeCbx.Items.IndexOf(sHydro);

          lItemIndex := 0;
          lType := lFeature.DemandCentreType;
          if (lType = 'D') then
            lItemIndex := 0
          else if (lType = 'R') then
            lItemIndex := 1;

          DemandCentreTypeRadioGrp.ItemIndex := lItemIndex;

          IncludeInOutputChkBox.Checked := lFeature.IncludeInOutput;

          AnnualDemandEdit.SetFieldValue(lFeature.AnnualDemand);

          MinimumDemandEdit.SetFieldValue(lFeature.MinimumDemand);

          RePopulateDistributionFactorGrids;
          RePopulateLoadCasesGrids;
          DisableModelControls;
          RecalculateMonthlyDemand(-1);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.RePopulateDistributionFactorGrids;
const OPNAME = 'TMasterControlChannelValidator.RePopulateDistributionFactorGrids';
var
  lIndexA        : integer;
  lCol           : integer;
  lKeyValues     : string;
  lFieldIndex    : string;
  lChannelType   : string;
  lMonths        : TMonthNamesArray;
  lFeature       : IMasterControlFeature;
  lFieldProperty : TAbstractFieldProperty;
begin
  lMonths := nil;
  try
    with MasterControlChannelDialog do
    begin
      if (FFeatureID >= 0) then
      begin
        lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
        if (lFeature <> nil) then
        begin
          lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNamesArray;
          lChannelType := lFeature.MasterControlType;
          WaterFactorLabel.Visible      := lChannelType = 'W';
          WaterTargetLabel.Visible      := lChannelType = 'W';
          MillionM3RadioButton.Visible  := lChannelType = 'W';
          M3RadioButton.Visible         := lChannelType = 'W';
          WaterDistributionGrid.Visible := lChannelType = 'W';
          PowerFactorLabel.Visible      := ((lChannelType = 'P') OR (lChannelType = 'H'));
          PowerTargetLabel.Visible      := ((lChannelType = 'P') OR (lChannelType = 'H'));
          PowerDistributionGrid.Visible := ((lChannelType = 'P') OR (lChannelType = 'H'));
          if (lChannelType = 'W') then
          begin
            for lCol := 1 to WaterDistributionGrid.ColCount -1  do
            begin
              lFieldProperty := WaterDistributionGrid.FieldProperty(WaterDistributionGrid.Col);
              for lIndexA := 0 to 11 do
              begin
                lFieldIndex := IntToStr(lCol)+ ',' + IntToStr(lIndexA);
                lKeyValues  := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
                WaterDistributionGrid.HasMetaData[lCol, lIndexA] :=
                FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

                WaterDistributionGrid.Cells[0, lIndexA] := lMonths[lIndexA+1];
                WaterDistributionGrid.Cells[1, lIndexA] :=
                Format(lFieldProperty.FormatStringGrid{'%6.3f'}, [lFeature.FactorByMonth[lIndexA+1]]);
              end;
            end;
            if ((NOT MillionM3RadioButton.Checked) AND (NOT M3RadioButton.Checked)) then
              MillionM3RadioButton.Checked := TRUE;
          end
          else if ((lChannelType = 'P') OR (lChannelType = 'H')) then
          begin
            for lCol := 1 to PowerDistributionGrid.ColCount -1 do
            begin
              lFieldProperty := PowerDistributionGrid.FieldProperty(PowerDistributionGrid.Col);
              for lIndexA := 0 to 11 do
              begin
                lFieldIndex := IntToStr(lCol)+ ',' + IntToStr(lIndexA);
                lKeyValues  := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
                PowerDistributionGrid.HasMetaData[lCol, lIndexA] :=
                FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

                PowerDistributionGrid.Cells[0, lIndexA] := lMonths[lIndexA+1];
                PowerDistributionGrid.Cells[1, lIndexA] :=
                  Format(lFieldProperty.FormatStringGrid{'%6.3f'}, [lFeature.FactorByMonth[lIndexA+1]]);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.RePopulateLoadCasesGrids;
const OPNAME = 'TMasterControlChannelValidator.RePopulateLoadCasesGrids';
var
  lConfigData      : TRunConfigurationData;
  lFeature         : IMasterControlFeature;
  LIndex           : integer;
  lIndexA          : integer;
  lValue           : double;
  lChannelType     : string;
  lActiveLoadCases : integer;
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
  lHasChanges      : boolean;
  lFieldIndex      : string;
  //lModelVersion    : string;
begin
  try
    with MasterControlChannelDialog do
    begin
      if (FFeatureID >= 0) then
      begin
        lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
        if ((lFeature <> nil) AND (lConfigData <> nil)) then
        begin
          lActiveLoadCases := lConfigData.NumberOfActiveLoadCases;
          FSystemAction := TRUE;
          for lIndexA := 0 to 9 do
          begin
            lValue := lConfigData.TargetYieldByIndex[lIndexA+1];
            if (lValue <> NullFloat) then
            begin
              lFieldProperty := WaterLoadCasesGrid.FieldProperty(0);
              lFieldIndex := IntToStr(lIndexA+1);
              lKeyValues  := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              lHasChanges := FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              WaterLoadCasesGrid.HasChanges [0, lIndexA] := lHasChanges;
              WaterLoadCasesGrid.HasMetaData[0, lIndexA] := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

              WaterLoadCasesGrid.Cells[0, lIndexA] := Format(lFieldProperty.FormatStringGrid {'%6.3f'}, [lValue]);
//              WaterLoadCasesGrid.Cells[0, lIndexA] := SmartFloatFormat(lValue,lFieldProperty.FieldWidth,lFieldProperty.NumberOfDecimals);
              LoadCaseSelectedChkBox(lIndexA+1).Enabled := TRUE;
              LoadCaseSelectedChkBox(lIndexA+1).Checked := lIndexA < lActiveLoadCases;
            end
            else
            begin
              WaterLoadCasesGrid.Cells[0, lIndexA] := '';
              LoadCaseSelectedChkBox(lIndexA+1).Enabled := FALSE;
              LoadCaseSelectedChkBox(lIndexA+1).Checked := FALSE;
            end;
            lValue := lConfigData.MaximumYieldByIndex[lIndexA+1];
            if (lValue <> NullFloat) then
            begin
              lFieldProperty := WaterLoadCasesGrid.FieldProperty(1);
              lFieldIndex := IntToStr(lIndexA+1);
              lKeyValues  := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              lHasChanges := FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              WaterLoadCasesGrid.HasChanges[1, lIndexA] := lHasChanges;
              WaterLoadCasesGrid.HasMetaData[1,lIndexA] := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

              WaterLoadCasesGrid.Cells[1, lIndexA] := Format(lFieldProperty.FormatStringGrid {'%6.3f'}, [lValue]);
//              WaterLoadCasesGrid.Cells[1, lIndexA] := SmartFloatFormat(lValue, lFieldProperty.FieldWidth, lFieldProperty.NumberOfDecimals);
            end
            else
              WaterLoadCasesGrid.Cells[1, lIndexA] := '';
            lValue := lConfigData.TargetPowerByIndex[lIndexA+1];
            if (lValue <> NullFloat) then
            begin
              lFieldProperty := PowerLoadCasesGrid.FieldProperty(0);
              lFieldIndex := IntToStr(lIndexA+1);
              lKeyValues  := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              lHasChanges := FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              PowerLoadCasesGrid.HasChanges[0, lIndexA] := lHasChanges;
              PowerLoadCasesGrid.HasMetaData[0, lIndexA] := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

              PowerLoadCasesGrid.Cells[0, lIndexA] := Format(lFieldProperty.FormatStringGrid {'%6.3f'}, [lValue]);
//              PowerLoadCasesGrid.Cells[0, lIndexA] := SmartFloatFormat(lValue, lFieldProperty.FieldWidth, lFieldProperty.NumberOfDecimals);
            end
            else
              PowerLoadCasesGrid.Cells[0, lIndexA] := '';
          end;
          FSystemAction := FALSE;
          lChannelType := lFeature.MasterControlType;
          WaterLoadCasesGrid.Enabled := (lChannelType = 'W');
          PowerLoadCasesGrid.Enabled := ((lChannelType = 'H') OR (lChannelType = 'P'));;
          if (lChannelType = 'W') then
          begin
            WaterLoadCasesGrid.Color := clWindow;
            PowerLoadCasesGrid.Color := clBtnFace;
           end
          else
          if ((lChannelType = 'H') OR (lChannelType = 'P')) then
          begin
            WaterLoadCasesGrid.Color := clBtnFace;
            PowerLoadCasesGrid.Color := clWindow;
          end;

         if ((lConfigData.CalculateHistoricFirmYield = 1) OR (lConfigData.CalculateHistoricFirmYield = 2)) then
         //lModelVersion := FAppModules.StudyArea.ModelVersion;
         //if((lConfigData.CalculateHistoricFirmYield = 1) AND ((lModelVersion = '6.1') OR
         //                                                      (lModelVersion = '6.2'))) then
          begin
            for LIndex := 2 to WaterLoadCasesGrid.RowCount -1 do
            begin
              WaterLoadCasesGrid.IsRowEnabled[LIndex] := False;
              LoadCaseSelectedChkBox(LIndex+1).Enabled := FALSE;
              if lConfigData.RunSequenceType = 'H' then
              begin
                HistoricTargetYieldLabel.Visible   := TRUE;
                StochasticTargetYieldLabel.Visible := FALSE;
              end
              else
              begin
                StochasticTargetYieldLabel.Visible := TRUE;
                HistoricTargetYieldLabel.Visible   := FALSE;
              end;
            end;
          end;
        end;
        if (WaterLoadCasesGrid.Row < 0) then
          WaterLoadCasesGrid.Row := 0;
        if (PowerLoadCasesGrid.Row < 0) then
          PowerLoadCasesGrid.Row := 0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.DisableModelControls;
const OPNAME = 'TMasterControlChannelValidator.DisableModelControls';
begin
  try
    if (FAppModules.Model.ModelName = CPlanning) then
    begin
      MasterControlChannelDialog.LoadCasesGroupBox.Visible         := False;
      MasterControlChannelDialog.DemandCentreGroupBox.Visible      := True;
      MasterControlChannelDialog.DemandCentreTypeRadioGrp.Visible := True;
    end
    else
    if (FAppModules.Model.ModelName = CYield) then
    begin
      MasterControlChannelDialog.LoadCasesGroupBox.Visible         := True;
      MasterControlChannelDialog.DemandCentreGroupBox.Visible      := False;
      MasterControlChannelDialog.DemandCentreTypeRadioGrp.Visible := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMasterControlChannelValidator.SaveState: boolean;
const OPNAME = 'TMasterControlChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelValidator.MasterControlChannelDialog : TMasterControlChannelDialog;
const OPNAME = 'TMasterControlChannelValidator.MasterControlChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TMasterControlChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMasterControlChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'CHANGELISTAPPLY') OR
       ((UpperCase(AFieldName) = 'CHANGELIST') AND (AContext = sdccDelete)) OR
       (UpperCase(AFieldName) = 'CHANGELISTCOPY')  OR
       (UpperCase(AFieldName) = 'ELEMENTACTIVE') OR
       (UpperCase(AFieldName) = 'ELEMENTORDER') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TMasterControlChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with MasterControlChannelDialog do
    begin
      if ((Sender = ChannelTypeCbx) AND (ChannelTypeCbx.HasValueChanged)) then
        UpdateMasterControlType
      else
      if ((Sender = FeatureNameEdit) AND (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName
      else
      if ((Sender = AnnualDemandEdit) AND (AnnualDemandEdit.HasValueChanged)) then
        UpdateAnnualDemand
      else
      if ((Sender = MinimumDemandEdit) AND (MinimumDemandEdit.HasValueChanged)) then
        UpdateMinimumDemand;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnReduceSequencesClick(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnReduceSequencesClick';
begin
  try
    UpdateReduceSequences;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateFeatureName;
const OPNAME = 'TMasterControlChannelValidator.UpdateFeatureName';
var
  lFeature  : IMasterControlFeature;
  lMessage  : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FeatureNameEdit.FieldProperty.FieldName,
            FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
          DoContextValidation(dvtMasterControlFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateMasterControlType;
const OPNAME = 'TMasterControlChannelValidator.UpdateMasterControlType';
var
  lFeature     : IMasterControlFeature;
  sChannelType : string;
  sNewType     : string;
  sWater       : string;
  sPower       : string;
  sHydro       : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        sWater := FAppModules.Language.GetString('MasterControl.Water');
        sPower := FAppModules.Language.GetString('MasterControl.Power');
        sHydro := FAppModules.Language.GetString('MasterControl.Hydro');
        sChannelType := lFeature.MasterControlType;
        if (ChannelTypeCbx.ItemIndex = ChannelTypeCbx.Items.IndexOf(sWater)) then
          sNewType := 'W'
        else if (ChannelTypeCbx.ItemIndex = ChannelTypeCbx.Items.IndexOf(sPower)) then
          sNewType := 'P'
        else if (ChannelTypeCbx.ItemIndex = ChannelTypeCbx.Items.IndexOf(sHydro)) then
          sNewType := 'H'
        else
          sNewType := '';
        if (((sChannelType = 'W') AND (ChannelTypeCbx.ItemIndex <> ChannelTypeCbx.Items.IndexOf(sWater))) OR
            ((sChannelType = 'P') AND (ChannelTypeCbx.ItemIndex <> ChannelTypeCbx.Items.IndexOf(sPower))) OR
            ((sChannelType = 'H') AND (ChannelTypeCbx.ItemIndex <> ChannelTypeCbx.Items.IndexOf(sHydro)))) then
        begin
          if (ChannelTypeCbx.ItemIndex = ChannelTypeCbx.Items.IndexOf(sWater)) then
            lFeature.MasterControlType := 'W'
          else if (ChannelTypeCbx.ItemIndex = ChannelTypeCbx.Items.IndexOf(sPower)) then
            lFeature.MasterControlType := 'P'
          else if (ChannelTypeCbx.ItemIndex = ChannelTypeCbx.Items.IndexOf(sHydro)) then
            lFeature.MasterControlType := 'H';
          if (lFeature.MasterControlType = 'W') then
            ChannelTypeCbx.ItemIndex := ChannelTypeCbx.Items.IndexOf(sWater)
          else if (lFeature.MasterControlType = 'P') then
            ChannelTypeCbx.ItemIndex := ChannelTypeCbx.Items.IndexOf(sPower)
          else if (lFeature.MasterControlType = 'H') then
            ChannelTypeCbx.ItemIndex := ChannelTypeCbx.Items.IndexOf(sHydro);

          RePopulateDistributionFactorGrids;
          RePopulateLoadCasesGrids;
          RecalculateMonthlyDemand(-1);
          DoContextValidation(dvtMasterControlFeatureType);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateCalculateFirmYield;
const OPNAME = 'TMasterControlChannelValidator.UpdateCalculateFirmYield';
var
  lConfigData    : TRunConfigurationData;
  lCalcFirmYield : Integer;
begin
  try
    with MasterControlChannelDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lCalcFirmYield := lConfigData.CalculateHistoricFirmYield;
        if ((lCalcFirmYield = 1) OR (lCalcFirmYield = 2)) then
        begin
          lConfigData.CalculateHistoricFirmYield := lCalcFirmYield;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateReduceSequences;
const OPNAME = 'TMasterControlChannelValidator.UpdateReduceSequences';
begin
  try
    with MasterControlChannelDialog do
    begin
      RePopulateLoadCasesGrids;
      RecalculateMonthlyDemand(-1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMasterControlChannelValidator.OnStringGridCellDataHasChanged';
var
  lFeature  : IMasterControlFeature;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with MasterControlChannelDialog do
        begin
          if ((ASender = WaterLoadCasesGrid) AND (ACol = 0) AND
              (NOT WaterLoadCasesGrid.HasChanges[ACol,ARow])) then
            UpdateTargetYield(ARow+1, Trim(WaterLoadCasesGrid.Cells[ACol, ARow]))
          else
          if ((ASender = WaterLoadCasesGrid) AND (ACol = 1) AND
              (NOT WaterLoadCasesGrid.HasChanges[ACol,ARow])) then
            UpdateMaximumYield(ARow+1, Trim(WaterLoadCasesGrid.Cells[ACol, ARow]))
          else
          if ((ASender = PowerLoadCasesGrid) AND (ACol = 0) AND
              (NOT PowerLoadCasesGrid.HasChanges[ACol,ARow])) then
            UpdatePowerDemand(ARow+1, Trim(PowerLoadCasesGrid.Cells[ACol, ARow]))
          else
          if ((ASender = WaterDistributionGrid) AND (ACol = 1)) then
             UpdateDistributionFactors(ARow+1, Trim(WaterDistributionGrid.Cells[ACol, ARow]))
          else
          if ((ASender = WaterDistributionGrid) AND (ACol = 2)) then
             UpdateMonthlyDemandWater(ARow+1, Trim(WaterDistributionGrid.Cells[ACol, ARow]))
          else
          if ((ASender = PowerDistributionGrid) AND (ACol = 1)) then
             UpdateDistributionFactors(ARow+1, Trim(PowerDistributionGrid.Cells[ACol, ARow]))
          else
          if ((ASender = PowerDistributionGrid) AND (ACol = 2)) then
             UpdateMonthlyDemandPower(ARow+1, Trim(PowerDistributionGrid.Cells[ACol, ARow]));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdatePowerDemand(AIndex : integer;
                                                           AValue : string);
const OPNAME = 'TMasterControlChannelValidator.UpdatePowerDemand';
var
  lConfigData : TRunConfigurationData;
  lValue      : double;
  lMessage    : string;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        lIndex := AIndex;
        while ((lIndex >= 1) AND
               (lConfigData.TargetPowerByIndex[lIndex-1] = NullFloat)) do
          lIndex := lIndex - 1;
        if (Trim(AValue) = '') then
          AValue := FloatToStr(NullFloat);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'TPower', AValue, lMessage, lIndex)) then
        begin
          PowerLoadCasesGrid.ValidationError[0, lIndex-1, gveCellField] := '';
          lValue := StrToFloat(Trim(AValue));
          lConfigData.TargetPowerByIndex[lIndex] := lValue;
          if ((lConfigData.TargetYieldByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.TargetYieldByIndex[lIndex] := 0;
          if ((lConfigData.MaximumYieldByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.MaximumYieldByIndex[lIndex] := 0;
          DoContextValidation(dvtTargetPower);
        end
        else
          PowerLoadCasesGrid.ValidationError[0, lIndex-1, gveCellField] := lMessage;
      end;
    end;
    RePopulateLoadCasesGrids;
    RecalculateMonthlyDemand(-1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateAnnualDemand;
const OPNAME = 'TMasterControlChannelValidator.UpdateAnnualDemand';
var
  lFeature  : IMasterControlFeature;
  lMessage  : string;
  LGrowthFactors : IGrowthFactors;
  LDemandCentreGrowthFactors : IDemandCentreGrowthFactors;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'AnnualDemand', AnnualDemandEdit.Text,lMessage)) then
        begin
          AnnualDemandEdit.FieldValidationError := lMessage;
          lFeature.AnnualDemand := StrToFloat(AnnualDemandEdit.Text);
          AnnualDemandEdit.SetFieldValue(lFeature.AnnualDemand);

          if FAppModules.StudyArea.ModelCode = CPlanning then
          begin
            LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
            if LFeature <> nil then
              if LFeature.Channel <> nil then
                LDemandCentreGrowthFactors := LGrowthFactors.DemandGrowthFactorsByChannel[LFeature.Channel.ChannelNumber];
            if LDemandCentreGrowthFactors <> nil then
              LDemandCentreGrowthFactors.ValidFactors := False;
          end;
          DoContextValidation(dvtAnnualDemand);
        end
        else
          AnnualDemandEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateMinimumDemand;
const OPNAME = 'TMasterControlChannelValidator.UpdateMinimumDemand';
var
  lFeature  : IMasterControlFeature;
  lMessage  : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'MinimumDemand', MinimumDemandEdit.Text,lMessage)) then
        begin
          MinimumDemandEdit.FieldValidationError := lMessage;
          lFeature.MinimumDemand := StrToFloat(MinimumDemandEdit.Text);
          MinimumDemandEdit.SetFieldValue(lFeature.MinimumDemand);
          DoContextValidation(dvtMinimumDemand);
        end
        else
          MinimumDemandEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateIncludeInOutput;
const OPNAME = 'TMasterControlChannelValidator.UpdateIncludeInOutput';
var
  lIncludeInOutput : boolean;
  lFeature         : IMasterControlFeature;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        lIncludeInOutput := lFeature.IncludeInOutput;
        if ((lIncludeInOutput AND (NOT IncludeInOutputChkBox.Checked)) OR
            ((NOT lIncludeInOutput) AND IncludeInOutputChkBox.Checked)) then
        begin
          lFeature.IncludeInOutput := IncludeInOutputChkBox.Checked;
          IncludeInOutputChkBox.Checked := lFeature.IncludeInOutput;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateDemandCentreType;
const OPNAME = 'TMasterControlChannelValidator.UpdateDemandCentreType';
var
  lFeature   : IMasterControlFeature;
  lOldValue  : string;
  lNewValue  : string;
  LItemIndex : integer;
begin
  try
    with MasterControlChannelDialog do
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        LItemIndex := 0;
        lOldValue := lFeature.DemandCentreType;
        if DemandCentreTypeRadioGrp.ItemIndex = 0 then
        begin
          lNewValue := 'D';
          LItemIndex  := 0;
        end
        else if DemandCentreTypeRadioGrp.ItemIndex = 1 then
        begin
          lNewValue := 'R';
           LItemIndex  := 1;
        end;
        if (lOldValue <> lNewValue) then
        begin
          lFeature.DemandCentreType := lNewValue;
          DemandCentreTypeRadioGrp.ItemIndex := LItemIndex;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateTargetYield(AIndex : integer;
                                                           AValue : string);
const OPNAME = 'TMasterControlChannelValidator.UpdateTargetYield';
var
  lConfigData : TRunConfigurationData;
  lValue      : double;
  lMessage    : string;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        lIndex := AIndex;
        while ((lIndex >= 1) AND
               (lConfigData.TargetYieldByIndex[lIndex-1] = NullFloat)) do
          lIndex := lIndex - 1;
        if (Trim(AValue) = '') then
          AValue := FloatToStr(NullFloat);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'TYield', AValue, lMessage, lIndex)) then
        begin
          WaterLoadCasesGrid.ValidationError[0, lIndex-1, gveCellField] := '';
          lValue := StrToFloat(Trim(AValue));
          lConfigData.TargetYieldByIndex[lIndex] := lValue;
          if (lConfigData.MaximumYieldByIndex[lIndex] = NullFloat) then
            lConfigData.MaximumYieldByIndex[lIndex] := lValue;
          if ((lConfigData.TargetPowerByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.TargetPowerByIndex[lIndex] := 0;
          DoContextValidation(dvtTargetYield);
        end
        else
          WaterLoadCasesGrid.ValidationError[0, lIndex-1,gveCellField] := lMessage;
      end;
    end;
    RePopulateLoadCasesGrids;
    RecalculateMonthlyDemand(-1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateMaximumYield(AIndex : integer;
                                                            AValue : string);
const OPNAME = 'TMasterControlChannelValidator.UpdateMaximumYield';
var
  lValue      : double;
  lMessage    : string;
  lConfigData : TRunConfigurationData;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        lIndex := AIndex;
        while ((lIndex >= 1) AND
               (lConfigData.MaximumYieldByIndex[lIndex-1] = NullFloat)) do
          lIndex := lIndex - 1;
        if (Trim(AValue) = '') then
          AValue := FloatToStr(NullFloat);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'MYield', AValue, lMessage, lIndex)) then
        begin
          WaterLoadCasesGrid.ValidationError[1, lIndex-1, gveCellField] := '';
          lValue := StrToFloat(Trim(AValue));
          lConfigData.MaximumYieldByIndex[lIndex] := lValue;
          if (lConfigData.TargetYieldByIndex[lIndex] = NullFloat) then
            lConfigData.TargetYieldByIndex[lIndex] := lValue;
          if ((lConfigData.TargetPowerByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.TargetPowerByIndex[lIndex] := 0;
          DoContextValidation(dvtMaximumYield);
        end
        else
          WaterLoadCasesGrid.ValidationError[1, lIndex-1, gveCellField] := lMessage;
      end;
    end;
    RePopulateLoadCasesGrids;
    RecalculateMonthlyDemand(-1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateDistributionFactors
                                         (AMonth : integer; AValue : string);
const OPNAME = 'TMasterControlChannelValidator.UpdateDistributionFactors';
var
  lFeature  : IMasterControlFeature;
  lValue    : double;
  lMessage  : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with MasterControlChannelDialog do
        begin
          if (lFeature.MasterControlType = 'W') then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'WaterSupplyDistribution', AValue, lMessage, AMonth)) then
            begin
              WaterDistributionGrid.ValidationError[1, AMonth-1, gveCellField] := '';
              lValue := StrToFloat(Trim(AValue));
              lFeature.FactorByMonth[AMonth] := lValue;
              RePopulateDistributionFactorGrids;
              RecalculateMonthlyDemand(-1);
              DoContextValidation(dvtMasterControlFeatureFactors);
            end
            else
              WaterDistributionGrid.ValidationError[1, AMonth-1, gveCellField] := lMessage;
          end
          else
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'MinEnergyDemand', AValue, lMessage, AMonth)) then
            begin
              PowerDistributionGrid.ValidationError[1, AMonth-1, gveCellField] := '';
              lValue := StrToFloat(Trim(AValue));
              lFeature.FactorByMonth[AMonth] := lValue;
              RePopulateDistributionFactorGrids;
              RecalculateMonthlyDemand(-1);
              DoContextValidation(dvtMasterControlFeatureFactors);
            end
            else
              PowerDistributionGrid.ValidationError[1, AMonth-1, gveCellField] := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateMonthlyDemandWater
                                         (AMonth : integer; AValue : string);
const OPNAME = 'TMasterControlChannelValidator.UpdateMonthlyDemandWater';
var
  lFeature   : IMasterControlFeature;
  dValue     : double;
  nIndexA    : integer;
  lMessage   : string;
  lMonthDays : TMonthDaysArray;
begin
  try
    if (FFeatureID >= 0) then
    begin
      SetLength(lMonthDays, 12);
      lFeature    := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                       MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      lMonthDays  := TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastRunConfigurationData.MonthsDaysArray;
      if (lFeature <> nil) then
      begin
        with MasterControlChannelDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'WaterSupplyDistribution', AValue, lMessage, AMonth)) then
          begin
            WaterDistributionGrid.ValidationError[2, AMonth-1, gveCellField] := '';
            FYearlyTotal := 0;
            for nIndexA := 1 to 12 do
            begin
              if (FAppModules.Model.ModelName = CYield) then
              begin
                dValue := StrToFloat(Trim(WaterDistributionGrid.Cells[2,nIndexA-1]));
              end
              else
              begin
                dValue := 0;
                if(Trim(WaterDistributionGrid.Cells[2,nIndexA-1]) <> '') then
                  dValue := StrToFloat(Trim(WaterDistributionGrid.Cells[2,nIndexA-1]));
              end;

              if (M3RadioButton.Checked) then
                dValue := dValue * 86400.0 * lMonthDays[nIndexA] / 1000000;
              FYearlyTotal := FYearlyTotal + dValue;
            end;
            for nIndexA := 1 to 12 do
            begin
              if (FAppModules.Model.ModelName = CYield) then
              begin
                dValue := StrToFloat(Trim(WaterDistributionGrid.Cells[2,nIndexA-1]));
              end
              else
              begin
                dValue := 0;
                if(Trim(WaterDistributionGrid.Cells[2,nIndexA-1]) <> '') then
                  dValue := StrToFloat(Trim(WaterDistributionGrid.Cells[2,nIndexA-1]));
              end;
              if (M3RadioButton.Checked) then
                dValue := dValue * 86400.0 * lMonthDays[nIndexA] / 1000000;
              if FYearlyTotal <> 0 then
                dValue := dValue * 12 / FYearlyTotal;
              lFeature.FactorByMonth[nIndexA] := dValue;
            end;
            UpdateTargetYield(WaterLoadCasesGrid.Row+1, FloatToStr(FYearlyTotal));
            RePopulateDistributionFactorGrids;
            RecalculateMonthlyDemand(-1);
            DoContextValidation(dvtMasterControlFeatureFactors);;
          end
          else
            WaterDistributionGrid.ValidationError[2, AMonth-1, gveCellField] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.UpdateMonthlyDemandPower
                                         (AMonth : integer; AValue : string);
const OPNAME = 'TMasterControlChannelValidator.UpdateMonthlyDemandPower';
var
  lFeature  : IMasterControlFeature;
  dValue    : double;
  nIndexA   : integer;
  lMessage  : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with MasterControlChannelDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'MinEnergyDemand', AValue, lMessage, AMonth)) then
          begin
            PowerDistributionGrid.ValidationError[2, AMonth-1, gveCellField] := '';
            FYearlyTotal := 0;
            for nIndexA := 0 to 11 do
              FYearlyTotal := FYearlyTotal + StrToFloat(PowerDistributionGrid.Cells[2,nIndexA]);
            for nIndexA := 1 to 12 do
            begin
              dValue := 0.0;
              if FYearlyTotal <> 0 then
              dValue := StrToFloat(Trim(PowerDistributionGrid.Cells[2, nIndexA-1]))
                        * 12 / FYearlyTotal;
              lFeature.FactorByMonth[nIndexA] := dValue;
            end;
            UpdatePowerDemand(PowerLoadCasesGrid.Col+1, FloatToStr(FYearlyTotal));
          end
          else
            PowerDistributionGrid.ValidationError[2, AMonth-1, gveCellField] := lMessage;
        end;
      end;
    end;
    RePopulateDistributionFactorGrids;
    RecalculateMonthlyDemand(-1);
    DoContextValidation(dvtMasterControlFeatureFactors);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnStringGridSelectCell (Sender : TObject;
                                                                 ACol   : LongInt;
                                                                 ARow   : Longint;
                                                                 var CanSelect : Boolean);
const OPNAME = 'TMasterControlChannelValidator.OnStringGridSelectCell';
begin
  try
    with MasterControlChannelDialog do
    begin
      if ((Sender = WaterLoadCasesGrid) OR (Sender = PowerLoadCasesGrid)) then
        RecalculateMonthlyDemand(ARow);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.RecalculateMonthlyDemand(ATargetRow : integer);
const OPNAME = 'TMasterControlChannelValidator.RecalculateMonthlyDemand';
var
  lConfigData : TRunConfigurationData;
  lFeature    : IMasterControlFeature;
begin
  try
    FFactorTotal := 0;
    FYearlyTotal := 0;
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if ((lFeature <> nil) AND (lConfigData <> nil)) then
      begin
        if (lFeature.MasterControlType = 'W') then
          RecalculateMonthlyDemandWater(ATargetRow)
        else if ((lFeature.MasterControlType = 'P') OR (lFeature.MasterControlType = 'H')) then
          RecalculateMonthlyDemandPower(ATargetRow);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.RecalculateMonthlyDemandWater(ATargetRow : integer);
const OPNAME = 'TMasterControlChannelValidator.RecalculateMonthlyDemandWater';
var
  lIndexA         : integer;
  lTarget         : double;
  lFactor         : double;
  lValue          : double;
  lMonthValue     : double;
  lConfigData     : TRunConfigurationData;
  lFeature        : IMasterControlFeature;
  lMonthDays      : TMonthDaysArray;
  LFieldProperty  : TAbstractFieldProperty;
begin
  try
    FFactorTotal := 0;
    FYearlyTotal := 0;
    SetLength(lMonthDays, 12);
    if (FFeatureID >= 0) then
    begin
      lFeature    := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      lMonthDays  := lConfigData.MonthsDaysArray;
      if ((lFeature <> nil) AND (lConfigData <> nil)) then
      begin
        with MasterControlChannelDialog do
        begin
          if (FAppModules.Model.ModelName = CYield) then
          begin
            if (ATargetRow < 0) then
              ATargetRow := WaterLoadCasesGrid.Row;
            lTarget := lConfigdata.TargetYieldByIndex[ATargetRow+1];
            LFieldProperty := WaterDistributionGrid.FieldProperty(1);
            if (lTarget <> NullFloat) then
            begin
              for lIndexA := 1 to 12 do
              begin
                lFactor      := lFeature.FactorByMonth[lIndexA];
                FFactorTotal := FFactorTotal + lFactor;
                lValue       := lTarget * lFactor * (1000000 / 365.25 / 86400.0);
                lMonthValue  := lValue * lMonthDays[lIndexA] * (86400.0 / 1000000);
                if (MillionM3RadioButton.Checked) then
                begin
                  WaterDistributionGrid.Cells[2, lIndexA-1] := Format(LFieldProperty.FormatStringGrid {'%11.3f'}, [lMonthValue]);
                  FYearlyTotal := FYearlyTotal + (lMonthValue / 12);
                end
                else
                begin
                  WaterDistributionGrid.Cells[2, lIndexA-1] := Format(LFieldProperty.FormatStringGrid {'%11.3f'}, [lValue]);
                  FYearlyTotal := FYearlyTotal + lMonthValue;
                end;
              end;

              if (MillionM3RadioButton.Checked) then
                TotalUnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.M3perAnnum')
              else
              begin
                FYearlyTotal := (FYearlyTotal * 1000000) / (86400 * (365.25/12));
                TotalUnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.Cumecs');
              end;
              FactorTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%6.3f'}, [FFactorTotal]);
              YearlyTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%12.3f'}, [FYearlyTotal]);
              FFactorTotal := Trunc(FFactorTotal * 100) / 100;
            end
            else
            begin
              for lIndexA := 1 to 12 do
                WaterDistributionGrid.Cells[2, lIndexA-1] := '';
              YearlyTotalEdit.Text := '';
            end;
          end
          else
          begin
            LFieldProperty := WaterDistributionGrid.FieldProperty(1);
            for lIndexA := 1 to 12 do
            begin
              lFactor      := lFeature.FactorByMonth[lIndexA];
              FFactorTotal := FFactorTotal + lFactor;
              lValue       := lFactor * 1000000 / 365.25 / 86400.0;
              lMonthValue  := lValue * lMonthDays[lIndexA] * 86400.0 / 1000000;
              if (MillionM3RadioButton.Checked) then
              begin
                WaterDistributionGrid.Cells[2, lIndexA-1] := Format(LFieldProperty.FormatStringGrid {'%11.3f'}, [lMonthValue]);
                FYearlyTotal := FYearlyTotal + lMonthValue
              end
              else
              begin
                WaterDistributionGrid.Cells[2, lIndexA-1] := Format(LFieldProperty.FormatStringGrid {'%11.3f'}, [lValue]);
                FYearlyTotal := FYearlyTotal + lValue;
              end;
            end;
            FactorTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%6.3f'}, [FFactorTotal]);
            YearlyTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%12.3f'}, [FYearlyTotal]);
            FFactorTotal := Trunc(FFactorTotal * 100) / 100;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnMonthlyDemandUnitsClick(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnMonthlyDemandUnitsClick';
begin
  try
    RecalculateMonthlyDemandWater(-1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnIncludeInOutputClick(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnIncludeInOutputClick';
begin
  try
    if(MasterControlChannelDialog.IncludeInOutputChkBox.HasValueChanged) then
      UpdateIncludeInOutput;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnDemandCentreTypeClick(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnDemandCentreTypeClick';
begin
  try
    if(MasterControlChannelDialog.DemandCentreTypeRadioGrp.HasValueChanged) then
    begin
      UpdateDemandCentreType;
      OnEditControlEnter(Sender);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.RecalculateMonthlyDemandPower(ATargetRow : integer);
const OPNAME = 'TMasterControlChannelValidator.RecalculateMonthlyDemandPower';
var
  lIndexA         : integer;
  lTarget         : double;
  lFactor         : double;
  lValue          : double;
  lConfigData     : TRunConfigurationData;
  lFeature        : IMasterControlFeature;
  LFieldProperty  : TAbstractFieldProperty;
begin
  try
    FFactorTotal := 0;
    FYearlyTotal := 0;
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if ((lFeature <> nil) AND (lConfigData <> nil)) then
      begin
        with MasterControlChannelDialog do
        begin
          if (FAppModules.Model.ModelName = CYield) then
          begin
            if (ATargetRow < 0) then
                ATargetRow := PowerLoadCasesGrid.Row;
            lTarget := lConfigdata.TargetPowerByIndex[ATargetRow+1];
            if (lTarget <> NullFloat) then
            begin
              LFieldProperty := PowerDistributionGrid.FieldProperty(1);
              for lIndexA := 1 to 12 do
              begin
                lFactor := lFeature.FactorByMonth[lIndexA];
                lValue  := lTarget * lFactor / 12;
                PowerDistributionGrid.Cells[2, lIndexA-1] := Format(LFieldProperty.FormatStringGrid{'%11.3f'}, [lValue]);
                FFactorTotal := FFactorTotal + lFactor;
                FYearlyTotal := FYearlyTotal + lValue;
              end;
              FactorTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%6.3f'}, [FFactorTotal]);
              YearlyTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%12.3f'}, [FYearlyTotal]);
              FFactorTotal := Trunc(FFactorTotal * 100) / 100;
            end
            else
            begin
              for lIndexA := 1 to 12 do
                PowerDistributionGrid.Cells[2, lIndexA-1] := '';
              YearlyTotalEdit.Text := '';
            end;
          end
          else
          begin
            LFieldProperty := PowerDistributionGrid.FieldProperty(1);
            for lIndexA := 1 to 12 do
            begin
              lFactor := lFeature.FactorByMonth[lIndexA];
              lValue  := lFactor / 12;
              PowerDistributionGrid.Cells[2, lIndexA-1] := Format(LFieldProperty.FormatStringGrid{'%11.3f'}, [lValue]);
              FFactorTotal := FFactorTotal + lFactor;
              FYearlyTotal := FYearlyTotal + lValue;
            end;
            FactorTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%6.3f'}, [FFactorTotal]);
            YearlyTotalEdit.Text := Format(LFieldProperty.FormatStringGrid {'%12.3f'}, [FYearlyTotal]);
            FFactorTotal := Trunc(FFactorTotal * 100) / 100;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnLoadCaseSelectedClick(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnLoadCaseSelectedClick';
var
  lIndexA     : integer;
  lChkBox     : TFieldChkBox;
  lConfigData : TRunConfigurationData;
begin
  try
    if (NOT FSystemAction) then
    begin
      with MasterControlChannelDialog do
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
        if (lConfigData <> nil) then
        begin
          lChkBox := TFieldChkBox(Sender);
          lIndexA := MasterControlChannelDialog.IndexOfLoadCaseCheckBox(lChkBox);
          if (lIndexA >= 0) then
          begin
            if (lChkBox.Checked) then
              lConfigData.ActivateLoadCase(lIndexA)
            else
              lConfigdata.DeactivateLoadCase(lIndexA);
            DoContextValidation(dvtNrOfLoadCases);
            DoContextValidation(dvtTargetYield);
          end;
        end;
      end;
      RePopulateLoadCasesGrids;
      RecalculateMonthlyDemand(-1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMasterControlChannelValidator.DoContextValidation';
var
  lFeature     : IMasterControlFeature;
  lFeatureList : IMasterControlFeatureList;
  lConfigData  : TRunConfigurationData;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MasterControlFeatureList;
      lFeature     := lFeatureList.MasterControlFeatureByID[FFeatureID];
      lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastRunConfigurationData;
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtMasterControlFeature, dvtMasterControlFeatureName,
                                dvtMasterControlWizardStep1]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtMasterControlFeature, dvtMasterControlFeatureType,
                                dvtMasterControlWizardStep1]) then
          ValidateMasterControlType(lFeature);
        if (AValidationType in [dvtMasterControlFeature, dvtMasterControlFeatureFactors]) then
          ValidateDistributionFactors(lFeature);
        if (AValidationType in [dvtMasterControlFeature, dvtTargetYield,
                                dvtMasterControlWizardStep1]) then
          ValidateTargetYield(lConfigData);
        if (AValidationType in [dvtMasterControlFeature, dvtMaximumYield, dvtTargetYield,
                                dvtMasterControlWizardStep1]) then
          ValidateMaximumYield(lConfigData);
        if (AValidationType in [dvtMasterControlFeature, dvtTargetPower,
                                dvtMasterControlWizardStep1]) then
          ValidateTargetPower(lConfigData);
        if (AValidationType in [dvtMasterControlFeature, dvtNrOfLoadCases,
                                dvtMasterControlWizardStep1]) then
          ValidateNrOfLoadCases(lConfigData);
        if (AValidationType = dvtMasterControlFeature) or (AValidationType = dvtAnnualDemand) then
          ValidateAnnualDemand(lFeature);
        if (AValidationType = dvtMasterControlFeature) or (AValidationType = dvtMinimumDemand) then
          ValidateMinimumDemand(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TMasterControlChannelValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    Result := 0;
    case ASequence of
      1 :
        begin
          DoContextValidation(dvtMasterControlWizardStep1);
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      2 :
        begin
          DoContextValidation(dvtMasterControlFeatureFactors);
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.ValidateFeatureName (AFeature : IMasterControlFeature);
const OPNAME = 'TMasterControlChannelValidator.ValidateFeatureName';
begin
  try
    with MasterControlChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'FeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.ValidateMasterControlType (AFeature : IMasterControlFeature);
const OPNAME = 'TMasterControlChannelValidator.ValidateMasterControlType';
begin
  try
    with MasterControlChannelDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'MasterControlType')) then
      begin
        ChannelTypeCbx.InValidationError := FALSE;
        ChannelTypeCbx.ShowErrorState(FALSE);
      end
      else
      begin
        ChannelTypeCbx.InValidationError := TRUE;
        ChannelTypeCbx.ValidationError := FErrorMessage;
        ChannelTypeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.ValidateDistributionFactors (AFeature : IMasterControlFeature);
const OPNAME = 'TMasterControlChannelValidator.ValidateDistributionFactors';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    if (AFeature <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
       try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'DistributionFactors')) then
          begin
            if (Trim(Uppercase(AFeature.MasterControlType)) = 'P') then
            begin
              for lCol := 1 to 12 do
              begin
                PowerDistributionGrid.ValidationError[1, lCol-1, gveCellContext] := '';
                FactorTotalEdit.ContextValidationError := FErrorMessage;
              end;
            end
            else
              for lCol := 1 to 12 do
              begin
                WaterDistributionGrid.ValidationError[1, lCol-1, gveCellContext] := '';
                FactorTotalEdit.ContextValidationError := FErrorMessage;
              end;
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            if (Trim(Uppercase(AFeature.MasterControlType)) = 'P') then
            begin
              for lCol := 1 to 12 do
              begin
                lIndex := lErrorCols.IndexOf(IntToStr(lCol));
                if (lIndex >= 0) then
                  PowerDistributionGrid.ValidationError[1, lCol-1, gveCellContext] := lErrorMessages.Strings[lIndex]
                else
                  PowerDistributionGrid.ValidationError[1, lCol-1, gveCellContext] := '';
                FactorTotalEdit.ContextValidationError := FErrorMessage;
              end;
            end
            else
            begin
              for lCol := 1 to 12 do
              begin
                lIndex := lErrorCols.IndexOf(IntToStr(lCol));
                if (lIndex >= 0) then
                  PowerDistributionGrid.ValidationError[1, lCol-1, gveCellContext] := lErrorMessages.Strings[lIndex]
                else
                  PowerDistributionGrid.ValidationError[1, lCol-1, gveCellContext] := '';
                FactorTotalEdit.ContextValidationError := FErrorMessage;
              end;
            end;
          end;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.ValidateTargetYield (ARunData : IRunConfigurationData);
const OPNAME = 'TMasterControlChannelValidator.ValidateTargetYield';
var
  lCol       : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    if (ARunData <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (ARunData.Validate(FErrorMessage,'TargetYield')) then
          begin
            for lCol := 1 to 10 do
              WaterLoadCasesGrid.ValidationError[0, lCol-1, gveCellContext] := '';
         end
         else
         begin
           ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
           for lCol := 1 to 10 do
           begin
             lIndex := lErrorCols.IndexOf(IntToStr(lCol));
             if (lIndex >= 0) then
               WaterLoadCasesGrid.ValidationError[0, lCol-1 , gveCellContext] := lErrorMsgs.Strings[lIndex]
             else
               WaterLoadCasesGrid.ValidationError[0, lCol-1 , gveCellContext] := ''
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

procedure TMasterControlChannelValidator.ValidateMaximumYield (ARunData : IRunConfigurationData);
const OPNAME = 'TMasterControlChannelValidator.ValidateMaximumYield';
var
  lCol       : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    if (ARunData <> nil) then
    begin
      with MasterControlChannelDialog do
        begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (ARunData.Validate(FErrorMessage,'MaximumYield')) then
          begin
            for lCol := 1 to 10 do
              WaterLoadCasesGrid.ValidationError[1, lCol-1, gveCellContext] := '';
         end
         else
         begin
           ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
           for lCol := 1 to 10 do
           begin
             lIndex := lErrorCols.IndexOf(IntToStr(lCol));
             if (lIndex >= 0) then
               WaterLoadCasesGrid.ValidationError[1, lCol-1 , gveCellContext] := lErrorMsgs.Strings[lIndex]
             else
               WaterLoadCasesGrid.ValidationError[1, lCol-1 , gveCellContext] := ''
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

procedure TMasterControlChannelValidator.ValidateTargetPower (ARunData : IRunConfigurationData);
const OPNAME = 'TMasterControlChannelValidator.ValidateTargetPower';
var
  lCol       : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    if (ARunData <> nil) then
    begin
      with MasterControlChannelDialog do
        begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (ARunData.Validate(FErrorMessage,'TargetPower')) then
          begin
            for lCol := 1 to 10 do
              PowerLoadCasesGrid.ValidationError[0, lCol-1, gveCellContext] := '';
         end
         else
         begin
           ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
           for lCol := 1 to 10 do
           begin
             lIndex := lErrorCols.IndexOf(IntToStr(lCol));
             if (lIndex >= 0) then
               PowerLoadCasesGrid.ValidationError[0, lCol-1 , gveCellContext] := lErrorMsgs.Strings[lIndex]
             else
               PowerLoadCasesGrid.ValidationError[0, lCol-1 , gveCellContext] := ''
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

procedure TMasterControlChannelValidator.ValidateNrOfLoadCases (ARunData : IRunConfigurationData);
const OPNAME = 'TMasterControlChannelValidator.ValidateNrOfLoadCases';
var
  lIndex      : integer;
  LMessage    : string;
  lConfigData : TRunConfigurationData;
begin
  try
    if (ARunData <> nil) then
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      with MasterControlChannelDialog do
      begin
        FErrorMessage := '';
        if (ARunData.Validate(FErrorMessage, 'NrOfLoadCases')) then
        begin
          for lIndex := 1 to 10 do
          begin
            LoadCaseSelectedChkBox(lIndex).InValidationError := FALSE;
            LoadCaseSelectedChkBox(lIndex).ValidationError := '';
            LoadCaseSelectedChkBox(lIndex).ShowErrorState(FALSE);
          end;
        end
        else
        begin
          for lIndex := 1 to 10 do
          begin
            LoadCaseSelectedChkBox(lIndex).InValidationError := TRUE;
            LoadCaseSelectedChkBox(lIndex).ValidationError := FErrorMessage;
            LoadCaseSelectedChkBox(lIndex).ShowErrorState(TRUE);
          end;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;

        if lConfigData.CalculateHistoricFirmYield = 1 then
        begin
          if lConfigData.NumberOfActiveLoadCases < 2 then
          begin
            LMessage := '';
            LMessage := (FAppModules.language.GetString('MasterControl.ActiveLoadCases'));
            MasterControlChannelDialog.WaterLoadCasesGrid.ValidationError[0,0, gveCellField] := LMessage;
          end
        else
          MasterControlChannelDialog.WaterLoadCasesGrid.ValidationError[0,0, gveCellField] := '';
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.ValidateAnnualDemand(AFeature: IMasterControlFeature);
const OPNAME = 'TMasterControlChannelValidator.ValidateAnnualDemand';
begin
  try
    with MasterControlChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'AnnualDemand')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      AnnualDemandEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.ValidateMinimumDemand(AFeature: IMasterControlFeature);
const OPNAME = 'TMasterControlChannelValidator.ValidateMinimumDemand';
begin
  try
    with MasterControlChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MinimumDemand')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      MinimumDemandEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMasterControlChannelValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TMasterControlChannelValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lRunConfig     : IRunConfigurationData;
  lIndex         : integer;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      lRunConfig := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lRunConfig <> nil) then
      begin
        with MasterControlChannelDialog do
        begin
          if (FActiveControl = WaterLoadCasesGrid) then
          begin
            lIndex := WaterLoadCasesGrid.Row + 1;
            lFieldIndex    := IntToStr(lIndex);
            lFieldProperty := WaterLoadCasesGrid.FieldProperty(WaterLoadCasesGrid.Col);
            if (lFieldProperty <> nil) then
            begin
              lKeyValues := lRunConfig.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              FAppModules.Changes.ShowParameterChanges
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              Result := TRUE;
              RePopulateDataViewer;
              FAppModules.Changes.SetParameterChanges(TRUE);
              Result := TRUE;
            end
          end
          else
          if (FActiveControl = PowerLoadCasesGrid) then
          begin
            lIndex := PowerLoadCasesGrid.Row + 1;
            lFieldIndex    := IntToStr(lIndex);
            lFieldProperty := PowerLoadCasesGrid.FieldProperty(0);
            if (lFieldProperty <> nil) then
            begin
              lKeyValues := lRunConfig.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
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

function TMasterControlChannelValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TMasterControlChannelValidator.ProcessMetaDataEvent';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IMasterControlFeature;
  lConfigData    : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if ((FPanel.Visible) AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lFeature <> nil) then
      begin
        with MasterControlChannelDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = ChannelTypeCbx) then
            lFieldProperty := ChannelTypeCbx.FieldProperty
          else
          if (FActiveControl = WaterLoadCasesGrid) then
          begin
            lFieldIndex := IntToStr(WaterLoadCasesGrid.Row+1);
            lFieldProperty := WaterLoadCasesGrid.FieldProperty(WaterLoadCasesGrid.Col);
          end
          else
          if(FActiveControl = PowerLoadCasesGrid) then
          begin
            lFieldIndex := IntToStr(PowerLoadCasesGrid.Row+1);
            lFieldProperty := PowerLoadCasesGrid.FieldProperty(PowerLoadCasesGrid.Col);
          end
          else
          if (FActiveControl = WaterDistributionGrid) then
          begin
            lFieldIndex    := IntToStr(WaterDistributionGrid.Col)+ ',' + IntToStr(WaterDistributionGrid.Row);
            lFieldProperty := WaterDistributionGrid.FieldProperty(WaterDistributionGrid.Col);
          end;
          if (FActiveControl = PowerDistributionGrid) then
          begin
            lFieldIndex    := IntToStr(PowerDistributionGrid.Col)+ ',' + IntToStr(PowerDistributionGrid.Row);
            lFieldProperty := PowerDistributionGrid.FieldProperty(PowerDistributionGrid.Col);
          end;
          if(FActiveControl = IncludeInOutputChkBox) then
            lFieldProperty := IncludeInOutputChkBox.FieldProperty;

          if ((lFieldProperty <> nil) AND (FActiveControl = WaterLoadCasesGrid) OR
                                          (FActiveControl = PowerLoadCasesGrid)) then
          begin
            lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
          end
          else
          if (lFieldProperty <> nil) then 
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
          end;
          RePopulateDataViewer;
          Result := TRUE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnAfterPastePowerColumnData(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnAfterPastePowerColumnData';
var
  LCol     : integer;
  LMonth   : integer;
  LFeature : IMasterControlFeature;
  LValue   : double;
begin
  try
    with MasterControlChannelDialog do
    begin
      LCol := PowerDistributionGrid.Col;
      if ((Sender = PowerDistributionGrid) AND (LCol = 1)) then
      begin
        if (FFeatureID >= 0) then
        begin
          LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
          if (LFeature <> nil) then
          begin
            if (LFeature.MasterControlType = 'P') then
            begin
              for LMonth := 0 to 11 do
              begin
                LValue := StrToFloat(Trim(PowerDistributionGrid.Cells[LCol,LMonth]));
                LFeature.FactorByMonth[LMonth + 1] := LValue;
                DoContextValidation(dvtMasterControlFeatureFactors);
              end;
              RePopulateDistributionFactorGrids;
              RecalculateMonthlyDemand(-1);
            end;
          end;
        end;
      end
      else if ((Sender = PowerDistributionGrid) AND (LCol = 2)) then
      begin
        for LMonth := 1 to 12 do
          UpdateMonthlyDemandPower(LMonth, Trim(PowerDistributionGrid.Cells[LCol, LMonth - 1]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnAfterPastePowerLoadCaseColumnData(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnAfterPastePowerLoadCaseColumnData';
var
  LConfigData : TRunConfigurationData;
  LValue      : double;
  LMessage    : string;
  LIndex      : integer;
begin
  try
    LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (LConfigData <> nil) then
    begin
      with MasterControlChannelDialog do
      begin
        if (Sender = PowerLoadCasesGrid) then
        begin
          for LIndex := 1 to 10 do
          begin
            LValue := StrToFloat(Trim(PowerLoadCasesGrid.Cells[0,LIndex - 1]));
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'TPower', FloatToStr(LValue), LMessage, LIndex)) then
            begin
              PowerLoadCasesGrid.ValidationError[0, LIndex - 1, gveCellField] := '';
              LConfigData.TargetPowerByIndex[LIndex] := LValue;
              if ((LConfigData.TargetYieldByIndex[LIndex] = NullFloat) AND(LValue <> NullFloat)) then
                LConfigData.TargetYieldByIndex[LIndex] := 0;
              if ((LConfigData.MaximumYieldByIndex[LIndex] = NullFloat) AND (LValue <> NullFloat)) then
                LConfigData.MaximumYieldByIndex[LIndex] := 0;
              DoContextValidation(dvtTargetPower);
            end
            else
              PowerLoadCasesGrid.ValidationError[0, LIndex-1, gveCellField] := LMessage;
          end;
        end;
      end;
    end;
    RePopulateLoadCasesGrids;
    RecalculateMonthlyDemand(-1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnAfterPasteWaterDistributionColumnData(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnAfterPasteWaterDistributionColumnData';
var
  LCol     : integer;
  LMonth   : integer;
  LFeature : IMasterControlFeature;
  LValue   : double;
begin
  try
    with MasterControlChannelDialog do
    begin
      LCol := WaterDistributionGrid.Col;
      if ((Sender = WaterDistributionGrid) AND (LCol = 1)) then
      begin
        if (FFeatureID >= 0) then
        begin
          LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[FFeatureID];
          if (LFeature <> nil) then
          begin
            if (LFeature.MasterControlType = 'W') then
            begin
              for LMonth := 0 to 11 do
              begin
                LValue := StrToFloat(Trim(WaterDistributionGrid.Cells[LCol,LMonth]));
                LFeature.FactorByMonth[LMonth + 1] := LValue;
                DoContextValidation(dvtMasterControlFeatureFactors);
              end;
              RePopulateDistributionFactorGrids;
              RecalculateMonthlyDemand(-1);
            end;
          end;
        end;
      end
      else if ((Sender = WaterDistributionGrid) AND (LCol = 2)) then
      begin
        for LMonth := 1 to 12 do
          UpdateMonthlyDemandWater(LMonth, Trim(WaterDistributionGrid.Cells[LCol, LMonth - 1]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnAfterPasteWaterLoadCaseColumnData(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnAfterPasteWaterLoadCaseColumnData';
var
  LIndex      : integer;
  LCol        : integer;
  LValue      : double;
  LConfigData : TRunConfigurationData;
begin
  try
    with MasterControlChannelDialog do
    begin
      LCol := WaterLoadCasesGrid.Col;
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if ((Sender = WaterLoadCasesGrid) AND (LCol = 0)) then
        begin
          for LIndex := 0 to 9 do
          begin
            LValue := StrToFloat(Trim(WaterLoadCasesGrid.Cells[LCol, LIndex]));
            LConfigData.TargetYieldByIndex[LIndex + 1] := LValue;
            if (LConfigData.MaximumYieldByIndex[LIndex] = NullFloat) then
              LConfigData.MaximumYieldByIndex[LIndex] := LValue;
            if ((LConfigData.TargetPowerByIndex[LIndex] = NullFloat) AND (LValue <> NullFloat)) then
              LConfigData.TargetPowerByIndex[LIndex] := 0;
            DoContextValidation(dvtTargetYield);
          end;
          RePopulateLoadCasesGrids;
          RecalculateMonthlyDemand(-1);
        end
        else
        if ((Sender = WaterLoadCasesGrid) AND (LCol = 1)) then
        begin
          for LIndex := 0 to 9 do
          begin
            LValue := StrToFloat(Trim(WaterLoadCasesGrid.Cells[LCol, LIndex]));
            LConfigData.MaximumYieldByIndex[LIndex + 1] := LValue;
            if (LConfigData.TargetYieldByIndex[LIndex] = NullFloat) then
              LConfigData.TargetYieldByIndex[LIndex] := LValue;
            if ((LConfigData.TargetPowerByIndex[LIndex] = NullFloat) AND (LValue <> NullFloat)) then
              LConfigData.TargetPowerByIndex[lIndex] := 0;
            DoContextValidation(dvtMaximumYield);
          end;
          RePopulateLoadCasesGrids;
          RecalculateMonthlyDemand(-1);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMasterControlChannelValidator.OnAfterPasteWaterLoadCaseGridData(Sender: TObject);
const OPNAME = 'TMasterControlChannelValidator.OnAfterPasteWaterLoadCaseGridData';
var
  LIndex      : integer;
  LValue      : double;
  LConfigData : TRunConfigurationData;
begin
  try
    with MasterControlChannelDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if (Sender = WaterLoadCasesGrid) then
        begin
          for LIndex := 0 to 9 do
          begin
            LValue := StrToFloat(Trim(WaterLoadCasesGrid.Cells[0, LIndex]));
            LConfigData.TargetYieldByIndex[LIndex + 1] := LValue;
            if (LConfigData.MaximumYieldByIndex[LIndex] = NullFloat) then
              LConfigData.MaximumYieldByIndex[LIndex] := LValue;
            if ((LConfigData.TargetPowerByIndex[LIndex] = NullFloat) AND (LValue <> NullFloat)) then
              LConfigData.TargetPowerByIndex[LIndex] := 0;

            LValue := StrToFloat(Trim(WaterLoadCasesGrid.Cells[1, LIndex]));
            LConfigData.MaximumYieldByIndex[LIndex + 1] := LValue;
            if (LConfigData.TargetYieldByIndex[LIndex] = NullFloat) then
              LConfigData.TargetYieldByIndex[LIndex] := LValue;
            if ((LConfigData.TargetPowerByIndex[LIndex] = NullFloat) AND (LValue <> NullFloat)) then
              LConfigData.TargetPowerByIndex[lIndex] := 0;
          end;
          DoContextValidation(dvtTargetYield);
          DoContextValidation(dvtMaximumYield);
          RePopulateLoadCasesGrids;
          RecalculateMonthlyDemand(-1);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


