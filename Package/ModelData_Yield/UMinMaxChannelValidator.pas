{******************************************************************************}
{*  UNIT      : Contains the class TMinMaxChannelValidator.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/12                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMinMaxChannelValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  VoaimsCom_TLB,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UMinMaxChannelDialog;

type
  TMinMaxChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    //procedure OnFirmYieldAnalysisClick(Sender: TObject);
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure OnAfterPasteGridData(Sender: TObject);
    procedure DoWQConChkBoxOnClick(Sender : TObject);
    procedure DoBoundChkBoxOnClick(Sender : TObject);
    procedure RePopulateDataViewer;
    procedure DisableModelControls;
    procedure RePopulateFlowConstraints;
    procedure RePopulateDistributionFactors;
    //procedure RePopulateFirmYieldAnalysis;
    procedure UpdateFeatureName;
    //procedure UpdateChannelFirmYieldAnalysis;
    procedure UpdateMonthlyFlowConstraints(AIndex : integer;
                                           AMonth : integer;
                                           AValue : string);
    procedure UpdateMonthlyDistributionFactors(AIndex : integer;
                                           AMonth : integer;
                                           AValue : string);
    procedure UpdateMonthlyFlowConstraintsDistribution(AIndex : integer;AMonth : integer;AValue : string);
    procedure ValidateMinMaxFeatureName (AFeature : IMinMaxFlowConstraint);
    procedure ValidateMinMaxFlowConstraint(AFeature : IMinMaxFlowConstraint);
    procedure ValidateMinMaxDistributionFactors(AFeature: IMinMaxFlowConstraint);
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
    function MinMaxChannelDialog: TMinMaxChannelDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UGrowthFactorData,
  UWQConstraintData,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TMinMaxChannelValidator                                                    *}
{******************************************************************************}

procedure TMinMaxChannelValidator.CreateMemberObjects;
const OPNAME = 'TMinMaxChannelValidator.CreateMemberObjects';
var
  lpPanel : TMinMaxChannelDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel  := TMinMaxChannelDialog.Create(FPanelOwner,FAppModules);
    lpPanel := MinMaxChannelDialog;
    with lpPanel do
    begin
      //FeatureNameEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('MinMaxChannelName');
      FeatureNameEdit.OnEnter        := OnEditControlEnter;
      FeatureNameEdit.OnExit         := OnEditControltExit;

      //FirmYieldAnalysisChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('FirmYieldCalc');
      //FirmYieldAnalysisChkBox.OnEnter       := OnEditControlEnter;
      //FirmYieldAnalysisChkBox.OnClick       := OnFirmYieldAnalysisClick;

      FlowConstraintGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      FlowConstraintGrid.OnColEnter                     := OnStringGridColEnter;
      FlowConstraintGrid.OnEnter                        := OnEditControlEnter;
      FlowConstraintGrid.ShowGridPopupMenu              := True;
      FlowConstraintGrid.AllowPasteFromExcel            := True;
      FlowConstraintGrid.OnPasteFromExcel               := Self.OnAfterPasteGridData;
      FlowConstraintGrid.OnAfterPasteColumnData         := Self.OnAfterPasteColumnData;
      FlowConstraintGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;


      DistributionGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      DistributionGrid.OnColEnter         := OnStringGridColEnter;
      DistributionGrid.OnEnter            := OnEditControlEnter;
      if (FAppModules.StudyArea.ModelCode = CPlanning) then
      begin
        IncludeInWQConChkBox.FieldProperty            := FAppModules.FieldProperties.FieldProperty('WQConstriantsChannel');
        IncludeInWQConChkBox.OnClick                  := DoWQConChkBoxOnClick;
        IncludeInWQConChkBox.OnEnter                  := OnEditControlEnter;
        IncludeInWQConChkBox.Visible                  := True;

        //IncludeInWQConChkBox.HasChanges               := False;
        //IncludeInWQConChkBox.HasMetaData              := False;

        IncludeInBoundChkBox.FieldProperty            := FAppModules.FieldProperties.FieldProperty('MinMaxUpperBoundChannel');
        IncludeInBoundChkBox.OnClick                  := DoBoundChkBoxOnClick;
        IncludeInBoundChkBox.OnEnter                  := OnEditControlEnter;
        IncludeInBoundChkBox.Visible                  := True;

        //IncludeInBoundChkBox.HasChanges               := False;
        //IncludeInBoundChkBox.HasMetaData              := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.DestroyMemberObjects;
const OPNAME = 'TMinMaxChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelValidator.Initialise: boolean;
const OPNAME = 'TMinMaxChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMinMaxChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.MinmaxFlowFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.ClearDataViewer;
const OPNAME = 'TMinMaxChannelValidator.ClearDataViewer';
var
  lPanel : TMinMaxChannelDialog;
  lMonth : integer;
  lCols  : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := MinMaxChannelDialog;
    with lPanel do
    begin
      FeatureNameEdit.Text := '';
      //FirmYieldAnalysisChkBox.Checked := FALSE;
      for lMonth := 1 to 12 do
        for lCols := 0 to FlowConstraintGrid.ColCount - 1 do
          FlowConstraintGrid.Cells[lCols, lMonth] := '-1';
      for lCols := 0 to TotalsGrid.ColCount - 1 do
        TotalsGrid.Cells[lCols, 0] := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.PopulateDataViewer;
const OPNAME = 'TMinMaxChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtMinMaxConstraints);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.RePopulateDataViewer;
const OPNAME = 'TMinMaxChannelValidator.RePopulateDataViewer';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lConstraint : IMinMaxFlowConstraint;
  lFieldProperty : TAbstractFieldProperty;
  LWQConstriantData : TWQConstriantData;
  LChannel : IGeneralFlowChannel;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        with MinMaxChannelDialog do
        begin
          lFieldIndex := '';
          LChannel := lConstraint.Channel;
          if lChannel.IFRFeature = nil then
          begin
            LFieldProperty    := FAppModules.FieldProperties.FieldProperty('MinMaxChannelName');
            MinMaxChannelDialog.FeatureNameEdit.FieldProperty := lFieldProperty;
            lKeyValues := lConstraint.GetKeyValues(lFieldProperty.FieldName, lKeyValues);
            FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
            MinMaxChannelDialog.FeatureNameEdit.SetFieldValue(lConstraint.FeatureName);
          end  //FeatureNameEdit.FieldProperty
            else
            begin
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxDistributionTotal');
              MinMaxChannelDialog.FeatureNameEdit.FieldProperty := lFieldProperty;
              MinMaxChannelDialog.FeatureNameEdit.Text := lConstraint.FeatureName;
            end;


          //RePopulateFirmYieldAnalysis;
          RePopulateFlowConstraints;
          if (FAppModules.Model.ModelName = CPlanning) then
          begin
            RePopulateDistributionFactors;
            LWQConstriantData := TPlanningModelDataObject(FAppModules.Model.ModelData).CastWQConstriantData;
            if LWQConstriantData <> nil then
            begin
              IncludeInWQConChkBox.Checked := (LWQConstriantData.WQConstraintsChannelByChannelNo[lConstraint.Channel.ChannelNumber]<>nil);
              IncludeInBoundChkBox.Checked := (LWQConstriantData.MinMaxUpperBoundChannelNo[lConstraint.Channel.ChannelNumber]<>nil);
            end;
          end;
          DisableModelControls
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.DisableModelControls;
const OPNAME = 'TMinMaxChannelValidator.DisableModelControls';
var
  LGrowthFactors : TGrowthFactors;
begin
  try
    with MinMaxChannelDialog do
    begin
      DistributionGroup.Visible  := False;
      GrowthWarningLabel.Visible := False;
      if (FAppModules.Model.ModelName = CPlanning) then
      begin
        //FirmYieldAnalysisChkBox.Enabled := False;
        DistributionGroup.Visible       := True;
        LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
        if (LGrowthFactors <> nil) then
          GrowthWarningLabel.Visible  :=  (LGrowthFactors.CastMinMaxChannelGrowthFactorByMinMaxChannel(FIdentifier) <> nil);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{procedure TMinMaxChannelValidator.RePopulateFirmYieldAnalysis;
const OPNAME = 'TMinMaxChannelValidator.RePopulateFirmYieldAnalysis';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lConstraint    : IMinMaxFlowConstraint;
  lChannel       : IGeneralFlowChannel;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        lChannel := lConstraint.Channel;
        with MinMaxChannelDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := FirmYieldAnalysisChkBox.FieldProperty;
          lKeyValues := lConstraint.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
          FirmYieldAnalysisChkBox.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

          FirmYieldAnalysisChkBox.Enabled := (Uppercase(Trim(lChannel.SummaryOutputRequired)) = 'Y');
          FirmYieldAnalysisChkBox.Checked := (UpperCase(Trim(lChannel.RequiresFirmYieldAnalysis)) = 'Y');
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TMinMaxChannelValidator.RePopulateFlowConstraints;
const OPNAME = 'TMinMaxChannelValidator.RePopulateFlowConstraints';
var
  lConstraint       : IMinMaxFlowConstraint;
  lRow              : integer;
  lArc              : integer;
  lPenaltyStructure : IChannelPenalty;
  lArcCount         : integer;
  lMonths           : TMonthNamesArray;
  lTotal            : double;
  lValue            : double;
  lMonthDays        : TMonthDaysArray;
  lChannel          : IGeneralFlowChannel;
  lKeyValues        : string;
  lHasChanges       : boolean;
  lFieldIndex       : string;
  LFieldProperty    : TAbstractFieldProperty;
begin
  lMonths := nil;
  lMonthDays := nil;
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      lMonthDays := TYieldModelDataObject(FAppModules.Model.ModelData).
                      CastRunConfigurationData.MonthsDaysArray;
      if (lConstraint <> nil) then
      begin
        with MinMaxChannelDialog do
        begin
          for lRow := 1 to 12 do
            FlowConstraintGrid.Cells[0, lRow] := lMonths[lRow];
          TotalsGrid.Cells[0,0] := FAppModules.Language.GetString('Channel.Totals');
          lChannel := lConstraint.Channel;
          lPenaltyStructure := lChannel.ChannelPenalty;
          if (lPenaltyStructure <> nil) then
          begin
            lArcCount := lPenaltyStructure.ChannelPenaltyArcCount;
            FlowConstraintGrid.ColCount := lArcCount + 1;
            TotalsGrid.ColCount         := FlowConstraintGrid.ColCount;
            for lArc := 1 to FlowConstraintGrid.ColCount - 1 do
              FlowConstraintGrid.Cells[lArc, 0] := FAppModules.Language.GetString('Channel.Arc') +
                                                   ' ' + IntToStr(lArc);
            FlowConstraintGrid.Width := (FlowConstraintGrid.DefaultColWidth + 1) * (1 + lArcCount) + 3;
            TotalsGrid.Width         := FlowConstraintGrid.Width;
            TotalsLabel.Left         := TotalsGrid.Left + TotalsGrid.Width + 10;
            FlowConstraintGrid.ClearFieldProperties;
            if lChannel.IFRFeature = nil then
              LFieldProperty    := FAppModules.FieldProperties.FieldProperty('Month')
            else
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxDistributionTotal');

            FlowConstraintGrid.AddFieldProperty(LFieldProperty);
            for lArc := 1 to lArcCount do
            begin
              if lChannel.IFRFeature  = nil  then
                LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints')
              else
                lFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxDistributionTotal');
              FlowConstraintGrid.AddFieldProperty(LFieldProperty);
              if (lArc <= lConstraint.FlowConstraintCount) then
              begin
                lTotal := 0;
                for lRow := 1 to 12 do
                begin
                  if (FAppModules.Model.ModelName = CPlanning) then
                    FlowConstraintGrid.IsColumnEnabled[LRow] := False;

                  lFieldIndex := IntToStr(lArc) + ',' + IntToStr(lRow);
                  lKeyValues  := lConstraint.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
                  lHasChanges := FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
                  FlowConstraintGrid.HasChanges[lArc, lRow] := lHasChanges;
                  FlowConstraintGrid.HasMetaData[lArc,lRow] :=
                     FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
                  if lChannel.IFRFeature = nil then
                    lFieldProperty := FAppModules.FieldProperties.FieldProperty('FlowConstraints')
                  else
                    lFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxDistributionTotal');
                  lValue := lConstraint.FlowConstraintByArcMonth[lArc, lRow];
                  FlowConstraintGrid.Cells[lArc, lRow] := Format(lFieldProperty.FormatStringGrid, [lValue]);//Format('%6.3f', [lValue]);
                  lTotal := lTotal + (lValue * lMonthDays[lRow] * 86400.0);

                  if LChannel.ChannelType in [ctMineToPCDChannel,ctMineToRiverDChannel,ctMineToUndergroundChannel] then
                  begin
                    FlowConstraintGrid.IsRowEnabled[lRow] := False;
                    FlowConstraintGrid.DisabledColor := clBtnFace;
                  end;

                end;
                lFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxDistributionTotal');
                lTotal := lTotal / 1000000;
                TotalsGrid.Cells[lArc, 0] :=Format(lFieldProperty.FormatStringGrid, [lTotal]); //Format('%8.4f', [lTotal]);
              end
              else
              begin
                for lRow := 1 to 12 do
                  FlowConstraintGrid.Cells[lArc, lRow] := '';
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMinMaxChannelValidator.RePopulateDistributionFactors;
const OPNAME = 'TMinMaxChannelValidator.RePopulateDistributionFactors';
var
  LConstraint       : IMinMaxFlowConstraint;
  LRow              : integer;
  LArcCol,
  LArc              : integer;
  LPenaltyStructure : IChannelPenalty;
  LArcCount         : integer;
  LMonths           : TMonthNamesArray;
  LTotal            : double;
  LValue            : double;
  LDistributionValue : double;
  LMonthDays        : TMonthDaysArray;
  LChannel          : IGeneralFlowChannel;
  LDistributionTotal: double;
  LFieldProperty : TAbstractFieldProperty;
begin
  LMonths := nil;
  LMonthDays := nil;
  try
    if (FFeatureID >= 0) then
    begin
      LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
      LMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      LMonthDays := TYieldModelDataObject(FAppModules.Model.ModelData).
                      CastRunConfigurationData.MonthsDaysArray;
      if (LConstraint <> nil) then
      begin
        with MinMaxChannelDialog do
        begin
          for LRow := 1 to 12 do
            DistributionGrid.Cells[0, lRow] := LMonths[LRow];
          TotalsGrid.Cells[0,0] := FAppModules.Language.GetString('Channel.Totals');
          DistributionTotalsGrid.Cells[0,0] := FAppModules.Language.GetString('Channel.Totals');
          DistributionGrid.Cells[0,0] := 'Months';
          LChannel := LConstraint.Channel;
          LPenaltyStructure := LChannel.ChannelPenalty;
          if (LPenaltyStructure <> nil) then
          begin

            LArcCount := LPenaltyStructure.ChannelPenaltyArcCount;
            DistributionGrid.ColCount := (LArcCount*2)+1;
            DistributionTotalsGrid.ColCount         := DistributionGrid.ColCount;
            DistributionGrid.Width := ((DistributionGrid.DefaultColWidth) * (DistributionGrid.ColCount))-40;
            DistributionTotalsGrid.Width         := DistributionGrid.Width;
            DistributionGridHeader.ColCount := 1;
            DistributionGridHeader.Width := DistributionGrid.Width;
            DistributionGrid.ClearFieldProperties;
            DistributionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
            DistributionGrid.RowHeights[0] := 40;
            DistributionGrid.ColWidths[0] := 40;
            DistributionTotalsGrid.ColWidths[0] := 40;
            SetFlowConstraintGridCols(LArcCount);
            for LArc := 1 to LArcCount do
            begin
              DistributionGridHeader.Cells[lArc-1, 0] := FAppModules.Language.GetString('Channel.Arc') +
                                                   ' ' + IntToStr(LArc);
              if (LArc <= LConstraint.FlowConstraintCount) then
              begin
                LTotal := 0;
                LDistributionTotal := 0;
                for LRow := 1 to 12 do
                begin
                  if LArc > 1 then
                    LArcCol := LArc + 1
                  else
                    LArcCol := LArc;

                  LFieldProperty := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
                  DistributionGrid.AddFieldProperty(LFieldProperty);
                  LValue := LConstraint.FlowConstraintByArcMonth[LArc, LRow];
                  LDistributionValue := LConstraint.DistributionByArcMonth[LArc, LRow];
                  DistributionGrid.Cells[LArcCol, LRow] :=Format(lFieldProperty.FormatStringGrid, [LValue]); //Format('%6.3f', [LValue]);
                  DistributionGrid.Cells[LArcCol, 0] := FAppModules.Language.GetString('Channel.FlowConstraints');

                  LFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxDistribution');
                  DistributionGrid.AddFieldProperty(LFieldProperty);
                  DistributionGrid.Cells[LArcCol+1, LRow] := Format(lFieldProperty.FormatStringGrid, [LDistributionValue]);
                  DistributionGrid.Cells[LArcCol+1, 0] := FAppModules.Language.GetString('Channel.DistributionFactors');
                  LDistributionTotal := LDistributionTotal + LDistributionValue;
                  LTotal := LTotal + (LValue * LMonthDays[LRow] * 86400.0);
                end;
                LTotal := LTotal / 1000000;
                LFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxDistributionTotal');
                DistributionGrid.AddFieldProperty(LFieldProperty);
                DistributionTotalsGrid.Cells[LArcCol, 0]  := Format(LFieldProperty.FormatStringGrid, [LTotal]);
                DistributionTotalsGrid.Cells[LArcCol+1, 0]:= Format(LFieldProperty.FormatStringGrid, [LDistributionTotal]);
              end
              else
              begin
                for LRow := 1 to 12 do
                  DistributionGrid.Cells[LArc, LRow] := '';
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMinMaxChannelValidator.SaveState: boolean;
const OPNAME = 'TMinMaxChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelValidator.MinMaxChannelDialog : TMinMaxChannelDialog;
const OPNAME = 'TMinMaxChannelValidator.MinMaxChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TMinMaxChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMinMaxChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'SummaryOutputRequired') then
    begin
      //RePopulateFirmYieldAnalysis;
      DisableModelControls;
    end
    else
    if (AFieldName = 'PenaltyNumber') then
    begin
      if (FAppModules.Model.ModelName = CPlanning) then
        RePopulateDistributionFactors;
      RePopulateFlowConstraints;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TMinMaxChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMinMaxChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMinMaxChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with MinMaxChannelDialog do
    begin
      if ((Sender = FeatureNameEdit) AND
          (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.UpdateFeatureName;
const OPNAME = 'TMinMaxChannelValidator.UpdateFeatureName';
var
  lFeature : IMinMaxFlowConstraint;
  lMessage : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with MinMaxChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FeatureNameEdit.FieldProperty.FieldName,
            FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
          DoContextValidation(dvtMinMaxFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMinMaxChannelValidator.UpdateChannelFirmYieldAnalysis;
const OPNAME = 'TMinMaxChannelValidator.UpdateChannelFirmYieldAnalysis';
var
  lConstraint : IMinMaxFlowConstraint;
  lChannel    : IGeneralFlowChannel;
  lOldValue   : string;
  lNewValue   : string;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[fFeatureID];
    if (lConstraint <> nil) then
    begin
      lChannel := lConstraint.Channel;
      with MinMaxChannelDialog do
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

procedure TMinMaxChannelValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMinMaxChannelValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with MinMaxChannelDialog do
    begin
      if ((FlowConstraintGrid = ASender) AND (ACol > 0) AND
          (NOT FlowConstraintGrid.HasChanges[ACol,ARow])) then
        UpdateMonthlyFlowConstraints(ACol, ARow, Trim(FlowConstraintGrid.Cells[ACol, ARow]));

      if (FAppModules.Model.ModelName = CPlanning) and (DistributionGrid = ASender) and (ACol > 0) and (ACol mod 2 <> 0) then
        if (ACol = 1) then
          UpdateMonthlyFlowConstraintsDistribution(ACol, ARow, Trim(DistributionGrid.Cells[ACol, ARow]))
        else
          UpdateMonthlyFlowConstraintsDistribution(ACol-1, ARow, Trim(DistributionGrid.Cells[ACol, ARow]));

      if (FAppModules.Model.ModelName = CPlanning) and (DistributionGrid = ASender) and (ACol > 0) and (ACol mod 2 = 0 ) then
        if (ACol = 2) then
          UpdateMonthlyDistributionFactors(ACol-1, ARow, Trim(DistributionGrid.Cells[ACol, ARow]))
        else
           UpdateMonthlyDistributionFactors(ACol-2, ARow, Trim(DistributionGrid.Cells[ACol, ARow]));

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.UpdateMonthlyFlowConstraints(AIndex : integer;
                                                               AMonth : integer;
                                                               AValue : string);
const OPNAME = 'TMinMaxChannelValidator.UpdateMonthlyFlowConstraints';
var
  lConstraint : IMinMaxFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with MinMaxChannelDialog do
      begin
        FlowConstraintGrid.ValidationError[AIndex, AMonth, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FlowConstraintGrid.FieldProperty(1).FieldName,
            AValue,lMessage, AIndex, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          if (AIndex > lConstraint.FlowConstraintCount) then
            lConstraint.CreateFlowConstraints;
          lConstraint.FlowConstraintByArcMonth[AIndex,AMonth] := lValue;
          RepopulateFlowConstraints;
          if (FAppModules.Model.ModelName = CPlanning) then
          begin
            LConstraint.CalculateDistributionFactors;
            RePopulateDistributionFactors;
          end;
          DoContextValidation(dvtMinMaxFlowConstraints);
        end
        else
          FlowConstraintGrid.ValidationError[AIndex, AMonth, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.UpdateMonthlyFlowConstraintsDistribution(AIndex : integer;
                                                               AMonth : integer;
                                                               AValue : string);
const OPNAME = 'TMinMaxChannelValidator.UpdateMonthlyFlowConstraintsDistribution';
var
  lConstraint : IMinMaxFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with MinMaxChannelDialog do
      begin
        DistributionGrid.ValidationError[AIndex, AMonth, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DistributionGrid.FieldProperty(1).FieldName,
            AValue,lMessage, AIndex, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          if (AIndex > lConstraint.FlowConstraintCount) then
            lConstraint.CreateFlowConstraints;
          lConstraint.FlowConstraintByArcMonth[AIndex,AMonth] := lValue;
          LConstraint.CalculateDistributionFactors;
          RepopulateFlowConstraints;
          RePopulateDistributionFactors;
          DoContextValidation(dvtMinMaxDistribution);
        end
        else
          DistributionGrid.ValidationError[AIndex, AMonth, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMinMaxChannelValidator.UpdateMonthlyDistributionFactors(AIndex : integer;AMonth : integer;AValue : string);
const OPNAME = 'TMinMaxChannelValidator.UpdateMonthlyDistributionFactors';
var
  lConstraint : IMinMaxFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with MinMaxChannelDialog do
      begin
        DistributionGrid.ValidationError[AIndex, AMonth, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DistributionGrid.FieldProperty(1).FieldName,
            AValue,lMessage, AIndex, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          if (AIndex > lConstraint.FlowConstraintCount) then
            lConstraint.CreateFlowConstraints;
          lConstraint.DistributionByArcMonth[AIndex,AMonth] := lValue;
          LConstraint.CalculateDistributionFactors;
          RepopulateFlowConstraints;
          RePopulateDistributionFactors;
//          DoContextValidation(dvtMinMaxFlowConstraints);
          DoContextValidation(dvtMinMaxDistribution);
        end
        else
         DistributionGrid.ValidationError[AIndex, AMonth, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMinMaxChannelValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMinMaxChannelValidator.DoContextValidation';
var
  lFeature     : IMinMaxFlowConstraint;
  lFeatureList : IMinMaxFlowConstraintList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MinMaxFlowConstraintList;
      lFeature := lFeatureList.MinMaxFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtMinMaxConstraints, dvtMinMaxFeatureName]) then
          ValidateMinMaxFeatureName(lFeature);
        if (AValidationType in [dvtMinMaxConstraints, dvtMinMaxFlowConstraints]) then
          ValidateMinMaxFlowConstraint(lFeature);
{        if (AValidationType = dvtMinMaxDistribution) then
          ValidateMinMaxDistributionFactors(lFeature);
}          
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TMinMaxChannelValidator.DetermineWizardStatus';
var
  lFeature        : IMinMaxFlowConstraint;
  lFeatureList    : IMinMaxFlowConstraintList;
  lMinFlowDemand  : TAbstractFieldProperty;
  lNotZero        : Boolean;
  lIndex          : integer;
  lCount          : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MinMaxFlowConstraintList;
      lFeature := lFeatureList.MinMaxFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtMinMaxConstraints);
        lMinFlowDemand := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
        lNotZero := FALSE;
        lCount   := 0;
        while ((NOT lNotZero) AND (lCount < lFeature.FlowConstraintCount)) do
        begin
          lIndex := lMinFlowDemand.ArrayLowDimTwo;
          while ((NOT lNotZero) AND (lIndex <= lMinFlowDemand.ArrayHighDimTwo)) do
          begin
            if (lFeature.FlowConstraintByArcMonth[lCount,lIndex] > 0) then
              lNotZero := TRUE
            else
              lIndex := lIndex + 1;
          end;
          lCount := lCount + 1;
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

procedure TMinMaxChannelValidator.ValidateMinMaxFeatureName(AFeature: IMinMaxFlowConstraint);
const OPNAME = 'TMinMaxChannelValidator.ValidateMinMaxFeatureName';
begin
  try
    with MinMaxChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MinMaxFeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxChannelValidator.ValidateMinMaxFlowConstraint(AFeature: IMinMaxFlowConstraint);
const OPNAME = 'TMinMaxChannelValidator.ValidateMinMaxFlowConstraint';
var
  lErrorCols     : TStringList;
  lIndex         : integer;
  lErrorMessages : TStringList;
begin
  try
    if (AFeature <> nil) then
    begin
      with MinMaxChannelDialog do
      begin
        FErrorMessage := '';
        lErrorCols := TStringList.Create;
        if (AFeature.Validate(FErrorMessage, 'MinMaxFlowConstraint')) then
        begin
          for lIndex := 1 to AFeature.FlowConstraintCount do
            FlowConstraintGrid.ValidationError[lIndex, 0, gveColContext] := '';
        end
        else
        begin
          lErrorMessages := TStringList.Create;
          try
            ExtractErrorsAndColumns(FErrorMessage,lErrorMessages,lErrorCols);
            for lIndex := 1 to AFeature.Channel.ChannelPenalty.ChannelPenaltyArcCount do
            begin
              if (lErrorCols.IndexOf(IntToStr(lIndex)) >= 0) then
                FlowConstraintGrid.ValidationError[lIndex, 0, gveColContext] := lErrorMessages.Text
              else
                FlowConstraintGrid.ValidationError[lIndex, 0, gveColContext] := '';
            end;
            FAllErrorMessages.AddStrings(lErrorMessages);
          finally
            FreeAndNil(lErrorMessages);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.ValidateMinMaxDistributionFactors(AFeature: IMinMaxFlowConstraint);
const OPNAME = 'TMinMaxChannelValidator.ValidateMinMaxDistributionFactors';
var
  lErrorCols     : TStringList;
  lIndex         : integer;
  lErrorMessages : TStringList;
begin
  try
    if (AFeature <> nil) then
    begin
      with MinMaxChannelDialog do
      begin
        FErrorMessage := '';
        lErrorCols := TStringList.Create;
        if (AFeature.Validate(FErrorMessage, 'MinMaxDistribution')) then
        begin
          for lIndex := 1 to AFeature.FlowConstraintCount do
            DistributionGrid.ValidationError[lIndex, 0, gveColContext] := '';
        end
        else
        begin
          lErrorMessages := TStringList.Create;
          try
            ExtractErrorsAndColumns(FErrorMessage,lErrorMessages,lErrorCols);
            for lIndex := 1 to AFeature.Channel.ChannelPenalty.ChannelPenaltyArcCount do
            begin
              if (lErrorCols.IndexOf(IntToStr(lIndex)) >= 0) then
               DistributionGrid.ValidationError[lIndex+1, 0, gveColContext] := lErrorMessages.Text
              else
                DistributionGrid.ValidationError[lIndex+1, 0, gveColContext] := '';
            end;
            FAllErrorMessages.AddStrings(lErrorMessages);
          finally
            FreeAndNil(lErrorMessages);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMinMaxChannelValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TMinMaxChannelValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFeature       : IMinMaxFlowConstraint;
  lFieldIndex    : string;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with MinMaxChannelDialog do
        begin
          if (FActiveControl = FlowConstraintGrid) then
          begin
            lFieldIndex := IntToStr(FlowConstraintGrid.Col) + ',' + IntToStr(FlowConstraintGrid.Row);
            lFieldProperty := FlowConstraintGrid.FieldProperty(1);
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

function TMinMaxChannelValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TMinMaxChannelValidator.ProcessMetaDataEvent';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IMinMaxFlowConstraint;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with MinMaxChannelDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
             lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = FlowConstraintGrid) then
          begin
            lFieldIndex := IntToStr(FlowConstraintGrid.Col) + ',' + IntToStr(FlowConstraintGrid.Row);
            lFieldProperty := FlowConstraintGrid.FieldProperty(FlowConstraintGrid.Col);
          end;
          {if (FActiveControl = FirmYieldAnalysisChkBox) then
            lFieldProperty := FirmYieldAnalysisChkBox.FieldProperty;}

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

procedure TMinMaxChannelValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TMinMaxChannelValidator.OnAfterPasteColumnData';
var
  LConstraint : IMinMaxFlowConstraint;
  LValue      : double;
  LRow,
  LIndex      : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil) then
    begin
      if(Sender = MinMaxChannelDialog.FlowConstraintGrid) then
      begin
        LIndex := MinMaxChannelDialog.FlowConstraintGrid.Col;
        if(LIndex <= LConstraint.FlowConstraintCount) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(MinMaxChannelDialog.FlowConstraintGrid.Cells[LIndex,LRow]));
            LConstraint.FlowConstraintByArcMonth[LIndex,LRow] := LValue;
          end;
        end;
      RepopulateFlowConstraints;
      DoContextValidation(dvtMinMaxFlowConstraints);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TMinMaxChannelValidator.OnAfterPasteGridData';
var
  LConstraint : IMinMaxFlowConstraint;
  LValue      : double;
  LRow,
  LCol        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil) then
    begin
      if(Sender = MinMaxChannelDialog.FlowConstraintGrid) then
      begin
        for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
        begin
          for LCol := TFieldStringGrid(Sender).FixedCols to TFieldStringGrid(Sender).ColCount - 1 do
          begin
            if(LCol <= LConstraint.FlowConstraintCount) then
            begin
              LValue := StrToFloat(Trim(MinMaxChannelDialog.FlowConstraintGrid.Cells[LCol,LRow]));
              LConstraint.FlowConstraintByArcMonth[LCol,LRow] := LValue;
            end;
          end;
        end;
      RepopulateFlowConstraints;
      DoContextValidation(dvtMinMaxFlowConstraints);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.DoWQConChkBoxOnClick(Sender : TObject);
const OPNAME = 'TMinMaxChannelValidator.DoWQConChkBoxOnClick';
var
  LWQConstriantData : TWQConstriantData;
  LWQConstriantsChannel : IWQConstriantsChannel;
  LConstraint : IMinMaxFlowConstraint;
  LChannelNumber : integer;
begin
  try
    LWQConstriantData := TPlanningModelDataObject(FAppModules.Model.ModelData).CastWQConstriantData;
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if LWQConstriantData <> nil then
    begin
      if MinMaxChannelDialog.IncludeInWQConChkBox.Checked then
      begin
        if(LConstraint <> nil) then
          LWQConstriantsChannel := LWQConstriantData.NewWQConstraintsChannels(LConstraint.Channel.ChannelNumber);
      end
      else
      begin
        LWQConstriantsChannel := LWQConstriantData.WQConstraintsChannelByChannelNo[LConstraint.Channel.ChannelNumber];
        if LWQConstriantsChannel <> nil then
        begin
          LChannelNumber        := LWQConstriantsChannel.ChannelNumber;
          LWQConstriantsChannel := nil;
          LWQConstriantData.RemoveWQConstriantsChannel(LChannelNumber);
        end;
      end;
    end;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'WQConstriantsChannel','',IntToStr(LConstraint.Channel.ChannelNumber));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxChannelValidator.DoBoundChkBoxOnClick(Sender : TObject);
const OPNAME = 'TMinMaxChannelValidator.DoBoundChkBoxOnClick';
var
  LWQConstriantData : TWQConstriantData;
  LMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel;
  LConstraint : IMinMaxFlowConstraint;
  LChannelNumber : integer;
begin
  try
    LWQConstriantData := TPlanningModelDataObject(FAppModules.Model.ModelData).CastWQConstriantData;
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[FFeatureID];
    if LWQConstriantData <> nil then
    begin
      if MinMaxChannelDialog.IncludeInBoundChkBox.Checked then
      begin
        if(LConstraint <> nil) then
          LMinMaxUpperBoundChannel := LWQConstriantData.NewMinMaxUpperBoundChannel(LConstraint.Channel.ChannelNumber);
      end
      else
      begin
        LMinMaxUpperBoundChannel := LWQConstriantData.MinMaxUpperBoundChannelNo[LConstraint.Channel.ChannelNumber];
        if LMinMaxUpperBoundChannel <> nil then
        begin
          LChannelNumber           := LMinMaxUpperBoundChannel.ChannelNumber;
          LMinMaxUpperBoundChannel := nil;
          LWQConstriantData.RemoveMinMaxUpperBoundChannel(LChannelNumber);
        end;
      end;
    end;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinMaxUpperBoundChannel','',IntToStr(LConstraint.Channel.ChannelNumber));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMinMaxChannelValidator.OnFirmYieldAnalysisClick(Sender: TObject);
const OPNAME = 'TMinMaxChannelValidator.OnFirmYieldAnalysisClick';
begin
  try
    if(MinMaxChannelDialog.FirmYieldAnalysisChkBox.HasValueChanged) then
      UpdateChannelFirmYieldAnalysis;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.

