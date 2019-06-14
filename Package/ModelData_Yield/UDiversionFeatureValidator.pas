{******************************************************************************}
{*  UNIT      : Contains the class TDiversionFeatureValidator.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UDiversionFeatureValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Graphics,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UDiversionFeatureDialog;

type
  TDiversionFeatureValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnAfterPasteType1OrType2RowsData(Sender: TObject);
    procedure OnAfterPasteType3ProportionAndLevelRowsData(Sender: TObject);
    procedure OnAfterPasteType1OrType2ColumnData(Sender: TObject);
    procedure OnAfterPasteType3ColumnData(Sender: TObject);
    procedure OnAfterPasteType3ProportionColumnData(Sender: TObject);

    procedure OnbtnGetStationsClick(Sender : TObject);

    procedure RePopulateDataViewer;
    procedure OnDiversionTypeChange(Sender: TObject);
    procedure OnInsertRow(Sender: TObject);
    procedure OnDeleteRow(Sender: TObject);
    procedure ImportRelationship;
    procedure RePopulateType1Data;
    procedure RePopulateType2Data;
    procedure RePopulateType3Data;
    procedure ResizeType3Grids;
    procedure RePopulateType3Grids;
    procedure LevelsGridTopLeftChanged(Sender: TObject);
    procedure FlowsGridTopLeftChanged(Sender: TObject);
    procedure ProportionsGridTopLeftChanged(Sender: TObject);
    procedure UpdateDivFeatureName;
    procedure UpdateDiversionType;
    procedure UpdateDiversionDemand (AIndex : integer;
                                     AValue : string);
    procedure UpdateNetNaturalInflow (AIndex : integer;
                                      AValue : string);
    procedure UpdateFlowRange (AIndex : integer;
                               AValue : string);
    procedure UpdateActualDivertedFlow (AIndex : integer;
                                        AValue : string);
    procedure UpdateNrOfFlows;
    procedure UpdateNrOfLevels;
    procedure UpdateReferenceFlow (AIndex : integer;
                                   AValue : string);
    procedure UpdateReservoirLevel (AIndex : integer;
                                    AValue : string);
    procedure UpdateFlowProportion (AFlowIndex  : integer;
                                    ALevelIndex : integer;
                                    AValue      : string);
    procedure UpdateControllingReservoir;
    procedure ValidateFeatureName (AFeature : IDiversionFeature);
    procedure ValidateFeatureType (AFeature : IDiversionFeature);
    procedure ValidateControllingReservoir (AFeature : IDiversionFeature);
    procedure ValidateType1DiversionDemands (AFeature : IDiversionFeature);
    procedure ValidateType1NetNaturalInflows (AFeature : IDiversionFeature);
    procedure ValidateType2FlowRanges (AFeature : IDiversionFeature);
    procedure ValidateType2ActualDivertedFlows (AFeature : IDiversionFeature);
    procedure ValidateType3Levels (AFeature : IDiversionFeature);
    procedure ValidateType3Flows (AFeature : IDiversionFeature);
    procedure ValidateType3Proportions (AFeature : IDiversionFeature);
    procedure ResetButtons;
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
    function DiversionFeatureDialog : TDiversionFeatureDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  VCL.Dialogs,
  VCL.Forms,
  SysUtils,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations, VCL.Grids,
  UImportDiversionDataDialog;

{******************************************************************************}
{* TDiversionFeatureValidator                                                 *}
{******************************************************************************}

procedure TDiversionFeatureValidator.CreateMemberObjects;
const OPNAME = 'TDiversionFeatureValidator.CreateMemberObjects';
var
  lPanel : TDiversionFeatureDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TDiversionFeatureDialog.Create(FPanelOwner,FAppModules);
    lPanel := DiversionFeatureDialog;
    with lPanel do
    begin
      DivFeatureNameEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('DiversionChannelName');
      DivFeatureNameEdit.OnEnter         := OnEditControlEnter;
      DivFeatureNameEdit.OnExit          := OnEditControltExit;

      DivTypeRadioGroup.FieldProperty    := FAppModules.FieldProperties.FieldProperty('DiversionChannelType');
      DivTypeRadioGroup.OnEnter          := OnEditControlEnter;
      DivTypeRadioGroup.OnClick          := OnDiversionTypeChange;

      Type1Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      Type1Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DiversionDemand'));
      Type1Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NetNaturalInflow'));
      Type1Grid.OnBeforeCellChange       := OnStringGridCellDataHasChanged;
      Type1Grid.OnColEnter               := OnStringGridColEnter;
      Type1Grid.OnExit                   := OnEditControltExit;
      Type1Grid.OnEnter                  := OnEditControlEnter;
      Type1Grid.ShowGridPopupMenu        := True;
      Type1Grid.AllowPasteFromExcel      := True;
      Type1Grid.OnPasteFromExcel         := Self.OnAfterPasteType1OrType2RowsData;
      Type1Grid.OnAfterPasteColumnData   := Self.OnAfterPasteType1OrType2ColumnData;
      Type1Grid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteType1OrType2RowsData;

      Type2Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      Type2Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('FlowRange'));
      Type2Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ActualDivertedFlow'));
      Type2Grid.OnBeforeCellChange       := OnStringGridCellDataHasChanged;
      Type2Grid.OnColEnter               := OnStringGridColEnter;
      Type2Grid.OnEnter                  := OnEditControlEnter;
      Type2Grid.ShowGridPopupMenu        := True;
      Type2Grid.AllowPasteFromExcel      := True;
      Type2Grid.OnPasteFromExcel         := Self.OnAfterPasteType1OrType2RowsData;
      Type2Grid.OnAfterPasteColumnData   := Self.OnAfterPasteType1OrType2ColumnData;
      Type2Grid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteType1OrType2RowsData;

      Type3FlowsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('FlowValue'));
      Type3FlowsGrid.OnTopLeftChanged    := FlowsGridTopLeftChanged;
      Type3FlowsGrid.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      Type3FlowsGrid.OnColEnter          := OnStringGridColEnter;
      Type3FlowsGrid.OnEnter             := OnEditControlEnter;
      Type3FlowsGrid.ShowGridPopupMenu   := True;
      Type3FlowsGrid.AllowPasteFromExcel := True;
      Type3FlowsGrid.OnPasteFromExcel     := Self.OnAfterPasteType3ColumnData;
      Type3FlowsGrid.OnAfterPasteColumnData := Self.OnAfterPasteType3ColumnData;

      Type3LevelsGrid.OnTopLeftChanged    := LevelsGridTopLeftChanged;
      Type3LevelsGrid.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      Type3LevelsGrid.OnColEnter          := OnStringGridColEnter;
      Type3LevelsGrid.OnEnter             := OnEditControlEnter;
      Type3LevelsGrid.ShowGridPopupMenu   := True;
      Type3LevelsGrid.AllowPasteFromExcel := True;
      Type3LevelsGrid.OnPasteFromExcel     := Self.OnAfterPasteType3ProportionAndLevelRowsData;
      Type3LevelsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteType3ProportionAndLevelRowsData;

      Type3ProportionsGrid.OnTopLeftChanged    := ProportionsGridTopLeftChanged;
      Type3ProportionsGrid.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      Type3ProportionsGrid.OnColEnter          := OnStringGridColEnter;
      Type3ProportionsGrid.OnEnter             := OnEditControlEnter;
      Type3ProportionsGrid.ShowGridPopupMenu   := True;
      Type3ProportionsGrid.AllowPasteFromExcel := True;
      Type3ProportionsGrid.OnPasteFromExcel       := Self.OnAfterPasteType3ProportionAndLevelRowsData;
      Type3ProportionsGrid.OnAfterPasteColumnData := Self.OnAfterPasteType3ProportionColumnData;
      Type3ProportionsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteType3ProportionAndLevelRowsData;

      ReservoirCbx.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ControllingResNodeNumber');
      ReservoirCbx.OnEnter               := OnEditControlEnter;
      ReservoirCbx.OnExit                := OnEditControltExit;

      NrOfFlowsEdit.FieldProperty        := FAppModules.FieldProperties.FieldProperty('ReferenceFlowsCount');
      NrOfFlowsEdit.OnEnter              := OnEditControlEnter;
      NrOfFlowsEdit.OnExit               := OnEditControltExit;

      NrOfLevelsEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ReservoirStorageNumber');
      NrOfLevelsEdit.OnEnter             := OnEditControlEnter;
      NrOfLevelsEdit.OnExit              := OnEditControltExit;

      MinimumEdit.OnEnter                := OnEditControlEnter;
      MaximumEdit.OnEnter                := OnEditControlEnter;

      BtnInsertRow.OnClick               := OnInsertRow;
      BtnDeleteRow.OnClick               := OnDeleteRow;

      cbxStations.FieldProperty          := FAppModules.FieldProperties.FieldProperty('DivStation');
      cbxStations.OnExit                 := OnEditControltExit;
      btnGetStations.OnClick             := OnbtnGetStationsClick;
      //DiversionRelationshipBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionRelationship');
      //DiversionRelationshipBtn.OnEnter       := OnEditControlEnter;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.DestroyMemberObjects;
const OPNAME = 'TDiversionFeatureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.Initialise: boolean;
const OPNAME = 'TDiversionFeatureValidator.Initialise';
var
  lLanguage : TAbstractLanguage;
  LDivFeatureList : IDiversionFeatureList;
  LDiversionList : TStringList;
  LIndex : integer;
  LStationID : integer;
begin
  Result := inherited Initialise;
  try
    lLanguage := FAppModules.Language;
    with DiversionFeatureDialog.DivTypeRadioGroup do
    begin
      Items.Clear;
      Items.Add(lLanguage.GetString('NetworkFeatures.DiversionType1'));
      Items.Add(lLanguage.GetString('NetworkFeatures.DiversionType2'));
      Items.Add(lLanguage.GetString('NetworkFeatures.DiversionType3'));
      Items.Add(lLanguage.GetString('NetworkFeatures.DiversionType4'));
    end;
    LDivFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList;
    if LDivFeatureList <> nil then
    begin
      LDiversionList := TStringList.Create;
      try
        LDiversionList.CommaText := LDivFeatureList.DiversionGaugeList;
        for LIndex := 0 to LDiversionList.Count-1 do
        begin
          LStationID := LDivFeatureList.GetStationIDByName(LDiversionList[LIndex]);
          DiversionFeatureDialog.cbxStations.Items.AddObject(LDiversionList[LIndex],TObject(LStationID));
        end;
      finally
        LDiversionList.Free;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.LanguageHasChanged: boolean;
const OPNAME = 'TDiversionFeatureValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.DiversionFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ClearDataViewer;
const OPNAME = 'TDiversionFeatureValidator.ClearDataViewer';
var
  lPanel : TDiversionFeatureDialog;
  lRow   : integer;
  lCol   : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := DiversionFeatureDialog;
    with lPanel do
    begin
      DivFeatureNameEdit.SetFieldValue('');
      DivTypeRadioGroup.ItemIndex := -1;
      for lCol := 0 to Type1Grid.ColCount - 1 do
        for lRow := 0 to Type1Grid.RowCount - 1 do
          Type1Grid.Cells[lCol, lRow] := '-1';
      for lCol := 0 to Type2Grid.ColCount - 1 do
        for lRow := 0 to Type2Grid.RowCount - 1 do
          Type2Grid.Cells[lCol, lRow] := '-1';
      for lRow := 0 to Type3FlowsGrid.RowCount - 1 do
        Type3FlowsGrid.Cells[0, lRow] := '-1';
      for lCol := 0 to Type3LevelsGrid.ColCount - 1 do
      begin
        Type3LevelsGrid.Cells[lCol, 0] := '-1';
        for lRow := 0 to Type3FlowsGrid.RowCount - 1 do
          Type3ProportionsGrid.Cells[lCol, lRow] := '-1';
      end;
      ReservoirCbx.ItemIndex := -1;
      ReservoirCbx.Items.Clear;
      NrOfFlowsEdit.Text     := '-1';
      NrOfLevelsEdit.Text    := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.PopulateDataViewer;
const OPNAME = 'TDiversionFeatureValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtDivFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.RePopulateDataViewer;
const OPNAME = 'TDiversionFeatureValidator.RePopulateDataViewer';
var
  lDivFeature    : IDiversionFeature;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          //DiversionRelationshipBtn.Visible := TRUE;
          //lFieldProperty := DiversionRelationshipBtn.FieldProperty;
          //lKeyValues     := lDivFeature.GetKeyValues(lFieldProperty.FieldName, '');
          //DiversionRelationshipBtn.HasMetaData :=
            //FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;

          lFieldProperty := DivFeatureNameEdit.FieldProperty;
          lKeyValues     := lDivFeature.GetKeyValues(lFieldProperty.FieldName, '');
          DivFeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          DivFeatureNameEdit.SetFieldValue(lDivFeature.FeatureName);

          lFieldProperty := DivTypeRadioGroup.FieldProperty;
          DivTypeRadioGroup.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          DivTypeRadioGroup.ItemIndex  := lDivFeature.DiversionType - 1;
//          OnDiversionTypeChange(Self);
          RePopulateType1Data;
          RePopulateType2Data;
          RePopulateType3Data;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.RePopulateType1Data;
const OPNAME = 'TDiversionFeatureValidator.RePopulateType1Data';
var
  lDivFeature : IDiversionFeature;
  lRow        : integer;
  lMonths     : TMonthNamesArray;
  lValue      : double;
  lFieldProp1 : TAbstractFieldProperty;
  lFieldProp2 : TAbstractFieldProperty;
  lFieldIndex : string;
  lKeyValues  : string;
begin
  lMonths := nil;
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastRunConfigurationData.MonthNamesArray;
        with DiversionFeatureDialog do
        begin
          Type1Grid.Cells[0, 0] := '';
          lFieldProp1 := Type1Grid.FieldProperty(1);
          lFieldProp2 := Type1Grid.FieldProperty(2);
          for lRow := 1 to 12 do
          begin
            lFieldIndex := IntToStr(lRow);
            Type1Grid.Cells[0, lRow] := lMonths[lRow];
            lKeyValues := lDivFeature.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
            Type1Grid.HasMetaData[1, lRow] := FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
            lValue := lDivFeature.DiversionDemandByIndex[lRow];
            if (lValue <> NullFloat) then
              Type1Grid.Cells[1, lRow] := Format(lFieldProp1.FormatStringGrid {'%6.2f'}, [lValue])
            else
              Type1Grid.Cells[1, lRow] := '';
            lKeyValues := lDivFeature.GetKeyValues(lFieldProp2.FieldName, lFieldIndex);
            Type1Grid.HasMetaData[2, lRow] := FAppModules.MetaData.FindMetaData(lFieldProp2.FieldName, lKeyValues, lFieldIndex) <> nil;
            lValue := lDivFeature.DivertedFlowByIndex[lRow];
            if (lValue <> NullFloat) then
              Type1Grid.Cells[2, lRow] := Format(lFieldProp2.FormatStringGrid {'%6.2f'}, [lValue])
            else
              Type1Grid.Cells[2, lRow] := '';
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.RePopulateType2Data;
const OPNAME = 'TDiversionFeatureValidator.RePopulateType2Data';
var
  lDivFeature : IDiversionFeature;
  lRow        : integer;
  lMonths     : TMonthNamesArray;

  LDivertedFlow,
  LDiversionDemandValue : double;

  lFieldProp1 : TAbstractFieldProperty;
  lFieldProp2 : TAbstractFieldProperty;
  lFieldIndex : string;
  lKeyValues  : string;
begin
  lMonths := nil;
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastRunConfigurationData.MonthNamesArray;
        with DiversionFeatureDialog do
        begin
          //Type2HeadLabel1.Visible := lDivFeature.DiversionType = 2;
          //Type2HeadLabel2.Visible := lDivFeature.DiversionType = 2;
          //Type4HeadLabel1.Visible := lDivFeature.DiversionType = 4;
          //Type4HeadLabel2.Visible := lDivFeature.DiversionType = 4;
          cbxStations.ItemIndex := cbxStations.Items.IndexOf(lDivFeature.Station);
          FlowDiversionRelationshipLineSeries.Clear;
          if lDivFeature.DiversionType = 2 then
          begin
            Type2Grid.Cells[1,0] := FAppModules.Language.GetString('NetworkFeatures.DiversionDemand2');
            Type2Grid.Cells[2,0] := FAppModules.Language.GetString('NetworkFeatures.NetNaturalInflow2');
          end
          else
          if lDivFeature.DiversionType = 4 then
          begin
            Type2Grid.Cells[1,0] := FAppModules.Language.GetString('NetworkFeatures.DiversionDemand4');
            Type2Grid.Cells[2,0] := FAppModules.Language.GetString('NetworkFeatures.NetNaturalInflow4');
          end;

          Type2Grid.Cells[0, 0] := '';
          lFieldProp1 := Type2Grid.FieldProperty(1);
          lFieldProp2 := Type2Grid.FieldProperty(2);
          for lRow := 1 to 12 do
          begin
            lFieldIndex := IntToStr(lRow);
            Type2Grid.Cells[0, lRow] := IntToStr(lRow);
            lKeyValues := lDivFeature.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
            Type2Grid.HasMetaData[1, lRow] := FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
            LDiversionDemandValue := lDivFeature.DiversionDemandByIndex[lRow];
            if (LDiversionDemandValue <> NullFloat) then
              Type2Grid.Cells[1, lRow] := Format(lFieldProp1.FormatStringGrid {'%6.2f'}, [LDiversionDemandValue])
            else
              Type2Grid.Cells[1, lRow] := '';
            lKeyValues := lDivFeature.GetKeyValues(lFieldProp2.FieldName, lFieldIndex);
            Type2Grid.HasMetaData[2, lRow] := FAppModules.MetaData.FindMetaData(lFieldProp2.FieldName, lKeyValues, lFieldIndex) <> nil;
            LDivertedFlow := lDivFeature.DivertedFlowByIndex[lRow];
            if (LDivertedFlow <> NullFloat) then
              Type2Grid.Cells[2, lRow] := Format(lFieldProp2.FormatStringGrid {'%6.2f'}, [LDivertedFlow])
            else
              Type2Grid.Cells[2, lRow] := '';
            if (LDiversionDemandValue <> NullFloat) and (LDivertedFlow <> NullFloat) then
              FlowDiversionRelationshipLineSeries.AddXY(LDiversionDemandValue,LDivertedFlow,'',clTeeColor);

          end;
        end;
      end;
    end;
    ResetButtons;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.RePopulateType3Data;
const OPNAME = 'TDiversionFeatureValidator.RePopulateType3Data';
var
  lDivFeature    : IDiversionFeature;
  lIndex         : integer;
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lCount         : integer;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndex := 0 to lReservoirList.ReservoirCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirByIndex[lIndex];
              ReservoirCbx.Items.AddObject
                ('(' + IntToStr(lReservoir.ReservoirConfigurationData.ReservoirIdentifier) + ') ' +
                   lReservoir.ReservoirConfigurationData.ReservoirName,
                   TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier));
            end;
            if (lDivFeature.ControllingReservoir <> nil) then
            begin
              lReservoir   := lDivFeature.ControllingReservoir;
              lReservoirNr := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
              lFieldProperty := ReservoirCbx.FieldProperty;
              lKeyValues     := lDivFeature.GetKeyValues(lFieldProperty.FieldName, '');
              ReservoirCbx.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
              ReservoirCbx.SetFieldIndex(ReservoirCbx.Items.IndexOfObject(TObject(lReservoirNr)));

              LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
              lCount := lReservoir.ReservoirConfigurationData.PointsCount;
              MaximumEdit.Text := Format(lFieldProperty.FormatStringGrid {'%8.3f'}, [lReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[1]]);
              MinimumEdit.Text := Format(lFieldProperty.FormatStringGrid {'%8.3f'}, [lReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[lCount]]);
            end;
          end;
          lFieldProperty := NrOfFlowsEdit.FieldProperty;
          lKeyValues     := lDivFeature.GetKeyValues(lFieldProperty.FieldName, '');
          NrOfFlowsEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          NrOfFlowsEdit.SetFieldValue(lDivFeature.ReferenceFlowsCount);

          lFieldProperty := NrOfLevelsEdit.FieldProperty;
          lKeyValues     := lDivFeature.GetKeyValues(lFieldProperty.FieldName, '');
          NrOfLevelsEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          NrOfLevelsEdit.SetFieldValue(lDivFeature.ReservoirElevationsCount);
          ResizeType3Grids;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ResizeType3Grids;
const OPNAME = 'TDiversionFeatureValidator.ResizeType3Grids';
var
  lDivFeature : IDiversionFeature;
  nRows       : integer;
  nHeight     : integer;
  nCols       : integer;
  nWidth      : integer;
  bColSize    : Boolean;
  bRowSize    : Boolean;
  lLevel      : integer;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          bColSize := FALSE;
          bRowSize := FALSE;

          nRows := lDivFeature.ReferenceFlowsCount;
          if (nRows >= 0) then
          begin
            bRowSize := TRUE;
            if ((nRows > 0) AND (nRows <> Type3ProportionsGrid.RowCount)) then
            begin
              if (nRows <= 8) then
                nHeight := nRows * (Type3ProportionsGrid.DefaultRowHeight + 1) + 3
              else
                nHeight := 8 * (Type3ProportionsGrid.DefaultRowHeight + 1) + 3;
              Type3FlowsGrid.Height         := nHeight;
              Type3ProportionsGrid.RowCount := nRows;
              Type3FlowsGrid.RowCount       := nRows;
            end;
          end;

          nCols := lDivFeature.ReservoirElevationsCount;
          if (nCols >= 0) then
          begin
            bColSize := TRUE;
            if ((nCols > 0) AND (nCols <> Type3ProportionsGrid.ColCount)) then
            begin
              if (nCols <= 6) then
                nWidth  := nCols * (Type3ProportionsGrid.DefaultColWidth + 1) + 3
              else
                nWidth  := 6 * (Type3ProportionsGrid.DefaultColWidth + 1) + 3;
              Type3LevelsGrid.Width         := nWidth;
              Type3LevelsGrid.ColCount      := nCols;
              Type3ProportionsGrid.ColCount := nCols;
            end;

            Type3LevelsGrid.ClearFieldProperties;
            for lLevel := 1 to nCols do
              Type3LevelsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ControllingResLevels'));

            Type3ProportionsGrid.ClearFieldProperties;
            for lLevel := 1 to nCols do
              Type3ProportionsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DivertedFlow'));
          end;

          if (bRowSize AND bColSize) then
          begin
            if ((nRows > 0) AND (nCols > 0)) then
            begin
              Type3LevelsGrid.Visible      := TRUE;
              Type3FlowsGrid.Visible       := TRUE;
              Type3ProportionsGrid.Visible := TRUE;
              RePopulateType3Grids;
            end
            else
            begin
              Type3LevelsGrid.Visible      := FALSE;
              Type3FlowsGrid.Visible       := FALSE;
              Type3ProportionsGrid.Visible := FALSE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.RePopulateType3Grids;
const OPNAME = 'TDiversionFeatureValidator.RePopulateType3Grids';
var
  lDivFeature : IDiversionFeature;
  lLevel      : integer;
  lFlow       : integer;
  lValue      : double;
  lFieldProp1 : TAbstractFieldProperty;
  lFieldIndex : string;
  lKeyValues  : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          lFieldProp1 := Type3LevelsGrid.FieldProperty(0);
          if (lFieldProp1 <> nil) then
          begin
            for lLevel := 1 to Type3LevelsGrid.ColCount do
            begin
              lFieldIndex := IntToStr(lLevel);
              lKeyValues  := lDivFeature.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
              Type3LevelsGrid.HasMetaData[lLevel-1, 0] := FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
              lValue := lDivFeature.ReservoirElevationByIndex[lLevel];
              if (lValue <> NullFloat) then
                Type3LevelsGrid.Cells[lLevel-1, 0] := Format(lFieldProp1.FormatStringGrid{ '%6.2f'}, [lValue])
              else
                Type3LevelsGrid.Cells[lLevel-1, 0] := '';
            end;

            lFieldProp1 := Type3FlowsGrid.FieldProperty(0);
            for lFlow := 1 to Type3FlowsGrid.RowCount do
            begin
              lFieldIndex := IntToStr(lFlow);
              lKeyValues  := lDivFeature.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
              Type3FlowsGrid.HasMetaData[0, lFlow-1] := FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
              lValue := lDivFeature.ReferenceFlowByIndex[lFlow];
              if (lValue <> NullFloat) then
                Type3FlowsGrid.Cells[0, lFlow-1] := Format(lFieldProp1.FormatStringGrid {'%6.2f'}, [lValue])
              else
                Type3FlowsGrid.Cells[0, lFlow-1] := '';
            end;

            lFieldProp1 := Type3ProportionsGrid.FieldProperty(0);
            for lLevel := 1 to Type3LevelsGrid.ColCount do
            begin
              for lFlow := 1 to Type3FlowsGrid.RowCount do
              begin
                lFieldIndex := IntToStr(lFlow) + ',' + IntToStr(lLevel);
                lKeyValues  := lDivFeature.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
                Type3ProportionsGrid.HasMetaData[lLevel-1, lFlow-1] :=
                  FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
                lValue := lDivFeature.DivertedFlowProportion[lFlow, lLevel];
                if (lValue <> NullFloat) then
                  Type3ProportionsGrid.Cells[lLevel-1, lFlow-1] := Format(lFieldProp1.FormatStringGrid {'%6.2f'}, [lValue])
                else
                  Type3ProportionsGrid.Cells[lLevel-1, lFlow-1] := '';
              end;
            end;
          end;  
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnDiversionTypeChange(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnDiversionTypeChange';
begin
  try
    with DiversionFeatureDialog do
    begin
      Type1GroupBox.Visible := DivTypeRadioGroup.ItemIndex = 0;
      Type2GroupBox.Visible := DivTypeRadioGroup.ItemIndex in [1,3];
      Type3GroupBox.Visible := DivTypeRadioGroup.ItemIndex = 2;
      Type3Panel.Visible    := DivTypeRadioGroup.ItemIndex = 2;
    end;
    UpdateDiversionType;
    OnEditControlEnter(Sender);
    DoContextValidation(dvtDivFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnDeleteRow(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnDeleteRow';
var
  lDivFeature : IDiversionFeature;
  lIndex      : integer;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          lIndex := Type2Grid.Row;
          if (lIndex >= 0) then
          begin
            if (lDivFeature.DeleteRow(LIndex)) then
            begin
              RePopulateType1Data;
              RePopulateType2Data;
              DoContextValidation(dvtDivFeatureType2FlowRanges);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ImportRelationship;
const OPNAME = 'TDiversionFeatureValidator.ImportRelationship';
var
  lDivFeature : IDiversionFeature;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];


      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          if (cbxStations.Text <> '') then
          begin
            if (lDivFeature.ImportedType2or4RelationshipFromPreProcessor(integer(cbxStations.items.Objects[cbxStations.ItemIndex]))) then
            begin
              lDivFeature.Station := cbxStations.Text;
              RePopulateType1Data;
              RePopulateType2Data;
              DoContextValidation(dvtDivFeatureType2FlowRanges);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnInsertRow(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnInsertRow';
var
  lDivFeature : IDiversionFeature;
  lIndex      : integer;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          lIndex := Type2Grid.Row;
          if (lIndex >= 0) then
          begin
            if (lDivFeature.InsertRow(LIndex)) then
            begin
              RePopulateType1Data;
              RePopulateType2Data;
              DoContextValidation(dvtDivFeatureType2FlowRanges);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.SaveState: boolean;
const OPNAME = 'TDiversionFeatureValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.DiversionFeatureDialog : TDiversionFeatureDialog;
const OPNAME = 'TDiversionFeatureValidator.DiversionFeatureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TDiversionFeatureDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDiversionFeatureValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.StudyHasChanged: boolean;
const OPNAME = 'TDiversionFeatureValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnbtnGetStationsClick(Sender : TObject);
const OPNAME = 'TDiversionFeatureValidator.OnbtnGetStationsClick';
var
  LImportDiversionData : TImportDiversionDataDialog;
  LDivFeature    : IDiversionFeature;
  LRefData : TStringList;
  LDivData : TStringList;
  LNonDivData  : TStringList;
  LIndex : integer;
  LMesgDlgResult  : Word;
  LMessage : string;
  LNonDiv : Boolean;
  LStationID : integer;
begin
  try
    LImportDiversionData := TImportDiversionDataDialog.CreateWithoutDFM(nil,FAppModules);
    LRefData := TStringList.Create;
    LDivData := TStringList.Create;
    LNonDivData  := TStringList.Create;
    try
      LImportDiversionData.Initialise;
      LImportDiversionData.LanguageHasChanged;
      LImportDiversionData.BorderStyle     := bsDialog;
      LImportDiversionData.Position        := poScreenCenter;
      LImportDiversionData.Width           := 500;
      LImportDiversionData.Height          := 502;
      LImportDiversionData.ShowModal;
      if (LImportDiversionData.ModalResult = 1) then
      begin
        LDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
        if (LDivFeature <> nil) then
        begin
          if LImportDiversionData.GetDiversionData(LStationID,LRefData,LDivData,LNonDivData) then
          begin
            if LRefData.Count>0 then
            begin
              LMessage := FAppModules.Language.GetString('Message.NonOrDivImport');
              LMesgDlgResult  := WRMFMessageDialog(LMessage,
              mtConfirmation,mbYesNoCancel,[FAppModules.Language.GetString('VNV.Diversion'),
              FAppModules.Language.GetString('Message.NonDiv')]);
              if (LMesgDlgResult = mrCancel) then
                Exit;
              LNonDiv := (LMesgDlgResult = mrNo);

              for LIndex := 0 to LRefData.Count-1 do
              begin
                UpdateDiversionDemand(LIndex+1,LRefData[LIndex]);
                if LNonDiv then
                  UpdateActualDivertedFlow(LIndex+1,LNonDivData[LIndex])
                else
                  UpdateActualDivertedFlow(LIndex+1,LDivData[LIndex]);
              end;
              DiversionFeatureDialog.cbxStations.Text :=
              LImportDiversionData.Study + '_'+ LImportDiversionData.SubArea + '_'+
              LImportDiversionData.Scenario + '_'+LImportDiversionData.DiversionStation;
              LDivFeature.Station := DiversionFeatureDialog.cbxStations.Text;
              DiversionFeatureDialog.cbxStations.Items.AddObject(DiversionFeatureDialog.cbxStations.Text,TObject(LStationID));
              RePopulateType1Data;
              RePopulateType2Data;
              DoContextValidation(dvtDivFeatureType2FlowRanges);
            end;  
          end;  
        end;
      end;
    finally
      FreeAndNil(LImportDiversionData);
      FreeAndNil(LRefData);
      FreeAndNil(LDivData);
      FreeAndNil(LNonDivData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with DiversionFeatureDialog do
    begin
      if ((Sender = DivFeatureNameEdit) AND
          (DivFeatureNameEdit.HasValueChanged)) then
        UpdateDivFeatureName
      else
      if ((Sender = ReservoirCbx) AND
          (ReservoirCbx.HasValueChanged)) then
        UpdateControllingReservoir
      else
      if ((Sender = NrOfFlowsEdit) AND
          (NrOfFlowsEdit.HasValueChanged)) then
        UpdateNrOfFlows
      else
      if ((Sender = NrOfLevelsEdit) AND
          (NrOfLevelsEdit.HasValueChanged)) then
        UpdateNrOfLevels
      else
      if ((Sender = cbxStations) AND
          (cbxStations.HasValueChanged)) then
      ImportRelationship;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateDivFeatureName;
const OPNAME = 'TDiversionFeatureValidator.UpdateDivFeatureName';
var
  lDivFeature  : IDiversionFeature;
  lMessage     : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DivFeatureNameEdit.FieldProperty.FieldName,
            DivFeatureNameEdit.Text,lMessage)) then
        begin
          DivFeatureNameEdit.FieldValidationError := lMessage;
          lDivFeature.FeatureName := Trim(DivFeatureNameEdit.Text);
          DivFeatureNameEdit.SetFieldValue(lDivFeature.FeatureName);
          DoContextValidation(dvtDivFeatureName);
        end
        else
          DivFeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateDiversionType;
const OPNAME = 'TDiversionFeatureValidator.UpdateDiversionType';
var
  lDivFeature : IDiversionFeature;
  lOldType    : integer;
  lNewType    : integer;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        lOldType := lDivFeature.DiversionType;
        lNewType := DivTypeRadioGroup.ItemIndex + 1;
        if (lOldType <> lNewType) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              DivTypeRadioGroup.FieldProperty.FieldName,
              IntToStr(lNewType), lMessage)) then
          begin
            lDivFeature.DiversionType := lNewType;
            RePopulateDataViewer;
          end
          else
            DivTypeRadioGroup.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateControllingReservoir;
const OPNAME = 'TDiversionFeatureValidator.UpdateControllingReservoir';
var
  lDivFeature    : IDiversionFeature;
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lMessage       : string;
  lCount         : integer;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir     := nil;
        lReservoirNr   := -1;
        if (ReservoirCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(ReservoirCbx.Items.Objects[ReservoirCbx.ItemIndex]);
          lReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            ReservoirCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr),lMessage)) then
        begin
          lDivFeature.ControllingReservoir := lReservoir;
          if (lReservoirList <> nil) then
          begin
            lReservoirNr := lDivFeature.ControllingReservoir.ReservoirConfigurationData.ReservoirIdentifier;
            ReservoirCbx.SetFieldIndex(ReservoirCbx.Items.IndexOfObject(TObject(lReservoirNr)));
          end;
      
          lFieldProperty := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
          lCount := lReservoir.ReservoirConfigurationData.PointsCount;
          MaximumEdit.Text := Format(lFieldProperty.FormatStringGrid {'%8.3f'},[lReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[1]]);
          MinimumEdit.Text := Format(lFieldProperty.FormatStringGrid {'%8.3f'}, [lReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[lCount]]);
          DoContextValidation(dvtDivFeatureControllingReservoir);
        end
        else
          ReservoirCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateNrOfFlows;
const OPNAME = 'TDiversionFeatureValidator.UpdateNrOfFlows';
var
  lDivFeature : IDiversionFeature;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            NrOfFlowsEdit.FieldProperty.FieldName,
            NrOfFlowsEdit.Text,lMessage)) then
        begin
          NrOfFlowsEdit.FieldValidationError := lMessage;
          lDivFeature.ReferenceFlowsCount := StrToInt(Trim(NrOfFlowsEdit.Text));
          NrOfFlowsEdit.SetFieldValue(lDivFeature.ReferenceFlowsCount);
          ResizeType3Grids;
          RePopulateType3Grids;
          DoContextValidation(dvtDivFeatureType3Levels);
        end
        else
          NrOfFlowsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateNrOfLevels;
const OPNAME = 'TDiversionFeatureValidator.UpdateNrOfLevels';
var
  lDivFeature : IDiversionFeature;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            NrOfLevelsEdit.FieldProperty.FieldName,
            NrOfLevelsEdit.Text,lMessage)) then
        begin
          NrOfLevelsEdit.FieldValidationError := lMessage;
          lDivFeature.ReservoirElevationsCount := StrToInt(Trim(NrOfLevelsEdit.Text));
          NrOfLevelsEdit.SetFieldValue(lDivFeature.ReservoirElevationsCount);
          ResizeType3Grids;
          RePopulateType3Grids;
          DoContextValidation(dvtDivFeatureType3Levels);
        end
        else
          NrOfLevelsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TDiversionFeatureValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with DiversionFeatureDialog do
    begin
      if ((Type1Grid = ASender) AND (ACol = 1)) then
        UpdateDiversionDemand(ARow, Trim(Type1Grid.Cells[ACol, ARow]))
      else
      if ((Type1Grid = ASender) AND (ACol = 2)) then
        UpdateNetNaturalInflow(ARow, Trim(Type1Grid.Cells[ACol, ARow]))
      else
      if ((Type2Grid = ASender) AND (ACol = 1)) then
        UpdateFlowRange(ARow, Trim(Type2Grid.Cells[ACol, ARow]))
      else
      if ((Type2Grid = ASender) AND (ACol = 2)) then
        UpdateActualDivertedFlow(ARow, Trim(Type2Grid.Cells[ACol, ARow]))
      else
      if (Type3FlowsGrid = ASender) then
        UpdateReferenceFlow(ARow+1, Trim(Type3FlowsGrid.Cells[ACol, ARow]))
      else
      if (Type3LevelsGrid = ASender) then
        UpdateReservoirLevel(ACol+1, Trim(Type3LevelsGrid.Cells[ACol, ARow]))
      else
      if (Type3ProportionsGrid = ASender) then
        UpdateFlowProportion(ARow+1, ACol+1, Trim(Type3ProportionsGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateReservoirLevel (AIndex : integer;
                                                           AValue : string);
const OPNAME = 'TDiversionFeatureValidator.UpdateReservoirLevel';
var
  lDivFeature : IDiversionFeature;
  lValue      : double;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        Type3LevelsGrid.ValidationError[AIndex-1, 0, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ControllingResLevels', AValue,lMessage, AIndex)) then
        begin
          lValue := StrToFloat(AValue);
          lDivFeature.ReservoirElevationByIndex[AIndex] := lValue;
          RePopulateType3Grids;
          DoContextValidation(dvtDivFeatureType3Levels);
        end
        else
          Type3LevelsGrid.ValidationError[AIndex-1, 0, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateReferenceFlow (AIndex : integer;
                                                          AValue : string);
const OPNAME = 'TDiversionFeatureValidator.UpdateReferenceFlow';
var
  LDivFeature : IDiversionFeature;
  LValue      : double;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        Type3FlowsGrid.ValidationError[0, AIndex-1, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'FlowValue', AValue,lMessage, AIndex)) then
        begin
          lValue := StrToFloat(AValue);
          lDivFeature.ReferenceFlowByIndex[AIndex] := lValue;
          RePopulateType3Grids;
          DoContextValidation(dvtDivFeatureType3Flows);
        end
        else
          Type3FlowsGrid.ValidationError[0, AIndex-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateFlowProportion (AFlowIndex  : integer;
                                                           ALevelIndex : integer;
                                                           AValue      : string);
const OPNAME = 'TDiversionFeatureValidator.UpdateFlowProportion';
var
  lDivFeature : IDiversionFeature;
  lValue      : double;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        Type3ProportionsGrid.ValidationError[ALevelIndex-1, AFlowIndex-1, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DivertedFlow', AValue,lMessage, AFlowIndex, ALevelIndex)) then
        begin
          lValue := StrToFloat(AValue);
          lDivFeature.DivertedFlowProportion[AFlowIndex, ALevelIndex] := lValue;
          RePopulateType3Grids;
          DoContextValidation(dvtDivFeatureType3Proportions);
        end
        else
          Type3ProportionsGrid.ValidationError[ALevelIndex-1, AFlowIndex-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateDiversionDemand (AIndex : integer;
                                                            AValue : string);
const OPNAME = 'TDiversionFeatureValidator.UpdateDiversionDemand';
var
  lDivFeature : IDiversionFeature;
  lValue      : double;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        Type1Grid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DiversionDemand', AValue,lMessage, AIndex)) then
        begin
          lValue := StrToFloat(Trim(AValue));
          lDivFeature.DiversionDemandByIndex[AIndex] := lValue;
          RePopulateType1Data;
          RePopulateType2Data;
          DoContextValidation(dvtDivFeatureType1DiversionDemands);
        end
        else
          Type1Grid.ValidationError[1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateNetNaturalInflow (AIndex : integer;
                                                             AValue : string);
const OPNAME = 'TDiversionFeatureValidator.UpdateNetNaturalInflow';
var
  lDivFeature : IDiversionFeature;
  lValue      : double;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        Type1Grid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'NetNaturalInflow', AValue,lMessage, AIndex)) then
        begin
          lValue := StrToFloat(Trim(AValue));
          lDivFeature.DivertedFlowByIndex[AIndex] := lValue;
          RePopulateType1Data;
          RePopulateType2Data;
          DoContextValidation(dvtDivFeatureType1NetNaturalInflows);
        end
        else
          Type1Grid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateFlowRange (AIndex : integer;
                                                      AValue : string);
const OPNAME = 'TDiversionFeatureValidator.UpdateFlowRange';
var
  lDivFeature : IDiversionFeature;
  lValue      : double;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        Type2Grid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'FlowRange', AValue,lMessage, AIndex)) then
        begin
          lDivFeature.DiversionDemandByIndex[AIndex] := lValue;
          RePopulateType1Data;
          RePopulateType2Data;
          DoContextValidation(dvtDivFeatureType2FlowRanges);
        end
        else
          Type2Grid.ValidationError[1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.UpdateActualDivertedFlow (AIndex : integer;
                                                               AValue : string);
const OPNAME = 'TDiversionFeatureValidator.UpdateActualDivertedFlow';
var
  lDivFeature : IDiversionFeature;
  lValue      : double;
  lMessage    : string;
begin
  try
    lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if (lDivFeature <> nil) then
    begin
      with DiversionFeatureDialog do
      begin
        Type2Grid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ActualDivertedFlow', AValue,lMessage, AIndex)) then
        begin
          lDivFeature.DivertedFlowByIndex[AIndex] := lValue;
          RePopulateType1Data;
          RePopulateType2Data;
          DoContextValidation(dvtDivFeatureType2ActualDivertedFlows);
        end
        else
          Type2Grid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ProportionsGridTopLeftChanged(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.ProportionsGridTopLeftChanged';
begin
  try
    with DiversionFeatureDialog do
    begin
      Type3LevelsGrid.LeftCol := Type3ProportionsGrid.LeftCol;
      Type3FlowsGrid.TopRow  := Type3ProportionsGrid.TopRow;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.FlowsGridTopLeftChanged(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.FlowsGridTopLeftChanged';
begin
  try
    with DiversionFeatureDialog do
    begin
      Type3ProportionsGrid.TopRow := Type3FlowsGrid.TopRow;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.LevelsGridTopLeftChanged(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.LevelsGridTopLeftChanged';
begin
  try
    with DiversionFeatureDialog do
    begin
      Type3ProportionsGrid.LeftCol := Type3LevelsGrid.LeftCol;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TDiversionFeatureValidator.DoContextValidation';
var
  lFeature         : IDiversionFeature;
  lFeatureList     : IDiversionFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.DiversionFeatureList;
      lFeature     := lFeatureList.DiversionFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtDivFeature, dvtDivFeatureName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType]) then
          ValidateFeatureType(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureType1DiversionDemands]) then
          ValidateType1DiversionDemands(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureType1NetNaturalInflows]) then
          ValidateType1NetNaturalInflows(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureType2FlowRanges,
                                dvtDivFeatureType2ActualDivertedFlows]) then
          ValidateType2FlowRanges(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureType2FlowRanges,
                                dvtDivFeatureType2ActualDivertedFlows]) then
          ValidateType2ActualDivertedFlows(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureControllingReservoir]) then
          ValidateControllingReservoir(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureControllingReservoir,
                                dvtDivFeatureType3Levels]) then
          ValidateType3Levels(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureType3Flows]) then
          ValidateType3Flows(lFeature);
        if (AValidationType in [dvtDivFeature, dvtDivFeatureType,
                                dvtDivFeatureType3Proportions]) then
          ValidateType3Proportions(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TDiversionFeatureValidator.DetermineWizardStatus';
var
  lFeature         : IDiversionFeature;
  lFeatureList     : IDiversionFeatureList;
  lFieldProperty   : TAbstractFieldProperty;
  lNotZero         : Boolean;
  lIndex           : integer;
  lLevel           : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.DiversionFeatureList;
      lFeature     := lFeatureList.DiversionFeatureByID[FFeatureID];
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
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateFeatureName(AFeature: IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateFeatureName';
begin
  try
    with DiversionFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'FeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      DivFeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateFeatureType(AFeature: IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateFeatureType';
begin
  try
    with DiversionFeatureDialog do
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
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateType1DiversionDemands (AFeature : IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateType1DiversionDemands';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType = 1)) then
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage,'Type1DiversionDemands')) then
        begin
          for lCol := 1 to 12 do
            DiversionFeatureDialog.Type1Grid.ValidationError[1, lCol, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 12 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
               DiversionFeatureDialog.Type1Grid.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
               DiversionFeatureDialog.Type1Grid.ValidationError[1, lCol, gveCellContext] := '';
          end;
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateType1NetNaturalInflows (AFeature : IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateType1NetNaturalInflows';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType = 1)) then
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage,'Type1NetNaturalInflows')) then
        begin
          for lCol := 1 to 12 do
            DiversionFeatureDialog.Type1Grid.ValidationError[2, lCol, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 12 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
               DiversionFeatureDialog.Type1Grid.ValidationError[2, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
               DiversionFeatureDialog.Type1Grid.ValidationError[2, lCol, gveCellContext] := '';
          end;
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateType2FlowRanges (AFeature : IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateType2FlowRanges';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType in [2,4])) then
    begin
      with DiversionFeatureDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'Type2FlowRanges')) then
          begin
            for lCol := 1 to 12 do
              Type2Grid.ValidationError[1, lCol, gveCellContext] := '';
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            for lCol := 1 to 12 do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                Type2Grid.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
              else
                Type2Grid.ValidationError[1, lCol, gveCellContext] := '';
            end;
            FAllErrorMessages.AddStrings(lErrorMessages);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateType2ActualDivertedFlows (AFeature : IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateType2ActualDivertedFlows';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType in [2,4])) then
    begin
      with DiversionFeatureDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AFeature.Validate(FErrorMessage, 'Type2ActualDivertedFlows')) then
          begin
            for lCol := 1 to 12 do
              Type2Grid.ValidationError[2, lCol, gveCellContext] := '';
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            for lCol := 1 to 12 do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                Type2Grid.ValidationError[2, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
              else
                Type2Grid.ValidationError[2, lCol, gveCellContext] := '';
            end;
            FAllErrorMessages.AddStrings(lErrorMessages);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateControllingReservoir(AFeature: IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateControllingReservoir';
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType = 3)) then
    begin
      with DiversionFeatureDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'ControllingReservoir')) then
        begin
          ReservoirCbx.InValidationError := FALSE;
          ReservoirCbx.ValidationError := '';
          ReservoirCbx.ShowErrorState(FALSE);
        end
        else
        begin
          ReservoirCbx.InValidationError := TRUE;
          ReservoirCbx.ValidationError := FErrorMessage;
          ReservoirCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateType3Levels (AFeature : IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateType3Levels';
var
  lColumns    : TStringList;
  lErrors     : TStringList;
  lIndex      : integer;
  lColumn     : integer;
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType = 3)) then
    begin
      with DiversionFeatureDialog do
      begin
        lColumns := TStringList.Create;
        lErrors  := TStringList.Create;
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'Type3Levels')) then
        begin
          for lIndex := 0 to Type3LevelsGrid.ColCount - 1 do
            Type3LevelsGrid.ValidationError[lIndex, 0, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors, lColumns);
          for lIndex := 0 to Type3LevelsGrid.ColCount - 1 do
            Type3LevelsGrid.ValidationError[lIndex, 0, gveCellContext] := '';
          for lIndex :=   0 to lColumns.Count - 1 do
          begin
            lColumn := StrToInt(lColumns.Strings[lIndex]);
            if (lColumn >= 0) then
              Type3LevelsGrid.ValidationError[lColumn-1, 0, gveCellContext] := lErrors.Strings[lIndex]
            else
              Type3LevelsGrid.ValidationError[lColumn-1, 0, gveCellContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lColumns);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateType3Flows (AFeature : IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateType3Flows';
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType = 3)) then
    begin
      with DiversionFeatureDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'Type3Flows')) then
          Type3FlowsGrid.ValidationError[0, 0, gveColContext] := ''
        else
        begin
          Type3FlowsGrid.ValidationError[0, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.ValidateType3Proportions (AFeature : IDiversionFeature);
const OPNAME = 'TDiversionFeatureValidator.ValidateType3Proportions';
var
  lErrorCols : TStringList;
  lErrors    : TStringList;
  lLevel     : integer;
begin
  try
    if ((AFeature <> nil) AND (AFeature.DiversionType = 3)) then
    begin
      with DiversionFeatureDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrors    := TStringList.Create;
        lErrorCols.Clear;
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'Type3Proportions')) then
        begin
          for lLevel := 1 to AFeature.ReferenceFlowsCount do
            Type3ProportionsGrid.ValidationError[lLevel-1, 0, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors, lErrorCols);
          for lLevel := 1 to AFeature.ReferenceFlowsCount do
          begin
            if (lErrorCols.IndexOf(IntToStr(lLevel)) >= 0) then
              Type3ProportionsGrid.ValidationError[lLevel-1, 0, gveColContext] := lErrors.Text
            else
              Type3ProportionsGrid.ValidationError[lLevel-1, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TDiversionFeatureValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lDivFeature    : IDiversionFeature;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) then
      begin
        with DiversionFeatureDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = DivFeatureNameEdit) then
            lFieldProperty := DivFeatureNameEdit.FieldProperty
          else
          if (FActiveControl = DivTypeRadioGroup) then
            lFieldProperty := DivTypeRadioGroup.FieldProperty
          else
          if (FActiveControl = ReservoirCbx) then
            lFieldProperty := ReservoirCbx.FieldProperty
          else
          if (FActiveControl = NrOfFlowsEdit) then
            lFieldProperty := NrOfFlowsEdit.FieldProperty
          else
          if (FActiveControl = NrOfLevelsEdit) then
            lFieldProperty := NrOfLevelsEdit.FieldProperty
          else
          {if (FActiveControl = DiversionRelationshipBtn) then
            lFieldProperty := DiversionRelationshipBtn.FieldProperty
          else}
          if (FActiveControl = Type1Grid) then
          begin
            lFieldProperty := Type1Grid.FieldProperty(Type1Grid.Col);
            lFieldIndex    := IntToStr(Type1Grid.Row);
          end
          else
          if (FActiveControl = Type2Grid) then
          begin
            lFieldProperty := Type2Grid.FieldProperty(Type2Grid.Col);
            lFieldIndex    := IntToStr(Type2Grid.Row);
          end
          else
          if (FActiveControl = Type3FlowsGrid) then
          begin
            lFieldProperty := Type3FlowsGrid.FieldProperty(Type3FlowsGrid.Col);
            lFieldIndex    := IntToStr(Type3FlowsGrid.Row+1);
          end
          else
          if (FActiveControl = Type3LevelsGrid) then
          begin
            lFieldProperty := Type3LevelsGrid.FieldProperty(Type3LevelsGrid.Col);
            lFieldIndex    := IntToStr(Type3LevelsGrid.Col+1);
          end
          else
          if (FActiveControl = Type3ProportionsGrid) then
          begin
            lFieldProperty := Type3ProportionsGrid.FieldProperty(Type3ProportionsGrid.Col);
            lFieldIndex    := IntToStr(Type3ProportionsGrid.Row+1) + ',' +
                              IntToStr(Type3ProportionsGrid.Col+1);
          end;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lDivFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
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

procedure TDiversionFeatureValidator.ResetButtons;
const OPNAME = 'TDiversionFeatureValidator.ResetButtons';
var
  lDivFeature : IDiversionFeature;
  lFieldProp  : TAbstractFieldProperty;
  lCount      : integer;
begin
  try
    lFieldProp := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (FFeatureID >= 0) then
    begin
      lDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
      if (lDivFeature <> nil) AND (lDivFeature.DiversionType in [2,4]) then
      begin
        lCount := lDivFeature.Type2and4RowCount;
        DiversionFeatureDialog.BtnInsertRow.Enabled := lCount < lFieldProp.ArrayHigh;
        DiversionFeatureDialog.BtnDeleteRow.Enabled := lCount > 2;
      end;
    end
    else
    begin
      DiversionFeatureDialog.BtnInsertRow.Enabled := FALSE;
      DiversionFeatureDialog.BtnDeleteRow.Enabled := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnAfterPasteType1OrType2RowsData(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnAfterPasteType1OrType2RowsData';
var
  LDivFeature            : IDiversionFeature;
  LRow                   : integer;
  LDiversionDemandValue,
  LNetNaturalInflowValue : double;
begin
  try
    LDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if(LDivFeature <> nil) then
    begin
      if(Sender = DiversionFeatureDialog.Type1Grid) then
      begin
        for LRow := DiversionFeatureDialog.Type1Grid.FixedRows to DiversionFeatureDialog.Type1Grid.RowCount - 1 do
        begin
          LDiversionDemandValue := StrToFloat(Trim(DiversionFeatureDialog.Type1Grid.Cells[1,LRow]));
          LDivFeature.DiversionDemandByIndex[LRow] := LDiversionDemandValue;
          LNetNaturalInflowValue := StrToFloat(Trim(DiversionFeatureDialog.Type1Grid.Cells[2,LRow]));
          LDivFeature.DivertedFlowByIndex[LRow] := LNetNaturalInflowValue;
        end;
      end
      else
      if(Sender = DiversionFeatureDialog.Type2Grid) then
      begin
        for LRow := DiversionFeatureDialog.Type2Grid.FixedRows to DiversionFeatureDialog.Type2Grid.RowCount - 1 do
        begin
          LDiversionDemandValue := StrToFloat(Trim(DiversionFeatureDialog.Type2Grid.Cells[1,LRow]));
          LDivFeature.DiversionDemandByIndex[LRow] := LDiversionDemandValue;
          LNetNaturalInflowValue := StrToFloat(Trim(DiversionFeatureDialog.Type2Grid.Cells[2,LRow]));
          LDivFeature.DivertedFlowByIndex[LRow] := LNetNaturalInflowValue;
        end;
      end;
      RePopulateType1Data;
      RePopulateType2Data;
      DoContextValidation(dvtDivFeatureType1DiversionDemands);
      DoContextValidation(dvtDivFeatureType1NetNaturalInflows);
      DoContextValidation(dvtDivFeatureType2FlowRanges);
      DoContextValidation(dvtDivFeatureType2ActualDivertedFlows);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnAfterPasteType3ProportionAndLevelRowsData(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnAfterPasteType3ProportionAndLevelRowsData';
var
  LDivFeature          : IDiversionFeature;
  LRow,
  LCol                 : integer;
  LReservoirLevelValue,
  LProportionValue     : double;
begin
  try
    LDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if(LDivFeature <> nil) then
    begin
      if(Sender = DiversionFeatureDialog.Type3LevelsGrid) then
      begin
        for LCol := DiversionFeatureDialog.Type3LevelsGrid.FixedCols to DiversionFeatureDialog.Type3LevelsGrid.ColCount - 1 do
        begin
          LReservoirLevelValue := StrToFloat(Trim(DiversionFeatureDialog.Type3LevelsGrid.Cells[LCol,0]));
          LDivFeature.ReservoirElevationByIndex[LCol + 1] := LReservoirLevelValue;
        end;
      end
      else
      if(Sender = DiversionFeatureDialog.Type3ProportionsGrid) then
      begin
        for LRow := DiversionFeatureDialog.Type3ProportionsGrid.FixedRows to DiversionFeatureDialog.Type3ProportionsGrid.RowCount - 1 do
        begin
          for LCol := DiversionFeatureDialog.Type3ProportionsGrid.FixedCols to DiversionFeatureDialog.Type3ProportionsGrid.ColCount - 1 do
          begin
            LProportionValue := StrToFloat(Trim(DiversionFeatureDialog.Type3ProportionsGrid.Cells[LCol,LRow]));
            LDivFeature.DivertedFlowProportion[LRow + 1, LCol + 1] := LProportionValue;
          end;
        end;
      end;
      RePopulateType3Grids;
      DoContextValidation(dvtDivFeatureType3Levels);
      DoContextValidation(dvtDivFeatureType3Proportions);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnAfterPasteType3ColumnData(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnAfterPasteType3ColumnData';
var
  LDivFeature : IDiversionFeature;
  LRow        : integer;
  LFlowValue  : double;
begin
  try
    LDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if(LDivFeature <> nil) then
    begin
      for LRow := DiversionFeatureDialog.Type3FlowsGrid.FixedRows to DiversionFeatureDialog.Type3FlowsGrid.RowCount - 1 do
      begin
          LFlowValue := StrToFloat(Trim(DiversionFeatureDialog.Type3FlowsGrid.Cells[0,LRow]));
          LDivFeature.ReferenceFlowByIndex[LRow + 1] := LFlowValue;
      end;
      RePopulateType3Grids;
      DoContextValidation(dvtDivFeatureType3Flows);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnAfterPasteType1OrType2ColumnData(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnAfterPasteType1OrType2ColumnData';
var
  LDivFeature            : IDiversionFeature;
  LRow                   : integer;
  LValue                 : double;
begin
  try
    LDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if(LDivFeature <> nil) then
    begin
      if((Sender = DiversionFeatureDialog.Type1Grid) AND (DiversionFeatureDialog.Type1Grid.Col = 1)) then
      begin
        for LRow := DiversionFeatureDialog.Type1Grid.FixedRows to DiversionFeatureDialog.Type1Grid.RowCount - 1 do
        begin
          LValue := StrToFloat(Trim(DiversionFeatureDialog.Type1Grid.Cells[1,LRow]));
          LDivFeature.DiversionDemandByIndex[LRow] := LValue;
        end;
      end
      else
      if((Sender = DiversionFeatureDialog.Type1Grid) AND (DiversionFeatureDialog.Type1Grid.Col = 2)) then
      begin
        for LRow := DiversionFeatureDialog.Type1Grid.FixedRows to DiversionFeatureDialog.Type1Grid.RowCount - 1 do
        begin
          LValue := StrToFloat(Trim(DiversionFeatureDialog.Type1Grid.Cells[2,LRow]));
          LDivFeature.DivertedFlowByIndex[LRow] := LValue;
        end;
      end
      else
      if((Sender = DiversionFeatureDialog.Type2Grid) AND (DiversionFeatureDialog.Type2Grid.Col = 1)) then
      begin
        for LRow := DiversionFeatureDialog.Type2Grid.FixedRows to DiversionFeatureDialog.Type2Grid.RowCount - 1 do
        begin
          LValue := StrToFloat(Trim(DiversionFeatureDialog.Type2Grid.Cells[1,LRow]));
          LDivFeature.DiversionDemandByIndex[LRow] := LValue;
        end;
      end
      else
      if((Sender = DiversionFeatureDialog.Type2Grid) AND (DiversionFeatureDialog.Type2Grid.Col = 2)) then
      begin
        for LRow := DiversionFeatureDialog.Type2Grid.FixedRows to DiversionFeatureDialog.Type2Grid.RowCount - 1 do
        begin
          LValue := StrToFloat(Trim(DiversionFeatureDialog.Type2Grid.Cells[2,LRow]));
          LDivFeature.DivertedFlowByIndex[LRow] := LValue;
        end;
      end;
      RePopulateType1Data;
      RePopulateType2Data;
      DoContextValidation(dvtDivFeatureType1DiversionDemands);
      DoContextValidation(dvtDivFeatureType1NetNaturalInflows);
      DoContextValidation(dvtDivFeatureType2FlowRanges);
      DoContextValidation(dvtDivFeatureType2ActualDivertedFlows);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureValidator.OnAfterPasteType3ProportionColumnData(Sender: TObject);
const OPNAME = 'TDiversionFeatureValidator.OnAfterPasteType3ProportionColumnData';
var
  LDivFeature : IDiversionFeature;
  LRow        : integer;
  LValue      : double;
begin
  try
    LDivFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.DiversionFeatureList.DiversionFeatureByID[FFeatureID];
    if(LDivFeature <> nil) then
    begin
      if(Sender = DiversionFeatureDialog.Type3ProportionsGrid) then
      begin
        for LRow := DiversionFeatureDialog.Type3ProportionsGrid.FixedRows to DiversionFeatureDialog.Type3ProportionsGrid.RowCount - 1 do
        begin
          LValue := StrToFloat(Trim(DiversionFeatureDialog.Type3ProportionsGrid.Cells[DiversionFeatureDialog.Type3ProportionsGrid.Col,LRow]));
          LDivFeature.DivertedFlowProportion[LRow + 1, DiversionFeatureDialog.Type3ProportionsGrid.Col + 1] := LValue;
        end;
      end;
      RePopulateType3Grids;
      DoContextValidation(dvtDivFeatureType3Proportions);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

