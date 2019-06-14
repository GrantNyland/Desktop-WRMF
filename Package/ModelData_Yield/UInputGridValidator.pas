//
//
//  UNIT      : Contains the class TInputGridValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UInputGridValidator;

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
  UInputGridDialog;

type
  TInputGridValidator = class(TAbstractDataDialogValidator)
  protected
    FFormCaption,
    FElementType : string;
    procedure CreateMemberObjects; override;
    procedure RePopulateDataViewer;

    procedure GetSpecifiedDemandFeatureDataSet(AData: TAbstractModelDataset);
    procedure GetSpecifiedInflowFeatureDataSet(AData: TAbstractModelDataset);
    procedure GetDamLevelsDataSet(AData : TAbstractModelDataset);
    procedure GetSFRUnitRunoffFileDataSet(AData : TAbstractModelDataset);
    procedure GetSFRSoilMoistureFileDataSet(AData : TAbstractModelDataset);
    procedure GetIrrigationBlockDemandFileFeatureDataSet(AData: TAbstractModelDataset);

    procedure PopulateSpecifiedDemandGridData(AData: TAbstractModelDataset);
    procedure PopulateSpecifiedInflowGridData(AData: TAbstractModelDataset);
    procedure PopulateDamLevelsGridData(AData: TAbstractModelDataset);
    procedure PopulateSFRUnitRunoffGridData(AData: TAbstractModelDataset);
    procedure PopulateSFRSoilMoistureGridData(AData: TAbstractModelDataset);

    function  GetScenarioWhereClause: string;
    procedure ClearGrid;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TInputGridDialog;
    property ElementType : string read FElementType write FElementType;
    property FormCaption : string read FFormCaption;
  end;

implementation

uses
  DB,
  VCLTee.Series,
  Windows,
  SysUtils,
  UFileNames,
  UOutputData,
  UDataSetType,
  UStreamFlowReduction,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math;

{ TInputGridValidator }

procedure TInputGridValidator.CreateMemberObjects;
const OPNAME = 'TInputGridValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TInputGridDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridValidator.Initialise: boolean;
const OPNAME = 'TInputGridValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FElementType := '';
    FFormCaption := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TInputGridValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridValidator.StudyHasChanged: boolean;
const OPNAME = 'TInputGridValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridValidator.LanguageHasChanged: boolean;
const OPNAME = 'TInputGridValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.InputGrid');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridValidator.SaveState: boolean;
const OPNAME = 'TInputGridValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridValidator.ViewDialog : TInputGridDialog;
const OPNAME = 'TInputGridValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TInputGridDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.ClearDataViewer;
const OPNAME = 'TInputGridValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.PopulateDataViewer;
const OPNAME = 'TInputGridValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.RePopulateDataViewer;
const OPNAME = 'TInputGridValidator.RePopulateDataViewer';
var
  LErrors: string;
  LDataSet : TAbstractModelDataset;
begin
  try
    ClearGrid;
    if(FIdentifier >= 0) and (FElementType <> '') then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LErrors := '';
        ViewDialog.ShowError(LErrors);
        if(FElementType = 'SPECIFIEDDEMANDFEATURE') then
        begin
          GetSpecifiedDemandFeatureDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedDemandGridData(LDataSet);
        end;
        if(FElementType = 'IRRIGATIONBLOCKDEMANDFEATURE') then
        begin
          GetIrrigationBlockDemandFileFeatureDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedDemandGridData(LDataSet);
        end;
        if (FElementType = 'DAMLEVELS') then
        begin
          GetDamLevelsDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateDamLevelsGridData(LDataSet);
        end;
        if (FElementType = 'SFRUNITRUNOFFFILE') then
        begin
          GetSFRUnitRunoffFileDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSFRUnitRunoffGridData(LDataSet);
        end;
        if (FElementType = 'SFRSOILMOISTURE') then
        begin
          GetSFRSoilMoistureFileDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSFRSoilMoistureGridData(LDataSet);
        end;
        if (FElementType = 'SPECIFIEDINFLOWFEATURE') then
        begin
          GetSpecifiedInflowFeatureDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedInflowGridData(LDataSet);
        end;
        LDataSet.DataSet.Close;
      finally
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.PopulateSpecifiedDemandGridData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.PopulateSpecifiedDemandGridData';
var
  LIndex: integer;
begin
  try
    ClearGrid;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      AData.DataSet.First;
      ViewDialog.grdDataGrid.ColCount := AData.DataSet.FieldCount;
      for LIndex := 0 to AData.DataSet.FieldCount-1 do
         ViewDialog.grdDataGrid.Cells[LIndex,0] := AData.DataSet.Fields.Fields[LIndex].DisplayName;
      while not AData.DataSet.Eof do
      begin
        ViewDialog.grdDataGrid.RowCount := ViewDialog.grdDataGrid.RowCount + 1;
        for LIndex := 0 to AData.DataSet.FieldCount-1 do
        begin
          if not AData.DataSet.Fields.Fields[LIndex].IsNull then
           ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] := AData.DataSet.Fields.Fields[LIndex].DisplayText;
        end;
        AData.DataSet.Next;
      end;
      if(ViewDialog.grdDataGrid.RowCount > 1) then
        ViewDialog.grdDataGrid.FixedRows := 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.PopulateSpecifiedInflowGridData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.PopulateSpecifiedInflowGridData';
var
  LIndex: integer;
begin
  try
    ClearGrid;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      AData.DataSet.First;
      ViewDialog.grdDataGrid.ColCount := AData.DataSet.FieldCount;
      for LIndex := 0 to AData.DataSet.FieldCount - 1 do
         ViewDialog.grdDataGrid.Cells[LIndex,0] := AData.DataSet.Fields.Fields[LIndex].DisplayName;
      while not AData.DataSet.Eof do
      begin
        ViewDialog.grdDataGrid.RowCount := ViewDialog.grdDataGrid.RowCount + 1;
        for LIndex := 0 to AData.DataSet.FieldCount - 1 do
        begin
          if not AData.DataSet.Fields.Fields[LIndex].IsNull then
           ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] := AData.DataSet.Fields.Fields[LIndex].DisplayText;
        end;
        AData.DataSet.Next;
      end;
      if(ViewDialog.grdDataGrid.RowCount > 1) then
        ViewDialog.grdDataGrid.FixedRows := 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.PopulateDamLevelsGridData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.PopulateDamLevelsGridData';
var
  LIndex: integer;
  LValue: string;
begin
  try
    ClearGrid;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      AData.DataSet.First;
      ViewDialog.grdDataGrid.ColCount := AData.DataSet.FieldCount;
      for LIndex := 0 to AData.DataSet.FieldCount-1 do
         ViewDialog.grdDataGrid.Cells[LIndex,0] := AData.DataSet.Fields.Fields[LIndex].DisplayName;
      while not AData.DataSet.Eof do
      begin
        ViewDialog.grdDataGrid.RowCount := ViewDialog.grdDataGrid.RowCount + 1;
        for LIndex := 0 to AData.DataSet.FieldCount-1 do
        begin
          if not AData.DataSet.Fields.Fields[LIndex].IsNull then
          begin
            LValue := AData.DataSet.Fields.Fields[LIndex].DisplayText;
            ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] := LValue;
          end;
          // ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] := AData.DataSet.Fields.Fields[LIndex].DisplayText;
        end;
        AData.DataSet.Next;
      end;
      if(ViewDialog.grdDataGrid.RowCount > 1) then
        ViewDialog.grdDataGrid.FixedRows := 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.ClearGrid;
const OPNAME = 'TInputGridValidator.ClearGrid';
var
  LIndex: integer;
begin
  try
    ViewDialog.grdDataGrid.FixedRows := 0;
    ViewDialog.grdDataGrid.ColCount := 1;
    ViewDialog.grdDataGrid.RowCount := 1;
    for LIndex := 0 to ViewDialog.grdDataGrid.ColCount -1 do
      ViewDialog.grdDataGrid.Cols[LIndex].Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridValidator.GetScenarioWhereClause: string;
const OPNAME = 'TInputGridValidator.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.GetSpecifiedDemandFeatureDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.GetSpecifiedDemandFeatureDataSet';
var
  LIndex: integer;
  LSQL: string;
  lFeature       : ISpecifiedDemandFeature;
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FIdentifier];
      if (lFeature <> nil) then
      begin
        LFileNameObject := nil;
        LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
        for LIndex:= 0 to LFileNamesList.FilesCount-1 do
        begin
          if(UpperCase(lFeature.SpecifiedDemandFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].FileName)) then
          begin
            LFileNameObject := LFileNamesList.FileNameObject[LIndex];
            Break;
          end;
        end;
        if(LFileNameObject <> nil) then
        begin
          FFormCaption := LFileNameObject.ShortName;
          LSQL := 'SELECT DemandYearValue as [Year],'+
                  'DemandMonthValue01 as Value01,'+
                  'DemandMonthValue02 as Value02,'+
                  'DemandMonthValue03 as Value03,'+
                  'DemandMonthValue04 as Value04,'+
                  'DemandMonthValue05 as Value05,'+
                  'DemandMonthValue06 as Value06,'+
                  'DemandMonthValue07 as Value07,'+
                  'DemandMonthValue08 as Value08,'+
                  'DemandMonthValue09 as Value09,'+
                  'DemandMonthValue10 as Value10,'+
                  'DemandMonthValue11 as Value11,'+
                  'DemandMonthValue12 as Value12'+
                  ' FROM DemandFileData A WHERE ' +
                   GetScenarioWhereClause +
                  //' AND FileType = '+IntToStr(LFileNameObject.FileGroup)+
                  ' AND FileNumber = '+ IntToStr(LFileNameObject.FileNumber) +
                  ' ORDER BY DemandYearValue';
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
          if not AData.DataSet.Eof then
          begin
            for LIndex:= 1 to 12 do
            begin
              TFloatField(AData.DataSet.Fields.Fields[LIndex]).DisplayFormat  := '##0.000';
                AData.DataSet.Fields.Fields[LIndex].DisplayLabel :=
                TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex];
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.GetSpecifiedInflowFeatureDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.GetSpecifiedInflowFeatureDataSet';
var
  LIndex          : integer;
  LSQL: string;
  LFeature        : ISpecifiedInflowFeature;
  LFileNamesList  : TFileNamesList;
  LFileNameObject : TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FIdentifier];
      if (lFeature <> nil) then
      begin
        LFileNameObject := nil;
        LFileNamesList  := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames;
        for LIndex:= 0 to LFileNamesList.FilesCount - 1 do
        begin
          if(UpperCase(LFeature.InflowFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].FileName)) then
          begin
            LFileNameObject := LFileNamesList.FileNameObject[LIndex];
            Break;
          end;
        end;
        if(LFileNameObject <> nil) then
        begin
          FFormCaption   := LFileNameObject.ShortName;
          LSQL := ' SELECT HydroYearValue as [Year],'+
                  ' HydroMonthValue01 as Value01, HydroMonthValue02 as Value02, HydroMonthValue03 as Value03, '+
                  ' HydroMonthValue04 as Value04, HydroMonthValue05 as Value05, HydroMonthValue06 as Value06, '+
                  ' HydroMonthValue07 as Value07, HydroMonthValue08 as Value08, HydroMonthValue09 as Value09, '+
                  ' HydroMonthValue10 as Value10, HydroMonthValue11 as Value11, HydroMonthValue12 as Value12  '+
                  ' FROM HydrologyFileData A WHERE A.StudyAreaName = ' +  QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                  ' AND FileName = ' + QuotedStr(LFileNameObject.ShortName) +
                  ' ORDER BY HydroYearValue';
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
          if not AData.DataSet.Eof then
          begin
            for LIndex:= 1 to 12 do
            begin
              TFloatField(AData.DataSet.Fields.Fields[LIndex]).DisplayFormat  := '##0.000';
                AData.DataSet.Fields.Fields[LIndex].DisplayLabel :=
                TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex];
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.GetDamLevelsDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.GetDamLevelsDataSet';
var
  LIndex: integer;
  LSQL: string;
  lReservoir     : IReservoirData;
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
      begin
        LFileNameObject := nil;
        LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
        for LIndex:= 0 to LFileNamesList.FilesCount-1 do
        begin
          if(UpperCase(lReservoir.ReservoirConfigurationData.DamLevelsFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].ShortName)) then
          begin
            LFileNameObject := LFileNamesList.FileNameObject[LIndex];
            Break;
          end;
        end;
        if(LFileNameObject <> nil) then
        begin
          FFormCaption := LFileNameObject.ShortName;
          LSQL := 'SELECT YearValue as [Year],'+
                  'MonthValue01 as Value01,'+
                  'MonthValue02 as Value02,'+
                  'MonthValue03 as Value03,'+
                  'MonthValue04 as Value04,'+
                  'MonthValue05 as Value05,'+
                  'MonthValue06 as Value06,'+
                  'MonthValue07 as Value07,'+
                  'MonthValue08 as Value08,'+
                  'MonthValue09 as Value09,'+
                  'MonthValue10 as Value10,'+
                  'MonthValue11 as Value11,'+
                  'MonthValue12 as Value12 '+
                  ' FROM HistoricDamLevels A WHERE ' +
                   GetScenarioWhereClause +
                  ' AND FileName = '+QuotedStr(LFileNameObject.ShortName)+
                  ' ORDER BY YearValue';
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
          if not AData.DataSet.Eof then
          begin
            for LIndex:= 1 to 12 do
            begin
              TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar.ModelMonthNameByIndex[LIndex];
              TFloatField(AData.DataSet.Fields.Fields[LIndex]).DisplayFormat  := '##0.000';
              AData.DataSet.Fields.Fields[LIndex].DisplayLabel :=
                TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByMonthNumber(LIndex);
            end;
          end;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.GetSFRUnitRunoffFileDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.GetSFRUnitRunoffFileDataSet';
var
  LIndex: integer;
  LSQL: string;
  LStreamFlowReduction : TStreamFlowReduction;
  LFileName     : string;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
      if (LStreamFlowReduction <> nil) then
      begin
        LFileName       := LStreamFlowReduction.UnitRunoffFileName;
        LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames.FindFile(LFileName);
        if(LFileNameObject <> nil) then
        begin
          FFormCaption := LFileNameObject.ShortName;
          LSQL := 'SELECT StudyAreaName,FileName,Identifier,   '+
                  ' HydroYearValue as [Year],      '+
                  ' HydroMonthValue01 as Value01,'+
                  ' HydroMonthValue02 as Value02,'+
                  ' HydroMonthValue03 as Value03,'+
                  ' HydroMonthValue04 as Value04,'+
                  ' HydroMonthValue05 as Value05,'+
                  ' HydroMonthValue06 as Value06,'+
                  ' HydroMonthValue07 as Value07,'+
                  ' HydroMonthValue08 as Value08,'+
                  ' HydroMonthValue09 as Value09,'+
                  ' HydroMonthValue10 as Value10,'+
                  ' HydroMonthValue11 as Value11,'+
                  ' HydroMonthValue12 as Value12 '+
                  ' FROM HydrologyFileData       '+
                  ' WHERE FileName = '+QuotedStr(ExtractFileName(LFileName))+
                  ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                  ' AND Identifier  IS NOT NULL'+
                  ' ORDER BY FileName,Identifier';
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
          if not AData.DataSet.Eof then
          begin
            for LIndex:= 4 to 15 do
            begin
              TFloatField(AData.DataSet.Fields.Fields[LIndex]).DisplayFormat  := '##0.000';
                AData.DataSet.Fields.Fields[LIndex].DisplayLabel :=
                TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex-3];
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.GetSFRSoilMoistureFileDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.GetSFRSoilMoistureFileDataSet';
var
  LIndex: integer;
  LSQL: string;
  LStreamFlowReduction : TStreamFlowReduction;
  LFileName     : string;
  LFileNameObject: TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByID(FIdentifier);
      if (LStreamFlowReduction <> nil) then
      begin
        LFileName       := LStreamFlowReduction.SoilMoistureFileName;
        LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames.FindFile(LFileName);
        if(LFileNameObject <> nil) then
        begin
          FFormCaption := LFileNameObject.ShortName;
          LSQL := 'SELECT StudyAreaName,FileName,Identifier,   '+
                  ' HydroYearValue as [Year],      '+
                  ' HydroMonthValue01 as Value01,'+
                  ' HydroMonthValue02 as Value02,'+
                  ' HydroMonthValue03 as Value03,'+
                  ' HydroMonthValue04 as Value04,'+
                  ' HydroMonthValue05 as Value05,'+
                  ' HydroMonthValue06 as Value06,'+
                  ' HydroMonthValue07 as Value07,'+
                  ' HydroMonthValue08 as Value08,'+
                  ' HydroMonthValue09 as Value09,'+
                  ' HydroMonthValue10 as Value10,'+
                  ' HydroMonthValue11 as Value11,'+
                  ' HydroMonthValue12 as Value12 '+
                  ' FROM HydrologyFileData       '+
                  ' WHERE FileName = '+QuotedStr(ExtractFileName(LFileName))+
                  ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                  ' AND Identifier  IS NOT NULL'+
                  ' ORDER BY FileName,Identifier';
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
          if not AData.DataSet.Eof then
          begin
            for LIndex:= 4 to 15 do
            begin
              TFloatField(AData.DataSet.Fields.Fields[LIndex]).DisplayFormat  := '##0.000';
                AData.DataSet.Fields.Fields[LIndex].DisplayLabel :=
                TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex-3];
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.GetIrrigationBlockDemandFileFeatureDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.GetIrrigationBlockDemandFileFeatureDataSet';
var
  LIndex          : integer;
  LSQL            : string;
  LFeature        : IIrrigationBlock;
  LFileNamesList  : TFileNamesList;
  LFileNameObject : TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      if (lFeature <> nil) then
      begin
        LFileNameObject := nil;
        LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
        for LIndex:= 0 to LFileNamesList.FilesCount-1 do
        begin
          if(UpperCase(lFeature.FileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].FileName)) then
          begin
            LFileNameObject := LFileNamesList.FileNameObject[LIndex];
            Break;
          end;
        end;
        if(LFileNameObject <> nil) then
        begin
          FFormCaption := LFileNameObject.ShortName;
          LSQL := 'SELECT DemandYearValue as [Year],'+
                  'DemandMonthValue01 as Value01,'+
                  'DemandMonthValue02 as Value02,'+
                  'DemandMonthValue03 as Value03,'+
                  'DemandMonthValue04 as Value04,'+
                  'DemandMonthValue05 as Value05,'+
                  'DemandMonthValue06 as Value06,'+
                  'DemandMonthValue07 as Value07,'+
                  'DemandMonthValue08 as Value08,'+
                  'DemandMonthValue09 as Value09,'+
                  'DemandMonthValue10 as Value10,'+
                  'DemandMonthValue11 as Value11,'+
                  'DemandMonthValue12 as Value12'+
                  ' FROM DemandFileData A WHERE ' +
                   GetScenarioWhereClause +
                  //' AND FileType = '+IntToStr(LFileNameObject.FileGroup)+
                  ' AND FileNumber = '+IntToStr(LFileNameObject.FileNumber)+
                  ' ORDER BY DemandYearValue';
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
          if not AData.DataSet.Eof then
          begin
            for LIndex:= 1 to 12 do
            begin
              TFloatField(AData.DataSet.Fields.Fields[LIndex]).DisplayFormat  := '##0.000';
                AData.DataSet.Fields.Fields[LIndex].DisplayLabel :=
                TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex];
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.PopulateSFRSoilMoistureGridData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.PopulateSFRSoilMoistureGridData';
var
  LIndex: integer;
begin
  try
    ClearGrid;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      AData.DataSet.First;
      ViewDialog.grdDataGrid.ColCount := AData.DataSet.FieldCount;
      for LIndex := 0 to AData.DataSet.FieldCount-1 do
         ViewDialog.grdDataGrid.Cells[LIndex,0] := AData.DataSet.Fields.Fields[LIndex].DisplayName;
      while not AData.DataSet.Eof do
      begin
        ViewDialog.grdDataGrid.RowCount := ViewDialog.grdDataGrid.RowCount + 1;
        for LIndex := 0 to AData.DataSet.FieldCount-1 do
        begin
          if not AData.DataSet.Fields.Fields[LIndex].IsNull then
           ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] := AData.DataSet.Fields.Fields[LIndex].DisplayText;
        end;
        AData.DataSet.Next;
      end;
      if(ViewDialog.grdDataGrid.RowCount > 1) then
        ViewDialog.grdDataGrid.FixedRows := 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridValidator.PopulateSFRUnitRunoffGridData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGridValidator.PopulateSFRUnitRunoffGridData';
var
  LIndex: integer;
begin
  try
    ClearGrid;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      AData.DataSet.First;
      ViewDialog.grdDataGrid.ColCount := AData.DataSet.FieldCount;
      for LIndex := 0 to AData.DataSet.FieldCount-1 do
         ViewDialog.grdDataGrid.Cells[LIndex,0] := AData.DataSet.Fields.Fields[LIndex].DisplayName;
      while not AData.DataSet.Eof do
      begin
        ViewDialog.grdDataGrid.RowCount := ViewDialog.grdDataGrid.RowCount + 1;
        for LIndex := 0 to AData.DataSet.FieldCount-1 do
        begin
          if not AData.DataSet.Fields.Fields[LIndex].IsNull then
           ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] := AData.DataSet.Fields.Fields[LIndex].DisplayText;
        end;
        AData.DataSet.Next;
      end;
      if(ViewDialog.grdDataGrid.RowCount > 1) then
        ViewDialog.grdDataGrid.FixedRows := 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

