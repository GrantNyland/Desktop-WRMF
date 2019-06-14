//
//
//  UNIT      : Contains the class TInputGraphValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UInputGraphValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,    VCL.dialogs,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UInputGraphDialog,
  VCLTee.Chart,
  VCLTee.TeeProcs,
  VCLTee.Series;

type
  TValueType  = (vtAnnual, vtMonthly);
  TInputGraphValidator = class(TAbstractDataDialogValidator)
  protected
    FFormCaption,
    FElementType           : string;
    
    FSpecifiedMean,
    FSpecifiedStdDevMAbove,
    FSpecifiedStdDevMBelow,

    FSpecifiedMeanA,
    FSpecifiedStdDevAAbove,
    FSpecifiedStdDevABelow : Double;
    
    procedure OnMeanChkBoxClick(Sender: TObject) ;
    procedure OnStdDevChkBoxClick(Sender: TObject);
    procedure SetGraphLabel(AValueType: TValueType);
    procedure SetGraphsStatus(AValueType: TValueType);
    procedure CreateMemberObjects; override;
    procedure ClearChart;
    procedure RePopulateDataViewer;
    procedure OnMonthlyAnnualClick(Sender: TObject);

    procedure GetSpecifiedDemandFeatureDataSet(AData: TAbstractModelDataset);
    procedure GetSpecifiedDemandFeatureYearlyDataSet(AData: TAbstractModelDataset);
    procedure GetSpecifiedInflowFeatureDataSet(AData: TAbstractModelDataset);
    procedure GetSpecifiedInflowFeatureYearlyDataSet(AData: TAbstractModelDataset);

    procedure GetDamLevelsDataSet(AData: TAbstractModelDataset);
    procedure GetSFRUnitRunoffFileDataSet(AData : TAbstractModelDataset);
    procedure GetSFRSoilMoistureFileDataSet(AData : TAbstractModelDataset);
    procedure GetIrrigationBlockDemandFileFeatureDataSet(AData: TAbstractModelDataset);
    procedure GetIrrigationBlockDemandFeatureYearlyDataSet(AData: TAbstractModelDataset);

    procedure PopulateSpecifiedDemandChartData(AData: TAbstractModelDataset);
    procedure PopulateSpecifiedDemandChartYearlyData(AData: TAbstractModelDataset);
    procedure PopulateSpecifiedInflowChartMonthlyData(AData: TAbstractModelDataset);
    procedure PopulateSpecifiedInflowChartYearlyData(AData: TAbstractModelDataset);
    procedure PopulateDamLevelsChartData(AData: TAbstractModelDataset);
    procedure PopulateSFRUnitRunoffChartData(AData: TAbstractModelDataset);
    procedure PopulateSFRSoilMoistureChartData(AData: TAbstractModelDataset);

    procedure ConfigureLineSeries(ALineSeries : TLineSeries; AValueType : TValueType);
    procedure ConfigureChartBottomAxis(ALineSeries : TLineSeries; AValueType : TValueType);

    function GetScenarioWhereClause: string;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TInputGraphDialog;
    property ElementType : string read FElementType write FElementType;
  end;

implementation

uses
  DB,
  Windows,
  SysUtils,
  UFileNames,
  UOutputData,
  UDataSetType,
  UStreamFlowReduction,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, VCLTee.TeEngine;

{ TInputGraphValidator }

procedure TInputGraphValidator.CreateMemberObjects;
const OPNAME = 'TInputGraphValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TInputGraphDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.MeanCheckBox.OnClick   := OnMeanChkBoxClick;
    ViewDialog.StdDevCheckBox.OnClick := OnStdDevChkBoxClick;

    ViewDialog.MonthlyAnnual.FieldProperty  := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ViewDialog.MonthlyAnnual.OnClick        := OnMonthlyAnnualClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphValidator.Initialise: boolean;
const OPNAME = 'TInputGraphValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FFormCaption := '';
    FElementType := '';
    ViewDialog.MeanCheckBox.Checked := FAppModules.ViewIni.ReadInteger(Self.ClassName, 'MeanVisible', 0) = 1;
    ViewDialog.StdDevCheckBox.Checked := FAppModules.ViewIni.ReadInteger(Self.ClassName, 'StdDevVisible', 0) = 1;
    FSpecifiedMean          := 0.0;
    FSpecifiedMeanA         := 0.0;

    FSpecifiedStdDevMAbove  := 0.0;
    FSpecifiedStdDevMBelow  := 0.0;
    FSpecifiedStdDevAAbove  := 0.0;
    FSpecifiedStdDevABelow  := 0.0;

    ViewDialog.MonthlyAnnual.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TInputGraphValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphValidator.StudyHasChanged: boolean;
const OPNAME = 'TInputGraphValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphValidator.LanguageHasChanged: boolean;
const OPNAME = 'TInputGraphValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.InputGraph');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphValidator.ViewDialog : TInputGraphDialog;
const OPNAME = 'TInputGraphValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TInputGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.ClearDataViewer;
const OPNAME = 'TInputGraphValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.PopulateDataViewer;
const OPNAME = 'TInputGraphValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    ViewDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.RePopulateDataViewer;
const OPNAME = 'TInputGraphValidator.RePopulateDataViewer';
var
  LErrors: string;
  LDataSet : TAbstractModelDataset;
begin
  try
    ClearChart;
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
            PopulateSpecifiedDemandChartData(LDataSet);

          GetSpecifiedDemandFeatureYearlyDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedDemandChartYearlyData(LDataSet);
        end;
        if(FElementType = 'IRRIGATIONBLOCKDEMANDFEATURE') then
        begin
          GetIrrigationBlockDemandFileFeatureDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedDemandChartData(LDataSet);

          GetIrrigationBlockDemandFeatureYearlyDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedDemandChartYearlyData(LDataSet);
        end;
        if (FElementType = 'DAMLEVELS') then
        begin
          GetDamLevelsDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateDamLevelsChartData(LDataSet);
        end;
        if (FElementType = 'SFRUNITRUNOFFFILE') then
        begin
          GetSFRUnitRunoffFileDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSFRUnitRunoffChartData(LDataSet);
        end;
        if (FElementType = 'SFRSOILMOISTURE') then
        begin
          GetSFRSoilMoistureFileDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSFRSoilMoistureChartData(LDataSet);
        end;
        if(FElementType = 'SPECIFIEDINFLOWFEATURE') then
        begin
          GetSpecifiedInflowFeatureDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedInflowChartMonthlyData(LDataSet);

          GetSpecifiedInflowFeatureYearlyDataSet(LDataSet);
          if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            PopulateSpecifiedInflowChartYearlyData(LDataSet);
        end;
        LDataSet.DataSet.Close;
      finally
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.PopulateSpecifiedDemandChartData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.PopulateSpecifiedDemandChartData';
var
  LMonthNumbers  : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex: integer;
  LMonthlyValue : double;
  LDate: TDateTime;
  LFieldName: string;
  LYValues : array of double;
  LMean,
  LStdDev: double;
begin
  try
    //ClearChart;
    FSpecifiedMean    := 0.0;
    FSpecifiedMeanA   := 0.0;

    FSpecifiedStdDevMAbove  := 0.0;
    FSpecifiedStdDevMBelow  := 0.0;

    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.LineSeries.Active            := True;
      ViewDialog.LineSeries.XValues.DateTime  := True;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth-1;
      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;


      AData.DataSet.First;
      While not AData.DataSet.Eof do
      begin
        LYear := AData.DataSet.FieldByName('Year').AsInteger;
        for LIndex := 1 to 12 do
        begin
          LDate := EncodeDate(LYear,LMonthNumbers[LIndex-1],1);
          LFieldName    := Format('%s%2.2d',['Value',LIndex]);
          LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
          ViewDialog.LineSeries.AddXY(LDate,LMonthlyValue);
          if(LMonthNumbers[LIndex-1] = 12) then
            LYear := LYear + 1;
        end;
        AData.DataSet.Next;
      end;

      SetLength(LYValues,ViewDialog.LineSeries.YValues.Count);
      for LIndex := Low(LYValues) to High(LYValues) do
        LYValues[LIndex] := ViewDialog.LineSeries.YValues[LIndex];

      LMean   := Mean(LYValues);
      try
        LStdDev := StdDev(LYValues);
      except
        LStdDev := 0.0;
      end;

      for LIndex := Low(LYValues) to High(LYValues) do
      begin
        ViewDialog.MeanLineSeries.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean);
        ViewDialog.StdDevLineSeriesMAbove.AddXY(ViewDialog.LineSeries.XValues[LIndex], LMean + LStdDev);
        ViewDialog.StdDevLineSeriesMBelow.AddXY(ViewDialog.LineSeries.XValues[LIndex], LMean - LStdDev);
      end;
      //ViewDialog.MeanLabel.Caption := Format(FAppModules.Language.GetString('LabelMean.Caption'),
        //                              [FormatFloat('0.00',LMean),FormatFloat('0.00',LStdDev)]);

      FSpecifiedMean          := LMean;
      FSpecifiedStdDevMAbove  := LMean + LStdDev;
      FSpecifiedStdDevMBelow  := LMean - LStdDev;

      OnMeanChkBoxClick(nil);
      OnStdDevChkBoxClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.ClearChart;
const OPNAME = 'TInputGraphValidator.ClearChart';
begin
  try
    ViewDialog.LineSeries.Clear;
    ViewDialog.Chart.Title.Caption := '';
    ViewDialog.LineSeries.Active := False;
    ViewDialog.Chart.Title.Text.Clear;
    ViewDialog.Chart.LeftAxis.Title.Caption := '';
    ViewDialog.Chart.BottomAxis.Title.Caption := '';
    ViewDialog.Chart.Title.Text.Clear;
    ViewDialog.Chart.UndoZoom;
    ViewDialog.Chart.LeftAxis.AxisValuesFormat := '###0.0##';

    FSpecifiedMean         := 0.0;
    FSpecifiedMeanA        := 0.0;

    FSpecifiedStdDevMAbove := 0.0;
    FSpecifiedStdDevMBelow := 0.0;
    FSpecifiedStdDevAAbove := 0.0;
    FSpecifiedStdDevABelow := 0.0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.GetSpecifiedDemandFeatureDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetSpecifiedDemandFeatureDataSet';
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
          ViewDialog.Chart.Title.Text.Clear;
          ViewDialog.Chart.Title.Text.Add(LFileNameObject.ShortName);
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.DemandFileMonthly');
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
                  ' AND FileNumber = '+IntToStr(LFileNameObject.FileNumber);
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

function TInputGraphValidator.GetScenarioWhereClause: string;
const OPNAME = 'TInputGraphValidator.GetScenarioWhereClause';
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

procedure TInputGraphValidator.GetDamLevelsDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetDamLevelsDataSet';
var
  LIndex         : integer;
  LSQL           : string;
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
          ViewDialog.Chart.Title.Text.Add(LFileNameObject.ShortName);
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.DamLevel');
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
                  ' AND FileName = '+QuotedStr(LFileNameObject.ShortName);
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TInputGraphValidator.PopulateDamLevelsChartData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.PopulateDamLevelsChartData';
var
  LYear,
  LIndex: integer;
  LMonthlyValue : double;
  LDate: TDateTime;
  LFieldName: string;
begin
  try
    //ClearChart;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.LineSeries.Active := True;
      ViewDialog.LineSeries.XValues.DateTime := True;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;

      AData.DataSet.First;
      While not AData.DataSet.Eof do
      begin
        LYear := AData.DataSet.FieldByName('Year').AsInteger;
        for LIndex := 1 to 12 do
        begin
          LDate := EncodeDate(LYear,LIndex,1);
          LFieldName    := Format('%s%2.2d',['Value',LIndex]);
          if not AData.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
            ViewDialog.LineSeries.AddXY(LDate,LMonthlyValue);
          end;
        end;
        AData.DataSet.Next;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.OnMeanChkBoxClick(Sender: TObject);
const OPNAME = 'TInputGraphValidator.OnMeanChkBoxClick';
begin
  FAppModules.ViewIni.WriteInteger(Self.ClassName, 'MeanVisible', Ord(ViewDialog.MeanCheckBox.Checked));
  if ViewDialog.MonthlyAnnual.ItemIndex = 0 then
  begin
    SetGraphLabel(vtMonthly);
    ViewDialog.MeanLineSeries.Active  := ViewDialog.MeanCheckBox.Checked;
    ViewDialog.MeanLineSeriesA.Active := False;
  end else
  if ViewDialog.MonthlyAnnual.ItemIndex = 1 then
  begin
    SetGraphLabel(vtAnnual);
    ViewDialog.MeanLineSeriesA.Active := ViewDialog.MeanCheckBox.Checked;
    ViewDialog.MeanLineSeries.Active  := False;
  end;
end;

procedure TInputGraphValidator.OnStdDevChkBoxClick(Sender: TObject);
const OPNAME = 'TInputGraphValidator.OnStdDevChkBoxClick';
begin

  FAppModules.ViewIni.WriteInteger(Self.ClassName, 'StdDevVisible', Ord(ViewDialog.StdDevCheckBox.Checked));
  if ViewDialog.MonthlyAnnual.ItemIndex = 0 then
  begin
    SetGraphLabel(vtMonthly);
    ViewDialog.StdDevLineSeriesMAbove.Active := ViewDialog.StdDevCheckBox.Checked;
    ViewDialog.StdDevLineSeriesMBelow.Active := ViewDialog.StdDevCheckBox.Checked;
    ViewDialog.StdDevLineSeriesAAbove.Active := False;
    ViewDialog.StdDevLineSeriesABelow.Active := False;
  end else
  if ViewDialog.MonthlyAnnual.ItemIndex = 1 then
  begin
    SetGraphLabel(vtAnnual);
    ViewDialog.StdDevLineSeriesMAbove.Active := False;
    ViewDialog.StdDevLineSeriesMBelow.Active := False;
    ViewDialog.StdDevLineSeriesAAbove.Active := ViewDialog.StdDevCheckBox.Checked;
    ViewDialog.StdDevLineSeriesABelow.Active := ViewDialog.StdDevCheckBox.Checked;
  end;  
end;

procedure TInputGraphValidator.GetSFRUnitRunoffFileDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetSFRUnitRunoffFileDataSet';
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
          FFormCaption :=  ExtractFileName(LFileName);
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

procedure TInputGraphValidator.GetSFRSoilMoistureFileDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetSFRSoilMoistureFileDataSet';
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
          FFormCaption :=  ExtractFileName(LFileName);
          LSQL := 'SELECT StudyAreaName, FileName,Identifier,   '+
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

procedure TInputGraphValidator.GetIrrigationBlockDemandFileFeatureDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetIrrigationBlockDemandFileFeatureDataSet';
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

procedure TInputGraphValidator.PopulateSFRSoilMoistureChartData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.PopulateSFRSoilMoistureChartData';
var
  LMonthNumbers  : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex: integer;
  LMonthlyValue : double;
  LDate: TDateTime;
  LFieldName: string;
  LYValues : array of double;
  LMean,
  LStdDev: double;
begin
  try
    //ClearChart;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.LineSeries.Active := True;
      ViewDialog.LineSeries.XValues.DateTime := True;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;
      ViewDialog.Chart.Title.Caption:=  FFormCaption;

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth-1;
      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;


      AData.DataSet.First;
      While not AData.DataSet.Eof do
      begin
        LYear := AData.DataSet.FieldByName('Year').AsInteger;
        for LIndex := 1 to 12 do
        begin
          LDate := EncodeDate(LYear,LMonthNumbers[LIndex-1],1);
          LFieldName    := Format('%s%2.2d',['Value',LIndex]);
          LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
          ViewDialog.LineSeries.AddXY(LDate,LMonthlyValue);
          if(LMonthNumbers[LIndex-1] = 12) then
            LYear := LYear + 1;
        end;
        AData.DataSet.Next;
      end;

      SetLength(LYValues,ViewDialog.LineSeries.YValues.Count);
      for LIndex := Low(LYValues) to High(LYValues) do
        LYValues[LIndex] := ViewDialog.LineSeries.YValues[LIndex];

      LMean   := Mean(LYValues);
      try
        LStdDev := StdDev(LYValues);
      except
        LStdDev := 0.0;
      end;

      for LIndex := Low(LYValues) to High(LYValues) do
      begin
        ViewDialog.MeanLineSeries.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean);
        ViewDialog.StdDevLineSeriesMAbove.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean + LStdDev);
        ViewDialog.StdDevLineSeriesMBelow.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean - LStdDev);
      end;
      ViewDialog.MeanLabel.Caption := Format(FAppModules.Language.GetString('LabelMean.Caption'),
                                      [FormatFloat('0.00',LMean),FormatFloat('0.00',LStdDev)]);
      OnMeanChkBoxClick(nil);
      OnStdDevChkBoxClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.PopulateSFRUnitRunoffChartData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.PopulateSFRUnitRunoffChartData';
var
  LMonthNumbers  : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex: integer;
  LMonthlyValue : double;
  LDate: TDateTime;
  LFieldName: string;
  LYValues : array of double;
  LMean,
  LStdDev: double;
begin
  try
    //ClearChart;
    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.LineSeries.Active := True;
      ViewDialog.LineSeries.XValues.DateTime := True;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;
      ViewDialog.Chart.Title.Caption:=  FFormCaption;

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth-1;
      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;


      AData.DataSet.First;
      While not AData.DataSet.Eof do
      begin
        LYear := AData.DataSet.FieldByName('Year').AsInteger;
        for LIndex := 1 to 12 do
        begin
          LDate := EncodeDate(LYear,LMonthNumbers[LIndex-1],1);
          LFieldName    := Format('%s%2.2d',['Value',LIndex]);
          LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
          ViewDialog.LineSeries.AddXY(LDate,LMonthlyValue);
          if(LMonthNumbers[LIndex-1] = 12) then
            LYear := LYear + 1;
        end;
        AData.DataSet.Next;
      end;

      SetLength(LYValues,ViewDialog.LineSeries.YValues.Count);
      for LIndex := Low(LYValues) to High(LYValues) do
        LYValues[LIndex] := ViewDialog.LineSeries.YValues[LIndex];

      LMean   := Mean(LYValues);
      try
        LStdDev := StdDev(LYValues);
      except
        LStdDev := 0.0;
      end;

      for LIndex := Low(LYValues) to High(LYValues) do
      begin
        ViewDialog.MeanLineSeries.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean);
        ViewDialog.StdDevLineSeriesMAbove.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean + LStdDev);
        ViewDialog.StdDevLineSeriesMBelow.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean - LStdDev);
      end;
      ViewDialog.MeanLabel.Caption := Format(FAppModules.Language.GetString('LabelMean.Caption'),
                                      [FormatFloat('0.00',LMean),FormatFloat('0.00',LStdDev)]);
      OnMeanChkBoxClick(nil);
      OnStdDevChkBoxClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.SetGraphLabel(AValueType: TValueType);
const OPNAME = 'TInputGraphValidator.SetGraphLabel';
var
  LCount : Integer;
begin
  for LCount := 0 to ViewDialog.Chart.SeriesCount - 1 do
  begin
    ViewDialog.Chart.Series[LCount].Title         := '';
    ViewDialog.Chart.Series[LCount].ShowInLegend  := False;
  end;

  if ViewDialog.MeanCheckBox.Checked then
  begin
    for LCount := 0 to ViewDialog.Chart.SeriesCount - 1 do
    begin
      if (ViewDialog.Chart.Series[LCount].Tag = 2) and (AValueType = vtMonthly) then
      begin
        ViewDialog.Chart.Series[LCount].Title := Format(FAppModules.Language.GetString('TInputGraphDialog.MeanSeriesTitle'),
                                                   [FormatFloat('0.00',FSpecifiedMean)]);
      end;
      if (ViewDialog.Chart.Series[LCount].Tag = 3) and (AValueType = vtAnnual) then
      begin
        ViewDialog.Chart.Series[LCount].Title := Format(FAppModules.Language.GetString('TInputGraphDialog.MeanSeriesTitle'),
                                                   [FormatFloat('0.00',FSpecifiedMeanA)]);
      end;
    end;
  end;

  if ViewDialog.StdDevCheckBox.Checked then
  begin
    for LCount := 0 to ViewDialog.Chart.SeriesCount - 1 do
    begin
      if (ViewDialog.Chart.Series[LCount].Tag in [4]) and (AValueType = vtMonthly) then
      begin
        ViewDialog.Chart.Series[LCount].Title := Format(FAppModules.Language.GetString('TInputGraphDialog.StdDevAboveSeriesTitle'),
                                                    [FormatFloat('0.00',FSpecifiedStdDevMAbove)]);
      end;
      if (ViewDialog.Chart.Series[LCount].Tag in [5]) and (AValueType = vtMonthly) then
      begin
        ViewDialog.Chart.Series[LCount].Title := Format(FAppModules.Language.GetString('TInputGraphDialog.StdDevBelowSeriesTitle'),
                                                    [FormatFloat('0.00',FSpecifiedStdDevMBelow)]);
      end;
      if (ViewDialog.Chart.Series[LCount].Tag in [6]) and (AValueType = vtAnnual) then
      begin
        ViewDialog.Chart.Series[LCount].Title := Format(FAppModules.Language.GetString('TInputGraphDialog.StdDevAboveSeriesTitle'),
                                                    [FormatFloat('0.00',FSpecifiedStdDevAAbove)]);
      end;
      if (ViewDialog.Chart.Series[LCount].Tag in [7]) and (AValueType = vtAnnual) then
      begin
        ViewDialog.Chart.Series[LCount].Title := Format(FAppModules.Language.GetString('TInputGraphDialog.StdDevBelowSeriesTitle'),
                                                    [FormatFloat('0.00',FSpecifiedStdDevABelow)]);
      end;
    end;
  end;

  //Show the series in legend
  for LCount := 0 to ViewDialog.Chart.SeriesCount - 1 do
  begin
    if AValueType = vtAnnual then
    begin
      if ViewDialog.Chart.Series[LCount].Tag in [4,5] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := False;
        end;
      end;
      if ViewDialog.Chart.Series[LCount].Tag in [6,7] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := True;
        end;
      end;
      if ViewDialog.Chart.Series[LCount].Tag in [3] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := True;
        end;
      end;
      if ViewDialog.Chart.Series[LCount].Tag in [2] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := False;
        end;
      end;
    end;

    if AValueType = vtMonthly then
    begin
      if ViewDialog.Chart.Series[LCount].Tag in [4,5] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := True;
        end;
      end;
      if ViewDialog.Chart.Series[LCount].Tag in [6,7] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := False;
        end;
      end;
      if ViewDialog.Chart.Series[LCount].Tag in [2] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := True;
        end;
      end;
      if ViewDialog.Chart.Series[LCount].Tag in [3] then
      begin
        with ViewDialog do
        begin
          Chart.Series[LCount].ShowInLegend  := False;
        end;
      end;
    end;
  end;

  if (not ViewDialog.MeanCheckBox.Checked) and (not ViewDialog.StdDevCheckBox.Checked) then
    ViewDialog.Chart.Legend.Visible := False
  else
    ViewDialog.Chart.Legend.Visible := True;
end;

procedure TInputGraphValidator.OnMonthlyAnnualClick(Sender: TObject);
const OPNAME = 'TInputGraphValidator.OnMonthlyAnnualClick';
var
  LInflowFeature : ISpecifiedInflowFeature;
  LDemandFeature : ISpecifiedDemandFeature;
begin
  try
    ViewDialog.Chart.UndoZoom;
    LInflowFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FIdentifier];
    LDemandFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[FIdentifier];
    if ViewDialog.MonthlyAnnual.ItemIndex = 0 then
    begin
      ConfigureLineSeries(ViewDialog.LineSeries,vtMonthly);
      ConfigureChartBottomAxis(ViewDialog.LineSeries,vtMonthly);
      if(LInflowFeature <> nil) then
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.InflowFileMonthly');
      if(LDemandFeature <> nil) then
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.DemandFileMonthly');
      SetGraphsStatus(vtMonthly);
      SetGraphLabel(vtMonthly);
    end;
    if ViewDialog.MonthlyAnnual.ItemIndex = 1 then
    begin
      ConfigureLineSeries(ViewDialog.LineSeriesA,vtAnnual);
      ConfigureChartBottomAxis(ViewDialog.LineSeriesA,vtAnnual);
      if(LInflowFeature <> nil) then
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.InflowFile');
      if(LDemandFeature <> nil) then
        ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.DemandFile');
      SetGraphsStatus(vtAnnual);
      SetGraphLabel(vtAnnual);
    end;  
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TInputGraphValidator.GetSpecifiedDemandFeatureYearlyDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetSpecifiedDemandFeatureYearlyDataSet';
var
  LIndex          : integer;
  LSQL            : string;
  lFeature        : ISpecifiedDemandFeature;
  LFileNamesList  : TFileNamesList;
  LFileNameObject : TAbstractModelFileName;
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
          ViewDialog.Chart.Title.Text.Clear;
          ViewDialog.Chart.Title.Text.Add(LFileNameObject.ShortName);
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.DemandFile');
          LSQL := ' SELECT DemandYearValue as [Year],'+
                  ' DemandTotalValue '+
                  ' FROM DemandFileData A WHERE ' +
                   GetScenarioWhereClause +
                  ' AND FileNumber = '+IntToStr(LFileNameObject.FileNumber);
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.GetSpecifiedInflowFeatureDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetSpecifiedInflowFeatureDataSet';
var
  LIndex          : integer;
  LSQL            : string;
  LFeature        : ISpecifiedInflowFeature;
  LFileNamesList  : TFileNamesList;
  LFileNameObject : TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FIdentifier];
      if (LFeature <> nil) then
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
          ViewDialog.Chart.Title.Text.Clear;
          ViewDialog.Chart.Title.Text.Add(LFileNameObject.ShortName);
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.InflowFileMonthly');
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

procedure TInputGraphValidator.GetSpecifiedInflowFeatureYearlyDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetSpecifiedInflowFeatureYearlyDataSet';
var
  LIndex          : integer;
  LSQL            : string;
  LFeature        : ISpecifiedInflowFeature;
  LFileNamesList  : TFileNamesList;
  LFileNameObject : TAbstractModelFileName;
begin
  try
    if (AData <> nil) then
    begin
      LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[FIdentifier];
      if (LFeature <> nil) then
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
          ViewDialog.Chart.Title.Text.Clear;
          ViewDialog.Chart.Title.Text.Add(LFileNameObject.ShortName);
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.InflowFile');
          LSQL := ' SELECT HydroYearValue as [Year],'+
                  ' HydroTotalValue '+
                  ' FROM HydrologyFileData A WHERE A.StudyAreaName = ' +  QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                  ' AND FileName = ' + QuotedStr(LFileNameObject.ShortName) +
                  ' ORDER BY HydroYearValue';
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.PopulateSpecifiedDemandChartYearlyData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.PopulateSpecifiedDemandChartYearlyData';
var
  LMonthNumbers  : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex: integer;
  LYearlyValue : double;
  LYValues : array of double;
  LMean,
  LStdDev: double;
begin
  try
    //ClearChart;
    FSpecifiedMeanA         := 0.0;
    FSpecifiedStdDevAAbove  := 0.0;
    FSpecifiedStdDevABelow  := 0.0;

    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.LineSeriesA.Active             := True;
      ViewDialog.LineSeriesA.XValues.DateTime   := False;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      ViewDialog.Chart.BottomAxis.LabelsAngle   := 90;

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth-1;
      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;

      AData.DataSet.First;
      while not AData.DataSet.Eof do
      begin
        LYear         := AData.DataSet.FieldByName('Year').AsInteger;
        LYearlyValue  := AData.DataSet.FieldByName('DemandTotalValue').AsFloat;
        ViewDialog.LineSeriesA.AddXY(LYear,LYearlyValue);

        AData.DataSet.Next;
      end;

      SetLength(LYValues,ViewDialog.LineSeriesA.YValues.Count);
      for LIndex := Low(LYValues) to High(LYValues) do
        LYValues[LIndex] := ViewDialog.LineSeriesA.YValues[LIndex];

      LMean   := Mean(LYValues);
      try
        LStdDev := StdDev(LYValues);
      except
        LStdDev := 0.0;
      end;

      for LIndex := Low(LYValues) to High(LYValues) do
      begin
        ViewDialog.MeanLineSeriesA.AddXY(ViewDialog.LineSeriesA.XValues[LIndex],LMean);
        ViewDialog.StdDevLineSeriesAAbove.AddXY(ViewDialog.LineSeriesA.XValues[LIndex],LMean + LStdDev);
        ViewDialog.StdDevLineSeriesABelow.AddXY(ViewDialog.LineSeriesA.XValues[LIndex],LMean - LStdDev);
      end;
      //ViewDialog.MeanLabel.Caption := Format(FAppModules.Language.GetString('LabelMean.Caption'),
        //                              [FormatFloat('0.00',LMean),FormatFloat('0.00',LStdDev)]);

      FSpecifiedMeanA         := LMean;
      FSpecifiedStdDevAAbove  := LMean + LStdDev;
      FSpecifiedStdDevABelow  := LMean - LStdDev;

      OnMeanChkBoxClick(nil);
      OnStdDevChkBoxClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.PopulateSpecifiedInflowChartMonthlyData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.PopulateSpecifiedInflowChartMonthlyData';
var
  LMonthNumbers : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex        : integer;
  LMonthlyValue : double;
  LDate         : TDateTime;
  LFieldName    : string;
  LYValues      : array of double;
  LMean,
  LStdDev       : double;
begin
  try
    //ClearChart;
    FSpecifiedMean    := 0.0;
    FSpecifiedMeanA   := 0.0;

    FSpecifiedStdDevMAbove  := 0.0;
    FSpecifiedStdDevMBelow  := 0.0;

    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.LineSeries.Active            := True;
      ViewDialog.LineSeries.XValues.DateTime  := True;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      ViewDialog.Chart.BottomAxis.LabelsAngle := 90;

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth - 1;
      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;

      AData.DataSet.First;
      While not AData.DataSet.Eof do
      begin
        LYear := AData.DataSet.FieldByName('Year').AsInteger;
        for LIndex := 1 to 12 do
        begin
          LDate := EncodeDate(LYear,LMonthNumbers[LIndex - 1],1);
          LFieldName    := Format('%s%2.2d',['Value',LIndex]);
          LMonthlyValue := AData.DataSet.FieldByName(LFieldName).AsFloat;
          ViewDialog.LineSeries.AddXY(LDate,LMonthlyValue);
          if(LMonthNumbers[LIndex - 1] = 12) then
            LYear := LYear + 1;
        end;
        AData.DataSet.Next;
      end;

      SetLength(LYValues,ViewDialog.LineSeries.YValues.Count);
      for LIndex := Low(LYValues) to High(LYValues) do
        LYValues[LIndex] := ViewDialog.LineSeries.YValues[LIndex];

      LMean := Mean(LYValues);
      try
        LStdDev := StdDev(LYValues);
      except
        LStdDev := 0.0;
      end;

      for LIndex := Low(LYValues) to High(LYValues) do
      begin
        ViewDialog.MeanLineSeries.AddXY(ViewDialog.LineSeries.XValues[LIndex],LMean);
        ViewDialog.StdDevLineSeriesMAbove.AddXY(ViewDialog.LineSeries.XValues[LIndex], LMean + LStdDev);
        ViewDialog.StdDevLineSeriesMBelow.AddXY(ViewDialog.LineSeries.XValues[LIndex], LMean - LStdDev);
      end;

      FSpecifiedMean          := LMean;
      FSpecifiedStdDevMAbove  := LMean + LStdDev;
      FSpecifiedStdDevMBelow  := LMean - LStdDev;

      OnMeanChkBoxClick(nil);
      OnStdDevChkBoxClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.PopulateSpecifiedInflowChartYearlyData(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.PopulateSpecifiedInflowChartYearlyData';
var
  LMonthNumbers  : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex         : integer;
  LYearlyValue   : double;
  LYValues       : array of double;
  LMean,
  LStdDev        : double;
begin
  try
    //ClearChart;
    FSpecifiedMeanA         := 0.0;
    FSpecifiedStdDevAAbove  := 0.0;
    FSpecifiedStdDevABelow  := 0.0;

    if (AData <> nil) and (not(AData.DataSet.Eof and AData.DataSet.Bof)) then
    begin
      ViewDialog.LineSeriesA.Active             := True;
      ViewDialog.LineSeriesA.XValues.DateTime   := False;
      ViewDialog.Chart.BottomAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.Time');
      ViewDialog.Chart.BottomAxis.LabelsAngle   := 90;

      LStartMonth := FAppModules.StudyArea.CalendarStartMonth - 1;
      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;

      AData.DataSet.First;
      while not AData.DataSet.Eof do
      begin
        LYear         := AData.DataSet.FieldByName('Year').AsInteger;
        LYearlyValue  := AData.DataSet.FieldByName('HydroTotalValue').AsFloat;
        ViewDialog.LineSeriesA.AddXY(LYear,LYearlyValue);
        AData.DataSet.Next;
      end;

      SetLength(LYValues,ViewDialog.LineSeriesA.YValues.Count);
      for LIndex := Low(LYValues) to High(LYValues) do
        LYValues[LIndex] := ViewDialog.LineSeriesA.YValues[LIndex];

      LMean := Mean(LYValues);
      try
        LStdDev := StdDev(LYValues);
      except
        LStdDev := 0.0;
      end;

      for LIndex := Low(LYValues) to High(LYValues) do
      begin
        ViewDialog.MeanLineSeriesA.AddXY(ViewDialog.LineSeriesA.XValues[LIndex],LMean);
        ViewDialog.StdDevLineSeriesAAbove.AddXY(ViewDialog.LineSeriesA.XValues[LIndex],LMean + LStdDev);
        ViewDialog.StdDevLineSeriesABelow.AddXY(ViewDialog.LineSeriesA.XValues[LIndex],LMean - LStdDev);
      end;

      FSpecifiedMeanA         := LMean;
      FSpecifiedStdDevAAbove  := LMean + LStdDev;
      FSpecifiedStdDevABelow  := LMean - LStdDev;

      OnMeanChkBoxClick(nil);
      OnStdDevChkBoxClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.ConfigureChartBottomAxis(ALineSeries: TLineSeries; AValueType: TValueType);
const OPNAME = 'TInputGraphValidator.ConfigureChartBottomAxis';
begin
  try
    case AValueType of
      vtAnnual:
        ViewDialog.Chart.BottomAxis.AxisValuesFormat  := '###0';
      vtMonthly:
        ViewDialog.Chart.BottomAxis.DateTimeFormat    := 'yyyy/mm';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.ConfigureLineSeries(ALineSeries: TLineSeries; AValueType: TValueType);
const OPNAME = 'TInputGraphValidator.ConfigureLineSeries';
begin
  try
    if Assigned(ALineSeries) then
    begin
      case AValueType of
        vtAnnual  : ALineSeries.XValues.DateTime := False;
        vtMonthly : ALineSeries.XValues.DateTime := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphValidator.SetGraphsStatus(AValueType: TValueType);
const OPNAME = 'TInputGraphValidator.SetGraphsStatus';
var
  LCount : Integer;
begin
  if AValueType = vtAnnual then
  begin
    with ViewDialog do
    begin
      for LCount := 0 to Chart.SeriesCount - 1 do
      begin
        if Chart.Series[LCount].Tag in [1] then
          Chart.Series[LCount].Active := True;

        if (Chart.Series[LCount].Tag in [3]) then
          if ViewDialog.MeanCheckBox.Checked then
            Chart.Series[LCount].Active := True
          else
            Chart.Series[LCount].Active := False;

        if (Chart.Series[LCount].Tag in [6,7]) then
          if ViewDialog.StdDevCheckBox.Checked then
            Chart.Series[LCount].Active := True
          else
            Chart.Series[LCount].Active := False;

        if Chart.Series[LCount].Tag in [0,2,4,5] then
          Chart.Series[LCount].Active   := False;
      end;
    end;
  end;

  if AValueType = vtMonthly then
  begin
    with ViewDialog do
    begin
      for LCount := 0 to Chart.SeriesCount - 1 do
      begin
        if Chart.Series[LCount].Tag in [1,3,6,7] then
          Chart.Series[LCount].Active := False;

        if Chart.Series[LCount].Tag in [0] then
          Chart.Series[LCount].Active := True;

        if (Chart.Series[LCount].Tag in [2]) then
          if ViewDialog.MeanCheckBox.Checked then
            Chart.Series[LCount].Active := True
          else
            Chart.Series[LCount].Active := False;

        if (Chart.Series[LCount].Tag in [4,5]) then
          if ViewDialog.StdDevCheckBox.Checked then
            Chart.Series[LCount].Active := True
          else
            Chart.Series[LCount].Active := False;
      end;
    end;
  end;
end;

procedure TInputGraphValidator.GetIrrigationBlockDemandFeatureYearlyDataSet(AData: TAbstractModelDataset);
const OPNAME = 'TInputGraphValidator.GetIrrigationBlockDemandFeatureYearlyDataSet';
var
  LIndex          : integer;
  LSQL            : string;
  lFeature        : IIrrigationBlock;
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
          ViewDialog.Chart.Title.Text.Clear;
          ViewDialog.Chart.Title.Text.Add(LFileNameObject.ShortName);
          ViewDialog.Chart.LeftAxis.Title.Caption := FAppModules.Language.GetString('DialogCaption.DemandFile');
          LSQL := ' SELECT DemandYearValue as [Year],'+
                  ' DemandTotalValue '+
                  ' FROM DemandFileData A WHERE ' +
                   GetScenarioWhereClause +
                  ' AND FileNumber = '+IntToStr(LFileNameObject.FileNumber);
          AData.DataSet.Close;
          AData.SetSQL(LSQL);
          AData.DataSet.Open;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

