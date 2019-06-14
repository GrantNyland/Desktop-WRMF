//
//
//  UNIT      : Contains TTimeSeriesComparitorPopulatingAgent Class
//  AUTHOR    : Dziedzi Ramulondi (Cornastone)
//  DATE      : 2003/06/12
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorPopulatingAgent;

interface

uses
  DB,
  Classes,
  VCLTee.Chart,
  VCL.ComCtrls,
  VoaimsCom_TLB,
  UAbstractObject,
  UViewDataItem,
  VCL.Dialogs,
  Dbclient,

  UTimeSeriesComparitorLinkClasses,
  UTimeSeriesComparitorDisplayPanel,
  UTimeSeriesComparitorSubAreaDataManager,
  UTimeSeriesComparitorScenarioDataManager,

  UTimeSeriesComparitorViewManager,
  UTimeSeriesComparitorViewList,

  UTimeSeriesComparitorChartManager,
  UTimeSeriesComparitorChartList,

  UTimeSeriesComparitorSeriesManager,
  UTimeSeriesComparitorSeriesList;

type
  TGraphTreeItemDataObject = class(TAbstractAppObject)
  protected
    FLeftAxisCaption: string;
    FLeftAxisFormat: string;
    FLeftAxisUnits: string;
    FBottomAxisCaption: string;
    FBottomAxisFormat: string;
    FBottomAxisUnits: string;
    FGraphHint: string;
  public
    constructor Create(AAppModules: TAppModules);
    procedure PopulateTreeNodeDataObject(AViewID: string);
    property LeftAxisCaption: string read FLeftAxisCaption write FLeftAxisCaption;
    property LeftAxisFormat: string read FLeftAxisFormat write FLeftAxisFormat;
    property LeftAxisUnits: string read FLeftAxisUnits write FLeftAxisUnits;
    property BottomAxisCaption: string read FBottomAxisCaption write FBottomAxisCaption;
    property BottomAxisFormat: string read FBottomAxisFormat write FBottomAxisFormat;
    property BottomAxisUnits: string read FBottomAxisUnits write FBottomAxisUnits;
    property GraphHint: string read FGraphHint write FGraphHint;
  end;

type
  TTimeSeriesComparitorPopulatingAgent = class(TAbstractAppObject)
  private
    procedure PopulateSeriesFromClientDataset(AType: TChartSeriesType;
      AGraphData: TClientDataSet ; ASeries: TTimeSeriesComparitorSeries;
      ACurrentView: TTimeSeriesComparitorView;
      ADataObject: TViewDataTreeNodeData);

  protected
    FCurrentDataset: TAbstractModelDataset;
    FTreeNodeItems: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function SeriesCommaTextName(ADataObject: TViewDataTreeNodeData): string;
    function DeleteSubAreaDataFromDatabase: boolean;
    (*
      // Added by JDT
         Added the Isnumeric Function to determine if the the values
         Added the GetDelimitext since commatext directly from Stringlist Cuts if there are spaces in the name

     *)
    procedure GetDelimitedTextFromString(const AString: string; separator: Char; substrings: TStringList);
    function IsNumeric(AString: String): Boolean;
    function FormatSeriesName(ASeriesName : string) : string;

  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;

    function PopulateNodesData(ANodesList: TTreeNodes): boolean;

    function SeriesCommaTextCaption(ADataObject: TViewDataTreeNodeData): string;
    function PopulateCurrentSeries(ASQLIndex: integer; ASeries: TTimeSeriesComparitorSeries;
                                   ADataObject: TViewDataTreeNodeData;
                                   ACurrentView: TTimeSeriesComparitorView):boolean; virtual;
    procedure PopulateSeriesFromDataset(AType: TChartSeriesType; AGraphData: TDataSet;
                                        ASeries: TTimeSeriesComparitorSeries;
                                        ACurrentView: TTimeSeriesComparitorView;
                                        ADataObject: TViewDataTreeNodeData); virtual;
    procedure PopulateSeriesFromFile(AType: TChartSeriesType;
                                        ASeries: TTimeSeriesComparitorSeries;
                                        ACurrentView: TTimeSeriesComparitorView;
                                        ADataObject: TViewDataTreeNodeData); virtual;
    procedure PostProccessSeriesData(ASeries: TTimeSeriesComparitorSeries);
    procedure PopulateEmptySeriesForCurrentScenario(ASeriesDataManager: TTimeSeriesComparitorSeriesManager;
                                                    ACurrentView: TTimeSeriesComparitorView);
    function PopulateSubAreaDataFromDatabase(
      ADisplayPanel    : TTimeSeriesComparitorDisplayPanel;
      ASubareaDataManager: TTimeSeriesComparitorSubareaDataManager;
      AScenarioViewData : TTimeSeriesComparitorScenarioViewData;
      ASeriesDataManager: TTimeSeriesComparitorSeriesManager;
      var ACurrentSeries: TTimeSeriesComparitorSeries;
      AChartDataManager: TTimeSeriesComparitorChartManager;
      var ACurrentChart: TTimeSeriesComparitorChart;
      AViewDataManager: TTimeSeriesComparitorViewManager;
      var ACurrentView: TTimeSeriesComparitorView
    ): boolean;
    function SaveSubAreaDataToDatabase(
      ADisplayPanel    : TTimeSeriesComparitorDisplayPanel;
      ASubareaDataManager: TTimeSeriesComparitorSubareaDataManager;
      AScenarioViewData : TTimeSeriesComparitorScenarioViewData;
      ASeriesDataManager: TTimeSeriesComparitorSeriesManager;
      ACurrentSeries: TTimeSeriesComparitorSeries;
      AChartDataManager: TTimeSeriesComparitorChartManager;
      ACurrentChart: TTimeSeriesComparitorChart;
      AViewDataManager: TTimeSeriesComparitorViewManager;
      ACurrentView: TTimeSeriesComparitorView
    ): boolean;
    function GetSeriesUnitsFromName(AName : string) : string;
    property CurrentDataset     : TAbstractModelDataset        read FCurrentDataset    write FCurrentDataset;
  end;

implementation

uses
  System.UITypes,
  VCLTee.Series,
  Math,
  VCLTee.TeEngine,
  SysUtils,
  UDataSetType,
  VCL.Graphics,
  VCLTee.TeeProcs,
  UUtilities,
  //UOutputData,
  UErrorHandlingOperations, StrUtils;


{ TTimeSeriesComparitorPopulatingAgent }

procedure TTimeSeriesComparitorPopulatingAgent.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTreeNodeItems        := TStringList.Create;
    FTreeNodeItems.Sorted := True;
    FTreeNodeItems.Duplicates := dupAccept;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorPopulatingAgent.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FTreeNodeItems);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.Initialise';
begin
  Result := inherited Initialise;
  try
    FTreeNodeItems.Clear;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FTreeNodeItems.Clear;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.PopulateCurrentSeries(ASQLIndex: integer;
         ASeries: TTimeSeriesComparitorSeries; ADataObject: TViewDataTreeNodeData;
         ACurrentView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PopulateCurrentSeries';
var
  LDataset      : TAbstractModelDataset;
begin
  Result := False;
  try
    if Assigned(ASeries) and Assigned(ADataObject) and Assigned(ADataObject.ViewDataNode) then
    begin
      if Assigned(FAppModules) and Assigned(FAppModules.Model()) and
        (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
      begin
        // This is to ensure the Broadscale Pre-processor/PesIMS system can use nornalised datasets
        // that are de-normalised into a "in-memory" client dataset or similar
        // VGN 2007/01/10
         // JDT note Use submodels when submodels come in
         //Showmessage(inttostr(TClientDataSet(CurrentDataset).RecordCount));
        TClientDataSet(CurrentDataset).Open;
        try
                                          //
          PopulateSeriesFromClientDataset(cstLineDateTime,TClientDataSet(CurrentDataset) ,ASeries,ACurrentView,ADataObject);
          PostProccessSeriesData(ASeries);
          Result := True;
        finally
          TClientDataSet(CurrentDataset).Close;
        End;
      end
      else
      begin
        if(ASQLIndex >= 0) and (ASQLIndex < ADataObject.ViewDataNode.ViewDataSetCount) then
        begin
          FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
          if Assigned(LDataSet) then
          begin
            try
              LDataSet.SetSQL(ADataObject.ViewDataNode.ViewDataSet[ASQLIndex].ViewSQL);
              LDataSet.DataSet.Close;
              FAppModules.StudyArea.SetDefaultParams(LDataSet);
              LDataSet.SetParams(['AScenarioCode'],[ASeries.ScenarioCode]);
              if (ADataObject.ViewDataNode.ViewDataSet[ASQLIndex].ParamCount > 0) then
                LDataSet.SetParams(
                  ADataObject.ViewDataNode.ViewDataSet[ASQLIndex].ParamNames,
                  ADataObject.ViewDataNode.ViewDataSet[ASQLIndex].ParamValues);

              if not (ADataObject.ViewDataNode.ViewDataSet[ASQLIndex].LoadFromFile) and (LDataSet.AreAllParamsBound) then
              begin
                LDataSet.DataSet.Open;
                try
                  PopulateSeriesFromDataset(cstLineDateTime,LDataset.DataSet,ASeries,ACurrentView,ADataObject);
                  PostProccessSeriesData(ASeries);
                finally
                  LDataSet.DataSet.Close;
                end;
              end
              else
              if ADataObject.ViewDataNode.ViewDataSet[ASQLIndex].LoadFromFile then
              begin
                PopulateSeriesFromFile(cstLineDateTime,ASeries,ACurrentView,ADataObject);
                PostProccessSeriesData(ASeries);
              end;
            finally
              FreeAndNil(LDataSet);
            end;
            Result := True;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorPopulatingAgent.FormatSeriesName(ASeriesName : string) : string;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.FormatSeriesName';
var
  LTmpStr : string;
  LPos : integer;
begin
  Result := '';
  try
    LTmpStr := ASeriesName;
    LPos    := 21;
    Delete(LTmpStr, LPos, (Length(LTmpStr) - LPos) + 1);
    Result  := LTmpStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorPopulatingAgent.PopulateSeriesFromDataset(AType: TChartSeriesType; AGraphData: TDataSet;
          ASeries: TTimeSeriesComparitorSeries;
          ACurrentView: TTimeSeriesComparitorView;
          ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PopulateSeriesFromDataset';
var
  LLineSeries: TLineSeries;
  //LSeriesName: string;
  LLanguageStr,
  LCaption,
  LUnits : string;
  LXValues: TStringList;
  //LValidAxisCaption : boolean;
  LYear,LIndex: integer;
  LData : TGraphTreeItemDataObject;
begin
  try
    // Loop for all records of the current series.
    LXValues := TStringList.Create;
    try
      LData := TGraphTreeItemDataObject(ADataObject.Data);
      if Assigned(LData) then
      begin
        LLanguageStr        := LData.LeftAxisCaption;
        LUnits              := LData.LeftAxisUnits;
        LCaption := FAppModules.Language.GetString(LLanguageStr);
        ASeries.AxisCaption := LCaption + ' (' + LUnits + ')';
        ASeries.Units := LUnits;
      end;

      while (not AGraphData.EOF) do
      begin
        LXValues.CommaText := Trim(AGraphData.FieldByName('XValues').AsString);
        LLineSeries := ASeries.LineSeries;
        LYear := AGraphData.FieldByName('Year').AsInteger;
        for LIndex := 0 to LXValues.Count - 1 do
          LLineSeries.AddXY(EncodeDate(LYear, 1 + LIndex, 1), AGraphData.FieldByName(LXValues[LIndex]).AsFloat);
        LLineSeries.Title := LowerCase(FormatSeriesName(ASeries.SeriesName));
        AGraphData.Next;
      end;
    finally
      LXValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorPopulatingAgent.PopulateSeriesFromFile(AType: TChartSeriesType;
                                                                      ASeries: TTimeSeriesComparitorSeries;
                                                                      ACurrentView: TTimeSeriesComparitorView;
                                                                      ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PopulateSeriesFromFile';
var
  LLineSeries: TLineSeries;
  LErrors,
  LDataStr : WideString;
  LLanguageStr,
  LCaption,
  LUnits : string;
  LValues: TStringList;
  LXValues: TStringList;
  LYear,LIndex,LCount: integer;
  LData : TGraphTreeItemDataObject;
  LOutputData: IOutputData;
  LElementDataType:TOutputDataType;
  LElementID : integer;
  LRun : IRunConfigurationData;
begin
  try
    LRun := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;
    LDataStr := ADataObject.ViewDataNode.OverrideCaption;
    LElementID := ADataObject.ViewDataNode.ElementID;
    LElementDataType := GetElementDataTypeBySubNodesDatasetID(ADataObject.ViewDataNode.SubNodesDatasetID);
    LOutputData := (FAppModules.Model.ModelData as IYieldModelData).OutputData;
    if LOutputData <> nil then
    begin
      LOutputData.GetSelection.LoadCase := ADataObject.ViewDataNode.LoadCase;
      if LRun <> nil then
        if LRun.RunSequenceType = 'S' then
          ADataObject.ViewDataNode.Sequence := ADataObject.ViewDataNode.Sequence;
      LOutputData.SummaryOutputData.GetBlockDataByElementID(LDataStr,LElementDataType,LElementID,LErrors);
    end;
    if Trim(LDataStr) <> '' then
    begin
      LValues := TStringList.Create;
      LXValues := TStringList.Create;
      try
        LData := TGraphTreeItemDataObject(ADataObject.Data);
        if Assigned(LData) then
        begin
          LLanguageStr        := LData.LeftAxisCaption;
          LUnits              := LData.LeftAxisUnits;
          LCaption := FAppModules.Language.GetString(LLanguageStr);
          ASeries.AxisCaption := LCaption + ' (' + LUnits + ')';
          ASeries.Units := LUnits;
        end;

          LValues.CommaText := LDataStr;
          LLineSeries := ASeries.LineSeries;
          for LIndex := 0 to LValues.Count - 1 do
          begin
            LXValues.CommaText := LValues[LIndex];
            LYear := StrToInt(LXValues[0]);
            for LCount := 1 to LXValues.Count-3 do
              LLineSeries.AddXY(EncodeDate(LYear, 1 + LCount, 1), StrToFloat(LXValues[LCount]));
          end;
          LLineSeries.Title := LowerCase(FormatSeriesName(ASeries.SeriesName));
      finally
        LXValues.Free;
        LValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;

end;


procedure TTimeSeriesComparitorPopulatingAgent.PostProccessSeriesData(ASeries: TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PostProccessSeriesData';
var
  LMaxValue: double;
begin
  try
    if Assigned(ASeries) and Assigned(ASeries.LineSeries)then
    begin
      LMaxValue := Abs(Max(Abs(ASeries.LineSeries.YValues.MaxValue),Abs(ASeries.LineSeries.YValues.MinValue)));
      if (LMaxValue > 0.0) then
      begin
        ASeries.AbsoluteMaxValue := LMaxValue;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.SeriesCommaTextCaption(ADataObject: TViewDataTreeNodeData): string;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.SeriesCommaTextCaption';
var
  LNames: TStringList;
begin
  Result := '';
  try
    if Assigned(ADataObject) and Assigned(ADataObject.TreeNode) and (ADataObject.TreeNode.Level > 0)then
    begin
      LNames := TStringList.Create;
      try
        if (FAppModules.StudyArea.ModelCode = CRainfall) then
        begin
          if (ADataObject.TreeNode.Level = 1) then
          begin
            LNames.Add(ADataObject.TreeNode.Text);
            if (ADataObject.TreeNode.Count > 0) then
              LNames.Add(ADataObject.TreeNode.Item[0].Text);
          end
          else
          if (ADataObject.TreeNode.Level = 2) then
          begin
            LNames.Add(ADataObject.TreeNode.Parent.Text);
            LNames.Add(ADataObject.TreeNode.Text);
          end
          else
          if (ADataObject.TreeNode.Level = 3) then
          begin
            if (ADataObject.TreeNode.Text = 'WRC') then
            begin
              LNames.Add(ADataObject.TreeNode.Parent.Parent.Text);
              LNames.Add(ADataObject.TreeNode.Parent.Text);
              LNames.Add(ADataObject.TreeNode.Text);
            end
            else
            begin
              LNames.Add(FAppModules.StudyArea.StudyAreaCode);
              LNames.Add(FAppModules.StudyArea.SubAreaCode);
              LNames.Add(ADataObject.TreeNode.Parent.Parent.Text);
              LNames.Add(ADataObject.TreeNode.Parent.Text);
              LNames.Add(ADataObject.TreeNode.Text);
            end;
          end;
        end
        else
        begin
          LNames.Add(FAppModules.StudyArea.ScenarioCode);
          if(ADataObject.TreeNode.Level = 1) then
            LNames.Add('');
          if(ADataObject.TreeNode.Level = 2) then
            LNames.Add(ADataObject.TreeNode.Parent.Parent.Text);
          LNames.Add(ADataObject.TreeNode.Parent.Text);
          LNames.Add(ADataObject.TreeNode.Text);
        end;
        Result := LNames.CommaText;
      finally
        FreeAndNil(LNames);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.PopulateNodesData(ANodesList: TTreeNodes): boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PopulateNodesData';
var
  LIndex: integer;
  LSeriesName: string;
begin
  Result := False;
  try
    FTreeNodeItems.Clear;
    if Assigned(ANodesList) then
    begin
      for LIndex := 0 to ANodesList.Count-1 do
      begin
        if (ANodesList.Item[LIndex].Level = 2) OR
           ((FAppModules.Model.ModelName = CRainfall) AND (ANodesList.Item[LIndex].Level >= 1)) then
        begin
          if Assigned(ANodesList.Item[LIndex].Data)  then
          begin
            LSeriesName := SeriesCommaTextName(TViewDataTreeNodeData(ANodesList.Item[LIndex].Data));
            FTreeNodeItems.AddObject(LSeriesName,ANodesList.Item[LIndex].Data);
          end;
        end;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.SeriesCommaTextName(ADataObject: TViewDataTreeNodeData): string;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.SeriesCommaTextName';
var
  LNames: TStringList;
begin
  Result := '';
  try
    if Assigned(ADataObject) and Assigned(ADataObject.ViewDataNode) then
    begin
      LNames := TStringList.Create;
      try
        LNames.Add(ADataObject.ViewDataNode.TopParentID);
        LNames.Add(ADataObject.ViewDataNode.ParentID);
        LNames.Add(ADataObject.ViewDataNode.ViewID);
        LNames.Add(ADataObject.ViewDataNode.OverrideCaption);
        Result := LNames.CommaText;
      finally
        FreeAndNil(LNames);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.PopulateSubAreaDataFromDatabase(
  ADisplayPanel: TTimeSeriesComparitorDisplayPanel;
  ASubareaDataManager: TTimeSeriesComparitorSubareaDataManager;
  AScenarioViewData: TTimeSeriesComparitorScenarioViewData;
  ASeriesDataManager: TTimeSeriesComparitorSeriesManager;
  var ACurrentSeries: TTimeSeriesComparitorSeries;
  AChartDataManager: TTimeSeriesComparitorChartManager;
  var ACurrentChart: TTimeSeriesComparitorChart;
  AViewDataManager: TTimeSeriesComparitorViewManager;
  var ACurrentView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PopulateSubAreaDataFromDatabase';
var
  LSeries  : TTimeSeriesComparitorSeries;
  LChart   : TTimeSeriesComparitorChart;
  LView    : TTimeSeriesComparitorView;
  LDataset : TAbstractModelDataset;
  LIndex   : integer;
  LNodeDataIndex: integer;
  LTreeNodeData: TViewDataTreeNodeData;
  LSeriesName: string;
begin
  Result := False;
  try
    if Assigned(ADisplayPanel) and
       Assigned(ASubareaDataManager) and
       Assigned(AScenarioViewData) and
       Assigned(AViewDataManager) and
       Assigned(AChartDataManager) and
       Assigned(ASeriesDataManager) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      if Assigned(LDataSet) then
      begin
        try
          LDataSet.DataSet.Close;
          LDataSet.SetSQL('SELECT Model,StudyAreaName,SubArea,Scenario,ChartName,SeriesName,ViewID,ParentID,TopParentID,CommaTextCaption,Color,YAxisPosition'+
          ' FROM TSCSeries'+
          ' WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LSeries := ASeriesDataManager.CreateSeriesData(
                Trim(LDataSet.DataSet.FieldByName('Model').AsString),
                Trim(LDataSet.DataSet.FieldByName('StudyAreaName').AsString),
                Trim(LDataSet.DataSet.FieldByName('SubArea').AsString),
                Trim(LDataSet.DataSet.FieldByName('Scenario').AsString),
                Trim(LDataSet.DataSet.FieldByName('ChartName').AsString),
                Trim(LDataSet.DataSet.FieldByName('SeriesName').AsString),
                Trim(LDataSet.DataSet.FieldByName('ViewID').AsString),
                Trim(LDataSet.DataSet.FieldByName('ParentID').AsString),
                Trim(LDataSet.DataSet.FieldByName('TopParentID').AsString));
              if Assigned(LSeries) then
              begin
                LSeries.CommaTextCaption := Trim(LDataSet.DataSet.FieldByName('CommaTextCaption').AsString);
                LSeries.Color            := LDataSet.DataSet.FieldByName('Color').AsInteger;
                if (LDataSet.DataSet.FieldByName('YAxisPosition').AsInteger = 1) then
                  LSeries.LineSeries.VertAxis := aRightAxis
                else
                  LSeries.LineSeries.VertAxis := aLeftAxis;
                LSeries.Changed          := False;
                LSeries.AddedToChart     := True;
              end;
              LDataSet.DataSet.Next;
            end;
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('SELECT Model,StudyAreaName,SubArea,ChartName,CurrentSeriesName,DateCreated,HeaderCaption,FooterCaption'+
          ' FROM TSCChart'+
          ' WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LChart := AChartDataManager.CreateChartData(Trim(LDataSet.DataSet.FieldByName('ChartName').AsString));
              if Assigned(LChart) then
              begin
                LChart.ModelCode       :=  Trim(LDataSet.DataSet.FieldByName('Model').AsString);
                LChart.StudyAreaCode   :=  Trim(LDataSet.DataSet.FieldByName('StudyAreaName').AsString);
                LChart.SubAreaCode     :=  Trim(LDataSet.DataSet.FieldByName('SubArea').AsString);
                LChart.SavedSeriesName :=  Trim(LDataSet.DataSet.FieldByName('CurrentSeriesName').AsString);
                LChart.DateCreated     :=  LDataSet.DataSet.FieldByName('DateCreated').AsDateTime;
                LChart.HeaderCaption   :=  Trim(LDataSet.DataSet.FieldByName('HeaderCaption').AsString);
                LChart.FooterCaption   :=  Trim(LDataSet.DataSet.FieldByName('FooterCaption').AsString);
                LChart.Changed         :=  False;
              end;
              LDataSet.DataSet.Next;
            end;
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('SELECT Model,StudyAreaName,SubArea,ViewName,CurrentChartName,DateCreated,HeaderCaption,FooterCaption'+
          ' FROM TSCView'+
          ' WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LView := AViewDataManager.CreateViewData(Trim(LDataSet.DataSet.FieldByName('ViewName').AsString));
              if Assigned(LView) then
              begin
                LView.ModelCode      :=  Trim(LDataSet.DataSet.FieldByName('Model').AsString);
                LView.StudyAreaCode  :=  Trim(LDataSet.DataSet.FieldByName('StudyAreaName').AsString);
                LView.SubAreaCode    :=  Trim(LDataSet.DataSet.FieldByName('SubArea').AsString);
                LView.SavedChartName :=  Trim(LDataSet.DataSet.FieldByName('CurrentChartName').AsString);
                LView.DateCreated    :=  LDataSet.DataSet.FieldByName('DateCreated').AsDateTime;
                LView.HeaderCaption  :=  Trim(LDataSet.DataSet.FieldByName('HeaderCaption').AsString);
                LView.FooterCaption  :=  Trim(LDataSet.DataSet.FieldByName('FooterCaption').AsString);
                LView.Changed        :=  False;
                ADisplayPanel.SelectorsPanel.AddView(LView);
              end;
              LDataSet.DataSet.Next;
            end;
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('SELECT Model,StudyAreaName,SubArea,ViewName,ChartName,LeftAxisMin,LeftAxisMax,'+
          ' BottomAxisMin,BottomAxisMax,RightAxisMin,RightAxisMax'+
          ' FROM TSCViewChart'+
          ' WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LChart := AChartDataManager.ChartDataList.ChartByName(Trim(LDataSet.DataSet.FieldByName('ChartName').AsString));
              LView  := AViewDataManager.TSCViewDataList.ViewByName(Trim(LDataSet.DataSet.FieldByName('ViewName').AsString));
              if Assigned(LView) and Assigned(LChart) then
              begin
                LView.AddChart(LChart);
                if (LView.SavedChartName = lChart.ChartName) then
                  LView.SelectChart(LChart);
                LView.AddChartDataToView(LChart);
              end;
              LChart.LeftAxisMin := LDataSet.DataSet.FieldByName('LeftAxisMin').AsFloat;
              LChart.LeftAxisMax := LDataSet.DataSet.FieldByName('LeftAxisMax').AsFloat;
              LChart.BottomAxisMin := LDataSet.DataSet.FieldByName('BottomAxisMin').AsDateTime;
              LChart.BottomAxisMax := LDataSet.DataSet.FieldByName('BottomAxisMax').AsDateTime;
              LChart.RightAxisMin := LDataSet.DataSet.FieldByName('RightAxisMin').AsFloat;
              LChart.RightAxisMax := LDataSet.DataSet.FieldByName('RightAxisMax').AsFloat;
              LDataSet.DataSet.Next;
            end;
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('SELECT Model,StudyAreaName,SubArea,Scenario,ViewName'+
          ' FROM TSCViewScenario'+
          ' WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            if not LDataSet.DataSet.Eof  then
            begin
              AScenarioViewData.ModelCode       :=  Trim(LDataSet.DataSet.FieldByName('Model').AsString);
              AScenarioViewData.StudyAreaCode   :=  Trim(LDataSet.DataSet.FieldByName('StudyAreaName').AsString);
              AScenarioViewData.SubAreaCode     :=  Trim(LDataSet.DataSet.FieldByName('SubArea').AsString);
              AScenarioViewData.ScenarioCode    :=  Trim(LDataSet.DataSet.FieldByName('Scenario').AsString);
              AScenarioViewData.SenarioViewName :=  Trim(LDataSet.DataSet.FieldByName('ViewName').AsString);
            end;
          end;

          ACurrentView := AViewDataManager.ViewDataByName[AScenarioViewData.SenarioViewName];
          if Assigned(ACurrentView) then
          begin
            ACurrentChart := AChartDataManager.ChartDataByName[ACurrentView.SavedChartName];
          end;

          for LIndex := 0 to ASeriesDataManager.SeriesDataList.SeriessCount -1 do
          begin
            LSeries := ASeriesDataManager.SeriesDataList.SeriesByIndex[LIndex];
            LSeriesName := LSeries.CommaTextNameExcScenario;
            LNodeDataIndex := FTreeNodeItems.IndexOf(LSeriesName);
            if(LNodeDataIndex >= 0) then
            begin
              if Assigned(FTreeNodeItems.Objects[LNodeDataIndex]) then
              begin
                LTreeNodeData := TViewDataTreeNodeData(FTreeNodeItems.Objects[LNodeDataIndex]);
                PopulateCurrentSeries(0,LSeries,LTreeNodeData,ACurrentView)
              end;
            end;
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('SELECT Model,StudyAreaName,SubArea,ChartName,SeriesName'+
          ' FROM TSCChartSeries'+
          ' WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LSeriesName := AnsiDequotedStr(Trim(LDataSet.DataSet.FieldByName('SeriesName').AsString),#39);
              LSeries  := ASeriesDataManager.SeriesDataList.SeriesByCommaTextCaption(
                          Trim(LDataSet.DataSet.FieldByName('ChartName').AsString),LSeriesName);
              LChart   := AChartDataManager.ChartDataList.ChartByName(Trim(LDataSet.DataSet.FieldByName('ChartName').AsString));
              if Assigned(LSeries) and Assigned(LChart) then
              begin
                LChart.AddSeries(LSeries);
                if (LChart.SavedSeriesName = LSeries.CommaTextCaption) then
                  LChart.SelectSeries(LSeries, TRUE)
                else
                  LChart.SelectSeries(LSeries, FALSE);
                LChart.AddSeriesToChart(LSeries);
                LSeries.LineSeries.LinePen.Width := 0;
                LChart.SetChartAxis;
              end;

              LDataSet.DataSet.Next;
            end;
          end;
        finally
          LDataSet.DataSet.Close;
          FreeAndNil(LDataSet);
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.SaveSubAreaDataToDatabase(
  ADisplayPanel: TTimeSeriesComparitorDisplayPanel;
  ASubareaDataManager: TTimeSeriesComparitorSubareaDataManager;
  AScenarioViewData: TTimeSeriesComparitorScenarioViewData;
  ASeriesDataManager: TTimeSeriesComparitorSeriesManager;
  ACurrentSeries: TTimeSeriesComparitorSeries;
  AChartDataManager: TTimeSeriesComparitorChartManager;
  ACurrentChart: TTimeSeriesComparitorChart;
  AViewDataManager: TTimeSeriesComparitorViewManager;
  ACurrentView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.SaveSubAreaDataToDatabase';
var
  LSeries  : TTimeSeriesComparitorSeries;
  LChart   : TTimeSeriesComparitorChart;
  LView    : TTimeSeriesComparitorView;
  LDataset : TAbstractModelDataset;
  LIndex,
  LCount   : integer;
begin
  Result := False;
  try
    // Added by BSP since we need to exit this point if BSP not to save BSP to DB
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and
        (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
    begin
      exit;
    End;
    if Assigned(ADisplayPanel) and
       Assigned(ASubareaDataManager) and
       Assigned(AScenarioViewData) and
       Assigned(AViewDataManager) and
       Assigned(AChartDataManager) and
       Assigned(ASeriesDataManager) then
    begin
      if not DeleteSubAreaDataFromDatabase then
        Exit;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      if Assigned(LDataSet) then
      begin
        try
          if Assigned(ACurrentView) then
          begin
            ACurrentView.HeaderCaption := ADisplayPanel.ChartPanel.HeaderCaption.Caption;
            ACurrentView.FooterCaption := ADisplayPanel.ChartPanel.FooterCaption.Caption;
          end;

          for LIndex := 0 to ASeriesDataManager.SeriesDataList.SeriessCount -1 do
          begin
            LSeries := ASeriesDataManager.SeriesDataList.SeriesByIndex[LIndex];

            LDataSet.DataSet.Close;
            LDataSet.ClearSQL;
            LDataSet.ClearQueryParams();
            
            LDataSet.SetSQL('INSERT INTO TSCSeries'+
            ' (Model,StudyAreaName,SubArea,Scenario,ChartName,SeriesName,ViewID,ParentID,TopParentID,CommaTextCaption,Color,YAxisPosition)'+
            ' VALUES'+
            ' (:AModelCode,:AStudyAreaCode,:ASubAreaCode,:AScenarioCode,:AChartName,:ASeriesName,:AViewID,:AParentID,:ATopParentID,:ACommaTextCaption,:AColor,:AYAxisPosition)');

            LDataSet.SetParams(['AScenarioCode','AChartName','ASeriesName','AViewID','AParentID','ATopParentID','ACommaTextCaption','AColor','AYAxisPosition'],
                               [LSeries.ScenarioCode,LSeries.ChartName,LSeries.SeriesName,LSeries.ViewID,LSeries.ParentID,LSeries.TopParentID,LSeries.CommaTextCaption,IntToStr(LSeries.LineSeries.SeriesColor),IntToStr(Integer(LSeries.LineSeries.VertAxis))]);

            FAppModules.StudyArea.SetDefaultParams(LDataSet);
            LDataSet.SetParams(['AScenarioCode'],[LSeries.ScenarioCode]);
            if LDataSet.AreAllParamsBound then
              LDataSet.ExecSQL;

            LDataSet.DataSet.Close;
            LDataSet.ClearSQL;
            LDataSet.ClearQueryParams();

            LDataSet.SetSQL('INSERT INTO TSCChartSeries'+
            ' (Model,StudyAreaName,SubArea,ChartName,SeriesName)'+
            ' VALUES'+
            ' (:AModelCode,:AStudyAreaCode,:ASubAreaCode,:AChartName,:ASeriesName)');

            LDataSet.SetParams(['AChartName','ASeriesName'],
                               [LSeries.ChartName,QuotedStr(LSeries.CommaTextCaption)]);

            FAppModules.StudyArea.SetDefaultParams(LDataSet);
            if LDataSet.AreAllParamsBound then
              LDataSet.ExecSQL;
          end;

          for LIndex := 0 to AChartDataManager.ChartDataList.ChartCount -1 do
          begin
            LChart := AChartDataManager.ChartDataList.ChartByIndex[LIndex];
            LChart.HeaderCaption := LChart.Chart.Title.Text.Text;
            LDataSet.DataSet.Close;
            LDataSet.ClearSQL;
            LDataSet.ClearQueryParams();
            LDataSet.SetSQL('INSERT INTO TSCChart'+
            ' (Model,StudyAreaName,SubArea,ChartName,CurrentSeriesName,DateCreated,HeaderCaption,FooterCaption)'+
            ' VALUES'+
            ' (:AModelCode,:AStudyAreaCode,:ASubAreaCode,:AChartName,:ACurrentSeriesName,:DateCreated,:AHeaderCaption,:AFooterCaption)');

            LDataSet.SetParams(['AChartName','ACurrentSeriesName','DateCreated','AHeaderCaption','AFooterCaption'],
                               [LChart.ChartName,LChart.SavedSeriesName ,DateTimeToStr(LChart.DateCreated),LChart.HeaderCaption,LChart.FooterCaption]);



            FAppModules.StudyArea.SetDefaultParams(LDataSet);
            if LDataSet.AreAllParamsBound then
              LDataSet.ExecSQL;

            for LCount := 0 to LChart.ChartInViewList.Count -1 do
            begin
              LDataSet.DataSet.Close;
              LDataSet.ClearSQL;
              LDataSet.ClearQueryParams();
              LDataSet.SetSQL('INSERT INTO TSCViewChart'+
              ' (Model,StudyAreaName,SubArea,ViewName,ChartName,LeftAxisMin,LeftAxisMax,BottomAxisMin,BottomAxisMax,'+
              '  RightAxisMin,RightAxisMax)'+
              ' VALUES'+
              ' (:AModelCode,:AStudyAreaCode,:ASubAreaCode,:AViewName,:AChartName,:ALeftAxisMin,:ALeftAxisMax,:ABottomAxisMin,:ABottomAxisMax,'+
              '  :ARightAxisMin,:ARightAxisMax)');

              LDataSet.SetParams(['AViewName','AChartName'],
                                 [LChart.ChartInViewList[LCount],LChart.ChartName]);

              LDataSet.SetParams(['ALeftAxisMin','ALeftAxisMax'],
                                 [FloatToStr(LChart.Chart.LeftAxis.Minimum),FloatToStr(LChart.Chart.LeftAxis.Maximum)]);

              LDataSet.SetParams(['ABottomAxisMin','ABottomAxisMax'],
                                 [DateToStr(LChart.Chart.BottomAxis.Minimum),DateTostr(LChart.Chart.BottomAxis.Maximum)]);

              LDataSet.SetParams(['ARightAxisMin','ARightAxisMax'],
                                 [FloatToStr(LChart.Chart.RightAxis.Minimum),FloatToStr(LChart.Chart.RightAxis.Maximum)]);

              FAppModules.StudyArea.SetDefaultParams(LDataSet);
              if LDataSet.AreAllParamsBound then
                LDataSet.ExecSQL;
            end;
          end;

          for LIndex := 0 to AViewDataManager.TSCViewDataList.ViewsCount -1 do
          begin
            LView := AViewDataManager.TSCViewDataList.ViewByIndex[LIndex];
            LDataSet.DataSet.Close;
            LDataSet.ClearSQL;
            LDataSet.ClearQueryParams();
            LDataSet.SetSQL('INSERT INTO TSCView'+
            ' (Model,StudyAreaName,SubArea,ViewName,CurrentChartName,DateCreated,HeaderCaption,FooterCaption)'+
            ' VALUES'+
            ' (:AModelCode,:AStudyAreaCode,:ASubAreaCode,:AViewName,:ACurrentChartName,:DateCreated,:AHeaderCaption,:AFooterCaption)');

            LDataSet.SetParams(['AViewName','ACurrentChartName','DateCreated','AHeaderCaption','AFooterCaption'],
                               [LView.ViewName,LView.SavedChartName,DateTimeToStr(LView.DateCreated),LView.HeaderCaption,LView.FooterCaption]);
            FAppModules.StudyArea.SetDefaultParams(LDataSet);
            if LDataSet.AreAllParamsBound then
              LDataSet.ExecSQL;
          end;

            LDataSet.DataSet.Close;
            LDataSet.ClearSQL;
            LDataSet.ClearQueryParams();
            LDataSet.SetSQL('INSERT INTO TSCViewScenario'+
            ' (Model,StudyAreaName,SubArea,Scenario,ViewName)'+
            ' VALUES'+
            ' (:AModelCode,:AStudyAreaCode,:ASubAreaCode,:AScenarioCode,:AViewName)');

            LDataSet.SetParams(['AViewName'],
                               [AScenarioViewData.SenarioViewName]);
            FAppModules.StudyArea.SetDefaultParams(LDataSet);
            if LDataSet.AreAllParamsBound then
              LDataSet.ExecSQL;


        finally
          LDataSet.DataSet.Close;
          FreeAndNil(LDataSet);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.DeleteSubAreaDataFromDatabase: boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.DeleteSubAreaDataFromDatabase';

var
  LDataset      : TAbstractModelDataset;
begin
  Result := False;
  try
     // Added by BSP since we need to exit this point if BSP not to save BSP to DB
      if Assigned(FAppModules) and Assigned(FAppModules.Model()) and
          (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
      begin
        exit;
      end;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      if Assigned(LDataSet) then
      begin
        try
          LDataSet.DataSet.Close;
          LDataSet.SetSQL('DELETE FROM TSCChartSeries WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
            LDataSet.ExecSQL;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('DELETE FROM TSCSeries WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
            LDataSet.ExecSQL;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('DELETE FROM TSCViewChart WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
            LDataSet.ExecSQL;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('DELETE FROM TSCChart WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
            LDataSet.ExecSQL;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('DELETE FROM TSCView WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
            LDataSet.ExecSQL;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL('DELETE FROM TSCViewScenario WHERE Model = :AModelCode AND StudyAreaName = :AStudyAreaCode and SubArea = :ASubAreaCode');
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if LDataSet.AreAllParamsBound then
            LDataSet.ExecSQL;
        finally
          FreeAndNil(LDataSet);
        end;
      end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorPopulatingAgent.PopulateEmptySeriesForCurrentScenario(ASeriesDataManager: TTimeSeriesComparitorSeriesManager;
                                                                                     ACurrentView: TTimeSeriesComparitorView);
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PopulateEmptySeriesForCurrentScenario';
var
  LSeries  : TTimeSeriesComparitorSeries;
  LIndex   : integer;
  LNodeDataIndex: integer;
  LTreeNodeData: TViewDataTreeNodeData;
  LSeriesName: string;
begin
  try
    if Assigned(ASeriesDataManager) then
    begin
      for LIndex := 0 to ASeriesDataManager.SeriesDataList.SeriessCount -1 do
      begin
        LSeries := ASeriesDataManager.SeriesDataList.SeriesByIndex[LIndex];
        if(LSeries.LineSeries.Count > 0) then Continue;
        if(LSeries.ScenarioCode <> FAppModules.StudyArea.ScenarioCode) then Continue;

        LSeriesName := LSeries.CommaTextNameExcScenario;
        LNodeDataIndex := FTreeNodeItems.IndexOf(LSeriesName);
        if(LNodeDataIndex >= 0) then
        begin
          if Assigned(FTreeNodeItems.Objects[LNodeDataIndex]) then
          begin
            LTreeNodeData := TViewDataTreeNodeData(FTreeNodeItems.Objects[LNodeDataIndex]);
            PopulateCurrentSeries(0,LSeries,LTreeNodeData,ACurrentView)
          end;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.GetSeriesUnitsFromName(AName: string): string;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.GetSeriesUnitsFromName';
var
  LPos1,
  LPos2 : integer;
  LCanGetUnits : boolean;
begin
  try
    LCanGetUnits := (Pos('(', AName) > 0) or (Pos(')', AName) > 0);
    if not LCanGetUnits then Exit;

    LPos1  := Pos('(', AName);
    LPos2  := Pos(')', AName);
    Result := Copy(AName, LPos1 + 1,
                   (LPos2 - LPos1) - 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGraphTreeItemDataObject }

constructor TGraphTreeItemDataObject.Create(AAppModules: TAppModules);
const OPNAME = 'TGraphTreeItemDataObject.Create';
begin
  try
    inherited Create(AAppModules);
    FLeftAxisCaption      := '';
    FLeftAxisFormat       := '';
    FLeftAxisUnits        := '';
    FBottomAxisCaption    := '';
    FBottomAxisFormat     := '';
    FBottomAxisUnits      := '';
    FGraphHint            := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGraphTreeItemDataObject.PopulateTreeNodeDataObject(AViewID: string);
const OPNAME = 'TGraphTreeItemDataObject.PopulateTreeNodeDataObject';
var LFieldPropertyX, LFieldPropertyY, LFieldPropertyGraph: TAbstractFieldProperty;
begin
  try
    if Assigned(FAppModules.FieldProperties()) then
    begin
      LFieldPropertyX     := FAppModules.FieldProperties.FieldProperty(AViewID + 'X');
      LFieldPropertyY     := FAppModules.FieldProperties.FieldProperty(AViewID + 'Y');
      LFieldPropertyGraph := FAppModules.FieldProperties.FieldProperty(AViewID + 'G');
      if Assigned(LFieldPropertyGraph) then
      begin
        GraphHint := LFieldPropertyGraph.FieldDescription;
      end;
      if Assigned(LFieldPropertyX) then
      begin
        BottomAxisCaption := LFieldPropertyX.FieldDescription;
        BottomAxisUnits   := LFieldPropertyX.FieldUnits;
        BottomAxisFormat  := LFieldPropertyX.FormatStringGraph;
      end;
      if Assigned(LFieldPropertyY) then
      begin
        LeftAxisCaption := LFieldPropertyY.FieldDescription;
        LeftAxisUnits   := LFieldPropertyY.FieldUnits;
        LeftAxisFormat  := LFieldPropertyY.FormatStringGraph;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorPopulatingAgent.PopulateSeriesFromClientDataset(AType: TChartSeriesType;
          AGraphData: TClientDataSet;
          ASeries: TTimeSeriesComparitorSeries;
          ACurrentView: TTimeSeriesComparitorView;
          ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.PopulateSeriesFromClientDataset';
(*  function FormatSeriesName(ASeriesName : string) : string;
  const OPNAME = 'UTimeSeriesComparitorPopulatingAgent.FormatSeriesName';
  var
    LTmpStr : string;
    LPos : integer;
  begin
    Result := '';
    try
      LTmpStr := ASeriesName;
      LPos    := 21;
      Delete(LTmpStr, LPos, (Length(LTmpStr) - LPos) + 1);
      Result  := LTmpStr;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;
  *)
var
  LLineSeries: TLineSeries;
  //LSeriesName: string;
  LLanguageStr,
  LCaption,
  LUnits : string;
  LXValues: TStringList;
//  LAreas  : String;
 // Lindex2 : Integer;
  LColCounnt  : Integer;
  //LValidAxisCaption : boolean;
  LYear,LIndex: integer;
  LData : TGraphTreeItemDataObject;
  LColor: TColor;

begin
  try

    LXValues := TStringList.Create;
    try
      LData := TGraphTreeItemDataObject(ADataObject.Data);
      if Assigned(LData) then
      begin
        LLanguageStr        := LData.LeftAxisCaption;
        LUnits              := LData.LeftAxisUnits;
        LCaption            := FAppModules.Language.GetString(LLanguageStr);
        ASeries.AxisCaption := LCaption + ' (' + LUnits + ')';
        ASeries.Units := LUnits;
      end;

      while (not AGraphData.Eof) do
      begin
        Lxvalues.Clear;
        GetDelimitedTextFromString(Trim(AGraphData.FieldList.FieldByName('XValues').AsString),',',Lxvalues);
        //LXValues.CommaText          := Trim(AGraphData.FieldList.FieldByName('XValues').AsString);

        LLineSeries := ASeries.LineSeries;
        ASeries.LineSeries.Pointer.Size := 4;
        ASeries.LineSeries.Pointer.Show;
        ASeries.LineSeries.Pointer.Style := psCircle;

        ASeries.LineSeries.LinePen.Visible := True;
        for LIndex := 0 to LXValues.Count - 1 do
        Begin
          LColor := Random(MaxInt);
          LYear := AGraphData.FieldList.FieldByName('Year').AsInteger;
          If IsNumeric(Trim(AGraphData.FieldList.FieldByName(LXValues[LIndex]).AsString)) then
          begin
            LColCounnt :=  LIndex;
            If Lindex  > 12 then
            begin
              LColCounnt := 11;
            end;
            If  LColCounnt < 12 then
            begin
              LLineSeries.addxy(EncodeDate(LYear,1 + LColCounnt, 1) ,AGraphData.FieldList.FieldByName(LXValues[LIndex]).AsFloat,FloatToStr(AGraphData.FieldList.FieldByName(LXValues[LIndex]).AsFloat),LColor);
            end;  
          end
          Else
          LLineSeries.Title := 'Value : '+Trim(AGraphData.FieldList.FieldByName(LXValues[LIndex]).AsString) +' is not numeric';
        end;
        LLineSeries.Title := LowerCase(FormatSeriesName(ASeries.SeriesName));
        AGraphData.Next;
      end;
    finally
      LXValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorPopulatingAgent.IsNumeric(AString: String): Boolean;
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.IsNumeric';
var
  LIndex        : Integer;
  LPeriodCount  : Integer;
begin
  Result := False;
  try
    AString := Trim(AString);
    if AString = '' then
      Exit;
    LPeriodCount := 0;
    for LIndex := 1 to Length(AString) do
    begin
      if not CharInSet(AString[LIndex], ['0'..'9','.']) then
        Exit;
      Inc(LPeriodCount, Ord(AString[LIndex] = '.'));
    end;
    Result := LPeriodCount <= 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorPopulatingAgent.GetDelimitedTextFromString(const AString: string; separator: Char; substrings: TStringList);
const OPNAME = 'TTimeSeriesComparitorPopulatingAgent.GetDelimitedTextFromString';
    function FindDelimetedText(Achar: Char; const AString: string; fromPos: Integer): Integer;
    const OPNAME = 'UTimeSeriesComparitorPopulatingAgent.FindDelimetedText';
    var
      LPostionCounter: Integer;
    begin
    Result := 0;
    for LPostionCounter := fromPos to Length(AString) do
    begin
      if AString[LPostionCounter] = Achar then
      begin
        Result := LPostionCounter;
        Break;
      end;
    end;
    end;
var
  Lindex, LPositionCount: Integer;
begin
  if Assigned(substrings) and (Length(AString) > 0) then
  begin
    Lindex := 1;
    While Lindex <= Length(AString) do
    begin
      LPositionCount := FindDelimetedText(separator, AString, Lindex);
      if LPositionCount = 0 then
        LPositionCount := Length(AString) + 1;
      substrings.Add(Copy(AString, Lindex, LPositionCount - Lindex));
      Lindex := LPositionCount + 1;
    end;
  end;
end;




end.

