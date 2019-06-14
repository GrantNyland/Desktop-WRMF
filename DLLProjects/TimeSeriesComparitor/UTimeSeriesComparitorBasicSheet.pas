//
//
//  UNIT      : Contains TTimeSeriesComparitorBasicSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/01
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTimeSeriesComparitorBasicSheet;

interface

uses
  Chart,
  ComCtrls,
  UDataSetType,
  UViewDataItem,
  UViewDataList,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UDataViewerSheet;

type

  TTimeSeriesComparitorBasicSheet = class(TDataViewerSheet)
  protected

    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    function GetToolBar: TAbstractToolBar; override;

    // Overriden from TDataViewerSheet.
    procedure ClearDataViewer; override;
    procedure PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData); override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;

  public
  end;

implementation

uses
  SysUtils,
  Controls,
  Graphics,
  Windows,
  Clipbrd,
  Printers,
  //UTimeSeriesComparitorChart,
  UVaalDBMSMenuEventType,
  UErrorHandlingOperations;

procedure TTimeSeriesComparitorBasicSheet.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorBasicSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorBasicSheet.AssignHelpContext;
const OPNAME = 'TTimeSeriesComparitorBasicSheet.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorBasicSheet.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorBasicSheet.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorBasicSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TTimeSeriesComparitorBasicSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := FToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorBasicSheet.PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TTimeSeriesComparitorBasicSheet.PopulateTreeNodeDataObject';
end;

procedure TTimeSeriesComparitorBasicSheet.ClearDataViewer;
const OPNAME = 'TTimeSeriesComparitorBasicSheet.ClearDataViewer';
begin
  try
    if Assigned(FChart) then
    begin
      FChart.Parent := nil;
      FreeAndNil(FChart);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorBasicSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TTimeSeriesComparitorBasicSheet.PopulateDataViewer';
var
  LLanguageStr: string;
  LUnits: string;
  LCaption: string;
  LData: TGraphTreeItemDataObject;
  LDataset      : TAbstractModelDataset;
  LDataSetIndex : integer;
begin
  try
    inherited PopulateDataViewer(ADataObject);
    if Assigned(ADataObject) and
       Assigned(ADataObject.ViewDataNode) and
       (ADataObject.ViewDataNode.ViewDataSetCount > 0) and
       (ADataObject.ViewDataNode.ShowSQL) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      if Assigned(LDataSet) then
      begin
        for LDataSetIndex := 0 to ADataObject.ViewDataNode.ViewDataSetCount - 1 do
        begin
          LDataSet.SetSQL(ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ViewSQL);
          LDataSet.DataSet.Close;
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if (ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ParamCount > 0) then
            LDataSet.SetParams(
              ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ParamNames,
              ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ParamValues);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            try
              if (LDataSet.DataSet.RecordCount > 0) then
              begin
                if Assigned(FChart) then
                begin
                  FChart.Parent := nil;
                  FreeAndNil(FChart);
                end;
                FChart := TTimeSeriesComparitorChart.Create(nil, FAppModules);
                FChart.Parent := self;
                FChart.Align := alClient;

                // Get the data object.
                LData := TGraphTreeItemDataObject(ADataObject.Data);

                // Set chart text and Construct units string.
                FChart.Title.Text.Text := FTreeView.Selected.Text;

                LCaption     := '';
                LLanguageStr := LData.FLeftAxisCaption;
                LUnits       := LData.FLeftAxisUnits;
                if(Trim(LLanguageStr) <> '') then
                  LCaption     := FAppModules.Language.GetString(LLanguageStr);
                if(Trim(LUnits) <> '') then
                   LCaption := LCaption + ' (' + LUnits +')';
                FChart.LeftAxis.Title.Caption := LCaption;

                LCaption     := '';
                LLanguageStr := LData.FBottomAxisCaption;
                LUnits       := LData.FBottomAxisUnits;
                if(Trim(LLanguageStr) <> '') then
                  LCaption     := FAppModules.Language.GetString(LLanguageStr);
                if(Trim(LUnits) <> '') then
                   LCaption := LCaption + ' (' + LUnits +')';
                FChart.BottomAxis.Title.Caption := LCaption;
                FChart.LeftAxis.AxisValuesFormat   := LData.FLeftAxisFormat;
                FChart.BottomAxis.AxisValuesFormat := LData.FBottomAxisFormat;

                FChart.ShowHint := False;
                LLanguageStr := LData.GraphHint;
                if (LLanguageStr <> '') then
                begin
                  FChart.ShowHint := True;
                  FChart.Hint := FAppModules.Language.GetString(LLanguageStr);
                end;

                // Create the series.
                TTimeSeriesComparitorChart(FChart).CreateSeries(
                  TChartSeriesType(ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].SQLType), LDataset.DataSet);
              end;
            finally
              LDataSet.DataSet.Close;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

