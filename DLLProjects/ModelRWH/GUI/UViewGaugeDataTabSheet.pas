//
//
//  UNIT      : Contains TViewGaugeDataTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 25/02/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit UViewGaugeDataTabSheet;

interface
uses
  System.Classes,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  VCLTee.Chart,
  VCLTee.GanttCh,
  VCLTee.TeEngine,
  VCLTee.TeeProcs,
  UAbstractObject,
  UAbstractComponent,
  UFrameViewGaugeData,
  URWHDataObject,
  UDataViewerSheet;
type




  TViewGaugeDataTabSheet = class(TDataViewerSheet)
  protected
    FStationData : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    function RWHModelData : TRWHModelData;
    procedure DoGaugeTreeViewMouseDown (Sender : TObject;Button : TMouseButton;Shift  : TShiftState;X, Y   : Integer);
    function AddSeriesToChart (ASeries: TGanttSeries; AStartDate: TDateTime; AEndDate: TDateTime;
                                                  AIndex: Integer;ADescription: string; AColor: TColor): boolean;
    procedure ClearDataViewer; override;

  public
    function StudyDataHasChanged (AContext   : TChangeContext;AFieldName : string; AOldValue  : string;  ANewValue  : string): Boolean; override;
    procedure PopulateTreeView; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure TabHasChanged; override;
  end;


implementation
uses
  System.UITypes,
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;


const
  FirstNodeCaption = 'Selected Stations List';


{ TRWHTabSheet }

procedure TViewGaugeDataTabSheet.CreateMemberObjects;
const OPNAME = 'TViewGaugeDataTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey              := 'RWHViewGaugeData';
    FViewTypeConstant           := 'RWHViewGaugeData';
    frmViewGaugeData            := TfrmViewGaugeData.Create(Self);
    frmViewGaugeData.Parent     := Self;
    frmViewGaugeData.Align       := alClient;
    frmViewGaugeData.AppModules := FAppModules;
    frmViewGaugeData.RecordLengthChart.Title.Text.Text := 'Record Length of Selected Stations';
    FTreeView.OnMouseDown := DoGaugeTreeViewMouseDown;
    FTreeView.MultiSelect := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TViewGaugeDataTabSheet.DestroyMemberObjects;
const OPNAME = 'TViewGaugeDataTabSheet.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewGaugeDataTabSheet.RWHModelData: TRWHModelData;
const OPNAME = 'TViewGaugeDataTabSheet.RWHModelData';
begin
  Result := nil;
  try
    Result :=  TRWHModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TViewGaugeDataTabSheet.DoGaugeTreeViewMouseDown (Sender : TObject;Button : TMouseButton;Shift  : TShiftState;X, Y   : Integer);
const OPNAME = 'TViewGaugeDataTabSheet.OnTreeViewItemSelected';
var
  LSelectedStation : TRainfallStation;
  LValueData,
  LData : TStringList;
  LIndex : integer;
  LStartDate, LEndDate :TDate;
  LCount : integer;
  LHasGaps : boolean;
  LNode : TTreeNode;
begin
  try
    ClearDataViewer;
    LData := TStringList.Create;
    LValueData := TStringList.Create;

    frmViewGaugeData.stgDailyData.RowHeights[0] := 30;


    try
      for LIndex := 0 to FTreeView.SelectionCount-1 do
      begin
        LNode := FTreeView.Selections[LIndex];
        if LNode.Data <> nil then
        begin
          LSelectedStation := RWHModelData.SelectedRainfallStationList.RainfallStationByStationNumber[LNode.Text];
          if LSelectedStation <> nil then
          begin
            case RWHModelData.DailyDataSource of
            ddsWRCPatch : RWHModelData.GetDailyDataList(LData,LSelectedStation.StationNumber,'WRC');
            ddsDwaSwas : RWHModelData.GetDailyDataList(LData,LSelectedStation.StationNumber);
            end;

            LHasGaps := False;
            LStartDate := 0;
            if LIndex>0 then
              frmViewGaugeData.stgDailyData.ColCount := FTreeView.SelectionCount * 2;
            frmViewGaugeData.stgDailyData.ColWidths[(LIndex*2)+1] := 110;
            frmViewGaugeData.stgDailyData.ColWidths[(LIndex*2)] := 110;
            frmViewGaugeData.stgDailyData.Cells[(LIndex*2),0] := LNode.Text + ' Date';
            frmViewGaugeData.stgDailyData.Cells[(LIndex*2)+1,0] := LNode.Text + ' Rainfall';
            for LCount := 0 to LData.Count-1 do
            begin
              LValueData.Clear;
              LValueData.CommaText := LData[LCount];
              if LData.Count> frmViewGaugeData.stgDailyData.RowCount then
                frmViewGaugeData.stgDailyData.RowCount := frmViewGaugeData.stgDailyData.RowCount +1;
              if LValueData.Count>0 then
              begin
                frmViewGaugeData.stgDailyData.Cells[LIndex*2,LCount+1] := LValueData[0];

                if LValueData.Count>1 then
                begin
                  frmViewGaugeData.stgDailyData.Cells[(LIndex*2)+1,LCount+1] := LValueData[1];
                end;
                if (LHasGaps) or (LCount = 0) then
                begin

                  LStartDate := StrToDate(LValueData[0]);
                  LHasGaps := False;
                end;
                if LValueData.Count = 1 then
                begin
                  LHasGaps := True;
                  LEndDate := StrToDate(LValueData[0]);
                  AddSeriesToChart(frmViewGaugeData.ChartSeries,LStartDate,LEndDate,LIndex,LSelectedStation.StationNumber, clYellow);
                end;
                if (LCount = LData.Count-1) then
                begin
                  LEndDate := StrToDate(LValueData[0]);
                  AddSeriesToChart(frmViewGaugeData.ChartSeries,LStartDate,LEndDate,LIndex,LSelectedStation.StationNumber, clYellow);
                end;

              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LData);
      FreeAndNil(LValueData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewGaugeDataTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TViewGaugeDataTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewGaugeDataTabSheet.Initialise: boolean;
const OPNAME = 'TViewGaugeDataTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    Self.Font.Style := Self.Font.Style + [fsBold];
    frmViewGaugeData.Initialise;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TViewGaugeDataTabSheet.ClearDataViewer;
const OPNAME = 'TViewGaugeDataTabSheet.ClearDataViewer';
var
  LRow,
  LCol : integer;
begin
  try

    for LCol := 0 to frmViewGaugeData.stgDailyData.ColCount-1 do
      for LRow := 1 to frmViewGaugeData.stgDailyData.RowCount-1 do
        frmViewGaugeData.stgDailyData.Cells[LCol,LRow] := '';
    frmViewGaugeData.ChartSeries.Clear;
    frmViewGaugeData.stgDailyData.RowCount := 2;
    frmViewGaugeData.stgDailyData.ColCount := 2;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TViewGaugeDataTabSheet.PopulateTreeView;
const OPNAME = 'TViewGaugeDataTabSheet.PopulateTreeView';
var
  LIndex     : integer;
  LNode      : TTreeNode;
  LSelectedStation : TRainfallStation;
begin
  try
    FTreeView.Items.Clear;
    if(RWHModelData.SelectedRainfallStationList.Count > 0) then
    begin
      LNode := FTreeView.Items.Add(Nil,FirstNodeCaption);
      for LIndex := 0 to RWHModelData.SelectedRainfallStationList.Count-1 do
      begin
        LSelectedStation := RWHModelData.SelectedRainfallStationList.RainfallStationByIndex[LIndex];
        FTreeView.Items.AddChildObject(LNode,LSelectedStation.StationNumber,LSelectedStation);
      end;
      LNode.Expand(True);
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewGaugeDataTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TViewGaugeDataTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TViewGaugeDataTabSheet.TabHasChanged;
const OPNAME = 'TViewGaugeDataTabSheet.TabHasChanged';
begin
  inherited TabHasChanged;
  try
    PopulateTreeView;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewGaugeDataTabSheet.StudyDataHasChanged(AContext: TChangeContext;AFieldName, AOldValue, ANewValue: String ): Boolean;
const OPNAME = 'TViewGaugeDataTabSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if AFieldName = 'Gauge' then
    begin
      ClearDataViewer;
      PopulateTreeView;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewGaugeDataTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TViewGaugeDataTabSheet.LanguageHasChanged';
begin
  FTreeView.Items.Clear;
  Result := inherited LanguageHasChanged;
  try
    PopulateTreeView;
    frmViewGaugeData.stgDailyData.Cells[0,0] := 'Date';
    frmViewGaugeData.stgDailyData.Cells[1,0] := 'Rainfall';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TViewGaugeDataTabSheet.AddSeriesToChart (ASeries: TGanttSeries; AStartDate: TDateTime; AEndDate: TDateTime;
                                                  AIndex: Integer;ADescription: string; AColor: TColor): boolean;
const OPNAME = 'TViewGaugeDataTabSheet.AddSeriesToChart';
begin
  Result := False;
  try
    ASeries.AddGanttColor(AStartDate, AEndDate, AIndex, ADescription, AColor);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.
