
unit UOutputDemandChannelsGraphSummaryDialog;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  Contnrs,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCLTee.Series,
  VCLTee.TeEngine,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UStringGridWithCellChange,
  ULongtermSupplyData,
  UDataComponent;

type

  TLongtermGraph = class(TAbstractChart)
  protected
    FChannelNumber : integer;
    FRILineSeriesList    : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
    procedure ClearRecurrenceIntervals;
  public
    function AddLineSeries(ASeries : string; AObject : TObject): boolean;
    procedure ConfigureRILineSeriers(ARISeries: TLineSeries);
    function RILineSeriesListCount : integer;
    property ChannelNumber : integer read FChannelNumber write FChannelNumber;

end;

  TOutputDemandChannelsGraphSummaryDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel        : TPanel;
    FLeftPanel : TPanel;

    FSpliter : TSplitter;
    FViewDataLabel        : TLabel;
    FViewDataType         : TFieldComboBox;
    FlsvDemandChannels : TFieldCheckListBox;

    FLongtermGraphs : TObjectList;
    FNumberOfGraphs : integer;
    FNoOfCol :integer;
    FScrollBox : TScrollBox;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    function GetLongtermGraphsByIndex(Index: integer): TLongtermGraph;
    function GetLongtermGraphsByChannelNo(AChannelNo: integer): TLongtermGraph;
    procedure OnScrollBoxMouseWheelDown(Sender: TObject;  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ShowControl(AControl: TControl); override;
  public
    function AddGraph(AChannelNo: integer) : TLongtermGraph;
    function DeleteGraph(ALongtermGraph: TLongtermGraph) : boolean;
    procedure ClearGraphs;
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property lsvDemandChannels : TFieldCheckListBox read FlsvDemandChannels;
    property ViewDataLabel : TLabel read FViewDataLabel;
    property ViewDataType : TFieldComboBox read FViewDataType;
    property NumberOfGraphs : integer read FNumberOfGraphs write FNumberOfGraphs;
    property NoOfCol :integer read FNoOfCol write FNoOfCol;
    property LongtermGraphsByIndex[Index : integer] : TLongtermGraph read GetLongtermGraphsByIndex;
    property LongtermGraphsByChannelNo[AChannelNo : integer] : TLongtermGraph read GetLongtermGraphsByChannelNo;

  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  VCLTee.TeExport,
  VCLTee.TeeProcs,
  UHelpContexts,
  UControlCreationUtilities,
  UYieldModelDataObject,
  UErrorHandlingOperations, VCL.Grids, VCLTee.Chart;

{ TOutputDemandChannelsGraphSummaryDialog }

procedure TOutputDemandChannelsGraphSummaryDialog.CreateMemberObjects;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try


    FSelectorPanel                := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent         := ControlsParent;

    FViewDataLabel                := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent         := FSelectorPanel;
    FViewDataLabel.Alignment      := taCenter;
    FViewDataLabel.AutoSize       := False;

    FViewDataType                 := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent          := FSelectorPanel;

    FLeftPanel := TPanel.Create(ControlsOwner);
    FLeftPanel.Parent := ControlsParent;

    FScrollBox                    := TScrollBox.Create(ControlsOwner);
    FScrollBox.Parent             := ControlsParent;

    FScrollBox.Align  := alClient;
    FScrollBox.AutoScroll := True;

    FlsvDemandChannels := TFieldCheckListBox.Create(ControlsOwner,FAppModules);
    FlsvDemandChannels.Parent := FLeftPanel;

    FSpliter := TSplitter.Create(ControlsOwner);
    FSpliter.Parent := ControlsParent;
    FLongtermGraphs := TObjectList.Create;

    FScrollBox.OnMouseWheelDown := OnScrollBoxMouseWheelDown;
    FScrollBox.OnMouseWheelUp   := OnScrollBoxMouseWheelUp;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.OnScrollBoxMouseWheelDown(Sender: TObject;  Shift: TShiftState; MousePos: TPoint;
          var Handled: Boolean);
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.OnScrollBoxMouseWheelDown';
begin
  try
    if FScrollBox.VertScrollBar.IsScrollBarVisible and
      (FScrollBox.VertScrollBar.Position < FScrollBox.VertScrollBar.Range) then
    begin
      FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position + FScrollBox.VertScrollBar.Increment;
      Handled := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.OnScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
          var Handled: Boolean);
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.OnScrollBoxMouseWheelUp';
begin
  try
    if FScrollBox.VertScrollBar.IsScrollBarVisible and
      (FScrollBox.VertScrollBar.Position > 0) then
    begin
      FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position - FScrollBox.VertScrollBar.Increment;
      Handled := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;



procedure TOutputDemandChannelsGraphSummaryDialog.DestroyMemberObjects;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.DestroyMemberObjects';
begin
  try
    FreeAndNil(FLongtermGraphs);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryDialog.Initialise: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FSelectorPanel.BorderStyle := bsSingle;
    FSpliter.AutoSnap := True;
    FNoOfCol := 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGraphSummaryDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FViewDataLabel.Caption    := FAppModules.Language.GetString('LabelText.ViewData');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.AssignHelpContext;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.Resize;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.Resize';
var
  LIndex : integer;
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align := alTop;
      FSelectorPanel.ClientHeight := 30;
      FViewDataType.Align := alLeft;
      FViewDataType.Width := 160;
      FViewDataLabel.Align := alLeft;
      FViewDataLabel.Width := 60;
      FViewDataLabel.Layout := tlCenter;
      FLeftPanel.Align := alLeft;
      FLeftPanel.Width := 150;
      FSpliter.Left := FLeftPanel.Width+5;
      FScrollBox.Left := FSpliter.Left+5;
      FScrollBox.Align := alClient;
      FlsvDemandChannels.Align := alClient;
      for LIndex := 0 to FLongtermGraphs.Count-1 do
      begin
        if TLongtermGraph(FLongtermGraphs.Items[LIndex])<> nil then
        begin
          if (FNoOfCol>0) then
          begin
            if (LIndex = 0) then
            begin
              TLongtermGraph(FLongtermGraphs.Items[LIndex]).Left := 0;
              TLongtermGraph(FLongtermGraphs.Items[LIndex]).Top := 0;
              if FLongtermGraphs.Count <= FNoOfCol then
              begin
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Width := ClientWidth;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Height := ClientHeight ;
              end
              else
              begin
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Width := (ClientWidth div FNoOfCol)-25;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Height := (ClientHeight div FNoOfCol);
              end;

            end
            else
            begin
              if ((LIndex mod (FNoOfCol)) = 0) then
              begin
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Top := TLongtermGraph(FLongtermGraphs.Items[LIndex-1]).Top + TLongtermGraph(FLongtermGraphs.Items[LIndex-1]).Height+15;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Left := 0;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Width := (ClientWidth div FNoOfCol)-25;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Height := (ClientHeight div FNoOfCol);
              end
              else
              begin
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Top := TLongtermGraph(FLongtermGraphs.Items[LIndex-1]).Top;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Left :=TLongtermGraph(FLongtermGraphs.Items[LIndex-1]).Width+15;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Width := (ClientWidth div FNoOfCol)-25;
                TLongtermGraph(FLongtermGraphs.Items[LIndex]).Height := (ClientHeight div FNoOfCol);
                if (FLongtermGraphs.Count = FNoOfCol) and (LIndex = 1)then
                begin
                  TLongtermGraph(FLongtermGraphs.Items[LIndex-1]).Height := (ClientHeight div FNoOfCol);
                  TLongtermGraph(FLongtermGraphs.Items[LIndex]).Width := ClientWidth;
                  TLongtermGraph(FLongtermGraphs.Items[LIndex-1]).Width := ClientWidth;
                  TLongtermGraph(FLongtermGraphs.Items[LIndex]).Top := TLongtermGraph(FLongtermGraphs.Items[LIndex-1]).Height+15;
                  TLongtermGraph(FLongtermGraphs.Items[LIndex]).Left := 0;
                end
              end;
            end;
          end;
        end;
      end;
    finally
      LockWindowUpdate(0);
    end;
    inherited Resize;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.DoExport';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to FLongtermGraphs.Count-1 do
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(TLongtermGraph(FLongtermGraphs.Items[LIndex])));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.DoPrint;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.DoPrint';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to FLongtermGraphs.Count-1 do
      TLongtermGraph(FLongtermGraphs.Items[LIndex]).DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDemandChannelsGraphSummaryDialog.CanExport: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.CanExport';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := 0 to FLongtermGraphs.Count-1 do
      Result := Assigned(TLongtermGraph(FLongtermGraphs.Items[LIndex])) and
                (TLongtermGraph(FLongtermGraphs.Items[LIndex]).Visible);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDemandChannelsGraphSummaryDialog.CanPrint: boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.CanPrint';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := 0 to FLongtermGraphs.Count-1 do
      Result := Assigned(TLongtermGraph(FLongtermGraphs.Items[LIndex])) and
                (TLongtermGraph(FLongtermGraphs.Items[LIndex]).Visible);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDemandChannelsGraphSummaryDialog.GetLongtermGraphsByIndex(Index: integer): TLongtermGraph;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.GetLongtermGraphsByIndex';
begin
  Result := nil;
  try
    if (Index>=0) and (Index<FLongtermGraphs.Count) then
      Result := TLongtermGraph(FLongtermGraphs.Items[Index]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.ClearGraphs;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.ClearGraphs';
var
  LLongtermGraph : TLongtermGraph;
begin
  try
    while (FLongtermGraphs.Count > 0) do
    begin
      LLongtermGraph := TLongtermGraph(FLongtermGraphs.Items[0]);
      if LLongtermGraph <> nil then
      begin
        LLongtermGraph.Parent := nil;
        FLongtermGraphs.Delete(0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDemandChannelsGraphSummaryDialog.AddGraph(AChannelNo: integer) : TLongtermGraph;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.AddGraph';
begin
  Result := nil;
  try
    Result := TLongtermGraph.Create(ControlsOwner,FAppModules);
    Result.FChannelNumber := AChannelNo;
    Result.Parent := FScrollBox;
    FLongtermGraphs.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TOutputDemandChannelsGraphSummaryDialog.DeleteGraph(ALongtermGraph: TLongtermGraph) : boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.DeleteGraph';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := 0 to FLongtermGraphs.Count-1 do
    begin
      if TLongtermGraph(FLongtermGraphs.Items[LIndex]).ChannelNumber = ALongtermGraph.ChannelNumber then
      begin
        TLongtermGraph(FLongtermGraphs.Items[LIndex]).Parent := nil;
        FLongtermGraphs.Delete(LIndex);
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDemandChannelsGraphSummaryDialog.GetLongtermGraphsByChannelNo(AChannelNo: integer): TLongtermGraph;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.GetLongtermGraphsByChannelNo';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FLongtermGraphs.Count-1 do
    begin
      if TLongtermGraph(FLongtermGraphs.Items[LIndex]).FChannelNumber = AChannelNo then
      begin
        Result := TLongtermGraph(FLongtermGraphs.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGraphSummaryDialog.ShowControl(AControl: TControl);
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.ShowControl';
begin
  inherited;
 try
    FAppModules.MainForm.MainForm.ActiveControl := FScrollBox;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TLongtermGraph }

function TLongtermGraph.AddLineSeries(ASeries: string;AObject: TObject): boolean;
const OPNAME = 'TOutputDemandChannelsGraphSummaryDialog.AddGraph';
begin
  Result := False;
  try
    FRILineSeriesList.AddObject(ASeries, AObject);
    TLineSeries(AObject).OnGetMarkText    := OnGetLineSeriesMarkText;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLongtermGraph.CreateMemberObjects;
const OPNAME = 'TLongtermGraph.CreateMemberObjects';
begin
  inherited;
  try
    FRILineSeriesList := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongtermGraph.DestroyMemberObjects;
const OPNAME = 'TLongtermGraph.DestroyMemberObjects';
begin
  try
    FreeAndNil(FRILineSeriesList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongtermGraph.ConfigureRILineSeriers(ARISeries: TLineSeries);
const OPNAME = 'TLongtermGraph.ConfigureRILineSeriers';
begin
  try
    if Assigned(ARISeries) then
    begin
      ARISeries.XValues.Order    := loNone;
      ARISeries.Pointer.Visible  := False;
      ARISeries.Marks.Visible    := False;
      ARISeries.Marks.Clip       := False;
      ARISeries.ShowInLegend     := False;
      ARISeries.SeriesColor      := clBlue;
      ARISeries.LinePen.Width    := 1;
      //ARISeries.OnGetMarkText    := OnGetLineSeriesMarkText;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TLongtermGraph.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TLongtermGraph.OnGetLineSeriesMarkText';
var
  LYearsMarkFormatStr: string;
  LCount,
  LIndex: integer;
  LSeries: TChartSeries;
  LZoomPerc,
  LYValue : Double;
  LXPos,
  LYPos: Longint;
  //LLongtermGraph : TLongtermGraph;
begin
  try
    MarkText := '';
    if (ValueIndex <> 1) then
      Exit;

    if not Assigned(Sender) then
      Exit;

//    LLongtermGraph := LongtermGraphsByChannelNo[Identifier];
    LIndex := -1;
    for LCount := 0 to FRILineSeriesList.Count - 1 do
    begin
     LSeries := TChartSeries(FRILineSeriesList.Objects[LCount]);
     if(LSeries = Sender) then
     begin
       LIndex := LCount;
       Break;
     end;
    end;
    LYearsMarkFormatStr := '';
    if (LIndex >= 0) and
       (LIndex < TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FSplits.Count) then
    begin
      LYearsMarkFormatStr := FAppModules.Language.GetString('OutputReview.YearsFormatStr');
      LYearsMarkFormatStr := Format(LYearsMarkFormatStr,[1, StrToInt(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FSplits[LIndex])]);
    end;

    if (LYearsMarkFormatStr <> '') then
    begin
      LYValue  := Sender.YValues.MaxValue - Sender.YValues.MinValue;
      LYValue  := LYValue/3.0;
      LZoomPerc := Sender.ParentChart.BottomAxis.Maximum - Sender.ParentChart.BottomAxis.Minimum;
      LZoomPerc := LZoomPerc/100;
      LXPos   := Sender.CalcXPosValue(Sender.XValue[ValueIndex] - LZoomPerc);
      LYPos   := Sender.CalcYPosValue(LYValue);
      Self.BottomAxis.DrawAxisLabel(LXPos,LYPos,90,LYearsMarkFormatStr);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongtermGraph.RILineSeriesListCount: integer;
const OPNAME = 'TLongtermGraph.RILineSeriesListCount';
begin
  Result := 0;
  try
    Result := FRILineSeriesList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongtermGraph.ClearRecurrenceIntervals;
const OPNAME = 'TLongtermGraph.ClearRecurrenceIntervals';
var
  LLineSeries : TLineSeries;
begin
  try
    while (FRILineSeriesList.Count > 0) do
    begin
      LLineSeries := TLineSeries(FRILineSeriesList.Objects[0]);
      LLineSeries := TLineSeries(FRILineSeriesList.Objects[0]);
      LLineSeries.Clear;
      LLineSeries.Active := False;
      LLineSeries.ParentChart := nil;
      FRILineSeriesList.Delete(0);
      FreeAndNil(LLineSeries);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
