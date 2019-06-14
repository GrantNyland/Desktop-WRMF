//
//
//  UNIT      : Contains the class TSystemYieldHistoricDialog.
//  AUTHOR    : Sam Dhlamini(ARIVIA)
//  DATE      : 2005/02/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit USystemYieldHistoricDialog;

interface

uses
  Classes,
  VCLTee.Chart,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCLTee.Series,
  VCLTee.TeEngine,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  THistoricLineSeries = array of TLineSeries;
  THistoricPointSeries = array of TPointSeries;
  TSystemYieldHistoricDialog = class(TAbstractScrollablePanel)
  protected
    FHistoricGrid : TFieldStringGrid;
    FClientPanel : TPanel;
    FHistoricGraph :TFieldChart;
    FHistoricLineSeries :THistoricLineSeries;
    FHistoricPointSeries : THistoricPointSeries;
    FTDElements : integer;
    FVerSplitter : TSplitter;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;
  public
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure PrepareChart;
    procedure ClearChart;
    property HistoricGrid: TFieldStringGrid read FHistoricGrid;
    property TDElements : integer read FTDElements write FTDElements;
    property HistoricGraph :TFieldChart read FHistoricGraph;
    property HistoricLineSeries :THistoricLineSeries read FHistoricLineSeries;
    property HistoricPointSeries : THistoricPointSeries read FHistoricPointSeries;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UConstants,
  UControlCreationUtilities,
  UErrorHandlingOperations, VCL.Grids, Math;

{ TSystemYieldHistoricDialog }

procedure TSystemYieldHistoricDialog.CreateMemberObjects;
const OPNAME = 'TSystemYieldHistoricDialog.CreateMemberObjects';
begin
  inherited;
  try

    FClientPanel := TPanel.Create(ControlsOwner);
    FClientPanel.Parent := ControlsParent;
    FClientPanel.Align := alClient;


    FHistoricGrid                    := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FHistoricGrid.Parent             := FClientPanel;
    FHistoricGrid.RowCount           := 2;
    FHistoricGrid.ColCount           := 3;
    FHistoricGrid.FixedRows          := 1;
    FHistoricGrid.FixedCols          := 0;
    FHistoricGrid.Alignment          := taCenter;
    FHistoricGrid.Options            := FHistoricGrid.Options - [goEditing	];

    FVerSplitter                 := TSplitter.Create(ControlsOwner);
    FVerSplitter.Parent          := FClientPanel;

    FHistoricGraph := TFieldChart.Create(ControlsOwner, FAppModules);
    FHistoricGraph.Parent := FClientPanel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricDialog.DestroyMemberObjects;
const OPNAME = 'TSystemYieldHistoricDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricDialog.Initialise: boolean;
const OPNAME = 'TSystemYieldHistoricDialog.Initialise';
var
  lIndex: integer;
begin
  Result := inherited Initialise;
  try

    for lIndex := 0 to FHistoricGrid.ColCount - 1 do
      FHistoricGrid.ColWidths[lIndex] := FHistoricGrid.ColWidths[lIndex] + 60;


    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSystemYieldHistoricDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FHistoricGrid.Cells[0, 0] := FAppModules.Language.GetString('TField.SumOutTargetDraft');
    FHistoricGrid.Cells[1, 0] := FAppModules.Language.GetString('TField.SumOutDeficitPropotion');
    FHistoricGrid.Cells[2, 0] := FAppModules.Language.GetString('TField.SumOutYield');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricDialog.Resize;
const OPNAME = 'TSystemYieldHistoricDialog.Resize';
begin
  inherited Resize;
  try
    {FHistoricGrid.Left := 0;
    FHistoricGrid.Top := 0;
    }
    FHistoricGrid.Align := alLeft;
    FHistoricGrid.Width := (ClientWidth div 2) -(FHistoricGrid.ColWidths[0]);
    FHistoricGrid.Height := ClientHeight;

    FVerSplitter.Align           := alLeft;
    FVerSplitter.Left            := FHistoricGrid.Width + 1;
    FVerSplitter.Height          := FHistoricGrid.Height;
    FVerSplitter.Beveled         := True;
    FVerSplitter.Width           := 4;
    FVerSplitter.Visible         := True;

    //FHistoricGraph.Left := FHistoricGrid.Width + FVerSplitter.Width;
    FHistoricGraph.Align := alClient;
    FHistoricGraph.Width := FHistoricGrid.Width + (FHistoricGrid.ColWidths[0]+120);
    FHistoricGraph.Height := FHistoricGrid.Height;
    VCL.Controls.TControl(FHistoricGraph).Top := 0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricDialog.PrepareChart;
const OPNAME = 'TSystemYieldHistoricDialog.PrepareChart';
var
  LIndex : integer;
begin
  try

    if FTDElements > 0 then
    begin
      SetLength(FHistoricPointSeries, FTDElements);
      for LIndex := 0 to FTDElements-1 do
      begin
        FHistoricPointSeries[LIndex]                  := TPointSeries.Create(FHistoricGraph);
        FHistoricPointSeries[LIndex].ParentChart      := FHistoricGraph;
        FHistoricPointSeries[LIndex].LinePen.Style    := psSolid ;
        FHistoricPointSeries[LIndex].LinePen.Width    := 1;
        FHistoricPointSeries[LIndex].XValues.Order    := loNone;
        FHistoricPointSeries[LIndex].Pointer.Visible  := False;
        FHistoricPointSeries[LIndex].Visible          := True;
        FHistoricPointSeries[LIndex].Marks.Visible := False;
        FHistoricPointSeries[LIndex].LinePen.Width := 1;
        FHistoricPointSeries[LIndex].Pointer.Size := 1;
        FHistoricPointSeries[LIndex].Clear;

      end;
    end;

    SetLength(FHistoricLineSeries, 3);
    for LIndex := 0 to 2 do
    begin
      FHistoricLineSeries[LIndex]                  := TLineSeries.Create(FHistoricGraph);
      FHistoricLineSeries[LIndex].ParentChart      := FHistoricGraph;
      FHistoricLineSeries[LIndex].LinePen.Style    := psSolid;
      FHistoricLineSeries[LIndex].LinePen.Width    := 1;
      FHistoricLineSeries[LIndex].XValues.Order    := loNone;
      FHistoricLineSeries[LIndex].Visible          := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricDialog.ClearChart;
const OPNAME = 'TSystemYieldHistoricDialog.ClearChart';
var
  LIndex : integer;
begin
  try
    if FTDElements > 0 then
    begin
      for LIndex := 0 to FTDElements - 1 do
      begin
        if (Length(FHistoricPointSeries) > 0)  then
        begin
          FHistoricPointSeries[LIndex].Active := False;
          FHistoricPointSeries[LIndex].Clear;
          FHistoricPointSeries[LIndex].ParentChart := nil;
          FHistoricPointSeries[LIndex]             := nil;
        end;
      end;
    end;

    for LIndex := 0 to 2 do
    begin
      if (Length(FHistoricLineSeries) > 0)  then
      begin
        FHistoricLineSeries[LIndex].Active := False;
        FHistoricLineSeries[LIndex].Clear;
        FHistoricLineSeries[LIndex].ParentChart := nil;
        FHistoricLineSeries[LIndex]             := nil;
      end;
    end;


 except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TSystemYieldHistoricDialog.AssignHelpContext;
const OPNAME = 'TSystemYieldHistoricDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,             HC_DeterminingTheSystemYield);
    SetControlHelpContext(FHistoricGrid,    HC_DeterminingTheSystemYield);
    SetControlHelpContext(VCL.Controls.TControl(FHistoricGraph),   HC_DeterminingTheSystemYield);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
