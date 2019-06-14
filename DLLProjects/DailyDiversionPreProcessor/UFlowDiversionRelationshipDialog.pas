//
//
//  UNIT      : Contains TFlowDiversionRelationshipDialog Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 01/09/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UFlowDiversionRelationshipDialog;


interface
uses
  VCL.Buttons,
  VCL.ExtCtrls,
  VCL.Controls,
  VCL.Forms,
  VCL.ComCtrls,
  Types,
  VCL.Menus,
  VCL.Graphics,
  VCL.Grids,
  Classes,
  Windows,
  VCL.StdCtrls,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;
type

  TFlowDiversionRelationshipDialog = class(TAbstractScrollablePanel)
  protected
    FgbRankedRelationship : TGroupBox;
    FgbUnRankedRelationship : TGroupBox;
    FgbWRYMData : TGroupBox;
    FstrgrdUnRankedRelationship : TFieldStringGrid;
    FstrgrdRankedRelationship : TFieldStringGrid;
    FstrgrdWRYMData : TFieldStringGrid;
    FpgcFlowDiversionRelationship : TAbstractPageControl;
    FGridTab : TTabSheet;
    FDiversionGraphTab : TTabSheet;
    FNonDiversionGraphTab : TTabSheet;
    FFlowDiversionRelationshipChart : TAbstractChart;
    FFlowDiversionRelationshipLineSeries : TLineSeries;
    FFlowDiversionRelationshipPointSeries : TPointSeries;

    FFlowNonDiversionRelationshipChart : TAbstractChart;
    FFlowNonDiversionRelationshipLineSeries : TLineSeries;
    FFlowNonDiversionRelationshipPointSeries : TPointSeries;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: Boolean; override;

    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;

    property strgrdUnRankedRelationship : TFieldStringGrid read FstrgrdUnRankedRelationship;
    property strgrdRankedRelationship : TFieldStringGrid read FstrgrdRankedRelationship;
    property strgrdWRYMData : TFieldStringGrid read FstrgrdWRYMData;
    property pgcFlowDiversionRelationship : TAbstractPageControl read FpgcFlowDiversionRelationship;
    property GridTab : TTabSheet read FGridTab;
    property DiversionGraphTab : TTabSheet read FDiversionGraphTab;
    property NonDiversionGraphTab : TTabSheet read FNonDiversionGraphTab;

    property FlowDiversionRelationshipChart : TAbstractChart read FFlowDiversionRelationshipChart;
    property FlowDiversionRelationshipLineSeries : TLineSeries read FFlowDiversionRelationshipLineSeries;
    property FlowDiversionRelationshipPointSeries : TPointSeries read FFlowDiversionRelationshipPointSeries;

    property FlowNonDiversionRelationshipChart : TAbstractChart read FFlowNonDiversionRelationshipChart;
    property FlowNonDiversionRelationshipLineSeries : TLineSeries read FFlowNonDiversionRelationshipLineSeries;
    property FlowNonDiversionRelationshipPointSeries : TPointSeries read FFlowNonDiversionRelationshipPointSeries;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
  UDatasetType,
  VCL.Printers,
  UConstants,
  UHelpContexts,
  VoaimsCom_TLB,
  VCLTee.TeExport,
  UErrorHandlingOperations;

{ TFlowDiversionRelationshipDialog }

procedure TFlowDiversionRelationshipDialog.CreateMemberObjects;
const OPNAME = 'TFlowDiversionRelationshipDialog.CreateMemberObjects';
begin
  inherited;
  try
    FgbRankedRelationship := TGroupBox.Create(ControlsOwner);
    FgbUnRankedRelationship := TGroupBox.Create(ControlsOwner);
    FgbWRYMData := TGroupBox.Create(ControlsOwner);

    FstrgrdUnRankedRelationship := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdRankedRelationship := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdWRYMData := TFieldStringGrid.Create(ControlsOwner,FAppModules);

    FpgcFlowDiversionRelationship := TAbstractPageControl.Create(ControlsOwner,FAppModules);
    FGridTab := TTabSheet.Create(ControlsOwner);
    FDiversionGraphTab := TTabSheet.Create(ControlsOwner);
    FNonDiversionGraphTab := TTabSheet.Create(ControlsOwner);

    FFlowDiversionRelationshipChart := TAbstractChart.Create(ControlsOwner, FAppModules);
    FFlowDiversionRelationshipLineSeries := TLineSeries.Create(FFlowDiversionRelationshipChart);
    FFlowDiversionRelationshipPointSeries := TPointSeries.Create(FFlowDiversionRelationshipChart);

    FFlowNonDiversionRelationshipChart := TAbstractChart.Create(ControlsOwner, FAppModules);
    FFlowNonDiversionRelationshipLineSeries := TLineSeries.Create(FFlowNonDiversionRelationshipChart);
    FFlowNonDiversionRelationshipPointSeries := TPointSeries.Create(FFlowNonDiversionRelationshipChart);

    FgbRankedRelationship.Parent :=  FGridTab;
    FgbUnRankedRelationship.Parent :=  FGridTab;
    FgbWRYMData.Parent :=  FGridTab;

    FstrgrdUnRankedRelationship.Parent :=  FgbUnRankedRelationship;
    FstrgrdRankedRelationship.Parent :=  FgbRankedRelationship;
    FstrgrdWRYMData.Parent :=  FgbWRYMData;

    FpgcFlowDiversionRelationship.Parent := ControlsParent;
    FGridTab.Parent := FpgcFlowDiversionRelationship;
    FDiversionGraphTab.Parent := FpgcFlowDiversionRelationship;
    FNonDiversionGraphTab.Parent := FpgcFlowDiversionRelationship;

    FFlowDiversionRelationshipChart.Parent := FDiversionGraphTab;
    FFlowNonDiversionRelationshipChart.Parent := FNonDiversionGraphTab;

    FFlowDiversionRelationshipLineSeries.ParentChart := FFlowDiversionRelationshipChart;
    FFlowDiversionRelationshipPointSeries.ParentChart := FFlowDiversionRelationshipChart;

    FFlowNonDiversionRelationshipLineSeries.ParentChart := FFlowNonDiversionRelationshipChart;
    FFlowNonDiversionRelationshipPointSeries.ParentChart := FFlowNonDiversionRelationshipChart;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFlowDiversionRelationshipDialog.DestroyMemberObjects;
const OPNAME = 'TFlowDiversionRelationshipDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFlowDiversionRelationshipDialog.Initialise: boolean;
const OPNAME = 'TFlowDiversionRelationshipDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FpgcFlowDiversionRelationship.Align := alClient;
    FpgcFlowDiversionRelationship.TabPosition := tpBottom;

    FFlowDiversionRelationshipChart.Align                         := alClient;
    FFlowDiversionRelationshipChart.BevelOuter                    := bvNone;
    FFlowDiversionRelationshipChart.Legend.Visible                := False;
    FFlowDiversionRelationshipChart.AxisVisible                   := True;
    FFlowDiversionRelationshipChart.AllowZoom                     := True;
    FFlowDiversionRelationshipChart.AllowPanning                  := pmBoth;
    FFlowDiversionRelationshipChart.Gradient.Visible              := False;
    FFlowDiversionRelationshipChart.View3D                        := False;
    FFlowDiversionRelationshipChart.Title.Visible                 := True;
    FFlowDiversionRelationshipChart.Title.Font.Style              := [fsBold];
    FFlowDiversionRelationshipChart.Title.Font.Color              := clBlack;
    FFlowDiversionRelationshipChart.LeftAxis.Title.Angle          := 90;
    FFlowDiversionRelationshipChart.LeftAxis.Title.Caption        := FAppModules.Language.GetString('ChartLeftAxis.DiversionFlow');
    FFlowDiversionRelationshipChart.LeftAxis.LabelsSize           := 30;
    FFlowDiversionRelationshipChart.BottomAxis.LabelsAngle        := 0;
    FFlowDiversionRelationshipChart.BottomAxis.Title.Caption      := FAppModules.Language.GetString('ChartBottomAxis.ReferenceFlow');
    FFlowDiversionRelationshipChart.BottomAxis.LabelsSize         := 20;
    FFlowDiversionRelationshipChart.BottomAxis.Automatic          := True;
    FFlowDiversionRelationshipChart.BottomAxis.TickLength         := 6;
    FFlowDiversionRelationshipChart.ScaleLastPage                 := False;
    FFlowDiversionRelationshipChart.BottomAxis.MinorTickCount     := 4;

    FFlowDiversionRelationshipLineSeries.Marks.Visible := False;
    FFlowDiversionRelationshipLineSeries.LinePen.Width := 2;
    FFlowDiversionRelationshipLineSeries.Clear;

    FFlowDiversionRelationshipPointSeries.Marks.Visible := False;
    FFlowDiversionRelationshipPointSeries.LinePen.Width := 1;
    FFlowDiversionRelationshipPointSeries.Pointer.Size := 1;
    FFlowDiversionRelationshipPointSeries.Clear;


    FstrgrdUnRankedRelationship.ColCount         := 4;
    FstrgrdUnRankedRelationship.RowCount         := 2;
    FstrgrdUnRankedRelationship.FixedCols        := 1;
    FstrgrdUnRankedRelationship.FixedRows        := 1;
    FstrgrdUnRankedRelationship.WrapHeaderText   := True;


    FstrgrdRankedRelationship.ColCount         := 4;
    FstrgrdRankedRelationship.RowCount         := 2;
    FstrgrdRankedRelationship.FixedCols        := 1;
    FstrgrdRankedRelationship.FixedRows        := 1;
    FstrgrdRankedRelationship.WrapHeaderText   := True;

    FstrgrdWRYMData.ColCount         := 4;
    FstrgrdWRYMData.RowCount         := 2;
    FstrgrdWRYMData.FixedCols        := 1;
    FstrgrdWRYMData.FixedRows        := 1;
    FstrgrdWRYMData.WrapHeaderText   := True;

    FGridTab.PageControl := FpgcFlowDiversionRelationship;
    FDiversionGraphTab.PageControl := FpgcFlowDiversionRelationship;
    FNonDiversionGraphTab.PageControl := FpgcFlowDiversionRelationship;

    FGridTab.Align := alClient;
    FDiversionGraphTab.Align   := alClient;
    FNonDiversionGraphTab.Align   := alClient;

    FFlowNonDiversionRelationshipChart.Align                         := alClient;
    FFlowNonDiversionRelationshipChart.BevelOuter                    := bvNone;
    FFlowNonDiversionRelationshipChart.Legend.Visible                := False;
    FFlowNonDiversionRelationshipChart.AxisVisible                   := True;
    FFlowNonDiversionRelationshipChart.AllowZoom                     := True;
    FFlowNonDiversionRelationshipChart.AllowPanning                  := pmBoth;
    FFlowNonDiversionRelationshipChart.Gradient.Visible              := False;
    FFlowNonDiversionRelationshipChart.View3D                        := False;
    FFlowNonDiversionRelationshipChart.Title.Visible                 := True;
    FFlowNonDiversionRelationshipChart.Title.Font.Style              := [fsBold];
    FFlowNonDiversionRelationshipChart.Title.Font.Color              := clBlack;
    FFlowNonDiversionRelationshipChart.LeftAxis.Title.Angle          := 90;
    FFlowNonDiversionRelationshipChart.LeftAxis.Title.Caption        := FAppModules.Language.GetString('ChartLeftAxis.NonDiversionFlow');
    FFlowNonDiversionRelationshipChart.LeftAxis.LabelsSize           := 30;
    FFlowNonDiversionRelationshipChart.BottomAxis.LabelsAngle        := 0;
    FFlowNonDiversionRelationshipChart.BottomAxis.Title.Caption      := FAppModules.Language.GetString('ChartBottomAxis.ReferenceFlow');
    FFlowNonDiversionRelationshipChart.BottomAxis.LabelsSize         := 20;
    FFlowNonDiversionRelationshipChart.BottomAxis.Automatic          := True;
    FFlowNonDiversionRelationshipChart.BottomAxis.TickLength         := 6;
    FFlowNonDiversionRelationshipChart.ScaleLastPage                 := False;
    FFlowNonDiversionRelationshipChart.BottomAxis.MinorTickCount     := 4;

    FFlowNonDiversionRelationshipLineSeries.Marks.Visible := False;
    FFlowNonDiversionRelationshipLineSeries.LinePen.Width := 2;
    FFlowNonDiversionRelationshipLineSeries.Clear;

    FFlowNonDiversionRelationshipPointSeries.Marks.Visible := False;
    FFlowNonDiversionRelationshipPointSeries.LinePen.Width := 1;
    FFlowNonDiversionRelationshipPointSeries.Pointer.Size := 1;
    FFlowNonDiversionRelationshipPointSeries.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFlowDiversionRelationshipDialog.AssignHelpContext;
const OPNAME = 'TFlowDiversionRelationshipDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(Self                        , HC_ReferenceFlowVsDiversionFlowRelationship);
    SetControlHelpContext(FstrgrdUnRankedRelationship , HC_ReferenceFlowVsDiversionFlowRelationship);
    SetControlHelpContext(FstrgrdRankedRelationship   , HC_ReferenceFlowVsDiversionFlowRelationship);
    SetControlHelpContext(FstrgrdWRYMData             , HC_ReferenceFlowVsDiversionFlowRelationship);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFlowDiversionRelationshipDialog.Resize;
const OPNAME = 'TFlowDiversionRelationshipDialog.Resize';
begin
  inherited Resize;
  try
    FgbUnRankedRelationship.Height := Self.ClientHeight - (C_ControlOffset*6);
    FstrgrdUnRankedRelationship.Top := C_ControlOffset;
    FstrgrdUnRankedRelationship.Left := C_ControlBorder;
    FstrgrdUnRankedRelationship.DefaultRowHeight := 15;
    //FstrgrdUnRankedRelationship.DefaultColWidth  := 85;
    FstrgrdUnRankedRelationship.Width := (FstrgrdUnRankedRelationship.DefaultColWidth * FstrgrdUnRankedRelationship.ColCount)+50;
    FstrgrdUnRankedRelationship.Height := FgbUnRankedRelationship.Height - (C_ControlOffset*2);
    FstrgrdUnRankedRelationship.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];
    FstrgrdUnRankedRelationship.RowHeights[0] := 30;

    FgbUnRankedRelationship.Top := C_GroupBoxOffset;
    FgbUnRankedRelationship.Left := C_GroupBoxOffset;
    FgbUnRankedRelationship.Width := Self.ClientWidth div 3;

    FgbRankedRelationship.Height := Self.ClientHeight - (C_ControlOffset*6);
    FstrgrdRankedRelationship.Top := C_ControlOffset;
    FstrgrdRankedRelationship.Left := C_ControlBorder;
    FstrgrdRankedRelationship.DefaultRowHeight := 15;
    //FstrgrdRankedRelationship.DefaultColWidth  := 85;
    FstrgrdRankedRelationship.Width := (FstrgrdUnRankedRelationship.DefaultColWidth * FstrgrdUnRankedRelationship.ColCount)+50;
    FstrgrdRankedRelationship.Height := FgbRankedRelationship.Height - (C_ControlOffset*2);
    FstrgrdRankedRelationship.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];
    FstrgrdRankedRelationship.RowHeights[0] := 30;

    FgbRankedRelationship.Top := FgbUnRankedRelationship.Top;
    FgbRankedRelationship.Left := FgbUnRankedRelationship.Width + C_GroupBoxOffset;
    FgbRankedRelationship.Width := FgbUnRankedRelationship.Width;

    FgbWRYMData.Height := Self.ClientHeight - (C_ControlOffset*6);
    FstrgrdWRYMData.Top := C_ControlOffset;
    FstrgrdWRYMData.Left := C_ControlBorder;
    FstrgrdWRYMData.DefaultRowHeight := 15;
    //FstrgrdWRYMData.DefaultColWidth  := 85;
    FstrgrdWRYMData.Width := (FstrgrdUnRankedRelationship.DefaultColWidth * FstrgrdUnRankedRelationship.ColCount)+50;
    FstrgrdWRYMData.Height := FgbWRYMData.Height - (C_ControlOffset*2);
    FstrgrdWRYMData.RowHeights[0] := 30;
    FstrgrdWRYMData.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];

    FgbWRYMData.Top := FgbRankedRelationship.Top;
    FgbWRYMData.Left := FgbUnRankedRelationship.Width + FgbRankedRelationship.Width + C_GroupBoxOffset;
    FgbWRYMData.Width :=FgbRankedRelationship.Width;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationshipDialog.LanguageHasChanged: Boolean;
const OPNAME = 'TFlowDiversionRelationshipDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FGridTab.Caption := FAppModules.Language.GetString('TabCaption.Grid');
    FDiversionGraphTab.Caption := FAppModules.Language.GetString('TabCaption.DiversionGraph');
    FNonDiversionGraphTab.Caption := FAppModules.Language.GetString('TabCaption.NonDiversionGraph');

    FstrgrdUnRankedRelationship.cells[0,0] := FAppModules.Language.GetString('GridHeading.Date');
    FstrgrdUnRankedRelationship.cells[1,0] := FAppModules.Language.GetString('GridHeading.ReferenceFlow');
    FstrgrdUnRankedRelationship.cells[2,0] := FAppModules.Language.GetString('GridHeading.DiversionFlow');
    FstrgrdUnRankedRelationship.cells[3,0] := FAppModules.Language.GetString('GridHeading.NonDiversionFlow');

    FstrgrdRankedRelationship.cells[0,0] := FAppModules.Language.GetString('GridHeading.Rank');
    FstrgrdRankedRelationship.cells[1,0] := FAppModules.Language.GetString('GridHeading.ReferenceFlow');
    FstrgrdRankedRelationship.cells[2,0] := FAppModules.Language.GetString('GridHeading.DiversionFlow');
    FstrgrdRankedRelationship.cells[3,0] := FAppModules.Language.GetString('GridHeading.NonDiversionFlow');

    FstrgrdWRYMData.cells[0,0] := FAppModules.Language.GetString('GridHeading.Rank');
    FstrgrdWRYMData.cells[1,0] := FAppModules.Language.GetString('GridHeading.ReferenceFlow');
    FstrgrdWRYMData.cells[2,0] := FAppModules.Language.GetString('GridHeading.DiversionFlow');
    FstrgrdWRYMData.cells[3,0] := FAppModules.Language.GetString('GridHeading.NonDiversionFlow');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationshipDialog.CanExport: boolean;
const OPNAME = 'TFlowDiversionRelationshipDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FFlowDiversionRelationshipChart) and FFlowDiversionRelationshipChart.Visible and
               Assigned(FFlowDiversionRelationshipLineSeries) and FFlowDiversionRelationshipLineSeries.Visible or
               Assigned(FFlowDiversionRelationshipPointSeries) and FFlowDiversionRelationshipPointSeries.Visible)
               or
               (Assigned(FFlowNonDiversionRelationshipChart) and FFlowNonDiversionRelationshipChart.Visible and
               Assigned(FFlowNonDiversionRelationshipLineSeries) and FFlowNonDiversionRelationshipLineSeries.Visible or
               Assigned(FFlowNonDiversionRelationshipPointSeries) and FFlowNonDiversionRelationshipPointSeries.Visible);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowDiversionRelationshipDialog.CanPrint: boolean;
const OPNAME = 'TFlowDiversionRelationshipDialog.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FpgcFlowDiversionRelationship) and FpgcFlowDiversionRelationship.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipDialog.DoExport(AFileName: string);
const OPNAME = 'TFlowDiversionRelationshipDialog.DoExport';
begin
  try
    case FpgcFlowDiversionRelationship.ActivePageIndex of
      1: TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FFlowDiversionRelationshipChart));
      2: TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FFlowNonDiversionRelationshipChart));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipDialog.DoPrint;
const OPNAME = 'TFlowDiversionRelationshipDialog.DoPrint';
begin
  try
    case FpgcFlowDiversionRelationship.ActivePageIndex of
      1: FFlowDiversionRelationshipChart.DoPrint;
      2: FFlowNonDiversionRelationshipChart.DoPrint
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
