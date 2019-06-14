//
//
//  UNIT      : Contains TRainTabSheetManager Class
//  AUTHOR    : Sam Dhlamini(arivia.kom)
//  DATE      : 06/02/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallCommonGaugeSheet;

interface                             

uses

  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  VCL.Grids,
  VCL.Controls,
  Math,
  VCL.Menus,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent;

type
  TRainfallCommonGaugeSheet = class (TAbstractScrollablePanel)
  protected
    FPanelLeft     : TPanel;
    FPanelClient   : TPanel;
    FSplitter      : TSplitter;
    FPanelGrid     : TPanel;
    FPanelTreeView : TPanel;
    FGrdZone       : TAbstractStringGrid;
    FTvwGauges     : TAbstractTreeView;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    property GaugeTreeview : TAbstractTreeView   read FTvwGauges;
    property GaugeGrid     : TAbstractStringGrid read FGrdZone;
    property PanelGrid     : TPanel              read FPanelGrid;
    property PanelClient   : TPanel              read FPanelClient;
    property PanelTreeView : TPanel              read FPanelTreeView;
    property PanelLeft     : TPanel              read FPanelLeft;
    property Splitter      : TSplitter           read FSplitter;
  end;

implementation

uses
  VCL.ImgList,
  VCL.Graphics,
  SysUtils,
  UErrorHandlingOperations;

{ TRainfallModelTabSheet }

procedure TRainfallCommonGaugeSheet.CreateMemberObjects;
const OPNAME = 'TRainfallCommonGaugeSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanelLeft             := TPanel.Create(Self);
    FPanelLeft.Parent      := self;
    FPanelLeft.Width       := 175;
    FPanelLeft.Align       := alLeft;
    FPanelLeft.BorderStyle := bsNone;
    FPanelLeft.BevelInner  := bvNone;
    FPanelLeft.BevelOuter  := bvNone;

    FPanelClient             := TPanel.Create(Self);
    FPanelClient.Parent      := self;
    FPanelClient.Align       := alClient;
    FPanelClient.BorderStyle := bsNone;
    FPanelClient.BevelInner  := bvNone;
    FPanelClient.BevelOuter  := bvNone;

    FSplitter         := TSplitter.Create(nil);
    FSplitter.Parent  := Self;
    FSplitter.Width   := 4;
    FSplitter.Left    := FPanelLeft.Left + FPanelLeft.Width + 1;
    FSplitter.Beveled := TRUE;

    FPanelTreeView             := TPanel.Create(Self);
    FPanelTreeView.Parent      := FPanelLeft;
    FPanelTreeView.Align       := alClient;
    FPanelTreeView.BorderStyle := bsNone;
    FPanelTreeView.BevelInner  := bvNone;
    FPanelTreeView.BevelOuter  := bvNone;

    FPanelGrid             := TPanel.Create(Self);
    FPanelGrid.Parent      := FPanelClient;
    FPanelGrid.Align       := alClient;
    FPanelGrid.BorderStyle := bsNone;
    FPanelGrid.BevelInner  := bvNone;
    FPanelGrid.BevelOuter  := bvNone;

    FGrdZone                  := TAbstractStringGrid.Create(FPanelGrid, FAppModules);
    FGrdZone.Parent           := FPanelGrid;
    FGrdZone.Align            := alClient;
    FGrdZone.DefaultColWidth  := 40;
    FGrdZone.ColCount         := 14;
    FGrdZone.Font.Name        := FAppModules.Language.GetString('GridZone.GridZoneFont');
    FGrdZone.Font.Size        := 10;
    FGrdZone.RowCount         := 6;
    FGrdZone.FixedRows        := 1;
    FGrdZone.FixedCols        := 0;
    FGrdZone.DefaultRowHeight := 18;
    FGrdZone.Options          := FGrdZone.Options - [goEditing] + [goColSizing] + [goRowSelect] - [goRangeSelect];

    FTvwGauges          := TAbstractTreeView.Create(FPanelTreeView, FAppModules);
    FTvwGauges.Parent   := FPanelTreeView;
    FTvwGauges.Align    := alClient;
    FTvwGauges.ReadOnly := True;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallCommonGaugeSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallCommonGaugeSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    SaveState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCommonGaugeSheet.Initialise: boolean;
const OPNAME = 'TRainfallCommonGaugeSheet.Initialise';
var
  LGridZoneType : String;
begin
  Result := inherited Initialise;
  try
    LGridZoneType := FAppModules.Language.GetString('GridZone.GridZoneType');
    FGrdZone.ColWidths[0] := 80;
    FGrdZone.ColWidths[1] := 140;
    FGrdZone.ColWidths[2] := 60;
    FGrdZone.ColWidths[3] := 60;
    FGrdZone.ColWidths[4] := 50;
    FGrdZone.ColWidths[5] := 45;
    FGrdZone.ColWidths[6] := 45;
    FGrdZone.ColWidths[7] := 60;
    FGrdZone.ColWidths[8] := 85;
    FGrdZone.ColWidths[9] := 40;
    FGrdZone.ColWidths[10]:= 130;
    FGrdZone.ColWidths[11]:= 55;
    FGrdZone.ColWidths[12]:= 55;
    FGrdZone.ColWidths[13]:= 75;
    FGrdZone.Cells[0,0] := FAppModules.Language.GetString('GridZone.GridZoneNumber');
    FGrdZone.Cells[1,0] := FAppModules.Language.GetString('GridZone.GridZoneName');
    FGrdZone.Cells[2,0] := LGridZoneType;
    FGrdZone.Cells[3,0] := FAppModules.Language.GetString('GridZone.GridZoneLat');
    FGrdZone.Cells[4,0] := FAppModules.Language.GetString('GridZone.GridZoneLong');
    FGrdZone.Cells[5,0] := FAppModules.Language.GetString('GridZone.GridZoneStart');
    FGrdZone.Cells[6,0] := FAppModules.Language.GetString('GridZone.GridZoneEnd');
    FGrdZone.Cells[7,0] := FAppModules.Language.GetString('GridZone.GridZoneMAP');
    FGrdZone.Cells[8,0] := FAppModules.Language.GetString('GridZone.GridZoneStdDev');
    FGrdZone.Cells[9,0] := FAppModules.Language.GetString('GridZone.GridZoneCV');
    FGrdZone.Cells[10,0] := FAppModules.Language.GetString('GridZone.GridZoneMissingMonths');
    FGrdZone.Cells[11,0] := FAppModules.Language.GetString('GridZone.GridZoneLength');
    FGrdZone.Cells[12,0] := FAppModules.Language.GetString('GridZone.GridZoneHeight');
    FGrdZone.Cells[13,0] := LGridZoneType;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.


