unit URainfallCatchmentSummaryDialog;

interface
uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  VCL.Grids,

  UDataComponent,
  UAbstractObject,
  UAbstractComponent,
  URainfallGaugeSelectionMenuItemManager,
  UShellExecuteObject;
type
  {TRainfallCatchmentSummaryDialog}

  TRainfallCatchmentSummaryDialog = class(TAbstractScrollablePanel)
  private
    FHorSplitter: TSplitter;

  protected
    FPanelLeft          : TPanel;
    FPanelAClient       : TPanel;
    FPanelTop           : TPanel;
    FPanelMiddle        : TPanel;
    FPanelBottom        : TPanel;
    FSplitter           : TSplitter;

    FCatchmentListbox   : TListBox;
    FAvrgRainfallGrd    : TStringGrid;
    FRainfallInput      : TStringGrid;
    FCatchment          : TStringGrid;

    FLblRainfallDetails : TLabel;
    FLblRainfallInput   : TLabel;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;

  public
    function Initialise: Boolean; override;
    property HorSplitter     : TSplitter     read FHorSplitter;
    property PanelTopGrid    : TPanel        read FPanelTop;
    property PanelMiddleGrid : TPanel        read FPanelMiddle;
    property PanelBottomGrid : TPanel        read FPanelBottom;
    property PanelClient     : TPanel        read FPanelAClient;
    property CatchmentListbox : TListBox read FCatchmentListbox;
    property AvrgRainfallGrd    : TStringGrid read FAvrgRainfallGrd;
    property RainfallInput      : TStringGrid read FRainfallInput;
    property Catchment          : TStringGrid read FCatchment;
  end;

implementation

uses
  SysUtils,
  VCL.Controls,
  VCL.Graphics,
  VCL.Clipbrd,
  VCL.Printers,
  UHelpContexts,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

function TRainfallCatchmentSummaryDialog.Initialise: Boolean;
const OPNAME = 'TRainfallCatchmentSummaryDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FAvrgRainfallGrd.ColCount := 6;
    FAvrgRainfallGrd.RowCount := 2;

    FAvrgRainfallGrd.Cells[0,0] := FAppModules.Language.GetString('Rainfall.CatchmentSummarySection');
    FAvrgRainfallGrd.Cells[1,0] := FAppModules.Language.GetString('Rainfall.CatchmentSummaryPosition');
    FAvrgRainfallGrd.Cells[2,0] := FAppModules.Language.GetString('Rainfall.CatchmentSummaryMAP');
    FAvrgRainfallGrd.Cells[3,0] := FAppModules.Language.GetString('Rainfall.CatchmentSummaryPeriod');
    FAvrgRainfallGrd.Cells[4,0] := FAppModules.Language.GetString('Rainfall.CatchmentSummaryLatitude');
    FAvrgRainfallGrd.Cells[5,0] := FAppModules.Language.GetString('Rainfall.CatchmentSummaryLongitude');

    FAvrgRainfallGrd.DefaultRowHeight := 18;
    FAvrgRainfallGrd.DefaultColWidth  := 150;

    FRainfallInput.ColCount           := 15;
    FRainfallInput.RowCount           := 2;
    FRainfallInput.DefaultRowHeight   := 18;

    FRainfallInput.Cells[ 0, 0] := 'Year';
    FRainfallInput.Cells[ 1, 0] := 'No. Gauges';
    FRainfallInput.Cells[ 2, 0] := 'Oct';
    FRainfallInput.Cells[ 3, 0] := 'Nov';
    FRainfallInput.Cells[ 4, 0] := 'Dec';
    FRainfallInput.Cells[ 5, 0] := 'Jan';
    FRainfallInput.Cells[ 6, 0] := 'Feb';
    FRainfallInput.Cells[ 7, 0] := 'Mar';
    FRainfallInput.Cells[ 8, 0] := 'Apr';
    FRainfallInput.Cells[ 9, 0] := 'May';
    FRainfallInput.Cells[10, 0] := 'Jun';
    FRainfallInput.Cells[11, 0] := 'Jul';
    FRainfallInput.Cells[12, 0] := 'Aug';
    FRainfallInput.Cells[13, 0] := 'Sep';

    FCatchment.ColCount      := 13;
    FCatchment.RowCount      := 2;
    FCatchment.DefaultRowHeight := 18;

    FCatchment.Cells[ 0, 0] := 'Year';
    FCatchment.Cells[ 1, 0] := 'Oct';
    FCatchment.Cells[ 2, 0] := 'Nov';
    FCatchment.Cells[ 3, 0] := 'Dec';
    FCatchment.Cells[ 4, 0] := 'Jan';
    FCatchment.Cells[ 5, 0] := 'Feb';
    FCatchment.Cells[ 6, 0] := 'Mar';
    FCatchment.Cells[ 7, 0] := 'Apr';
    FCatchment.Cells[ 8, 0] := 'May';
    FCatchment.Cells[ 9, 0] := 'Jun';
    FCatchment.Cells[10, 0] := 'Jul';
    FCatchment.Cells[11, 0] := 'Aug';
    FCatchment.Cells[12, 0] := 'Sep';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryDialog.CreateMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanelLeft                        := TPanel.Create(Self);
    FPanelLeft.Parent                 := self;
    FPanelLeft.Width                  := 175;
    FPanelLeft.Height                 := 600;
    FPanelLeft.Align                  := alLeft;
    FPanelLeft.BorderStyle            := bsNone;
    FPanelLeft.BevelInner             := bvNone;
    FPanelLeft.BevelOuter             := bvNone;

    FCatchmentListbox                 := TListBox.Create(Self);
    FCatchmentListbox.Parent          := FPanelLeft;
    FCatchmentListbox.Align           := alClient;

    FPanelAClient                     := TPanel.Create(Self);
    FPanelAClient.Parent              := self;
    FPanelAClient.Align               := alClient;
    FPanelAClient.BorderStyle         := bsNone;
    FPanelAClient.BevelInner          := bvNone;
    FPanelAClient.BevelOuter          := bvNone;

    FPanelTop                         := TPanel.Create(Self);
    FPanelTop.Parent                  := FPanelAClient;
    FPanelTop.Align                   := alTop;
    FPanelTop.BorderStyle             := bsNone;
    FPanelTop.BevelInner              := bvNone;
    FPanelTop.BevelOuter              := bvNone;
    FPanelTop.Height                  := 185;

    FPanelMiddle                      := TPanel.Create(Self);
    FPanelMiddle.Parent               := FPanelAClient;
    FPanelMiddle.Align                := alClient;
    FPanelMiddle.BorderStyle          := bsNone;
    FPanelMiddle.BevelInner           := bvNone;
    FPanelMiddle.BevelOuter           := bvNone;

    FPanelBottom                      := TPanel.Create(Self);
    FPanelBottom.Parent               := FPanelAClient;
    FPanelBottom.Align                := alBottom;
    FPanelBottom.BorderStyle          := bsNone;
    FPanelBottom.BevelInner           := bvNone;
    FPanelBottom.BevelOuter           := bvNone;
    FPanelBottom.Height               := 185;

    FSplitter                         := TSplitter.Create(nil);
    FSplitter.Parent                  := Self;
    FSplitter.Width                   := 4;
    FSplitter.Left                    := FPanelLeft.Left + FPanelLeft.Width + 1;
    FSplitter.Beveled                 := TRUE;

    FAvrgRainfallGrd                  := TStringGrid.Create(Self);
    FAvrgRainfallGrd.Parent           := FPanelTop;
    FAvrgRainfallGrd.FixedCols        := 0 ;
    FAvrgRainfallGrd.Top              := 0;
    FAvrgRainfallGrd.Align            := alClient;
    FAvrgRainfallGrd.Font.Size        := 10;
    FAvrgRainfallGrd.Font.Name        := 'Courier New';
    FAvrgRainfallGrd.Options          := FAvrgRainfallGrd.Options - [goEditing];
    FAvrgRainfallGrd.ScrollBars       := ssBoth;

    FRainfallInput                    := TStringGrid.Create(Self);
    FRainfallInput.Parent             := FPanelMiddle;
    FRainfallInput.FixedCols          := 0;
    FRainfallInput.Top                := 0;
    FRainfallInput.Align              := alClient;
    FRainfallInput.Font.Size          := 10;
    FRainfallInput.Font.Name          := 'Courier New';
    FRainfallInput.Options            := FRainfallInput.Options - [goEditing];
    FRainfallInput.ScrollBars         := ssBoth;

    FCatchment                        := TStringGrid.Create(Self);
    FCatchment.Parent                 := FPanelBottom;
    FCatchment.FixedCols              := 0;
    FCatchment.Top                    := 0;
    FCatchment.Align                  := alClient;
    FCatchment.Font.Size              := 10;
    FCatchment.Font.Name              := 'Courier New';
    FCatchment.Options                := FCatchment.Options - [goEditing];
    FCatchment.ScrollBars             := ssBoth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryDialog.Resize;
const OPNAME = 'TRainfallCatchmentSummaryDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryDialog.AssignHelpContext;
const OPNAME = 'TRainfallCatchmentSummaryDialog.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
