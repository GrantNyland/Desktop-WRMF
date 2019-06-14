//
//
//  UNIT      : Contains class TStringGridWithCellChange.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/09/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UCellChangeStringGrid;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  VCL.Grids;

type
  TCellNotifyEvent = procedure (ASender: TObject; ACol, ARow: integer) of object;
  TColumnResizeChangeEvent = procedure (ASender: TObject; ACol: integer) of object;
  TCellChangeStringGrid = class(TStringGrid)
  protected
    FCurCol                   : Integer;
    FCurRow                   : Integer;
    FDblClickColAutoSize      : Boolean;
    FAutoSizeFixedCols        : Boolean;
    FColAutoSizeIgnoreHeading : boolean;
    FAlignment                : TAlignment;
    FCurrentCellStartContents : String;
    FBeforeCellChange         : TCellNotifyEvent;
    FAfterCellChange          : TCellNotifyEvent;
    FOnDataCellExit           : TCellNotifyEvent;
    FColumnResizeChangeEvent  : TColumnResizeChangeEvent;

    // Overriden from TStringGrid.
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheelDown(AShift: TShiftState; AMousePos: TPoint): boolean; override;
    function DoMouseWheelUp(AShift: TShiftState; AMousePos: TPoint): boolean; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
//    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;

    // Introduced in this class.
    function DoBeforeCellChange: boolean; virtual;
    procedure DoAfterCellChange (AOldCol : Integer; AOldRow : Integer); virtual;
    procedure SetDblClickColAutoSize(AValue: boolean);
    procedure SetAlignment(Value: TAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    procedure StoreStartValue; virtual;
    procedure CancelEdits; virtual;
    property OnColumnResize: TColumnResizeChangeEvent read FColumnResizeChangeEvent write FColumnResizeChangeEvent;
  published
    property DblClickColAutoSize      : boolean read FDblClickColAutoSize write SetDblClickColAutoSize;
    property ColAutoSizeIgnoreHeading : boolean read FColAutoSizeIgnoreHeading write FColAutoSizeIgnoreHeading;
    property AutoSizeFixedCols        : boolean read FAutoSizeFixedCols write FAutoSizeFixedCols;
    property Alignment                : TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property OnBeforeCellChange : TCellNotifyEvent read FBeforeCellChange write FBeforeCellChange;
    property OnAfterCellChange  : TCellNotifyEvent read FAfterCellChange  write FAfterCellChange;
    property OnDataCellExit     : TCellNotifyEvent read FOnDataCellExit   write FOnDataCellExit;
  end;

implementation

uses
  Math,
  VCL.Graphics,
  SysUtils,
  UErrorHandlingOperations;

const
  C_NotUsedYetFlag = #10;

constructor TCellChangeStringGrid.Create(AOwner: TComponent);
const OPNAME = 'TCellChangeStringGrid.Create';
begin
  try
    inherited Create(AOwner);
    Self.Options := Self.Options + [goTabs];
    FCurrentCellStartContents := C_NotUsedYetFlag;
    FBeforeCellChange := nil;
    FAfterCellChange := nil;
    FColumnResizeChangeEvent := nil;
    FColAutoSizeIgnoreHeading := False;
    FAutoSizeFixedCols := False;
    FAlignment := taLeftJustify;
    FCurCol  := -1;
    FCurRow  := -1;
    SetDblClickColAutoSize(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCellChangeStringGrid.DoBeforeCellChange: boolean;
const OPNAME = 'TCellChangeStringGrid.DoBeforeCellChange';
begin
  Result := False;
  try
    if (FCurrentCellStartContents <> C_NotUsedYetFlag) then
    begin
      if (FCurrentCellStartContents <> Cells[Col, Row]) then
      begin
        Result := True;
        if Assigned(FBeforeCellChange) then
          FBeforeCellChange(Self, Col, Row);
        StoreStartValue;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.DoAfterCellChange (AOldCol : Integer; AOldRow : Integer);
const OPNAME = 'TCellChangeStringGrid.DoAfterCellChange';
begin
  try
    StoreStartValue;
    if Assigned(FAfterCellChange) then
      FAfterCellChange(Self, Col, Row);
    if Assigned(FOnDataCellExit) then
      FOnDataCellExit(self, AOldCol, AOldRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.StoreStartValue;
const OPNAME = 'TCellChangeStringGrid.StoreStartValue';
begin
  try
    FCurrentCellStartContents := Cells[Col, Row];
    FCurCol := Col;
    FCurRow := Row;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.CancelEdits;
const OPNAME = 'TCellChangeStringGrid.CancelEdits';
begin
  try
    if (FCurrentCellStartContents <> C_NotUsedYetFlag) then
      Cells[Col, Row] := FCurrentCellStartContents;
    StoreStartValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.KeyDown(var AKey: Word; AShift: TShiftState);
const OPNAME = 'TCellChangeStringGrid.KeyDown';
var LOldCol, LOldRow: integer;
begin
  try
    LOldCol := Col;
    LOldRow := Row;
    // Call the event handler if the cell could possibly change.
    if (AKey in [VK_TAB,VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_HOME, VK_END]) then
      DoBeforeCellChange;
    // Esc key : Reset the contents of the cell.
    if (AKey = VK_ESCAPE) then
      if (FCurrentCellStartContents <> C_NotUsedYetFlag) then
        CancelEdits;
    // Call the ancestor.
    inherited KeyDown(AKey, AShift);
    // Check if a new cell is active.
    if ((Col <> LOldCol) OR (Row <> LOldRow)) then
      DoAfterCellChange(LOldCol, LOldRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TCellChangeStringGrid.MouseDown';
var LOldCol, LOldRow: integer;
begin
  try
    // Call the event handler because the cell could change.
    if (Button = mbLeft) then
      DoBeforeCellChange;
    // Call the ancestor.
    LOldCol := Col;
    LOldRow := Row;
    inherited MouseDown(Button, Shift, X, Y);
    // Check if a new cell is active.
    if ((Col <> LOldCol) OR (Row <> LOldRow)) then
      DoAfterCellChange(LOldCol, LOldRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCellChangeStringGrid.DoMouseWheelDown(AShift: TShiftState; AMousePos: TPoint): boolean;
const OPNAME = 'TCellChangeStringGrid.DoMouseWheelDown';
var LOldCol, LOldRow: integer;
begin
  Result := True;
  try
    DoBeforeCellChange;
    LOldCol := Col;
    LOldRow := Row;
    Result := inherited DoMouseWheelDown(AShift, AMousePos);
    if ((Col <> LOldCol) OR (Row <> LOldRow)) then
      DoAfterCellChange(LOldCol, LOldRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCellChangeStringGrid.DoMouseWheelUp(AShift: TShiftState; AMousePos: TPoint): boolean;
const OPNAME = 'TCellChangeStringGrid.DoMouseWheelUp';
var LOldCol, LOldRow: integer;
begin
  Result := True;
  try
    DoBeforeCellChange;
    LOldCol := Col;
    LOldRow := Row;
    Result := inherited DoMouseWheelUp(AShift, AMousePos);
    if ((Col <> LOldCol) OR (Row <> LOldRow)) then
      DoAfterCellChange(LOldCol, LOldRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.DoEnter;
const OPNAME = 'TCellChangeStringGrid.DoEnter';
begin
  try
    StoreStartValue;
    inherited DoEnter;
    SelectCell(Col,Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.DoExit;
const OPNAME = 'TCellChangeStringGrid.DoExit';
begin
  try
    DoBeforeCellChange;
    inherited DoExit;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TCellChangeStringGrid.MouseUp';
begin
  try
    inherited;
    Self.MouseToCell(X, Y,FCurCol,FCurRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.DblClick;
const OPNAME = 'TCellChangeStringGrid.DblClick';
var
  LStartRow,
  LCount,
  LMaxWidth: integer;
begin
  inherited;
  try
    if FDblClickColAutoSize  and (goColSizing in Self.Options) then
    begin
      if(FCurRow = 0) and (Self.FixedRows > 0)  then
      begin
        if (FAutoSizeFixedCols) or ((not FAutoSizeFixedCols) and (FCurCol > (Self.FixedCols -1))) then
        begin
          LMaxWidth := 0;
          LStartRow := 0;
          if FColAutoSizeIgnoreHeading then
            LStartRow := 1;

          for LCount := LStartRow to Self.RowCount -1 do
            LMaxWidth := Max(LMaxWidth,Self.Canvas.TextWidth(Self.Cells[FCurCol,LCount]));
          LMaxWidth := LMaxWidth + 4;
          Self.ColWidths[FCurCol] := LMaxWidth;
          if Assigned(FColumnResizeChangeEvent) then
            FColumnResizeChangeEvent(Self,FCurCol);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.SetDblClickColAutoSize(AValue: boolean);
const OPNAME = 'TCellChangeStringGrid.SetDblClickColAutoSize';
begin
  try
    FDblClickColAutoSize := AValue;
    if AValue then
      Self.Options := Self.Options + [goColSizing]
    else
      Self.Options := Self.Options - [goColSizing];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCellChangeStringGrid.SetAlignment(Value: TAlignment);
const OPNAME = 'TCellChangeStringGrid.SetAlignment';
begin
  try
    if FAlignment <> Value then
    begin
      FAlignment := Value;
      Invalidate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
procedure TCellChangeStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TCellChangeStringGrid.DrawCell';
  procedure WriteText (StringGrid : TStringGrid; ACanvas: TCanvas; const ARect: TRect; const Text: string; AAlignment: TAlignment);
  const OPNAME = 'WriteText';
        DX = 2;
        DY = 2;
  var
    S: array[0..255] of Char;
  begin
    with Stringgrid, ACanvas, ARect do
    begin
      case AAlignment of
        taLeftJustify: ExtTextOut(Handle, Left + DX, Top + DY,
            ETO_OPAQUE or ETO_CLIPPED, @ARect, StrPCopy(S, Text), Length(Text), nil);
        taRightJustify: ExtTextOut(Handle, Right - TextWidth(Text) - 3, Top + DY,
            ETO_OPAQUE or ETO_CLIPPED, @ARect, StrPCopy(S, Text),
            Length(Text), nil);
        taCenter: ExtTextOut(Handle, Left + (Right - Left - TextWidth(Text)) div 2,
            Top + DY, ETO_OPAQUE or ETO_CLIPPED, @ARect,
            StrPCopy(S, Text), Length(Text), nil);
      end;
    end;
  end;
begin
  inherited;
  try
    if FAlignment <> taLeftJustify then
    begin
      WriteText(Self,Self.Canvas,ARect,Self.Cells[ACol, ARow],FAlignment);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)
end.
