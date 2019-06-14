//
//
//  UNIT      : Contains class TStringGridWithCellChange.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/09/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStringGridWithCellChange;

interface

uses
  Classes,
  Windows,
  Vcl.Controls,
  Vcl.Grids,
  UAbstractObject,
  UAbstractComponent;

type
  TCellChangeEvent = procedure (ASender: TObject; ACol, ARow: integer) of object;
  TColumnResizeChangeEvent = procedure (ASender: TObject; ACol: integer) of object;
  TStringGridWithCellChange = class(TAbstractStringGrid)
  protected
    FCurCol,
    FCurRow : integer;
    FDataHasChanged,
    FDblClickColAutoSize,
    FAutoSizeFixedCols,
    FColAutoSizeIgnoreHeading: boolean;

    FCurrentCellStartContents: string;
    FBeforeCellChange: TCellChangeEvent;
    FAfterCellChange: TCellChangeEvent;
    FColumnResizeChangeEvent:TColumnResizeChangeEvent;

    // Overriden from TStringGrid.
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheelDown(AShift: TShiftState; AMousePos: TPoint): boolean; override;
    function DoMouseWheelUp(AShift: TShiftState; AMousePos: TPoint): boolean; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

    // Introduced in this class.
    function DoBeforeCellChange: boolean; virtual;
    procedure DoAfterCellChange; virtual;
    procedure SetDblClickColAutoSize(AValue: boolean);
  public

    // Constructor.
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); override;

    // Introduced in this class.
    procedure ResetStartValue; virtual;
    procedure StoreStartValue; virtual;
    procedure CancelEdits; virtual;
    procedure DoubleClickCurrentColumn; virtual;

    // Properties.
    property OnBeforeCellChange: TCellChangeEvent read FBeforeCellChange write FBeforeCellChange;
    property OnAfterCellChange: TCellChangeEvent read FAfterCellChange write FAfterCellChange;
    property OnColumnResize: TColumnResizeChangeEvent read FColumnResizeChangeEvent write FColumnResizeChangeEvent;

    property DblClickColAutoSize: boolean read FDblClickColAutoSize write SetDblClickColAutoSize;
    property ColAutoSizeIgnoreHeading: boolean read FColAutoSizeIgnoreHeading write FColAutoSizeIgnoreHeading;
    property AutoSizeFixedCols: boolean read FAutoSizeFixedCols write FAutoSizeFixedCols;

    property CurCol: integer read FCurCol write FCurCol;
    property CurRow: integer read FCurRow write FCurRow;

  end;

implementation

uses
  Math,
  Vcl.Graphics,
  SysUtils,
  UErrorHandlingOperations;

const
  C_NotUsedYetFlag = #10;

constructor TStringGridWithCellChange.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TStringGridWithCellChange.Create';
begin
  try
    inherited Create(AOwner, AAppModules);
    Self.Options := Self.Options + [goTabs];
    FCurrentCellStartContents := C_NotUsedYetFlag;
    FBeforeCellChange := nil;
    FAfterCellChange := nil;
    FColumnResizeChangeEvent := nil;
    FColAutoSizeIgnoreHeading := False;
    FAutoSizeFixedCols := False;
    FDataHasChanged := False;
    FCurCol  := -1;
    FCurRow  := -1;
    SetDblClickColAutoSize(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringGridWithCellChange.DoBeforeCellChange: boolean;
const OPNAME = 'TStringGridWithCellChange.DoBeforeCellChange';
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

procedure TStringGridWithCellChange.DoAfterCellChange;
const OPNAME = 'TStringGridWithCellChange.DoAfterCellChange';
begin
  try
    StoreStartValue;
    if Assigned(FAfterCellChange) then
      FAfterCellChange(Self, Col, Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.ResetStartValue;
const OPNAME = 'TStringGridWithCellChange.ResetStartValue';
begin
  try
    FCurrentCellStartContents := C_NotUsedYetFlag;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.StoreStartValue;
const OPNAME = 'TStringGridWithCellChange.StoreStartValue';
begin
  try
    FCurrentCellStartContents := Cells[Col, Row];
    FDataHasChanged := False;
    FCurCol := Col;
    FCurRow := Row;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.CancelEdits;
const OPNAME = 'TStringGridWithCellChange.CancelEdits';
begin
  try
    if (FCurrentCellStartContents <> C_NotUsedYetFlag) then
      Cells[Col, Row] := FCurrentCellStartContents;
    DoAfterCellChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.KeyDown(var AKey: Word; AShift: TShiftState);
const OPNAME = 'TStringGridWithCellChange.KeyDown';
var LOldCol, LOldRow: integer;
begin
  try

    // Call the event handler if the cell could possibly change.
    if (AKey in [VK_TAB,VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_HOME, VK_END]) then
      DoBeforeCellChange;

    // Esc key : Reset the contents of the cell.
    if (AKey = VK_ESCAPE) then
      if (FCurrentCellStartContents <> C_NotUsedYetFlag) then
        CancelEdits;

    // Call the ancestor.
    LOldCol := Col;
    LOldRow := Row;
    inherited KeyDown(AKey, AShift);

    // Check if a new cell is active.
    if (Col <> LOldCol) or (Row <> LOldRow) then
      DoAfterCellChange;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TStringGridWithCellChange.MouseDown';
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
    if (Col <> LOldCol) or (Row <> LOldRow) then
      DoAfterCellChange;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringGridWithCellChange.DoMouseWheelDown(AShift: TShiftState; AMousePos: TPoint): boolean;
const OPNAME = 'TStringGridWithCellChange.DoMouseWheelDown';
var LOldCol, LOldRow: integer;
begin
  Result := True;
  try
    DoBeforeCellChange;
    LOldCol := Col;
    LOldRow := Row;
    Result := inherited DoMouseWheelDown(AShift, AMousePos);
    if (Col <> LOldCol) or (Row <> LOldRow) then
      DoAfterCellChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringGridWithCellChange.DoMouseWheelUp(AShift: TShiftState; AMousePos: TPoint): boolean;
const OPNAME = 'TStringGridWithCellChange.DoMouseWheelUp';
var LOldCol, LOldRow: integer;
begin
  Result := True;
  try
    DoBeforeCellChange;
    LOldCol := Col;
    LOldRow := Row;
    Result := inherited DoMouseWheelUp(AShift, AMousePos);
    if (Col <> LOldCol) or (Row <> LOldRow) then
      DoAfterCellChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.DoubleClickCurrentColumn;
const OPNAME = 'TStringGridWithCellChange.DoEnter';
begin
  try
    DblClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.DoEnter;
const OPNAME = 'TStringGridWithCellChange.DoEnter';
begin
  try
    DoAfterCellChange;
    inherited DoEnter;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.DoExit;
const OPNAME = 'TStringGridWithCellChange.DoExit';
begin
  try
    DoBeforeCellChange;
    inherited DoExit;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TStringGridWithCellChange.MouseUp';
begin
  try
    inherited;
    Self.MouseToCell(X, Y,FCurCol,FCurRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.DblClick;
const OPNAME = 'TStringGridWithCellChange.DblClick';
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

procedure TStringGridWithCellChange.SetDblClickColAutoSize(AValue: boolean);
const OPNAME = 'TStringGridWithCellChange.SetDblClickColAutoSize';
begin
  try
    FDblClickColAutoSize := AValue;
    if AValue then
      Self.Options := Self.Options + [goColSizing]
    else
      Self.Options := Self.Options - [goColSizing];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringGridWithCellChange.SetEditText(ACol, ARow: Integer;
  const Value: string);
const OPNAME = 'TStringGridWithCellChange.SetEditText';
begin
  try
    inherited;
    FDataHasChanged := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
