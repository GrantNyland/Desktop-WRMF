//
//
//  UNIT      : Contains TGridEditorColumnGrid Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/06/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorColumnGrid;

interface

uses
  Types,
  vcl.Grids,
  UGridEditorStringGrid;

type
  TGridEditorColumnGrid = class(TGridEditorStringGrid)
  protected
    procedure CreateMemberObjects; override;
    procedure SetGridFixedDimension; override;
    procedure RemoveEmptyGridRecords; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
  end;

implementation

uses
  System.UITypes,
  vcl.Graphics,
  SysUtils,
  UErrorHandlingOperations;

procedure TGridEditorColumnGrid.CreateMemberObjects;
const OPNAME = 'TGridEditorColumnGrid.CreateMemberObjects';
begin
  try
    ColWidths[0] := 150;
    RowHeights[0] := 10;
    inherited CreateMemberObjects;
    FOrientationFlag := 'C';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridEditorColumnGrid.SetGridFixedDimension;
const OPNAME = 'TGridEditorColumnGrid.SetGridFixedDimension';
begin
  try
    FCellChangeEventActive := False;
    try
      if (FGridData.FieldCount >= 1) then
      begin
        RowCount := FGridData.FieldCount + 1;
        if (FGridData.GridField[0].RecordCount >= 1) then
          ColCount := FGridData.GridField[0].RecordCount + 1;
      end;
    finally
      FCellChangeEventActive := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorColumnGrid.RemoveEmptyGridRecords;
const OPNAME = 'TGridEditorColumnGrid.RemoveEmptyGridRecords';
var
  LRowIndex, LColIndex: integer;
  LBuffer: string;
begin
  try
    for LColIndex := ColCount - 1 downto 1 do
    begin
      LBuffer := '';
      for LRowIndex := 1 to RowCount - 1 do
      begin
        LBuffer := LBuffer + Trim(Cells[LColIndex, LRowIndex]);
        if (LBuffer <> '') then
          break;
      end;
      if (LBuffer = '') and (ColCount > 1) then
      begin
        DeleteColumn(LColIndex);
        FGridData.DeleteRecord(LColIndex - 1);
      end;  
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorColumnGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TGridEditorColumnGrid.DrawCell';
var
  LBoldAdded: boolean;
begin
  try
    LBoldAdded := False;
    if (FixedCols > 0) and (ACol = 0) then
    begin
      if not (fsBold in Canvas.Font.Style) then
      begin
        LBoldAdded := True;
        Canvas.Font.Style := Font.Style + [fsBold];
      end;
    end;
    inherited;
    if LBoldAdded then
      Canvas.Font.Style := Font.Style - [fsBold];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
