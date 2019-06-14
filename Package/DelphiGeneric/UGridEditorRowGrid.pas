//
//
//  UNIT      : Contains TGridEditorRowGrid Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/06/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorRowGrid;

interface

uses
  Types,
  vcl.Grids,
  Classes,
  UGridEditorStringGrid;

type
  TGridEditorRowGrid = class(TGridEditorStringGrid)
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
  Windows,
  vcl.Graphics,
  SysUtils,
  UErrorHandlingOperations;

procedure TGridEditorRowGrid.CreateMemberObjects;
const OPNAME = 'TGridEditorRowGrid.CreateMemberObjects';
begin
  try
    ColWidths[0] := 10;

    inherited CreateMemberObjects;
    FOrientationFlag := 'R';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridEditorRowGrid.SetGridFixedDimension;
const OPNAME = 'TGridEditorRowGrid.SetGridFixedDimension';
begin
  try
    FCellChangeEventActive := False;
    try
      if (FGridData.FieldCount >= 1) then
      begin
        ColCount := FGridData.FieldCount + 1;
        if (FGridData.GridField[0].RecordCount >= 1) then
          RowCount := FGridData.GridField[0].RecordCount + 1;
      end;
    finally
      FCellChangeEventActive := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorRowGrid.RemoveEmptyGridRecords;
const OPNAME = 'TGridEditorRowGrid.RemoveEmptyGridRecords';
var
  LRowIndex, LColumnIndex: integer;
  LBuffer: string;
begin
  try
    for LRowIndex := RowCount - 1 downto 1 do
    begin
      LBuffer := '';
      for LColumnIndex := 1 to ColCount - 1 do
      begin
        LBuffer := LBuffer + Trim(Cells[LColumnIndex, LRowIndex]);
        if (LBuffer <> '') then
          break;
      end;
      if (LBuffer = '') and (RowCount > 1) then
      begin
        DeleteRow(LRowIndex);
        FGridData.DeleteRecord(LRowIndex - 1);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorRowGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TGridEditorRowGrid.DrawCell';
var
  LBoldAdded: boolean;
begin
  try
    LBoldAdded := False;
    if(FixedRows > 0) and (ARow = 0) then
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
