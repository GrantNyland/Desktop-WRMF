//
//
//  UNIT      : Contains TGridEditorStringGrid Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/06/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorStringGrid;

interface

uses
  Classes,
  Windows,
  vcl.Grids,
  UConstants,
  UAbstractObject,
  UAbstractGridData,
  UGridEditorGridInterface,
  UStringGridWithCellChange;

const
  CIniSection_ColumnWidth = 'ColumnWidth';

type
  TGridEditorStringGrid = class(TStringGridWithCellChange, IGridEditorGrid)
  protected
    FReadOnlyDataSource: boolean;
    FGridData: TAbstractGridData;
    FOrientationFlag: char;
    FPivotRowMultiplier: integer;
    FCellChangeEventActive: boolean;
    procedure CreateMemberObjects; override;

    // Overriden from Delphi.
    function SelectCell(ACol, ARow: longint): Boolean; override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;

    // Overriden from TStringGridWithCellChange.
    function DoBeforeCellChange: boolean; override;
    procedure DoAfterCellChange; override;

    // Abstract methods that depend on how the data is set out on the grid.
    procedure SetGridFixedDimension; virtual; abstract;
    procedure DisplayFieldName(AVisibleFieldIndex: integer; AFieldName: string);
    procedure RemoveEmptyGridRecords; virtual; abstract;
    //procedure PopulateTotals;virtual; abstract;

    // Introduced in this class.
    procedure DisplayFieldNames;
    procedure DisplayData;
    procedure DisplayField(AVisibleFieldIndex, AVisibleRecordIndex: integer; AValue: string);
    function GetCellFieldProperty(ACol, ARow: integer): TAbstractFieldProperty;
    procedure PutValueInCell(ACol, ARow: integer; AValue: string);
    procedure LoadColumnWidths;
  public
    function SaveState: boolean; override;
    function IsVisible: boolean;
    procedure SetBrowseMode;
    procedure SetReadOnlyMode(AIsReadOnly: boolean);
    procedure ClearDataViewer;
    procedure CopyGridDataInto(var AStringList: TStringList);
    procedure PopulateDataViewer; virtual;
    procedure UpdateHintText;
    function CurrentFieldHintText: string;
    procedure DoPrint(Title: string); override;
    property GridData: TAbstractGridData read FGridData write FGridData;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  vcl.Controls,
  vcl.Graphics,
  vcl.Printers,
  UDBConstants,
  UErrorHandlingOperations, UAbstractComponent;

const
  CBrowseOptions = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goTabs];
  CEditOptions   = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs];

procedure TGridEditorStringGrid.CreateMemberObjects;
const OPNAME = 'TGridEditorStringGrid.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    // Set hint properties.
    Hint := 'Grid';
    ShowHint := False;
    ParentShowHint := False;
    FOrientationFlag := #0;
    FPivotRowMultiplier := 0;
    FGridData := nil;
    FCellChangeEventActive := True;

    // Set the default mode.
    ClearDataViewer;
    SetBrowseMode;

  // Handle exception.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TGridEditorStringGrid.IsVisible: boolean;
const OPNAME = 'TGridEditorStringGrid.IsVisible';
begin
  Result := False;
  try
    Result := Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.SetBrowseMode;
const OPNAME = 'TGridEditorStringGrid.SetBrowseMode';
begin
  try
    EditorMode := False;
    Options := CBrowseOptions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.SetReadOnlyMode(AIsReadOnly: boolean);
const OPNAME = 'TGridEditorStringGrid.SetReadOnlyMode';
begin
  try
    FReadOnlyDataSource := AIsReadOnly;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.ClearDataViewer;
const OPNAME = 'TGridEditorStringGrid.ClearDataViewer';
var LColIndex, LRowIndex: integer;
begin
  try

    // Clear the data (go one row past for hiden data).
    ResetStartValue;
    for LColIndex := 0 to ColCount do
      for LRowIndex := 0 to RowCount do
        Cells[LColIndex, LRowIndex] := '';

    // Set the default grid dimensions.
    FCellChangeEventActive := False;
    try
      EditorMode := False;
      ColCount   := 2;
      RowCount   := 2;
      FixedCols  := 1;
      FixedRows  := 1;
      Col        := 1;
      Row        := 1;
    finally
      FCellChangeEventActive := True;
    end;

    // Hide the grid and clear the hint.
    Visible := False;
    UpdateHintText;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridEditorStringGrid.PopulateDataViewer;
const OPNAME = 'TGridEditorStringGrid.PopulateDataViewer';
begin
  try
    if Assigned(FGridData) and (FGridData.FieldCount > 0) then
    begin
      SetGridFixedDimension;
      DisplayFieldNames;
      DisplayData;
      LoadColumnWidths;
      Visible := True;
      UpdateHintText;
      Resize;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.DisplayFieldNames;
const OPNAME = 'TGridEditorStringGrid.DisplayFieldNames';
var LVisibleFieldIndex: integer;
begin
  try
    if Assigned(FGridData) and (FGridData.FieldCount > 0) then
    begin
      for LVisibleFieldIndex := 0 to FGridData.FieldCount - 1 do
        DisplayFieldName(LVisibleFieldIndex + 1,
          FGridData.GridField[LVisibleFieldIndex].FieldProperty.FieldLangDescr);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.DisplayData;
const OPNAME = 'TGridEditorStringGrid.DisplayData';
var
  LFieldIndex, LRecordIndex: integer;
begin
  try
    if Assigned(FGridData) and (FGridData.FieldCount > 0) then
    begin
      for LFieldIndex := 0 to FGridData.FieldCount - 1 do
        for LRecordIndex := 0 to FGridData.GridField[LFieldIndex].RecordCount - 1 do
          DisplayField(LFieldIndex, LRecordIndex,
            FGridData.FieldData[LFieldIndex, LRecordIndex].DisplayText);

      // Remove empty rows.
      RemoveEmptyGridRecords;
    end;
    
  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.DisplayField(AVisibleFieldIndex, AVisibleRecordIndex: integer; AValue: string);
const OPNAME = 'TGridEditorStringGrid.DisplayField';
begin
  try
    case FOrientationFlag of
      'R' : PutValueInCell(AVisibleFieldIndex  + 1, AVisibleRecordIndex + 1, AValue);
      'C' : PutValueInCell(AVisibleRecordIndex + 1, AVisibleFieldIndex  + 1, AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorStringGrid.GetCellFieldProperty(ACol, ARow: Longint): TAbstractFieldProperty;
const OPNAME = 'TGridEditorStringGrid.GetFieldProperties';
var
  LGridFieldData: TAbstractGridFieldData;
begin
  Result := nil;
  try
    if Assigned(FGridData) and (FGridData.FieldCount > 0) then
    begin
      if (ACol > 0) and (ARow > 0) then
      begin
        LGridFieldData := nil;
        case FOrientationFlag of
          'R' : LGridFieldData := FGridData.FieldData[ACol - 1, ARow - 1];
          'C' : LGridFieldData := FGridData.FieldData[ARow - 1, ACol - 1];
        end;
        if(LGridFieldData <> nil) then
          Result := LGridFieldData.FieldProperty;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.PutValueInCell(ACol, ARow: integer; AValue: string);
const OPNAME = 'TGridEditorStringGrid.PutValueInCell';
var LFieldProperty: TAbstractFieldProperty;
begin
  try
    if(Trim(AValue) = '') then
    begin
      Cells[ACol, ARow] := '';
    end
    else
    begin
      LFieldProperty := GetCellFieldProperty(ACol, ARow);
      if Assigned(LFieldProperty) then
      begin
        case LFieldProperty.FieldDataType of
          FieldStringType : AValue := Trim(Format(LFieldProperty.FormatStringGrid, [Trim(AValue)]));
          FieldFloatType  : try AValue := Trim(Format(LFieldProperty.FormatStringGrid, [StrToFloat(Trim(AValue))])); except end;
          FieldIntegerType: try AValue := Trim(Format(LFieldProperty.FormatStringGrid, [StrToInt(Trim(AValue))])); except end;
          FieldDTimeType  : ;
          FieldCharType   : ;
        else
          raise Exception.CreateFmt('Unknown field data type [%d].', [LFieldProperty.FieldDataType]);
        end;
        Cells[ACol, ARow] := AValue;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorStringGrid.SaveState: boolean;
const OPNAME = 'TGridEditorStringGrid.SaveState';
var
  LColIndex: integer;
  LWidths: TStringList;
begin
  Result := False;
  try
    inherited SaveState;
    if Assigned(FGridData) and Visible and (FGridData.DataIDCommaText <> '') then
    begin
      LWidths := TStringList.Create;
      try
        for LColIndex := 1 to ColCount - 1 do
          LWidths.Add(Format('%2.2d', [ColWidths[LColIndex]]));
          FAppModules.ViewIni.WriteString(CIniSection_ColumnWidth,
          FGridData.DataIDCommaText + FOrientationFlag, LWidths.CommaText);
      finally
        LWidths.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.LoadColumnWidths;
const OPNAME = 'TGridEditorStringGrid.LoadColumnWidths';
var
  LColIndex: integer;
  LWidths: TStringList;
begin
  try
    if Assigned(FGridData) and (FGridData.FieldCount > 0) then
    begin
      LWidths := TStringList.Create;
      try
        LWidths.CommaText :=
          FAppModules.ViewIni.ReadString(
            CIniSection_ColumnWidth, FGridData.DataIDCommaText + FOrientationFlag, '');
        for LColIndex := 1 to ColCount - 1 do
          if ((LColIndex - 1) < LWidths.Count) then
            try ColWidths[LColIndex] := StrToInt(LWidths[LColIndex - 1]) except end;
      finally
        LWidths.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.CopyGridDataInto(var AStringList: TStringList);
const OPNAME = 'TGridEditorStringGrid.CopyGridDataInto';
var LRowIndex: integer;
function FirstRowEmpty: boolean;
var LCount: integer;
begin
  Result := True;
  for LCount := 0 to ColCount - 1 do
    if (Trim(Cells[LCount,0]) <> '') then
    begin
      Result := False;
      Break;
    end;
end;
function FirstColEmpty: boolean;
var LCount: integer;
begin
  Result := True;
  for LCount := 0 to RowCount - 1 do
    if (Trim(Cells[0,LCount]) <> '') then
    begin
      Result := False;
      Break;
    end;
end;

begin
  try
    AStringList.Clear;
    if FirstRowEmpty then
    begin
      for LRowIndex := 1 to RowCount - 1 do
        AStringList.Add(Rows[LRowIndex].CommaText);
    end
    else
    if FirstColEmpty then
    begin
      for LRowIndex := 0 to RowCount - 1 do
        AStringList.Add(Rows[LRowIndex].CommaText);
      for LRowIndex := 0 to AStringList.Count - 1 do
        if(Length(AStringList[LRowIndex]) > 1) and (AStringList[LRowIndex][1] = ',') then
          AStringList[LRowIndex] := Copy(AStringList[LRowIndex],2,Length(AStringList[LRowIndex]));
    end
    else
    begin
      for LRowIndex := 0 to RowCount - 1 do
        AStringList.Add(Rows[LRowIndex].CommaText);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorStringGrid.DoBeforeCellChange: boolean;
const OPNAME = 'TGridEditorStringGrid.DoBeforeCellChange';
var
  LFieldName, LNewValue, LOldValue: string;
  LContextData: TStringList;
begin
  Result := False;
  try
    try
      if FDataHasChanged then
      begin
        if Assigned(FGridData) and (FGridData.FieldCount > 0) then
        begin
          LFieldName := GetCellFieldProperty(Col, Row).FieldName;
          LNewValue := Trim(Cells[Col, Row]);
          LOldValue := FCurrentCellStartContents;
          LContextData := nil;
          if (Col > 0) and (Row > 0) then
          begin
            case FOrientationFlag of
              'R' : LContextData := FGridData.FieldData[Col - 1, Row - 1].ContextData;
              'C' : LContextData := FGridData.FieldData[Row - 1, Col - 1].ContextData;
            end;
          end;
          if Assigned(LContextData) then
          begin
            if not FAppModules.Model.UpdateFieldValue(LFieldName, LNewValue, LOldValue, LContextData) then
              CancelEdits
            else
              FAppModules.FieldProperties.LastFieldToChange.ChangedBy := 'Grid';
          end;
        end;
      end;
    finally
      Result := inherited DoBeforeCellChange;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.DoAfterCellChange;
const OPNAME = 'TGridEditorStringGrid.DoAfterCellChange';
begin
  try
    inherited DoAfterCellChange;
    UpdateHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorStringGrid.SelectCell(ACol, ARow: Longint): Boolean;
const OPNAME = 'TGridEditorStringGrid.SelectCell';
var
  LFieldName: string;
begin
  Result := False;
  try

    // Set browse mode by default.
    Result := (inherited SelectCell(ACol, ARow));
    Options := CBrowseOptions;

    // Check data set and user privelege.
    if Result and FCellChangeEventActive then
    begin
      if (ACol > 0) and (ARow > 0) then
      begin
        if (FAppModules.User.UserRights in CUR_EditData) then
        begin
          if Assigned(GetCellFieldProperty(ACol, ARow)) then
          begin
            LFieldName := GetCellFieldProperty(ACol, ARow).FieldName;
            if FAppModules.FieldProperties.ModeIsEditable(LFieldName) then
            begin
              Options := CEditOptions;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.DrawCell(ACol, ARow: integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TGridEditorStringGrid.DrawCell';
var
  LFieldProperty: TAbstractFieldProperty;
  LOldColor: TColor;
begin
  try

    // Call inherited for columns and rows that require normal drawing.
    if (ACol < 1) or (ARow < 1) then
    begin
      inherited DrawCell(ACol, ARow, ARect, AState);
    end else begin

      // Check if the cell can be edited.
      LFieldProperty := GetCellFieldProperty(ACol, ARow);

      if Assigned(LFieldProperty) and (FAppModules.FieldProperties.ModeIsEditable(LFieldProperty.FieldName)) then
      begin
        inherited DrawCell(ACol, ARow, ARect, AState);
      end else begin

        // Gray the read only and unkown cells.
        LOldColor := Canvas.Brush.Color;
        try
          Canvas.Brush.Color := clBtnFace;
          inherited DrawCell(ACol, ARow, ARect, AState);
        finally
          Canvas.Brush.Color := LOldColor;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.DisplayFieldName(AVisibleFieldIndex: integer; AFieldName: string);
const OPNAME = 'TGridEditorStringGrid.DisplayFieldName';
begin
  try
    case FOrientationFlag of
      'R' : Cells[AVisibleFieldIndex, 0] := Trim(AFieldName);
      'C' : Cells[0, AVisibleFieldIndex] := Trim(AFieldName);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorStringGrid.UpdateHintText;
const OPNAME = 'TGridEditorStringGrid.UpdateHintText';
var
  LFieldProperty: TAbstractFieldProperty;
  LGridHintText: string;
begin
  try
    LGridHintText := '';
    if Visible and Assigned(GridData) then
    begin
      LFieldProperty := GetCellFieldProperty(Col, Row);
      if Assigned(LFieldProperty) then
      begin
        LGridHintText := LFieldProperty.FieldDescription;
        LGridHintText := LFieldProperty.FieldName + ': ' + FAppModules.Language.GetString(LGridHintText);
      end;
    end;
    Hint := LGridHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorStringGrid.CurrentFieldHintText: string;
const OPNAME = 'TGridEditorStringGrid.CurrentFieldHintText';
begin
  Result := '';
  try
    Result := Hint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//Author: Reinhard Schatzl
//Site:http://www.swissdelphicenter.ch/torry/showcode.php?id=1577
// Modified from German to english
procedure TGridEditorStringGrid.DoPrint(Title: string);
const OPNAME = 'TGridEditorStringGrid.DoPrint';
var
  LPageIndex, LLinesIndex, LColumnIndex, LYPos, LXPos, LHorzSize, LVertSize: Integer;
  LPageCount, LPageCurrent, LLines, LHeaderSize, LFooterSize, LLineSize, LFontHeight: Integer;
  LPageWidth, LPageHeight: Extended;
  LFooter: string;
begin
  LHeaderSize := 100;
  LFooterSize := 200;
  LLineSize := 36;
  LFontHeight := 36;

  Printer.Title  := Title;
  Printer.BeginDoc;

  LPageWidth := GetDeviceCaps(Printer.Canvas.Handle, PHYSICALWIDTH) /
    GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX) * 25.4; 
  LPageHeight := GetDeviceCaps(Printer.Canvas.Handle, PHYSICALHEIGHT) / 
    GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY) * 25.4; 

  LVertSize := Trunc(LPageHeight) * 10;
  LHorzSize := Trunc(LPageWidth) * 10;
  SetMapMode(Printer.Canvas.Handle, MM_LOMETRIC);

  LLines := (LVertSize - LHeaderSize - LFooterSize) div LLineSize;
  if Self.RowCount mod LLines <> 0 then
    LPageCount := Self.RowCount div LLines + 1
  else
    LPageCount := Self.RowCount div LLines;

  LPageCurrent := 1;
  for LPageIndex := 1 to LPageCount do
  begin
    Printer.Canvas.Font.Height := 48;
    Printer.Canvas.TextOut((LHorzSize div 2 - (Printer.Canvas.TextWidth(Title) div 2)),
      - 20,Title);
    Printer.Canvas.Pen.Width := 5;
    Printer.Canvas.MoveTo(0, - LHeaderSize);
    Printer.Canvas.LineTo(LHorzSize, - LHeaderSize);
    Printer.Canvas.MoveTo(0, - LVertSize + LFooterSize);
    Printer.Canvas.LineTo(LHorzSize, - LVertSize + LFooterSize);
    Printer.Canvas.Font.Height := 36;
    LFooter := 'Page: ' + IntToStr(LPageCurrent) + ' of ' + IntToStr(LPageCount);
    Printer.Canvas.TextOut((LHorzSize div 2 - (Printer.Canvas.TextWidth(LFooter) div 2)),
      - VertSize + 150,LFooter);
    Printer.Canvas.Font.Height := LFontHeight;
    LYPos := LHeaderSize + 10;
    for LLinesIndex := 1 to LLines do
    begin
      if Self.RowCount >= LLinesIndex + (LPageCurrent - 1) * LLines then
      begin
        LXPos := 0;
        for LColumnIndex := 0 to Self.ColCount - 1 do
        begin
          Printer.Canvas.TextOut(LXPos, - LYPos,
            Self.Cells[LColumnIndex, LLinesIndex + (LPageCurrent - 1) * LLines - 1]);
          LXPos := LXPos + Self.ColWidths[LColumnIndex] * 3;
        end;
        LYPos := LYPos + LLineSize;
      end;
    end;
    Inc(LPageCurrent);
    if LPageCurrent <= LPageCount then Printer.NewPage;
  end; 
  Printer.EndDoc;
end;

end.
