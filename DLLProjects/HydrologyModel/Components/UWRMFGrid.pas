unit UWRMFGrid;

interface

uses
  VCL.Grids,
  Classes,
  Windows,
  Contnrs,
  VCL.Controls,
  UCellChangeStringGrid;

type
  TWRMFGrid = class;

  TCellInfo = class(TCollectionItem)
  private
    FRow            : Integer;
    FColumn         : Integer;
    FPropertyName   : String;
    FHasParamChange : Boolean;
    FHasMetaData    : Boolean;
    FIsValid        : Boolean;
    FActive         : Boolean;
  protected
//    procedure DefineProperties(Filer: TFiler); override;
    procedure SetInfo (APropertyName   : String;
                       var AChanged    : Boolean);
  published
    property PropertyName    : String     read FPropertyName    write FPropertyName;
    property HasParamChange  : Boolean    read FHasParamChange  write FHasParamChange;
    property HasMetaData     : Boolean    read FHasMetaData     write FHasMetaData;
    property IsValid         : Boolean    read FIsValid         write FIsValid;
    property Row             : Integer    read FRow             write FRow;
    property Column          : Integer    read FColumn          write FColumn;
    property Active          : Boolean    read FActive          write FActive;
  end;

  TCellInfoCollection = class(TCollection)
  private
    FRowCount        : Integer;
    FColCount        : Integer;
    FGrid            : TWRMFGrid;
    FAssignOption    : Integer;
    FNoOfHeadingRows : Integer;
    FNoOfHeadingCols : Integer;
    FDataListName    : String;
    function GetCellInfo (ARow : Integer; ACol : Integer) : TCellInfo;
    function AddCellInfo (ARow : Integer; ACol : Integer) : TCellInfo;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create (AGrid : TWRMFGrid);
    procedure ChangeSize (ANewRowCount : Integer; ANewColCount : Integer);
    procedure SetCellsInfo (ARow             : Integer;
                            AColumn          : Integer;
                            AAssignOption    : Integer;
                            APropertyName    : String;
                            var AChanged     : Boolean);
    procedure SetActive (AActive : Boolean);
    procedure SetRowActive (ARow : Integer; AActive : Boolean);
    procedure SetColumnActive (AColumn : Integer; AActive : Boolean);
    function DeleteRow (ARow : Integer) : Boolean;
    function InsertRow (ARow : Integer) : Boolean;
    function MoveRowUp (ARow : Integer) : Boolean;
    function MoveRowDown (ARow : Integer) : Boolean;
    property CellInfo[ARow : Integer; ACol : Integer] : TCellInfo read GetCellInfo;
    property RowCount        : Integer    read FRowCount;
    property ColCount        : Integer    read FColCount;
    property Grid            : TWRMFGrid read FGrid;
  published
    property AssignOption    : Integer read FAssignOption    write FAssignOption;
    property NoOfHeadingRows : Integer read FNoOfHeadingRows write FNoOfHeadingRows;
    property NoOfHeadingCols : Integer read FNoOfHeadingCols write FNoOfHeadingCols;
    property DataListName    : String  read FDataListName    write FDataListName;
  end;

  TBeforeCellChangeEvent = procedure (Sender: TObject; Col, Row: integer; HasValueChanged: boolean) of object;
  TAfterCellChangeEvent = procedure (Sender: TObject; Col, Row: integer) of object;
  TCellNotifyEvent = procedure (Sender: TObject; Col, Row: integer) of object;
  TWRMFGrid = class(TCellChangeStringGrid)
  private
    FCellsInfo                : TCellInfoCollection;
    FWrapHeaderText           : Boolean;
    FOnParamChangeClick       : TCellNotifyEvent;
    FOnMetaDataClick          : TCellNotifyEvent;
    procedure SetCellsInfo (AValue : TCellInfoCollection);
    function GetCellInfo (ARow : Integer; ACol : Integer) : TCellInfo;
  protected

    // Overriden from TStringGrid.
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var AKey: Char); override;
    procedure DoEnter; override;
    procedure DoExit; override;

    // Introduced in this class.
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function ParamChangeShapeClicked (X, Y: Integer): boolean;
    function MetaDataShapeClicked (X, Y: Integer): boolean;
    procedure DoParamChangeIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMetaDataIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  public
    // Constructor.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    // Introduced in this class.
    procedure FixCellFormat(Col, Row: integer); virtual;
    procedure ReformatEntireGrid; virtual;
    procedure SetActive (AActive : Boolean);
    procedure SetRowActive (ARow : Integer; AActive : Boolean);
    procedure SetColumnActive (AColumn : Integer; AActive : Boolean);
    procedure DeleteRow (ARow : Integer); override;
    procedure InsertRow (ARow : Integer);
    procedure MoveRowUp (ARow : Integer);
    procedure MoveRowDown (ARow : Integer);
    // Properties.
    property CellInfo[ARow : Integer; ACol : Integer] : TCellInfo read GetCellInfo;
    property Active        : Boolean write SetActive;
    property RowActive[ARow : Integer] : Boolean write SetRowActive;
    property ColumnActive[ACol : Integer] : Boolean write SetColumnActive;
  published
    property CellsInfo          : TCellInfoCollection    read FCellsInfo          write SetCellsInfo;
    property WrapHeaderText     : Boolean                read FWrapHeaderText     write FWrapHeaderText;
    property OnParamChangeClick : TCellNotifyEvent       read FOnParamChangeClick write FOnParamChangeClick;
    property OnMetaDataClick    : TCellNotifyEvent       read FOnMetaDataClick    write FOnMetaDataClick;
  end;


implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Graphics,
  Math,

  UErrorHandlingOperations;


const
  C_NotUsedYetFlag = #10;

{ TCellInfo *******************************************************************}

procedure TCellInfo.SetInfo (APropertyName   : String;
                             var AChanged    : Boolean);
const OPNAME = 'TCellInfo.SetInfo';
begin
  try
    AChanged := FALSE;
    if (FPropertyName <> APropertyName) then
    begin
      FPropertyName := APropertyName;
      AChanged      := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

(*
procedure TCellInfo.DefineProperties(Filer: TFiler);
const OPNAME = 'TCellInfo.DefineProperties';
begin
  try
    inherited DefineProperties(Filer);
//    Filer.DefineProperty('Row', ReadData, WriteData, TRUE);
//    Filer.DefineProperty('Column', ReadData, WriteData, TRUE);
    Filer.DefineProperty('PropertyName', ReadPropertyName, WritePropertyName, TRUE);
//    Filer.DefineProperty('HasParamChange', ReadData, WriteData, TRUE);
//    Filer.DefineProperty('HasMetaData', ReadData, WriteData, TRUE);
//    Filer.DefineProperty('IsValid', ReadData, WriteData, TRUE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCellInfo.WritePropertyName(Writer: TWriter);
const OPNAME = 'TCellInfo.WritePropertyName';
begin
  try
    with Writer do
    begin
      WriteListBegin;
      WriteInteger(Row);
      WriteInteger(Column);
      WriteString(PropertyName);
      WriteBoolean(HasParamChange);
      WriteBoolean(HasMetaData);
      WriteBoolean(IsValid);
      WriteListEnd;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCellInfo.ReadPropertyName(Reader: TReader);
const OPNAME = 'TCellInfo.ReadPropertyName';
begin
  try
    with Reader do
    begin
      ReadListBegin;
      Row            := ReadInteger;
      Column         := ReadInteger;
      PropertyName   := ReadString;
      HasParamChange := ReadBoolean;
      HasMetaData    := ReadBoolean;
      IsValid        := ReadBoolean;
      ReadListEnd;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
*)
{ TCellInfoCollection *********************************************************}

constructor TCellInfoCollection.Create (AGrid : TWRMFGrid);
const OPNAME = 'TCellInfoCollection.Create';
var
  LRow       : Integer;
  LCol       : Integer;
begin
  try
    inherited Create(TCellInfo);
    FAssignOption  := 1;
    FGrid          := AGrid;
    FRowCount      := AGrid.RowCount;
    FColCount      := AGrid.ColCount;
    FNoOfHeadingRows := AGrid.FixedRows;
    FNoOfHeadingCols := AGrid.FixedCols;
    FDataListName    := 'DataList';
    PropName       := 'CellsInfo';
    for LRow := 0 to FRowCount - 1 do
    begin
      for LCol := 0 to FColCount - 1 do
      begin
        AddCellInfo(LRow, LCol);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCellInfoCollection.AddCellInfo (ARow : Integer; ACol : Integer) : TCellInfo;
const OPNAME = 'TCellInfoCollection.AddCellInfo';
var
  LCellInfo : TCellInfo;
begin
  Result := nil;
  try
    LCellInfo := TCellInfo(Add);
    LCellInfo.FRow           := ARow;
    LCellInfo.FColumn        := ACol;
    LCellInfo.FPropertyName  := '';
    LCellInfo.HasParamChange := FALSE;
    LCellInfo.HasMetaData    := FALSE;
    LCellInfo.IsValid        := TRUE;
    LCellInfo.Active         := TRUE;

    Result := LCellInfo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCellInfoCollection.GetOwner: TPersistent;
const OPNAME = 'TCellInfoCollection.GetOwner';
begin
  Result := nil;
  try
    Result := FGrid;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCellInfoCollection.GetCellInfo (ARow : Integer;
                                          ACol : Integer) : TCellInfo;
const OPNAME = 'TCellInfoCollection.GetCellInfo';
var
  LIndex    : Integer;
  LCellInfo : TCellInfo;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < Self.Count)) do
    begin
      LCellInfo := TCellInfo(Items[LIndex]);
      if ((LCellInfo.Row = ARow) AND (LCellInfo.Column = ACol)) then
        Result := LCellInfo
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCellInfoCollection.ChangeSize (ANewRowCount : Integer;
                                          ANewColCount : Integer);
const OPNAME = 'TCellInfoCollection.ChangeSize';
var
  LCellInfo    : TCellInfo;
  LIndex       : Integer;
  LRow         : Integer;
  LCol         : Integer;
begin
  try
    if ((ANewRowCount < FRowCount) OR (ANewColCount < FColCount)) then
    begin
      LIndex := 0;
      while (LIndex < Count) do
      begin
        LCellInfo := TCellInfo(Items[LIndex]);
        if ((LCellInfo.Row >= ANewRowCount) OR (LCellInfo.Column >= ANewColCount)) then
          Self.Delete(LIndex)
        else
          LIndex := LIndex + 1;
      end;
    end;

    if (ANewRowCount > FRowCount) then
    begin
      for LRow := FRowCount to ANewRowCount - 1 do
      begin
        for LCol := 0 to FColCount - 1 do
        begin
          AddCellInfo(LRow, LCol);
        end;
      end;
    end;
    FRowCount := ANewRowCount;
    if (ANewColCount > FColCount) then
    begin
      for LRow := 0 to FRowCount - 1 do
      begin
        for LCol := FColCount to ANewColCount - 1 do
        begin
          AddCellInfo(LRow, LCol);
        end;
      end;
    end;
    FColCount := ANewColCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCellInfoCollection.SetCellsInfo (ARow             : Integer;
                                            AColumn          : Integer;
                                            AAssignOption    : Integer;
                                            APropertyName    : String;
                                            var AChanged     : Boolean);
const OPNAME = 'TCellInfoCollection.SetCellsInfo';
var
  LRowIndex : Integer;
  LColIndex : Integer;
  LChanged  : Boolean;
begin
  try
    AChanged := FALSE;
    FAssignOption := AAssignOption;
    for LRowIndex := 0 to FRowCount - 1 do
    begin
      for LColIndex := 0 to FColCount - 1 do
      begin
        if ((FAssignOption = 0) OR
            ((FAssignOption = 1) AND (LColIndex = AColumn)) OR
            ((FAssignOption = 2) AND (LRowIndex = ARow)) OR
            ((FAssignOption = 3) AND (LRowIndex = ARow) AND (LColIndex = AColumn))) then
        begin
          CellInfo[LRowIndex, LColIndex].SetInfo(APropertyName, LChanged);
          AChanged := AChanged OR LChanged;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCellInfoCollection.SetActive (AActive : Boolean);
const OPNAME = 'TCellInfoCollection.SetActive';
var
  LColIndex : Integer;
  LRowIndex : Integer;
begin
  try
    for LRowIndex := 0 to FRowCount - 1 do
    begin
      for LColIndex := 0 to FColCount - 1 do
      begin
        CellInfo[LRowIndex, LColIndex].Active := AActive;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCellInfoCollection.SetRowActive (ARow : Integer; AActive : Boolean);
const OPNAME = 'TCellInfoCollection.SetRowActive';
var
  LColIndex : Integer;
begin
  try
    for LColIndex := 0 to FColCount - 1 do
    begin
      CellInfo[ARow, LColIndex].Active := AActive;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCellInfoCollection.SetColumnActive (AColumn : Integer; AActive : Boolean);
const OPNAME = 'TCellInfoCollection.SetColumnActive';
var
  LRowIndex : Integer;
begin
  try
    for LRowIndex := 0 to FRowCount - 1 do
    begin
      CellInfo[LRowIndex, AColumn].Active := AActive;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCellInfoCollection.DeleteRow (ARow : Integer) : Boolean;
const OPNAME = 'TCellInfoCollection.DeleteRow';
var
  LIndex    : Integer;
  LCellInfo : TCellInfo;
begin
  Result := FALSE;
  try
//    if (ARow >= FGrid.FixedRows) then
    if (ARow >= FGrid.CellsInfo.NoOfHeadingRows) then
    begin
      LIndex := 0;
      while (LIndex < Count) do
      begin
        LCellInfo := TCellInfo(Self.Items[LIndex]);
        if (LCellInfo.Row = ARow) then
          Self.Delete(LIndex)
        else
        begin
          if (LCellInfo.Row > ARow) then
            LCellInfo.Row := LCellInfo.Row - 1;
          LIndex := LIndex + 1;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCellInfoCollection.InsertRow (ARow : Integer) : Boolean;
const OPNAME = 'TCellInfoCollection.InsertRow';
var
  LIndex       : Integer;
  LCellInfo    : TCellInfo;
  LNewCellInfo : TCellInfo;
  LInsertRow   : Integer;
begin
  Result := FALSE;
  try
    LInsertRow := ARow + 1;
    for LIndex := 0  to Count-1 do
    begin
      LCellInfo := TCellInfo(Self.Items[LIndex]);
      if (LCellInfo.Row >= LInsertRow) then
        LCellInfo.Row := LCellInfo.Row + 1;
    end;
    for LIndex := 0 to FColCount - 1 do
    begin
      LNewCellInfo := AddCellInfo(LInsertRow, LIndex);
      LCellInfo := CellInfo[ARow, LIndex];
      LNewCellInfo.PropertyName := LCellInfo.PropertyName;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCellInfoCollection.MoveRowUp (ARow : Integer) : Boolean;
const OPNAME = 'TCellInfoCollection.MoveRowUp';
var
  LIndex    : Integer;
  LCellInfo : TCellInfo;
begin
  Result := FALSE;
  try
    if (ARow > FGrid.FixedRows) then
    begin
      for LIndex := 0  to Count-1 do
      begin
        LCellInfo := TCellInfo(Self.Items[LIndex]);
        if (LCellInfo.Row = ARow) then
          LCellInfo.Row := LCellInfo.Row - 1
        else if (LCellInfo.Row = ARow-1) then
          LCellInfo.Row := LCellInfo.Row + 1;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCellInfoCollection.MoveRowDown (ARow : Integer) : Boolean;
const OPNAME = 'TCellInfoCollection.MoveRowDown';
var
  LIndex    : Integer;
  LCellInfo : TCellInfo;
begin
  Result := FALSE;
  try
    if (ARow >= FGrid.FixedRows) AND (ARow < FGrid.RowCount - 1) then
    begin
      for LIndex := 0  to Count-1 do
      begin
        LCellInfo := TCellInfo(Self.Items[LIndex]);
        if (LCellInfo.Row = ARow) then
          LCellInfo.Row := LCellInfo.Row + 1
        else if (LCellInfo.Row = ARow+1) then
          LCellInfo.Row := LCellInfo.Row - 1;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TWRMFGrid ******************************************************************}

constructor TWRMFGrid.Create(AOwner: TComponent);
const OPNAME = 'TWRMFGrid.Create';
begin
  try
    inherited Create(AOwner);
    Options := Options + [goEditing, goTabs];
    FCellsInfo := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRMFGrid.AfterConstruction;
const OPNAME = 'TWRMFGrid.AfterConstruction';
begin
  try
    FCellsInfo := TCellInfoCollection.Create(Self);
    inherited AfterConstruction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TWRMFGrid.Destroy;
const OPNAME = 'TWRMFGrid.Destroy';
begin
  try
    FCellsInfo.Free;
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Fixes the format of the required cell.
//
procedure TWRMFGrid.FixCellFormat(Col, Row: integer);
const OPNAME = 'TWRMFGrid.FixCellFormat';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRMFGrid.ReformatEntireGrid;
const OPNAME = 'TWRMFGrid.ReformatEntireGrid';
var
  LColIndex : integer;
  LRowIndex : integer;
begin
  try
    for LColIndex := 1 to ColCount - 1 do
      for LRowIndex := 1 to RowCount - 1 do
        FixCellFormat(LColIndex, LRowIndex);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// The event handler for mouse button presses. Calls the cell change event handler
// if the cell might change.
//
procedure TWRMFGrid.MouseDown (AButton : TMouseButton;
                                AShift  : TShiftState;
                                X, Y    : Integer);
const OPNAME = 'TWRMFGrid.MouseDown';
var
  LDone   : Boolean;
begin
  try
    LDone := FALSE;
    if (AButton = mbLeft) then
    begin
      if (ParamChangeShapeClicked(X, Y)) then
      begin
        DoParamChangeIndicatorClicked(Self, AButton, AShift, X, Y);
        LDone := TRUE;
      end
      else
      begin
        if (MetaDataShapeClicked(X,Y)) then
        begin
          DoMetaDataIndicatorClicked(Self, AButton, AShift, X, Y);
          LDone := TRUE;
        end;  
      end;
    end;

    if (NOT LDone) then
      inherited MouseDown(AButton, AShift, X, Y);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRMFGrid.KeyPress(var AKey: Char);
const OPNAME = 'TFieldStringGrid.KeyPress';
begin
  try
    if (NOT Self.CellInfo[Row, Col].Active) then
      AKey := #0;
    if(AKey = #0) then
      SysUtils.Beep;
    inherited KeyPress(AKey);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Called when the control recieves focus.
//
procedure TWRMFGrid.DoEnter;
const OPNAME = 'TWRMFGrid.DoEnter';
begin
  try
    StoreStartValue;
    inherited DoEnter;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Called when the control looses focus.
//
procedure TWRMFGrid.DoExit;
const OPNAME = 'TWRMFGrid.DoExit';
begin
  try
    DoBeforeCellChange;
    inherited DoExit;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRMFGrid.SizeChanged (OldColCount, OldRowCount: Integer);
const OPNAME = 'TWRMFGrid.SizeChanged';
begin
  inherited SizeChanged(OldColCount, OldRowCount);
  try
    if not (csDestroying in ComponentState) then
    begin
      FCellsInfo.ChangeSize(RowCount, ColCount);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.SetActive (AActive : Boolean);
const OPNAME = 'TWRMFGrid.SetRowActive';
begin
  try
    FCellsInfo.SetActive(AActive);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRMFGrid.SetRowActive (ARow : Integer; AActive : Boolean);
const OPNAME = 'TWRMFGrid.SetRowActive';
begin
  try
    FCellsInfo.SetRowActive(ARow, AActive);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRMFGrid.SetColumnActive (AColumn : Integer; AActive : Boolean);
const OPNAME = 'TWRMFGrid.SetColumnActive';
begin
  try
    FCellsInfo.SetColumnActive(AColumn, AActive);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRMFGrid.DeleteRow (ARow : Integer);
const OPNAME = 'TWRMFGrid.DeleteRow';
begin
  try
    if (FCellsInfo.DeleteRow(ARow)) then
    begin
      inherited DeleteRow(ARow);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.InsertRow (ARow : Integer);
const OPNAME = 'TWRMFGrid.InsertRow';
var
  LRowIndex : Integer;
  LColIndex : Integer;
begin
  try
    if (FCellsInfo.InsertRow(ARow)) then
    begin
      RowCount := RowCount + 1;
      for LRowIndex := RowCount - 1 downto ARow + 2 do
      begin
        for LColIndex := 0 to ColCount - 1 do
          Cells[LColIndex, LRowIndex] := Cells[LColIndex, LRowIndex-1];
      end;
      for LColIndex := 0 to ColCount - 1 do
        Cells[LColIndex, ARow+1] := '';
      Row := ARow + 1;  
      Invalidate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.MoveRowUp (ARow : Integer);
const OPNAME = 'TWRMFGrid.MoveRowUp';
var
  LColIndex : Integer;
  LTemp     : String;
begin
  try
    if (FCellsInfo.MoveRowUp(ARow)) then
    begin
      for LColIndex := 0 to ColCount - 1 do
      begin
        LTemp := Cells[LColIndex, ARow];
        Cells[LColIndex, ARow] := Cells[LColIndex, ARow-1];
        Cells[LColIndex, ARow-1] := LTemp;
      end;
      Invalidate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.MoveRowDown (ARow : Integer);
const OPNAME = 'TWRMFGrid.MoveRowDown';
var
  LColIndex : Integer;
  LTemp     : String;
begin
  try
    if (FCellsInfo.MoveRowDown(ARow)) then
    begin
      for LColIndex := 0 to ColCount - 1 do
      begin
        LTemp := Cells[LColIndex, ARow];
        Cells[LColIndex, ARow] := Cells[LColIndex, ARow+1];
        Cells[LColIndex, ARow+1] := LTemp;
      end;
      Invalidate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.SetCellsInfo (AValue : TCellInfoCollection);
const OPNAME = 'TWRMFGrid.SetCellsInfo';
begin
  try
    FCellsInfo.Assign(AValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFGrid.GetCellInfo (ARow : Integer; ACol : Integer) : TCellInfo;
const OPNAME = 'TWRMFGrid.GetCellInfo';
begin
  Result := nil;
  try
    Result := FCellsInfo.GetCellInfo(ARow, ACol);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TWRMFGrid.DrawCell';
  procedure WriteText (StringGrid : TStringGrid; ACanvas: TCanvas; const ARect: TRect; const Text: string; AAlignment: TAlignment);
  const OPNAME = 'WriteText';
        DX = 2;
        DY = 2;
  var
    S: array[0..255] of Char;
  begin
    with StringGrid, ACanvas, ARect do
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
var
  LOldBrushColor : TColor;
  LNewBrushColor : TColor;
  LOldPenColor   : TColor;
  LNewPenColor   : TColor;
  LLeft          : integer;
  LTop           : Integer;
  LHeight        : Integer;
  LNewRect       : TRect;
  LCellInfo      : TCellInfo;
begin
  try
    if ((ACol < FixedCols) OR (ARow < FixedRows)) then
    begin
      inherited;
      if (ARow < FixedRows) then
      begin
        if WrapHeaderText then
        begin
          LNewRect:= ARect;
          Canvas.FillRect(LNewRect);
          DrawText(Canvas.Handle,pchar(Cells[ACol, ARow]),length(Cells[ACol, ARow]),
                                 LNewRect, DT_CENTER or DT_NOPREFIX OR DT_WORDBREAK ); //OR DT_CALCRECT
        end;
      end;
    end
    else
    begin
      LOldBrushColor := Self.Canvas.Brush.Color;
      LOldPenColor   := Self.Canvas.Pen.Color;

      LCellInfo := FCellsInfo.GetCellInfo(ARow, ACol);

      if (NOT LCellInfo.IsValid) then
        LNewBrushColor := clRed
      else if (NOT LCellInfo.Active) then
        LNewBrushColor := clSilver
      else
        LNewBrushColor := LOldBrushColor;
      try
        Self.Canvas.Brush.Color := LNewBrushColor;
        inherited;
        if (FAlignment <> taLeftJustify) then
        begin
          LNewRect := ARect;
          LNewRect.Right := LNewRect.Right - 10;
          WriteText(Self, Self.Canvas, LNewRect, Self.Cells[ACol, ARow], FAlignment);
        end;
      finally
        Self.Canvas.Brush.Color := LOldBrushColor;
      end;

      if ((ARow >= FCellsInfo.NoOfHeadingRows) AND (ACol >= FCellsInfo.NoOfHeadingCols)) then
      begin
        if (LCellInfo.HasParamChange) then
        begin
          LNewPenColor   := clBlack;
          LNewBrushColor := clLime;
        end
        else
        begin
          LNewPenColor   := clDkGray;
          LNewBrushColor := clSilver;
        end;
        LLeft   := Max((ARect.Right - 10), ARect.Left);
        LTop    := ARect.Top;
        LHeight := Min(10,Trunc((ARect.Bottom - ARect.Top)/2));
        try
          Self.Canvas.Brush.Color := LNewBrushColor;
          Self.Canvas.Pen.Color   := LNewPenColor;
          Self.Canvas.RoundRect(LLeft, LTop, LLeft+10, LTop+LHeight, 4, 4);
        finally
          Self.Canvas.Brush.Color := LOldBrushColor;
          Self.Canvas.Pen.Color   := LOldPenColor;
        end;

        if (LCellInfo.HasMetaData) then
        begin
          LNewPenColor   := clBlack;
          LNewBrushColor := clAqua;
        end
        else
        begin
          LNewPenColor   := clDkGray;
          LNewBrushColor := clSilver;
        end;
        LTop := LTop + LHeight;
        try
          Self.Canvas.Brush.Color := LNewBrushColor;
          Self.Canvas.Pen.Color   := LNewPenColor;
          Self.Canvas.RoundRect(LLeft, LTop, LLeft+10, LTop+LHeight, 4, 4);
        finally
          Self.Canvas.Brush.Color := LOldBrushColor;
          Self.Canvas.Pen.Color   := LOldPenColor;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFGrid.ParamChangeShapeClicked (X, Y: Integer): boolean;
const OPNAME = 'TWRMFGrid.ParamChangeShapeClicked';
var
  LCellRect : TRect;
  LLeft     : Integer;
  LTop      : Integer;
  LHeight   : Integer;
  LWidth    : Integer;
begin
  Result := False;
  try
    LCellRect := CellRect(Self.Col, Self.Row);
    LHeight   := Min(10, Trunc((LCellRect.Bottom - LCellRect.Top)/2));
    LLeft     := Max((LCellRect.Right - 10), LCellRect.Left);
    LTop      := LCellRect.Top;
    LWidth    := 10;
    Result := (X >= 0) AND (Y >= 0) AND
              (X >= LLeft) AND (X <= LLeft+LWidth) AND
              (Y >= LTop) AND (Y <= LTop+LHeight);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFGrid.MetaDataShapeClicked (X, Y: Integer): boolean;
const OPNAME = 'TWRMFGrid.MetaDataShapeClicked';
var
  LCellRect : TRect;
  LLeft     : Integer;
  LTop      : Integer;
  LHeight   : Integer;
  LWidth    : Integer;
begin
  Result := False;
  try
    LCellRect := CellRect(Self.Col, Self.Row);
    LHeight   := Min(10, Trunc((LCellRect.Bottom - LCellRect.Top)/2));
    LLeft     := Max((LCellRect.Right - 10), LCellRect.Left);
    LTop      := LCellRect.Top + LHeight;
    LWidth    := 10;
    Result := (X >= 0) AND (Y >= 0) AND
              (X >= LLeft) AND (X <= LLeft+LWidth) AND
              (Y >= LTop) AND (Y <= LTop+LHeight);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.DoParamChangeIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TWRMFGrid.DoParamChangeIndicatorClicked';
begin
  try
    if Assigned(FOnParamChangeClick) then
      FOnParamChangeClick(self, Col, Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFGrid.DoMetaDataIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TWRMFGrid.DoMetaDataIndicatorClicked';
begin
  try
    if Assigned(FOnMetaDataClick) then
      FOnMetaDataClick(self, Col, Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
