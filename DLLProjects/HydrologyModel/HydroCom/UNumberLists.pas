//
// Contains : Number list classes.
//
unit UNumberLists;


interface


//
// Classes that assist with number list manipulation.
//
type
  TNumberList = class(TObject)
  private
    FNumbers: pointer;
    FCount, FCapacity, FNumberWidth: integer;
  protected
    function Get(AIndex: integer): pointer;
    function CheckIndex(AIndex: integer): boolean;
    procedure Grow;
    procedure SetCapacity(ANewCapacity: integer);
    procedure SetCount(ANewCount: integer);
    function AddNumberPosition: integer;
    procedure QuickSort(ALeftIndex, ARightIndex: integer);

    // Introduced as abstract. Every list must provide an implementation.
    function GetAsExtended(AIndex: integer): extended; virtual; abstract;
    procedure SetFromExtended(AIndex: integer; AValue: extended); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; 
    procedure Sort;
    procedure AddNumbers(const ANumbers: array of const);
    procedure AddNumberList(ANumbers: TNumberList);
    function Total: extended;
    function Average: extended;
    function Minimum: extended;
    function Maximum: extended;
    function Range: extended;
    property Count: integer read FCount write SetCount;
  end;

  // Integer oriented lists.
  TIntList = class(TNumberList)
  protected
    function GetAsInt64(AIndex: integer): Int64; virtual; abstract;
  public
    function GetFirstGapValue: Int64;
  end;

  // Signed integers.
  TShortIntList = class(TIntList) // 1 byte signed integer.
  protected
    function GetNumber(AIndex: integer): ShortInt;
    procedure SetNumber(AIndex: integer; AValue: ShortInt);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
    function GetAsInt64(AIndex: integer): Int64; override;
  public
    constructor Create;
    function Add(ANumber: ShortInt): integer;
    property Number[AIndex: integer]: ShortInt read GetNumber write SetNumber; default;
  end;
  TSmallIntList = class(TIntList) // 2 byte signed integer.
  protected
    function GetNumber(AIndex: integer): SmallInt;
    procedure SetNumber(AIndex: integer; AValue: SmallInt);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
    function GetAsInt64(AIndex: integer): Int64; override;
  public
    constructor Create;
    function Add(ANumber: SmallInt): integer;
    property Number[AIndex: integer]: SmallInt read GetNumber write SetNumber; default;
  end;
  TIntegerList = class(TIntList) // 4 byte signed integer.
  protected
    function GetNumber(AIndex: integer): integer;
    procedure SetNumber(AIndex: integer; AValue: integer);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
    function GetAsInt64(AIndex: integer): Int64; override;
  public
    constructor Create;
    function Add(ANumber: integer): integer;
    property Number[AIndex: integer]: integer read GetNumber write SetNumber; default;
  end;
  TInt64List = class(TIntList) // 8 byte signed integer.
  protected
    function GetNumber(AIndex: integer): Int64;
    procedure SetNumber(AIndex: integer; AValue: Int64);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
    function GetAsInt64(AIndex: integer): Int64; override;
  public
    constructor Create;
    function Add(ANumber: Int64): integer;
    property Number[AIndex: integer]: Int64 read GetNumber write SetNumber; default;
  end;

  // Un-signed integers.
  TByteList = class(TIntList) // 1 byte un-signed integer.
  protected
    function GetNumber(AIndex: integer): byte;
    procedure SetNumber(AIndex: integer; AValue: byte);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
    function GetAsInt64(AIndex: integer): Int64; override;
  public
    constructor Create;
    function Add(ANumber: byte): integer;
    property Number[AIndex: integer]: byte read GetNumber write SetNumber; default;
  end;
  TWordList = class(TIntList) // 2 byte un-signed integer.
  protected
    function GetNumber(AIndex: integer): word;
    procedure SetNumber(AIndex: integer; AValue: word);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
    function GetAsInt64(AIndex: integer): Int64; override;
  public
    constructor Create;
    function Add(ANumber: word): integer;
    property Number[AIndex: integer]: word read GetNumber write SetNumber; default;
  end;
  TLongWordList = class(TIntList) // 4 byte un-signed integer.
  protected
    function GetNumber(AIndex: integer): LongWord;
    procedure SetNumber(AIndex: integer; AValue: LongWord);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
    function GetAsInt64(AIndex: integer): Int64; override;
  public
    constructor Create;
    function Add(ANumber: LongWord): integer;
    property Number[AIndex: integer]: LongWord read GetNumber write SetNumber; default;
  end;

  // Float.
  TSingleList = class(TNumberList) // 4 byte float.
  protected
    function GetNumber(AIndex: integer): single;
    procedure SetNumber(AIndex: integer; AValue: single);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
  public
    constructor Create;
    function Add(ANumber: single): integer;
    property Number[AIndex: integer]: single read GetNumber write SetNumber; default;
  end;
  TDoubleList = class(TNumberList) // 8 byte float.
  protected
    function GetNumber(AIndex: integer): double;
    procedure SetNumber(AIndex: integer; AValue: double);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
  public
    constructor Create;
    function Add(ANumber: double): integer;
    property Number[AIndex: integer]: double read GetNumber write SetNumber; default;
  end;
  TExtendedList = class(TNumberList) // 10 byte float.
  protected
    function GetNumber(AIndex: integer): extended;
    procedure SetNumber(AIndex: integer; AValue: extended);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
  public
    constructor Create;
    function Add(ANumber: extended): integer;
    property Number[AIndex: integer]: extended read GetNumber write SetNumber; default;
  end;

  // Currency.
  TCurrencyList = class(TNumberList) // 8 byte float that is stored as integer.
  protected
    function GetNumber(AIndex: integer): currency;
    procedure SetNumber(AIndex: integer; AValue: currency);
    function GetAsExtended(AIndex: integer): extended; override;
    procedure SetFromExtended(AIndex: integer; AValue: extended); override;
  public
    constructor Create;
    function Add(ANumber: currency): integer;
    property Number[AIndex: integer]: currency read GetNumber write SetNumber; default;
  end;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils, Windows, Math;


//
// Implementation constants
//
const
  MaxListSize = Maxint div 16;


{ TNumberList }


constructor TNumberList.Create;
begin
  inherited Create;
  FNumbers := nil;
  FCount := 0;
  FCapacity := 0;
  FNumberWidth := 0;
end;

destructor TNumberList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TNumberList.Get(AIndex: integer): pointer;
begin
  Result := nil;
  if CheckIndex(AIndex) then
    Result := pointer(  integer(@(FNumbers^)) + AIndex * FNumberWidth  );
end;

function TNumberList.CheckIndex(AIndex: integer): boolean;
begin
  Result := (not((AIndex < 0) or (AIndex >= FCount)));
  if (not Result) then
    raise Exception.CreateFmt('List index out of bounds [%d].', [AIndex]);
end;

procedure TNumberList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TNumberList.Sort;
begin
  if (Count > 0) then
    QuickSort(0, Count - 1);
end;

procedure TNumberList.AddNumbers(const ANumbers: array of const);
var
  LThisIndex, LMainIndex: integer;
  LNumber: extended;
  LArgumentType: string;
begin
  for LThisIndex := Low(ANumbers) to High(ANumbers) do
  begin
    LMainIndex := AddNumberPosition;
    case ANumbers[LThisIndex].VType of
      vtInteger:    LNumber := ANumbers[LThisIndex].VInteger;
      vtInt64:      LNumber := ANumbers[LThisIndex].VInt64^;
      vtExtended:   LNumber := ANumbers[LThisIndex].VExtended^;
      vtCurrency:   LNumber := ANumbers[LThisIndex].VCurrency^;
    else
      case ANumbers[LThisIndex].VType of
        vtInteger    : LArgumentType := 'Integer';
        vtBoolean    : LargumentType := 'Boolean';
        vtChar       : LargumentType := 'Char';
        vtExtended   : LargumentType := 'Extended';
        vtString     : LargumentType := 'String';
        vtPointer    : LargumentType := 'Pointer';
        vtPChar      : LargumentType := 'PChar';
        vtObject     : LargumentType := 'Object';
        vtClass      : LargumentType := 'Class';
        vtWideChar   : LargumentType := 'WideChar';
        vtPWideChar  : LargumentType := 'PWideChar';
        vtAnsiString : LargumentType := 'AnsiString';
        vtCurrency   : LargumentType := 'Currency';
        vtVariant    : LargumentType := 'Variant';
        vtInterface  : LargumentType := 'Interface';
        vtWideString : LargumentType := 'WideString';
        vtInt64      : LargumentType := 'Int64';
      else
        raise Exception.CreateFmt('Unknown argument type [%d].', [integer(ANumbers[LThisIndex].VType)]);
      end;
      raise Exception.CreateFmt('Argument type [%s] can not be added to this number list.', [LargumentType]);
    end;
    SetFromExtended(LMainIndex, LNumber);
  end;
end;

procedure TNumberList.AddNumberList(ANumbers: TNumberList);
var LDestination, LSource: pointer;
begin
  if (ANumbers.Count > 0) then
  begin
    SetCapacity(Count + ANumbers.Count);
    LDestination := pointer(  integer(@(FNumbers^)) + Count * FNumberWidth  );
    LSource := ANumbers.Get(0);
    CopyMemory(LDestination, LSource, ANumbers.Count * ANumbers.FNumberWidth);
    FCount := Count + ANumbers.Count;
  end;
end;

procedure TNumberList.Grow;
var Delta: integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TNumberList.SetCapacity(ANewCapacity: Integer);
begin
  if (ANewCapacity < FCount) or (ANewCapacity > MaxListSize) then
    raise Exception.CreateFmt('Could not set list capacity to [%d].', [ANewCapacity]);
  if (ANewCapacity <> FCapacity) then
  begin
    ReallocMem(FNumbers, ANewCapacity * FNumberWidth);
    FCapacity := ANewCapacity;
  end;
end;

procedure TNumberList.SetCount(ANewCount: Integer);
begin
  if (ANewCount < 0) or (ANewCount > MaxListSize) then
    raise Exception.CreateFmt('Could not set list count to [%d].', [ANewCount]);
  if (ANewCount > FCapacity) then
    SetCapacity(ANewCount);
  if (ANewCount > FCount) then
    FillChar(Get(FCount)^, (ANewCount - FCount) * FNumberWidth, 0);
  FCount := ANewCount;
end;

function TNumberList.AddNumberPosition: integer;
begin
  Result := FCount;
  if (Result = FCapacity) then
    Grow;
  Inc(FCount);
end;

procedure TNumberList.QuickSort(ALeftIndex, ARightIndex: integer);
var
  I, J: integer;
  P, T: extended;
begin
  repeat
    I := ALeftIndex;
    J := ARightIndex;
    P := GetAsExtended((ALeftIndex + ARightIndex) shr 1);
    repeat
      while (GetAsExtended(I) > P) do
        Inc(I);
      while (GetAsExtended(J) < P) do
        Dec(J);
      if (I <= J) then
      begin
        T := GetAsExtended(I);
        SetFromExtended(I, GetAsExtended(J));
        SetFromExtended(J, T);
        Inc(I);
        Dec(J);
      end;
    until (I > J);
    if (ALeftIndex < J) then
      QuickSort(ALeftIndex, J);
    ALeftIndex := I;
  until (I >= ARightIndex);
end;

function TNumberList.Total: extended;
var LIndex: integer;
begin
  Result := 0.0;
  for LIndex := 0 to FCount - 1 do
    Result := Result + GetAsExtended(LIndex);
end;

function TNumberList.Average: extended;
begin
  Result := Total / FCount;
end;

function TNumberList.Minimum: extended;
var LIndex: integer;
begin
  Result := 0.0;
  if (FCount > 0) then
    Result := GetAsExtended(0);
  for LIndex := 1 to FCount - 1 do
    if (GetAsExtended(LIndex) < Result) then
      Result := GetAsExtended(LIndex);
end;

function TNumberList.Maximum: extended;
var LIndex: integer;
begin
  Result := 0.0;
  if (FCount > 0) then
    Result := GetAsExtended(0);
  for LIndex := 0 to FCount - 1 do
    if (GetAsExtended(LIndex) > Result) then
      Result := GetAsExtended(LIndex);
end;

function TNumberList.Range: extended;
begin
  Result := Maximum / Minimum;
end;


{ TIntList }


function TIntList.GetFirstGapValue: Int64;
var
  LIndex: integer;
  LGapValue: Int64;
begin
  Result := -1;
  if (Count > 0) then
  begin
    Sort;
    for LIndex := 1 to Count do
    begin
      LGapValue := GetAsInt64(LIndex - 1) + 1;
      if (LIndex >= Count) then
      begin
        Result := LGapValue;
        break;
      end else begin
        if (GetAsInt64(LIndex) <> LGapValue) then
        begin
          Result := LGapValue;
          break;
        end;
      end;
    end;
  end;
end;


{ TShortIntList }

constructor TShortIntList.Create;                                     begin inherited Create; FNumberWidth := 1; end;
function TShortIntList.GetNumber(AIndex: integer): ShortInt;          begin Result := 0; if CheckIndex(AIndex) then Result := ShortInt(Get(AIndex)^); end;
procedure TShortIntList.SetNumber(AIndex: integer; AValue: ShortInt); begin              if CheckIndex(AIndex) then ShortInt(Get(AIndex)^) := AValue; end;
function TShortIntList.GetAsExtended(AIndex: integer): extended;      begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TShortIntList.SetFromExtended(AIndex: integer; AValue: extended); begin        if CheckIndex(AIndex) then Number[AIndex] := Trunc(AValue); end;
function TShortIntList.Add(ANumber: ShortInt): integer;               begin Result := AddNumberPosition; Number[Result] := ANumber; end;
function TShortIntList.GetAsInt64(AIndex: integer): Int64;            begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;

{ TSmallIntList }

constructor TSmallIntList.Create;                                     begin inherited Create; FNumberWidth := 2; end;
function TSmallIntList.GetNumber(AIndex: integer): SmallInt;          begin Result := 0; if CheckIndex(AIndex) then Result := SmallInt(Get(AIndex)^); end;
procedure TSmallIntList.SetNumber(AIndex: integer; AValue: SmallInt); begin              if CheckIndex(AIndex) then SmallInt(Get(AIndex)^) := AValue; end;
function TSmallIntList.GetAsExtended(AIndex: integer): extended;      begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TSmallIntList.SetFromExtended(AIndex: integer; AValue: extended); begin        if CheckIndex(AIndex) then Number[AIndex] := Trunc(AValue); end;
function TSmallIntList.Add(ANumber: SmallInt): integer;               begin Result := AddNumberPosition; Number[Result] := ANumber; end;
function TSmallIntList.GetAsInt64(AIndex: integer): Int64;            begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;

{ TIntegerList }

constructor TIntegerList.Create;                                      begin inherited Create; FNumberWidth := 4; end;
function TIntegerList.GetNumber(AIndex: integer): integer;            begin Result := 0; if CheckIndex(AIndex) then Result := integer(Get(AIndex)^); end;
procedure TIntegerList.SetNumber(AIndex: integer; AValue: integer);   begin              if CheckIndex(AIndex) then integer(Get(AIndex)^) := AValue; end;
function TIntegerList.GetAsExtended(AIndex: integer): extended;       begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TIntegerList.SetFromExtended(AIndex: integer; AValue: extended); begin         if CheckIndex(AIndex) then Number[AIndex] := Trunc(AValue); end;
function TIntegerList.Add(ANumber: integer): integer;                 begin Result := AddNumberPosition; Number[Result] := ANumber; end;
function TIntegerList.GetAsInt64(AIndex: integer): Int64;            begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;

{ TInt64List }

constructor TInt64List.Create;                                        begin inherited Create; FNumberWidth := 8; end;
function TInt64List.GetNumber(AIndex: integer): Int64;                begin Result := 0; if CheckIndex(AIndex) then Result := Int64(Get(AIndex)^); end;
procedure TInt64List.SetNumber(AIndex: integer; AValue: Int64);       begin              if CheckIndex(AIndex) then Int64(Get(AIndex)^) := AValue; end;
function TInt64List.GetAsExtended(AIndex: integer): extended;         begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TInt64List.SetFromExtended(AIndex: integer; AValue: extended); begin           if CheckIndex(AIndex) then Number[AIndex] := Trunc(AValue); end;
function TInt64List.Add(ANumber: Int64): integer;                     begin Result := AddNumberPosition; Number[Result] := ANumber; end;
function TInt64List.GetAsInt64(AIndex: integer): Int64;            begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;

{ TByteList }

constructor TByteList.Create;                                         begin inherited Create; FNumberWidth := 1; end;
function TByteList.GetNumber(AIndex: integer): byte;                  begin Result := 0; if CheckIndex(AIndex) then Result := byte(Get(AIndex)^); end;
procedure TByteList.SetNumber(AIndex: integer; AValue: byte);         begin              if CheckIndex(AIndex) then byte(Get(AIndex)^) := AValue; end;
function TByteList.GetAsExtended(AIndex: integer): extended;          begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TByteList.SetFromExtended(AIndex: integer; AValue: extended); begin            if CheckIndex(AIndex) then Number[AIndex] := Trunc(AValue); end;
function TByteList.Add(ANumber: byte): integer;                       begin Result := AddNumberPosition; Number[Result] := ANumber; end;
function TByteList.GetAsInt64(AIndex: integer): Int64;            begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;

{ TWordList }

constructor TWordList.Create;                                         begin inherited Create; FNumberWidth := 2; end;
function TWordList.GetNumber(AIndex: integer): word;                  begin Result := 0; if CheckIndex(AIndex) then Result := word(Get(AIndex)^); end;
procedure TWordList.SetNumber(AIndex: integer; AValue: word);         begin              if CheckIndex(AIndex) then word(Get(AIndex)^) := AValue; end;
function TWordList.GetAsExtended(AIndex: integer): extended;          begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TWordList.SetFromExtended(AIndex: integer; AValue: extended); begin            if CheckIndex(AIndex) then Number[AIndex] := Trunc(AValue); end;
function TWordList.Add(ANumber: word): integer;                       begin Result := AddNumberPosition; Number[Result] := ANumber; end;
function TWordList.GetAsInt64(AIndex: integer): Int64;                begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;

{ TLongWordList }

constructor TLongWordList.Create;                                     begin inherited Create; FNumberWidth := 4; end;
function TLongWordList.GetNumber(AIndex: integer): LongWord;          begin Result := 0; if CheckIndex(AIndex) then Result := LongWord(Get(AIndex)^); end;
procedure TLongWordList.SetNumber(AIndex: integer; AValue: LongWord); begin              if CheckIndex(AIndex) then LongWord(Get(AIndex)^) := AValue; end;
function TLongWordList.GetAsExtended(AIndex: integer): extended;      begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TLongWordList.SetFromExtended(AIndex: integer; AValue: extended); begin        if CheckIndex(AIndex) then Number[AIndex] := Trunc(AValue); end;
function TLongWordList.Add(ANumber: LongWord): integer;               begin Result := AddNumberPosition; Number[Result] := ANumber; end;
function TLongWordList.GetAsInt64(AIndex: integer): Int64;            begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;

{ TSingleList }

constructor TSingleList.Create;                                       begin inherited Create; FNumberWidth := 4; end;
function TSingleList.GetNumber(AIndex: integer): single;              begin Result := 0; if CheckIndex(AIndex) then Result := single(Get(AIndex)^); end;
procedure TSingleList.SetNumber(AIndex: integer; AValue: single);     begin              if CheckIndex(AIndex) then single(Get(AIndex)^) := AValue; end;
function TSingleList.GetAsExtended(AIndex: integer): extended;        begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TSingleList.SetFromExtended(AIndex: integer; AValue: extended); begin          if CheckIndex(AIndex) then Number[AIndex] := AValue; end;
function TSingleList.Add(ANumber: single): integer;                   begin Result := AddNumberPosition; Number[Result] := ANumber; end;

{ TDoubleList }

constructor TDoubleList.Create;                                       begin inherited Create; FNumberWidth := 8; end;
function TDoubleList.GetNumber(AIndex: integer): double;              begin Result := 0; if CheckIndex(AIndex) then Result := double(Get(AIndex)^); end;
procedure TDoubleList.SetNumber(AIndex: integer; AValue: double);     begin              if CheckIndex(AIndex) then double(Get(AIndex)^) := AValue; end;
function TDoubleList.GetAsExtended(AIndex: integer): extended;        begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TDoubleList.SetFromExtended(AIndex: integer; AValue: extended); begin          if CheckIndex(AIndex) then Number[AIndex] := AValue; end;
function TDoubleList.Add(ANumber: double): integer;                   begin Result := AddNumberPosition; Number[Result] := ANumber; end;

{ TExtendedList }

constructor TExtendedList.Create;                                     begin inherited Create; FNumberWidth := 10; end;
function TExtendedList.GetNumber(AIndex: integer): extended;          begin Result := 0; if CheckIndex(AIndex) then Result := extended(Get(AIndex)^); end;
procedure TExtendedList.SetNumber(AIndex: integer; AValue: extended); begin              if CheckIndex(AIndex) then extended(Get(AIndex)^) := AValue; end;
function TExtendedList.GetAsExtended(AIndex: integer): extended;      begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TExtendedList.SetFromExtended(AIndex: integer; AValue: extended); begin        if CheckIndex(AIndex) then Number[AIndex] := AValue; end;
function TExtendedList.Add(ANumber: extended): integer;               begin Result := AddNumberPosition; Number[Result] := ANumber; end;

{ TCurrencyList }

constructor TCurrencyList.Create;                                     begin inherited Create; FNumberWidth := 8; end;
function TCurrencyList.GetNumber(AIndex: integer): currency;          begin Result := 0; if CheckIndex(AIndex) then Result := currency(Get(AIndex)^); end;
procedure TCurrencyList.SetNumber(AIndex: integer; AValue: currency); begin              if CheckIndex(AIndex) then currency(Get(AIndex)^) := AValue; end;
function TCurrencyList.GetAsExtended(AIndex: integer): extended;      begin Result := 0; if CheckIndex(AIndex) then Result := Number[AIndex]; end;
procedure TCurrencyList.SetFromExtended(AIndex: integer; AValue: extended); begin        if CheckIndex(AIndex) then Number[AIndex] := AValue; end;
function TCurrencyList.Add(ANumber: currency): integer;               begin Result := AddNumberPosition; Number[Result] := ANumber; end;


end.