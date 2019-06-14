//
//  Contains : Class TFileReader
//
unit UFileReader;


interface


//
// Interface dependencies
//
uses

  // Delphi
  Classes;


//
// A class that reads a file one variable at a time.
//
type
  TSetDoubleFunction = procedure(AIndexes: array of integer; AValue: double) of object;
  TFileReader = class(TObject)
  protected
    FFileName: string;
    FFileData: TStringList;
    FFileLoaded: boolean;
    FLineIndex, FCharIndex: integer;

    // Block extraction.
    function ExtractBlock(ALength: integer; var ABlock: string; AAllowLineChange, ARaiseException: boolean; AName, AType: string): boolean; overload;
    function ExtractBlock(var ABlock: string): boolean; overload;
  public

    // Construction, destruction.
    constructor Create;
    destructor Destroy; override;

    // Initialisation.
    procedure Clear;
    procedure Reset;
    function LoadFromFile(AFileName: string; AMustExist: boolean = True): boolean;
    function LoadFromStrings(AFileData: TStringList): boolean;
    function HasData: boolean;

    // File reading.
    function LineCount: integer;
    function CurrentLineLength: integer;
    function CurrentLineNumber: integer;
    function CurrentLineRemainingCharacters: integer;
    procedure NextLine;
    procedure PreviousLine;
    procedure GoBack(ALength: integer);
    procedure Read(ALength: integer); overload;
    procedure Read(var AValue: integer; AName: string); overload;
    procedure Read(var AValue: integer; AName: string; AIndexes: array of const); overload;
    procedure Read(var AValue: integer; ALength: integer; AName: string; AAllowLineChange: boolean = False); overload;
    procedure Read(var AValue: integer; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False); overload;
    procedure Read(var AValue: array of integer; ALength: integer; AName: string; AAllowLineChange: boolean = False); overload;
    procedure Read(var AValue: array of integer; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False); overload;
    procedure Read(var AValue: array of char; AName: string); overload;
    procedure Read(var AValue: array of char; AName: string; AIndexes: array of const); overload;
    procedure ReadLine(var AValue: array of char; AName: string); overload;
    procedure ReadLine(var AValue: array of char; AName: string; AIndexes: array of const); overload;
    procedure Read(var AValue: char; AName: string); overload;
    procedure Read(var AValue: char; AName: string; AIndexes: array of const); overload;
    procedure Read(var AValue: double; AName: string); overload;
    procedure Read(var AValue: double; AName: string; AIndexes: array of const); overload;
    procedure ReadFlagged(var AValue: double; AFlag : String; AName: string; AIndexes: array of const); overload;
    procedure Read(var AValue: double; ALength: integer; AName: string; AAllowLineChange: boolean = False); overload;
    procedure Read(var AValue: double; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False); overload;
    procedure ReadLine(var AValue: array of double; ALength: integer; AName: string); overload;
    procedure ReadLine(var AValue: array of double; ALength: integer; AName: string; AIndexes: array of const); overload;
    procedure ReadLine(ASetter: TSetDoubleFunction; AFixedIndexes: array of integer; AIndexToVary, ACount, ALength: integer; AName: string); overload;
    procedure ReadLine(ASetter: TSetDoubleFunction; AFixedIndexes: array of integer; AIndexToVary, ACount, ALength: integer; AName: string; AIndexes: array of const); overload;
    procedure Read(var AValue: array of double; ALength: integer; AName: string; AAllowLineChange: boolean = False); overload;
    procedure Read(var AValue: array of double; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False); overload;
    procedure ReadLine(var AValue: array of integer; ALength: integer; AName: string); overload;
    procedure ReadLine(var AValue: array of integer; ALength: integer; AName: string; AIndexes: array of const); overload;
    procedure Read(var AValue: string; ALength: integer; AName: string); overload;
    procedure Read(var AValue: string; ALength: integer; AName: string; AIndexes: array of const); overload;
    procedure Read(var AValue: string; AName: string); overload;
    procedure Read(var AValue: string; AName: string; AIndexes: array of const); overload;

    property FileName: string read FFileName;
  end;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils,

  // Alborak
  UErrorHandlingOperations;


//
// Constructor.
//
constructor TFileReader.Create;
const OPNAME = 'TFileReader.Create';
begin
  try
    inherited Create;
    FFileName := '';
    FFileLoaded := False;
    FFileData := TStringList.Create;
    FLineIndex := 0;
    FCharIndex := 1;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Destructor.
//
destructor TFileReader.Destroy;
const OPNAME = 'TFileReader.Destroy';
begin
  try
    Clear;
    FreeAndNil(FFileData);
    inherited Destroy;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Discards the data from memory.
//
procedure TFileReader.Clear;
const OPNAME = 'TFileReader.Clear';
begin
  try
    FFileName := '';
    FFileLoaded := False;
    FFileData.Clear;
    Reset;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Resets the line and character pointers without clearing the file data.
//
procedure TFileReader.Reset;
const OPNAME = 'TFileReader.Reset';
begin
  try
    FLineIndex := 0;
    FCharIndex := 1;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Loads the raw file data into the buffer.
//
function TFileReader.LoadFromFile(AFileName: string; AMustExist: boolean = True): boolean;
const OPNAME = 'TFileReader.LoadFromFile';
begin
  Result := False;
  try
    Clear;

    // Attempt to load the file.
    if FileExists(AFileName) or AMustExist then
      FFileData.LoadFromFile(AFileName);

    // Set the member properties if the load succeeded.
    FFileName := AFileName;
    FFileLoaded := True;
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Loads the passed file data into the buffer.
//
function TFileReader.LoadFromStrings(AFileData: TStringList): boolean;
const OPNAME = 'TFileReader.LoadFromStrings';
begin
  Result := False;
  try
    Clear;
    FFileData.AddStrings(AFileData);
    FFileName := '';
    FFileLoaded := True;
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns true if any line has any data.
//
function TFileReader.HasData: boolean;
const OPNAME = 'TFileReader.HasData';
var LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FFileData.Count - 1 do
    begin
      if (Trim(FFileData[LIndex]) <> '') then
      begin
        Result := True;
        break;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the number of lines in the file.
//
function TFileReader.LineCount: integer;
const OPNAME = 'TFileReader.LineCount';
begin
  Result := 0;
  try
    Result := FFileData.Count;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Moves to the start of the next line.
//
function TFileReader.CurrentLineLength: integer;
const OPNAME = 'TFileReader.CurrentLineLength';
begin
  Result := 0;
  try
    if (FLineIndex < FFileData.Count) then
      Result := Length(FFileData[FLineIndex]);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the index of the current line.
//
function TFileReader.CurrentLineNumber: integer;
const OPNAME = 'TFileReader.CurrentLineNumber';
begin
  Result := 0;
  try
    if (FLineIndex > 0) then
    begin
      if (FLineIndex < FFileData.Count) then
      begin
        Result := FLineIndex + 1;
      end else begin
        Result := FFileData.Count;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Returns the number of characters left in the current line.
//
function TFileReader.CurrentLineRemainingCharacters: integer;
const OPNAME = 'TFileReader.CurrentLineRemainingCharacters';
begin
  Result := 0;
  try
    if (FLineIndex < FFileData.Count) then
      Result := (Length(FFileData[FLineIndex]) - FCharIndex + 1);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Moves to the start of the next line.
//
procedure TFileReader.NextLine;
const OPNAME = 'TFileReader.NextLine';
begin
  try
    FLineIndex := FLineIndex + 1;
    FCharIndex := 1;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Moves to the end of the previous line.
//
procedure TFileReader.PreviousLine;
const OPNAME = 'TFileReader.PreviousLine';
begin
  try
    if (FLineIndex <= 0) then
    begin
      FLineIndex := 0;
      FCharIndex := 1;
    end else begin
      FLineIndex := FLineIndex - 1;
      FCharIndex := Length(FFileData[FLineIndex]);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Extracts a block of text.
//
function TFileReader.ExtractBlock(ALength: integer; var ABlock: string; AAllowLineChange, ARaiseException: boolean; AName, AType: string): boolean;
const OPNAME = 'TFileReader.ExtractBlock';
begin
  Result := False;
  ABlock := '';

  // Check that there is enough space on the current line.
  if AAllowLineChange then
  begin

    // Skip empty lines or if at the end of the current lines.
    while (FLineIndex < FFileData.Count) and (               // No more lines.
            (Length(FFileData[FLineIndex]) < FCharIndex) or  // Empty line
            (Trim(Copy(FFileData[FLineIndex], FCharIndex, Length(FFileData[FLineIndex]))) = '') // Only spaces left.
          ) do
      NextLine;
  end;

  // Make sure that there is enough space.
  if (FLineIndex < FFileData.Count) and (FCharIndex <= Length(FFileData[FLineIndex])) then
  begin

    // Extract the block.
    ABlock := Copy(FFileData[FLineIndex], FCharIndex, ALength);

    // Advance the pointers.
    FCharIndex := FCharIndex + ALength;

    // Return True if the block contains some data.
    if (Length(ABlock) > 0) then
      Result := True;
  end;

  // Raise an exception if the block could not be extracted.
  if ARaiseException and (not Result) and (AName <> '') then
    raise Exception.CreateFmt('Not enough characters left on line [%d, %d] of file [%s] to read %s [%s]. ',
      [FLineIndex, FCharIndex, ExtractFileName(FFileName), AType, AName]);
end;


//
// Extracts a block of text using the free format method.
//
function TFileReader.ExtractBlock(var ABlock: string): boolean;
const OPNAME = 'TFileReader.ExtractBlock';
begin
  Result := False;
  ABlock := '';

  // Make sure there are enough lines.
  if (FLineIndex < FFileData.Count) then
  begin

    // Discard all characters that do not contain data.
    while ((FCharIndex <= Length(FFileData[FLineIndex])) and (FFileData[FLineIndex][FCharIndex] = ' ')) do
      Inc(FCharIndex);

    // Extract all characters until the next space or quote.
    if (FCharIndex <= Length(FFileData[FLineIndex])) then
    begin
      if (FFileData[FLineIndex][FCharIndex] = '"') or (FFileData[FLineIndex][FCharIndex] = '''') then
      begin
        Inc(FCharIndex);
        while ((FCharIndex <= Length(FFileData[FLineIndex])) and
              ((FFileData[FLineIndex][FCharIndex] <> '"') and (FFileData[FLineIndex][FCharIndex] <> ''''))) do
        begin
          ABlock := ABlock + FFileData[FLineIndex][FCharIndex];
          Inc(FCharIndex);
        end;
      end else begin
        while ((FCharIndex <= Length(FFileData[FLineIndex])) and (FFileData[FLineIndex][FCharIndex] <> ' ')) do
        begin
          ABlock := ABlock + FFileData[FLineIndex][FCharIndex];
          Inc(FCharIndex);
        end;
      end;
      ABlock := Trim(ABlock);
    end;

    // Move to the next character.
    if (FCharIndex <= Length(FFileData[FLineIndex])) then
      Inc(FCharIndex);

    // Return true if there is data.
    if (ABlock <> '') then
      Result := True;
  end;
end;


//
// Reads and discards the required number of characters.
//
procedure TFileReader.Read(ALength: integer);
const OPNAME = 'TFileReader.Read.Ignore';
var LBlock: string;
begin
  try
    ExtractBlock(ALength, LBlock, False, False, '', '');

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Goeas back a set number of characters.
//
procedure TFileReader.GoBack(ALength: integer);
const OPNAME = 'TFileReader.GoBack';
var LCharacterCount: integer;
begin
  try
    LCharacterCount := 0;
    while (LCharacterCount < ALength) do
    begin
      Inc(LCharacterCount);
      if (FCharIndex <= 1) then
      begin
        PreviousLine;
      end else begin
        Dec(FCharIndex);
      end;
    end;
    if (FCharIndex = 1) then
      PreviousLine;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


//
// Extracts an integer with the free format method. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: integer; AName: string); begin Read(AValue, AName, []); end;
procedure TFileReader.Read(var AValue: integer; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadFreeFormat.Integer';
var LBlock: string;
begin
  AValue := 0;
  AName := Format(AName, AIndexes);
  if ExtractBlock(LBlock) then
  begin
    try
      AValue := StrToInt(LBlock);
    except on E: Exception do
      raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to integer [%s]. ',
        [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);
    end;
  end;
end;


//
// Extracts an integer. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: integer; ALength: integer; AName: string; AAllowLineChange: boolean = False); begin Read(AValue, ALength, AName, [], AAllowLineChange) end;
procedure TFileReader.Read(var AValue: integer; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False);
const OPNAME = 'TFileReader.Read.Integer';
var LBlock: string;
begin
  AValue := 0;
  AName := Format(AName, AIndexes);
  if ExtractBlock(ALength, LBlock, AAllowLineChange, True, AName, 'integer') then
  begin
    LBlock := Trim(LBlock);
    try
      AValue := StrToInt(LBlock);
    except on E: Exception do
      raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to integer [%s]. ',
        [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);
    end;
  end;
end;


//
// Extracts an array of integers. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: array of integer; ALength: integer; AName: string; AAllowLineChange: boolean = False); begin Read(AValue, ALength, AName, [], AAllowLineChange) end;
procedure TFileReader.Read(var AValue: array of integer; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False);
const OPNAME = 'TFileReader.Read.IntegerArray';
var
  LBlock: string;
  LValueIndex: integer;
begin
  AName := Format(AName, AIndexes);
  for LValueIndex := Low(AValue) to High(AValue) do
  begin
    AValue[LValueIndex] := 0;
    if ExtractBlock(ALength, LBlock, AAllowLineChange, True, AName, 'integer') then
    begin
      LBlock := Trim(LBlock);
      try
        AValue[LValueIndex] := StrToInt(LBlock);
      except on E: Exception do
        raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to integer [%s]. ',
          [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);
      end;
    end;
  end;
end;


//
// Extracts a character array. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: array of char; AName: string); begin Read(AValue, AName, []) end;
procedure TFileReader.Read(var AValue: array of char; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.Read.String';
var
  LBlock: string;
  LCharIndex: integer;
begin
  AName := Format(AName, AIndexes);
  if ExtractBlock(Length(AValue), LBlock, False, True, AName, 'character array') then
  begin
    for LCharIndex := 0 to Length(AValue) - 1 do
    begin
      if (LCharIndex < Length(LBlock)) then
      begin
        AValue[LCharIndex] := LBlock[LCharIndex + 1];
      end else begin
        AValue[LCharIndex] := ' ';
      end;
    end;
  end;
end;


//
// Extracts a character array. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: char; AName: string); begin Read(AValue, AName, []) end;
procedure TFileReader.Read(var AValue: char; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.Read.Char';
var LBlock: string;
begin
  AName := Format(AName, AIndexes);
  if ExtractBlock(1, LBlock, False, True, AName, 'character') then
  begin
    if (Length(LBlock) > 0) then
    begin
      AValue := LBlock[1];
    end else begin
      raise Exception.CreateFmt('Could not extract a character [%s] from line [%d, %d] of file [%s]. ',
        [AName, FLineIndex, FCharIndex, ExtractFileName(FFileName)]);
    end;
  end;
end;


//
// Extracts a double with the free format method. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: double; AName: string); begin Read(AValue, AName, []); end;
procedure TFileReader.Read(var AValue: double; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadFreeFormat.Integer';
var
  LBlock: string;
  LNumber: double;
  LCode: integer;
begin
  AValue := 0.0;
  AName := Format(AName, AIndexes);
  if ExtractBlock(LBlock) then
  begin

    // Check if there is data after the trim. Assume that this means 0.0.
    if (Length(LBlock) > 0) then
    begin

      // Complain if any offending characters are located.
      Val(LBlock, LNumber, LCode);
      if (LCode > 0) then
        raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to double [%s]. ',
          [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);

      // Set the result.
      AValue := LNumber;
    end;
  end;
end;

procedure TFileReader.ReadFlagged(var AValue: double; AFlag : String; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadFlagged.Integer';
var
  LBlock: string;
  LNumber: double;
  LCode: integer;
  LFlag : string;
begin
  AValue := 0.0;
  AFlag  := '';
  AName := Format(AName, AIndexes);
  if ExtractBlock(LBlock) then
  begin

    // Check if there is data after the trim. Assume that this means 0.0.
    if (Length(LBlock) > 0) then
    begin
      LFlag := Copy(LBlock, Length(LBlock), 1);
      try
        StrToFloat(LFlag);
      except
        on EConvertError do
        begin
          AFlag := LFlag;
          LBlock := Copy(LBlock, 1, Length(LBlock) - 1);
        end;
      end;

      // Complain if any offending characters are located.
      Val(LBlock, LNumber, LCode);
      if (LCode > 0) then
        raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to double [%s]. ',
          [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);

      // Set the result.
      AValue := LNumber;
    end;
  end;
end;


//
// Extracts a double. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: double; ALength: integer; AName: string; AAllowLineChange: boolean = False); begin Read(AValue, ALength, AName, [], AAllowLineChange) end;
procedure TFileReader.Read(var AValue: double; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False);
const OPNAME = 'TFileReader.Read.Double';
var
  LBlock: string;
  LNumber: double;
  LCode: integer;
begin
  AValue := 0.0;
  AName := Format(AName, AIndexes);
  if ExtractBlock(ALength, LBlock, AAllowLineChange, True, AName, 'double') then
  begin
    LBlock := Trim(LBlock);

    // Check if there is data after the trim. Assume that this means 0.0.
    if (Length(LBlock) > 0) then
    begin

      // Complain if any offending characters are located.
      Val(LBlock, LNumber, LCode);
      if (LCode > 0) then
        raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to double [%s]. ',
          [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);

      // Set the result.
      AValue := LNumber;
    end;
  end;
end;


//
// Extracts an array of double. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: array of double; ALength: integer; AName: string; AAllowLineChange: boolean = False); begin Read(AValue, ALength, AName, [], AAllowLineChange) end;
procedure TFileReader.Read(var AValue: array of double; ALength: integer; AName: string; AIndexes: array of const; AAllowLineChange: boolean = False);
const OPNAME = 'TFileReader.Read.DoubleArray';
var
  LBlock: string;
  LNumber: double;
  LValueIndex, LCode: integer;
begin
  AName := Format(AName, AIndexes);
  for LValueIndex := Low(AValue) to High(AValue) do
  begin
    AValue[LValueIndex] := 0.0;
    if ExtractBlock(ALength, LBlock, AAllowLineChange, True, AName, 'double') then
    begin
      LBlock := Trim(LBlock);

      // Check if there is data after the trim. Assume that this means 0.0.
      if (Length(LBlock) > 0) then
      begin

        // Complain if any offending characters are located.
        Val(LBlock, LNumber, LCode);
        if (LCode > 0) then
          raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to double [%s]. ',
            [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);

        // Set the result.
        AValue[LValueIndex] := LNumber;
      end;
    end;
  end;
end;


//
// Extracts string of a specific length. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: string; ALength: integer; AName: string); begin Read(AValue, ALength, AName, []) end;
procedure TFileReader.Read(var AValue: string; ALength: integer; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.Read.String';
var
  LBlock: string;
  LCharIndex: integer;
begin
  AName := Format(AName, AIndexes);
  AValue := '';
  if ExtractBlock(ALength, LBlock, False, False, AName, 'string') then
  begin
    for LCharIndex := 0 to ALength - 1 do
    begin
      if (LCharIndex < Length(LBlock)) then
      begin
        AValue := AValue + LBlock[LCharIndex + 1];
      end else begin
        AValue := AValue + ' ';
      end;
    end;
  end;
end;


//
// Extracts an integer with the free format method. Raises an exception on error.
//
procedure TFileReader.Read(var AValue: string; AName: string); begin Read(AValue, AName, []); end;
procedure TFileReader.Read(var AValue: string; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadFreeFormat.String';
begin
  ExtractBlock(AValue);
end;


//
// Extracts an array of integers without allowing a line change. Raises an exception on error.
//
procedure TFileReader.ReadLine(var AValue: array of integer; ALength: integer; AName: string); begin ReadLine(AValue, ALength, AName, []) end;
procedure TFileReader.ReadLine(var AValue: array of integer; ALength: integer; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadLine.IntegerArray';
var
  LStartLineNumber, LValueIndex: integer;
  LBlock: string;
begin
  LStartLineNumber := FLineIndex;
  AName := Format(AName, AIndexes);
  for LValueIndex := Low(AValue) to High(AValue) do
  begin
    AValue[LValueIndex] := 0;
    if (not ExtractBlock(ALength, LBlock, False, False, AName, 'integer')) then
    begin
      break;
    end else begin
      if (LStartLineNumber <> FLineIndex) then
      begin
        GoBack(ALength);
        break;
      end else begin
        LBlock := Trim(LBlock);
        try
          AValue[LValueIndex] := StrToInt(LBlock);
        except on E: Exception do
          raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to integer [%s]. ',
            [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);
        end;
      end;
    end;
  end;
  NextLine;
end;


//
// Extracts a character array to the end of the line. Raises an exception on error.
//
procedure TFileReader.ReadLine(var AValue: array of char; AName: string); begin ReadLine(AValue, AName, []) end;
procedure TFileReader.ReadLine(var AValue: array of char; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadLine.String';
var
  LBlock: string;
  LCharIndex: integer;
begin
  AName := Format(AName, AIndexes);
  LBlock := Copy(FFileData[FLineIndex], FCharIndex, Length(FFileData[FLineIndex]));
  for LCharIndex := 0 to Length(AValue) - 1 do
  begin
    if (LCharIndex < Length(LBlock)) then
    begin
      AValue[LCharIndex] := LBlock[LCharIndex + 1];
    end else begin
      AValue[LCharIndex] := ' ';
    end;
  end;
  NextLine;
end;


//
// Extracts an array of doubles without allowing a line change. Raises an exception on error.
//
procedure TFileReader.ReadLine(var AValue: array of double; ALength: integer; AName: string); begin ReadLine(AValue, ALength, AName, []) end;
procedure TFileReader.ReadLine(var AValue: array of double; ALength: integer; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadLine.IntegerArray';
var
  LStartLineNumber, LValueIndex, LCode: integer;
  LNumber: double;
  LBlock: string;
begin
  LStartLineNumber := FLineIndex;
  AName := Format(AName, AIndexes);
  for LValueIndex := Low(AValue) to High(AValue) do
  begin
    AValue[LValueIndex] := 0.0;
    if (not ExtractBlock(ALength, LBlock, False, False, AName, 'double')) then
    begin
      break;
    end else begin
      if (LStartLineNumber <> FLineIndex) then
      begin
        GoBack(ALength);
        break;
      end else begin

        // Complain if any offending characters are located.
        Val(LBlock, LNumber, LCode);
        if (LCode > 0) then
          break;

        // Set the result.
        AValue[LValueIndex] := LNumber;
      end;
    end;
  end;
  NextLine;
end;


//
// Extracts an array of doubles without allowing a line change.
// The wrong index is incremented in the array. This function must not be used if the first index is to be incremented.
// Raises an exception on error.
//
procedure TFileReader.ReadLine(ASetter: TSetDoubleFunction; AFixedIndexes: array of integer; AIndexToVary, ACount, ALength: integer; AName: string); begin ReadLine(ASetter, AFixedIndexes, AIndexToVary, ACount, ALength, AName, []) end;
procedure TFileReader.ReadLine(ASetter: TSetDoubleFunction; AFixedIndexes: array of integer; AIndexToVary, ACount, ALength: integer; AName: string; AIndexes: array of const);
const OPNAME = 'TFileReader.ReadLine.IntegerArray';
var
  LStartLineNumber, LValueIndex, LCode: integer;
  LNumber: double;
  LBlock: string;
begin
  LStartLineNumber := FLineIndex;
  AName := Format(AName, AIndexes);
  for LValueIndex := 1 to ACount do
  begin
    AFixedIndexes[AIndexToVary] := LValueIndex;
    ASetter(AFixedIndexes, 0.0);
    if (not ExtractBlock(ALength, LBlock, False, False, AName, 'double')) then
    begin
      break;
    end else begin
      if (LStartLineNumber <> FLineIndex) then
      begin
        GoBack(ALength);
        break;
      end else begin

        // Complain if any offending characters are located.
        Val(LBlock, LNumber, LCode);
        if (LCode > 0) then
          raise Exception.CreateFmt('Could not convert block [%s] on line [%d, %d] of file [%s] to double [%s]. ',
            [LBlock, FLineIndex, FCharIndex, ExtractFileName(FFileName), AName]);

        // Set the result.
        ASetter(AFixedIndexes, LNumber);
      end;
    end;
  end;
  NextLine;
end;


end.
