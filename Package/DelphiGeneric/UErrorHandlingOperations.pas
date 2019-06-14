//
// Contains : ErrorHandlingOperations.
// Author   : Grant Nyland
//
unit UErrorHandlingOperations;


interface


//
// Interface dependencies
//
uses

  // Delphi

  Classes,
  SysUtils;


//
// Interface types.
//
type
  TSetOfChar = set of AnsiChar;


//
// A set of operations that assist with error handling.
//
procedure HandleError(AException: Exception; OPNAME: string; AShowDialog: boolean = True);
procedure HandleErrorFunction(AException: Exception; OPNAME: string; var AResult: boolean; AShowDialog: boolean = True);
procedure LogSilentError(AMessage, OPNAME: string); overload;
procedure LogSilentError(AMessage: string; AData: array of const; OPNAME: string); overload;
procedure ReportError(AMessage, OPNAME: string); overload;
procedure ReportError(AMessage: string; AData: array of const; OPNAME: string); overload;
function OpenTheFileForWrite(AFilename: string; var AFileStream: TFileStream; ALogSilent, AAlwaysCreate: boolean; ALocation: string): boolean;
function AppendLine(AFilename, ARecord, OPNAME: string; ALogSilent: boolean = False): boolean;
function LogToFile(AFileName: string; ANewRecords: TStrings): boolean; overload;
function LogToFile(AFileName, ANewRecord: string): boolean; overload;
function GetLastErrorText: string;
function GetSystemErrorText(ASystemError: integer): string;
function CleanString(AString: string; AAllowedChars: TSetOfChar): string;
function StripString(AString: string; ACharsNotAllowed: TSetOfChar): string;
function CompactStr(AString: string): string;
function ApplicationExeName: string;
function CurrentModuleFileName: string;
function MainInstanceFileName: string;
function ExtractFields(AFromString, ADelimiters: string; AFields: TStrings): boolean;
function CompileFields(AFields: TStrings; ADelimiter: string = ','): string;
function FreeFormToCommaText(AFreeForm: string): string;
procedure CopyCommaTextToIntegerArray(var ADestination: array of integer; ACommaText: string);
function GetVersionNumeric(AVersionString: string): integer;
function GetModuleVersion(AModuleFileName: string): string;
function GetUserSettingsFolder: string;
function GetSpecialFolder(FolderId: integer): string;
procedure CopyToChar(var ADestination: char; ASource: string);
procedure CopyToCharArray(var ADestination: array of char; ASource: string);
procedure CopyToCharArrayRight(var ADestination: array of char; ASource: string);
function IIF(ACondition: boolean; ATrueResult: char = 'Y'; AFalseResult: char = 'N'): char; overload;
function IIF(ACondition: boolean; ATrueResult: integer = 1; AFalseResult: integer = 0): integer; overload;
function IIF(ACondition: boolean; ATrueResult: double = 1.0; AFalseResult: double = 0.0): double; overload;
function IIF(ACondition: boolean; ATrueResult: string = 'Y'; AFalseResult: string = 'N'): string; overload;
function SafeFormat(AText: string; AArgs: array of const): string;
function GetAsString(ASource: array of char; CALLEROPNAME: string): string; overload;
function GetAsString(ASource: array of char; ALen: integer; CALLEROPNAME: string): string; overload;
function GetAsString(AFormat: string; AArray: array of integer; CALLEROPNAME: string): string; overload;
function GetAsString(AFormat: string; AArray: array of integer; ALen: integer; CALLEROPNAME: string): string; overload;
function GetAsString(AFormat: string; AArray: array of double; CALLEROPNAME: string): string; overload;
function GetAsString(AFormat: string; AArray: PDouble; ARowIndex, ALeftCount, ARightCount: integer; CALLEROPNAME: string): string; overload;
function FortFixd(AWidth, AValue: integer): string; overload;
function FortFixd(AWidth: integer; AArray: array of integer; AMaxCount: integer): string; overload;
function FortFixd(AWidth, ADecimals: integer; AValue: extended): string; overload;
function FortFixd(AWidth, ADecimals: integer; AArray: array of double; AMaxCount: integer): string; overload;
function FortFixd(AWidth, ADecimals: integer; AArray: array of extended; AMaxCount: integer): string; overload;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  Windows, ShlObj, Math, System.UITypes, Vcl.Dialogs, Vcl.Forms;


//
// Global variables
//
var
  GLogLineCount: integer;	 // Maintains a count of how many times the error log file has been written to.
  GErrorLogFileName: string; // Contains the name of the error log file.


//
// Retry constants.
//
const
  CFileRetries     = 20;
  CFileRetryDelay  = 50;
  CMaxLogFileLines = 10000;


//
//  Opens the file stream object. Creates the file if it does not exist.
//  Will retry a predefined number of times before failing if the file
//  exists. The thread is suspended between retries.
//  Returns True if successful. Returns False if an exception is raised.
//
//  No error logging because the error logging uses this function.
//
function InternalOpenTheFileForWrite(AFilename: string; var AFileStream: TFileStream;
                                     AAlwaysCreate: boolean): boolean;
var
  LTry: integer;
  LFileOpened: boolean;
  LLogFolder, LError: string;
begin

  // Make sure that the directory exists.
  LLogFolder := ExtractFilePath(AFileName);
  if (not DirectoryExists(LLogFolder)) then
  begin
    CreateDir(LLogFolder);
    if (not DirectoryExists(LLogFolder)) then
      raise Exception.CreateFmt('Could not create disk folder to write to file [%s]', [AFileName]);
  end;

  // Attempt to open the file
  if FileExists(AFilename) then
  begin

    // Attempt to open the existing file.
    LTry := 0;
    LError := '';
    LFileOpened := False;
    while (not LFileOpened) and (LTry < CFileRetries) do
    begin

      // If the open attempt fails then wait for a bit.
      try
        Inc(LTry);
        if (AAlwaysCreate) then
        begin
          AFileStream := TFileStream.Create(AFilename, fmCreate OR fmShareExclusive);
        end else begin
          AFileStream := TFileStream.Create(AFilename, fmOpenReadWrite OR fmShareExclusive);
        end;
        LFileOpened := True;
      except on E: Exception do
        begin
          LError := E.Message;
          Sleep(Random(CFileRetryDelay));
        end;
      end;
    end;

    // Abort if the file could not be opened after all the retries.
    if (not LFileOpened) then
      raise Exception.Create(LError);
    Result := True;

  // Attempt to create a new file if it does not exist.
  end else begin
    AFileStream := TFileStream.Create(AFilename, fmCreate OR fmShareExclusive);
    Result := True;
  end;
end;


//
// Adds error trapping to the internal function.
//
function OpenTheFileForWrite(AFilename: string; var AFileStream: TFileStream;
                             ALogSilent, AAlwaysCreate: boolean; ALocation: string): boolean;
begin
  Result := False;
  try
    Result := InternalOpenTheFileForWrite(AFilename, AFileStream, AAlwaysCreate);
  except on E: Exception do
    if (not ALogSilent) then
      raise Exception.CreateFmt('Error opening file [%s]', [AFilename + ' : ' + E.Message]);
  end;
end;


//
//  Opens the file and appends the record at the end. Creates the file
//  if it does not exist.
//
//  Returns True if successful. Returns False if an exception is raised.
//
function AppendLine(AFilename, ARecord, OPNAME: string; ALogSilent: boolean = False): boolean;
var LFileStream: TFileStream;
begin
  Result := False;

  // Open the file. Creates it if it does not exist. Abort of failure.
  if OpenTheFileForWrite(AFilename, LFileStream, ALogSilent, False, OPNAME) then
  begin

    // Write the data.
    try
      try
        ARecord := ARecord + #13#10;
        LFileStream.Seek(0, soFromEnd);
        LFileStream.Write(PChar(ARecord)^, Length(ARecord));
        Result := True;
      except on E: Exception do

        // Re-raise the exception with more information.
        if (not ALogSilent) then
          raise Exception.CreateFmt('Error writing to file [%s]', [AFilename + ' : ' + E.Message]);
      end;

    // Close the file stream.
    finally
      LFileStream.Free;
    end;
  end;
end;


//
// A Now() stamp. Structure : YYYY-MM-DD HH:MM:SS.CCC.
//
function NowStamp: string;
var y, m, d, h, mn, s, ms: Word;
begin
  DecodeDate(Now, y, m, d);
  DecodeTime(Now, h, mn, s, ms);
  Result := Format('%0.4d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d.%0.3d', [y, m, d, h, mn, s, ms]);
end;


//
// This function will load the data in the file and do a size check and save the change.
// There is no error logging because this function is used by the error logging.
// An exception is raised if an error occurs.
//
function LogToFile(AFileName, ANewRecord: string): boolean;
const OPNAME = 'UErrorHandlingOperations.LogToFile';
var
  LLogFile: TStringList;
  LFileStream: TFileStream;
begin
  Result := False;

  // Pre-pend a date time stamp.
  ANewRecord := NowStamp + ',' + ANewRecord;
  Inc(GLogLineCount);

  // Check if the file size needs to be checked. Use the fast Append() if not.
  if ((GLogLineCount mod 500) <> 0) then
  begin
    AppendLine(AFilename, ANewRecord, OPNAME);
  end else begin

    // Open the file. Creates it if it does not exist. Abort of failure.
    if InternalOpenTheFileForWrite(AFilename, LFileStream, False) then
    begin
      try

        // Create the stringlist and load the current log from file.
        LLogFile := TStringList.Create;
        try
          try
            LLogFile.LoadFromStream(LFileStream);
          except on E: Exception do
            raise Exception.CreateFmt('Could not read log file [%s]. %s', [AFilename, E.Message]);
          end;

          // Add the new records.
          LLogFile.Add(ANewRecord);

          // Delete all rows in excess of the maximum allowed.
          while (LLogFile.Count > CMaxLogFileLines) do
            LLogFile.Delete(0);

          // Save the new file.
          try
            LFileStream.Position := 0;
            LLogFile.SaveToStream(LFileStream);
            SetEndOfFile(LFileStream.Handle);
          except on E: Exception do
            raise Exception.CreateFmt('Could not save log file [%s]. %s', [AFilename, E.Message]);
          end;

          // Done.
          Result := True;

        // Clean up.
        finally
          LLogFile.Free;
        end;
      finally
        LFileStream.Free;
      end;
    end;
  end;
end;


//
// Converts the string list to a string and calls the main function.
//
function LogToFile(AFileName: string; ANewRecords: TStrings): boolean;
const OPNAME = 'UErrorHandlingOperations.LogToFile';
var
  LIndex: integer;
  LNowStamp: string;
  LLogFile: TStringList;
begin
  Result := False;
  if (ANewRecords.Count <= 0) then
  begin
    Result := True;
  end else begin
    if (ANewRecords.Count = 1) then
    begin
      Result := True;
    end else begin
      case CompareValue(ANewRecords.Count, 1) of
        -1 : Result := True;
         0 : LogToFile(AFileName, ANewRecords[0]);
      else
        LLogFile := TStringList.Create;
        try
          LNowStamp := NowStamp;
          LLogFile.Add(ANewRecords[0]);
          for LIndex := 1 to ANewRecords.Count - 1 do
            LLogFile.Add(LNowStamp + ',' + ANewRecords[LIndex]);
          Result := LogToFile(AFileName, LLogFile.Text);
        finally
          LLogFile.Free;
        end;
      end;
    end;
  end;
end;


//
// Handles a run time error by displaying an error dialog and logging the error.
//
procedure LogError(AMessage, OPNAME: string; AShowDialog: boolean = True);
var
  LModule: array[0..100] of char;
  LText, LVersion: string;
begin
  try

    // Get the name of the current DLL or application.
    LModule[0] := #0;
    GetModuleFileName(HInstance, LModule, SizeOf(LModule));
    LVersion := GetModuleVersion(LModule);
    if(Trim(LVersion) <> '') then
      LVersion := 'Version: ' + LVersion;

    // Display an error dialog.
    if AShowDialog then
    begin

      // Build the message text.
      LText := AMessage + #10#10 + OPNAME + #10 + LModule + #10 + LVersion;

      // Display a dialog.
      MessageDlg(LText, mtError, [mbOK], 0);
    end;

    // Log the error to the application log file if required.
    if (GErrorLogFileName <> '') then
    begin

      // Build the message text.
      LText := '"' + AMessage + '","' + OPNAME + '","' + LModule + '","' + LVersion + '"';

      // Write to file.
      LogToFile(GErrorLogFileName, LText);
    end;

  // Trap a general exception.
  except
    on E: Exception do
    begin

      // Show a dialog that should not fail.
      LText := 'Unhandled Exception: ' + E.Message + #10#10 +
               'Original Exception: ' + AMessage + #10#10 + OPNAME;
      MessageDlg(LText, mtError, [mbOK], 0);
    end;
  end; // TRY : Whole operation.
end;


//
// Handles a run time error by displaying an error dialog and logging the error.
//
procedure HandleError(AException: Exception; OPNAME: string; AShowDialog: boolean = True);
begin
  LogError(AException.Message, OPNAME, AShowDialog);
end;


//
// Forces the Result to False.
//
procedure HandleErrorFunction(AException: Exception; OPNAME: string;
                              var AResult: boolean; AShowDialog: boolean = True);
begin
  AResult := False;
  HandleError(AException, OPNAME, AShowDialog);
end;


//
// Logs a silent error.
//
procedure LogSilentError(AMessage, OPNAME: string);
begin
  LogError(AMessage, OPNAME, False)
end;
procedure LogSilentError(AMessage: string; AData: array of const; OPNAME: string);
begin
  LogError(Format(AMessage, AData), OPNAME, False)
end;

//
// Shows the dialog and logs to file.
//
procedure ReportError(AMessage, OPNAME: string);
begin
  LogError(AMessage, OPNAME, True);
end;
procedure ReportError(AMessage: string; AData: array of const; OPNAME: string);
begin
  LogError(Format(AMessage, AData), OPNAME, True)
end;


//
// Returns the text from GetLastError().
//
function GetLastErrorText: string;
var LSystemError: Pointer;
begin

  // Get the system error description.
  try
    FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER OR
        FORMAT_MESSAGE_FROM_SYSTEM OR
        FORMAT_MESSAGE_IGNORE_INSERTS,
      nil,
      GetLastError,
      0,
      @LSystemError,
      0,
      nil
    );

    // Get the text from the system and strip off the line feed.
    Result := StrPas(PChar(LSystemError));
    Result := Copy(Result, 1, Length(Result) - 2);

  // Free the buffer allocated int FormatMessage().
  finally
    LocalFree(DWORD(LSystemError));
  end;
end;


//
// Returns the text from the system for the passed error code.
//
function GetSystemErrorText(ASystemError: integer): string;
var LSystemError: Pointer;
begin

  // Get the system error description.
  try
    FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER OR
        FORMAT_MESSAGE_FROM_SYSTEM OR
        FORMAT_MESSAGE_IGNORE_INSERTS,
      nil,
      ASystemError,
      0,
      @LSystemError,
      0,
      nil
    );

    // Get the text from the system and strip off the line feed.
    Result := StrPas(PChar(LSystemError));
    Result := Copy(Result, 1, Length(Result) - 2);

  // Free the buffer allocated int FormatMessage().
  finally
    LocalFree(DWORD(LSystemError));
  end;
end;

//
// Removes all duplicate white space.
//
function CompactStr(AString: string): string;
var
  LIndex: integer;
  LIsWhite: boolean;
begin
  AString := Trim(AString);
  LIsWhite := False;
  LIndex := Length(AString);
  while (LIndex > 0) do
  begin
    if (Ord(AString[LIndex]) <= 32) then
    begin
      if LIsWhite then
        Delete(AString, LIndex, 1);
      LIsWhite := True;
    end else begin
      LIsWhite := False;
    end;
    Dec(LIndex);
  end;
  Result := AString;
end;

//
// Strips out all characters that are not in the allowed set.
//
function CleanString(AString: string; AAllowedChars: TSetOfChar): string;
var LIndex: integer;
begin
  Result := '';
  for LIndex := 1 to Length(AString) do
    if CharInSet(AString[LIndex],AAllowedChars) then
      Result := Result + AString[LIndex];
end;

//
// Strips out all characters that are not in the allowed set.
//
function StripString(AString: string; ACharsNotAllowed: TSetOfChar): string;
var LIndex: integer;
begin
  Result := '';
  for LIndex := 1 to Length(AString) do
    if (not CharInSet(AString[LIndex],ACharsNotAllowed)) then
      Result := Result + AString[LIndex];
end;

function GetLongPathName(AFileName: string): string;
var
  LHandle: Integer;
  Buffer: array[0..MAX_PATH] of Char;
  LGetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
    cchBuffer: Integer): Integer stdcall;
begin
  Result := AFileName;
  LHandle := GetModuleHandle('kernel32.dll');
  if LHandle <> 0 then
  begin
    @LGetLongPathName := GetProcAddress(LHandle, 'GetLongPathNameA');
    if Assigned(LGetLongPathName) and (LGetLongPathName(PChar(AFileName), Buffer, SizeOf(Buffer)) <> 0) then
    Result := Buffer;
  end;
end;
function ApplicationExeName: string;
var
  ls_Module: array[0..500] of char; // Contains the name of the DLL.
  LBinStr,
  LPath: string;
begin
    // Get the name of the current DLL or application.
    ls_Module[0] := #0;
    GetModuleFileName(HInstance, ls_Module, SizeOf(ls_Module));
    Result := ls_Module;
    if(Result <> '') then
    begin
      LPath := ExtractFilePath(Result);
      if(Length(LPath) > 4) then
      begin
        LBinStr := Copy(LPath,Length(LPath)- 3,4);

        if(UpperCase(LBinStr) = 'BIN\') then
        begin
           LPath := Copy(LPath,1,Length(LPath)- 4);
           Result := LPath + ExtractFileName(Result);
        end;
      end;
      if(Length(LPath) > 7) then
      begin
        LBinStr := Copy(LPath,Length(LPath)- 6,7);

        if(UpperCase(LBinStr) = 'STOMSA\') then
        begin
           LPath := Copy(LPath,1,Length(LPath)- 7);
           Result := LPath + ExtractFileName(Result);
        end;
      end;

      Result := GetLongPathName(Result);
    end;
end;

function CurrentModuleFileName: string;
var LModule: array[0..100] of char;
begin
    LModule[0] := #0;
    GetModuleFileName(HInstance, LModule, SizeOf(LModule));
    Result := LModule;
    Result := GetLongPathName(Result);
end;

function MainInstanceFileName: string;
var LModule: array[0..100] of char;
begin
    LModule[0] := #0;
    GetModuleFileName(MainInstance, LModule, SizeOf(LModule));
    Result := LModule;
    Result := GetLongPathName(Result);
end;

//
// Returns the character position of the closest delimeter.
//
function GetPosOfClosestDelimeter(AFromString, ADelimiters: string; AFromPos: integer): integer;
const OPNAME = 'UErrorHandlingOperations.GetPosOfClosestDelimeter';
var
  LBuffer: string;
  LDelimiterIndex, LThisPos: integer;
begin
  Result := 0;
  try

    // Set an impossible value.
    Result := Length(AFromString) + 1;

    // Loop for every delimeter.
    if (AFromPos < Result) then
    begin
      for LDelimiterIndex := 1 to Length(ADelimiters) do
      begin
        LBuffer := Copy(AFromString, AFromPos, Length(AFromString));
        LThisPos := Pos(ADelimiters[LDelimiterIndex], LBuffer);
        if (LThisPos < Result) and (LThisPos > 0) then
          Result := LThisPos;
      end;
    end;

    // Reset the impossible value if no delimeters were found.
    if (Result > Length(AFromString)) then
      Result := 0;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Extracts the fields from the string. Works the same as CommaText.
// It is not sensitive to the quotation mark. The delimiter does not
// need to be a comma.
//
function ExtractFields(AFromString, ADelimiters: string; AFields: TStrings): boolean;
const OPNAME = 'UErrorHandlingOperations.ExtractFields';
var
  LOneField: string;
  LLen, LPos, LTotalLength: integer;
begin
  Result := False;
  try

    // Start by clearing the string list. Proceed if there is data.
    AFields.Clear;
    LTotalLength := Length(AFromString);
    if (LTotalLength > 0) then
    begin

      // Run loop if more than one field.
      LLen := 0;
      LPos := 1;
      repeat

        // Move to the start of the next field.
        LPos := LPos + LLen;

        // Get the next closest delimeter.
        LLen := GetPosOfClosestDelimeter(AFromString, ADelimiters, LPos);

        // Add the field if found.
        if (LLen > 0) then
        begin
          LOneField := Copy(AFromString, LPos, LLen - 1);
          AFields.Add(LOneField);
        end;

      // Until no delimiters are found.
      until (LLen <= 0);

      // Add last field.
      LOneField := Copy(AFromString, LPos, LTotalLength);
      AFields.Add(LOneField);

    end; // IF : Is there data.

    // Done
    Result := True;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Compiles the fields into a delimited string. Works the same as CommaText.
// It is not sensitive to the quotation mark. The delimiter does not
// need to be a comma.
//
function CompileFields(AFields: TStrings; ADelimiter: string = ','): string;
const OPNAME = 'UErrorHandlingOperations.CompileFields';
var LIndex: integer;
begin
  Result := '';
  try
    if Assigned(AFields) then
    begin
      if (AFields.Count > 0) then
      begin
        Result := AFields[0];
        for LIndex := 1 to AFields.Count - 1 do
          Result := Result + ADelimiter + AFields[LIndex];
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Takse a free form line and converts it to a commatext record.
//
function FreeFormToCommaText(AFreeForm: string): string;
begin
  AFreeForm := CompactStr(AFreeForm);
  Result := StringReplace(AFreeForm, ' ', ',', [rfReplaceAll, rfIgnoreCase]);
end;


//
// Copies a comma text string to an array of integers with length checking.
//
procedure CopyCommaTextToIntegerArray(var ADestination: array of integer; ACommaText: string);
var
  LIndex: integer;
  LNumbers: TStringList;
begin
  LNumbers := TStringList.Create;
  try
    LNumbers.CommaText := ACommaText;
    for LIndex := 0 to Length(ADestination) - 1 do
    begin
      ADestination[LIndex] := 0;
      if (LIndex < LNumbers.Count) then
        try ADestination[LIndex] := StrToInt(LNumbers[LIndex]) except end;
    end;
  finally
    LNumbers.Free;
  end;
end;


//
// Compiles a integer from a version string.
//
function GetVersionNumeric(AVersionString: string): integer;
const OPNAME = 'UErrorHandlingOperations.GetVersionNumeric';
var LFields: TStringList;
begin
  Result := 0;
  try
    LFields := TStringList.Create;
    try
      ExtractFields(AVersionString, '.', LFields);
      if (LFields.Count > 0) then try Result := Result + StrToInt(LFields[0]) * 1000000; except end;
      if (LFields.Count > 1) then try Result := Result + StrToInt(LFields[1]) * 1000;    except end;
      if (LFields.Count > 2) then try Result := Result + StrToInt(LFields[2]) * 1;       except end;
    finally
      LFields.Free;
    end;
  except end;
end;


//
// Get the version information for the module.
//
function GetModuleVersion(AModuleFileName: string): string;
var
  LSizeOfVersionInfoStructure: integer;
  LVersionInfoStructure: pointer;
  LIndex: dword;
  LDataStr: string;
begin

  // Get the size of the version info data.
  LSizeOfVersionInfoStructure := GetFileVersionInfoSize(PChar(AModuleFileName), LIndex);
  if (LSizeOfVersionInfoStructure > 0) then
  begin

    // Allocate enough memory for the version info data.
    LVersionInfoStructure := AllocMem(LSizeOfVersionInfoStructure);
    try

      // Get the version info data.
      GetFileVersionInfo(PChar(AModuleFileName), 0, LSizeOfVersionInfoStructure, LVersionInfoStructure);

      // Discard all the nulls which also converts from wide string.
      LDataStr := '';
      for LIndex := 0 to LSizeOfVersionInfoStructure - 1 do
        if (PChar(LVersionInfoStructure)[LIndex] <> #0) then
          LDataStr := LDataStr + PChar(LVersionInfoStructure)[LIndex];
    finally
      FreeMem(LVersionInfoStructure);
    end;

    // Find the file version number.
    Result := Copy(LDataStr, Pos('FileVersion', LDataStr) + 11, 10);

    // Discard garbage.
    Result := CleanString(Result, ['.','0'..'9']);

    // Discard the zero's on the right.
    {if (Result[Length(Result)] = '.') then
      Result := Copy(Result, 1, Length(Result) - 1);
    for i := 1 to 3 do
      if (Result[Length(Result)] = '0') then
        Result := Copy(Result, 1, Length(Result) - 2);}
  end;
end;


//
// This function returns the root user folder.
//
function GetUserSettingsFolder: string;
begin

  // Get the start menu which is located directly in the user's root.
  Result := GetSpecialFolder(CSIDL_STARTMENU);

  // Go back one folder to get the root.
  if (Result <> '') then
  begin
    Result := ExcludeTrailingPathDelimiter(Result);
    while (Length(Result) > 0) and (not(CharInSet(Result[Length(Result)],['\','/']))) do
       Delete(Result,Length(Result),1);
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

//
// Returns the special folder as a string. FolderId should be one of the
// ShlObj.pas CSIDL_ constants such as CSIDL_APPDATA.
//
function GetSpecialFolder(FolderId: integer): string;
var LItemId: PItemIdList;
begin
  Result := '';
  if (SHGetSpecialFolderLocation(0, FolderId, LItemId) = NOERROR) then
  begin
    SetLength(Result,MAX_PATH);
    if SHGetPathFromIDList(LItemId, PChar(Result)) then
    begin
      SetLength(Result,StrLen(PChar(Result)));
      Result := IncludeTrailingPathDelimiter(Result);
    end;
  end;
end;


//
// Copies the first character in a string to a character with length checking.
//
procedure CopyToChar(var ADestination: char; ASource: string);
begin
  if (Length(ASource) > 0) then
  begin
    ADestination := ASource[1];
  end else begin
    ADestination := ' ';
  end;
end;


//
// Copies a string to a character array with length checking.
//
procedure CopyToCharArray(var ADestination: array of char; ASource: string);
var LIndex: integer;
begin
  for LIndex := 0 to Length(ADestination) - 1 do
  begin
    if ((LIndex + 1) <= Length(ASource)) then
    begin
      ADestination[LIndex] := ASource[LIndex + 1];
    end else begin
      ADestination[LIndex] := ' ';
    end;
  end;
end;


//
// Copies a string to the right hand side of a character array with length checking.
//
procedure CopyToCharArrayRight(var ADestination: array of char; ASource: string);
var LIndex: integer;
begin
  for LIndex := 0 to Length(ADestination) - 1 do
  begin
    if ((LIndex + 1 - Length(ASource)) > 0) then
    begin
      ADestination[LIndex] := ASource[LIndex + 1 - Length(ASource)];
    end else begin
      ADestination[LIndex] := ' ';
    end;
  end;
end;


//
// Inline IF operations.
//
function IIF(ACondition: boolean; ATrueResult: char = 'Y'; AFalseResult: char = 'N'): char;
begin
  if ACondition then
  begin
    Result := ATrueResult;
  end else begin
    Result := AFalseResult;
  end;
end;

function IIF(ACondition: boolean; ATrueResult: integer = 1; AFalseResult: integer = 0): integer;
begin
  if ACondition then
  begin
    Result := ATrueResult;
  end else begin
    Result := AFalseResult;
  end;
end;

function IIF(ACondition: boolean; ATrueResult: double = 1.0; AFalseResult: double = 0.0): double;
begin
  if ACondition then
  begin
    Result := ATrueResult;
  end else begin
    Result := AFalseResult;
  end;
end;

function IIF(ACondition: boolean; ATrueResult: string = 'Y'; AFalseResult: string = 'N'): string;
begin
  if ACondition then
  begin
    Result := ATrueResult;
  end else begin
    Result := AFalseResult;
  end;
end;


//
// Compiles a string from arrays of various types.
//
function SafeFormat(AText: string; AArgs: array of const): string;
begin
  try
    Result := Format(AText, AArgs);
  except on E: Exception do
    begin
      Result := AText;
      raise Exception.Create(E.Message + '. In format string [' + AText + ']. ' +
              'This is usually caused by unusual characters in element names. ');
    end;
  end;
end;
function GetAsString(ASource: array of char; CALLEROPNAME: string): string;
const OPNAME = 'GetAsString.ArrayOfChar';
var LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to Length(ASource) - 1 do
      Result := Result + ASource[LIndex];
    Result := Trim(Result);
  except on E: Exception do HandleError(E, CALLEROPNAME + '.' + OPNAME) end;
end;
function GetAsString(ASource: array of char; ALen: integer; CALLEROPNAME: string): string;
const OPNAME = 'GetAsString.ArrayOfChar';
var LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to ALen - 1 do
    begin
      if (LIndex < Length(ASource)) then
      begin
        Result := Result + ASource[LIndex];
      end else begin
        break;
      end;
    end;
    Result := Trim(Result);
  except on E: Exception do HandleError(E, CALLEROPNAME + '.' + OPNAME) end;
end;
function GetAsString(AFormat: string; AArray: array of integer; CALLEROPNAME: string): string;
const OPNAME = 'GetAsString.ArrayOfInteger';
var LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to High(AArray) do
      Result := Result + SafeFormat(AFormat, [AArray[LIndex]]);
  except on E: Exception do HandleError(E, CALLEROPNAME + '.' + OPNAME) end;
end;
function GetAsString(AFormat: string; AArray: array of integer; ALen: integer; CALLEROPNAME: string): string;
const OPNAME = 'GetAsString.ArrayOfIntegerLen';
var LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to ALen - 1 do
      if (LIndex <= High(AArray)) then
        Result := Result + SafeFormat(AFormat, [AArray[LIndex]]);
  except on E: Exception do HandleError(E, CALLEROPNAME + '.' + OPNAME) end;
end;
function GetAsString(AFormat: string; AArray: array of double; CALLEROPNAME: string): string;
const OPNAME = 'GetAsString.ArrayOfDouble';
var LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to High(AArray) do
      Result := Result + SafeFormat(AFormat, [AArray[LIndex]]);
  except on E: Exception do HandleError(E, CALLEROPNAME + '.' + OPNAME) end;
end;
function DoubleOffSet(ABasePointer: PDouble; ALeftIndex, ARightIndex, ARightCount: integer): double;
begin
  Result := double(pointer(ALeftIndex * ARightCount * SizeOf(double) + integer(@(ABasePointer^)) + ARightIndex * SizeOf(double))^);
end;
function GetAsString(AFormat: string; AArray: PDouble; ARowIndex, ALeftCount, ARightCount: integer; CALLEROPNAME: string): string;
const OPNAME = 'GetAsString.DoublePointer';
var
  LIndex: integer;
  LStandInArray: array of double;
begin
  Result := '';
  try
    SetLength(LStandInArray, ALeftCount);
    try
      for LIndex := 0 to ALeftCount - 1 do
        LStandInArray[LIndex] := DoubleOffSet(AArray, LIndex, ARowIndex, ARightCount);
      Result := GetAsString(AFormat, LStandInArray, CALLEROPNAME);
    finally
      Finalize(LStandInArray);
    end;
  except on E: Exception do HandleError(E, CALLEROPNAME + '.' + OPNAME) end;
end;


//
// These functions simulate the fixed width number formatting produced by Fortran.
// The number is replaced with *'s if it is too wide to fit into the space provided.
//
function FortFixd(AWidth, AValue: integer): string;
begin
  Result := Format('%' + IntToStr(AWidth) + 'd', [AValue]);
  if (Length(Result) > AWidth) then
    Result := StringOfChar('*', AWidth);
end;
function FortFixd(AWidth: integer; AArray: array of integer; AMaxCount: integer): string;
var LIndex: integer;
begin
  Result := '';
  for LIndex := 0 to High(AArray) do
    if (LIndex < AMaxCount) then
      Result := Result + FortFixd(AWidth, AArray[LIndex]);
end;
function FortFixd(AWidth, ADecimals: integer; AValue: extended): string;
begin
  Result := Format('%' + IntToStr(AWidth) + '.' + IntToStr(ADecimals) + 'f', [AValue]);
  if (Length(Result) > AWidth) then
    Result := StringOfChar('*', AWidth);
end;
function FortFixd(AWidth, ADecimals: integer; AArray: array of double; AMaxCount: integer): string;
var LIndex: integer;
begin
  Result := '';
  for LIndex := 0 to High(AArray) do
    if (LIndex < AMaxCount) then
      Result := Result + FortFixd(AWidth, ADecimals, AArray[LIndex]);
end;
function FortFixd(AWidth, ADecimals: integer; AArray: array of extended; AMaxCount: integer): string;
var LIndex: integer;
begin
  Result := '';
  for LIndex := 0 to High(AArray) do
    if (LIndex < AMaxCount) then
      Result := Result + FortFixd(AWidth, ADecimals, AArray[LIndex]);
end;


//
// Set the error log file name.
//
initialization
begin
  GLogLineCount := -1;
  GErrorLogFileName :=
    ExtractFilePath(Application.ExeName) +
    'Logs\' +
    Copy(ExtractFileName(Application.ExeName), 1, Length(ExtractFileName(Application.ExeName)) - 4) + '_ErrorLog.txt';
    if not DirectoryExists(ExtractFilePath(GErrorLogFileName)) then
       ForceDirectories(ExtractFilePath(GErrorLogFileName));
end;


end.

