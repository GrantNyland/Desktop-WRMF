//
//  UNIT      : Contains Class TSQLScriptEntry.
//  AUTHOR    : Philemon Setshedi.
//  DATE      : 2004/03/30
//  COPYRIGHT : Copyright © 2004 DWAF
//
unit USQLScriptList;

interface

uses
  Classes,
  Dialogs,
  UStringListOfStringLists;

type
  TSQLType = (
    sftError,
    sftDropTable,
    sftCreateTable,
    sftCreateIndex,
    sftForeignKey,
    sftStoredProcedure,
    sftLoadData,
    sftLoadDataSpecial,
    sftScript
  );

//
// This class contains a list of filenames that contain
// SQL statements. The class can expand contained script files.
//
type
  TSQLScriptEntry = class(TObject)
    Filename: string;
    Name: string;
    TableName: string;
    SQLType: TSQLType;
    Enabled: boolean;
    SQL: string;
    StatementCount: integer;
    function SQLStatementType: TSQLType;
  end;
  TSQLScriptList = class(TObject)
  protected
    FItems: TStringListOfStringLists;
    FPathBase: string;
    FPathScript: string;
    FPathSchemas: string;
    FPathData: string;
    FLineEndedOn: integer;
    FEndOfFile : boolean;
    function GetSQLType(AFileName: string): TSQLType;
    function ExpandScript(AScriptFile: TStringList; ACurrentIndex: integer): boolean;
    procedure LoadSQL(AScriptFile: TStringList);
    procedure ConstructFilePaths(const AFileName: string);
    procedure RemoveScriptComments(AScriptFile: TStringList);
    function GetFullFileName(AScriptFile: string; var ASQLType: TSQLType): string;
    function GetScriptFilename(AIndex: integer): string;
    function GetScriptName(AScriptIndex: integer): string;
    function GetScriptType(AScriptIndex: integer): TSQLType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    function RowCount: integer;
    function RowStatementsCount(ARowIndex: integer): integer;
    procedure LoadFromFile(const AFileName: string);
    procedure SetEnabled(AScriptIndex: integer; AnEnabledState: boolean);
    function IsScriptEnabled(AScriptIndex: integer): boolean;
    function LoadScriptEntry(AScriptIndex: integer): boolean;
    procedure AddDefaultScriptEntry(AScriptIndex: integer);
    function ScriptEntry(AScriptIndex, ASQLStatementIndex: integer): TSQLScriptEntry;
    function GetTotalScriptCount: integer;
    property ScriptFilename[AIndex: integer]: string read GetScriptFilename;
    property ScriptName[AIndex: integer]: string read GetScriptName;
    property ScriptType[AIndex: integer]: TSQLType read GetScriptType;
    property LineEndedOn : integer read FLineEndedOn write FLineEndedOn;
    property EndOfFile : boolean read FEndOfFile write FEndOfFile;

  end;
  function RemoveExtraSpaces(AString: String): String;

implementation

uses
  SysUtils,
  UFileOperations,
  UStringFieldOperations,
  UErrorHandlingOperations;

function RemoveExtraSpaces(AString: String): String;
const OPNAME = 'USQLScriptList.RemoveExtraSpaces';
begin
  Result := Trim(AString);
  while (Pos('  ', Result) > 0) do
    Delete(Result,Pos('  ', Result),1);
end;

//
// Creates the member objects.
//
constructor TSQLScriptList.Create;
const OPNAME = 'TSQLScriptList.Create';
begin
  try
    inherited;
    FItems := TStringListOfStringLists.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Destroys the member objects.
//
destructor TSQLScriptList.Destroy;
const OPNAME = 'TSQLScriptList.Destroy';
begin
  try
    Clear;
    FreeAndNil(FItems);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Clears the member objects
//
procedure TSQLScriptList.Clear;
const OPNAME = 'TSQLScriptList.Clear';
begin
  try
    FItems.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Returns the number of script files in the list.
//
function TSQLScriptList.RowCount: integer;
const OPNAME = 'TSQLScriptList.RowCount';
begin
  Result := 0;
  try
    Result := FItems.RowCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Returns the number of SQL statements at the given row index.
//
function TSQLScriptList.RowStatementsCount(ARowIndex: integer): integer;
const OPNAME = 'TSQLScriptList.RowStatementsCount';
begin
  Result := 0;
  try
    if (FItems.RowItemsCount(ARowIndex) > 1) then
    begin
      Result := FItems.RowItemsCount(ARowIndex);
    end else begin
      if (FItems.Row[ARowIndex].Count > 0) then
        Result := TSQLScriptEntry(FItems.Row[ARowIndex].Objects[0]).StatementCount;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Returns the name of the script file at the given index.
//
function TSQLScriptList.GetScriptFilename(AIndex: integer): string;
const OPNAME = 'TSQLScriptList.GetScriptFilename';
begin
  Result := '';
  try
    Result := FItems.Items[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Returns the entry at the given index.
//
function TSQLScriptList.ScriptEntry(AScriptIndex, ASQLStatementIndex: integer): TSQLScriptEntry;
const OPNAME = 'TSQLScriptList.ScriptEntry';
begin
  Result := nil;
  try
    Result := TSQLScriptEntry(FItems.Row[AScriptIndex].Objects[ASQLStatementIndex]);

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Sets all the items in the list to the required state.
//
procedure TSQLScriptList.SetEnabled(AScriptIndex: integer; AnEnabledState: boolean);
const OPNAME = 'TSQLScriptList.SetEnabled';
var LSQLStatementIndex: integer;
begin
  try
    for LSQLStatementIndex := 0 to FItems.RowItemsCount(AScriptIndex) - 1 do
      ScriptEntry(AScriptIndex, LSQLStatementIndex).Enabled := AnEnabledState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Returns true if the script is enabled.
//
function TSQLScriptList.IsScriptEnabled(AScriptIndex: integer): boolean;
const OPNAME = 'TSQLScriptList.IsScriptEnabled';
begin
  Result := False;
  try
    Result := ScriptEntry(AScriptIndex, 0).Enabled;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Returns the name of the SQL statements.
//
function TSQLScriptList.GetScriptName(AScriptIndex: integer): string;
const OPNAME = 'TSQLScriptList.GetScriptName';
begin
  Result := '';
  try
    Result := ScriptEntry(AScriptIndex, 0).Name;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Returns the type of the SQL statements.
//
function TSQLScriptList.GetScriptType(AScriptIndex: integer): TSQLType;
const OPNAME = 'TSQLScriptList.GetScriptType';
begin
  Result := sftError;
  try
    Result := ScriptEntry(AScriptIndex, 0).SQLType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Reads the data from the given file. If any of the entries
// are themselves script files then these script files are
// expanded.
//
procedure TSQLScriptList.LoadFromFile(const AFileName: string);
const OPNAME = 'TSQLScriptList.LoadFromFile';
var
  LScriptFile: TStringList;
  LIndex: integer;
  LFirstLine: string;
  //LExtractFilePath: string;

begin
  try

    // Set the paths.
    ConstructFilePaths(AFileName);

    // Call the parent to load the raw data.
    LScriptFile := TStringList.Create;
    try
      LScriptFile.LoadFromFile(AFileName);
      RemoveScriptComments(LScriptFile);

      //Remove version line
      if(LScriptFile.Count > 0) then
      begin
        LFirstLine := UpperCase(Trim(LScriptFile[0]));
        if(Pos('TABLEVERSION',LFirstLine) = 1) then
          LScriptFile.Delete(0);
      end;

      // Expand sub scripts if required.
      LIndex := 0;
      while (LIndex < LScriptFile.Count) do
      begin
        if (GetSQLType(LScriptFile[LIndex]) = sftError) then
        begin
          LScriptFile.Delete(LIndex);
        end
        else
        begin
          if (not ExpandScript(LScriptFile, LIndex)) then
            Inc(LIndex);
        end;
      end;

      // Remove duplicates.
      for LIndex := LScriptFile.Count - 1 downto 0 do
        if (LScriptFile.IndexOf(LScriptFile[LIndex]) <> LIndex) then
          LScriptFile.Delete(LIndex);

      // Load the SQL for each script entry.
      LoadSQL(LScriptFile);

    // Clean up.
    finally
      LScriptFile.Free;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// If the item at the current element is another script file
// then it is removed and replaced with the contents of that script.
// If the current element is not a script, it is left as is.
//
function TSQLScriptList.ExpandScript(AScriptFile: TStringList; ACurrentIndex: integer): boolean;
const OPNAME = 'TSQLScriptList.ExpandScript';
var
  LSubScript: TStringList;
  LPVCSFileIndex: integer;
begin
  Result := False;
  try

    // Load the sub script if there is one.
    if (GetSQLType(AScriptFile[ACurrentIndex]) = sftScript) then
    begin
      LSubScript := TStringList.Create;
      try
        LSubScript.LoadFromFile(FPathScript + ExtractFileName(AScriptFile[ACurrentIndex]));
        RemoveScriptComments(LSubScript);

        // Insert all of the sub script's lines.
        for LPVCSFileIndex := 0 to LSubScript.Count - 1 do
          AScriptFile.Insert(ACurrentIndex + LPVCSFileIndex, LSubScript[LPVCSFileIndex]);

        // Delete the sub script entry.
        AScriptFile.Delete(ACurrentIndex + LSubScript.Count);
        Result := True;

      // Done.
      finally
        LSubScript.Free;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSQLScriptList.ConstructFilePaths(const AFileName: string);
const OPNAME = 'TSQLScriptList.ConstructFilePaths';
begin
  try
    FPathScript := ExtractFilePath(AFileName);
    FPathBase := FPathScript;
    FPathSchemas := FPathScript;
    FPathData := FPathScript;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Strips out script comments.
//
procedure TSQLScriptList.RemoveScriptComments(AScriptFile: TStringList);
const OPNAME = 'TSQLScriptList.RemoveScriptComments';
var LIndex: integer;
begin
  try
    for LIndex := AScriptFile.Count - 1 downto 0 do
      if (AScriptFile[LIndex] = '') or
         (not (AScriptFile[LIndex][1] in ['A'..'Z', 'a'..'z'])) then
        AScriptFile.Delete(LIndex);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// This operation returns the file type for the given file name.
//
function TSQLScriptList.GetSQLType(AFileName: string): TSQLType;
const OPNAME = 'TSQLScriptList.GetSQLType';
var LFirstThreeCharacters: string;
begin
  Result := sftError;
  try
    LFirstThreeCharacters := UpperCase(Copy(ExtractFileName(AFileName), 1, 3));
    if LFirstThreeCharacters = 'CT_' then Result := sftCreateTable     else
    if LFirstThreeCharacters = 'IX_' then Result := sftCreateIndex     else
    if LFirstThreeCharacters = 'FK_' then Result := sftForeignKey      else
    if LFirstThreeCharacters = 'SP_' then Result := sftStoredProcedure else
    if LFirstThreeCharacters = 'LD_' then Result := sftLoadData        else
    if LFirstThreeCharacters = 'LS_' then Result := sftLoadDataSpecial else
    if LFirstThreeCharacters = 'SC_' then Result := sftScript          else
    raise Exception.CreateFmt('Unknown SQL file type [%s] in filename [%s].', [LFirstThreeCharacters, AFileName]);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Loads the SQL for each file in the script.
//
procedure TSQLScriptList.LoadSQL(AScriptFile: TStringList);
const OPNAME = 'TSQLScriptList.LoadSQL';
var LScriptIndex: integer;
begin
  try
    Clear;
    for LScriptIndex := 0 to AScriptFile.Count - 1 do
    begin
      FItems.Add(AScriptFile[LScriptIndex], '');
      AddDefaultScriptEntry(LScriptIndex);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Loads the SQL for each file in the script.
//
function TSQLScriptList.GetFullFileName(AScriptFile: string; var ASQLType: TSQLType): string;
const OPNAME = 'TSQLScriptList.GetFullFileName';
begin
  Result := '';
  ASQLType := sftError;
  try

    // Get the SQL type.
    ASQLType := GetSQLType(AScriptFile);

    // Constract the file name.
    case ASQLType of
      sftCreateTable, sftCreateIndex, sftForeignKey, sftStoredProcedure :
        Result := FPathSchemas + AScriptFile;
      sftLoadData, sftLoadDataSpecial :
        Result := FPathData + ExtractFileName(AScriptFile);
    else
      ReportError(Format('Could not load SQL file [%s].', [AScriptFile]), OPNAME);
    end;
    if (not FileExists(Result)) then
    begin
      Result := '';
      ASQLType := sftError;
      ReportError(Format('SQL file not found [%s].', [AScriptFile]), OPNAME);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Loads the SQL for each file in the script.
//
function TSQLScriptList.LoadScriptEntry(AScriptIndex: integer): boolean;
const OPNAME = 'TSQLScriptList.LoadScriptEntry';
const MaxLines = 1000;
var
  LSQLType: TSQLType;
  LSQLStatementIndex: integer;
  LScriptEntry: TSQLScriptEntry;
  LSQL: TStringList;
  LFirstLine,
  LBuffer: string;
  LScriptName,
  LTableName,
  LFilename,
  LFullFileName,
  LFileLine: string;
  LExtractedFileName: string;

  LIndex : integer;
  LFile : TextFile;

begin
  Result := False;
  try

    // Clear any currently loaded data.
    FItems.ClearRow(AScriptIndex);

    // Make sure that the file exists.
    LFullFileName := GetFullFileName(FItems.Items[AScriptIndex], LSQLType);
    LExtractedFileName := ExtractFileName(LFullFileName);
    if (LSQLType <> sftError) then
    begin

      // Load the SQL and split into separate statements.
      LSQL := TStringList.Create;
      try
        AssignFile(LFile,LFullFileName);
        Reset(LFile);
        LIndex := 0;
        //Go to Line ended on
        while (LIndex < FLineEndedOn) do
        begin
          Readln(LFile,LFileLine);
          Inc(LIndex);
        end;

        LIndex := 0;
        //Read from where you left off 1000 line max
        LSQL.Clear;
        while ((LIndex < MaxLines) and (not eof(LFile))) do
        begin
          Readln(LFile,LFileLine);
          LSQL.Add(LFileLine);
          Inc(FLineEndedOn);
          Inc(LIndex);
        end;

        FEndOfFile := eof(LFile);

        //Remove version line
        if(LSQL.Count > 0) then
        begin
          LFirstLine := UpperCase(Trim(LSQL[0]));
          if(Pos('TABLEVERSION',LFirstLine) = 1) then
            LSQL.Delete(0);
        end;
        LBuffer := LSQL.Text;
        ExtractFields(LBuffer, ';', LSQL);
        if (LSQL.Count > 1) then
          if((Trim(LSQL.Strings[LSQL.Count - 1]) = '') or
             (Trim(LSQL.Strings[LSQL.Count - 1]) = ';')) then
            LSQL.Delete(LSQL.Count - 1);

        LFilename := Trim(LExtractedFileName);
        if(Pos('.',LFilename) > 1) then
          LFilename := Copy(LFilename,1,Pos('.',LFilename)-1);

        LFilename   := Trim(LFilename);
        LScriptName := Copy(LFilename, 4, Length(LExtractedFileName));
        LTableName  := LScriptName;
        if(Pos('_',LTableName) > 1) then
          LTableName := Copy(LTableName,1,Pos('_',LTableName)-1);

        // Add the SQL entries.
        for LSQLStatementIndex := 0 to LSQL.Count - 1 do
        begin
          LScriptEntry := TSQLScriptEntry.Create;
          LScriptEntry.Filename  := LFullFileName;
          LScriptEntry.Name      := LScriptName;
          LScriptEntry.TableName := LTableName;
          LScriptEntry.SQLType   := LSQLType;
          LScriptEntry.Enabled   := True;
          LScriptEntry.SQL       := LSQL[LSQLStatementIndex];
          LScriptEntry.StatementCount := 1;
          FItems.Add(FItems.Items[AScriptIndex], IntToStr(AScriptIndex), LScriptEntry);
        end;
      finally
        LSQL.Free;
      end;
    end;
    CloseFile(LFile);
    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Loads the SQL for each file in the script.
//
procedure TSQLScriptList.AddDefaultScriptEntry(AScriptIndex: integer);
const OPNAME = 'TSQLScriptList.AddDefaultScriptEntry';
var
  LSQLType: TSQLType;
  LScriptEntry: TSQLScriptEntry;
  LFullFileName: string;
  LExtractedFileName: string;
begin
  try

    // Get the file name.
    LFullFileName := GetFullFileName(FItems.Items[AScriptIndex], LSQLType);
    LExtractedFileName := ExtractFileName(LFullFileName);
    if (LSQLType <> sftError) then
    begin
      FItems.ClearRow(AScriptIndex);

      // Create the generic script entry object with empty SQL.
      LScriptEntry := TSQLScriptEntry.Create;
      LScriptEntry.Filename  := LFullFileName;
      LScriptEntry.Name      := Copy(LExtractedFileName, 4, Length(LExtractedFileName) - 7);
      LScriptEntry.TableName := Copy(LScriptEntry.Name, 1, Pos('_', LScriptEntry.Name) - 1);
      LScriptEntry.SQLType   := LSQLType;
      LScriptEntry.Enabled   := True;
      LScriptEntry.SQL       := '';
      LScriptEntry.StatementCount := FileCountSubStr(LFullFileName, ';');

      // Add the generic object to the list.
      FItems.Add(FItems.Items[AScriptIndex], IntToStr(AScriptIndex), LScriptEntry);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Returns the total number of all statements in all script files.
//
function TSQLScriptList.GetTotalScriptCount: integer;
const OPNAME = 'TSQLScriptList.GetTotalScriptCount';
var LScriptIndex: integer;
begin
  Result := 0;
  try
    for LScriptIndex := 0 to FItems.RowCount - 1 do
      Inc(Result, RowStatementsCount(LScriptIndex));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TSQLScriptEntry }

function TSQLScriptEntry.SQLStatementType: TSQLType;
const OPNAME = 'TSQLScriptEntry.SQLStatementType';
var
  LSQL: string;
begin
  Result := SQLType;
  LSQL   := Trim(UpperCase(SQL));
  LSQL   := RemoveExtraSpaces(LSQL);
  if(Pos('CREATE TABLE',LSQL) = 1) then
    Result := sftCreateTable;
  if(Pos('DROP TABLE',LSQL) = 1) then
    Result := sftDropTable;
end;

end.
