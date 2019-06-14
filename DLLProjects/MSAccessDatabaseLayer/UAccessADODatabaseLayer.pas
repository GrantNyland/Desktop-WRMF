//
//
//  UNIT      : Contains TAccessODBCDatabaseLayer Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAccessADODatabaseLayer;

interface

uses
  Classes,
  Windows,
  UAbstractObject,
  UInstalledFileInfoObjectList,
  UADODatabaseLayer;

type
  TAccessADODatabaseLayer = class(TADODatabaseLayer)
  protected
    function LoadIniData: boolean;
    procedure SetConnected(AConnected: boolean); override;
    function SetupDatabasePath : boolean;
    procedure CheckDatabaseSize;
    procedure CopyWritableFilesToAppData(AFileInfoObjectList : TInstalledFileInfoObjectList; APath,AAppDataDir : string);
  public
    constructor Create(AAppModules : TAppModules);
    destructor Destroy; override;
    function ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean; override;
    function GetTableNames ( AContainer : TStrings ) : Boolean; override;

  end;

implementation

uses
  System.UITypes,
  Registry,
  SysUtils,
  VCL.Dialogs,
  Vcl.Forms,
  UUtilities,
  UErrorHandlingOperations;

procedure TAccessADODatabaseLayer.CopyWritableFilesToAppData(AFileInfoObjectList : TInstalledFileInfoObjectList; APath,AAppDataDir : string);
const OPNAME = 'TAccessADODatabaseLayer.Create';
var
  LSearchRec: TSearchRec;
  LMoreFiles: boolean;
  LFileData : TFileDataObject;
  LSearchStr: string;
  LAttributes : word;
  LWritableInstalledFiles : TInstalledFiles;
begin
  try
    Application.ProcessMessages;
    if (Length(Trim(APath)) <> 0) then
    begin
      LFileData := TFileDataObject.Create;
      LFileData.IsDirectory := True;
      APath := IncludeTrailingPathDelimiter(APath);
      LFileData.FileName    := APath;
      APath := ExcludeTrailingPathDelimiter(APath);
      APath := IncludeTrailingPathDelimiter(APath);
      LSearchStr := IncludeTrailingPathDelimiter(APath)+ '*.*';
      LMoreFiles := (FindFirst(LSearchStr, faAnyFile, LSearchRec) = 0);
      while LMoreFiles do
      begin
        if (LSearchRec.Name[1] <> '.') and (LSearchRec.Name <> 'Bin') and (LSearchRec.Name <> 'Document')
          and (LSearchRec.Name <> 'Dos') and (LSearchRec.Name <> 'Help') and (LSearchRec.Name <> 'Help')
          and (LSearchRec.Name <> 'Html Help') and (LSearchRec.Name <> 'HydroXSD')
          and (LSearchRec.Name <> 'Reports') and (LSearchRec.Name <> 'Stomsa')
          and (LSearchRec.Name <> 'WRPM') and (LSearchRec.Name <> 'WRYM') and (LSearchRec.Name <> 'WRMf.exe')then
        begin
        {$WARN SYMBOL_PLATFORM OFF}
          if((LSearchRec.Attr and faDirectory) <> 0) and
            ((LSearchRec.Attr and not SysUtils.faSysFile) <> 0) and
            ((LSearchRec.Attr and not SysUtils.faReadOnly) <> 0) then
          begin
              if  not DirectoryExists(AAppDataDir) then
                ForceDirectories(AAppDataDir);
              CopyWritableFilesToAppData(AFileInfoObjectList, APath + LSearchRec.Name, AAppDataDir+'\'+LSearchRec.Name);
          end
          else
          begin
            if((ExtractFileExt(LSearchRec.Name)) <> '') then
            begin
              LAttributes := SysUtils.FileGetAttr(APath + LSearchRec.Name);
              if ((LAttributes and not SysUtils.faReadOnly) <> 0) or
                 ((LAttributes and not SysUtils.faSysFile) <> 0)  or
                 ((LAttributes and not SysUtils.faArchive)  <> 0) or
                 ((LAttributes and not SysUtils.faHidden)  <> 0) then
               begin
                 LWritableInstalledFiles := AFileInfoObjectList.WriterbleFileByName[LSearchRec.Name];
                 if LWritableInstalledFiles <> nil then
                 begin
                   if  not DirectoryExists(AAppDataDir) then
                     ForceDirectories(AAppDataDir);
                   CopyFile(PChar(APath + LSearchRec.Name), PChar(AAppDataDir+'\'+LSearchRec.Name), False);
                 end;
               end;
            end;
          end;
        end;
        LMoreFiles := (FindNext(LSearchRec) = 0);
        {$WARN SYMBOL_PLATFORM ON}
      end;
      SysUtils.FindClose(LSearchRec);
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

constructor TAccessADODatabaseLayer.Create(AAppModules: TAppModules);
const OPNAME = 'TAccessADODatabaseLayer.Create';
begin
  try
    inherited Create(AAppModules);
    SetupDatabasePath;
  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TAccessADODatabaseLayer.Destroy;
const OPNAME = 'TAccessADODatabaseLayer.Create';
begin
  try
    inherited Destroy;
  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAccessADODatabaseLayer.SetupDatabasePath: boolean;
const OPNAME = 'TAccessADODatabaseLayer.SetupDatabasePath';
var
  LDBName,
  LOldPath,
  LNewPath,
  LConnectionString : string;
  LAppDataDir : string;
  LInstalledFileInfoObjectList : TInstalledFileInfoObjectList;
  LLaunched : integer;
begin
  Result := False;
  try
    LInstalledFileInfoObjectList := TInstalledFileInfoObjectList.Create;
    LInstalledFileInfoObjectList.Initialise;
    LAppDataDir := GetAppDataLocalDir;
    LLaunched := FAppModules.IniFile.ReadInteger('LaunchCounter','Launched', 0);

    if LLaunched = 0 then //not FileExists(LAppDataDir+'\Database\WRMFDatabase.mdb') then
      CopyWritableFilesToAppData(LInstalledFileInfoObjectList,ExtractFilePath(ApplicationExeName),LAppDataDir);

    FAppModules.IniFile.WriteInteger('LaunchCounter','Launched',LLaunched+1);

    LConnectionString := FAppModules.IniFile.ReadString('TAccessADODatabaseLayer','WRMFDatabase','');
    LDBName           := ExtractDBFileNameFromADOConnectionStr(LConnectionString);
    if(LDBName <> '') then
    begin
      LOldPath   := ExtractFilePath(LDBName);
      LNewPath   := LAppDataDir + '\Database\';  //ExtractFilePath(ApplicationExeName) + 'Database\';
      if(UpperCase(LOldPath) <> UpperCase(LNewPath)) then
      begin
        LConnectionString := StringReplace(LConnectionString,LOldPath,LNewPath,[rfIgnoreCase]);
        FAppModules.IniFile.WriteString('TAccessADODatabaseLayer','WRMFDatabase',LConnectionString);
      end;
    end;
    FreeAndNil(LInstalledFileInfoObjectList);
    Result := True;
  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAccessADODatabaseLayer.LoadIniData: boolean;
const OPNAME = 'TAccessADODatabaseLayer.LoadIniData';
var
  LConnectionName   : string;
  LConnectionString : string;
  LDatabaseFileName : string;
begin
  Result := False;
  try
    LConnectionName   := 'WRMFDatabase';
    LConnectionString := FAppModules.IniFile.ReadString(ClassName, LConnectionName, '');
    LDatabaseFileName := ExtractDBFileNameFromADOConnectionStr(LConnectionString);
    FDBConnection.Populate(LConnectionName,LConnectionString,LDatabaseFileName);
    // Load the name of the alias.
//    if not FileExists(FDBConnection.DatabaseFileName) then
//    begin
//      MessageDlg('Database specified in the configuration file [' + FAppModules.IniFile.FileName + '] - [' + FDBConnection.DatabaseFileName + '] does not exist.', mtError,[mbOK],0);
//    end
//    else
//    begin
      // Set the database properties.
      FDatabase.LoginPrompt      := False;
      FDatabase.KeepConnection   := True;
      FDatabase.ConnectionString := FDBConnection.ConnectionString;
      FDatabase.Name             := FDBConnection.ConnectionName;

      // Done.
      Result := True;
//    end;
  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAccessADODatabaseLayer.SetConnected(AConnected: boolean);
const
  OPNAME = 'TAccessADODatabaseLayer.SetConnected';
begin
  try

    // Check whether to connect or disconnect.
    FDatabase.Connected := False;
    if AConnected then
    begin

      // Load ini data.
      if LoadIniData then
      begin

        // Attempt the connection.
        FDatabase.Connected := True;
        CheckDatabaseSize;
      end;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAccessADODatabaseLayer.ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean;
const OPNAME = 'TAccessADODatabaseLayer.ExecSQL';
var LIndex: integer;
begin
  Result := False;
  try

    // Change blob to memo
    if (Pos('CREATE TABLE', ASQL) > 0) then
    begin
      repeat
        LIndex := Pos('BLOB', UpperCase(ASQL));
        if (LIndex > 0) then
        begin
          Delete(ASQL, LIndex, 4);
          Insert('memo', ASQL, LIndex);
        end;
      until (LIndex <= 0);
    end;

    // Call the ancestor.
    Result := inherited ExecSQL(ASQL, AShowErrors);

  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAccessADODatabaseLayer.GetTableNames ( AContainer : TStrings ): Boolean;
const OPNAME = 'TAccessADODatabaseLayer.GetTableNames';
begin
  Result := False;
  try
    FDatabase.Close;
    FDatabase.Open;
    Result := inherited GetTableNames(AContainer);
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TAccessADODatabaseLayer.CheckDatabaseSize;
const OPNAME = 'TAccessADODatabaseLayer.CheckDatabaseSize';
var
  LFileName  : String;
  LFileSize  : Int64;
begin
  try
    LFileName := FDBConnection.DatabaseFileName;
    if FileExists(LFileName) then
    begin
      LFileSize := GetFileSize(LFileName);
      if(LFileSize > 2000000000) then
         ShowMessage('Your Access Database is over the 2 GigaBytes limit.'+
                     #13+'Microsoft restrict Access databases to 2 GigaBytes'+
                     #13+'New inserts or updates will not go through.'+
                     #13+'Please reduce the size of the database. e.g Delete records or Compact the database');
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
