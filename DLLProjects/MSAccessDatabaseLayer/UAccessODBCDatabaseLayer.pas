//
//
//  UNIT      : Contains TAccessODBCDatabaseLayer Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAccessODBCDatabaseLayer;

interface

uses
  Classes,
  Windows,
  UAbstractObject,
  UBDEDatabaseLayer;

type
  TAccessODBCDatabaseLayer = class(TBDEDatabaseLayer)
  protected
    FAliasName: string;
    function LoadIniData: boolean;
    procedure SetConnected(AConnected: boolean); override;
    function SetupODBC : boolean;
    procedure CheckDatabaseSize;
  public
    constructor Create(AAppModules : TAppModules);
    function ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean; override;
    function GetTableNames ( AContainer : TStrings ) : Boolean; override;

  end;

implementation

uses
  Registry,
  SysUtils,
  VCL.Dialogs,
  UUtilities,
  UErrorHandlingOperations;

function TAccessODBCDatabaseLayer.LoadIniData: boolean;
const OPNAME = 'TAccessODBCDatabaseLayer.LoadIniData';
begin
  Result := False;
  try

    // Load the name of the alias.
    FAliasName := FAppModules.IniFile.ReadString(ClassName, 'AliasName', '');
    if not AliasNameExist(FAliasName) then
    begin
      MessageDlg('Access Denied, User not registered, either register user or log onto PC as registered user ',mtError,[mbOK],0);
    end
    else
    begin
      // Set the database properties.
      FDatabase.LoginPrompt    := False;
      FDatabase.KeepConnection := True;
      FDatabase.AliasName      := FAliasName;
      FDatabase.DatabaseName   := FAliasName;

      // Done.
      Result := True;
    end;
  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAccessODBCDatabaseLayer.SetConnected(AConnected: boolean);
const
  OPNAME = 'TAccessODBCDatabaseLayer.SetConnected';
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

function TAccessODBCDatabaseLayer.ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean;
const OPNAME = 'TAccessODBCDatabaseLayer.ExecSQL';
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

constructor TAccessODBCDatabaseLayer.Create(AAppModules: TAppModules);
const OPNAME = 'TAccessODBCDatabaseLayer.Create';
begin
  try
    SetupODBC;
    inherited Create(AAppModules);
  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAccessODBCDatabaseLayer.SetupODBC: boolean;
const OPNAME = 'TAccessODBCDatabaseLayer.SetupODBC';
var
  LRegistry    : TRegistry;
  LSrcRegistry : TRegistry;
  LDBPath      : string;
begin
  Result := False;
  try
    LSrcRegistry := TRegistry.Create;
    try
      LSrcRegistry.RootKey := HKEY_CURRENT_USER;
      if LSrcRegistry.OpenKey('Software\ODBC\ODBC.INI\WRMFDatabase', False) then
      begin
        LSrcRegistry.CloseKey;
        Result := True;
      end
      else
      begin
        LRegistry := TRegistry.Create;
        try
          LRegistry.RootKey := HKEY_CURRENT_USER;

          // Check if there is MS Access support
          if LSrcRegistry.OpenKey('Software\ODBC\ODBC.INI\MS Access Database', False) then
          begin

            // Create the WRMFDatabase Data Sources
            if LRegistry.OpenKey('Software\ODBC\ODBC.INI\ODBC Data Sources', True) then
               LRegistry.WriteString('Vaal Database','Microsoft Access Driver (*.mdb)');
            LRegistry.CloseKey;

            LDBPath := ExtractFilePath(ApplicationExeName);
            if LRegistry.OpenKey('Software\ODBC\ODBC.INI\WRMFDatabase', True) then
            begin
              LRegistry.WriteString('DBQ', LDBPath + '\Database\WRMFDatabase.mdb');
              LRegistry.WriteString('Description','WRMFDatabase');
              LRegistry.WriteString('Driver', LSrcRegistry.ReadString('Driver'));
              LRegistry.WriteInteger('DriverId', LSrcRegistry.ReadInteger('DriverId'));
              LRegistry.WriteInteger('SafeTransactions', LSrcRegistry.ReadInteger('SafeTransactions'));
              LRegistry.WriteString('UID', LSrcRegistry.ReadString('UID'));
              LRegistry.WriteString('FIL','MS Access;');
            end;
            LRegistry.CloseKey;

            if LRegistry.OpenKey('Software\ODBC\ODBC.INI\WRMFDatabase\Engines\Jet', True) then
            begin
              LSrcRegistry.OpenKey('Software\ODBC\ODBC.INI\MS Access Database\Engines\Jet', False);
              LRegistry.WriteString('ImplicitCommitSync', LSrcRegistry.ReadString('ImplicitCommitSync'));
              LRegistry.WriteString('UserCommitSync', LSrcRegistry.ReadString('UserCommitSync'));
              LRegistry.WriteInteger('MaxBufferSize', 2048);
              LRegistry.WriteInteger('PageTimeout', 5);

              LRegistry.CloseKey;
            end;
            Result := True;
          end;
        finally
          LRegistry.Free;
        end;
      end;
    finally
      LSrcRegistry.Free;
    end;

  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAccessODBCDatabaseLayer.GetTableNames ( AContainer : TStrings ): Boolean;
const OPNAME = 'TAccessODBCDatabaseLayer.GetTableNames';
begin
  Result := False;
  try
    FDatabase.Close;
    FDatabase.Open;
    Result := inherited GetTableNames(AContainer);
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TAccessODBCDatabaseLayer.CheckDatabaseSize;
const OPNAME = 'TAccessODBCDatabaseLayer.CheckDatabaseSize';
var
  LRegistry : TRegistry;
  LKeyName   : String;
  LFileName  : String;
  LFileSize  : Int64;
begin
  try
    LFileName := '';
    LRegistry := TRegistry.Create(KEY_READ);
    try
      LRegistry.RootKey := HKEY_CURRENT_USER;
      LKeyName          := 'Software\ODBC\ODBC.INI\WRMFDatabase';
      LRegistry.OpenKey(LKeyName, False);
      LFileName := LRegistry.ReadString('DBQ');
    finally
      LRegistry.Free;
    end;
    
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
