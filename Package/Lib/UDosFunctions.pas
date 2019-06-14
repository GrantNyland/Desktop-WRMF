unit UDosFunctions;
interface
{****************************************************************}
{* Programmer:  Kevin S. Gallagher                              *}
{*                                                              *}
{* Language:    Delphi 3.00, 32 bit                             *}
{*              All code is within this one source file.        *}
{*                                                              *}
{* Description: Used to programmically create a 'ShortCut' to a *}
{*              DOS batch file. The ShortCut when invoked will  *}
{*              run in a minimized state. Location of newly     *}
{*              created ShortCut is in the same directory as    *}
{*              the batch file.                                 *}
{*                                                              *}
{* Comments:    It is up to the programmer to insure that all   *}
{*              commands called in the batch file are valid.    *}
{*                                                              *}
{* Suggestions: Attempt running the batch file under abnormal   *}
{*              conditions to see how things go, does the DOS   *}
{*              calls hang? etc.                                *}
{*                                                              *}
{* Error Codes: 0 = Success                                     *}
{*              1 = Either to many or not enough parameters     *}
{*              2 = File passed to this util, does not exist    *}
{*              3 = Failed to created ShortCut                  *}
{****************************************************************}
uses
  Windows, ShlObj,ShellApi, ActiveX, ComObj, SysUtils, VCL.Dialogs;

  function RunDosApplication(AWindowHandle: THandle; AApplicationFileName: string; AWinExe: boolean = False): boolean;
  function CreateShortcut(AApplicationFileName, ApplicationArguments, ApplicationWorkingDir: String): string;

implementation

function RunDosApplication(AWindowHandle: THandle; AApplicationFileName: string; AWinExe: boolean): boolean;
const OPNAME = 'RunDosApplication';
var
 LPifFile,
 LAppPath: string;
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  Result := False;
  if(AApplicationFileName <> '') and (AWindowHandle <> 0)  and FileExists(AApplicationFileName) then
  begin
    if AWinExe then
    begin
      LAppPath := ExtractFilePath(AApplicationFileName);
      FillChar(SI, SizeOf(SI), 0);
      SI.cb := SizeOf(SI);

      Result := CreateProcess(nil,
                             PChar(AApplicationFileName),
                             nil,
                             nil,
                             True,
                             NORMAL_PRIORITY_CLASS,
                             nil,
                             PChar(LAppPath),
                             SI,
                             PI);
      if Result then
      begin
        WaitForSingleObject(PI.hProcess,INFINITE);
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
    end
    else
    begin
      LPifFile := ChangeFileExt(AApplicationFileName,'.pif');
      if not FileExists(LPifFile) then
        LPifFile := ChangeFileExt(AApplicationFileName,'.lnk');

      if FileExists(LPifFile) then
      begin
        LAppPath := ExtractFilePath(LPifFile);
        FillChar(SI, SizeOf(SI), 0);
        SI.cb := SizeOf(SI);

        Result := CreateProcess(nil,
                               PChar(LPifFile),
                               nil,
                               nil,
                               True,
                               CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                               nil,
                               PChar(LAppPath),
                               SI,
                               PI);
        if Result then
        begin
          WaitForSingleObject(PI.hProcess,INFINITE);
          CloseHandle(PI.hThread);
          CloseHandle(PI.hProcess);
        end;


        //ShellExecute(AWindowHandle,'open',PChar(LPifFile),nil,nil, SW_SHOWNORMAL);
        //WinExec(PChar(LPifFile), SW_SHOWNORMAL)
      end
    end;
  end;
end;

function CreateShortcut(AApplicationFileName, ApplicationArguments, ApplicationWorkingDir: String): String;
const OPNAME = 'CreateShortcut';
var
  LInterface: IUnknown;
  LShellLink: IShellLink;
  LPersistentFile: IPersistFile;
  LApplicationNameWide: WideString;
  LShortCutName: string;
begin
  Result := '';
  //LShortCutName := ExtractFileName(AApplicationFileName);
  LShortCutName := AApplicationFileName;
  if(LShortCutName <> '') then
  begin
    LShortCutName := ChangeFileExt(LShortCutName,'.lnk');
    LInterface := CreateComObject(CLSID_ShellLink);
    LShellLink := LInterface as IShellLink;
    LPersistentFile := LInterface as IPersistFile;

    with LShellLink do
    begin
      SetPath(PChar(AApplicationFileName));
      SetArguments(PChar(ApplicationArguments));
      SetShowCmd(SW_SHOWMINIMIZED);
      SetWorkingDirectory(PChar(ApplicationWorkingDir));
    end;
    LApplicationNameWide := LShortCutName;
    LPersistentFile.Save(PWChar(LApplicationNameWide), False);

    if not FileExists(LShortCutName) then
      LShortCutName := ChangeFileExt(LShortCutName,'.pif');
    if FileExists(LShortCutName) then
      Result := LShortCutName;
  end;
end;

end.
