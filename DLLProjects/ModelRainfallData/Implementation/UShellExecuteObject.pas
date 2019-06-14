unit UShellExecuteObject;

interface
                                                      
uses
  Windows, SysUtils, VCL.Forms, VCL.Dialogs;

type
  TShellExecuteObject = class(TObject)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    class procedure BrowseCD;
    class function ExecuteShellAction(const AAction, AExeName, AParmList, ADefaultDir : string) : integer;
  end;

implementation

uses
  UErrorHandlingOperations,
  ShellApi;

{ TShellExecuteObject }

class procedure TShellExecuteObject.BrowseCD;
begin
  ExecuteShellAction('Explore',ExtractFileDrive(ApplicationExeName) + '\' ,'','')
end;

class function TShellExecuteObject.ExecuteShellAction(const AAction, AExeName, AParmList,
  ADefaultDir: string): integer;
var
  LMessage : string;
begin
  Result := ShellExecute(0, pChar(AAction),  pChar (AExeName),  pChar (AParmList), pChar (ADefaultDir), SW_SHOWNORMAL);
  if Result <= 32 then
  begin
    case Result of
            0                      :   LMessage := 'Out of memory/resources';
            SE_ERR_OOM             :   LMessage := 'Out of memory/resources';
            ERROR_FILE_NOT_FOUND   :   LMessage := 'File "' + AExeName + '" not found';
            ERROR_PATH_NOT_FOUND   :   LMessage := 'Path not found';
            ERROR_BAD_FORMAT       :   LMessage := 'Damaged or invalid exe';
            SE_ERR_ACCESSDENIED    :   LMessage := 'Access denied';
            SE_ERR_NOASSOC         :   LMessage := 'Filename association invalid';
            SE_ERR_DDEBUSY         :   LMessage := 'DDE error';
            SE_ERR_ASSOCINCOMPLETE :   LMessage := 'Filename association invalid';
            SE_ERR_DDEFAIL         :   LMessage := 'DDE error';
            SE_ERR_SHARE           :   LMessage := 'Sharing violation';
            SE_ERR_DDETIMEOUT      :   LMessage := 'DDE error';
     else LMessage := 'No Text (*Untrapped error)';
     end; // of case
     raise Exception.Create ('Error #' + IntToStr (Result) + ': ' + LMessage);
  end;
end;

end.

