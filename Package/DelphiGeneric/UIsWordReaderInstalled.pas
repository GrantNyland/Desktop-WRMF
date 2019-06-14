unit UIsWordReaderInstalled;

interface

function IsWordReaderInstalled: boolean;

implementation

uses
  System.UITypes,
  Sysutils, VCL.Dialogs, Windows, Registry, UErrorHandlingOperations;

function IsWordReaderInstalled: boolean;
const OPNAME = 'TStudyDocumentManager.IsWordReaderInstalled';
var
  LTheRegistry: TRegistry;
  LMessage: string;
begin
  Result := False;
  try
    LTheRegistry:=TRegistry.Create;
    try
      LTheRegistry.RootKey:=HKEY_CLASSES_ROOT;
      Result:=LTheRegistry.KeyExists('Word.Application');
    finally
      LTheRegistry.Free;
    end;
    if not Result then
    begin
      LMessage := 'Microsft Word is not installed';
      MessageDlg(LMessage,mtError,[mbOK],0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
 