unit UIsAcrobatReaderInstalled;

interface

function IsAcrobatReaderInstalled: boolean;

implementation

uses
  System.UITypes,
  Sysutils, VCL.Dialogs, Windows, Registry, UErrorHandlingOperations;

function IsAcrobatReaderInstalled: boolean;
const OPNAME = 'TStudyDocumentManager.IsAcrobatReaderInstalled';
var
  LTheRegistry: TRegistry;
  LMessage: string;
begin
  Result := False;
  try
    LTheRegistry:=TRegistry.Create;
    try
      LTheRegistry.RootKey:=HKEY_LOCAL_MACHINE;
      if LTheRegistry.OpenKeyReadOnly('\Software\Classes') then
        Result:=LTheRegistry.KeyExists('AcroExch.Document');
    finally
      LTheRegistry.Free;
    end;
    if not Result then
    begin
      LMessage := 'Acrobat Reader not installed';
      MessageDlg(LMessage,mtError,[mbOK],0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
 