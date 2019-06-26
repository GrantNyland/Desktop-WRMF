library VoaimsCom;

uses
  ShareMem, ComServ,
  UVoaimsComObject;

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R 'WRMFVersion.res' '..\WRMFVersion.rc'}
{$IFDEF MERGE_DLLS}
{$R 'StomsaSQL.res' '..\DLLProjects\ModelStomsa\StomsaSQL.rc'}
{$ENDIF}

begin
end.
