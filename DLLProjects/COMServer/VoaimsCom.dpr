library VoaimsCom;

uses
  ShareMem,
  ComServ,
  VoaimsCom_TLB in 'VoaimsCom_TLB.pas',
  UVoaimsComObject in 'UVoaimsComObject.pas' {VoaimsComObject: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R '\WRMFVersion.res' '..\..\WRMFVersion.rc'}

begin
end.
