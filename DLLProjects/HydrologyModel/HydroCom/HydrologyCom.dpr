library HydrologyCom;

uses
  ShareMem,
  ComServ,
  HydrologyCom_TLB in 'HydrologyCom_TLB.pas',
  UHydrologyComObject in 'UHydrologyComObject.pas' {HydrologyComObject: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.res}

begin
end.
