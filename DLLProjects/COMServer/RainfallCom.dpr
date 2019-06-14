library RainfallCom;

uses
  ShareMem,
  ComServ,
  RainfallCom_TLB in 'RainfallCom_TLB.pas',
  RainfallComObject in 'RainfallComObject.pas' {RainfallComObject: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
