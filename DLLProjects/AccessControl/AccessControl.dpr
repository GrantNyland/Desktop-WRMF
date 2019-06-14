library AccessControl;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UAccessControlManager,
  UErrorHandlingOperations,
  UUserPasswordChangeDialog in 'UUserPasswordChangeDialog.pas' {UserPasswordChangeDialog};

function ConstructDLLObject(var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean;  export; stdcall;
const OPNAME = 'AccessControl.ConstructDLLObject';
begin
  pao_Object := nil;
  Result := False;
  try
    HImagesInstance := AImagesInstance;
    pao_Object := TAccessControlManager.Create(AAppModules);
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
