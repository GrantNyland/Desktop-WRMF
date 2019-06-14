library Changes;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UChangeManager,
  UErrorHandlingOperations;

function ConstructDLLObject(var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean;  export; stdcall;
const OPNAME = 'StudyDocuments.ConstructDLLObject';
begin
  pao_Object := nil;
  Result := False;
  try
    HImagesInstance := AImagesInstance;
    pao_Object := TChangeManager.Create(AAppModules);
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
