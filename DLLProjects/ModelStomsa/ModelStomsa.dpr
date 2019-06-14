library ModelStomsa;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}
{$R 'StomsaSQL.res' 'StomsaSQL.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UStomsaModelManager,
  UErrorHandlingOperations;

function ConstructDLLObject(var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean;  export; stdcall;
const OPNAME = 'ModelStomsa.ConstructDLLObject';
begin
  pao_Object := nil;
  Result := False;
  try
    HImagesInstance := AImagesInstance;
    pao_Object := TStomsaModelManager.Create(AAppModules);
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
