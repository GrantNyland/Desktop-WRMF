library ModelStomsa;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}
{$R 'StomsaSQL.res' 'StomsaSQL.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UStomsaModelManager,
  UErrorHandlingOperations;

function ConstructDLLObject(var AObject: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean; export; stdcall;
const OPNAME = 'ModelStomsa.ConstructDLLObject';
begin
  AObject := nil;
  Result := False;
  try
    HImagesInstance := AImagesInstance;
    AObject := TStomsaModelManager.Create(AAppModules);
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
