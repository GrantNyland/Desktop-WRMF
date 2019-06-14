library ModelRainfallData;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UErrorHandlingOperations,
  UModelRainfallDataManager;

function ConstructDLLObject(var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean;  export; stdcall;
const OPNAME = 'LanguageManager.ConstructDLLObject';
begin
  pao_Object := nil;
  Result := False;
  try
    //if(AAppModules.LicenceManager.ModelIsLicenced('RAINF')) then
    //begin
      HImagesInstance := AImagesInstance;
      pao_Object := TModelRainfallDataManager.Create( AAppModules );
      Result := True;
    //end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
