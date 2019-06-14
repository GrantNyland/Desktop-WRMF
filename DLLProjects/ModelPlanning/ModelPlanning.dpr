 library ModelPlanning;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UPlanningModelManager,
  UErrorHandlingOperations;

function ConstructDLLObject(var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean;  export; stdcall;
const OPNAME = 'ModelPlanning.ConstructDLLObject';
begin
  pao_Object := nil;
  Result := False;
  try
    //if(AAppModules.LicenceManager.ModelIsLicenced('PLANN')) then
    //begin
      HImagesInstance := AImagesInstance;
      pao_Object := TPlanningModelManager.Create(AAppModules);
      Result := True;
    //end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
