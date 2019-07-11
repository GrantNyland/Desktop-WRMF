 library ModelPlanning;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UPlanningModelManager,
  UErrorHandlingOperations;

function ConstructDLLObject(var AObject: TObject; AAppModules: TAppModules; AImagesInstance: Integer): Boolean;  export; stdcall;
const OPNAME = 'ModelPlanning.ConstructDLLObject';
begin
  AObject := nil;
  Result := False;
  try
    HImagesInstance := AImagesInstance;
    AObject := TPlanningModelManager.Create(AAppModules);
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
