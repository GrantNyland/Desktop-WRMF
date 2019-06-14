library GridEditor;

{$R 'WRMFVersion.res' '..\..\WRMFVersion.rc'}

uses
  ShareMem,
  SysUtils,
  UAbstractObject,
  UGridEditorManager,
  UErrorHandlingOperations;

function ConstructDLLObject(var pao_Object: TObject; AAppModules: TAppModules; AImagesInstance: integer): boolean;  export; stdcall;
const OPNAME = 'GridEditor.ConstructDLLObject';
begin
  pao_Object := nil;
  Result := False;
  try
    HImagesInstance := AImagesInstance;
    pao_Object := TGridEditorManager.Create(AAppModules);
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

exports
  ConstructDLLObject;

begin
end.
