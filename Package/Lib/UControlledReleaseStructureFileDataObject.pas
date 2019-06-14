unit UControlledReleaseStructureFileDataObject;

interface
uses
  Classes, sysutils,contnrs,
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UAbstractObject,
  UConstants;

type

  TControlledReleaseStructureFileDataObject  = class(TAbstractDataObject)
  protected
    FHDextraLines : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property HDextraLines : TStringList read FHDextraLines;

  end;

implementation
uses
  UErrorHandlingOperations;

procedure TControlledReleaseStructureFileDataObject.CreateMemberObjects;
CONST OPNAME = 'TControlledReleaseStructureFileDataObject.CreateMemberObjects';
begin
  try
    FHDextraLines := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TControlledReleaseStructureFileDataObject.DestroyMemberObjects;
CONST OPNAME = 'TControlledReleaseStructureFileDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FHDextraLines);
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TControlledReleaseStructureFileDataObject.Initialise: Boolean;
CONST OPNAME = 'TControlledReleaseStructureFileDataObject.Initialise';
begin
  Result := False;
  try
    FHDextraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TControlledReleaseStructureFileDataObject.Reset;
CONST OPNAME = 'TControlledReleaseStructureFileDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

 