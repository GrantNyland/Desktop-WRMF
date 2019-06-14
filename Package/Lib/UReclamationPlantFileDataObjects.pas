//
//  UNIT      : Contains TReclamationPlantFileDataObjects Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UReclamationPlantFileDataObjects;

interface
uses
  Classes, sysutils,contnrs,
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UAbstractObject,
  UConstants;

type

  TReclamationPlantFileDataObjects  = class(TAbstractDataObject)
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

{ TReclamationPlantFileDataObjects }

procedure TReclamationPlantFileDataObjects.CreateMemberObjects;
CONST OPNAME = 'TReclamationPlantFileDataObjects.CreateMemberObjects';
begin
  try
    FHDextraLines := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReclamationPlantFileDataObjects.DestroyMemberObjects;
CONST OPNAME = 'TReclamationPlantFileDataObjects.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FHDextraLines);
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TReclamationPlantFileDataObjects.Initialise: Boolean;
CONST OPNAME = 'TReclamationPlantFileDataObjects.Initialise';
begin
  Result := False;
  try
    FHDextraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TReclamationPlantFileDataObjects.Reset;
CONST OPNAME = 'TReclamationPlantFileDataObjects.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
