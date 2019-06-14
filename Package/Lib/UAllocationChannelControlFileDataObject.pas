unit UAllocationChannelControlFileDataObject;

interface
uses
  Classes, sysutils,contnrs,
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UAbstractObject,
  UConstants;

type

  TAllocationChannelControlFileDataObject  = class(TAbstractDataObject)
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

{ TAllocationChannelControlFileDataObject }

procedure TAllocationChannelControlFileDataObject.CreateMemberObjects;
CONST OPNAME = 'TAllocationChannelControlFileDataObject.CreateMemberObjects';
begin
  try
    FHDextraLines := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationChannelControlFileDataObject.DestroyMemberObjects;
CONST OPNAME = 'TAllocationChannelControlFileDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FHDextraLines);
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TAllocationChannelControlFileDataObject.Initialise: Boolean;
CONST OPNAME = 'TAllocationChannelControlFileDataObject.Initialise';
begin
  Result := False;
  try
    FHDextraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TAllocationChannelControlFileDataObject.Reset;
CONST OPNAME = 'TAllocationChannelControlFileDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

