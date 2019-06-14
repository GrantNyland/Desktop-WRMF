//
//  UNIT      : Contains TMonthlyWaterRequirementFileDataObjects Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UMonthlyWaterRequirementFileDataObjects;

interface
uses
  Classes, sysutils,contnrs,
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UAbstractObject,
  UConstants;

type

  TMonthlyWaterRequirementFileDataObjects  = class(TAbstractDataObject)
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

{ TMonthlyWaterRequirementFileDataObjects }

procedure TMonthlyWaterRequirementFileDataObjects.CreateMemberObjects;
CONST OPNAME = 'TMonthlyWaterRequirementFileDataObjects.CreateMemberObjects';
begin
  try
    FHDextraLines := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyWaterRequirementFileDataObjects.DestroyMemberObjects;
CONST OPNAME = 'TMonthlyWaterRequirementFileDataObjects.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FHDextraLines);
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TMonthlyWaterRequirementFileDataObjects.Initialise: Boolean;
CONST OPNAME = 'TMonthlyWaterRequirementFileDataObjects.Initialise';
begin
  Result := False;
  try
    FHDextraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TMonthlyWaterRequirementFileDataObjects.Reset;
CONST OPNAME = 'TMonthlyWaterRequirementFileDataObjects.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
