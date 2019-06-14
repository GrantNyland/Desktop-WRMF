//
//
//  UNIT      : Contains TReservoirHydrologyFilesObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 25/02/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UReservoirHydrologyFilesObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UAbstractObject,
  UConstants;

type
  TReservoirHydrologyFile = class(TAbstractDataObject)
  protected
  public
    FCatchRef   :Integer;
    FFileName   :String;
    procedure Reset;override;
    function AssignFrom(ANetworkElement: TReservoirHydrologyFile): Boolean;
    function Initialise: boolean;override;
  end;

  TReservoirHydrologyFilesObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FReservoirHydrologyFiles : TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddHydrologyFile(ACatchRef   :Integer; AFileName   :String): boolean;
  end;

implementation

uses UErrorHandlingOperations;

{ TReservoirHydrologyFile }

function TReservoirHydrologyFile.AssignFrom(ANetworkElement: TReservoirHydrologyFile): Boolean;
const OPNAME = 'TReservoirHydrologyFile.AssignFrom';
begin
  Result := False;
  try
    if Assigned(ANetworkElement) then
    begin
      FCatchRef := ANetworkElement.FCatchRef;
      FFileName := ANetworkElement.FFileName;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirHydrologyFile.Initialise: boolean;
const OPNAME = 'TReservoirHydrologyFile.Initialise';
Begin
  Result := False;
  try
    FCatchRef   := 0;
    FFileName   := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirHydrologyFile.Reset;
const OPNAME = 'TReservoirHydrologyFile.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TReservoirHydrologyFilesObject }

procedure TReservoirHydrologyFilesObject.CreateMemberObjects;
const OPNAME = 'TReservoirHydrologyFilesObject.CreateMemberObjects';
begin
  try
    FReservoirHydrologyFiles  := TObjectList.Create(True);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirHydrologyFilesObject.DestroyMemberObjects;
const OPNAME = 'TReservoirHydrologyFilesObject.DestroyMemberObjects';
begin
  try
    FReservoirHydrologyFiles.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirHydrologyFilesObject.Initialise: boolean;
const OPNAME = 'TReservoirHydrologyFilesObject.Initialise';
Begin
  Result := False;
  try
    Reset;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirHydrologyFilesObject.Reset;
const OPNAME = 'TReservoirHydrologyFilesObject.Reset';
Begin
  try
    FReservoirHydrologyFiles.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirHydrologyFilesObject.AddHydrologyFile(ACatchRef   :Integer; AFileName   :String): boolean;
const OPNAME = 'TReservoirHydrologyFilesObject.AddHydrologyFile';
var
  LReservoirHydrologyFile :TReservoirHydrologyFile;
Begin
  Result := False;
  try
    LReservoirHydrologyFile := TReservoirHydrologyFile.Create;
    LReservoirHydrologyFile.FCatchRef := ACatchRef;
    LReservoirHydrologyFile.FFileName := AFileName;
    FReservoirHydrologyFiles.Add(LReservoirHydrologyFile);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
