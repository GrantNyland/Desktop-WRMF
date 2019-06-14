//  UNIT      : Contains TWetlandObject Class
//  AUTHOR    : Maurice Marinus
//  DATE      : 05/08/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UWetlandObject;

interface

uses
  Classes, Sysutils, Contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TWetland = class(TAbstractDataObject)
  protected
    FIdentifier         : TInteger;
    FNodeNumber         : TInteger;
    FName               : TString;
    FUpstreamThreshold  : TDouble;
    FStorageVolume      : TDouble;
    FInflowProportion   : TDouble;
    FOutflowProportion  : TDouble;

    procedure SetIdentifier(const Value: TInteger);
    procedure SetInflowProportion(const Value: TDouble);
    procedure SetName(const Value: TString);
    procedure SetNodeNumber(const Value: TInteger);
    procedure SetOutflowProportion(const Value: TDouble);
    procedure SetStorageVolume(const Value: TDouble);
    procedure SetUpstreamThreshold(const Value: TDouble);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset; override;
    function Initialise: Boolean; override;

    property Identifier         : TInteger  read FIdentifier        write SetIdentifier;
    property NodeNumber         : TInteger  read FNodeNumber        write SetNodeNumber;
    property Name               : TString   read FName              write SetName;
    property StorageVolume      : TDouble   read FStorageVolume     write SetStorageVolume;
    property InflowProportion   : TDouble   read FInflowProportion  write SetInflowProportion;
    property OutflowProportion  : TDouble   read FOutflowProportion write SetOutflowProportion;
    property UpstreamThreshold  : TDouble   read FUpstreamThreshold write SetUpstreamThreshold;
  end;

  TWetlandObject = class(TAbstractDataObject)
  protected
    FExtraLines: TStringList;
    FWetlandObjectContainer : TObjectList;
    function GetWetlandByIndex(AIndex: Integer): TWetland;
    procedure SetExtraLines(const Value: TStringList);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: Boolean; override;
    procedure Reset;              override;
    function WetlandCount   : Integer;
    function AddWetland     : TWetland;
    property ExtraLines     : TStringList read FExtraLines write SetExtraLines;
    property WetlandObjectByIndex[AIndex: Integer] : TWetland read GetWetlandByIndex;
  end;

implementation

uses
  UErrorHandlingOperations;

{ TWetland }

procedure TWetland.CreateMemberObjects;
const OPNAME = 'TWetland.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier         := TInteger.Create;
    FNodeNumber         := TInteger.Create;
    FName               := TString.Create;
    FUpstreamThreshold  := TDouble.Create;
    FInflowProportion   := TDouble.Create;
    FStorageVolume      := TDouble.Create;
    FOutflowProportion  := TDouble.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWetland.DestroyMemberObjects;
const OPNAME = 'TWetland.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FNodeNumber.Free;
    FName.Free;
    FUpstreamThreshold.Free;
    FStorageVolume.Free;
    FInflowProportion.Free;
    FOutflowProportion.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetland.Initialise: Boolean;
const OPNAME = 'TWetland.Initialise';
begin
  Result := False;
  try
    FIdentifier.FData           := 0;
    FIdentifier.FInitalised     := False;
    FIdentifier.FLength         := 3;
    FIdentifier.FDefaultPadding := True;

    //Line 1
    FNodeNumber.FData           := 0;
    FNodeNumber.FInitalised     := False;
    FNodeNumber.FLength         := 5;
    FNodeNumber.FDefaultPadding := True;

    FName.FData           := '';
    FName.FInitalised     := False;
    FName.FLength         := 0;
    FName.FDefaultPadding := True;

    //Line 2
    FUpstreamThreshold.FData            := 0;
    FUpstreamThreshold.FInitalised      := False;
    FUpstreamThreshold.FLength          := 7;
    FUpstreamThreshold.FDefaultPadding  := True;
    FUpstreamThreshold.FDecimal         := 2;
    FUpstreamThreshold.ShowDecimalPoint := True;

    FInflowProportion.FData             := 0;
    FInflowProportion.FInitalised       := False;
    FInflowProportion.FLength           := 5;
    FInflowProportion.FDefaultPadding   := True;
    FInflowProportion.FDecimal          := 3;
    FInflowProportion.ShowDecimalPoint  := True;

    //Line 3
    FStorageVolume.FData                := 0;
    FStorageVolume.FInitalised          := False;
    FStorageVolume.FLength              := 7;
    FStorageVolume.FDefaultPadding      := True;
    FStorageVolume.FDecimal             := 2;
    FStorageVolume.ShowDecimalPoint     := True;

    FOutflowProportion.FData            := 0;
    FOutflowProportion.FInitalised      := False;
    FOutflowProportion.FLength          := 5;
    FOutflowProportion.FDefaultPadding  := True;
    FOutflowProportion.FDecimal         := 3;
    FOutflowProportion.ShowDecimalPoint := True;
    
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWetland.Reset;
const OPNAME = 'TWetland.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetland.SetIdentifier(const Value: TInteger);
const OPNAME = 'TWetland.SetIdentifier';
begin
  FIdentifier := Value;
end;

procedure TWetland.SetInflowProportion(const Value: TDouble);
const OPNAME = 'TWetland.SetInflowProportion';
begin
  FInflowProportion := Value;
end;

procedure TWetland.SetName(const Value: TString);
const OPNAME = 'TWetland.SetName';
begin
  FName := Value;
end;

procedure TWetland.SetNodeNumber(const Value: TInteger);
const OPNAME = 'TWetland.SetNodeNumber';
begin
  FNodeNumber := Value;
end;

procedure TWetland.SetOutflowProportion(const Value: TDouble);
const OPNAME = 'TWetland.SetOutflowProportion';
begin
  FOutflowProportion := Value;
end;

procedure TWetland.SetStorageVolume(const Value: TDouble);
const OPNAME = 'TWetland.SetStorageVolume';
begin
  FStorageVolume := Value;
end;

procedure TWetland.SetUpstreamThreshold(const Value: TDouble);
const OPNAME = 'TWetland.SetUpstreamThreshold';
begin
  FUpstreamThreshold := Value;
end;

{ TWetlandObject }

function TWetlandObject.AddWetland: TWetland;
const OPNAME = 'TWetlandObject.AddWetland';
begin
  Result := TWetland.Create;
  FWetlandObjectContainer.Add(Result);
end;

procedure TWetlandObject.CreateMemberObjects;
const OPNAME = 'TWetlandObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FExtraLines             := TStringlist.Create;
    FWetlandObjectContainer := TObjectList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWetlandObject.DestroyMemberObjects;
const OPNAME = 'TWetlandObject.DestroyMemberObjects';
begin
  try
    FWetlandObjectContainer.Clear;
    FreeAndNil(FWetlandObjectContainer);
    FExtraLines.Free;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWetlandObject.GetWetlandByIndex(AIndex: Integer): TWetland;
const OPNAME = 'TWetlandObject.GetWetlandByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FWetlandObjectContainer.Count) then
      Result :=  TWetland(FWetlandObjectContainer[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWetlandObject.Initialise: Boolean;
const OPNAME = 'TWetlandObject.Initialise';
begin
  Result := False;
  try
    FWetlandObjectContainer.Clear;
    FExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWetlandObject.Reset;
const OPNAME = 'TWetlandObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWetlandObject.SetExtraLines(const Value: TStringList);
const OPNAME = 'TWetlandObject.SetExtraLines';
begin
  FExtraLines := Value;
end;

function TWetlandObject.WetlandCount: Integer;
const OPNAME = 'TWetlandObject.WetlandCount';
begin
  Result := 0;
  try
    Result := FWetlandObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
