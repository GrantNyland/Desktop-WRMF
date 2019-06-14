//
//
//  UNIT      : Contains TSFRFileObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 22/11/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit USFRFileObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type


  TSFRObject = class(TAbstractDataObject)
  protected
    FInflowNodeNumber     : TInteger;
    FCoveredArea          : TDouble;
    FUnitRunoffFileName   : TString;
    FSoilMoistureFileName : TString;
    FSFRName             : TString;
    FSFRDescr            : TString;
    FComment1            : TString;
    FComment2            : TString;
    FComment3            : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property InflowNodeNumber     : TInteger read FInflowNodeNumber       write FInflowNodeNumber;
    property CoveredArea          : TDouble  read FCoveredArea            write FCoveredArea;
    property UnitRunoffFileName   : TString  read FUnitRunoffFileName     write FUnitRunoffFileName;
    property SoilMoistureFileName : TString  read FSoilMoistureFileName   write FSoilMoistureFileName;
    property SFRName             : TString  read FSFRName               write FSFRName;
    property SFRDescr            : TString  read FSFRDescr              write FSFRDescr;
    property Comment1             : TString  read FComment1               write FComment1;
    property Comment2             : TString  read FComment2               write FComment2;
    property Comment3             : TString  read FComment3               write FComment3;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TSFRFileObject  = class(TAbstractDataObject)
  protected
    FSFRObjectContainer : TObjectList;
    FComment: TStringlist;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetSFRObjectByIndex(AIndex: integer):TSFRObject;
  public
    procedure Reset;override;
    function SFRCount: integer;
    function Initialise: boolean;override;
    function AddSFRObject:TSFRObject;
    property SFRObjectByIndex[AIndex: integer] :TSFRObject read GetSFRObjectByIndex;
    property Comment :TStringlist read FComment;
  end;

implementation


uses UErrorHandlingOperations;

{TSFRObject}

procedure TSFRObject.CreateMemberObjects;
const OPNAME = 'TSFRObject.CreateMemberObjects';
Begin
  try
    FInflowNodeNumber         := TInteger.Create;
    FCoveredArea              := TDouble.Create;
    FUnitRunoffFileName       := TString.Create;
    FSoilMoistureFileName     := TString.Create;
    FSFRName                 := TString.Create;
    FSFRDescr                := TString.Create;
    FComment1                 := TString.Create;
    FComment2                 := TString.Create;
    FComment3                 := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSFRObject.DestroyMemberObjects;
const OPNAME = 'TSFRObject.DestroyMemberObjects';
Begin
  try
    FInflowNodeNumber.Free;
    FCoveredArea.Free;
    FUnitRunoffFileName.Free;
    FSoilMoistureFileName.Free;
    FSFRName.Free;
    FSFRDescr.Free;
    FComment1.Free;
    FComment2.Free;
    FComment3.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSFRObject.Reset;
const OPNAME = 'TSFRObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSFRObject.Initialise: boolean;
const OPNAME = 'TSFRObject.Initialise';
Begin
  Result := False;
  try
    FInflowNodeNumber.FData := 0;
    FInflowNodeNumber.FInitalised := False;
    FInflowNodeNumber.FLength := 2;
    FInflowNodeNumber.FDecimal := 0;
    FInflowNodeNumber.FDefaultPadding := True;


    FCoveredArea.FData := 0.0;
    FCoveredArea.FInitalised := False;
    FCoveredArea.FLength := 10;
    FCoveredArea.FDecimal := 3;
    FCoveredArea.FDefaultPadding := True;
    FCoveredArea.ShowDecimalPoint := True;

    FUnitRunoffFileName.FData := '';
    FUnitRunoffFileName.FInitalised := False;
    FUnitRunoffFileName.FLength := 255;
    FUnitRunoffFileName.FDecimal := 0;
    FUnitRunoffFileName.FDefaultPadding := True;

    FSoilMoistureFileName.FData := '';
    FSoilMoistureFileName.FInitalised := False;
    FSoilMoistureFileName.FLength := 255;
    FSoilMoistureFileName.FDecimal := 0;
    FSoilMoistureFileName.FDefaultPadding := True;

    FSFRName.FData := '';
    FSFRName.FInitalised := False;
    FSFRName.FLength := 255;
    FSFRName.FDecimal := 0;
    FSFRName.FDefaultPadding := True;

    FSFRDescr.FData := '';
    FSFRDescr.FInitalised := False;
    FSFRDescr.FLength := 255;
    FSFRDescr.FDecimal := 0;
    FSFRDescr.FDefaultPadding := True;

    FComment1.FData := '';
    FComment1.FInitalised := False;
    FComment1.FLength := 255;
    FComment1.FDecimal := 0;
    FComment1.FDefaultPadding := True;

    FComment2.FData := '';
    FComment2.FInitalised := False;
    FComment2.FLength := 255;
    FComment2.FDecimal := 0;
    FComment2.FDefaultPadding := True;

    FComment3.FData := '';
    FComment3.FInitalised := False;
    FComment3.FLength := 255;
    FComment3.FDecimal := 0;
    FComment3.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSFRFileObject }


procedure TSFRFileObject.CreateMemberObjects;
const OPNAME = 'TSFRFileObject.CreateMemberObjects';
begin
  try
    inherited;
    FSFRObjectContainer := TObjectList.Create(True);
    FComment             := TStringlist.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSFRFileObject.DestroyMemberObjects;
const OPNAME = 'TSFRFileObject.DestroyMemberObjects';
begin
  try
    inherited;
    FSFRObjectContainer.Clear;
    FSFRObjectContainer.Free;
    FComment.Clear;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSFRFileObject.AddSFRObject: TSFRObject;
const OPNAME = 'TSFRFileObject.AddSFRObject';
begin
  Result := nil;
  try
    Result := TSFRObject.Create;
    FSFRObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSFRFileObject.GetSFRObjectByIndex(AIndex: integer): TSFRObject;
const OPNAME = 'TSFRFileObject.GetSFRObjectByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FSFRObjectContainer.Count) then
      Result := TSFRObject(FSFRObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSFRFileObject.Initialise: boolean;
const OPNAME = 'TSFRFileObject.Initialise';
begin
  Result := False;
  try
    FSFRObjectContainer.Clear;
    FComment.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSFRFileObject.Reset;
const OPNAME = 'TSFRFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSFRFileObject.SFRCount: integer;
const OPNAME = 'TSFRFileObject.SFRCount';
begin
  Result := 0;
  try
    Result := FSFRObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
