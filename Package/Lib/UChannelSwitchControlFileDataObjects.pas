//
//
//  UNIT      : Contains TChannelSwitchControlFileDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 08/03/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UChannelSwitchControlFileDataObjects;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UDataFileObjects,
  UAbstractObject;

type

  TChannelSwitchControlObject = class(TAbstractDataObject)
  protected
    // Line 2
    FSwitchType         : TInteger;
    FChannelNumber      : TInteger;
    FNodeNumber         : TInteger;
    FSwitchLevel        : TDouble;
    FSwitchInitialState : TInteger;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property SwitchType         : TInteger  read FSwitchType;
    property ChannelNumber      : TInteger  read FChannelNumber;
    property NodeNumber         : TInteger  read FNodeNumber ;
    property SwitchLevel        : TDouble   read FSwitchLevel;
    property SwitchInitialState : TInteger  read FSwitchInitialState;
  end;

  TChannelSwitchControlFileDataObject      = class(TAbstractDataObject)
  protected
    FFileNumber                   : integer;
    FSwitchDefFileName            : TString;
    FSwitchDefStartYear           : TInteger;
    FSwitchDefStartMonth          : TInteger;

    // Line 2
    FSwitchChannelControlList   : TObjectList;

    //Line 3... : Extra useless lines
    FFMExtraLines: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetChannelSwitchControlByIndex(AIndex: integer):TChannelSwitchControlObject;
  public
    constructor Create(AFileNumber: integer); reintroduce;
    function Initialise: boolean;override;
    procedure Reset;override;
    function AddChannelSwitchControl:TChannelSwitchControlObject;
    function ChannelSwitchControlCount: integer;
    property ChannelSwitchControlByIndex[AIndex: integer] : TChannelSwitchControlObject read GetChannelSwitchControlByIndex;
    property FMExtraLines                                 : TStringList                 read FFMExtraLines ;
    property FileNumber                                   : integer                     read FFileNumber;
    property SwitchDefFileName                            : TString                     read FSwitchDefFileName;
    property SwitchDefStartYear                           : TInteger                    read FSwitchDefStartYear;
    property SwitchDefStartMonth                          : TInteger                    read FSwitchDefStartMonth;
  end;
implementation

uses
  UErrorHandlingOperations;

{ TChannelSwitchControlObject }

procedure TChannelSwitchControlObject.CreateMemberObjects;
const OPNAME = 'TChannelSwitchControlObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    // Line 2
    FSwitchType              := TInteger.Create;
    FChannelNumber           := TInteger.Create;
    FNodeNumber              := TInteger.Create;
    FSwitchLevel             := TDouble.Create;
    FSwitchInitialState     := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlObject.DestroyMemberObjects;
const OPNAME = 'TChannelSwitchControlObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    // Line 2
    FChannelNumber.Free;
    FSwitchType.Free;
    FNodeNumber.Free;
    FSwitchLevel.Free;
    FSwitchInitialState.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlObject.Initialise: boolean;
const OPNAME = 'TChannelSwitchControlObject.Initialise';
begin
  Result := False;
  try
    FSwitchType.FData := 0;
    FSwitchType.FInitalised := False;
    FSwitchType.FLength := 4;
    FSwitchType.FDecimal := 0;
    FSwitchType.FDefaultPadding := True;

    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 6;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FNodeNumber.FData := 0;
    FNodeNumber.FInitalised := False;
    FNodeNumber.FLength := 6;
    FNodeNumber.FDecimal := 0;
    FNodeNumber.FDefaultPadding := True;

    FSwitchLevel.FData := 0;
    FSwitchLevel.FInitalised := False;
    FSwitchLevel.FLength := 8;
    FSwitchLevel.FDecimal := 2;
    FSwitchLevel.FDefaultPadding := True;
    FSwitchLevel.ShowDecimalPoint := True;

    FSwitchInitialState.FData := 0;
    FSwitchInitialState.FInitalised := False;
    FSwitchInitialState.FLength := 2;
    FSwitchInitialState.FDecimal := 0;
    FSwitchInitialState.FDefaultPadding := True;

    Result :=  True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlObject.Reset;
const OPNAME = 'TChannelSwitchControlObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TChannelSwitchControlFileDataObject }

procedure TChannelSwitchControlFileDataObject.CreateMemberObjects;
const OPNAME = 'TChannelSwitchControlFileDataObject.CreateMemberObjects';
begin
  inherited;
  try
    FSwitchDefFileName            := TString.Create;
    FSwitchDefStartYear           := TInteger.Create;
    FSwitchDefStartMonth          := TInteger.Create;
    FSwitchChannelControlList     := TObjectList.Create(True);
    FFMExtraLines                 := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlFileDataObject.DestroyMemberObjects;
const OPNAME = 'TChannelSwitchControlFileDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FSwitchDefFileName.Free;
    FSwitchDefStartYear.Free;
    FSwitchDefStartMonth.Free;
    FSwitchChannelControlList.Free;
    FFMExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlFileDataObject.AddChannelSwitchControl: TChannelSwitchControlObject;
const OPNAME = 'TChannelSwitchControlFileDataObject.AddChannelSwitchControl';
begin
  Result := nil;
  try
    Result := TChannelSwitchControlObject.Create;
    FSwitchChannelControlList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlFileDataObject.ChannelSwitchControlCount: integer;
const OPNAME = 'TChannelSwitchControlFileDataObject.ChannelSwitchControlCount';
begin
  Result := 0;
  try
    Result := FSwitchChannelControlList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlFileDataObject.GetChannelSwitchControlByIndex(AIndex: integer): TChannelSwitchControlObject;
const OPNAME = 'TChannelSwitchControlFileDataObject.GetChannelSwitchControlByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FSwitchChannelControlList.Count) then
      Result := TChannelSwitchControlObject(FSwitchChannelControlList.Items[AIndex])
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlFileDataObject.Initialise: boolean;
const OPNAME = 'TChannelSwitchControlFileDataObject.Initialise';
begin
  Result := False;
  try
    FSwitchDefStartYear.FData := 0;
    FSwitchDefStartYear.FInitalised := False;
    FSwitchDefStartYear.FLength := 4;
    FSwitchDefStartYear.FDecimal := 0;
    FSwitchDefStartYear.FDefaultPadding := True;

    FSwitchDefStartMonth.FData := 0;
    FSwitchDefStartMonth.FInitalised := False;
    FSwitchDefStartMonth.FLength := 4;
    FSwitchDefStartMonth.FDecimal := 0;
    FSwitchDefStartMonth.FDefaultPadding := True;

    FSwitchDefFileName.FData := '';
    FSwitchDefFileName.FInitalised := False;
    FSwitchDefFileName.FLength := 0;
    FSwitchDefFileName.FDecimal := 0;
    FSwitchDefFileName.FDefaultPadding := True;

    FSwitchChannelControlList.Clear;
    FFMExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlFileDataObject.Reset;
const OPNAME = 'TChannelSwitchControlFileDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TChannelSwitchControlFileDataObject.Create(AFileNumber: integer);
const OPNAME = 'TChannelSwitchControlFileDataObject.Create';
begin
  try
    FFileNumber := AFileNumber;
    inherited Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


