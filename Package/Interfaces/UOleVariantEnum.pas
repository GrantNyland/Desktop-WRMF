unit UOleVariantEnum;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.win.StdVCL, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.OleServer, VCL.AxCtrls, Winapi.ActiveX;


type
  IOleVariantEnum  = interface
    function  GetCurrent: OLEVariant;
    function  MoveNext: Boolean;
    property  Current: OLEVariant read GetCurrent;
  end;

  IGetOleVariantEnum = interface
    function GetEnumerator: IOleVariantEnum;
  end;

  TOleVariantEnum = class(TInterfacedObject, IOleVariantEnum, IGetOleVariantEnum)
  private
    FCurrent : OLEVariant;
    FEnum    : IEnumVARIANT;
  public
    function GetEnumerator: IOleVariantEnum;
    constructor Create(Collection: OLEVariant);
    function  GetCurrent: OLEVariant;
    function  MoveNext: Boolean;
    property  Current: OLEVariant read GetCurrent;
  end;

  TOleVariantArrayEnum = class(TInterfacedObject, IOleVariantEnum, IGetOleVariantEnum)
  private
    FCollection : OLEVariant;
    FIndex      : Integer;
    FLowBound   : Integer;
    FHighBound  : Integer;
  public
    function GetEnumerator: IOleVariantEnum;
    constructor Create(Collection: OLEVariant);
    function  GetCurrent: OLEVariant;
    function  MoveNext: Boolean;
    property  Current: OLEVariant read GetCurrent;
  end;


function GetOleVariantEnum(Collection:OleVariant):IGetOleVariantEnum;
function GetOleVariantArrEnum(Collection:OleVariant):IGetOleVariantEnum;


implementation

uses
  UErrorHandlingOperations;

function GetOleVariantEnum(Collection:OleVariant):IGetOleVariantEnum;
const OPNAME = 'GetOleVariantEnum';
begin
  try
    Result := TOleVariantEnum.Create(Collection);
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function GetOleVariantArrEnum(Collection:OleVariant):IGetOleVariantEnum;
const OPNAME = 'GetOleVariantArrEnum';
begin
  try
    Result := TOleVariantArrayEnum.Create(Collection);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TOleVariantEnum }

constructor TOleVariantEnum.Create(Collection: OLEVariant);
const OPNAME = 'TOleVariantEnum.Create';
begin
  try
  inherited Create;
    FEnum := IUnknown(Collection._NewEnum) As IEnumVARIANT;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOleVariantEnum.GetCurrent: OLEVariant;
const OPNAME = 'TOleVariantEnum.GetCurrent';
begin
  try
    Result := FCurrent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOleVariantEnum.GetEnumerator: IOleVariantEnum;
const OPNAME = 'TOleVariantEnum.GetEnumerator';
begin
  try
    Result:=Self;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOleVariantEnum.MoveNext: Boolean;
const OPNAME = 'TOleVariantEnum.MoveNext';
var
  iValue        : LongWord;
begin
  Result := False;
  try
    FCurrent := Unassigned;//avoid memory leaks
    Result := FEnum.Next(1, FCurrent, iValue) = S_OK;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TOleVariantArrayEnum }

constructor TOleVariantArrayEnum.Create(Collection: OLEVariant);
const OPNAME = 'TOleVariantArrayEnum.Create';
begin
  try
    inherited Create;
    FCollection:= Collection;
    FLowBound := VarArrayLowBound(FCollection, 1);
    FHighBound := VarArrayHighBound(FCollection, 1);
    FIndex := FLowBound-1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOleVariantArrayEnum.GetCurrent: OLEVariant;
const OPNAME = 'TOleVariantArrayEnum.GetCurrent';
begin
  try
    Result:=FCollection[FIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOleVariantArrayEnum.GetEnumerator: IOleVariantEnum;
const OPNAME = 'TOleVariantArrayEnum.GetEnumerator';
begin
  try
    Result := Self;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOleVariantArrayEnum.MoveNext: Boolean;
const OPNAME = 'TOleVariantArrayEnum.MoveNext';
begin
  Result := False;
  try
    Result := FIndex < FHighBound;
    if Result then
      Inc(FIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
