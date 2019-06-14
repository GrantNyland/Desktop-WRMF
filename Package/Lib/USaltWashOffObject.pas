//
//
//  UNIT      : Contains TSaltWashOffObject Class
//  AUTHOR    : Lethabo Phatedi(Cornastone)
//  DATE      : 10/14/2016
//  COPYRIGHT : Copyright © 2016 DWS
//
//
unit USaltWashOffObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWS VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type

  TSaltWashOffObject = class(TAbstractDataObject)
  Public
  FFileNumber                  : Integer;
  FFileSaltWashOff01ExtraLines : TStringList;
  FSaltWashFile                : TString;

  Constructor Create(AFileNumber: Integer);
  procedure CreateMemberObjects; override;
  procedure DestroyMemberObjects; override;

  property FileSaltWashOff01ExtraLines : TStringList      read FFileSaltWashOff01ExtraLines;
  property FileNumber                  : Integer          read FFileNumber;
  property SaltWashOffFile             : TString          read FSaltWashFile;


  procedure Reset; override;
  function Initialise: boolean; override;

  end;

  implementation

{ TSaltWashOffObject }
uses UErrorHandlingOperations;

Procedure TSaltWashOffObject.CreateMemberObjects;
CONST OPNAME = 'TSaltWashOffObject.CreateMemberObjects';
begin
  try
   //Data in SaltWashOff(MISW).DAT Files

  FFileSaltWashOff01ExtraLines := TStringList.Create;
  FSaltWashFile                := TString.Create;

  Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


Procedure TSaltWashOffObject.DestroyMemberObjects;
CONST OPNAME = 'TSaltWashOffObject.DestroyMemberObjects';
begin
   try
   //Data in SaltWashOff(MISW).DAT Files

   FFileSaltWashOff01ExtraLines.Free;
   FSaltWashFile.Free;

   Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSaltWashOffObject.Initialise: boolean;
CONST OPNAME = 'TSaltWashOffObject.Initialise';
begin
  Result := False;
  try
  //Data in TSaltWashOff(MISW).DAT File

  FFileSaltWashOff01ExtraLines.clear;

  FSaltWashFile.FData := '';
  FSaltWashFile.FInitalised := False;
  FSaltWashFile.FLength := 0;
  FSaltWashFile.FDecimal := 0;
  FSaltWashFile.FDefaultPadding := True;

  Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSaltWashOffObject.Reset;
CONST OPNAME = 'TSaltWashOffObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TSaltWashOffObject.Create(AFileNumber: integer);
const OPNAME = 'TSaltWashOffObject.Create';
begin
  try
    FFileNumber := AFileNumber;
    inherited Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
