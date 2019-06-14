//
//  UNIT      : Contains TFieldFileReference Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UFieldFileReference;

interface

uses
  UAbstractObject;

type
  TFieldFileReference = class(TAbstractObject)
  protected
    FFieldProperty: TAbstractFieldProperty;
    FFileName ,FLineNumber: string;
    FStartCharacter: integer;
    FLength: integer;
    procedure CreateMemberObjects; override;
  public
    procedure PopulateMemberVariables(AFieldProperty: TAbstractFieldProperty;
      AFileName, ALineNo: string; AStartCharacter, ALength: integer);
    function HitTest(AFileName, ALineNo: string; ACharacterPosition: integer): boolean;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TFieldFileReference.CreateMemberObjects;
const OPNAME = 'TFieldFileReference.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FFieldProperty := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldFileReference.PopulateMemberVariables(
  AFieldProperty: TAbstractFieldProperty; AFileName, ALineNo: string; AStartCharacter, ALength: integer);
const OPNAME = 'TFieldFileReference.PopulateMemberVariables';
begin
  try
    FFieldProperty  := AFieldProperty;
    FFileName       := AFileName;
    FLineNumber     := ALineNo;
    FStartCharacter := AStartCharacter;
    FLength         := ALength;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldFileReference.HitTest(AFileName, ALineNo: string; ACharacterPosition: integer): boolean;
const OPNAME = 'TFieldFileReference.HitTest';
var LStart: integer;
begin
  Result := False;
  try

    // Check the file name.
    if (UpperCase(ExtractFileName(AFileName)) = UpperCase(ExtractFileName(FFileName))) then
    begin

      // Check the line number.
      if (ALineNo = FLineNumber) or (FLineNumber = '') then
      begin

        // Check the start position.
        if (ACharacterPosition >= FStartCharacter) or (FStartCharacter < 0) then
        begin

          // Calculate a realistic start.
          LStart := FStartCharacter;
          if (FStartCharacter <= 0) then
            LStart := 1;

          // Check the end position.
          if (ACharacterPosition < LStart + FLength) or (FLength < 0) then
          begin
            Result := True;
          end;
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
