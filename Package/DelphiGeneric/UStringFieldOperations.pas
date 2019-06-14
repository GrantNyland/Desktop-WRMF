//
//  UNIT      : Contains utility functions for managing strings fields.
//  AUTHOR    : Grant Nyland.
//  DATE      : 2002/03/15
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UStringFieldOperations;

interface

uses
  Classes;

procedure ExtractFields(AFromString: string; ADelimiters: string; AFields: TStrings);
procedure GetFieldNo(AFieldNo: integer; AFromString: string; ADelimiters: string; var AField: string);

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

//
// Extracts the fields from the string. Works the same as CommaText.
// It is not sensitive to the quotation mark. The delimiters do not
// need to be a comma. Multiple delimeters can be used.
//
procedure ExtractFields(
  AFromString: string; // Contains a number of fields in string format.
  ADelimiters: string; // Contains the delimeter characters.
  AFields: TStrings    // Points to a string list that is to contain the fields.
);
const OPNAME = 'ExtractFields';
var
  LDelimiterIndex: integer; // Contains the current delimiter character index.
  LThisPos: integer;        // Contains the current delimiter position index.
  LLen: integer;            // Contains a character index.
  LPos: integer;            // Contains a character index.
  LTotalLength: integer;    // Contains the total length of the string.
  LOneField: string;        // Contains a single extracted field.
begin
  try

    // Start by clearing the string list. Proceed if there is data.
    AFields.Clear;
    LTotalLength := Length(AFromString);
    if LTotalLength > 0 then
    begin

      // Run loop if more than one field.
      LLen := 0;
      LPos := 1;
      repeat

        // Move to the start of the next field.
        LPos := LPos + LLen;

        // Get the next closest delimeter.
        LLen := Length(AFromString) + 1;
        for LDelimiterIndex := 1 to Length(ADelimiters) do
        begin
          LThisPos := Pos(ADelimiters[LDelimiterIndex],
                            Copy(AFromString, LPos, LTotalLength));
          if (LThisPos < LLen) and (LThisPos > 0) then LLen := LThisPos;
        end;
        if (LLen > Length(AFromString)) then LLen := 0;

        // Add the field if found.
        if LLen > 0 then
        begin
          LOneField := Copy(AFromString, LPos, LLen - 1);
          AFields.Add(LOneField);
        end;

      // Until no delimiters are found.
      until LLen <= 0;

      // Add last field.
      LOneField := Copy(AFromString, LPos, LTotalLength);
      AFields.Add(LOneField);

    end; // IF : Is there data.

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Gets the required field number from the delimited string.
//
procedure GetFieldNo(
  AFieldNo: integer;   // Contains the required field number.
  AFromString: string; // Contains a number of fields in string format.
  ADelimiters: string; // Contains the delimeter characters.
  var AField: string  // Points the the string variable that will get the field.
);
const OPNAME = 'GetFieldNo';
var LAllFields: TStringList;
begin
  AField := '';
  try
    LAllFields := TStringList.Create;
    try
      ExtractFields(AFromString, ADelimiters, LAllFields);
      if (LAllFields.Count > AFieldNo) then
        AField := LAllFields[AFieldNo];
    finally
      LAllFields.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
