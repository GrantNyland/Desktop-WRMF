//
//
//  UNIT      : Contains string data time operations Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/11
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UStringDateTimeOperations;

interface

function DateTimeFromStamp(ADateTimeStamp: string): TDateTime;
function DateTimeToStamp(ADateTime: TDateTime): string;

implementation

uses SysUtils;

//
// Extracts the date time from date time stamp string.
//
// Returns the date time on success. Raises an exception on error.
//
function DateTimeFromStamp(
  ADateTimeStamp: string // Contains a string from which a date time can be extracted.
): TDateTime;
const
  OPNAME = 'UStringOperations.DateTimeFromStamp';
var
  LYear, LMonth, LDay: word; // Contains the date values.
  LHour, LMinute, LSecond, LMilliSecond: word; // Contains the time values.
begin

  // Structure : YYYYMMDDHHMMSSZZZ
  try
    LYear        := StrToInt(Copy(ADateTimeStamp,  1, 4));
    LMonth       := StrToInt(Copy(ADateTimeStamp,  5, 2));
    LDay         := StrToInt(Copy(ADateTimeStamp,  7, 2));
    LHour        := StrToInt(Copy(ADateTimeStamp,  9, 2));
    LMinute      := StrToInt(Copy(ADateTimeStamp, 11, 2));
    LSecond      := StrToInt(Copy(ADateTimeStamp, 13, 2));
    LMilliSecond := StrToInt(Copy(ADateTimeStamp, 15, 3));

    // Encode the value.
    Result := EncodeDate(LYear, LMonth, LDay) +
              EncodeTime(LHour, LMinute, LSecond, LMilliSecond);
  except

    // Re-Raise the exception with more information.
    on E: Exception do
      raise Exception.Create('Time stamp format error : [' + E.Message + '] : [' + ADateTimeStamp + ']');
  end;
end;

//
// Constructs a string date time stamp.
//
// Returns the string. This function cannot fail.
//
function DateTimeToStamp(
  ADateTime: TDateTime // Contains a date time value.
): string;
var
  LYear, LMonth, LDay: word; // Contains the date values.
  LHour, LMinute, LSecond, LMilliSecond: word; // Contains the time values.
begin
  // Structure : YYYYMMDDHHMMSSZZZ
  DecodeDate(ADateTime, LYear, LMonth, LDay);
  DecodeTime(ADateTime, LHour, LMinute, LSecond, LMilliSecond);
  Result := Format('%0.4d%0.2d%0.2d%0.2d%0.2d%0.2d%0.3d',
    [LYear, LMonth, LDay, LHour, LMinute, LSecond, LMilliSecond]);
end;

end.
