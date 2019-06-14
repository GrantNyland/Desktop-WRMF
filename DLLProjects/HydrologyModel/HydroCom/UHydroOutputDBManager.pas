(******************************************************************************)
(* Contains : THydroOutputDBManager.                                          *)
(* Manages the import and export of Simulation Result File data.              *)
(******************************************************************************)

unit UHydroOutputDBManager;

interface

uses
  Classes, Contnrs,
  UTimeSeries,

  UModuleDBManager,
  UHydroOutput;

type

  THydroOutputDBManager = class(TModuleDBManager)
  protected
  public
    function LoadOutputFromDB (ANetworkID      : Integer;
                               AElementType    : String;
                               AElementSubType : String;
                               AElementID      : Integer;
                               ASubElementID   : Integer): Boolean;
    function LoadResultTypesFromDB : Boolean;
  end;

var
  GHydroOutputDBManager : THydroOutputDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UHydroDBAgent,
  UErrorHandlingOperations;

(* THydroOutputDBManager ******************************************************)

function THydroOutputDBManager.LoadOutputFromDB (ANetworkID      : Integer;
                                                 AElementType    : String;
                                                 AElementSubType : String;
                                                 AElementID      : Integer;
                                                 ASubElementID   : Integer): Boolean;
const OPNAME = 'THydroOutputDBManager.LoadOutputFromDB';
var
  LBufferSize    : Integer;
  LBytesRead     : integer;
  LBuffer        : PChar;
  LDataSize      : Integer;
  LHydroOutput   : THydroOutput;
  LData          : array of Double;
  LIndex         : Integer;
  LBlobStream    : TStream;
  LQuery         : TDataSet;
  LSQL           : String;
  LYear          : Integer;
  LHeaderSize    : Integer;
begin
  Result := False;
  try
    LSQL   := 'SELECT * FROM SimulationResults WHERE NetworkID = ' + IntToStr(ANetworkID);
    if (AElementType <> '') then
      LSQL := LSQL + ' AND ElementType = ' + QuotedStr(AElementType);
    if (AElementType <> '') then
      LSQL := LSQL + ' AND ElementSubType = ' + QuotedStr(AElementSubType);
    if (AElementID <> 0) then
      LSQL := LSQL + ' AND ElementID = ' + IntToStr(AElementID);
    if (ASubElementID <> 0) then
      LSQL := LSQL + ' AND SubElementID = ' + IntToStr(ASubElementID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LHydroOutput := THydroOutputAgent(ModuleAgent).AddHydroOutput;
        if (LHydroOutput <> nil) then
        begin
          LHydroOutput.Populate(LQuery.FieldByName('ResultID').AsInteger,
                                LQuery.FieldByName('NetworkID').AsInteger,
                                Trim(LQuery.FieldByName('ElementType').AsString),
                                Trim(LQuery.FieldByName('ElementSubType').AsString),
                                LQuery.FieldByName('ElementID').AsInteger,
                                LQuery.FieldByName('SubElementID').AsInteger,
                                LQuery.FieldByName('ResultTypeID').AsInteger,
                                LQuery.FieldByName('AllZero').AsInteger = 1);
          if (NOT LHydroOutput.AllZero) then
          begin
            LBlobStream := LQuery.CreateBlobStream(LQuery.FieldByName('ResultData'), bmRead);
            try
              // Determine how much data is in the file.
              LBlobStream.Seek(0, soFromBeginning);
              LBufferSize := LBlobStream.Size;
              LBlobStream.Seek(0, soFromBeginning);

              // Create a buffer large enough to contain all the data.
              GetMem(LBuffer, LBufferSize);
              try
                ZeroMemory(LBuffer, LBufferSize);
                LBytesRead := LBlobStream.Read(LBuffer[0], LBufferSize);

                // Complain if any data was not read.
                if (LBytesRead <> LBufferSize) then
                  raise Exception.CreateFmt('Could not read all the data in the Blob file [%s]. ', ['DB']);

                // Copy the raw data to the header structure.
                LHeaderSize := SizeOf(TOutputHeader);
                if (LBytesRead >= LHeaderSize) then
                begin
                  CopyMemory(@LHydroOutput.Header, LBuffer, LHeaderSize);

                  // Copy the remainder of the buffer to the raw data section.
                  LDataSize := LBufferSize - LHeaderSize;
                  SetLength(LData, LHydroOutput.Header.FNoOfValues);
                  CopyMemory(@LData[0], @LBuffer[LHeaderSize], LDataSize);

                  LYear := LHydroOutput.Header.FStartDate.Year;
                  LIndex := 0;
                  while (LIndex < LHydroOutput.Header.FNoOfValues) do
                  begin
                    LHydroOutput.TimeSeries.AddPeriodValue(LYear, LData[LIndex]);
                    LIndex := LIndex + 1;
                    if ((LIndex mod 12) = 0) then
                      LYear := LYear + 1;
                  end;
                  Result := True;
                end;
              finally
                FreeMem(LBuffer);
              end;
            finally
              LBlobStream.Free
            end;
          end;
        end;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutputDBManager.LoadResultTypesFromDB : Boolean;
const OPNAME = 'THydroOutputDBManager.LoadResultTypesFromDB';
var
  LQuery           : TDataSet;
  LSQL             : String;
  LHydroResultType : THydroResultType;
begin
  Result := False;
  try
    LSQL   := 'SELECT * FROM SimulationResultTypes';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LHydroResultType := THydroOutputAgent(ModuleAgent).AddHydroResultType;
        if (LHydroResultType <> nil) then
        begin
          LHydroResultType.Populate(LQuery.FieldByName('ResultTypeID').AsInteger,
                                    Trim(LQuery.FieldByName('ElementType').AsString),
                                    Trim(LQuery.FieldByName('ElementSubType').AsString),
                                    Trim(LQuery.FieldByName('Description').AsString),
                                    Trim(LQuery.FieldByName('Units').AsString));
        end;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

