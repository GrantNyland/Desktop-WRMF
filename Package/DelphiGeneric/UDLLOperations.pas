//
//
//  UNIT      : Contains DLL utility operations.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/06/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDLLOperations;

interface

function LoadDLL(ADLLFileName: string; var ADLLHandle: longword; AReportError: boolean; ALocation: string): boolean;
function GetDLLFunction(ADLLHandle: longword; ADLLFileName, ADLLFunctionName: string; var AFunction: pointer; ALocation: string): boolean;

implementation

uses
  SysUtils,
  Windows,
  UErrorHandlingOperations;

function LoadDLL(ADLLFileName: string; var ADLLHandle: longword; AReportError: boolean; ALocation: string): boolean;
begin
  Result := False;
  ADLLHandle := 0;
  try

    // Check that DLL file exists. Do a silent ignore if it does not exist.
    if (not FileExists(ADLLFileName)) then
    begin
      if AReportError then
        raise Exception.Create('Could not find DLL [' + ADLLFileName + '].');
    end else begin

      // Attempt to load the DLL.
      //ADLLHandle := LoadLibrary(PChar(ADLLFileName));
      ADLLHandle := LoadLibrary(PChar(ADLLFileName));
      if (ADLLHandle = 0) then
      begin
        if AReportError then
          raise Exception.Create('Could not load DLL [' + ADLLFileName + ']. LoadLibrary Error Code : '+SysErrorMessage(GetLastError)); 
      end else begin
        Result := True;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, ALocation) end;
end;

function GetDLLFunction(ADLLHandle: longword; ADLLFileName, ADLLFunctionName: string; var AFunction: pointer; ALocation: string): boolean;
begin
  Result := False;
  AFunction := nil;
  try

    // Get the function address.
    AFunction := GetProcAddress(ADLLHandle, PChar(ADLLFunctionName));
    if (AFunction = nil) then
    begin
      raise Exception.Create(
        'Could not get procedure [' + ADLLFunctionName + '] in DLL [' + ExtractFileName(ADLLFileName) + '].');
    end else begin

      // Done.
      Result := True;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, ALocation) end;
end;

end.

