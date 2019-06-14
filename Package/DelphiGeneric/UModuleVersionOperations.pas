//
//
//  UNIT      : Contains a module version utility function.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UModuleVersionOperations;

interface

uses
  Classes;

function GetModuleVersion(AModuleFileName: string): string;
function GetCurrentModuleVersion: string;

implementation

uses
  SysUtils,
  Windows;
function GetCurrentModuleVersion: string;
var
  ls_Module: array[0..100] of char; // Contains the name of the DLL.
begin

    // Get the name of the current DLL or application.
    ls_Module[0] := #0;
    GetModuleFileName(HInstance, ls_Module, SizeOf(ls_Module));

    Result := GetModuleVersion(ls_Module);
end;

function GetModuleVersion(AModuleFileName: string): string;
var
  LSizeOfVersionInfoStructure: integer;
  LVersionInfoStructure: pointer;
  LIndex: dword;
  LDataStr: string;
begin

  // Get the size of the version info data.
  LSizeOfVersionInfoStructure := GetFileVersionInfoSize(PChar(AModuleFileName), LIndex);
  if (LSizeOfVersionInfoStructure > 0) then
  begin

    // Allocate enough memory for the version info data.
    LVersionInfoStructure := AllocMem(LSizeOfVersionInfoStructure);

    // Get the version info data.
    GetFileVersionInfo(PChar(AModuleFileName), 0, LSizeOfVersionInfoStructure, LVersionInfoStructure);

    // Discard all the nulls which also converts from wide string.
    LDataStr := '';
    for LIndex := 0 to LSizeOfVersionInfoStructure - 1 do
      if (PChar(LVersionInfoStructure)[LIndex] <> #0) then
        LDataStr := LDataStr + PChar(LVersionInfoStructure)[LIndex];

    // Find the file version number.
    Result := Copy(LDataStr, Pos('FileVersion', LDataStr) + 11, 6);

    // Truncate if five digit version number.
    if (not CharInSet(Result[6],['0'..'9'])) then
      Result := Copy(Result, 1, 5);
  end;
end;

end.
