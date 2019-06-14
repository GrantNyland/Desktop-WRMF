unit UConditionalOpp;

interface
uses
 System.Generics.Collections,
 Classes,
 Contnrs,
 SysUtils;

function ConditionalOpp(TrueValue: boolean; TrueResult, FalseResult: integer): string;

implementation
 uses
  UErrorHandlingOperations;
function ConditionalOpp(TrueValue: boolean; TrueResult, FalseResult: integer): string;
const OPNAME = 'TPlanningMineGrowthFactor.ConditionalOpp';
begin
  try
    if TrueValue then
      Result := IntToStr(TrueResult)
    else
      Result := IntToStr(FalseResult)
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
