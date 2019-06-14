library CurveCalculator;

uses
  SysUtils,
  Classes,
  UCurveFittingOperations,
  UErrorHandlingOperations;

function CalculateCurveCoefficients(
  ACurveType: integer;
  ACoefficientCount: integer;
  ACurveCoefficients: PDouble;
  AYXPairCount: integer;
  AYXPairs: PDouble
): boolean; export; stdcall;
const OPNAME = 'CurveCalculator.CalculateCurveCoefficients';
var
  LIndex: longword;
  LCurveCoefficients: array of double;
  LYXPairs: array of double;
begin
  Result := False;
  try

    // Create Delphi arrays.
    SetLength(LCurveCoefficients, ACoefficientCount);
    SetLength(LYXPairs, AYXPairCount * 2);

    // Copy the point values into the Delphi arrays.
    for LIndex := 0 to AYXPairCount - 1 do
    begin
      LYXPairs[LIndex * 2]     := (PDouble(longword(AYXPairs) + 8 * (LIndex * 2)))^;
      LYXPairs[LIndex * 2 + 1] := (PDouble(longword(AYXPairs) + 8 * (LIndex * 2 + 1)))^;
    end;

    // Call the function.
    Result := CalculateCurveCoefficientsDelphi(TCurveType(ACurveType), LCurveCoefficients, LYXPairs);

    // Copy the coefficients back.
    for LIndex := 0 to ACoefficientCount - 1 do
      (PDouble(longword(ACurveCoefficients) + 8 * LIndex))^ := LCurveCoefficients[LIndex];

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

exports
  CalculateCurveCoefficients;

begin
end.
 