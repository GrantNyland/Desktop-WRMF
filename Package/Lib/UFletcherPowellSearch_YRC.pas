//
//
//  UNIT      : Contains curve fitting operations using the Fletcher-Powell searcher.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/10/29
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFletcherPowellSearch_YRC;


interface

uses
  UCurveFittingConstants;


function Weight(APointIndex: integer): real;
procedure Evaluate(VarX: REAL; var Model: REAL; var Grad: array of real);
function DoFletcherPowell (ACurveType             : TCurveType;
                           var ACurveCoefficients : array of double;
                           var AYXPairs           : array of double;
                           AForceThroughLastPoint : boolean = FALSE): boolean;
function CalculateCurveCoefficients(ACurveType: integer;
           ACoefficientCount: integer; ACurveCoefficients: PDouble;
           AYXPairCount: integer; AYXPairs: PDouble): boolean; export; stdcall;


implementation

uses
  SysUtils,
  UFletcherPowellSearch;

var
  GCurveType : TCurveType;
  GForce     : boolean;


//
// The program minimizes a weighted residual sum of squares of the form:
//
//                   RSS = ä Weight*(ExptY-CalcY)ý
//
// In this case the wieght is 1 for all points except the first point. The weight
// for the first point is very large because the yield reliability curve must
// pass through the first point which is the firm yield.
//
function Weight(APointIndex: integer): real;
const OPNAME = 'Weight';
begin
  if (APointIndex = 1) OR (GForce AND (APointIndex = LastPt)) then
  begin
    Result := 999999999.9;
  end else begin
    Result := 1.0;
  end;
end;


//
// The function that is being used to fit to the points is placed here.
// A second or third order polynomial is being used in this case.
//
// This procedure calculates a Y value for a given X value and also
// computes the contribution of this point to the RSS gradients.
//
// The formulae for the gradients are obtained by differentiating the
// main formula with respect to the constant in question and not with respect
// to x.
//
procedure Evaluate(VarX: REAL; var Model: REAL; var Grad: array of real);
const OPNAME = 'Evaluate';
begin
  case GCurveType of
    ctQuadraticRegression :
      begin
        Model   := X[1]+X[2]*VarX+X[3]*VarX*VarX;                     // Y = A+Bx+Cx^2
        Grad[1] := 1;                                                 // dY/dA = 1
        Grad[2] := VarX;                                              // dY/dA = x
        Grad[3] := VarX*VarX;                                         // dY/dA = x^2
      end;
    ctTrinomialRegression :
      begin
        Model   := X[1]+X[2]*VarX+X[3]*VarX*VarX+X[4]*VarX*VarX*VarX; // Y = A+Bx+Cx^2+Dx^3
        Grad[1] := 1;                                                 // dY/dA = 1
        Grad[2] := VarX;                                              // dY/dA = x
        Grad[3] := VarX*VarX;                                         // dY/dA = x^2
        Grad[4] := VarX*VarX*VarX;                                    // dY/dA = x^3
      end;
  end;
end;


//
// This is the entry point to the curve fitting utility. The function
// signature is the same as all the other curve fitting utilities.
//
// The coefficients and data points are copied into the model data structures
// and the model is run.
//
function DoFletcherPowell (ACurveType             : TCurveType;
                           var ACurveCoefficients : array of double;
                           var AYXPairs           : array of double;
                           AForceThroughLastPoint : boolean = FALSE): boolean;
const OPNAME = 'DoFletcherPowell';
var
  LIndex: integer;
begin
  GForce := AForceThroughLastPoint;
  
  // Do some preliminary error checking.
  if (not(ACurveType in [ctQuadraticRegression, ctTrinomialRegression])) then
    raise Exception.CreateFmt('Curve type [%d] not supported by Fletcher-Powell searcher.', [integer(ACurveType)]);

  ExptX  := nil;
  ExptY  := nil;
  CalcY  := nil;
  X      := nil;
  XC     := nil;
  XR     := nil;
  G      := nil;
  GC     := nil;
  GR     := nil;
  Grad   := nil;

  // Allocate the arrays. NOTE: The zero element of the arrays are not used
  // as the program was originally written for one based arrays.
  GCurveType := ACurveType;
  NumVar := Length(ACurveCoefficients);
  SetLength(X,     NumVar+1);
  SetLength(XC,    NumVar+1);
  SetLength(XR,    NumVar+1);
  SetLength(G,     NumVar+1);
  SetLength(GC,    NumVar+1);
  SetLength(GR,    NumVar+1);
  SetLength(Grad,  NumVar+1);
  SetLength(Index, NumVar+1);
  NumDataPts := Length(AYXPairs) div 2;
  SetLength(ExptX, NumDataPts+1);
  SetLength(ExptY, NumDataPts+1);
  SetLength(CalcY, NumDataPts+1);
  try
    // Copy accross initial values for the coefficients.
    for LIndex := 0 to NumVar - 1 do
      X[LIndex + 1] := ACurveCoefficients[LIndex];

    // Copy the data accross to the model data arrays.
    for LIndex := 0 to NumDataPts - 1 do
    begin
      ExptY[LIndex + 1] := AYXPairs[LIndex * 2];
      ExptX[LIndex + 1] := AYXPairs[LIndex * 2 + 1];
    end;

    // Do the curve fitting.
    RunTheProgram(NumVar);

    // Copy back the calculated values for the coefficients.
    for LIndex := 0 to NumVar - 1 do
      ACurveCoefficients[LIndex] := X[LIndex + 1];

  finally
    ExptX  := nil;
    ExptY  := nil;
    CalcY  := nil;
    X      := nil;
    XC     := nil;
    XR     := nil;
    G      := nil;
    GC     := nil;
    GR     := nil;
    Grad   := nil;
  end;
  // Done
  Result := True;
end;


//
// This is an exported function for building a DLL that can be called from Excell.
//
function CalculateCurveCoefficients(
  ACurveType: integer;
  ACoefficientCount: integer;
  ACurveCoefficients: PDouble;
  AYXPairCount: integer;
  AYXPairs: PDouble
): boolean; export; stdcall;
const OPNAME = 'CalculateCurveCoefficients';
var
  LIndex: longword;
  LCoefArray, LPointArray: array of double;
begin

  // Create the Delphi style arrays.
  SetLength(LCoefArray, ACoefficientCount);
  SetLength(LPointArray, AYXPairCount * 2);

  // Copy accross initial values for the coefficients.
  for LIndex := 0 to ACoefficientCount - 1 do
    LCoefArray[LIndex] := (PDouble(longword(ACurveCoefficients) + 8 * LIndex))^;

  // Copy the data accross to the model data arrays.
  for LIndex := 0 to (AYXPairCount * 2) - 1 do
    LPointArray[LIndex] := (PDouble(longword(AYXPairs) + 8 * LIndex))^;

  // Do the curve fitting.
  DoFletcherPowell(TCurveType(ACurveType), LCoefArray, LPointArray);

  // Copy back the calculated values for the coefficients.
  for LIndex := 0 to ACoefficientCount - 1 do
    (PDouble(longword(ACurveCoefficients) + 8 * LIndex))^ := LCoefArray[LIndex];

  // Done
  Result := True;
end;


end.
